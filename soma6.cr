require "option_parser"
require "./src/wirewright"
require "./uiRb"
require "./sfpaint"
require "./pprint2"
require "./mstep4"
require "./d7vr"

alias UIR::Platform::Current = SFML

# FIXME: this thing is crazy big & complicated & nasty. Can we simplify?
# FIXME: due to complexity it's hard to *stop* a document, to e.g. implement pause/unpase
#  which we require for the command palette.
class Document
  class Mailbox
    @state = Atomic(State).new(State.new)

    def settled? : Bool
      state = @state.get(:relaxed)
      state.settled?
    end

    def enqueue(prompt : Term, & : ->) : Nil
      state0 = @state.get(:relaxed)
      while true
        state1 = state0.enqueue(prompt)
        state0, ok = @state.compare_and_set(state0, state1, :relaxed, :relaxed)
        break if ok
      end

      return unless state0.settled?

      yield
    end

    def dequeue? : Term?
      state0 = @state.get(:relaxed)
      while true
        state1, prompt = state0.dequeue
        state0, ok = @state.compare_and_set(state0, state1, :relaxed, :relaxed)
        break if ok
      end
      prompt
    end

    def settle? : Bool
      state0 = @state.get(:relaxed)

      while true
        state1 = state0.settled
        state0, ok = @state.compare_and_set(state0, state1, :relaxed, :relaxed)
        break if ok
      end

      state1.settled?
    end
  end

  class Mailbox::State
    getter? settled : Bool

    def initialize(@queue = BiList(Term).new, @settled = true)
    end

    delegate :empty?, to: @queue

    def enqueue(prompt : Term) : State
      State.new(@queue.append(prompt), settled: false)
    end

    def dequeue : {State, Term?}
      case @queue
      when .empty? then {State.new(@queue, @settled), nil}
      when .one?   then {State.new(@queue.rest, @settled), @queue.first}
      else
        {State.new(@queue.rest, @settled), @queue.first}
      end
    end

    def settled : State
      State.new(@queue, settled: @queue.empty?)
    end
  end

  # FIXME: this implementation of history is not *really* correct. What I envision is
  # us taking periodic snapshots of the document while it is running and also snapshots
  # after "important" events (a bit similarly to what we do now). We should then have the ability
  # to "pause" the document and review these snapshots using the UI, in a timeline-kind of way,
  # perhaps with branches or even a graph; or something like that. We should then be able to
  # select one of the versions we like and "un-pause" it.

  class History
    getter? present : Term::Dict?

    def initialize
      @past = [] of Term::Dict
      @future = [] of Term::Dict
    end

    def push(document : Term::Dict) : Nil
      return if document.empty?
      return if @present == document

      @future.clear

      unless present = @present
        @present = document
        return
      end

      @past << present
      @present = document
    end

    def undo : Nil
      return unless present = @present

      @future.unshift(present)
      @present = @past.pop?
    end

    def redo : Nil
      return unless succ = @future.shift?

      if present = @present
        @past << present
      end
      @present = succ
    end
  end

  @rem0 : Term::Num
  @rem1 : Term::Num
  @mouse : {Term::Num, Term::Num}

  def initialize(@title : String, @draw : Channel({Term::Dict, Channel(Term::Dict)}), @mstep : Meridium::Step)
    @document_thread = Fiber::ExecutionContext::SingleThreaded.new(@title)

    @mailbox = Mailbox.new

    # The following instance variables are owned exclusively by the document
    # thread. No one else must know they exist.
    @nictx = Nitrene::StepContext.new { alarm }

    @dwuir = Term[]
    @concealed = false
    @important = false
    @history = History.new
    @document = Term[]
    @drawn = Term[]
    @rem0 = @rem1 = Term[16]
    @initial = true
    @state = State::Clean
    @mouseq = Deque(Term).new
    @mouseq_state = :default
    @mouse = {Term[0], Term[0]}
  end

  def settled? : Bool
    @mailbox.settled?
  end

  # Wakes the document thread up if it's sleeping.
  def alarm : Nil
    send(Term.of(:alarm))
  end

  # Adds *prompt* to this document's mailbox.
  def send(prompt : Term) : Nil
    @mailbox.enqueue(prompt) do
      initial0 = @initial

      @initial = false

      # Force initial if it is (open ...) waking up the document thread.
      # Otherwise the document will manage kickstarting itself during
      # transition.
      Term.matchpi?(prompt, %{(open _)}) do
        initial0 = true
      end

      @document_thread.spawn do
        initial = initial0

        while mainloop?(initial: initial)
          initial = false
        end

        # WARNING: after we exit the loop above we're back in thread-
        # unsafe territory!
      end
    end
  end

  # Hosts the main loop run by the document thread.
  #
  # The main loop "runs the physics": advances the document step-by-step.
  # These advancements are interleaved with `rendezvous`.
  #
  # Once the document has settled the mainloop ends. Returns `true` if
  # the mainloop needs to be restarted; `false` otherwise.
  private def mainloop?(*, initial = true) : Bool
    draw

    @document = D7.run(@document,
      log: D7::Log::None.new,
      transition: Rhodium.transition,
      step: D7.steps(rendezvous, Rhodium.step, Nitrene.step(@nictx), @mstep.fn),
      goal: D7::Goal.none,
      initial: initial,
    )

    draw

    !@mailbox.settle?
  end

  enum DoTransition : UInt8
    Yes
    No
  end

  # Rendezvous step assesses and modifies the state of the document. It enhances
  # the document based on prompts (`peek`), and decides whether the document should
  # be drawn.
  private def rendezvous : D7::Step
    D7::Step.new do |document|
      @document = document

      transition = peek

      state0 = @state
      state1 = state

      case {state0, state1}
      when {State::Clean, State::Drawable}
        draw
        state1 = State::Clean
      when {State::Dirty, State::Drawable}
        state1 = State::Clean
      when {State::Dirty, _}
        state1 = state0
      end

      @state = state1

      {@document, transition.yes?}
    end
  end

  # :nodoc:
  enum State : UInt8
    Clean
    Dirty
    Drawable
  end

  # Returns the current state of the document (based on the front event or its absence).
  private def state : State
    unless event = Rhodium::Q.of(@document, Rhodium::Events).first?
      return State::Drawable
    end

    Term.case(event) do
      matchpi %{(edit @user _)} { State::Dirty }
      otherwise { State::Clean }
    end
  end

  # Draws the document.
  private def draw : Nil
    # TODO: what to do if its empty though? We have to do something...
    return if @document.empty? || @concealed

    instance0 = @drawn
    instance1 = D7VR.instance(@document)

    # Check if instance changed or mouse moved (potential hover/unhover).
    #   - If instance changed we must redraw.
    #   - If mouse moved we must redraw due to potential mouse hover/unhover.
    #   - Otherwise we may not redraw.
    same = instance0 == instance1 && @rem0 == @rem1
    return if same && @mouseq.empty?

    # We don't *really* need to redraw though if the mouse moved. The old
    # dwUIR will work just as well!
    unless same
      remember

      @drawn = instance1
      @rem0 = @rem1
      printout = D7VR.printout(instance1, rem: @rem0)
      unit = Term.of(:group, D7VR.unit(printout), style: "origin")
      uir = Microfold.uir(Microfold::SPEC, unit, rem: @rem0)

      dwuir_chan = Channel(Term::Dict).new

      @draw.send({uir.as_d, dwuir_chan})

      @dwuir = dwuir_chan.receive

      # The instance changed, so anything could be under the mouse. We have to
      # emit a fake mouse motion event, just to make sure.
      if @mouseq.empty?
        @mouseq << Term.of(:mouse, :motion, *@mouse)
      end
    end

    dwuir = Term.of(@dwuir)

    # FIXME: the way this is organized is pure instanity. WTF is mouse event handling
    # doing inside of the draw function ?!?!?!

    while mevent = @mouseq.shift?
      Term.case(mevent) do
        # Set `hover: true` on hovered nodes, and `hover: false` (or removed) on
        # unhovered ones.
        matchpi %{(mouse motion x_number y_number)} do
          @mouse = {x.unsafe_as_n, y.unsafe_as_n}

          target = below?(x.unsafe_as_n, y.unsafe_as_n)

          mark { |nodepath, node| hover(nodepath, node, target) }
        end

        matchpi %{(mouse press)}, %{(mouse release)} do
          event(mevent)
          return
        end

        otherwise { }
      end
    end

    @mouseq_state = :default
  end

  # Returns the nodepath of the topmost node that includes the point *x*, *y*,
  # if any. Returns `nil` otherwise.
  private def below?(x : Term::Num, y : Term::Num) : Stack(Int32)?
    stratum = UIR.stratum(Term.of(@dwuir), x, y)
    stratum.leftmost? do |keypath|
      hit = @dwuir.follow(keypath)
      next unless backlink = hit[:"#backlink"]?
      next unless backlink = backlink.as_itemsonly_d?

      nodepath = Stack(Int32).new(backlink.size)

      valid = backlink.items.all? do |index|
        nodepath << (index.to?(Int32) || next)
      end

      next unless valid

      nodepath
    end
  end

  private def mark(& : Stack(Int32), Term -> Term) : Nil
    nodepath = Stack(Int32).new

    while Rhodium.successor?(@document, nodepath)
      node0 = Rhodium.follow(@document, nodepath)
      node1 = yield nodepath, node0
      next if node0.same?(node1)

      @document = Rhodium.assign(@document, nodepath, node1)
    end
  end

  private def hover(nodepath : Stack(Int32), node : Term, mouseover : Stack(Int32)?) : Term
    unless Rhodium.active?(@document, nodepath, node)
      return node
    end

    pointee = nodepath == mouseover

    Term.case(node) do
      # If this particular button is hovered, set `hover: true`. If it is not,
      # remove the hover prop (this behavior is specific to buttons).
      matchpi %{[button _*]} do
        Term.of(node.morph({:hover, pointee ? true : nil}))
      end

      # If it has inbox, we also notify.
      matchpi %[{¦ hover_boolean inbox_dict}] do
        case {hover.true?, pointee}
        in {false, false}, {true, true}
          # No change
          node
        in {false, true}
          # Just got hovered
          Term.of(node.morph({:hover, true}, {:inbox, inbox.append({:hover})}))
        in {true, false}
          # Just got unhovered
          Term.of(node.morph({:hover, false}, {:inbox, inbox.append({:unhover})}))
        end
      end

      matchpi %[{¦ hover_boolean}] do
        case {hover.true?, pointee}
        in {false, false}, {true, true}
          # No change
          node
        in {false, true}
          # Just got hovered
          Term.of(node.morph({:hover, true}))
        in {true, false}
          # Just got unhovered
          Term.of(node.morph({:hover, false}))
        end
      end

      otherwise { node }
    end
  end

  private def remember : Nil
    return unless @important

    @history.push(@document)
    @important = false
  end

  # Checks the mailbox for new prompts. If none, returns immediately. If some,
  # handles the front prompt.
  private def peek : DoTransition
    if prompt = @mailbox.dequeue?
      return handle(prompt)
    end

    DoTransition::No
  end

  # Handles the given *prompt*. We call events directed toward the document *prompts*
  # to avoid confusion (e.g. relative to UI events in general). This method is the main
  # dispatch point for prompts.
  private def handle(prompt : Term) : DoTransition
    Term.case(prompt) do
      matchpi %{(open seed_dict)} do
        @important = true

        open(seed.unsafe_as_d)

        DoTransition::Yes
      end

      matchpi %{(conceal)} do
        @concealed = true

        DoTransition::No
      end

      matchpi %{(reveal)} do
        @concealed = false

        DoTransition::No
      end

      # FIXME: WTF?!
      matchpi %{(key f4)} do
        puts ML.display(@document)

        DoTransition::No
      end

      # Undo
      matchpi %{(key C-z)} do
        @history.undo

        if document = @history.present?
          @document = document
        end

        DoTransition::No
      end

      # Redo
      matchpi %{(key C-r)} do
        @history.redo

        if successor = @history.present?
          @document = successor
        end

        DoTransition::No
      end

      # Zoom in
      matchpi %{(key C-equal)} do
        @rem1 += 1

        DoTransition::No
      end

      # Zoom out
      matchpi %{(key C-minus)} do
        @rem1 = Math.max(Term[7], @rem1 - 1)

        DoTransition::No
      end

      matchpi %{(key _)}, %{(input _string)} do
        @important = true

        event(Term.of(:edit, {:edge, :user}, prompt))

        DoTransition::No
      end

      matchpi %{(event e_)} do
        event(e)

        DoTransition::No
      end

      matchpi %{(mouse motion x_number y_number)} do
        if @mouseq_state == :motion
          @mouseq[-1] = prompt
        else
          @mouseq << prompt
          @mouseq_state = :motion
        end

        DoTransition::No
      end

      matchpi %{(mouse press)}, %{(mouse release)} do
        @important = true
        @mouseq << prompt
        @mouseq_state = :default

        DoTransition::No
      end

      otherwise do
        DoTransition::No
      end
    end
  end

  # Replaces the document with a new *seed*.
  private def open(seed : Term::Dict) : Nil
    @document = seed
  end

  # Enqueues *event* onto the document's queue.
  private def event(event : Term) : Nil
    @document = Rhodium::Q.of(@document, Rhodium::Events)
      .enqueue(event)
      .commit(@document, Rhodium::Events)
  end
end

demo = ML.dict File.read(RESOURCES / "examples" / "f2.wwml")
welcome = ML.dict File.read(RESOURCES / "examples" / "welcome.wwml")
test = ML.dict <<-WWML
(group (h1 @count) exposes: (@count))

("" | "" () @user)
WWML

if ARGV[0]? == "host-tcp"
  start, stop = Meridium::Axis::Server.control { TCPServer.new("0.0.0.0", ARGV[1].to_i) }
  start.call
  sleep
end

docs = [] of Document
docs_lock = Mutex.new

mstep = Meridium::Step.new
alarm = -> { docs_lock.synchronize { docs.each(&.alarm) } }

# Register local termspace
local = Meridium::Tspace::InMemory.new
mstep.register(Term.of(:local), Meridium::StepSpace.new(local, alarm))

# Register user-provided remote termspaces.

seed = nil

OptionParser.parse do |parser|
  parser.banner = "Usage: soma [arguments]"

  parser.on("-s FILE", "--seed=FILE", "Uses the given file as a seed for the document (i.e. initial document)") do |file|
    source = File.read(file)
    seed = ML.term(source).as_d
  end

  parser.on("-r TSADDR", "--remote=TSADDR", "Connects to a remote termspace (e.g.: qux@tcp:0.0.0.0:9810, foo@unix:/path/to/file.sock)") do |tsaddr|
    case tsaddr
    when /(?<name>[a-z]\w*)@tcp:(?<host>\d+(?:\.\d+){3}):(?<port>\d+)/
      unless Socket::IPAddress.valid_v4?($~["host"]) && Socket::IPAddress.valid_port?($~["port"].to_i)
        STDERR.puts "invalid TSADDR #{tsaddr}"
        STDERR.puts parser
        abort
      end
      remote = Meridium::Tspace::Axis.new { TCPSocket.new($~["host"], $~["port"].to_i) }
      name = Term::Sym.new($~["name"])
    when /(?<name>[a-z]\w*)@unix:(?<path>.+)/
      remote = Meridium::Tspace::Axis.new { UNIXSocket.new($~["path"]) }
      name = Term::Sym.new($~["name"])
    else
      STDERR.puts "invalid TSADDR #{tsaddr}"
      STDERR.puts parser
      abort
    end
    at_exit { remote.disconnect }
    MT.spawn { remote.connect }
    mstep.register(Term.of(name), Meridium::StepSpace.new(remote, alarm))
  end
  parser.invalid_option do |flag|
    STDERR.puts "#{flag} is not a valid option"
    STDERR.puts parser
    abort
  end
end

seed ||= welcome
draw_chan = Channel({Term::Dict, Channel(Term::Dict)}).new
doc = Document.new("Untitled", draw_chan, mstep)
docs_lock.synchronize { docs << doc }
doc.send(Term.of(:open, seed))

frame = ML.term <<-WWML
((self window) icon: "icons/soma-256x256-white.png"
               title: "MuSoma"
               max-w: 1000
               max-h: 800
               style: "bg-neutral-900 max origin"
               .model: {mouse: (0 0), dwuir: (), concealed: false, settled: false, pan-x: 0, pan-y: 0}
  (group style: "max flow-none"
    (group style: "max flow-col gap-3 p-3 fr"
      (group style: "z-100 bg-neutral-800 w-max h-content px-2 py-1 rounded-sm"
        (p "Wirewright µsoma" style: "text-neutral-400 text-xs"))
      (^if concealed
        (group style: "w-max h-fr bg-neutral-950 border-2 border-neutral-800 rounded-sm center"
          (group style: "flow-col gap-5 min-w-lg max-w-lg"
            (group style: "w-max flow-col gap-4"
              (group style: "w-max gap-3"
                (icon "disabled_visible" style: "text-neutral-200 text-5xl")
                (p "Concealed" style: "h-max center-y leading-tight font-bold text-neutral-200 text-4xl"))
              (p style: "w-max text-neutral-300 font-normal"
                "This document is currently concealed. This means it’s running at full speed without you in the loop."))
            (group style: "border border-yellow-200 gap-2 rounded p-2 settled:border-green-200" settled: ^settled
              (p style: "px-1 py-0.5 font-mono leading-tight text-neutral-950 font-medium text-xs rounded-sm bg-yellow-200 settled:bg-green-200" settled: ^settled
                "Tab")
              (p style: "leading-tight text-yellow-200 settled:text-green-200 h-max center-y" settled: ^settled
                "Hit Tab to reveal")))))
      (^unless concealed
        (^if (= dwuir ())
          (group style: "w-max h-fr bg-neutral-800 center rounded-sm"
            (p "The document's view will appear here shortly, please wait..." style: "text-sm text-neutral-300")))
        (^unless (= dwuir ())
          ((self viewport) style: "w-max h-fr bg-neutral-900" pan-x: ^pan-x pan-y: ^pan-y id: viewport
            ((self) ^dwuir)))))))
;;    ;; Template for command palette
;;    (group style: "max center-x py-20 z-100 bg-neutral-950 opacity-80"
;;      (group style: "content min-w-lg flow-col gap-5"
;;        (group style: "w-max h-content p-5 bg-neutral-800 rounded-lg border border-blue-400"
;;          (p "Start typing to search..." style: "text-neutral-500 font-normal text-lg"))
;;        (group style: "w-max h-content p-5 bg-neutral-900 rounded-lg flow-col gap-5"
;;          (group style: "w-max h-content p-3 focused:bg-neutral-800 rounded-md flow-row fr gap-3" focused: true
;;            (group style: "w-content h-max center-y"
;;              (icon "\\u00e161" style: "text-neutral-300 text-xl")) ;; save
;;            (group style: "w-fr h-max flow-col gap-1"
;;              (p "Save" style: "text-sm font-bold text-neutral-300")
;;              (p "Saves this document on the disk." style: "text-xs text-neutral-400")))
;;          (group style: "w-max h-content p-3 focused:bg-neutral-800 rounded-md flow-row fr gap-3"
;;            (group style: "w-content h-max center-y"
;;              (icon "\\u00e89e" style: "text-neutral-300 text-xl")) ;; open_in_new
;;            (group style: "w-fr h-max flow-col gap-1"
;;              (p "Load" style: "text-sm font-bold text-neutral-300")
;;              (p "Loads a document from disk." style: "text-xs text-neutral-400"))))))))
WWML

# List, search: Save, Load
# When press save, pick directory and file. Option to create file. Option to go back.
# When press load, pick directory and file. Option to go back.

# frame = frame.morph({:".model", :dwuir, doc.dwuir})

ui = UIR::Reducers.microfold(Term.of(frame)) do |current, drawable, event|
  rerender = true

  Term.case(event) do
    matchpi %{(key tab)} do
      if concealed = frame[:".model", :concealed].true?
        doc.send(Term.of({:reveal}))
      else
        doc.send(Term.of({:conceal}))
      end

      frame = frame.morph({:".model", :concealed, !concealed})
    end

    matchpi %{(key f1)} do
      puts ML.display(drawable, style: ML::Style::Indent2)
    end

    if frame[:".model", :concealed].false?
      matchpi %{(key f2)} do
        doc.send(Term.of(:open, demo))
      end

      matchpi %{(key _symbol)}, %{(input _string)} do
        doc.send(event)
      end

      matchpi %{(mouse motion x_number y_number)} do
        frame = frame.morph({:".model", :mouse, {x, y}})

        if grip = frame[:".model", :grip]?
          gx, gy = grip
          dx = x - gx
          dy = y - gy

          frame = frame.morph(
            {:".model", :"pan-x", frame[:".model", :"pan-x"] + dx},
            {:".model", :"pan-y", frame[:".model", :"pan-y"] + dy},
            {:".model", :grip, frame[:".model", :mouse]},
          )
        elsif response = UIR.node_and_coords?(drawable) { |node| node[:id]? == Term.of(:viewport) }
          viewport, l, t = response
          relx = x - l
          rely = y - t
          if relx > 0 && rely > 0
            doc.send(Term.of(:mouse, :motion, relx - viewport[:"pan-x"], rely - viewport[:"pan-y"]))
          end
        end
      end

      matchpi %{(mouse press)} do
        if frame[:".model", :forward]? == Term[true]
          frame = frame.morph({:".model", :pressed, true})

          doc.send(event)
        else
          frame = frame.morph(
            {:".model", :grip, frame[:".model", :mouse]},
            {:cursor, :grabbing},
          )
        end
      end

      matchpi %{(mouse release)} do
        if frame[:".model", :pressed]? == Term[true]
          doc.send(event)
          frame = frame.morph({:".model", :pressed, nil})
        elsif frame[:".model", :forward]? == Term[true]
          doc.send(event)
        elsif grip = frame[:".model", :grip]?
          frame = frame.morph({:".model", :grip, nil}, {:cursor, nil})
        end
      end
    end

    matchpi %{(size w_number h_number)} do
      frame = frame.morph({:"max-w", w}, {:"max-h", h})
    end

    matchpi %{cycle} do
      rerender = true

      frame1 = frame

      # Rendezvous with the document thread. Help it draw its UIR -> dwUIR.
      # Keep a copy of dwUIR on our side to show it on the screen.
      select
      when request = draw_chan.receive
        uir, response = request
        dwuir = UIR.drawable(Term.of(uir))
        dwuir_dict = dwuir.as_d

        response.send(dwuir_dict)

        frame1 = frame1.morph({:".model", :dwuir, dwuir})
      else
      end

      frame1 = frame1.morph({:".model", :settled, doc.settled?})

      if frame.same?(frame1)
        rerender = false
      end
      frame = frame1
    end

    otherwise { }
  end

  unless frame[:".model", :grip]? || frame[:".model", :concealed].true?
    mousex, mousey = frame[:".model", :mouse]

    frame = frame.morph({:".model", :forward, nil})

    strata = UIR.strata(drawable, mousex.as_n, mousey.as_n)
    forward = strata.any? do |_, stratum|
      stratum.any? do |keypath|
        target = Keypath.follow(drawable, keypath)
        !!target[:"#backlink"]?
      end
    end

    frame = frame.morph({:".model", :forward, forward})
  end

  unless rerender
    next current
  end

  instance = Alloy.render(vars: frame[:".model"].as_d, template: Term.of(frame.without(:".model"))).as_d

  # Send instance to drawing
  Term.of(instance)
end

UIR::Platform::Current.show(ui)
