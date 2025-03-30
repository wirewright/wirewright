require "./src/wirewright"
require "./uiRb"
require "./sfuiR"
require "./pprint2"

module UIR::Platform
  alias Current = SFML
end

module DocR
  extend self

  struct Comment
    include Feature

    TEMPLATE = ML.term <<-WWML
    (box style: "content py-2"
      (box style: "w-max max-w-lg h-content pl-1 bg-neutral-700"
        (p ^desc style: "pl-3 text-neutral-500 bg-neutral-900 text-sm w-max leading-normal")))
    WWML

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{(comment lines_string+)} do
          unit = Alloy.render(Term[desc: lines.items.join('\n') { |line| line.to(String) }], TEMPLATE)

          continue unless block = DocR.block?(unit)

          postfixed(block, postfix)
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  struct Unit
    include Feature

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{[unit node_ children_+]} do
          # TODO: relax this a little bit
          continue unless Rhodium.cursordepth(term, pairspart: true) == -1

          unit = term.pairspart.transaction do |commit|
            commit << node
            commit.concat(children.items) do |child|
              Term.case(child) do
                matchpi %{_dict} { DocR.view(child.unsafe_as_d, aligned: false) }
                otherwise { child }
              end
            end
          end

          continue unless block = DocR.block?(Term.of(unit))

          postfixed(block, postfix)
        end

        matchpi %{[view @_ as _ instance_]} do
          # TODO: relax this a little bit
          continue unless Rhodium.cursordepth(term, pairspart: true) == -1
          continue unless block = DocR.block?(instance)

          postfixed(block, postfix)
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  struct Button
    include Feature

    TEMPLATE = ML.term <<-WWML
    (button ^caption id: ^id style: ^style hover: false)
    WWML

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi(
          %{(button caption_ to @_ (_*) ¦ _ style⋮ "" id_)},
          %{(button caption_ as _ to @_ (_*) ¦ _ style⋮ "" id_)},
          %{(button caption_ to @_ (_*) waiting @_ ¦ _ style⋮ "" id_)},
          %{(button caption_ as _ to @_ (_*) waiting @_ ¦ _ style⋮ "" id_)},
        ) do |caption|
          continue unless Rhodium.cursordepth(term, pairspart: true) == -1

          if caption.type.dict?
            # TODO: use pretty print with forced inline
            caption = Term.of(ML.display(caption, endl: false).gsub(/\s+/, ' '))
          end

          unit = Alloy.render(Term[id: id, caption: caption, style: style], TEMPLATE)

          continue unless block = DocR.block?(unit)

          postfixed(block, postfix)
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  struct Cursor
    include Feature

    CURSOR_TEMPLATE = ML.term <<-WWML
    (group style: "content"
      (code ^lhs style: "text-neutral-400 bg-neutral-700 ring")
      ((self rect) style: "w-px h-max bg-blue-500")
      (code ^rhs style: "text-neutral-400 bg-neutral-700 ring"))
    WWML

    SUGGESTION_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-10 dt-8 border border-neutral-600 bg-neutral-800 rounded p-2"
      (group style: "min-w-sm h-content flow-col gap-2"
        (p ^name style: "w-inherit px-2 py-1 font-mono bg-neutral-700 text-neutral-200 font-medium rounded-sm")
        (p ^intro style: "w-inherit px-2 pb-1 text-sm text-neutral-300")))
    WWML

    # TODO: use a template `if`
    SUGGESTION_MEMBER_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-10 dt-8 border border-neutral-600 bg-neutral-800 rounded p-2"
      (group style: "min-w-sm h-content flow-col gap-2"
        (box ^head style: "w-max h-content px-2 py-1 font-mono bg-neutral-700 text-neutral-200 font-medium rounded-sm")
        (box ^body style: "w-inherit h-content px-2 pb-1 text-sm text-neutral-300")))
    WWML

    SUGGESTION_MEMBER_ONEOF_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-10 dt-8 border border-neutral-600 bg-neutral-800 rounded p-2"
      (group style: "content flow-col gap-2"
        (group style: "w-max fr h-content flow-row gap-2 px-2 py-1 bg-neutral-700 rounded-sm"
          ;; FIXME: why can't we center vertically here?
          (p style: "text-sm text-neutral-200"
            "↑" ^current "/" ^total "↓")
          (box ^head style: "w-fr h-content font-mono text-neutral-200 font-medium"))
        (box ^body style: "content px-2 pb-1 text-sm text-neutral-300")))
    WWML

    # TODO: use a template `if`
    SUGGESTION_LIST_NOSCROLL_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-10 dt-8 border border-neutral-600 bg-neutral-800 rounded p-3"
      (main style: "min-w-sm h-content flow-col gap-3"
        (header style: "w-inherit h-content flow-row font-sans font-normal text-xs text-neutral-300"
          "Showing " ^begin ".." ^end " out of " ^total)
        (list style: "w-inherit h-content flow-col text-sm font-mono font-text text-neutral-200 gap-2"
          (^*part names 0 ..= -1))))
    WWML

    SUGGESTION_LIST_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-10 dt-8 border border-neutral-600 bg-neutral-800 rounded p-3"
      (main style: "min-w-sm h-content flow-col gap-3"
        (header style: "w-inherit h-content flow-row font-sans font-normal text-xs text-neutral-300"
          "Showing " ^begin ".." ^end " out of " ^total)
        (group style: "w-inherit h-content flow-row fr"
          (list style: "w-fr h-content flow-col text-sm font-mono font-text text-neutral-200 gap-2"
            (^*part names 0 ..= -1))
          (scroll style: "w-2 h-max pr-1"
            ((self translate) y: (* ^scroll-offset) style: "max"
              ((self rect/outline) style: "max border bg-neutral-600 rounded-sm" max-h: (* ^scroll-height)))))))
    WWML

    # FIXME: this does not belong here
    # TODO: ideally we should render markdown here, not this.
    # TODO: text wrapping, w-max
    def self.highlighted(string : String)
      Term::Dict.build do |col|
        col << :group
        col.with(:style, "content flow-col")

        string.each_line(chomp: true) do |line|
          bold = false

          row = Term::Dict.build do |commit|
            commit << :line
            commit.with(:style, "content flow-row")

            line.split('*') do |frag|
              next if frag.empty?

              if bold
                commit << Term.of(:p, frag, style: "text-sm font-bold text-blue-500")
              else
                commit << Term.of(:p, frag, style: "text-sm text-neutral-300")
              end
            ensure
              bold = !bold
            end

            # Empty line
            if commit.itemsize == 1
              # Insert something to have some height
              commit << Term.of(:p, "", style: "text-xs")
            end
          end

          col << row
        end
      end
    end

    # TODO: gosh gosh gosh refactor this!!!
    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions: (suggestions/list () ((name_string intro_string)) ()))} do
          cursor_unit = Alloy.render(Term[lhs: lhs, rhs: rhs], CURSOR_TEMPLATE)
          suggestion_unit = Alloy.render(Term[name: name, intro: intro], SUGGESTION_TEMPLATE)

          unit = Term.of(:group, cursor_unit, suggestion_unit, style: "content flow-none")

          continue unless block = DocR.block?(unit)

          postfixed(block, postfix)
        end

        matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions: (suggestions/list above←(_*) visible←((%past (_string _string) min: 1)) below←(_*)))} do
          cursor_unit = Alloy.render(Term[lhs: lhs, rhs: rhs], CURSOR_TEMPLATE)

          b = above.size
          e = b + visible.size
          total = above.size + visible.size + below.size
          names = visible.items.map { |(name, _)| name }

          if total == visible.size
            suggestion_vars = Term[begin: b, end: e, total: total, names: names]
            suggestion_unit = Alloy.render(suggestion_vars, SUGGESTION_LIST_NOSCROLL_TEMPLATE)
          else
            suggestion_vars = Term[
              begin: b,
              end: e,
              total: total,
              names: names,
              "scroll-offset": b / total,
              "scroll-height": visible.size / total,
            ]
            suggestion_unit = Alloy.render(suggestion_vars, SUGGESTION_LIST_TEMPLATE)
          end

          unit = Term.of(:group, cursor_unit, suggestion_unit, style: "content flow-none")

          continue unless block = DocR.block?(unit)

          postfixed(block, postfix)
        end

        matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions_: (suggestions/group prefix←(_*) suffix←((head_string body_string) _*)))} do
          cursor_unit = Alloy.render(Term[lhs: lhs, rhs: rhs], CURSOR_TEMPLATE)

          if prefix.empty? && suffix.size == 1
            suggestion_unit = Alloy.render(Term[
              head: Cursor.highlighted(head.to(String)),
              body: Cursor.highlighted(body.to(String)),
            ], SUGGESTION_MEMBER_TEMPLATE)
          else
            suggestion_unit = Alloy.render(Term[
              head: Cursor.highlighted(head.to(String)),
              body: Cursor.highlighted(body.to(String)),
              current: prefix.size + 1, # start from one
              total: prefix.size + suffix.size,
            ], SUGGESTION_MEMBER_ONEOF_TEMPLATE)
          end

          unit = Term.of(:group, cursor_unit, suggestion_unit, style: "content flow-none")

          continue unless block = DocR.block?(unit)

          postfixed(block, postfix)
        end

        matchpi %{[lhs_string | rhs_string (_*) @user]} do
          unit = Alloy.render(Term[lhs: lhs, rhs: rhs], CURSOR_TEMPLATE)

          continue unless block = DocR.block?(unit)

          postfixed(block, postfix)
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  # Renders H1-H6 and P text nodes.
  struct TextNode
    include Feature

    def initialize(@document : Term::Dict)
    end

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{[name←(%any h1 h2 h3 h4 h5 h6 text codebox) content_]} do
          continue unless Rhodium.cursordepth(term, pairspart: true) == -1

          node = term

          # TODO: Remove and make text -> p once we have smarter keypath-based rendering.
          # Currently using p messes up the UI sometimes (e.g. interpreted as P when
          # it must be interpreted as code).
          if name == Term[:text]
            node = Term.of(node.morph({0, :p}))
          end

          if ML.edge?(content)
            # If a cell changes, the visible part of the document inevitably changes
            # as well, since the cell primarily stores the data there; and Rhodium::Cells
            # is only used for caching/secondary access, like we do here. Thus a rerender
            # of associated h1-6/p will inevitably be triggered.
            continue unless value = @document[Rhodium::Cells, content]?

            node = Term.of(node.morph({1, value}))
          end

          # If content is not a string Microfold will take care of it and
          # convert it to string!

          continue unless block = DocR.block?(node)

          postfixed(block, postfix)
        end

        matchpi %{(hr ¦ pairspart_ style⋮ "")} do
          continue unless Rhodium.cursordepth(term, pairspart: true) == -1

          node = Term.of(
            pairspart.morph(
              {0, {:self, :rect}},
              {:style, Term[Microfold::SPEC[:defaults, :hr]?.try(&.as_s?) || ""].stitch(" ").stitch(style)},
            )
          )

          continue unless block = DocR.block?(node)

          postfixed(block, postfix)
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  def visible(document : Term::Dict) : Term::Dict
    D7.visible(document)
  end

  def annotated(document document0 : Term::Dict) : Term::Dict
    nodepath = Stack(Int32).new

    document1 = document0

    while Rhodium.successor?(document0, nodepath)
      node0 = Rhodium.follow(document0, nodepath)
      node1 = node0

      # TODO: extract into identity
      Term.case(node0) do
        matchpi(
          %{[button caption_ to @edge_ (_*)]},
          %{[button caption_ to @edge_ (_*) waiting @_]},
        ) do
          continue unless Rhodium.cursordepth(node0, pairspart: true) == -1

          node1 = Term.of(node0.morph({:id, {caption.hash, caption.hash, edge.hash}.hash}))
        end

        matchpi(
          %{[button caption_ as value_ to @edge_ (_*)]},
          %{[button caption_ as value_ to @edge_ (_*) waiting @_]},
        ) do
          continue unless Rhodium.cursordepth(node0, pairspart: true) == -1

          node1 = Term.of(node0.morph({:id, {caption.hash, value.hash, edge.hash}.hash}))
        end

        otherwise { }
      end

      next if node0.same?(node1)

      document1 = Rhodium.assign(document1, nodepath, node1)
    end

    document1
  end

  def ref(document document0 : Term::Dict, id : Term, & : Term -> Term)
    nodepath = Stack(Int32).new

    document1 = document0

    while Rhodium.successor?(document0, nodepath)
      node0 = Rhodium.follow(document0, nodepath)
      node1 = node0

      # TODO: extract into identity
      Term.case(node0) do
        matchpi(
          %{[button caption_ to @edge_ (_*)]},
          %{[button caption_ to @edge_ (_*) waiting @_]},
        ) do
          next unless Term.of({caption.hash, caption.hash, edge.hash}.hash) == id

          node1 = yield node0
        end

        matchpi(
          %{[button caption_ as value_ to @edge_ (_*)]},
          %{[button caption_ as value_ to @edge_ (_*) waiting @_]},
        ) do
          next unless Term.of({caption.hash, value.hash, edge.hash}.hash) == id

          node1 = yield node0
        end

        otherwise { }
      end

      next if node0.same?(node1)

      document1 = Rhodium.assign(document1, nodepath, node1)
    end

    document1
  end

  def pptree(document : Term::Dict, *, aligned : Bool = true) : Term
    ppin = pipe(document, visible, annotated)

    chain = ML::Display::MAIN_CHAIN.prepend(Button.new, Comment.new, Cursor.new, Unit.new, TextNode.new(document))

    ctx = DisplayContext.new(60, 120, features: chain)
    # FIXME: handle empty document
    if aligned && !ppin.empty?
      tree = LayoutSet::All.thunk(Term.of(ppin), "", aligned ? LayoutSet::DictAligned : LayoutSet::All)
    else
      tree = chain.call(ctx, Term.of(ppin), "")
    end
    flat, _ = flatten(ctx, tree)
    flat
  end

  private def ugroup(children : Term::Dict, style : String = "") : Term
    ugroup = Term::Dict.build do |commit|
      commit << :group
      commit.with(:style, style) unless style.empty?
      commit.concat(children.items) { |child| unit(child) }
    end

    Term.of(ugroup)
  end

  # Converts the given pretty-print tree *pptree* into a Microfold unit.
  def unit(pptree : Term, style = "") : Term
    Term.case(pptree) do
      matchpi %{(frag content_string)} do
        Term.of(:code, content, style: "#{style} text-neutral-400")
      end

      matchpi %{(frag content_string ¦ tag: symbol)} do
        Term.of(:code, content, style: "#{style} text-neutral-200")
      end

      matchpi %{(frag content_string ¦ tag: number)} do
        Term.of(:code, content, style: "#{style} text-violet-400")
      end

      matchpi %{(frag content_string ¦ tag: string)} do
        Term.of(:code, content, style: "#{style} text-yellow-600")
      end

      matchpi %{(frag content_string ¦ tag: boolean)} do
        Term.of(:code, content, style: "#{style} text-orange-600")
      end

      matchpi %{(frag content_string ¦ tag: edge)} do
        Term.of(:code, content, style: "#{style} text-green-400")
      end

      matchpi %{(indented child_ by: n←(%number (whole _)))} do
        unit(child, style: "#{style} content pl-#{n}")
      end

      matchpi %{(row children_+)} do
        ugroup(children.unsafe_as_d, "#{style} content flow-row")
      end

      matchpi %{(col children_+)} do
        ugroup(children.unsafe_as_d, "#{style} content gap-1 flow-col")
      end

      matchpi %{(row children_+ ¦ gap_: (%number (whole _) > 0))} do
        ugroup(children.unsafe_as_d, "#{style} content gap-#{gap*2} flow-row")
      end

      matchpi %{(col children_+ ¦ gap_: (%number (whole _) > 0))} do
        ugroup(children.unsafe_as_d, "#{style} content gap-#{gap} flow-col")
      end

      matchpi %{[block subunit_]}, %{[block/floating subunit_]} do
        subunit
      end
    end
  end

  def block?(unit : Term) : Term?
    rem = Term[16] # ?!

    uir = Microfold.uir(Microfold::SPEC, unit, rem: rem)
    drawable = rewrite(uir, UIR.rewriter)

    Term.case(drawable) do
      # If we know its size outside of UI context
      matchpi %[{¦ final-w: w←(%number i32) final-h: h←(%number i32)}] do
        if w.natural_nonzero? && h.natural_nonzero?
          # If both are valid
          Term.of(:block, unit, w: w//rem, h: h//rem)
        else
          # If they're invalid then we fall back to code.
        end
      end

      # If we do not know its size, leave it to the UI context.
      #
      # TODO: UI breaks if we do not know the size within the UI context as well,
      # we must have some kind of backtracking to return to this decision point
      # in that case!!!
      otherwise do
        Term.of(:"block/floating", unit)
      end
    end
  end

  def view(document : Term::Dict, *, aligned : Bool = true) : Term
    pipe(document, DocR.pptree(aligned: aligned), DocR.unit)
  end
end

class Document
  BLANK = ML.term <<-WWML
  (group style: "max center bg-neutral-800"
    (p "The view of the document is loading..." style: "text-neutral-300"))
  WWML

  # Used to generate document ids.
  @@counter = Atomic(UInt32).new(0u32)

  def initialize
    @view = Atomic(Term::Dict).new(BLANK.as_d)
    @mailbox = Channel(Term).new(128)

    # The following instance variables are owned exclusively by the document
    # thread. No one else must know they exist.
    @nitrene = Nitrene::JobContext.new
    @document = Term[]
    @initial = true
    @state = State::Clean

    # Finally, spin up the document thread.
    mt = ExecutionContext::MultiThreaded.new("Document #{@@counter.add(1, :relaxed)}", 1)
    mt.spawn { mainloop }
  end

  # Returns the latest view of this document.
  def view : Term::Dict
    @view.get(:relaxed)
  end

  # Adds *prompt* to this document's mailbox.
  def send(prompt : Term) : Nil
    @mailbox.send(prompt)
  end

  # Hosts the main loop run by the document thread.
  #
  # The main loop "runs the physics": advances the document step-by-step.
  # These advancements are interleaved with `rendezvous`.
  #
  # Once the document has settled the mainloop waits (`wait`) for further prompts.
  private def mainloop : Nil
    settled = false

    while true
      draw

      if settled
        wait

        settled = false
      end

      @document = D7.run(@document,
        log: D7::Log::None.new,
        transition: Rhodium.transition,
        step: D7.steps(rendezvous, Rhodium.step, Nitrene.step(@nitrene)),
        goal: D7::Goal.none,
        initial: @initial,
      )

      @initial = false
      settled = true
    end
  end

  # Rendezvous step assesses and modifies the state of the document. It enhances
  # the document based on prompts (`peek`), and decides whether the document should
  # be drawn.
  private def rendezvous : D7::Step
    D7::Step.new do |document|
      @document = document

      peek

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

      # Trigger a transition if the itemsparts are different. Peek may
      # override the document (perhaps even entirely!)
      {@document, @document.itemspart != document.itemspart}
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
    drawable = DocR.view(@document)

    @view.set(drawable.as_d, :relaxed)
  end

  # Blocks until a prompt arrives to this document's mailbox. Handles that prompt.
  #
  # Raises `Channel::ClosedError` if the mailbox is closed while waiting.
  private def wait : Nil
    select
    when prompt = @mailbox.receive
    when @nitrene.alarm.receive
      prompt = Term.of(:"job-completed")
    end

    handle(prompt)
  end

  # Checks the mailbox for new prompts. If none, returns immediately. If some,
  # handles the front prompt.
  #
  # Raises `Channel::ClosedError` if the mailbox is closed while waiting.
  private def peek : Nil
    select
    when prompt = @mailbox.receive
      handle(prompt)
    else
    end
  end

  # Handles the given *prompt*. We call events directed toward the document *prompts*
  # to avoid confusion (e.g. relative to UI events in general). This method is the main
  # dispatch point for prompts.
  private def handle(prompt : Term) : Nil
    Term.case(prompt) do
      matchpi %{(open seed_dict)} { open(seed.unsafe_as_d) }
      matchpi %{(key _)}, %{(input _string)} { edit(prompt) }
      matchpi %{(click id_)} { click(id) }
      otherwise { }
    end
  end

  # Replaces the document with a new *seed*.
  private def open(seed : Term::Dict) : Nil
    @document = seed
    # This is needed if we're currently `wait`ing. If `peek` is the caller
    # it doesn't care about initial/not because it'll trigger a transition
    # anyway -- due to document change. With wait it's another story.
    @initial = true
  end

  # Enqueues `(edit @user motion_)` event onto the document's queue.
  private def edit(motion : Term) : Nil
    @document = Rhodium::Q.of(@document, Rhodium::Events)
      .enqueue(Term.of(:edit, {:edge, :user}, motion))
      .commit(@document, Rhodium::Events)
  end

  # Clicks on an element with the given *id*.
  #
  # *id* is usually a big number, the hash of the relevant pieces of identity
  # of the target element.
  #
  # See also: `DocR.ref`.
  private def click(id : Term) : Nil
    @document = DocR.ref(@document, id) do |target|
      Term.of_case(target) do
        matchpi(
          %{[button _ to @_ (_*)]},
          %{[button _ to @_ (_*) waiting @_]},
        ) { target.morph({4, Tail, {:press}}) }

        matchpi(
          %{[button caption_ as value_ to @edge_ (_*)]},
          %{[button caption_ as value_ to @edge_ (_*) waiting @_]},
        ) { target.morph({6, Tail, {:press}}) }
      end
    end
  end
end

module Frame
  extend self

  def set(frame : Term::Dict, id, attr, value) : Term::Dict
    map(frame, id, &.with(attr, value))
  end

  def setchild(frame : Term::Dict, id, child) : Term::Dict
    set(frame, id, 1, child)
  end

  def map(frame : Term::Dict, id : Term, &fn : Term::Dict -> Term::Dict) : Term::Dict
    Keypath.each_item(Term.of(frame)) do |keypath, item|
      next unless item = item.as_d?
      next unless id == item[:id]?

      frame = Keypath.assign(Term.of(frame), keypath, Term.of(fn.call(item))).as_d

      true # continue
    end

    frame
  end

  def map(frame : Term::Dict, id, &fn : Term::Dict -> Term::Dict) : Term::Dict
    map(frame, Term.of(id), &fn)
  end
end

demo = ML.dict <<-WWML
(h1 "Heading 1")
(h2 "Heading 2")
(h3 "Heading 3")
(h4 "Heading 4")
(h5 "Heading 5")
(h6 "Heading 6")
(text "Simple text")
(hr style: "min-w-lg")
(text "Show counter using text:")
(hr style: "min-w-lg")
(text @count style: "text-xl font-bold text-green-500")
(hr style: "min-w-lg")

(unit group style: "content flow-col gap-5 bg-neutral-800 p-5"
  (unit group style: "w-max h-content center-x"
    (view @count-envs as (p ^count style: "text-7xl font-bold text-neutral-100")))
  (unit group style: "content flow-row gap-5"
    (button "Increment" as 1 to @deltas ())
    (button "Decrement" as -1 to @deltas ())))

(transform @counts to @count-envs {count: _})
(initial @count to @counts)

(comment "Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.")
(cell 0 @count)
(transform (@deltas delta_number) to @counts with @count (+ count delta))
(latest @counts @count)
("" | "" () @user)
WWML

seed = ML.dict <<-WWML
;; (comment
;;   "Welcome to µsoma, a GUI for Wirewright"
;;   ""
;;   "µsoma to Wirewright is roughly what a web browser is to the Internet."
;;   ""
;;   "You're looking at a *self-embodied program*. Well, sort of — it only contains one comment right now. Hit left/right arrow to see for yourself. Or type `;;` and write your own!"
;;   ""
;;   "Try typing the following:"
;;   ""
;;   "  (cell 0 @count)"
;;   "  (button \\"Increment\\" as 1 to @deltas ())"
;;   "  (button \\"Decrement\\" as -1 to @deltas ())"
;;   "  (transform @deltas to @counts with @count (+ count _))"
;;   "  (latest @counts @count)"
;;   ""
;;   "Click on the buttons and see what happens! :^)"
;;   ""
;;   "- Drag on empty/non-clickable space to pan around if something overflows."
;;   "- Hit Enter to escape from a pair."
;;   "- Hit F2 to replace this document with a more sophisticated demo."
;;   "- Hit Ctrl-Backspace to remove this comment (and any *node* before the cursor in general)."
;;   "- Play! The semi-readable implementation of this editor is in `editor.soma.wwml`; check it out for key bindings & what they do"
;;   "- Take a look at D7 tests: `delta7.test.wwml`. Plenty of examples there.")

("" | "" () @user)

WWML

# libsfml
# gmp
# pcre2
# freetype
#   harfbuzz
#   graphite2
#   brotli
# x11
# xrandr
# xcursor
# xi
# udev
# opengl
# flac
# ogg
# vorbis
# vorbisenc
# vorbisfile
# pthread

doc = Document.new
doc.send(Term.of(:open, seed))

frame0 = ML.term <<-WWML
((self window) style: "bg-neutral-900 max origin" max-w: 1000 max-h: 800 mouse: (0 0)
  (group style: "max flow-none"
    (group style: "max flow-col gap-3 p-3 fr"
      (group style: "bg-neutral-800 w-max h-content px-2 py-1 rounded-sm"
        (p "Wirewright µsoma" style: "text-neutral-400 text-xs"))
      ((self viewport) style: "w-max h-fr bg-neutral-900" x: 0 y: 0 id: viewport
        (group style: "max" id: view)))))
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

frame0 = Frame.setchild(frame0.as_d, :view, doc.view)

ui = UIR::Reducers.microfold(Term.of(frame0)) do |inframe, drawable, event|
  frame = inframe.as_d

  Term.case(event) do
    matchpi %{(key f1)} do
      puts ML.display(doc.view, style: ML::Style::Indent2)
      puts ML.display(drawable, style: ML::Style::Indent2)
    end

    matchpi %{(key f2)} do
      doc.send(Term.of(:open, demo))
    end

    matchpi %{(key _symbol)}, %{(input _string)} do
      doc.send(event)
    end

    matchpi %{(mouse motion x_number y_number)} do
      frame = frame.morph({:mouse, {x, y}})

      if grip = frame[:grip]?
        gx, gy = grip
        dx = gx - x
        dy = gy - y

        frame = Frame.map(frame, :viewport) do |viewport|
          viewport.morph({:x, viewport[:x] + dx}, {:y, viewport[:y] + dy})
        end

        frame = frame.morph({:grip, frame[:mouse]})
      end
    end

    matchpi %{(mouse press)} do
      if hovered = frame[:hovered]?
        frame = frame.morph({:active, hovered})
      else
        frame = frame.morph({:grip, frame[:mouse]}, {:cursor, :grabbing})
      end
    end

    matchpi %{(mouse release)} do
      if (active = frame[:active]?) && (frame[:active]? == frame[:hovered]?)
        doc.send(Term.of(:click, active))
      elsif grip = frame[:grip]?
        frame = frame.morph({:grip, nil}, {:cursor, nil})
      end
    end

    matchpi %{(size w_number h_number)} do
      frame = frame.morph({:"max-w", w}, {:"max-h", h})
    end

    matchpi %{cycle} do
      frame = Frame.setchild(frame, :view, doc.view)
    end

    otherwise { }
  end

  # Unhover
  if hovered = frame[:hovered]?
    frame = Frame.map(frame, hovered, &.morph({:hover, false}))
    frame = frame.morph({:hovered, nil})
  end

  # Hover
  UIR.hit(drawable, frame[:mouse, 0].as_n, frame[:mouse, 1].as_n) do |keypath|
    target = Keypath.follow(drawable, keypath)
    next unless target[:hover]?
    next unless id = target[:id]?

    frame = Frame.map(frame, id, &.morph({:hover, true}))
    frame = frame.morph({:hovered, id})
  end

  Term.of(frame)
end

UIR::Platform::Current.show(ui)

