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
                matchpi %{_dict} do
                  DocR.unit(DocR.pptree(Term.of(child), toplevel: false))
                end

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
    (button ^caption id: ^id hover: false)
    WWML

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi(
          %{(button caption_ to @_ (_*) ¦ () id_)},
          %{(button caption_ as _ to @_ (_*) ¦ () id_)},
          %{(button caption_ to @_ (_*) waiting @_ ¦ () id_)},
          %{(button caption_ as _ to @_ (_*) waiting @_ ¦ () id_)},
        ) do |caption|
          continue unless Rhodium.cursordepth(term, pairspart: true) == -1

          if caption.type.dict?
            # TODO: use pretty print with forced inline
            caption = Term.of(ML.display(caption, endl: false).gsub(/\s+/, ' '))
          end

          unit = Alloy.render(Term[id: id, caption: caption], TEMPLATE)

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
      (group style: "w-max h-content min-w-sm max-w-sm flow-col gap-2"
        (p ^name style: "w-max px-2 py-1 font-mono bg-neutral-700 text-neutral-200 font-medium rounded-sm")
        (p ^intro style: "w-max px-2 pb-1 text-sm text-neutral-300")))
    WWML

    # TODO: use a template `if`
    SUGGESTION_MEMBER_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-10 dt-8 border border-neutral-600 bg-neutral-800 rounded p-2"
      (group style: "content min-w-sm max-w-sm flow-col gap-2"
        (box ^head style: "w-max h-content px-2 py-1 font-mono bg-neutral-700 text-neutral-200 font-medium rounded-sm")
        (box ^body style: "content px-2 pb-1 text-sm text-neutral-300")))
    WWML

    SUGGESTION_MEMBER_ONEOF_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-10 dt-8 border border-neutral-600 bg-neutral-800 rounded p-2"
      (group style: "content min-w-sm max-w-sm flow-col gap-2"
        (group style: "w-max fr h-content flow-row gap-2 px-2 py-1 bg-neutral-700 rounded-sm"
          ;; FIXME: why can't we center vertically here?
          (p style: "text-xs text-neutral-200"
            "↑" ^current "/" ^total "↓")
          (box ^head style: "w-fr h-content font-mono text-neutral-200 font-medium"))
        (box ^body style: "content px-2 pb-1 text-sm text-neutral-300")))
    WWML

    # TODO: use a template `if`
    SUGGESTION_LIST_NOSCROLL_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-10 dt-8 border border-neutral-600 bg-neutral-800 rounded p-3"
      (main style: "w-max h-content min-w-sm max-w-sm flow-col gap-3"
        (header style: "w-max h-content flow-row font-sans font-normal text-xs text-neutral-300"
          "Showing " ^begin ".." ^end " out of " ^total)
        (list style: "w-max h-content flow-col text-sm font-mono font-text text-neutral-200 gap-2"
          (^*part names 0 ..= -1))))
    WWML

    SUGGESTION_LIST_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-10 dt-8 border border-neutral-600 bg-neutral-800 rounded p-3"
      (main style: "w-max h-content min-w-sm max-w-sm flow-col gap-3"
        (header style: "w-max h-content flow-row font-sans font-normal text-xs text-neutral-300"
          "Showing " ^begin ".." ^end " out of " ^total)
        (group style: "w-max h-content flow-row fr"
          (list style: "w-fr h-content flow-col text-sm font-mono font-text text-neutral-200 gap-2"
            (^*part names 0 ..= -1))
          (scroll style: "w-3 h-max pr-1"
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

  # Returns the pretty-print tree for *document* of a fragment thereof.
  #
  # *toplevel* specifies whether *term* is the toplevel document.
  #
  # FIXME: only accept Term::Dict!!
  def pptree(document : Term, *, toplevel : Bool = true) : Term
    ppin = document

    if document = document.as_d?
      ppin = pipe(document, visible, annotated)
    end

    chain = ML::Display::MAIN_CHAIN
      .prepend(Button.new)
      .prepend(Comment.new)
      .prepend(Cursor.new)
      .prepend(Unit.new)

    ctx = DisplayContext.new(60, 120, features: chain)
    if toplevel
      tree = LayoutSet::All.thunk(Term.of(ppin), "", toplevel ? LayoutSet::DictAligned : LayoutSet::All)
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
      matchpi %[{¦ final-w: w←(%number +i32) final-h: h←(%number +i32)}] do
        Term.of(:block, unit, w: w//rem, h: h//rem)
      end

      otherwise do
        Term.of(:"block/floating", unit)
      end
    end
  rescue Microfold::UnitError
    # Note how we do not provide the floating backup here. Doing so would simply
    # cause an explosion higher up -- the unit is malformed, period; we will show
    # it as code instead.
  end
end

def find_by_id(tree : Term::Dict, id, &fn : Term -> Term)
  id = Term.of(id)

  Keypath.each_item(tree.upcast) do |keypath, item0|
    next unless id == item0[:id]?

    item1 = fn.call(item0)

    tree = tree.follow(keypath) { item1 }.as_d

    false # break
  end

  tree
end

blank = ML.term <<-WWML
(group (p "Loading..." style: "text-neutral-300") style: "max center bg-neutral-800")
WWML

view = Atomic(Term::Dict).new(blank.as_d)
events = Channel(Term).new(128)

class LoadExample < Exception
end

d7ctx = ExecutionContext::MultiThreaded.new("D7", 1)
d7ctx.spawn do
  demo = ML.terms <<-WWML
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

  document = ML.terms <<-WWML
  (comment
    "Welcome to µsoma, a GUI for Wirewright"
    ""
    "µsoma to Wirewright is roughly what a web browser is to the Internet."
    ""
    "You're looking at a *self-embodied program*. Well, sort of — it only contains one comment right now. Hit left/right arrow to see for yourself. Or type `;;` and write your own!"
    ""
    "Try typing the following:"
    ""
    "  (cell 0 @count)"
    "  (button \\"Increment\\" as 1 to @deltas ())"
    "  (button \\"Decrement\\" as -1 to @deltas ())"
    "  (transform @deltas to @counts with @count (+ count _))"
    "  (latest @counts @count)"
    ""
    "Click on the buttons and see what happens! :^)"
    ""
    "- Drag on empty/non-clickable space to pan around if something overflows."
    "- Hit Enter to escape from a pair."
    "- Hit F2 to replace this document with a more sophisticated demo."
    "- Hit Ctrl-Backspace to remove this comment (and any *node* before the cursor in general)."
    "- Play! The sem-readable implementation of this editor is in `editor.soma.wwml`; check it out for key bindings & what they do"
    "- Take a look at D7 tests: `delta7.test.wwml`. Plenty of examples there.")

  ("" | "" () @user)

  WWML

  # TODO: figure out what's going on here (esp. with should_draw) and refactor
  # into something comprehendible. This is madness.
  #
  # the idea with should_draw is to draw here:
  #
  #  <BEGIN OF CYCLE> E1 E2 E3 ... <END OF CYCLE>
  #                                -------------
  #                                        draw
  #
  # but then:
  #                                                   provide suggestions (maybe)
  #                                                     vvvv
  #  <BEGIN OF CYCLE> E1 E2 (edit @user ...) E3 ... <END OF CYCLE> <BEGIN OF CYCLE> E1 E2 E3 ... <END OF CYCLE>
  #                                                                                              -------------
  #                                                                                                   draw
  # => then we have no flickering of suggestions.
  #
  # or obviously before polling we force a draw!

  initial = true
  settled = false

  docframe0 = DocR.unit(DocR.pptree(document))
  docframe0 = docframe0.morph({:document, document})

  should_draw = false
  previously_drawn_doc = nil
  show = ->do
    return unless should_draw

    doc = docframe0[:document].as_d

    # Do not overdraw
    doc_to_draw = DocR.visible(doc)
    if previously_drawn_doc == doc_to_draw
      return
    end

    docframe0 = DocR.unit(DocR.pptree(Term.of(doc)))
    docframe0 = docframe0.morph({:document, doc})

    previously_drawn_doc = doc_to_draw

    view.set(docframe0.without(:document), :relaxed)
  end

  handle = ->(event : Term) do
    Term.case(event) do
      matchpi %{(key f2)} do
        raise LoadExample.new # duh...
      end

      matchpi %{(key _)}, %{(input _string)} do
        document0 = docframe0[:document].as_d
        document1 = Rhodium::Q.of(document0, Rhodium::Events)
          .enqueue(Term.of(:edit, {:edge, :user}, event))
          .commit(document0, Rhodium::Events)

        docframe0 = docframe0.morph({:document, document1})
      end

      matchpi %{(click id_)} do
        document1 = DocR.ref(docframe0[:document].as_d, id) do |node|
          Term.case(node) do
            matchpi(
              %{[button _ to @_ (_*)]},
              %{[button _ to @_ (_*) waiting @_]},
            ) do
              Term.of(node.morph({4, Tail, {:press}}))
            end

            matchpi(
              %{[button caption_ as value_ to @edge_ (_*)]},
              %{[button caption_ as value_ to @edge_ (_*) waiting @_]},
            ) do
              Term.of(node.morph({6, Tail, {:press}}))
            end
          end
        end
        docframe0 = docframe0.morph({:document, document1})
      end

      otherwise { }
    end
  end

  peek = ->do
    select
    when event = events.receive
      handle.call(event)
    else
    end
  end

  wait = ->do
    handle.call(events.receive)
    show.call
  end

  edited = false

  check_should_draw = D7::Step.new do |document|
    queue = Rhodium::Q.of(document, Rhodium::Events)

    if e = queue.first?
      Term.case(e) do
        matchpi %{(edit @user _)} { edited = true }
        otherwise { }
      end
    else
      # cycle
      should_draw = !edited
      edited = false
    end

    # We do not modify the document and therefore we never trigger
    # a transition.
    {document, false}
  end

  cycle = D7::Step.new do |document|
    docframe0 = docframe0.morph({:document, document})

    peek.call
    show.call

    # We do not modify the document and therefore never need to trigger
    # a transition.
    {docframe0[:document].as_d, false}
  end

  nitrene = Nitrene::JobContext.new

  ctx0 = ExecutionContext::MultiThreaded.new("Nitrene Alarm", 1)
  ctx0.spawn do
    while true
      nitrene.alarm.receive
      events.send(Term.of(:"job-completed"))
    end
  rescue Channel::ClosedError
    # Noop. We just stop polling.
  end

  while true
    begin
      should_draw = true

      show.call

      should_draw = false

      while settled
        wait.call

        settled = false
      end

      seed0 = docframe0[:document].as_d
      seed1 = D7.run(seed0,
        log: D7::Log::None.new,
        transition: Rhodium.transition,
        step: D7.steps(check_should_draw, Rhodium.step, Nitrene.step(nitrene), cycle),
        goal: D7::Goal.none,
        initial: initial,
      )

      docframe0 = docframe0.morph({:document, seed1})

      initial = false
      settled = true
    rescue LoadExample
      # reset
      initial = true
      settled = false
      docframe0 = docframe0.morph({:document, demo})
    end
  end
end

frame0 = ML.term <<-WWML
((self window) style: "bg-neutral-900 max origin" max-w: 1000 max-h: 800
  (group style: "max flow-col gap-3 p-3 fr"
    (group style: "bg-neutral-800 w-max h-content px-2 py-1 rounded-sm"
      (p "Wirewright µsoma" style: "text-neutral-400 text-xs"))
    ((self viewport) style: "w-max h-fr bg-neutral-900" x: 0 y: 0 id: viewport
      (group style: "max" id: view))))
WWML

# TODO: remove in favor of frame.getById(view)
visible = Term[]

# TODO: move to frame.mouseX, frame.mouseY
mouse_x = Term[0]
mouse_y = Term[0]

ui = UIR::Reducers.microfold(frame0) do |frame, drawable, event|
  changed = false

  Term.case(event) do
    matchpi %{(key f1)} do
      puts ML.display(drawable, style: ML::Style::Indent2)
    end

    matchpi %{open}, %{(key _)}, %{(input _string)} do
      events.send(event)
    end

    matchpi %{(mouse motion x_number y_number)} do
      mouse_x = x.unsafe_as_n
      mouse_y = y.unsafe_as_n

      if grip = frame[:grip]?
        gx, gy = grip
        dx = gx - x
        dy = gy - y
        frame = Term.of(find_by_id(frame.as_d, :viewport) do |viewport|
          Term.of(viewport.morph(
            {:x, viewport[:x] + dx},
            {:y, viewport[:y] + dy},
          ))
        end)
        frame = Term.of(frame.morph({:grip, {mouse_x, mouse_y}}))
      end

      changed = true
    end

    matchpi %{(mouse press)} do
      if hovered = frame[:hovered]?
        frame = Term.of(frame.morph({:active, hovered}))
      else
        frame = Term.of(frame.morph({:grip, {mouse_x, mouse_y}}, {:cursor, :grabbing}))
      end

      changed = true
    end

    matchpi %{(mouse release)} do
      if (active = frame[:active]?) && (frame[:active]? == frame[:hovered]?)
        events.send(Term.of(:click, active))
      elsif grip = frame[:grip]?
        frame = Term.of(frame.morph({:grip, nil}, {:cursor, nil}))
      end

      frame = Term.of(frame.morph({:active, nil}))
      changed = true
    end

    matchpi %{(size w_number h_number)} do
      frame = Term.of(frame.morph({:"max-w", w}, {:"max-h", h}))
      changed = true
    end

    otherwise { }
  end

  view1 = view.get(:relaxed)
  changed ||= !visible.same?(view1)
  if changed
    visible = view1
    frame = Term.of(find_by_id(frame.as_d, :view) { |g| Term.of(g.morph({1, view1})) })
    if prev = frame[:hovered]?
      Keypath.each_item(frame) do |keypath, item|
        next unless item = item.as_d?
        next unless item[:hover]?
        next unless prev == item[:id]?

        frame = Term.of(frame.as_d.follow(keypath) { item.morph({:hover, false}).upcast })

        true # continue
      end
    end

    frame = Term.of(frame.morph({:hovered, nil}))

    UIR.hit(drawable, mouse_x, mouse_y) do |kp|
      target = drawable.follow(kp)
      next unless target[:hover]?
      next unless id = target[:id]?

      frame = Term.of(frame.morph({:hovered, id}))

      Keypath.each_item(frame) do |keypath, item|
        next unless item = item.as_d?
        next unless id == item[:id]?

        frame = Term.of(frame.as_d.follow(keypath) { item.morph({:hover, true}).upcast })

        true # continue
      end
    end
  end

  frame
end

UIR::Platform::Current.show(ui)

