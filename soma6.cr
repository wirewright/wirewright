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
        matchpi %{(comment desc_string)} do
          unit = Alloy.render(Term[desc: desc], TEMPLATE)

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
          %{(button caption_ to @_ (_*) ¦ _ id_)},
          %{(button caption_ as _ to @_ (_*) ¦ _ id_)}
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

    TEMPLATE = ML.term <<-WWML
    (group style: "content"
      (code ^lhs style: "text-neutral-400 bg-neutral-700 ring-1")
      ((self rect) style: "w-px h-max bg-blue-500")
      (code ^rhs style: "text-neutral-400 bg-neutral-700 ring-1"))
    WWML

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{[lhs_string | rhs_string (_*) @user]} do
          unit = Alloy.render(Term[lhs: lhs, rhs: rhs], TEMPLATE)

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
        matchpi %{[button caption_ to @edge_ (_*)]} do
          continue unless Rhodium.cursordepth(node0, pairspart: true) == -1

          node1 = Term.of(node0.morph({:id, {caption.hash, caption.hash, edge.hash}.hash}))
        end

        matchpi %{[button caption_ as value_ to @edge_ (_*)]} do
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
        matchpi %{[button caption_ to @edge_ (_*)]} do
          next unless Term.of({caption.hash, caption.hash, edge.hash}.hash) == id

          node1 = yield node0
        end

        matchpi %{[button caption_ as value_ to @edge_ (_*)]} do
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

d7ctx = ExecutionContext::MultiThreaded.new("D7", 1)
d7ctx.spawn do
  document = ML.terms <<-WWML
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

  initial = true
  settled = false

  docframe0 = DocR.unit(DocR.pptree(document))
  docframe0 = docframe0.morph({:document, document})

  handle = ->(event : Term) do
    Term.case(event) do
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
            matchpi %{[button _ to @_ (_*)]} do
              Term.of(node.morph({4, Tail, {:press}}))
            end

            matchpi %{[button caption_ as value_ to @edge_ (_*)]} do
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

    docframe0 = DocR.unit(DocR.pptree(doc = docframe0[:document]))
    docframe0 = docframe0.morph({:document, doc})

    view.set(docframe0.without(:document), :relaxed)
  end

  cycle = D7::Step.new do |document|
    docframe0 = docframe0.morph({:document, document})

    peek.call

    docframe0 = DocR.unit(DocR.pptree(doc = docframe0[:document]))
    docframe0 = docframe0.morph({:document, doc})

    view.set(docframe0.without(:document), :relaxed)

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
    while settled
      wait.call

      settled = false
    end

    seed0 = docframe0[:document].as_d
    seed1 = D7.run(seed0,
      log: D7::Log::None.new,
      transition: Rhodium.transition,
      step: D7.steps(Rhodium.step, Nitrene.step(nitrene), cycle),
      goal: D7::Goal.none,
      initial: initial,
    )

    docframe0 = docframe0.morph({:document, seed1})

    initial = false
    settled = true
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

