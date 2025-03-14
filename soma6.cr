require "./src/wirewright"
require "./uiRb"
require "./sfuiR"
require "./pprint2"
require "./templ"

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

          postfixed(DocR.block(unit), postfix)
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

          postfixed(DocR.block(unit), postfix)
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

          postfixed(DocR.block(unit), postfix)
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

  @@counter = Atomic(UInt32).new(0u32)

  def annotated(document document0 : Term::Dict) : Term::Dict
    nodepath = Stack(Int32).new

    document1 = document0

    while Rhodium.successor?(document0, nodepath)
      node0 = Rhodium.follow(document0, nodepath)
      node1 = node0

      Term.case(node0) do
        matchpi %{[button caption_ to @_ (_*)]} do
          mailpath = Term.of(nodepath).append(4)

          node1 = Term.of(node0.morph({:id, @@counter.add(1, :relaxed)}))
        end

        matchpi %{[button caption_ as _ to @_ (_*)]} do
          mailpath = Term.of(nodepath).append(6)

          node1 = Term.of(node0.morph({:id, @@counter.add(1, :relaxed)}))
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
  def pptree(document : Term, *, toplevel : Bool = true) : Term
    ppin = document

    if document = document.as_d?
      ppin = visible(document)
      if toplevel
        ppin = annotated(ppin)
      end
      ppin = Term.of(ppin)
    end

    chain = ML::Display::MAIN_CHAIN
      .prepend(Button.new)
      .prepend(Comment.new)
      .prepend(Cursor.new)

    ctx = DisplayContext.new(60, 120, features: chain)
    tree = LayoutSet::All.thunk(ppin, "", toplevel ? LayoutSet::DictAligned : LayoutSet::All)
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

      matchpi %{[block subunit_]} do
        subunit
      end
    end
  end

  def block(unit : Term)
    rem = Term[16] # ?!

    uir = Microfold.uir(Microfold::SPEC, unit, rem: rem)
    drawable = rewrite(uir, UIR.rewriter)

    Term.case(drawable) do
      matchpi %[{¦ final-w: w←(%number +i32) final-h: h←(%number +i32)}] do
        Term.of(:block, unit, w: w//rem, h: h//rem)
      end
    end
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

# TODO: figure out how to actually wire everything up. Esp. with hover/click and such.

views = Channel(Term).new
events = Channel(Term).new

d7ctx = ExecutionContext::SingleThreaded.new("D7")
d7ctx.spawn do
  document = ML.terms <<-WWML
  (comment "Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.")
  (cell 0 @count)
  (button "Increment" as 1 to @deltas ())
  (button "Decrement" as -1 to @deltas ())
  (transform (@deltas delta_number) to @counts with @count (+ count delta))
  (latest @counts @count)
  ("" | "" () @user)
  WWML

  docframe = DocR.unit(DocR.pptree(document))

  while event = events.receive
    puts "#{event}"

    views.send(docframe)
  end
end

frame0 = ML.term <<-WWML
((self window) style: "bg-neutral-900 max origin" max-w: 1000 max-h: 800
  (group style: "max flow-col gap-3 p-3 fr"
    (group style: "bg-neutral-800 w-max h-content px-2 py-1 rounded-sm"
      (p "Wirewright µsoma" style: "text-neutral-400 text-xs"))
    (group style: "w-max h-fr pl-32 pt-16" id: view)))
WWML

ui = UIR::Reducers.microfold(frame0) do |frame, drawable, event|
  if event == Term.of(:cycle)
    select
    when view = views.receive
      # Rendezvous with the D7 thread on view.
    else
      next frame
    end
  else
    # Ensure the event is handled in the displayed view.
    events.send(event)
    view = views.receive
  end

  Term.of(find_by_id(frame.as_d, :view) { |g| Term.of(g.morph({1, view})) })

  # Term.case(event) do
  #   matchpi %{(motion x_number y_number)} do
  #     if prev = frame[:hovered]?
  #       Keypath.each_item(frame) do |keypath, item|
  #         next unless item = item.as_d?
  #         next unless item[:hover]?
  #         next unless prev == item[:id]?

  #         frame = Term.of(frame.as_d.follow(keypath) { item.morph({:hover, false}).upcast })

  #         false # break
  #       end
  #     end

  #     frame = Term.of(frame.morph({:hovered, nil}))

  #     UIR.hit(drawable, x.unsafe_as_n, y.unsafe_as_n) do |kp|
  #       target = drawable.follow(kp)
  #       next unless target[:hover]?
  #       next unless id = target[:id]?

  #       frame = Term.of(frame.morph({:hovered, id}))

  #       Keypath.each_item(frame) do |keypath, item|
  #         next unless item = item.as_d?
  #         next unless id == item[:id]?

  #         frame = Term.of(frame.as_d.follow(keypath) { item.morph({:hover, true}).upcast })

  #         false # break
  #       end
  #     end
  #   end

  #   otherwise { }
  # end

  # frame
end

UIR::Platform::Current.show(ui)

