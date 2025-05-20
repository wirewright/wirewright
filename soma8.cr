require "./src/wirewright"
require "./pprint2"
require "./uiRb"
require "./sfpaint"
require "./ufold"

alias UIR::Platform::Current = SFML

# Contains facilities for turning a D7 document into a *document model*
# (or *dmodel* for short); and for merging it back into the document.
module Dmodel
  extend self

  private def pluck1(docnode, key, default, & : Term -> T?) : T forall T
    return Term[default] unless dict = docnode.as_d?
    return Term[default] unless value = dict[key]?
    return Term[default] unless subtype = yield value

    subtype
  end

  private def pluck(docnode : Term, el) : Term::Dict
    Term[
      el: el,
      active: pluck1(docnode, :active, false, &.as_b?),
      hover: pluck1(docnode, :hover, false, &.as_b?),
      focus: pluck1(docnode, :focus, false, &.as_b?),
      in: pluck1(docnode, :in, Term[], &.as_itemspart_d?),
      out: pluck1(docnode, :out, Term[], &.as_itemspart_d?),
      behaviors: pluck1(docnode, :behaviors, Term[], &.as_itemspart_d?),
    ]
  end

  private def unpluck(mnode : Term, docnode : Term) : Term::Dict
    docnode | mnode.pluck(:active, :hover, :focus, :behaviors, :in, :out)
  end

  private def extract1(dmodel, document : Term::Dict, nodepath : Stack(Int32), node : Term) : Nil
    return unless el = node[:"#el"]? # Fast rejection
    return unless Rhodium.passable?(document, nodepath)

    Term.case(node) do
      # Treat the following nodes as branches.

      matchpi %{[view child_]}, %{[changes/view child_ @_]} do
        dmodel << pluck(node, el).transaction do |commit|
          commit << :group

          nodepath.push(1) do
            extract1(commit, document, nodepath, child)
          end
        end
      end

      matchpi %{[unit _ children_+]} do
        dmodel << pluck(node, el).transaction do |commit|
          commit << :group

          children.items.each_with_index do |child, index|
            nodepath.push(2 + index) do
              extract1(commit, document, nodepath, child)
            end
          end
        end
      end

      # The remaining nodes are treated as leaves.

      matchpi %{[input l_string m_ r_string to @_]} do
        dmodel << (Term.of(:input, l, m, r) | pluck(node, el) | Term[limit: node[:limit]?])
      end

      matchpi(
        %{[button _ to @_ (_*)]},
        %{[button _ as _ to @_ (_*)]},
        %{[button _ to @_ waiting @_ (_*)]},
        %{[button _ as _ to @_ waiting @_ (_*)]},
      ) do
        dmodel << (Term.of({:button}) | pluck(node, el) | Term[action: {:press, el}])
      end

      # Text nodes cannot receive focus
      matchpi %{[head←(%any h1 h2 h3 h4 h5 h6 p src) _*]} do
        dmodel << (Term.of({head}) | pluck(node, el).without(:focus))
      end

      matchpi %{[head_ _*]} do
        dmodel << (Term.of({head}) | pluck(node, el))
      end

      otherwise { }
    end
  end

  # Converts *document* into a *document model*, which is later embedded into
  # an *application model* -- the rest of µsoma.
  def extract(document : Term::Dict) : Term::Dict
    nodepath = Stack(Int32).new

    Term::Dict.build do |commit|
      commit << :document
      commit.with(:focus, false)
      commit.with(:in, Term[])
      commit.with(:out, Term[])

      document.items.each_with_index do |node, index|
        nodepath.push(index) do
          extract1(commit, document, nodepath, node)
        end
      end
    end
  end

  def index(dmodel : Term::Dict, keypath = Term[], els = {} of Term => Term::Dict)
    dmodel.each_item_with_index do |item, index|
      next unless mnode = item.as_d?

      if el = mnode[:el]?
        unless els.put?(el, keypath.append(index))
          raise ArgumentError.new("invalid dmodel: duplicate `el`")
        end
      end

      index(mnode, keypath.append(index), els)
    end

    els
  end

  private def merge1(document : Term::Dict, nodepath : Stack(Int32), node0 : Term, mnode : Term) : Term
    node1 = unpluck(mnode, node0)

    Term.case(mnode) do
      matchpi %{[input l_string m_ r_string]} do
        node1 = node1.morph({1, l}, {2, m}, {3, r})
      end

      otherwise { }
    end

    Term.of(node1)
  end

  # Merges UI elements from *dmodel* into nodes with matching `#el` id in
  # *document*. This is basically a way to "convert" a dmodel back into
  # a document after some processing had been applied to the former.
  #
  # NOTE: this method, without any checks, assumes that if `#el` of a node
  # in the document and `el` of an mnode are equal, then the mnode was produced
  # by the document node and by no other node. In other words, the document node
  # did not change in any significant way between `extract` and `merge`.

  MSG_PRESS = Term.of({:press})

  # TODO(?): merge1(#el, & : node0 -> node1)

  # rename to integrate?
  def merge(model, els : Hash(Term, Term::Dict), document document0 : Term::Dict) : Term::Dict
    document1 = document0

    # NOTE: In the vast majority of cases we should expect just one locus at
    # (ui <id>). But since the format of #populatio nassumes multiple such loci,
    # we handle all of them anyway; that would be a stupid reason to crash.

    outbox = model[:out]? || Term[]
    outbox.items.each do |message|
      Term.case(message) do
        matchpi %{(press id_string)} do
          loci = document0[Rhodium::Population, {:ui, id}]? || Term[]
          loci.each_entry do |locus, _|
            next unless nodepath = Rhodium.keypath?(document0, locus.items)

            document1 = Rhodium.rewrite(document1, nodepath) do |node0|
              Term.case(node0) do
                matchpi %{[button _* mailbox0←(_*)]} do
                  mailbox1 = mailbox0.append(MSG_PRESS)
                  node1 = node0.morph({node0.itemsize - 1, mailbox1})
                  Rewrite.one(node1)
                end

                otherwise { Rewrite.none }
              end
            end
          end
        end

        otherwise { }
      end
    end

    els.each do |id, keypath|
      mnode = model.follow(keypath.items)

      loci = document0[Rhodium::Population, {:ui, id}]? || Term[]
      loci.each_entry do |locus, _|
        next unless nodepath = Rhodium.keypath?(document0, locus.items)

        document1 = Rhodium.rewrite(document1, nodepath) do |node0|
          Rewrite.one(merge1(document0, nodepath, node0, mnode)).diff(node0)
        end
      end
    end

    document1
  end
end

module Dview
  extend self

  record Printable, document : Term::Dict
  record Printout, term : Term

  private def stringify(term : Term) : Term::Str
    # TODO: use pretty print with forced inline
    term.as_s? || Term[ML.display(term, endl: false).gsub(/\s+/, ' ')]
  end

  private def printable(document : Term::Dict, nodepath : Stack(Int32), term : Term) : Term
    return term unless node = term.as_d?

    Term.of_case(node) do
      if node[:"#el"]? # UI elements
        matchpi(
          %{[button caption_ to @_ (_*)]},
          %{[button caption_ as _ to @_ (_*)]},
          %{[button caption_ to @_ waiting @_ (_*)]},
          %{[button caption_ as _ to @_ waiting @_ (_*)]},
        ) do |caption|
          continue unless caption = Rhodium.const?(document, nodepath, caption)

          node.morph({1, stringify(caption)}, {:"#visual", true})
        end

        matchpi %{[(%any h1 h2 h3 h4 h5 h6 p src) caption_]} do |caption|
          continue unless caption = Rhodium.const?(document, nodepath, caption)

          node.morph({1, stringify(caption)}, {:"#visual", true})
        end

        matchpi(
          %{[hr]},
          %{[input _string m_ _string to @_]},
          %{[comment _string+]},
        ) do
          node.morph({:"#visual", true})
        end

        matchpi %{[cover title_ _+]} do |title|
          continue if Rhodium.has_cursor_at_any_depth?(document, nodepath, term)
          continue unless title = nodepath.push(1) { Rhodium.const?(document, nodepath, title) }

          Term[:cover, stringify(title)] | node.pairspart | Term["#visual": true]
        end

        matchpi %{[view view_]} do |view|
          continue unless view = Rhodium.const?(document, nodepath, view)

          node.morph({1, view}, {:"#visual", true})
        end

        matchpi %{(changes/view view_ @_ ¦ attrs_)} do
          Term[:view, view] | attrs | Term["#visual": true]
        end

        # TODO: what should we do if invisible frag has style:, hover:, active:, etc.?
        # FIXME: if invisible frag has style hover active etc., wrap its contents in a unit group.
        matchpi %{(frag value_ @_ ¦ _ visible: false)} do
          nodepath.push(1) do
            printable(document, nodepath, value)
          end
        end

        matchpi %{[unit head_ _+]} do |head|
          continue if Rhodium.has_cursor_at_any_depth?(document, nodepath, term)
          continue unless head = Rhodium.const?(document, nodepath, head)

          node.morph({1, head}, {:"#visual", true})
        end
      end

      matchpi %{[sensor pattern_ in tspace_symbol to @_]} do
        syncd = Rhodium.sensor_in_sync?(document, tspace, pattern, node[:secret]?)

        node.morph({:"#visual", true}, {:"#syncd", syncd})
      end

      matchpi %{[appearance value_ in tspace_symbol]} do
        syncd = Rhodium.appearance_in_sync?(document, tspace, value, node[:secret]?)

        node.morph({:"#visual", true}, {:"#syncd", syncd})
      end

      otherwise { node }
    end
  end

  # Converts the given *document* into a *printable document*.
  #
  # This function attaches special attributes to active nodes and reachable
  # `@user` cursors (thus `Rhodium.cursorfind` must have been run on *document*
  # before calling this function) to discriminate them from everything else.
  #
  # This function also instantiates some nodes within the document. E.g. it will
  # replace `(h1 @count)` with `(h1 "0")` if `@count = 0`.
  #
  # The special attributes later help the pretty printing machinery to figure
  # out what it should pretty print as code and what it should display visually
  # (e.g. `h1` node vs. `(cell 0 @qux)`)
  def printable(document : Term::Dict) : Printable
    nodepath = Stack(Int32).new

    while Rhodium.successor?(document, nodepath)
      node0 = Rhodium.follow(document, nodepath)
      next unless Rhodium.active?(document, nodepath, node0)

      node1 = printable(document, nodepath, node0)
      document = Rhodium.assign(document, nodepath, node1)
    end

    # Normally there's just one @user cursor, but we write it as if there
    # is a possibility of having several of them.
    cursorpaths = document[Rhodium::Cursors, Term.of(:edge, :user)]? || Term[]
    cursorpaths.each_entry do |cursorpath, _|
      cursor0 = document.follow(cursorpath.items)
      cursor1 = cursor0.morph({:"#visual", true})
      document = document.where(cursorpath.items, eq: cursor1)
    end

    Printable.new(document)
  end

  # Hides the shadow pairspart for downstream features.
  struct Fnonshadow
    include Feature

    def call(ctx, term, postfix, head, rest)
      rest.call(ctx, D7.nonshadow1(term), postfix)
    end
  end

  # Replaces terms that look like a cursor *and* were marked by `printable`
  # as such, with a cursor view.
  struct Fcursor
    include Feature

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions⋮ () #visual: true)} do
          view = Term.of(:cursor, lhs, rhs, suggestions)
          block = Term.of(:block, view, w: Math.max(lhs.charcount + rhs.charcount, 1), h: 1)

          postfixed(block, postfix)
        end

        otherwise { rest.call(ctx, term, postfix) }
      end
    end
  end

  # Replaces visual nodes with instantiations of the appropriate view.
  struct Fvisual
    include Feature

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{(button caption_string _* ¦ _ #visual: true)} do
          view = Term.of(:button, caption) | term.pairspart
          block = Term.of(:block, view, w: Math.max(caption.charcount, 1), h: 1)

          postfixed(block, postfix)
        end

        matchpi %{((%any h1 h2 h3 h4 h5 h6 p src) caption_string ¦ _ #visual: true)} do
          block = Term.of(:block, term, w: Math.max(caption.charcount, 1), h: 1)

          postfixed(block, postfix)
        end

        matchpi %{(input l_string m_ r_string _* ¦ _ #visual: true)} do
          view = Term.of(:input, l, m, r) | term.pairspart
          block = Term.of(:block, view, w: Math.max(l.charcount + r.charcount, 1), h: 1)

          postfixed(block, postfix)
        end

        matchpi %{(hr ¦ _ #visual: true)} do
          block = Term.of(:block, term, w: ctx.normal_width, h: 1)

          postfixed(block, postfix)
        end

        matchpi %{(cover title_string ¦ _ #visual: true)} do
          block = Term.of(:block, term, w: Math.max(title.charcount, 1), h: 1)

          postfixed(block, postfix)
        end

        matchpi %{(comment lines_string+ ¦ _ #visual: true)} do
          block = Term.of(:block, term, w: Math.max(lines.items.max_of(&.charcount), 1), h: lines.size)

          postfixed(block, postfix)
        end

        matchpi %{(view _ ¦ _ #visual: true)} do
          # I would say we *never* want a view to be printed inline. Most likely this
          # would look very ugly. So instead of doing a lot of complicated (or not)
          # measurement, just hard-code views to w: 100%.
          #
          # NOTE: we do not simply render the argument because the view itself may have
          # some `style`s (e.g.) This is allowed hierarchy-wise, so we have to wrap in
          # `group` -- and hope that Microfold will be smart enough to remove it if it
          # has no effect on the overall look & feel.
          block = Term.of(:block, term.morph({0, :group}), w: ctx.normal_width, h: 1)

          postfixed(block, postfix)
        end

        matchpi %{(unit head_ children_+ ¦ _ #visual: true)} do |head|
          blockw = blockh = 1

          view = term.pairspart.transaction do |commit|
            commit << head

            children.items.each_with_last do |child, last|
              printout, _ = flatten(ctx, ctx.features.call(ctx, child, last ? postfix : ""))

              # NOTE: blocks can **only** contain a view; because we do not recurse on
              # blocks in view0(). Thus we have to do view0() here.
              commit << Dview.view0(Printout.new(printout))

              childw, childh = measure(ctx, printout)
              blockw = Math.max(blockw, childw)
              blockh += childh
            end
          end

          Term.of(:block, view, w: blockw, h: blockh)
        end

        matchpi(
          %{(sensor _ in _symbol to @_ ¦ _ #visual: true #syncd: syncd_)},
          %{(appearance _ in _symbol ¦ _ #visual: true #syncd: syncd_)},
        ) do
          indicator = Term.of(:block, Term.of(:indicator, syncd), w: 1, h: 1)
          suffix = rest.call(ctx, term, postfix)

          Term.of(:row, indicator, suffix)
        end

        otherwise { rest.call(ctx, term, postfix) }
      end
    end
  end

  MAIN_CHAIN = ML::Display::MAIN_CHAIN.prepend(Fvisual.new, Fcursor.new, Fnonshadow.new)

  # Converts the given printable *document* into a *document printout*.
  #
  # A *document printout* is *almost* a document view; there is nothing else
  # interesting to say about it.
  def printout(document wrapper : Printable, *, wide = 80, wider = 140) : Printout
    document = wrapper.document
    document = D7.nonshadow1(document)
    if document.empty?
      raise ArgumentError.new("cannot print an empty document")
    end

    ctx = DisplayContext.new(wide, wider, features: MAIN_CHAIN)
    thunk = LayoutSet::All.thunk(Term.of(document), postfix: "", myself: LayoutSet::DictAligned)
    printout, _ = flatten(ctx, thunk)

    Printout.new(printout)
  end

  # Converts the given *printout* of a document into a *document view*.
  def view(printout : Printout) : Term
    Term.of(:document, view0(printout))
  end

  # :nodoc:
  def view0(printout : Printout) : Term
    Term.of_case(printout.term) do
      # Rename frag -> token because 'frag' would be quite a confusing & generic
      # name once we gain some distance from the pretty-printing machinery.
      matchpi %{(frag caption_string ¦ attrs_)} do
        Term.of(:token, caption) | attrs
      end

      matchpi %{(indented child_ ¦ attrs_)} do
        Term.of(:indented, view0(Printout.new(child))) | attrs
      end

      matchpi %{(tag←row children_+ ¦ attrs_)}, %{(tag←col children_+ ¦ attrs_)} do
        view0(tag, children.items) | attrs
      end

      matchpi %{[block view_]}, %{[block/floating view_]} do
        view
      end

      otherwise { printout.term }
    end
  end

  # :nodoc:
  def view0(tag : Term, children : Term::Dict::ItemsView) : Term
    view = Term::Dict.build do |commit|
      commit << tag
      commit.concat(children) { |child| view0(Printout.new(child)) }
    end

    Term.of(view)
  end

  # Returns the document view for *document*.
  def view(document : Term::Dict) : Term
    pipe(document, printable, printout, view)
  end
end

document = ML.dict <<-WWML
(cell "Hello World" @qux)
(sensor x_number in foo to @xs)
(fooze hover: false)
(unit group style: "min-w-xs flow-col p-3 border border-neutral-800 focus:border-blue-500 gap-5 rounded-lg" behaviors: (y-list)
  (h1 "My Form")
  (input "" (| runes) "" to @first-names label: "First name" placeholder: "John" limit: 10)
  (input "" (| runes) "" to @last-names label: "Last name" placeholder: "Doe")
  (input "" (| runes) "" to @emails label: "Email" placeholder: "jd@example.com" icon: "mail" hint: "Temporary mails not allowed"))
(unit group style: "p-5 flow-col gap-5 bg-neutral-800"
  (h1 @qux style: "qux:bg-neutral-500" qux: true)
  (button "Hello" to @quxes () hover: false style: "w-max")
  (button "World" to @quxes () qux: true style: "qux:bg-neutral-500"))
(hr)
(comment "Hello World" "John Doe was here")
(cover @qux style: "w-max"
  (button "Hello" to @quxes ())
  (button "World" to @quxes ()))
(("latest" | "" () @user))
WWML

puts

doc = D7.run(document)

model = Dmodel.extract(doc)
model = model.morph({:focus, true})
els = Dmodel.index(model)
# puts ML.display(m)

v = Dview.view(doc)

# puts ML.display(v)

# TODO: convert Renderer/render to a rewriter circuit

struct Renderer
  SELECTOR = ML.term %{(rule pattern_ template_)}

  def initialize(@spec : Term)
    @ruleset = Ruleset.select(SELECTOR, @spec)
  end

  def call(spec : Term, view : Term) : {Renderer, Term}
    if @spec == spec
      {self, render(@ruleset, view)}
    else
      rr = Renderer.new(spec)
      rr.call(spec, view)
    end
  end
end

def renderer(spec : Term)
  Renderer.new(spec)
end

def render(ruleset : Ruleset, view : Term)
  unless view.type.dict? # base case
    return view
  end

  responses = ruleset.responses(view)
  responses.each do |response|
    pr, rule = response

    case pr
    in Pr::One  then env = pr.env
    in Pr::Many then env = pr.envs[0]
    end

    unless rule.is_a?(Rule::Template)
      raise "render: unsupported rule type"
    end

    instance = Alloy.render(env, rule.body)

    if view == instance # base case
      return instance
    end

    return render(ruleset, instance)
  end

  view = view.pairspart.transaction do |commit|
    view.items.each do |item|
      commit << render(ruleset, item)
    end
  end

  Term.of(view)
end

private def els(view : Term::Dict, els = {} of Term => Term)
  view.each_item_unordered do |item|
    next unless node = item.as_d?

    if el = node[:"#el"]?
      unless els.put?(el, item)
        raise ArgumentError.new("invalid dmodel: duplicate `el`")
      end
    end

    els(node, els)
  end

  els
end

def inherit(views : Hash(Term, Term), unit : Term) : Term
  unless node = unit.as_d?
    return unit
  end

  node = node.transaction do |commit|
    node.each_item_with_index do |item, index|
      commit.with(index, inherit(views, item))
    end

    if (id = node[:inherit]?) && (view = views[id]?)
      commit.without(:inherit)
      commit.with(:"#el", id)

      view.each_pair do |key, rhs|
        # Concatenate styles.
        if key == Term[:style]
          style0 = unit[key]? || Term[""]
          style1 = rhs
          if (s0 = style0.as_s?) && (s1 = style1.as_s?)
            commit.with(key, s0.stitch(" ").stitch(s1))
          end
        end

        # Copy boolean-valued keys. They may be referenced in the style so we
        # have to copy them so that Microfold can take a look.
        if rhs.type.boolean?
          next if Rhodium.shadow?(key)

          commit.with(key, rhs)
        end
      end
    end
  end

  Term.of(node)
end

def inherit(view : Term::Dict, unit : Term) : Term
  inherit(els(view), unit)
end

def update(model, els : Hash(Term, Term::Dict), el : Term, &) : Term::Dict
  return model unless keypath = els[el]?

  node0 = model.follow(keypath.items)
  node1 = yield node0

  model.where(keypath.items, eq: node1)
end

def update(model, els, el, key, eq value)
  update(model, els, el) { |node| Term.of(node.with(key, value)) }
end

def mkbhvr(base : String)
  refR = dfsR(
    switchR(
      { %[($my rewritee_)], envR(Term.of(:"$my")) },
      { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
      { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
    )
  )

  primitives = ProcRuleset.build do
  end

  onceR = callR(PRIMITIVES)

  # First rewrite entries, then rewrite self.
  set, exhevalR = recR
  set.call chainR(entriesR(exhevalR), onceR)

  evalR = dfsR(
    switchR(
      { %[($ rewritee_)], exhevalR },
      { %[($once rewritee_)], onceR },
    )
  )

  backmapR = chainR(refR, evalR)

  selector = ML.term(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_))])

  set, rec = recR

  exhR(
    set.call choiceR(
      exhR(rulesetR(Ruleset.select(selector, ML.terms(base)), noR, backmapR, noR)),
      itemsR(rec),
    )
  )
end

spec = ML.terms File.read("./component.spec.wwml")

rr = renderer(spec)
rr, tree = rr.call(spec, v)

scene = inherit(v.as_d, tree)

stratum_state = nil

mouse_x = Term[0]
mouse_y = Term[0]

hovered0 = hovered1 = nil
active = nil
model0 = nil

# FIXME: use NIL instead of FALSE on active: focus: etc. Aka do not pollute
# the document with crap

behavior_source = Atomic(String).new("")
watch(Path["behavior.spec.wwml"]) do |updated|
  behavior_source.set(updated.gets_to_end)
end
backup = ""
r = nil
src0 = nil

# NOTE: what I see here is a "Model" entity of some kind that we can
# run a transaction() on. During transaction we can modify the raw
# model content (e.g. access elements by #el, and/or enqueue events).
# After transaction, the model is passed to the rewriter (which is
# maintained up-to-date by the same Model entity). After such transactions
# we trigger further updates, e.g. absorption into the document.

frame = Term.of({:self, :window}, scene, "max-w": 1000, "max-h": 800, style: "max origin bg-neutral-900")
ui = UIR::Reducers.microfold(frame) do |_, dwuir, event|
  Term.case(event) do
    matchpi %{(key f1)} do
      puts ML.display(dwuir)
    end
    matchpi %{(key f3)} do
      puts ML.display(model)
    end
    matchpi %{(key f4)} do
      puts ML.display(doc)
    end

    matchpi %{(mouse motion x_number y_number)} do
      mouse_x = x.unsafe_as_n
      mouse_y = y.unsafe_as_n
    end

    matchpi %{(mouse press)} do
      active = hovered0

      active.try do |el|
        model = update(model, els, el, :active, eq: true)
      end
    end

    matchpi %{(mouse release)} do
      model = model.morph({:in, (model[:in]? || Term[]).append({:blur})})
      model = rewrite(Term.of(model), r.not_nil!).as_d

      active.try do |el|
        next unless el == hovered0

        model = update(model, els, el) do |node|
          node.morph({:active, false}, {:in, (node[:in]? || Term[]).append({:click})})
        end
      end
      active = nil
    end

    matchpi %{(key _)}, %{(key-up _)}, %{(input _)} do
      model = model.morph({:in, (model[:in]? || Term[]).append(event)})
    end

    otherwise { }
  end

  unless stratum_state == {dwuir, mouse_x, mouse_y}
    stratum = UIR.stratum(dwuir, mouse_x, mouse_y)
    stratum_state = {dwuir, mouse_x, mouse_y}

    stratum.sort_by!(&.size)
    hovered1 = stratum.reverse_each do |hit|
      target = dwuir.follow(hit)
      next unless el = target[:"#el"]?
      break el
    end

    unless hovered0 == hovered1
      hovered0.try { |el| model = update(model, els, el, :hover, eq: false) } # unhover
      hovered1.try { |el| model = update(model, els, el, :hover, eq: true) }  # hover
    end

    hovered0 = hovered1
  end

  # on model changed
  unless model0 == model
    src = behavior_source.get

    # regen behavior rewriter circuit for live reload
    unless src == src0
      begin
        r = mkbhvr(src)
        backup = src
      rescue e : ML::SyntaxError
        e.humanize(STDERR, src)
        r = mkbhvr(backup)
      ensure
        src0 = src
      end
    end

    # puts "Rewrite"
    # puts ML.display(model)
    model = rewrite(Term.of(model), r.not_nil!).as_d
    # puts "----"
    # puts ML.display(model)
  end

  # close the feedback loop if model changed
  #
  # FIXME: do this only if the DOCmodel changed!!!
  unless model0 == model
    doc = Dmodel.merge(model, els, doc)
    # ... on doc changed (?)

    # X = fork do
    model = Dmodel.extract(doc)
    model = model.morph({:focus, true})
    els = Dmodel.index(model)
    # end
    # Y = fork do
    v = Dview.view(doc)
    rr, tree = rr.call(spec, v)
    scene = inherit(v.as_d, tree)
    # end
    # model, els, scene = join(X, Y)
    frame = Term.of(frame.morph({1, scene}))
    model0 = model
  end

  # frame = Term.of(frame.morph({:"max-w", vw}, {:"max-h", vh}))

  Term.of(frame)
end

UIR::Platform::Current.show(ui)

# - think about ways to let the document process events from the inbox/outbox.
#   ? if deferred: true, the inbox/outbox will queue events without dequeueing them.
#     this lets us wait for the document to handle events.
#   ? if -deferred, then we handle them at behavior level without even letting
#     the document see them.
#   ---- ^ THIS should probably be a rewriter circuit level flag. If it has deferred: true
#          then just stop there & do not rewrite. But how do we let child components receive
#          events then?
# - add a way to cycle through all elements globally (e.g. tab/S-tab in browsers --
#   but in our case Tab is taken so maybe something else)
# - indicate focus on focused elements that are not buttons/inputs. currently it's
#   impossible to tell.
# - focus vs. focus-within; add a way to disable focus. maybe have focus: none/disabled/within/self
#   instead of a boolean. Clarify focus: ... defaults for different kinds of nodes.
#     - if document has focus: self, events should go to the cursor (VIA THE OUTBOX; the model wasn't able to handle --
#       let the cursor try to)
#     - in ufold, add a way to scope based on non-boolean attrs: e.g. focus: disabled should be accessible
#       through e.g. `focus-disabled:bg-neutral-500`, `focus-within:...`, etc.
#          so for booleans it is `focus:<...>` for `true`
#             for symbols or nats it is `focus-<symbol or nat>:...`
# - selection handling in input, multiline input
#     i think the best bet is to use a range based solution and that's it. maybe
#     <b>...<e> or maybe anchor and span based.
# - input: selection using mouse, put cursor on click
#     text nodes should support the ability to hit() a text. this will require an sfpaint/uiRb
#     rewrite though. It is impossible or very difficult otherwise. We should be able to pass
#     the uiR node **itself** to hit(). It should then work with platform to figure out what
#     was hit. Then on soma side we just annotate the node with #owner or #chars-to or smth.
#     and point it to the owner node in the model. Then when we click we determine the index
#     of what was hit and send that off to the owner node's mailbox. The node can keep track
#     of state to e.g. move the selection anchor there instead of the cursor, so we'll get
#     mouse selection working.
# - allow the user to define button hot keys. (key pressed) must map onto (mouse press)
#   on the buttons, and key released onto (mouse release)
# - think about ways to scroll inside inputs. If an input is bound by both
#   w: max and h: max (how to determine that?!) it must use a viewport & implement scroll.
# - implement x-list behavior. make sure xy-list can be composed by nesting x or y-lists inside y or x-lists corr.
# - complete TODOs in this subsection
