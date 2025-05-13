require "./src/wirewright"
require "./pprint2"
require "./uiRb"
require "./sfpaint"
require "./ufold"

alias UIR::Platform::Current = SFML

# D7MR (short for "D7 model representation") contains functions that form
# the pipeline for document-to-model conversion.
#
# NOTE: The function you most likely want is `model`.
module D7MR
  extend self

  private def stringify(term : Term) : Term::Str
    unless term.type.string?
      # TODO: use pretty print with forced inline
      term = Term[ML.display(term, endl: false).gsub(/\s+/, ' ')]
    end

    term.as_s
  end

  private def printable(document : Term::Dict, nodepath : Stack(Int32), node : Term) : Term
    Term.of_case(node) do
      matchpi(
        %{[button caption_ to @_ (_*)]},
        %{[button caption_ as _ to @_ (_*)]},
        %{[button caption_ to @_ waiting @_ (_*)]},
        %{[button caption_ as _ to @_ waiting @_ (_*)]},
      ) do |caption|
        continue unless caption = Rhodium.const?(document, nodepath, caption)

        node.morph({1, stringify(caption)}, {:"#addr", nodepath})
      end

      matchpi %{[(%any h1 h2 h3 h4 h5 h6 p src) caption_]} do |caption|
        continue unless caption = Rhodium.const?(document, nodepath, caption)

        node.morph({1, stringify(caption)}, {:"#addr", nodepath})
      end

      matchpi %{[hr]} do
        node.morph({:"#addr", nodepath})
      end

      matchpi %{[cover title_ _+]} do |title|
        continue if Rhodium.has_cursor_at_any_depth?(document, nodepath, node)
        continue unless title = nodepath.push(1) { Rhodium.const?(document, nodepath, title) }

        Term[:cover, stringify(title)] | node.pairspart | Term["#addr": nodepath]
      end

      matchpi %{[comment _string+]} do
        node.morph({:"#addr", nodepath})
      end

      matchpi %{[view view_]} do |view|
        continue unless view = Rhodium.const?(document, nodepath, view)

        node.morph({1, view}, {:"#addr", nodepath})
      end

      matchpi %{(changes/view view_ @_ ¦ attrs_)} do
        Term[:view, view] | attrs | Term["#addr": nodepath]
      end

      # TODO: what should we do if invisible frag has style:, hover:, active:, etc.?
      matchpi %{(frag value_ @_ ¦ _ visible: false)} do
        nodepath.push(1) do
          printable(document, nodepath, value)
        end
      end

      matchpi %{[unit head_ _+]} do |head|
        continue if Rhodium.has_cursor_at_any_depth?(document, nodepath, node)
        continue unless head = Rhodium.const?(document, nodepath, head)

        node.morph({1, head}, {:"#addr", nodepath})
      end

      matchpi %{[sensor pattern_ in tspace_symbol to @_]} do
        syncd = Rhodium.sensor_in_sync?(document, tspace, pattern, node[:secret]?)

        node.morph({:"#addr", nodepath}, {:"#syncd", syncd})
      end

      matchpi %{[appearance value_ in tspace_symbol]} do
        syncd = Rhodium.appearance_in_sync?(document, tspace, value, node[:secret]?)

        node.morph({:"#addr", nodepath}, {:"#syncd", syncd})
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
  # out what it should pretty print and what it should treat specially (e.g. `h1`
  # node for which a model is created, vs. `(cell 0 @qux)`, which we can roughly
  # say is "pretty printed as code").
  def printable(document : Term::Dict) : Term::Dict
    nodepath = Stack(Int32).new

    while Rhodium.successor?(document, nodepath)
      node0 = Rhodium.follow(document, nodepath)
      next unless Rhodium.active?(document, nodepath, node0)

      node1 = printable(document, nodepath, node0)

      # If any of the following (combinations of) properties are present, we
      # consider the node as willing to participate in the interaction model.
      # We sign it ourselves, even if it makes no sense for that specific kind
      # of node.
      node1 = Term.of_case(node1) do
        matchpi(
          %[{¦ hover_boolean -#addr}],
          %[{¦ active_boolean -#addr}],
          %[{¦ in: (_*) out: (_*) focus_boolean -#addr}],
        ) do
          node1.morph({:"#addr", nodepath})
        end

        otherwise { node1 }
      end

      document = Rhodium.assign(document, nodepath, node1)
    end

    # Normally there's just one @user cursor, but we write it as if there
    # is a possibility of having several of them.
    cursorpaths = document[Rhodium::Cursors, Term.of(:edge, :user)]? || Term[]
    cursorpaths.each_entry do |cursorpath, _|
      cursor0 = document.follow(cursorpath.items)
      cursor1 = cursor0.morph({:"#cursor", true})
      document = document.where(cursorpath.items, eq: cursor1)
    end

    document
  end

  # Hides the shadow pairspart for downstream features.
  struct Fnonshadow
    include Feature

    def call(ctx, term, postfix, head, rest)
      rest.call(ctx, D7.nonshadow1(term), postfix)
    end
  end

  # Replaces terms that look like a cursor *and* were marked by `printable`
  # as such, with a cursor model.
  struct Fcursor
    include Feature

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions⋮ () #cursor: true)} do
          model = Term.of(:cursor, lhs, rhs, suggestions)
          block = Term.of(:block, model, w: Math.max(lhs.charcount + rhs.charcount, 1), h: 1)

          postfixed(block, postfix)
        end

        otherwise { rest.call(ctx, term, postfix) }
      end
    end
  end

  # Replaces nodes that look like a model *and* were marked by `printable`
  # as such, with the appropriate model.
  struct Fmodel
    include Feature

    private def prune(node)
      node.itemspart | node.pairspart.pluck(:style, :hover, :active, :focus, :"#addr")
    end

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{(%all (button caption_string _* ¦ attrs_) {¦ #addr: _})} do
          model = Term.of(:button, caption) | prune(attrs)
          block = Term.of(:block, model, w: Math.max(caption.charcount, 1), h: 1)

          postfixed(block, postfix)
        end

        matchpi %{((%any h1 h2 h3 h4 h5 h6 p src) caption_string ¦ _ #addr: _)} do
          block = Term.of(:block, prune(term), w: Math.max(caption.charcount, 1), h: 1)

          postfixed(block, postfix)
        end

        matchpi %{(hr ¦ _ #addr: _)} do
          block = Term.of(:block, prune(term), w: ctx.normal_width, h: 1)

          postfixed(block, postfix)
        end

        matchpi %{(cover title_string ¦ _ #addr: _)} do
          block = Term.of(:block, prune(term), w: Math.max(title.charcount, 1), h: 1)

          postfixed(block, postfix)
        end

        matchpi %{(comment lines_string+ ¦ _ #addr: _)} do
          block = Term.of(:block, prune(term), w: Math.max(lines.items.max_of(&.charcount), 1), h: lines.size)

          postfixed(block, postfix)
        end

        matchpi %{(%all (view view_ ¦ attrs_) {¦ #addr: _})} do
          # I would say we *never* want a view to be inline with anything else.
          # Most probably this would look very ugly. So instead of doing a lot of
          # complicated (or not) measurement, just hard-code to w: 100%.
          block = Term.of(:block, view | prune(attrs), w: ctx.normal_width, h: 1)

          postfixed(block, postfix)
        end

        matchpi %{(%all (unit head_ children_+ ¦ attrs_) {¦ #addr: _})} do |head|
          blockw = 1
          blockh = 1

          model = prune(attrs).transaction do |commit|
            commit << head

            children.items.each_with_last do |child, last|
              printout, _ = flatten(ctx, ctx.features.call(ctx, child, last ? postfix : ""))

              # NOTE: blocks can **only** contain models; because we do not recurse on
              # blocks in printout_model()
              commit << D7MR.printout_model0(printout)

              childw, childh = measure(ctx, printout)
              blockw = Math.max(blockw, childw)
              blockh += childh
            end
          end

          Term.of(:block, model, w: blockw, h: blockh)
        end

        matchpi(
          %{(sensor _ in _ to @_ ¦ attrs_ #addr: addr_ #syncd: syncd_)},
          %{(appearance _ in _symbol ¦ attrs_ #addr: addr_ #syncd: syncd_)},
        ) do
          pattrs = prune(attrs)

          indicator = Term.of(:block, Term.of(:indicator, syncd, "#addr": addr) | pattrs, w: 1, h: 1)
          suffix = rest.call(ctx, Term.of(term.itemspart | pattrs | Term["#addr": addr]), postfix)

          Term.of(:row, indicator, suffix)
        end

        otherwise { rest.call(ctx, term, postfix) }
      end
    end
  end

  MAIN_CHAIN = ML::Display::MAIN_CHAIN.prepend(Fmodel.new, Fcursor.new, Fnonshadow.new)

  # Converts the given *printable* document into a *document printout*.
  #
  # A *document printout* is *almost* a document model; there is nothing else
  # interesting to say about it.
  def printout(printable document : Term::Dict, *, normw = 80, longerw = 140) : Term::Dict
    document = D7.nonshadow1(document)
    if document.empty?
      raise ArgumentError.new("cannot print an empty document")
    end

    ctx = DisplayContext.new(normw, longerw, features: MAIN_CHAIN)
    thunk = LayoutSet::All.thunk(Term.of(document), postfix: "", myself: LayoutSet::DictAligned)
    printout, _ = flatten(ctx, thunk)
    printout.as_d
  end

  # Converts the given *printout* of a document into a *document model*.
  def printout_model(printout : Term::Dict) : Term
    Term.of(:document, printout_model0(Term.of(printout)))
  end

  # :nodoc:
  def printout_model0(printout : Term) : Term
    Term.of_case(printout) do
      # Rename frag -> token because 'frag' would be quite a confusing & generic
      # name once we gain some distance from the pretty-printing machinery.
      matchpi %{(frag caption_string ¦ attrs_)} do
        Term.of(:token, caption) | attrs
      end

      matchpi %{(indented child_ ¦ attrs_)} do
        Term.of(:indented, printout_model0(child)) | attrs
      end

      matchpi %{(tag←row children_+ ¦ attrs_)}, %{(tag←col children_+ ¦ attrs_)} do
        printout_model0(tag, children.items) | attrs
      end

      matchpi %{[block model_]}, %{[block/floating model_]} do
        model
      end

      otherwise { printout }
    end
  end

  # :nodoc:
  def printout_model0(tag : Term, children : Term::Dict::ItemsView) : Term
    model = Term::Dict.build do |commit|
      commit << tag
      commit.concat(children) { |child| printout_model0(child) }
    end

    Term.of(model)
  end

  # Returns the document model for *document*.
  def model(document : Term::Dict) : Term
    pipe(document, printable, printout, printout_model)
  end
end

# TODO: convert render()/renderer to a rewriter circuit, something like this:
# ```
# ;; Try to look up each term in the ruleset. If it can be found there, then
# ;; it is a component -- we should instantiate it using Alloy and replace,
# ;; then recurse. Otherwise, just recurse.

# (ruleset soma.render selector: (rule pattern_ template_))

# (rewriter rec
#   (itemsR master))

# (rewriter (component env_ template_)
#   (alloyR rec :env :template))

# (rewriter master
#   (rulesetR (component - rec) ruleset: soma.render))
# ```

struct Renderer
  SELECTOR = ML.term %{(rule pattern_ template_)}

  def initialize(@spec : Term)
    @ruleset = Ruleset.select(SELECTOR, @spec)
  end

  def call(spec : Term, model : Term) : {Renderer, Term}
    if @spec == spec
      {self, render(@ruleset, model)}
    else
      rr = Renderer.new(spec)
      rr.call(spec, model)
    end
  end
end

def renderer(spec : Term)
  Renderer.new(spec)
end

def render(ruleset : Ruleset, model : Term)
  unless model.type.dict? # base case
    return model
  end

  responses = ruleset.responses(model)
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

    # Transplant #addr, we'll need it to refer back to the model from the UI
    # (e.g. for hit-testing).
    if (addr = model[:"#addr"]?) && instance.type.dict?
      instance = Term.of(instance.morph({:"#addr", addr}))
    end

    if model == instance # base case
      return instance
    end

    return render(ruleset, instance)
  end

  model = model.pairspart.transaction do |commit|
    model.items.each do |item|
      commit << render(ruleset, item)
    end
  end

  Term.of(model)
end

spec = ML.terms File.read("./render.spec.wwml")

document = ML.dict <<-WWML
(cell "Hello World" @qux)
(sensor x_number in foo to @xs)
(fooze hover: false)
(unit group style: "p-5 flow-col gap-5 bg-neutral-800"
  (h1 @qux)
  (button "Hello" to @quxes () hover: true style: "w-max")
  (button "World" to @quxes ()  style: "w-max"))
(hr)
(comment "Hello World" "John Doe was here")
(cover @qux
  (button "Hello" to @quxes ())
  (button "World" to @quxes ()))
(("latest" | "" () @user))
WWML
# document = ML.dict File.read("./examples/f2.wwml")

doc = D7.run(document)

puts "Model"
model = D7MR.model(doc)

puts "Render"
rr = renderer(spec)
rr, tree = rr.call(spec, model)

frame = Term.of({:self, :window}, tree, "max-w": 1000, "max-h": 800, style: "max origin bg-neutral-900")
ui = UIR::Reducers.microfold(frame) do |_, drawable, event|
  Term.case(event) do
    matchpi %{(key enter)} do
      puts ML.display(drawable)
    end

    otherwise { }
  end

  # frame = Term.of(frame.morph({:"max-w", vw}, {:"max-h", vh}))

  Term.of(frame)
end

UIR::Platform::Current.show(ui)

# puts "UIR -> dwUIR"

# dwuir = UIR.drawable(uir)

# puts ML.display(dwuir)

# # puts ML.display(render(spec, model))
