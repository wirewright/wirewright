module D7VR
  extend self

  struct AttrLeader
    include Feature

    def initialize(@rem : Term::Num, @inherited : Term::Dict)
    end

    def call(ctx, term, postfix, head, rest)
      unless dict = term.as_d?
        return rest.call(ctx, term, postfix)
      end

      # NOTE: We assume that instantiation worked properly. This means that #-attrs
      # are only where they should be; the user cannot inject them all the way in the
      # document (perhaps maliciously) for them to end up here -- instance() would
      # have removed them as they would have looked like shadow attribute keys.

      dict, view = dict.without?(:"#view")
      dict, status = dict.without?(:"#status")
      dict, extension = dict.without?(:"#extend")

      printout = nil

      # If the term defines a #view for itself attempt to render that as
      # a block.
      if view
        dict, fallback = dict.without?(:"#fallback")

        unless fallback
          raise "BUG: view provided #view but did not provide #fallback"
        end

        # block? requires fallback for fallback at runtime, if the size
        # cannot be figured out.
        if printout = D7VR.block?(Term.of(view), fallback, @rem, @inherited)
          printout = postfixed(printout, postfix)
        else
          # ... If it fails right now though, at printout, we'll resort to
          # the same fallback here.
          printout = rest.call(ctx, fallback, postfix)
        end
      else
        # If no view then render the dict (i.e. without any #-attrs)
        printout ||= rest.call(ctx, Term.of(dict), postfix)
      end

      # FIXME: this will include postfix into info, so e.g. if we hover on
      # postfix it'll count as a hover on printout too -- wrongly.
      if extension
        printout = Term.of(:info, printout) | extension
      end

      if status
        if status.true?
          printout = Term.of(:row, Term.of(:block, Term.of({:self, :rect}, style: "w-2 h-2 bg-green-500 dt-2 mr-1 rounded-xs"), w: 1, h: 1), printout)
        else
          printout = Term.of(:row, Term.of(:block, Term.of({:self, :rect}, style: "w-2 h-2 bg-yellow-500 dt-2 mr-1 rounded-xs"), w: 1, h: 1), printout)
        end
      end

      Term.of(printout)
    end
  end

  struct Unit
    include Feature

    def initialize(@document : Term::Dict, @rem : Term::Num, @inherited : Term::Dict)
    end

    def call(ctx, term, postfix, head, rest)
      Term.case(term) do
        matchpi %{(unit node_ children_+ ¦ attrs_ style⋮ "" #fallback: fallback_)} do
          cascaded = Microfold.cascaded(Microfold::SPEC, attrs.unsafe_as_d, style.to(String), @rem, base: @inherited)

          unit = term.pairspart.transaction do |commit|
            commit << node
            commit.concat(children.items) do |child|
              Term.case(child) do
                matchpi %{_dict} { D7VR.document_node_unit(@document, child, @rem, inherit: cascaded) }
                otherwise { child }
              end
            end
          end

          continue unless block = D7VR.block?(Term.of(unit), fallback, @rem, @inherited)

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
      (code ^lhs style: "text-neutral-400 bg-neutral-700")
      ;; If both are empty w-0 won't work so we have to create a rectangle
      ;; that is explicitly w-px. Also, if lhs is empty, use ring-r, because
      ;; if we're using ring-l, it may overflow outside of the viewport
      ;; if the cursor is located at origin.
      ((self rect) style: "h-max z-10 bg-blue-500 w-0 ring-l lempty:ring-l-0 lempty:ring-r empty:w-px empty:ring-0"
        empty: (^ (= lhs rhs ""))
        lempty: (^ (= lhs "")))
      (code ^rhs style: "text-neutral-400 bg-neutral-700"))
    WWML

    SUGGESTION_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-20 dt-8 border border-neutral-600 bg-neutral-800 rounded p-2"
      (group style: "content min-w-xs max-w-md flow-col gap-2"
        (p ^name style: "w-max px-2 py-1 font-mono bg-neutral-700 text-neutral-200 font-medium rounded-sm")
        (p ^intro style: "w-max px-2 pb-1 text-sm text-neutral-300")))
    WWML

    SUGGESTION_GROUP_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-20 dt-8 border border-neutral-600 bg-neutral-800 rounded p-2"
      (group style: "content flow-col gap-2"
        (^match (> total 1)
          (when true
            (group style: "w-max fr h-content flow-row gap-2 px-2 py-1 bg-neutral-700 rounded-sm"
              (p style: "text-sm center-y text-neutral-200"
                "↑" ^current "/" ^total "↓")
              (box ^head style: "w-fr h-content font-mono text-neutral-200 font-medium")))
          (when false
            (box ^head style: "w-max h-content px-2 py-1 font-mono bg-neutral-700 text-neutral-200 font-medium rounded-sm")))
        (box ^body style: "content px-2 pb-1 text-sm text-neutral-300")))
    WWML

    SUGGESTION_LIST_TEMPLATE = ML.term <<-WWML
    (box style: "floating z-20 dt-8 border border-neutral-600 bg-neutral-800 rounded p-3"
      (main style: "content min-w-xs max-w-md flow-col gap-3"
        (header style: "w-max h-content flow-row font-sans font-normal text-xs text-neutral-300"
          "Showing " ^begin ".." ^end " out of " ^total ". Use PgUp/Dn to scroll")
        (group style: "w-max h-content flow-row fr"
          (list style: "w-fr h-content flow-col text-sm leading-sm font-mono font-text text-neutral-200 gap-2"
            (^*paste names 0 ..= -1))
          (^if (< scroll-offset 1)
            (scroll style: "w-3 h-max pr-1"
              ((self y-translate) offset: (* ^scroll-offset) style: "max"
                ((self rect/outline) style: "max border bg-neutral-600 rounded-sm" max-h: (* ^scroll-height))))))))
    WWML

    def initialize(@rem : Term::Num)
    end

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
          # FIXME: continue if not passable

          cursor_unit = Alloy.render(Term[lhs: lhs, rhs: rhs], CURSOR_TEMPLATE)
          suggestion_unit = Alloy.render(Term[name: name, intro: intro], SUGGESTION_TEMPLATE)

          unit = Term.of(:group, cursor_unit, suggestion_unit, style: "content flow-none")

          continue unless block = D7VR.block?(unit, term, @rem)

          postfixed(block, postfix)
        end

        matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions: (suggestions/list above←(_*) visible←((%past (_string _string) min: 1)) below←(_*)))} do
          # FIXME: continue if not passable

          cursor_unit = Alloy.render(Term[lhs: lhs, rhs: rhs], CURSOR_TEMPLATE)

          b = above.size
          e = b + visible.size
          total = above.size + visible.size + below.size
          names = visible.items.map { |(name, _)| name }

          suggestion_vars = Term[
            begin: b,
            end: e,
            total: total,
            names: names,
            "scroll-offset": b / total,
            "scroll-height": visible.size / total,
          ]
          suggestion_unit = Alloy.render(suggestion_vars, SUGGESTION_LIST_TEMPLATE)
          unit = Term.of(:group, cursor_unit, suggestion_unit, style: "content flow-none")

          continue unless block = D7VR.block?(unit, term, @rem)

          postfixed(block, postfix)
        end

        matchpi %{(lhs_string | rhs_string (_*) @user ¦ _ suggestions_: (suggestions/group prefix←(_*) suffix←((head_string body_string) _*)))} do
          # FIXME: continue if not passable

          cursor_unit = Alloy.render(Term[lhs: lhs, rhs: rhs], CURSOR_TEMPLATE)

          suggestion_unit = Alloy.render(Term[
            head: Cursor.highlighted(head.to(String)),
            body: Cursor.highlighted(body.to(String)),
            current: prefix.size + 1, # start from one
            total: prefix.size + suffix.size,
          ], SUGGESTION_GROUP_TEMPLATE)

          unit = Term.of(:group, cursor_unit, suggestion_unit, style: "content flow-none")

          continue unless block = D7VR.block?(unit, term, @rem)

          postfixed(block, postfix)
        end

        matchpi %{[lhs_string | rhs_string (_*) @user]} do
          unit = Alloy.render(Term[lhs: lhs, rhs: rhs], CURSOR_TEMPLATE)

          continue unless block = D7VR.block?(unit, term, @rem)

          postfixed(block, postfix)
        end

        otherwise do
          rest.call(ctx, term, postfix)
        end
      end
    end
  end

  # :nodoc:
  def block?(unit : Term, code : Term, rem : Term::Num, inherited : Term::Dict = Term[]) : Term?
    return unless unit.type.dict?

    unit = unit.morph({:fallback, :source, code}, {:fallback, :rem, rem})
    uir = D7VR.uir(Term.of(unit), rem: rem, inherited: inherited)

    inline_charcount = UIR.approx_inline_charcount(uir)

    Term.of(:block, { {:self}, uir }, w: inline_charcount, h: 1)
  end

  TEMPLATE_COVER = ML.term <<-WWML
  (group style: "content my-2 p-3 border border-neutral-700 rounded-sm"
    (p "…" ^title "…" style: "text-xs text-neutral-400 gap-1"))
  WWML

  TEMPLATE_COMMENT = ML.term <<-WWML
  (box style: "content py-2"
    (box style: "w-max max-w-lg h-content pl-1 bg-neutral-700"
      (p ^desc style: "pl-3 text-neutral-500 bg-neutral-900 text-sm w-max leading-normal")))
  WWML

  private def instance1(document0 : Term::Dict, node0 : Term, nodepath : Stack(Int32)) : Term
    if Rhodium.passable_node?(document0, node0)
      # If node is passable, remove only its own shadow attributes so that child
      # instantiations have a chance of seeing them.
      node1 = D7.nonshadow1(node0)
    else
      # If node is impassable, remove shadow attributes recursively.
      node1 = D7.nonshadow(node0)
    end

    node1 = Term.of_case(node0) do
      # Instantiate BUTTON node.
      matchpi(
        %{(button caption_ to @_ (_*) ¦ attrs_)},
        %{(button caption_ as _ to @_ (_*) ¦ attrs_)},
        %{(button caption_ to @_ waiting @_ (_*) ¦ attrs_)},
        %{(button caption_ as _ to @_ waiting @_ (_*) ¦ attrs_)},
      ) do |caption|
        continue unless Rhodium.active?(document0, nodepath, node0)

        if ML.edge?(caption)
          continue unless caption = Rhodium.cell?(document0, nodepath, caption)
        end

        if caption.type.dict?
          # TODO: use pretty print with forced inline
          caption = Term.of(ML.display(caption, endl: false).gsub(/\s+/, ' '))
        end

        view = Term.of(:button, caption) | attrs

        node1.morph(
          {:"#view", view},
          {:"#fallback", node1},
          # Button is given hover and active automatically. They are later removed
          # so the user will likely never see them unless they e.g. observe the button
          # through (frag).
          {:hover, node1[:hover]? || false},
          {:active, node1[:active]? || false},
        )
      end

      # Instantiate H1-H6, P, SRC nodes.
      matchpi %{[(%any h1 h2 h3 h4 h5 h6 p src) content_]} do
        continue unless Rhodium.active?(document0, nodepath, node0)

        if ML.edge?(content)
          continue unless caption = Rhodium.cell?(document0, nodepath, content)

          # Instantiate caption if it's an edge.
          view = node1.morph({1, caption})
        else
          # A curious case where a node is its own view.
          view = node1
        end

        node1.morph({:"#view", view}, {:"#fallback", node1})
      end

      # Instantiate HR node.
      matchpi %{[hr]} do
        continue unless Rhodium.active?(document0, nodepath, node0)

        view = node1.morph({0, {:self, :hr, :rect}})

        node1.morph({:"#view", view}, {:"#fallback", node1})
      end

      # Instantiate COVER node.
      #
      # Note how we also replace the cover node with one without its children.
      # This is useful to avoid wasting compute on whatever is under the cover
      # later on.
      matchpi %{[cover title_ _+]} do |title|
        continue unless Rhodium.active?(document0, nodepath, node0)
        continue if Rhodium.has_cursor_at_any_depth?(document0, nodepath, node0)

        if ML.edge?(title)
          # NOTE: cover references its title from inside itself.
          continue unless title = nodepath.push(1) { Rhodium.cell?(document0, nodepath, title) }
        end

        view = Alloy.render(Term[title: title], TEMPLATE_COVER)

        Term.of(:cover, "#view": view, "#fallback": node1)
      end

      # Instantiate COMMENT node.
      matchpi %{[comment lines_string+]} do
        view = Alloy.render(Term[desc: lines.items.join('\n') { |line| line.to(String) }], TEMPLATE_COMMENT)

        node1.morph({:"#view", view}, {:"#fallback", node1})
      end

      # Instantiate VIEW node.
      matchpi %{(view view_ ¦ attrs_)} do |view|
        continue unless Rhodium.active?(document0, nodepath, node0)

        if ML.edge?(view)
          continue unless view = Rhodium.cell?(document0, nodepath, view)
        end

        node1.morph({:"#view", view | attrs}, {:"#fallback", node1})
      end

      matchpi %{(changes/view view_ @_ ¦ attrs_)} do
        continue unless Rhodium.active?(document0, nodepath, node0)

        node1.morph({:"#view", view | attrs}, {:"#fallback", node1})
      end

      # Instantiate invisible FRAG node (hide it).
      matchpi %{(frag value_ @_ ¦ _ visible: false)} do
        continue unless Rhodium.active?(document0, nodepath, node0)

        # If the node is impassable, instance() will skip it once we return;
        # thus we have to recurse manually.
        instance1(document0, value, nodepath)
      end

      # NOTE: we do not handle UNIT nodes and the cursor here. This is because
      # units require full recursion and it is too early to do it here; and cursors
      # must work at any depth, not just at passable spots. We handle both during
      # pretty printing which visits everything.

      matchpi %{[unit _ _+]} do
        continue unless Rhodium.active?(document0, nodepath, node0)
        continue if Rhodium.has_cursor_at_any_depth?(document0, nodepath, node0)

        node1.morph({:"#fallback", node1})
      end

      matchpi %{[sensor pattern_ in tspace_symbol to @_]} do
        continue unless Rhodium.active?(document0, nodepath, node0)
        continue unless in_sync = document0[Rhodium::Tspaces, tspace, :sensors, {query: pattern, secret: node0[:secret]?}]?

        node1 = node1.morph({:"#status", in_sync.true?})
      end

      matchpi %{[appearance value_ in tspace_symbol]} do
        continue unless Rhodium.active?(document0, nodepath, node0)
        continue unless in_sync = document0[Rhodium::Tspaces, tspace, :appearances, {value: value, secret: node0[:secret]?}]?

        node1 = node1.morph({:"#status", in_sync.true?})
      end

      otherwise { node1 }
    end

    # Equip anything with an inbox: (...) attribute or with hover: attribute
    # with a backlink via #extend. This includes buttons, for example.
    Term.case(node1) do
      matchpi %[{¦ inbox: _dict}], %[{¦ hover: _boolean}] do
        continue unless Rhodium.active?(document0, nodepath, node0)

        node1 = node1.morph({:"#extend", :"#backlink", nodepath})
      end

      otherwise { }
    end

    Term.of(node1)
  end

  # *Instantiation* is the first-ever step you need to do to see an arbitrary
  # *document* through µsoma. During instantiation nodes are labeled, inbox
  # addresses remembered, cells resolved, etc. The end result of instantiation
  # is ready for *printing*: see `printout`.
  def instance(document document0 : Term::Dict) : Term::Dict
    nodepath = Stack(Int32).new
    document1 = document0

    while Rhodium.successor?(document1, nodepath)
      node0 = Rhodium.follow(document1, nodepath)
      node1 = instance1(document0, node0, nodepath)
      next if node0.same?(node1)

      document1 = Rhodium.assign(document1, nodepath, node1)
    end

    # Remove shadow attributes of document1.
    document1 = D7.nonshadow1(document1)
    document1
  end

  # Returns the main pretty print chain for D7VR.
  private def ppchain(instance : Term::Dict, rem : Term::Num, inherited : Term::Dict = Term[])
    ML::Display::MAIN_CHAIN.prepend(
      AttrLeader.new(rem, inherited),
      Cursor.new(rem),
      Unit.new(instance, rem, inherited),
    )
  end

  # Returns the printout of a *node instance* of the given document *instance*.
  def node_printout(instance : Term::Dict, node_instance : Term, rem : Term::Num, inherited : Term::Dict = Term[]) : Term
    ctx = DisplayContext.new(60, 120, features: ppchain(instance, rem, inherited))
    tree = ctx.features.call(ctx, node_instance, "")
    printout, _ = flatten(ctx, tree)
    printout
  end

  # Returns the printout of an arbitrary *term*.
  #
  # This method is interesting because it does not accept an instance of some
  # kind (i.e. the result of `instance` or a part thereof); but rather,
  # an arbitrary term. In effect, this method can convert any term into
  # a printout, unlocking the rest of the rendering chain.
  def term_printout(term : Term) : Term
    ctx = DisplayContext.new(60, 120)
    tree = ctx.features.call(ctx, term, "")
    printout, _ = flatten(ctx, tree)
    printout
  end

  # *Printing* converts a D7VR document *instance* into something a bit
  # more visual -- the pretty print (or ML display) tree, along with blocks
  # and more. Most importantly, the D7VR document printout is ready to
  # become a Microfold unit; see `unit`.
  #
  # Raises `ArgumentError` if *instance* is empty.
  def printout(instance : Term::Dict, *, rem : Term::Num) : Term
    if instance.empty?
      raise ArgumentError.new("cannot print an empty document")
    end

    ctx = DisplayContext.new(60, 120, features: ppchain(instance, rem))
    tree = LayoutSet::All.thunk(Term.of(instance), "", LayoutSet::DictAligned)
    flat, _ = flatten(ctx, tree)
    flat
  end

  # Converts a D7VR document *printout* into a Microfold unit. This is where
  # D7VR ends; it is now Microfold's job to get the document to drawing.
  def unit(printout : Term, style = "") : Term
    Term.of_case(printout) do
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
        hashcode = Term.hashcode(content)
        l = 0.6 + (hashcode % 1024)*((0.9 - 0.6)/1024)
        c = 0.1 + (hashcode % 1024)*((0.25 - 0.1)/1024)
        h = hashcode % 360

        Term.of(:code, content, style: "#{style} text-{color}", color: {:oklch, l, c, h})
      end

      matchpi %{(indented child_ by: n←(%number (whole _)))} do
        unit(child, style: "#{style} content pl-#{n}")
      end

      matchpi %{(info child_ ¦ info_)} do
        unit(child) | info
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

  private def ugroup(children : Term::Dict, style : String = "") : Term
    ugroup = Term::Dict.build do |commit|
      commit << :group
      commit.with(:style, style) unless style.empty?
      commit.concat(children.items) { |child| unit(child) }
    end

    Term.of(ugroup)
  end

  # Renders an arbitrary *term* as a Microfold unit by showing it in its
  # code form (i.e. no fancy visuals such as buttons, cursor, etc.)
  def term_unit(term : Term) : Term
    pipe(term, term_printout, unit)
  end

  # Renders the given *node instance* that belongs to a document *instance*
  # as a Microfold unit.
  def document_node_unit(instance : Term::Dict, node_instance : Term, rem : Term::Num, *, inherit = Term[]) : Term
    printout = node_printout(instance, node_instance, rem, inherit)

    unit(printout)
  end

  # Renders the given *document* as a Microfold unit.
  #
  # WARNING: Raises `ArgumentError` if *document* is empty.
  def document_unit(document : Term::Dict, rem : Term::Num) : Term
    pipe(document, instance, printout(rem: rem), unit)
  end

  # Converts *unit* into UIR using `Microfold`. This method exists so that
  # there is a centralized, D7VR-controlled way to convert into UIR.
  #
  # You can obtain *unit* mainly using `term_unit` or `document_unit`.
  def uir(unit : Term, **kwargs) : Term
    Microfold.uir(Microfold::SPEC, unit, **kwargs)
  end
end
