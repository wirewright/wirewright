require "option_parser"
require "./src/wirewright"
require "./uiRb"
require "./sfpaint"
require "./pprint2"
require "./mstep4"
# require "./surfsrv"

alias UIR::Platform::Current = SFML

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
        empty: (^expr (= lhs rhs ""))
        lempty: (^expr (= lhs "")))
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
            (scroll style: "w-2 h-max pr-1"
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
        continue if Rhodium.cursor_in_node?(document0, nodepath)

        if ML.edge?(caption)
          continue unless caption = document0[Rhodium::Cells, caption]?
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
        continue if Rhodium.cursor_in_node?(document0, nodepath)

        if ML.edge?(content)
          continue unless caption = document0[Rhodium::Cells, content]?

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
        continue if Rhodium.cursor_in_node?(document0, nodepath)

        view = node1.morph({0, {:self, :hr, :rect}})

        node1.morph({:"#view", view}, {:"#fallback", node1})
      end

      # Instantiate COVER node.
      #
      # Note how we also replace the cover node with one without its children.
      # This is useful to avoid wasting compute on whatever is under the cover
      # later on.
      matchpi %{[cover title_ _+]} do |title|
        continue if Rhodium.cursor_in_node?(document0, nodepath)

        if ML.edge?(title)
          continue unless title = document0[Rhodium::Cells, title]?
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
        # TODO: relax this a little bit
        continue if Rhodium.cursor_in_node?(document0, nodepath)

        if ML.edge?(view)
          continue unless view = document0[Rhodium::Cells, view]?
        end

        node1.morph({:"#view", view | attrs}, {:"#fallback", node1})
      end

      matchpi %{(changes/view view_ @_ ¦ attrs_)} do
        # TODO: relax this a little bit
        continue if Rhodium.cursor_in_node?(document0, nodepath)

        node1.morph({:"#view", view | attrs}, {:"#fallback", node1})
      end

      # Instantiate invisible FRAG node (hide it).
      matchpi %{(frag value_ @_ ¦ _ visible: false)} do
        continue if Rhodium.cursor_in_node?(document0, nodepath)

        # If the node is impassable, instance() will skip it once we return;
        # thus we have to recurse manually.
        instance1(document0, value, nodepath)
      end

      # NOTE: we do not handle UNIT nodes and the cursor here. This is because
      # units require full recursion and it is too early to do it here; and cursors
      # must work at any depth, not just at passable spots. We handle both during
      # pretty printing which visits everything.

      matchpi %{[unit _ _+]} do
        # TODO: relax this a little bit
        continue if Rhodium.cursor_in_node?(document0, nodepath)

        node1.morph({:"#fallback", node1})
      end

      matchpi %{[sensor pattern_ in tspace_symbol to @_]} do
        continue if Rhodium.cursor_in_node?(document0, nodepath)
        continue unless in_sync = document0[Rhodium::Tspaces, tspace, :sensors, {query: pattern, secret: node0[:secret]?}]?

        node1 = node1.morph({:"#status", in_sync.true?})
      end

      matchpi %{[appearance value_ in tspace_symbol]} do
        continue if Rhodium.cursor_in_node?(document0, nodepath)
        continue unless in_sync = document0[Rhodium::Tspaces, tspace, :appearances, {value: value, secret: node0[:secret]?}]?

        node1 = node1.morph({:"#status", in_sync.true?})
      end

      otherwise { node1 }
    end

    # Equip anything with an inbox: (...) attribute or with hover: attribute
    # with a backlink via #extend. This includes buttons, for example.
    Term.case(node1) do
      matchpi %[{¦ inbox: _dict}], %[{¦ hover: _boolean}] do
        continue if Rhodium.cursor_in_node?(document0, nodepath)

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
        Term.of(:code, content, style: "#{style} text-green-400")
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
    pointee = nodepath == mouseover

    Term.case(node) do
      # If this particular button is hovered, set `hover: true`. If it is not,
      # remove the hover prop (this behavior is specific to buttons).
      matchpi %{[button _*]} do
        return Term.of(node.morph({:hover, pointee ? true : nil}))
      end

      otherwise do
        # Fallthrough
      end
    end

    if Rhodium.cursor_in_node?(@document, nodepath)
      return node
    end

    Term.case(node) do
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

demo = ML.dict <<-WWML
("" | "" () @user)

(unit group style: "flow-row gap-10"
  (p style: "p-5 text-5xl bg-neutral-700 hover:bg-blue-500 hover:cursor-pointer"
     hover: false
     mail: @hover/events
     active: false
     inbox: ()
   @hover/message)
  (view @hover/rect))

(cover "guts"
  (cell @hover/rect)
  (latest @hover/rects @hover/rect)
  (alloy (@hover/updates to @hover/rects)
    ((self rect) style: "w-10 h-max bg-[color]" color: ^color))
  (cell "Hover or click me" @hover/message)
  (transform (@hover/events (mail (hover)) to @colors) red-500)
  (transform (@hover/events (mail (hover)) to @hover/messages) "Hovering!")
  (transform (@hover/events (mail (unhover)) to @colors) green-500)
  (transform (@hover/events (mail (unhover)) to @hover/messages) "Hover or click me")
  (transform (@hover/events (mail (press)) to @colors) blue-500)
  (transform (@hover/events (mail (press)) to @hover/messages) "Pressed!")
  (latest @hover/messages @hover/message)
  (transform (@colors to @hover/updates) {color: _}))

(hr)

(unit group style: "max max-sm bg-neutral-800 flow-col gap-5 fr p-3"
  (view @color-rect)
  (unit group style: "w-max h-content flow-row gap-5 fr"
    (button "Make red" as red-500 to @colors () style: "w-fr")
    (button "Make green" as green-500 to @colors () style: "w-fr")
    (button "Make blue" as blue-500 to @colors () style: "w-fr")))

(cover "guts"
  (transform (@colors to @color-envs) {color: _})
  (cell @color-rect)
  (alloy (@color-envs to @color-rects)
    ((self rect) color: ^color style: "w-max h-fr bg-[color] rounded"))
  (latest @color-rects @color-rect)
  (event (pulse @colors red-500)))

(hr)

(h1 "Heading 1")
(h2 "Heading 2")
(h3 "Heading 3")
(h4 "Heading 4")
(h5 "Heading 5")
(h6 "Heading 6")
(p "Simple text")
(hr)
(p "Show counter using text:")
(hr)
(p @count style: "text-xl font-bold text-green-500")
(hr)

(unit group style: "content bg-neutral-800 gap-5 p-5 flow-col"
  (h1 @count)
  (button "Increment" as 1 to @deltas ())
  (button "Decrement" as -1 to @deltas ()))

(unit group style: "content flow-col gap-5 bg-neutral-800 p-5"
  (unit group style: "w-max h-content center-x"
    (p @count style: "text-7xl leading-none font-bold text-neutral-100"))
  (unit group style: "content flow-row gap-5"
    (button "Increment" as 1 to @deltas ())
    (button "Decrement" as -1 to @deltas ())))

(transform (@counts to @count-envs) {count: _})
(initial @count to @counts)

(comment "Lorem ipsum dolor sit amet, officia excepteur ex fugiat reprehenderit enim labore culpa sint ad nisi Lorem pariatur mollit ex esse exercitation amet. Nisi anim cupidatat excepteur officia. Reprehenderit nostrud nostrud ipsum Lorem est aliquip amet voluptate voluptate dolor minim nulla est proident. Nostrud officia pariatur ut officia. Sit irure elit esse ea nulla sunt ex occaecat reprehenderit commodo officia dolor Lorem duis laboris cupidatat officia voluptate. Culpa proident adipisicing id nulla nisi laboris ex in Lorem sunt duis officia eiusmod. Aliqua reprehenderit commodo ex non excepteur duis sunt velit enim. Voluptate laboris sint cupidatat ullamco ut ea consectetur et est culpa et culpa duis.")
(cell 0 @count)
(transform (@deltas delta_number to @counts with @count) (+ count delta))
(latest @counts @count)
WWML

fb_loop = ML.dict <<-WWML
(sensor x_number in remote to @stimuli/in)

(cover "sensor queue"
  (queue @stimuli/in to @stimuli/gated in () waiting @sensor/acks))

(transform (@stimuli/gated to @stimuli/0) (nth _ 0))
(transform (@stimuli/0 (some ({¦ x_} _)) to @percepts) x)
(bridge (@stimuli/0 (none)) to @sensor/acks)
(bridge @percepts to @sensor/acks)
(latest @percepts @percept)

(comment "This cell is the origin & subject of the feedback loop")
(cell @percept)

(frag
  (group
   (button "Start loop" as 0 to @percept/starters ())
   (latest @percept/starters @percept)
   (bridge @percept/starters as destroy to @igniter))
  @igniter)

(comment "Publish percept as an appearance")
(changes @percept to @appearance/in)

(cover "appearance queue"
  (queue @appearance/in to @appearance/percepts in () waiting @appearance/acks))

(transform (@appearance/percepts to @appearance/values) (+ _ 1))
(transform (@appearance/values to @appearances) (appearance _ in remote))
(latest @appearances @appearance)
(frag @appearance)
(changes @appearance to @appearance/acks)

("" | "" () @user)
WWML

welcome = ML.dict <<-WWML
(unit group style: "w-max h-content flow-col gap-3 max-w-4xl"
  (h1 "Welcome to µsoma, a GUI for Wirewright!")
  (hr style: "bg-neutral-600")
  (unit group style: "w-max h-content flow-col gap-3 text-sm text-neutral-300"
    (p "µsoma to Wirewright is roughly what a web browser is to the Internet." style: "w-max")
    (p "You're looking at a *self-embodied program*. Well, sort of — it only contains some text nodes right now. Hit left/right arrow to see for yourself." style: "w-max")
    (p "Try typing the following:" style: "w-max")
    (src style: "w-max text-sm rounded"
      "(h1 @count)
       (cell 0 @count)
       (button \\"Increment\\" as 1 to @deltas ())
       (button \\"Decrement\\" as -1 to @deltas ())
       (transform (@deltas to @counts with @count) (+ count _))
       (latest @counts @count)")
    (p "Click on the buttons and see what happens! :^)" style: "w-max text-sm")
    (unit ul style: "w-max h-content flow-col gap-1 pl-3 text-sm"
      (p "- Drag on empty/non-clickable space to pan around if something overflows." style: "w-max")
      (p "- Use Ctrl-Plus to zoom in and Ctrl-Minus to zoom out." style: "w-max")
      (p "- Use Ctrl-Z to undo and Ctrl-R to redo (experimental)." style: "w-max")
      (p "- Hit F2-F3 to replace this document with more sophisticated demos." style: "w-max")
      (p "- Hit Ctrl-Backspace to remove this comment (and any *node* before the cursor in general)." style: "w-max")
      (p "- Hit Enter to escape from a pair." style: "w-max")
      (p "- Play! The semi-readable implementation of this editor is in `editor.soma.wwml`; check it out for key bindings & what they do" style: "w-max")
      (p "- Take a look at D7 tests: `delta7.test.wwml`. Plenty of examples in there." style: "w-max"))))

("" | "" () @user)

WWML

test = ML.dict <<-WWML
(sensor x_number in local to @xs)
(appearance 100 in local)

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

OptionParser.parse do |parser|
  parser.banner = "Usage: soma [arguments]"
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

seed = welcome
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

      matchpi %{(key f3)} do
        doc.send(Term.of(:open, fb_loop))
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

  instance = Alloy.render(vars: frame[:".model"].as_d, template: Term.of(frame.without(:".model")), strict: true).as_d

  # Send instance to drawing
  Term.of(instance)
end

UIR::Platform::Current.show(ui)
