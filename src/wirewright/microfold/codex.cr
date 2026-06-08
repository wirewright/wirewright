module Ww::Microfold
  alias Defn = UtilityDefn | AliasDefn | ShorthandDefn | PropertyDefn |
               BoxDefn | BoxCascadeDefn | BoxOrderDefn

  defrecord UtilityDefn,
    ref : Term,
    parser : Parser::Any,
    box : Term,
    name : Term?,
    contrib : Term,
    cascade_pref : CascadePref

  # Utility cascade preference.
  alias CascadePref = CascadePrefUnset | CascadePrefAll | CascadePrefExcept

  # Unspecified: Cascade preference for a utility is not specified and must
  # be checked by other means (e.g. `Codex#cascade?`).
  defrecord CascadePrefUnset

  # Target subtree: Utility wants to participate in the cascade.
  defrecord CascadePrefAll

  # Target subtree excluding: Utility wants to participate in the cascade,
  # but nodes in the subtree whose head is listed in *exceptions* must
  # ignore it.
  defrecord CascadePrefExcept, exceptions : Slice(Term::Sym)

  defrecord AliasDefn,
    ref : Term,
    input : String,
    expansion : String

  defrecord ShorthandDefn,
    ref : Term,
    parser : Parser::Any,
    name : Term?,
    calls : Slice(ShorthandCall)

  defrecord ShorthandCall, callee : Term, args : Term::Dict

  defrecord PropertyDefn, ref : Term, key : Term, dst : Term, box : Term

  defrecord BoxDefn, ref : Term, name : Term, pattern : Term, template : Term
  defrecord BoxCascadeDefn, ref : Term, names : Slice(Term)
  defrecord BoxOrderDefn, ref : Term, names : Slice(Term)

  # A Microfold codex, in the abstract, contains definitions for Microfold
  # utilities, boxes, etc. If Microfold was a programming language, the codex
  # would be its standard library.
  #
  # This `Codex` object is a parsed, indexed, machine-friendly representation of
  # the WwML codex (which lives at `runtime/codices/ufold.codex.wwml`).
  #
  # The codex also holds caches, so it is **not thread-safe**. Since the caches
  # used here are `GenerationalCache`s, just like in Scenery, it's advised that
  # you have a codex for each "frame source", as in, an entity producing a sequence
  # of frames.
  #
  # `SyncCodex` must be used for thread-safety (in fact, it is the only accepted
  # kind of codex in high-level functions such as `codex` and `render`, so it is
  # harder for clients to mess up).
  class Codex
    # :nodoc:
    getter feature_cache
    # :nodoc:
    getter eval_cache
    # :nodoc:
    getter recognize_cache
    # :nodoc:
    getter designate_cache
    # :nodoc:
    getter instantiate_cache

    # Returns the evaluated global variables dict (section `globals`).
    getter globals : Term::Dict

    # :nodoc:
    def initialize(
      @globals : Term::Dict,
      @presets : Hash(Term, FeatureSeq),
      @tables : Hash({Term::Sym, String}, Term),
      @utilities : Array(UtilityDefn | ShorthandDefn),
      @utilities_by_name : Hash(Term, UtilityDefn | ShorthandDefn),
      @aliases : Hash(String, AliasDefn),
      @properties : Hash(Term, PropertyDefn),
      @boxes : Hash(Term, BoxDefn),
      @order : Slice(Term),
      @cascade : Slice(Term),
    )
      @feature_cache = GenerationalCache({StyleOrigin, String}, Outcome::Accepted(FeatureSeq)).new
      @recognize_cache = GenerationalCache(Term::Dict, Outcome::Accepted(StyleThunk)).new
      @eval_cache = GenerationalCache({EvalContext, FeatureSeq}, FeatureEvaluation).new
      @designate_cache = GenerationalCache(UncuedStyleNode, DirectedDesignationNode?).new
      @instantiate_cache = GenerationalCache({Term::Dict, DirectedDesignationNode, Slice(DownboundDesignation)}, {Outcome::Accepted(Term), Slice(UpboundDesignation)}).new
    end

    # *Forks* this codex.
    #
    # Microfold codices are used for caching. Sometimes, you will need to
    # have multiple copies of the same underlying codex to apply them in different
    # places where you want cache boundaries to exist.
    #
    # One way to do this would be to call `compile` multiple times, but
    # this is expensive.
    #
    # `clone` is also rather expensive, and it is also intrusive, and it
    # will clone the caches, too.
    #
    # Instead, you should use `fork`. Since everything except the caches
    # in a codex is immutable, `fork` is vastly cheaper, as it only has
    # to copy some references and allocate clean caches.
    def fork : Codex
      Codex.new(
        @globals, @presets, @tables, @utilities, @utilities_by_name,
        @aliases, @properties, @boxes, @order, @cascade,
      )
    end

    # Minimum possible value for rem (clamp).
    MIN_REM_PX = 4

    # Maximum possible value for rem (clamp).
    MAX_REM_PX = 128

    # Constructs a `Codex` object based on the codex dict *codex*, and *rem*, the base
    # font size (in pixels).
    def self.compile(codex : Term::Dict, rem : Term::Num) : Outcome::Accepted(Codex)
      rem = Math.min(Math.max(rem, Term[MIN_REM_PX]), Term[MAX_REM_PX])

      globals_section = codex[:globals]?.as_d? || Term[]
      globals_out = globals(globals_section.with(:rem, rem)).at(:globals)

      presets_section = codex[:presets].as_d? || Term[]
      presets_out = presets(presets_section).at(:presets)

      tables_out = tables(globals_out.unwrap, codex)

      defns_section = codex[:definitions].as_d? || Term[]
      defns_out = definitions(defns_section).at(:definitions)

      Outcome.bind(globals_out, presets_out, tables_out, defns_out) do |globals, presets, tables, defns|
        Outcome.accumulate do |acc|
          utilities = [] of UtilityDefn | ShorthandDefn
          utilities_by_name = {} of Term => UtilityDefn | ShorthandDefn
          aliases = {} of String => AliasDefn
          properties = {} of Term => PropertyDefn
          boxes = {} of Term => BoxDefn
          order = Slice(Term).empty
          cascade = Slice(Term).empty

          # ... then extract various parts into more efficient data structures.
          defns.each do |defn|
            case defn
            in UtilityDefn, ShorthandDefn
              utilities << defn
              next unless name = defn.name

              if utilities_by_name.has_key?(name)
                acc << Diagnostic.of("duplicate utility name").at(:diagnostics, defn.ref)
                next
              end

              utilities_by_name[name] = defn
            in AliasDefn
              if aliases.has_key?(defn.input)
                acc << Diagnostic.of("duplicate alias definition").at(:diagnostics, defn.ref)
                next
              end

              aliases[defn.input] = defn
            in PropertyDefn
              if properties.has_key?(defn.key)
                acc << Diagnostic.of("duplicate property definition").at(:diagnostics, defn.ref)
                next
              end

              properties[defn.key] = defn
            in BoxDefn
              if boxes.has_key?(defn.name)
                acc << Diagnostic.of("duplicate box definition").at(:diagnostics, defn.ref)
                next
              end

              boxes[defn.name] = defn
            in BoxCascadeDefn
              if cascade.present?
                acc << Diagnostic.of("duplicate box cascade definition").at(:diagnostics, defn.ref)
                next
              end

              cascade = defn.names
            in BoxOrderDefn
              if order.present?
                acc << Diagnostic.of("duplicate box order definition").at(:diagnostics, defn.ref)
                next
              end

              order = defn.names
            end
          end

          Outcome.ok(new(globals, presets, tables, utilities, utilities_by_name, aliases, properties, boxes, order, cascade))
        end
      end
    end

    private def self.global(table, expr : Term, seen : Pf::Set(Term)) : Outcome::Accepted(Term?)
      Term.case(expr) do
        matchpi %{(* l_ r_)} do
          global(table, l, seen).at(1).bind do |a|
            global(table, r, seen).at(2).bind do |b|
              Outcome.ok(a && b ? Term.of(a * b) : nil)
            end
          end
        end

        matchpi %{_dict} do
          Outcome.ok_despite(nil.as(Term?), "unrecognized expression")
        end

        matchpi %{_symbol} do
          unless subexpr = table[expr]?
            return Outcome.ok_despite(nil.as(Term?), "undefined global variable `#{expr}`")
          end

          # Add *expr* to the seen set, to prevent infinite evaluation such as in:
          #
          #   foo: bar
          #   bar: foo
          global(table, subexpr, seen.add(expr)).at(expr)
        end

        otherwise do
          Outcome.ok(expr.as(Term?))
        end
      end
    end

    private def self.global(table, expr : Term) : Outcome::Accepted(Term?)
      global(table, expr, seen: Pf::Set(Term).new)
    end

    # Evaluates the `globals` *section* of the codex. Returns the evaluated section.
    # Diagnostics are rooted at *section*.
    #
    # For example, the following globals section:
    #
    # ```wwml
    # rem: 16
    # spacing: (* 0.25 (global rem))
    # -rem: (* -1 (global rem))
    # -spacing: (* -1 (global spacing))
    # ```
    #
    # ... evaluates to:
    #
    # ```wwml
    # rem: 16
    # spacing: 4
    # -rem: -16
    # -spacing: -4
    # ```
    #
    # We do not use Alloy as the outer interpreter because we actually require graph-
    # walking here, since the order of definitions is arbitrary. Cycles are terminated
    # by path-tracking. If we ever need to, Alloy can be used as the inner interpreter
    # (e.g. you can collect globals ahead-of-time and then do Alloy with them). The approach
    # used here reminds me of [miniKanren](https://minikanren.org/).
    private def self.globals(section : Term::Dict) : Outcome::Accepted(Term::Dict)
      Outcome.accumulate do |acc|
        globals = Term::Dict.build do |commit|
          section.each_entry do |key, expr|
            value = acc.unwrap(global(section, expr).at(key))
            commit.with(key, value)
          end
        end

        Outcome.ok(globals)
      end
    end

    # Parses presets in the`presets` *section* of the codex. Returns the evaluated
    # section. Diagnostics are rooted at *section*.
    private def self.presets(section : Term::Dict) : Outcome::Accepted(Hash(Term, FeatureSeq))
      Outcome.accumulate do |acc|
        presets = Hash(Term, FeatureSeq).new(initial_capacity: section.size)

        section.each_entry do |head, value|
          unless style = value.as_s?
            acc << Diagnostic.of("style is not a string").at(head)
            next
          end

          preset = acc.unwrap(Microfold.features(:preset, style.to(String)))
          presets[head] = preset
        end

        Outcome.ok(presets)
      end
    end

    # Finds and evaluates all tables defined in *codex*. Returns the resulting map
    # of `(table name, key)` to evaluated values. Diagnostics are rooted at *codex*.
    private def self.tables(globals : Term::Dict, codex : Term::Dict) : Outcome::Accepted(Hash({Term::Sym, String}, Term))
      Outcome.accumulate do |acc|
        tables = {} of {Term::Sym, String} => Term

        codex.each_entry do |key, section|
          next unless section = section.as_d?

          Term.matchpi?(key, %{(table name-arg_)}) do
            unless table_name = name_arg.as_sym?
              acc << Diagnostic.of("table name must be a symbol").at(key)
              next
            end

            section.each_entry do |table_key, table_expr|
              unless lhs = table_key.as_s?
                acc << Diagnostic.of("table key must be a string").at(key, table_key)
                next
              end

              rhs = Alloy.render(globals, table_expr)

              tables[{table_name, lhs.to(String)}] = rhs
            end
          end
        end

        Outcome.ok(tables)
      end
    end

    # Parses definitions in the `definitions` *section* of the codex. Returns
    # the resulting array of `Defn`s. This is a "primary" parse: more work is
    # needed to separate and index the various `Defn`s for efficient access.
    private def self.definitions(section : Term::Dict) : Outcome::Accepted(Array(Defn))
      Outcome.accumulate do |acc|
        defns = [] of Defn

        section.items.each_with_index do |item, item_key|
          item_ref = Term.of(item_key)

          Term.case(item) do
            matchpi %{(defn name_ (shorthand form_ rest_*))} do
              next unless parser = acc.unwrap(Parser.of?(form).at(item_key))

              calls = Pf::Kit.stack_array(ShorthandCall, 4)
              rest.items.each do |call|
                Term.matchpi?(call, %{(callees_* ¦ args_)}) do
                  callees.items.each do |callee|
                    calls << ShorthandCall.new(callee, args.as_d)
                  end
                end
              end

              defns << ShorthandDefn.new(item_ref, parser, name, calls.to_readonly_slice)
            end

            matchpi %{(defn (box_ name_) defn←[utility form_ contrib_])} do
              next unless parser = acc.unwrap(Parser.of?(form).at(item_key))

              cascade_pref = acc.unwrap(cascade_pref(defn).at(item_ref, 1))
              defns << UtilityDefn.new(item_ref, parser, box, name, contrib, cascade_pref)
            end

            matchpi %{(for box_ defn←[utility form_ contrib_])} do
              next unless parser = acc.unwrap(Parser.of?(form).at(item_key))

              name = nil
              cascade_pref = acc.unwrap(cascade_pref(defn).at(item_ref, 1))
              defns << UtilityDefn.new(item_ref, parser, box, name, contrib, cascade_pref)
            end

            matchpi %{(for box_ (property key_ as: dst_))} do
              defns << PropertyDefn.new(item_ref, key, dst, box)
            end

            matchpi %{(for box_ (property keys_*))} do
              keys.items.each do |key|
                defns << PropertyDefn.new(item_ref, key, key, box)
              end
            end

            matchpi %{(alias input_string expansion_string)} do
              defns << AliasDefn.new(item_ref, input.to(String), expansion.to(String))
            end

            matchpi %{(group children_*)} do
              children.items.each_with_index(offset: 1) do |child, child_key|
                defns.concat(acc.unwrap(definitions(Term[{child}]).at(child_key)))
              end
            end

            matchpi %{(defn name_ (box pattern_ template_))} do
              defns << BoxDefn.new(item_ref, name, pattern, template)
            end

            matchpi %{(box-cascade names_*)} do
              defns << BoxCascadeDefn.new(item_ref, names.items.to_readonly_slice(&.itself))
            end

            matchpi %{(box-order names_*)} do
              defns << BoxOrderDefn.new(item_ref, names.items.to_readonly_slice(&.itself))
            end

            otherwise do
              acc << Diagnostic.of("unrecognized item").at(item_ref)
            end
          end
        end

        Outcome.ok(defns)
      end
    end

    private def self.cascade_pref(defn : Term) : Outcome::Accepted(CascadePref)
      Term.case(defn) do
        matchpi %{{¦ -cascade}} do
          Outcome.ok(CascadePrefUnset.new.as(CascadePref))
        end

        matchpi %{{¦ cascade: true}} do
          Outcome.ok(CascadePrefAll.new.as(CascadePref))
        end

        matchpi %{{¦ cascade: (except exceptions_symbol*)}} do
          Outcome.ok(CascadePrefExcept.new(exceptions.items.to_readonly_slice(&.as_sym)).as(CascadePref))
        end

        otherwise do
          Outcome.ok_despite(CascadePrefUnset.new.as(CascadePref), "unrecognized `cascade` setting on utility")
        end
      end
    end

    # Returns the preset for a node *head*, if any; `nil` otherwise.
    #
    # A node head is e.g. `p` in `(p "hello world")` or `group` in `(group "A" "B" "C")`.
    def preset?(head : Term) : FeatureSeq?
      @presets[head]?
    end

    # Looks up the alias definition for *matchee*.
    def alias?(matchee : String) : AliasDefn?
      @aliases[matchee]?
    end

    # Looks up the property definition for *key*.
    def property?(key : Term) : PropertyDefn?
      @properties[key]?
    end

    # Returns `true` if *box* participates in the cascade.
    def cascade?(box : Term) : Bool
      box.in?(@cascade)
    end

    # Looks up a utility or shorthand definition by its *name*. Names are
    # optional for utilities; anonymous utilities cannot be found this way.
    def find_by_name?(name : Term) : UtilityDefn | ShorthandDefn | Nil
      @utilities_by_name[name]?
    end

    defrecord Match, vars : Term::Dict, defn : UtilityDefn | ShorthandDefn

    # Lets utilities and shorthands look at *matchee*. Returns the resulting `Match`,
    # or `nil` if an error occurred while the utility or shorthand was parsing
    # *matchee*. Rejects if nothing matched.
    #
    # If the same *matchee* excited several utilities or shorthands, this method
    # considers only the first such excitation; the meaning of "first" here being
    # implementation-defined.
    def match?(matchee : String) : Outcome::Accepted(Match?) | Outcome::Rejected
      @utilities.each do |defn|
        parseout = Parser.parse?(@tables, defn.parser, matchee)
        match = parseout.map { |vars| vars ? Match.new(vars, defn) : nil }

        case match
        in Outcome::Accepted then return match
        in Outcome::Rejected
        end
      end

      Outcome.rej
    end

    enum InstantiateMode
      # Pass base and designations as-is.
      Call

      # ```
      # (el) -> (box (el))
      # ```
      Surround

      # ```
      # (el) -> (el (box))
      # ```
      Nest
    end

    # Instantiates the box named *box* over *base*, the box equipped with *designations*.
    # Some instantiations may be noop, resulting in *base* (this depends on the semantics
    # of *box* and how it works with *designations*, and whether such a box exists at all)
    def instantiate(box : Term, base : Term, designations : Term::Dict, mode : InstantiateMode) : Term
      return base unless defn = @boxes[box]?

      case mode
      in .call?
        unless vars = M1.match?(defn.pattern, Term.of(base, designations))
          return base
        end

        Alloy.render(vars, defn.template)
      in .surround?
        unless vars = M1.match?(defn.pattern, Term.of({base}, designations))
          return base
        end

        result = Alloy.render(vars, defn.template)
        return base unless response = result.as_d?
        return base unless node = response[0]?

        Term.union(node, Term.of(response.pairspart))
      in .nest?
        return base unless dict = base.as_d?
        return base unless head = dict[0]?

        children = dict.items.move(1)
        unless vars = M1.match?(defn.pattern, Term.of(children, designations))
          return base
        end

        result = Alloy.render(vars, defn.template)

        Term.of(Term.union(result.prepend(head), dict.pairspart))
      end
    end

    # Yields the names of all defined boxes bottom-up (most nested to least nested).
    def each_box_bottom_up(& : Term ->) : Nil
      @order.each { |box| yield box }
    end

    # Clears all underlying caches.
    def invalidate : Nil
      @feature_cache.clear
      @eval_cache.clear
      @recognize_cache.clear
      @designate_cache.clear
      @instantiate_cache.clear
    end
  end
end
