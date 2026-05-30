module Ww::Microfold
  # Holds functions that construct utility parsers and parse strings with them.
  # A kind of very lightweight and slightly "mutant" parser combinator framework.
  module Parser
    extend self

    alias Any = Exact | Var | Number | Symbol | OneOf | Prefix | Suffix | Circumfix | Infix | And

    defrecord Exact, stem : String
    defrecord Var, name : Term::Sym
    defrecord Number, name : Term::Sym
    defrecord Symbol, name : Term::Sym
    defrecord OneOf, table : Term::Sym, name : Term::Sym
    defcase Prefix, prefix : String, stem : Any
    defcase Suffix, stem : Any, suffix : String
    defcase Circumfix, l : String, stem : Any, r : String
    defcase Infix, l : Any, infix : String, r : Any
    defcase And, arms : Slice(Any)

    private def ok_parser(parser : Any?) : Outcome::Accepted(Any?)
      Outcome.ok(parser.as(Any?))
    end

    private def ok_parser_despite(parser : Any?, *args) : Outcome::Accepted(Any?)
      Outcome.ok_despite(parser.as(Any?), *args)
    end

    # Constructs a parser from *term*, if possible. Returns `nil` otherwise.
    # Returns diagnostics related to *term* alongside, rooted at *term*.
    def of?(term : Term) : Outcome::Accepted(Any?)
      Term.case(term) do
        matchpi %{_string} do
          ok_parser(Exact.new(term.to(String)))
        end

        matchpi %{_symbol} do
          ok_parser(Var.new(term.as_sym))
        end

        matchpi %{(number arg_)} do
          unless name = arg.as_sym?
            return ok_parser_despite(nil, "`number` argument must be a name (symbol)").at(1)
          end

          ok_parser(Number.new(name))
        end

        matchpi %{(symbol arg_)} do
          unless name = arg.as_sym?
            return ok_parser_despite(nil, "`symbol` argument must be a name (symbol)").at(1)
          end

          ok_parser(Symbol.new(name))
        end

        matchpi %{(one-of arg0_ arg1_)} do
          unless table = arg0.as_sym?
            return ok_parser_despite(nil, "`one-of` table argument must be a symbol").at(1)
          end

          unless name = arg1.as_sym?
            return ok_parser_despite(nil, "`one-of` name argument must be a symbol").at(2)
          end

          ok_parser(OneOf.new(table, name))
        end

        matchpi %{(prefix arg0_ successor_)} do
          unless prefix = arg0.as_s?
            return ok_parser_despite(nil, "`prefix` argument must be a string").at(1)
          end

          stem_out = Parser.of?(successor).at(2)
          stem_out.bind do |stem|
            ok_parser(stem ? Prefix.new(prefix.to(String), stem) : nil)
          end
        end

        matchpi %{(suffix successor_ arg0_)} do
          unless suffix = arg0.as_s?
            return ok_parser_despite(nil, "`suffix` argument must be a string").at(2)
          end

          stem_out = Parser.of?(successor).at(1)
          stem_out.bind do |stem|
            ok_parser(stem ? Suffix.new(stem, suffix.to(String)) : nil)
          end
        end

        matchpi %{(circumfix arg0_ successor_ arg1_)} do
          unless l = arg0.as_s?
            return ok_parser_despite(nil, "`left` argument must be a string").at(1)
          end

          unless r = arg1.as_s?
            return ok_parser_despite(nil, "`right` argument must be a string").at(3)
          end

          stem_out = Parser.of?(successor).at(2)
          stem_out.bind do |stem|
            ok_parser(stem ? Circumfix.new(l.to(String), stem, r.to(String)) : nil)
          end
        end

        matchpi %{(infix l_ arg0_ r_)} do
          unless infix = arg0.as_s?
            return ok_parser_despite(nil, "`infix` argument must be a string").at(2)
          end

          Outcome.bind(Parser.of?(l).at(0), Parser.of?(r).at(2)) do |a, b|
            ok_parser(a && b ? Infix.new(a, infix.to(String), b) : nil)
          end
        end

        matchpi %{(and args_*)} do
          Outcome.accumulate do |acc|
            arms = Pf::Kit.stack_array(Any, 4)

            args.items.each_with_index(offset: 1) do |item, key|
              next unless arm = acc.unwrap(Parser.of?(item).at(key))

              arms << arm
            end

            ok_parser(And.new(arms.to_unsafe_readonly_slice!))
          end
        end

        otherwise do
          ok_parser_despite(nil, "invalid parser")
        end
      end
    end

    def requirements(parser : Exact) : Set(String)
      Set{parser.stem}
    end

    def requirements(parser : Var | Number | Symbol | OneOf) : Set(String)
      Set(String).new
    end

    def requirements(parser : Prefix) : Set(String)
      Set{parser.prefix} | requirements(parser.stem)
    end

    def requirements(parser : Suffix) : Set(String)
      requirements(parser.stem) | Set{parser.suffix}
    end

    def requirements(parser : Circumfix) : Set(String)
      requirements(parser.stem) | Set{parser.l, parser.r}
    end

    def requirements(parser : Infix) : Set(String)
      requirements(parser.l) | requirements(parser.r) | Set{parser.infix}
    end

    def requirements(parser : And) : Set(String)
      parser.arms.reduce(Set(String).new) do |set, arm|
        set | requirements(arm)
      end
    end

    alias Out = Outcome::Accepted(Term::Dict?) | Outcome::Rejected

    private def ok_parse(vars : Term::Dict?) : Out
      Outcome.ok(vars.as(Term::Dict?))
    end

    private def ok_parse_despite(vars : Term::Dict?, *args) : Out
      Outcome.ok_despite(vars.as(Term::Dict?), *args)
    end

    private def parse?(parser : Exact, seln : StringView) : Out
      seln == parser.stem ? ok_parse(Term[]) : Outcome.rej
    end

    private def parse?(parser : Var, seln : StringView) : Out
      ok_parse(Term[].with(parser.name, seln.to_s))
    end

    private def parse?(parser : Number, seln : StringView) : Out
      return Outcome.rej unless seln.size >= 1

      # Fast path
      seln.each_char do |chr|
        unless chr.number? || chr.in?('.', '/', '-')
          return Outcome.rej
        end
      end

      begin
        decimal = ML::Kit.decimal(seln, exact: true)
      rescue ML::SyntaxError
        return Outcome.rej
      end

      unless decimal.is_a?(Term::Num)
        return ok_parse_despite(nil, "expected a decimal number (e.g., `10`, `1/3`, `0.5`)")
      end

      ok_parse(Term[].with(parser.name, decimal))
    end

    private def parse?(parser : Symbol, seln : StringView) : Out
      ok_parse(Term[].with(parser.name, Term::Sym.new(seln.to_s)))
    end

    # Maps `(table name, key)` to a value term. See: `Codex`.
    alias Tables = Hash({Term::Sym, String}, Term)

    def parse?(tables : Tables, parser : Exact | Number | Var | Symbol, seln : StringView) : Out
      parse?(parser, seln)
    end

    def parse?(tables : Tables, parser : OneOf, seln : StringView) : Out
      unless value = tables[{parser.table, seln.to_s}]?
        return Outcome.rej
      end

      ok_parse(Term[].with(parser.name, value))
    end

    def parse?(tables : Tables, parser : Prefix, seln : StringView) : Out
      return Outcome.rej unless seln.starts_with?(parser.prefix)

      rest = seln.byte_subview(parser.prefix.bytesize, seln.bytesize)
      parse?(tables, parser.stem, rest)
    end

    def parse?(tables : Tables, parser : Suffix, seln : StringView) : Out
      return Outcome.rej unless seln.ends_with?(parser.suffix)

      prior = seln.byte_subview(0, seln.bytesize - parser.suffix.bytesize)
      parse?(tables, parser.stem, prior)
    end

    def parse?(tables : Tables, parser : Circumfix, seln : StringView) : Out
      return Outcome.rej unless seln.bytesize >= parser.l.bytesize + parser.r.bytesize
      return Outcome.rej unless seln.starts_with?(parser.l)
      return Outcome.rej unless seln.ends_with?(parser.r)

      mid = seln.byte_subview(parser.l.bytesize, seln.bytesize - parser.r.bytesize)

      parse?(tables, parser.stem, mid)
    end

    def parse?(tables : Tables, parser : Infix, seln : StringView) : Out
      seln.each_inflection do |l, r|
        next unless r.starts_with?(parser.infix)

        r = r.byte_subview(parser.infix.bytesize, r.bytesize)

        l_out = parse?(tables, parser.l, l)
        next if l_out.is_a?(Outcome::Rejected)

        r_out = parse?(tables, parser.r, r)
        next if r_out.is_a?(Outcome::Rejected)

        return (Outcome.bind(l_out, r_out) do |l_vars, r_vars|
          ok_parse(l_vars && r_vars ? Term.union(l_vars, r_vars) : nil)
        end)
      end

      ok_parse(nil)
    end

    def parse?(tables : Tables, parser : And, seln : StringView) : Out
      memo = Term[]

      parser.arms.each do |arm|
        parseout = parse?(tables, arm, seln)
        if parseout.is_a?(Outcome::Rejected)
          return Outcome.rej
        end

        unless vars = parseout.unwrap
          return parseout # Parse error in one of branches
        end

        memo = Term.union(memo, vars)
      end

      ok_parse(memo)
    end

    def parse?(tables : Tables, parser : Any, string : String) : Out
      parse?(tables, parser, string.view)
    end
  end
end
