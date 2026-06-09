# Nitrene is an expression language for Wirewright.
module Ww::Nitrene
  extend self

  # :nodoc:
  alias Arith = ArithConst | ArithPosInf | ArithNegInf | ArithIndet

  # :nodoc:
  defrecord ArithConst, value : Term::Num
  # :nodoc:
  defrecord ArithPosInf
  # :nodoc:
  defrecord ArithNegInf
  # :nodoc:
  defrecord ArithIndet

  # :nodoc:
  SYM_NEG_INFINITY = Term.of(:"-∞")
  # :nodoc:
  SYM_INFINITY = Term.of(:∞)
  # :nodoc:
  SYM_INDET = Term.of(:indet)

  # :nodoc:
  def arith?(term : Term) : Arith?
    case term
    when .subtype?(:number)
      ArithConst.new(term.as_n)
    when SYM_INFINITY
      ArithPosInf.new
    when SYM_NEG_INFINITY
      ArithNegInf.new
    when SYM_INDET
      ArithIndet.new
    end
  end

  # :nodoc:
  def render(a : Arith) : Term
    case a
    in ArithConst  then Term.of(a.value)
    in ArithPosInf then SYM_INFINITY
    in ArithNegInf then SYM_NEG_INFINITY
    in ArithIndet  then SYM_INDET
    end
  end

  # NOTE: The behavior of Arith operations is mostly based on [Wolfram Mathematica](https://www.wolfram.com/mathematica/),
  # which seems to be a good reference for this kind of stuff.

  # :nodoc:
  def add(a : Arith, b : Arith) : Arith
    case {a, b}
    in {ArithConst, ArithConst}
      ArithConst.new(a.value + b.value)
    in {ArithPosInf, ArithConst},
       {ArithConst, ArithPosInf},
       {ArithPosInf, ArithPosInf}
      ArithPosInf.new
    in {ArithNegInf, ArithConst},
       {ArithConst, ArithNegInf},
       {ArithNegInf, ArithNegInf}
      ArithNegInf.new
    in {ArithPosInf, ArithNegInf},
       {ArithNegInf, ArithPosInf},
       {ArithIndet, _},
       {_, ArithIndet}
      ArithIndet.new
    end
  end

  # :nodoc:
  def negate(a : Arith) : Arith
    case a
    in ArithConst  then ArithConst.new(-a.value)
    in ArithPosInf then ArithNegInf.new
    in ArithNegInf then ArithPosInf.new
    in ArithIndet  then ArithIndet.new
    end
  end

  # :nodoc:
  def sub(a : Arith, b : Arith) : Arith
    case {a, b}
    in {ArithConst, ArithConst}         then ArithConst.new(a.value - b.value)
    in {ArithConst, ArithPosInf}        then ArithNegInf.new
    in {ArithPosInf, ArithConst}        then ArithPosInf.new
    in {ArithConst, ArithNegInf}        then ArithPosInf.new
    in {ArithNegInf, ArithConst}        then ArithNegInf.new
    in {ArithPosInf, ArithPosInf}       then ArithIndet.new
    in {ArithPosInf, ArithNegInf}       then ArithPosInf.new
    in {ArithNegInf, ArithPosInf}       then ArithNegInf.new
    in {ArithNegInf, ArithNegInf}       then ArithIndet.new
    in {ArithIndet, _}, {_, ArithIndet} then ArithIndet.new
    end
  end

  # :nodoc:
  def mul(a : Arith, b : Arith) : Arith
    case {a, b}
    in {ArithConst, ArithConst}
      ArithConst.new(a.value * b.value)
    in {ArithConst, ArithPosInf},
       {ArithPosInf, ArithConst}
      ArithPosInf.new
    in {ArithConst, ArithNegInf},
       {ArithNegInf, ArithConst}
      ArithNegInf.new
    in {ArithPosInf, ArithPosInf}
      ArithPosInf.new
    in {ArithPosInf, ArithNegInf},
       {ArithNegInf, ArithPosInf}
      ArithNegInf.new
    in {ArithNegInf, ArithNegInf}
      ArithPosInf.new
    in {ArithIndet, _}, {_, ArithIndet}
      ArithIndet.new
    end
  end

  # :nodoc:
  def div(a : Arith, b : Arith) : Arith
    case {a, b}
    in {ArithConst, ArithConst}
      if b.value.zero?
        ArithIndet.new
      else
        ArithConst.new(a.value / b.value)
      end
    in {ArithConst, ArithPosInf}
      ArithConst.new(Term[0])
    in {ArithPosInf, ArithConst}
      ArithPosInf.new
    in {ArithConst, ArithNegInf}
      ArithConst.new(Term[0])
    in {ArithNegInf, ArithConst}
      ArithNegInf.new
    in {ArithPosInf, ArithPosInf},
       {ArithPosInf, ArithNegInf},
       {ArithNegInf, ArithPosInf},
       {ArithNegInf, ArithNegInf},
       {ArithIndet, _}, {_, ArithIndet}
      ArithIndet.new
    end
  end

  # :nodoc:
  def lt?(a : ArithConst | ArithPosInf | ArithNegInf, b : ArithConst | ArithPosInf | ArithNegInf) : Bool
    case {a, b}
    in {ArithConst, ArithConst}   then a.value < b.value
    in {ArithConst, ArithPosInf}  then true
    in {ArithPosInf, ArithConst}  then false
    in {ArithConst, ArithNegInf}  then false
    in {ArithNegInf, ArithConst}  then true
    in {ArithPosInf, ArithPosInf} then false
    in {ArithPosInf, ArithNegInf} then false
    in {ArithNegInf, ArithPosInf} then true
    in {ArithNegInf, ArithNegInf} then false
    end
  end

  private def calc(term : Term, args : Indexable(Term), *, allow : T.class = Arith, &) forall T
    operands = Pf::Kit.stack_array(T, 8)

    args.each do |arg|
      next unless operand = arith?(arg).as?(T)

      operands << operand
    end

    unless args.size == operands.size
      return term
    end

    yield operands
  end

  # TODO: Since we depend on libunibreak anyway we should use it here!
  module WordTokenizer
    extend self

    # Reference: https://github.com/microsoft/vscode/blob/7dd556f54d68b8ac6c15ca27566acc6d0f3c1f9a/src/vs/editor/common/config/editorOptions.ts#L101
    #
    # Added WwML-specific delimiters.
    #
    # TODO: this set belongs to editR.codex.wwml, and must be read from the kernel
    # and configurable.
    def wsep?(char : Char)
      char.in_set?("~!@#$%^&*()\\-=+[{]}\\|;:'\",.<>/?←→↑↓¦⍊⟨⟩⟪⟫")
    end

    def wdrop(text : StringView, head : StringView -> Char, tail : StringView -> StringView)
      initial = text

      if text.nonempty? && head.call(text).vspace?
        return tail.call(text)
      end

      # Skip whitespace at which we're currently standing, if we are, as in
      # `hello⏏    world` -> `hello    ⏏world`, or in `hel⏏lo world` this would
      # be noop.
      while text.nonempty? && head.call(text).hspace?
        text = tail.call(text)
      end

      wseps = false

      # Like VSCode, skip word separators, if any.
      while text.nonempty? && wsep?(head.call(text))
        text = tail.call(text)
        wseps = true
      end

      # If we managed to skip some word separators, that's it.
      if wseps
        return text
      end

      # Skip until word separator.
      until text.empty? || (head.call(text).whitespace? || wsep?(head.call(text)))
        text = tail.call(text)
      end

      text
    end

    def lwdrop(text : StringView) : StringView
      wdrop(text, head: ->(view : StringView) { view.first_char }, tail: ->(view : StringView) { view.rest })
    end

    def rwdrop(text : StringView) : StringView
      wdrop(text, head: ->(view : StringView) { view.last_char }, tail: ->(view : StringView) { view.prior })
    end

    def lwdrop(text : StringView, n : Int) : StringView
      n.times { text = lwdrop(text) }

      text
    end

    def rwdrop(text : StringView, n : Int) : StringView
      n.times { text = rwdrop(text) }

      text
    end

    def lwtake(text : StringView, n : Int) : StringView
      rest = lwdrop(text, n)

      StringView.between(text.before_begin, rest.before_begin)
    end

    def rwtake(text : StringView, n : Int) : StringView
      rest = rwdrop(text, n)

      StringView.between(rest.after_end, text.after_end)
    end

    # NOTE: *e* is inclusive!
    def words(text : StringView, b : Int, e : Int)
      text = b.negative? ? rwtake(text, b.abs) : lwdrop(text, b)
      text = e.negative? ? rwdrop(text, e.abs - 1) : lwtake(text, e - b + 1)
      text
    end
  end

  private def idfs(depth : UInt32, dict0 : Term::Dict, &fn : Term -> Term) : {Term::Dict, Bool}
    dict1 = dict0
    deeper = false

    dict0.each_entry do |key, value0|
      if depth.zero?
        value1 = fn.call(value0)
        deeper ||= value0.type.dict?
      elsif child0 = value0.as_d?
        child1, child_deeper = idfs(depth - 1, child0, &fn)
        value1 = Term.of(child1)
        deeper ||= child_deeper
      else
        next
      end

      next if value0 == value1

      dict1 = dict1.with(key, value1)
    end

    {dict1, deeper}
  end

  private def idfs(dict0 : Term::Dict, *, limit : UInt32 = UInt32::MAX, &fn : Term -> Term)
    dict1 = dict0

    limit.times do |depth|
      dict1, deeper = idfs(depth, dict1, &fn)
      break unless deeper
      break unless dict0 == dict1
    end

    dict1
  end

  alias Eval = Interpreter, Term::Dict, Term -> Term

  # Represents a Nitrene interpreter.
  #
  # FIXME: Flip order
  defrecord Interpreter, composite : Eval, primitive : Eval

  struct Interpreter
    DEFAULT = new(Nitrene.composite, Nitrene.primitive)
  end

  def composite
    ->Nitrene.composite(Interpreter, Term::Dict, Term)
  end

  def primitive
    ->Nitrene.primitive(Interpreter, Term::Dict, Term)
  end

  def either(a : Eval, b : Eval) : Eval
    ->(it : Interpreter, vars : Term::Dict, expr : Term) do
      value = a.call(it, vars, expr)
      unless value == expr
        return value
      end

      b.call(it, vars, expr)
    end
  end

  def eval(it : Interpreter, vars : Term::Dict, expr : Term) : Term
    value = it.composite.call(it, vars, expr)
    unless expr == value
      return value
    end

    # Evaluate recursively.
    if dict = expr.as_d?
      dict = dict.transaction do |commit|
        dict.each_entry do |key, value|
          commit.with(key, eval(it, vars, value))
        end
      end

      expr = Term.of(dict)
    end

    value = it.primitive.call(it, vars, expr)
    unless expr == value
      return value
    end

    vars[expr]? || expr
  end

  # See `Interpreter`.
  def composite(it : Interpreter, vars : Term::Dict, expr : Term) : Term
    unless expr.type.dict?
      return expr
    end

    Term.case(expr) do
      matchpi %{(literal subexpr_)} do
        subexpr
      end

      matchpi %{(fn _ _)} do
        expr
      end

      matchpi %{(and _*)} do
        subexprs = expr.items.move(1)
        result = subexprs.none? { |subexpr| eval(it, vars, subexpr) == Term.of(false) }
        Term.of(result)
      end

      matchpi %{(or _*)} do
        subexprs = expr.items.move(1)
        result = !subexprs.all? { |subexpr| eval(it, vars, subexpr) == Term.of(false) }
        Term.of(result)
      end

      matchpi %{(attn subexpr_)} do
        arg = eval(it, vars, subexpr)
        unless dict = arg.as_d?
          return Term.of(:attn, Term[], Term[])
        end

        Term.of(:attn, dict, dict)
      end

      matchpi %{(filter attnQ_ patternQ_)} do
        attn = eval(it, vars, attnQ)

        Term.case(attn) do
          matchpiT %{(attn _dict _dict)} do
            _, data, mask = attn

            mask.each_entry do |key, _|
              unless value = data[key]?
                mask = mask.without(key)
                next
              end

              unless env = M1.match?(patternQ, value)
                mask = mask.without(key)
                next
              end

              next unless rep = env[:value]?

              data = data.with(key, rep)
            end

            Term.of(:attn, data, mask)
          end

          otherwise do
            Term.of(:attn, Term[], Term[])
          end
        end
      end

      # TODO: Remove in favor of (-> x (attn _) (filter _ pattern_) (gather _))
      matchpi %{(select haystackQ_ pattern_)} do
        arg = eval(it, vars, haystackQ)
        unless dict = arg.as_d?
          return Term.of
        end

        result = Term::Dict.build do |commit|
          dict.items.each do |item|
            next unless env = M1.match?(pattern, item)

            commit << (env[:value]? || item)
          end

          dict.each_entry(in: Term::Dict.pairspart) do |key, value|
            next unless env = M1.match?(pattern, value)

            commit.with(key, env[:value]? || value)
          end
        end

        Term.of(result)
      end

      matchpi %{(morph matcheeQ_ _*)} do
        matchee = eval(it, vars, matcheeQ)

        subexprs = expr.items.move(2)
        backsys = subexprs.to_compact_readonly_slice do |subexpr|
          next unless subexpr = subexpr.as_d?
          next unless subexpr.size == 3
          next unless subexpr.itemsonly?

          head, pattern, backspec = subexpr
          next unless head == Term[:backmap]

          {pattern, backspec}
        end

        M1.backmap(backsys, matchee)
      end

      matchpi %{(-> argQ_ _*)} do
        arg = eval(it, vars, argQ)

        subexprs = expr.items.move(2)
        subexprs.each do |subexpr|
          unless subdict = subexpr.as_d?
            arg = subexpr
            next
          end

          subdict = idfs(subdict) do |value|
            next value unless value == Term[:_]

            # Since we don't want it evaluated again, we substitute 'arg rather
            # than simply arg.
            Term.of(:literal, arg)
          end

          subexpr = Term.of(subdict)
          arg = eval(it, vars, subexpr)
        end

        arg
      end

      matchpi %{(template template_)} do
        Alloy.render(vars, template)
      end

      matchpi %{(let bodyQ_ ¦ assignments_)} do
        vars1 = vars
        assignments.each_entry do |var, valueQ|
          vars1 = vars1.with(var, eval(it, vars, valueQ))
        end

        eval(it, vars1, bodyQ)
      end

      matchpi %{(let* assignments←(_*) bodyQ_)} do
        vars1 = vars
        assignments.items.each do |assignment|
          # (_ _)
          next unless assignment = assignment.as_d?
          next unless assignment.itemsonly? && assignment.size == 2

          var, valueQ = assignment
          vars1 = vars1.with(var, eval(it, vars1, valueQ))
        end

        eval(it, vars1, bodyQ)
      end

      otherwise do
        expr
      end
    end
  end

  # See `Interpreter`.
  def primitive(it : Interpreter, vars : Term::Dict, expr : Term) : Term
    unless expr.type.dict?
      return expr
    end

    Term.case(expr) do
      matchpi %{(+)} do
        Term.of(0)
      end

      matchpi %{(+ _*)} do
        calc(expr, args: expr.items.move(1)) do |operands|
          render(operands.reduce { |a, b| add(a, b) })
        end
      end

      matchpi %{(- _)} do
        calc(expr, args: expr.items.move(1)) do |(operand, *_)|
          render(negate(operand))
        end
      end

      matchpi %{(- _ _ _*)} do
        calc(expr, args: expr.items.move(1)) do |operands|
          render(operands.reduce { |a, b| sub(a, b) })
        end
      end

      matchpi %{(*)} do
        Term.of(1)
      end

      matchpi %{(* _ _ _*)} do
        calc(expr, args: expr.items.move(1)) do |operands|
          render(operands.reduce { |a, b| mul(a, b) })
        end
      end

      matchpi %{(/ _)} do
        calc(expr, args: expr.items.move(1)) do |(operand, *_)|
          render(div(ArithConst.new(Term[1]), operand))
        end
      end

      matchpi %{(/ _ _ _*)} do
        calc(expr, args: expr.items.move(1)) do |operands|
          render(operands.reduce { |a, b| div(a, b) })
        end
      end

      matchpiT %{(// ±a (%all ±b (%not 0)))} do
        Term.of(a // b)
      end

      matchpiT %{(mod ±a (%all ±b (%not 0)))} do
        Term.of(a % b)
      end

      matchpiT %{(** ±a ±b)} do
        Term.of(a ** b)
      rescue MathDomainError
        continue
      end

      matchpi %{(< _*)} do
        calc(expr, args: expr.items.move(1), allow: ArithConst | ArithPosInf | ArithNegInf) do |operands|
          result = true

          operands.each_cons_pair do |pred, succ|
            next if lt?(pred, succ)

            result = false
            break
          end

          Term.of(result)
        end
      end

      matchpi %{(<= _*)} do
        calc(expr, args: expr.items.move(1), allow: ArithConst | ArithPosInf | ArithNegInf) do |operands|
          result = true

          operands.each_cons_pair do |pred, succ|
            next if pred == succ || lt?(pred, succ)

            # NOTE: This is necessary because when doing comparison, ≈0 is considered
            # to be equal to 0 (and so on for approx-exact comparisons). `==` is
            # too strict as it requires both parties to be approx or exact.
            next if pred.is_a?(ArithConst) && succ.is_a?(ArithConst) && (pred.value <=> succ.value).zero?

            result = false
            break
          end

          Term.of(result)
        end
      end

      matchpi %{(> _*)} do
        calc(expr, args: expr.items.move(1), allow: ArithConst | ArithPosInf | ArithNegInf) do |operands|
          result = true

          operands.each_cons_pair do |pred, succ|
            next if lt?(succ, pred)

            result = false
            break
          end

          Term.of(result)
        end
      end

      matchpi %{(>= _*)} do
        calc(expr, args: expr.items.move(1), allow: ArithConst | ArithPosInf | ArithNegInf) do |operands|
          result = true

          operands.each_cons_pair do |pred, succ|
            next if pred == succ || lt?(succ, pred)

            # NOTE: This is necessary because when doing comparison, ≈0 is considered
            # to be equal to 0 (and so on for approx-exact comparisons). `==` is
            # too strict as it requires both parties to be approx or exact.
            next if pred.is_a?(ArithConst) && succ.is_a?(ArithConst) && (pred.value <=> succ.value).zero?

            result = false
            break
          end

          Term.of(result)
        end
      end

      matchpi %{(=)} do
        Term.of(true)
      end

      matchpi %{(= ref_ _*)} do
        args = expr.items.move(2)

        Term.of(args.all? { |other| ref == other })
      end

      matchpiT %{(floor ±arg)} do
        Term.of(arg.floor)
      end

      matchpiT %{(ceil ±arg)} do
        Term.of(arg.ceil)
      end

      matchpiT %{(round ±arg)} do
        Term.of(arg.round)
      end

      matchpiT %{(exact ±arg)} do
        Term.of(Term::Num.exact(arg))
      end

      matchpiT %{(approx ±arg)} do
        Term.of(Term::Num.approx(arg))
      end

      # FIXME: These things must use Arith!

      matchpi %{(sum ())} do
        Term.of(0)
      end

      matchpi %{(sum args←(_number+))} do
        Term.of(args.items.reduce { |a, b| a.as_n + b.as_n })
      end

      matchpi %{(product ())} do
        Term.of(1)
      end

      matchpi %{(product args←(_number+))} do
        Term.of(args.items.reduce { |a, b| a.as_n * b.as_n })
      end

      matchpi %{(min _number+)} do
        args = expr.items.move(1)
        Term.of(args.min_by(&.as_n))
      end

      matchpi %{(min args←(_number+))} do
        Term.of(args.items.min_by(&.as_n))
      end

      matchpi %{(max _number+)} do
        args = expr.items.move(1)
        Term.of(args.max_by(&.as_n))
      end

      matchpi %{(max args←(_number+))} do
        Term.of(args.items.max_by(&.as_n))
      end

      matchpi %{(abs _number+)} do
        args = expr.items.move(1)
        Term.of(args.reduce { |memo, arg| memo - arg }.abs)
      end

      matchpi %{(not false)} do
        Term.of(true)
      end

      matchpi %{(not _)} do
        Term.of(false)
      end

      matchpi %{(~ _*)} do
        args = expr.items.move(1)

        result = args.reduce(Term[""]) do |a, arg|
          b = arg.as_s?
          b ||= Term[ML.compact(arg)]
          a.stitch(b)
        end

        Term.of(result)
      end

      matchpiT %{(repeat a_string n←(%number +i32))} do
        Term.of(a.to(String) * n)
      end

      matchpi %{(ml/compact arg_)} do
        Term.of(ML.compact(arg))
      end

      matchpi %{(ml/display arg_)} do
        Term.of(ML.display(arg, endl: false))
      end

      matchpi %{(ml/term ml_string)} do
        Term.of(:ok, ML.term(ml.to(String)))
      rescue e : ML::SyntaxError
        excerpt, line, column = ML::SyntaxError.lookaround(e.text)

        Term.of(:err, detail: e.detail, excerpt: excerpt, line: line, column: column)
      end

      matchpi %{(ml/terms ml_string)} do
        Term.of(:ok, ML.terms(ml.to(String)))
      rescue e : ML::SyntaxError
        excerpt, line, column = ML::SyntaxError.lookaround(e.text)

        Term.of(:err, detail: e.detail, excerpt: excerpt, line: line, column: column)
      end

      matchpi %{(ml/document ml_string)} do
        Term.of(:ok, ML.document(ml.to(String)))
      rescue e : ML::SyntaxError
        excerpt, line, column = ML::SyntaxError.lookaround(e.text)

        Term.of(:err, detail: e.detail, excerpt: excerpt, line: line, column: column)
      end

      matchpiT %{(escape arg_string)} do
        Term.of(arg.escaped.to_s)
      end

      matchpi %{(charcount _*)} do
        args = expr.items.move(1)

        charcount = args.reduce(Term[0]) do |memo, arg|
          if string = arg.as_s?
            memo + Term[string.charcount]
          else
            memo
          end
        end

        Term.of(charcount)
      end

      matchpiT %{(upcase arg_string)} do
        Term.of(arg.upcase)
      end

      matchpiT %{(dncase arg_string)} do
        Term.of(arg.downcase)
      end

      matchpi %{(bytesize _*)} do
        args = expr.items.move(1)

        bytesize = args.reduce(Term[0]) do |memo, arg|
          if blob = arg.as_blob?
            memo + Term[blob.ubytesize64]
          else
            memo
          end
        end

        Term.of(bytesize)
      end

      matchpi %{(hashcode arg_)} do
        Term.of(Term.hashcode(arg))
      end

      matchpi %{(entry key_ value_)} do
        Term.of(Term[].with(key, value))
      end

      matchpiT %{(entries arg_dict)} do
        Term.of(arg.ee(ordered: true))
      end

      matchpiT %{(value arg_dict key_)} do
        continue unless value = arg[key]?

        Term.of(value)
      end

      matchpiT %{(value arg_dict _* ⍊ default_)} do
        keys = expr.items.move(2)
        value = arg.follow?(keys)
        value ||= default
        Term.of(value)
      end

      matchpi %{(size _*)} do
        args = expr.items.move(1)

        size = args.reduce(Term[0]) do |memo, arg|
          if dict = arg.as_d?
            memo + Term[dict.usize]
          else
            memo
          end
        end

        Term.of(size)
      end

      matchpi %{(itemsize _*)} do
        args = expr.items.move(1)

        size = args.reduce(Term[0]) do |memo, arg|
          if dict = arg.as_d?
            memo + Term[dict.uitemsize]
          else
            memo
          end
        end

        Term.of(size)
      end

      matchpi %{(pairsize _*)} do
        args = expr.items.move(1)

        size = args.reduce(Term[0]) do |memo, arg|
          if dict = arg.as_d?
            memo + Term[dict.pairsize]
          else
            memo
          end
        end

        Term.of(size)
      end

      matchpiT %{(itemspart arg_dict)} do
        Term.of(arg.itemspart)
      end

      matchpiT %{(pairspart arg_dict)} do
        Term.of(arg.pairspart)
      end

      matchpi %{(union _*)} do
        args = expr.items.move(1)
        args.reduce(Term.of) { |a, b| Term.union(a, b) }
      end

      # multiset union
      matchpiT %{(mset/union a_dict b_dict)} do
        if a.size < b.size
          sm, lg = {a, b}
        else
          sm, lg = {b, a}
        end

        result = lg.transaction do |commit|
          sm.each_entry do |key, sm_value|
            next unless sm_count = sm_value.as_n?
            next unless lg_count = lg[key]? || Term.of(0)
            next unless lg_count = lg_count.as_n?

            commit.with(key, sm_count + lg_count)
          end
        end

        Term.of(result)
      end

      matchpi %{(merge _*)} do
        args = expr.items.move(1)
        args.reduce(Term.of) { |a, b| Term.merge(a, b) }
      end

      matchpiT %{(pluck arg_dict mask_dict)} do
        Term.of(Term.pluck(arg, mask))
      end

      matchpiT %{(iota arg←(%number +i32))} do
        result = Term::Dict.build do |commit|
          (0...arg).each do |i|
            commit << i
          end
        end

        Term.of(result)
      end

      matchpi %{(flatten arg_ ¦ -depth)}, %{(flatten arg_ ¦ depth: ∞)} do
        Term.flatten(arg, depth: nil)
      end

      matchpi %{(flatten arg_ ¦ depth_: (%number +i32))}, depth: Int32 do
        Term.flatten(arg, depth: depth)
      end

      matchpiT %{(map (attn data_dict mask_dict) (fn pattern_ bodyQ_))} do |data, mask|
        data = data.transaction do |commit|
          mask.each_entry do |key, _|
            unless value = data[key]?
              mask = mask.without(key)
              next
            end

            next unless env = M1.match?(pattern, value)

            rep = eval(it, Term.union(vars, env), bodyQ)
            commit.with(key, rep)
          end
        end

        Term.of(:attn, data, mask)
      end

      matchpiT %{(gather (attn data_dict mask_dict))} do
        result = Term::Dict.build do |commit|
          data.items.each_with_index do |item, index|
            next unless mask[index]?

            commit << item
          end

          data.each_entry(in: Term::Dict.pairspart) do |key, value|
            next unless mask[key]?

            commit.with(key, value)
          end
        end

        Term.of(result)
      end

      matchpi %{(gather _)} do
        Term.of
      end

      matchpi %{(in? (b_ ..< e_) value_)} do
        lo = arith?(b)
        continue if lo.nil? || lo.is_a?(ArithIndet)

        hi = arith?(e)
        continue if hi.nil? || hi.is_a?(ArithIndet)

        n = arith?(value)
        continue if n.nil? || n.is_a?(ArithIndet)

        Term.of((lo == n || lt?(lo, n)) && lt?(n, hi))
      end

      # TODO: rename to `substring?`
      matchpi %{(any? haystack_string needle_string)}, haystack: String, needle: String do
        Term.of(haystack.includes?(needle))
      end

      matchpiT %{(any? haystack_dict needle_)} do
        Term.of(haystack.any?(needle))
      end

      matchpi %{(hex arg←(%number (whole _)))} do
        Term.of(arg.to(BigInt).to_s(base: 16))
      end

      matchpiT %{(rune arg_string i←(%number i32))}, arg: String do
        Term.of(arg[i]? || Term.of(""))
      end

      # TODO: (_ ..= _) [instead of inlining it pass it as a unit, (_ ..= _) is composite]
      matchpiT %{(runes arg_string b←(%number i32) ..= e←(%number i32))}, arg: String do
        Term.of(arg[b..e]? || "")
      end

      # TODO: (_ ..< _) [instead of inlining it pass it as a unit, (_ ..< _) is composite]
      matchpiT %{(runes arg_string b←(%number i32) ..< e←(%number i32))}, arg: String do
        Term.of(arg[b...e]? || "")
      end

      matchpiT %{(word arg_string i←(%number i32))}, arg: StringView do
        Term.of(WordTokenizer.words(arg, i, i))
      end

      matchpiT %{(words arg_string b←(%number i32) ..= e←(%number i32))}, arg: StringView do
        Term.of(WordTokenizer.words(arg, b, e))
      end

      matchpiT %{(words arg_string)}, arg: StringView do
        result = Term::Dict.build do |commit|
          remaining = arg
          until remaining.empty?
            l, _, r = remaining.partition do |chr|
              chr.vspace? || chr.hspace?
            end

            commit << l
            remaining = r
          end
        end

        Term.of(result)
      end

      matchpi %{(partition text_string sep_string)}, text: String, sep: String do
        Term.of(text.partition(sep))
      end

      matchpi %{(rpartition text_string sep_string)}, text: String, sep: String do
        Term.of(text.rpartition(sep))
      end

      matchpi %{(line/stem arg_string)}, arg: StringView do
        l, _, _ = arg.partition('\n')
        Term.of(l)
      end

      matchpi %{(line/rest arg_string)}, arg: StringView do
        _, sep, r = arg.partition('\n')
        Term.of(sep + r)
      end

      matchpi %{(rline/stem arg_string)}, arg: StringView do
        _, _, r = arg.rpartition('\n')
        Term.of(r)
      end

      matchpi %{(rline/rest arg_string)}, arg: StringView do
        l, sep, _ = arg.rpartition('\n')
        Term.of(l + sep)
      end

      matchpi %{(prefix-run matchee_string prefix_string)}, matchee: StringView, prefix: StringView do |matchee|
        run = String.build do |io|
          while matchee.starts_with?(prefix)
            matchee = matchee.lskip(prefix.size)
            io << prefix
          end
        end

        Term.of(run)
      end

      matchpi %{(codepoints arg_string)}, arg: String do
        codepoints = Term::Dict.build do |commit|
          arg.each_char do |chr|
            commit << chr.ord
          end
        end

        Term.of(codepoints)
      end

      # TODO: WTF is this?
      matchpi %{(repr ns←((%past (%number (whole _)))) (digits ¦ () alphabet_string))}, alphabet: String do
        repr = Term::Dict.build do |commit|
          ns.items.each do |n|
            Int.each_digit(n, base: alphabet.size) do |digit|
              assert digit.natural?

              commit << alphabet[digit.to(Int32)]
            end
          end
        end

        Term.of(repr)
      end

      matchpi %{(pigment arg_ ¦ default_⋮ transparent)} do
        Term.of(Pigment.rgba?(arg) || Pigment.rgba?(default) || Pigment.transparent)
      end

      matchpi %{(pigment? arg_)} do
        outcome = Pigment.eval(arg)
        if outcome.is_a?(Outcome::Rejected)
          return Term.of(:err, details: {"rejected"})
        end

        rgba = outcome.unwrap

        details = outcome.diagnostics.to_compact_readonly_slice do |diagnostic|
          case entity = diagnostic.entity
          when Diagnostic::Text
            entity.detail
          when Diagnostic::TermRef
            "#{entity.detail}: #{entity.term}"
          end
        end

        if rgba
          Term.of(:ok, rgba, details: details.present? ? details : nil)
        else
          Term.of(:err, details: details.present? ? details : nil)
        end
      end

      otherwise { expr }
    end
  end

  # Evaluates a Nitrene expression *expr* using the default evaluation context.
  # Returns the resulting term.
  def eval(vars : Term::Dict, expr : Term) : Term
    eval(Interpreter::DEFAULT, vars, expr)
  end
end
