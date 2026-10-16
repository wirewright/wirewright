# |@ nitrene
#
# |@summary
# An expression language.
#
# |@block
# Nitrene is an expression language for Wirewright.
#
# Nitrene is mostly meant to be embedded in Alloy; but you can embed Alloy in Nitrene,
# too, or use Nitrene separately.
#
# An *expression language* in the sense I am employing here can be likened to
# Excel formulas, in terms of its scope and the kinds of computations it allows
# you to do; that is, raw computation at the "leaves" (e.g. `(+ 2 2)`, `(max 1 2 3)`).
#
# |@example
# The following Nitrene program is a baroque way to "stitch" digits
# together into a number.
#
# ```wwml
# (let digits: (1 2 3 4)
#   (-> (iota (size digits)) ;; (0 1 2 3)
#     (attn _) ;; (⏏0 1 2 3⏏)
#     (map _ (fn ±p (** 10 p))) ;; (⏏1 10 100 1000⏏)
#     (reverse _) ;; (⏏1000 100 10 1⏏)
#     (zip _ digits) ;; (⏏(1000 1) (100 2) (10 3) (1 4)⏏)
#     (map _ (fn (±a ±b) (* a b))) ;; (⏏1000 200 30 4⏏)
#     (sum _))) ;; 1234
# ```
#
# Notice the use of the threading operator, `->` (see `nitrene.thread`).
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

  # |@ nitrene.arith
  #
  # |@block
  # Arithmetic units recognized by the arithmetic operators of Nitrene.
  #
  # NOTE: The behavior of Arith operations is based on Wolfram Mathematica.
  #
  # Reference: https://www.wolfram.com/mathematica/
  # See in particular:
  #   - https://reference.wolfram.com/language/ref/Infinity.html
  #   - https://reference.wolfram.com/language/ref/Indeterminate.html

  # :nodoc:
  def arith?(term : Term) : Arith?
    case term
    when .subtype?(:number)
      # |@ nitrene.arith
      #
      # |@pattern
      # _number
      #
      # |@block
      # A number constant.
      ArithConst.new(term.as_n)
    when SYM_INFINITY
      # |@ nitrene.arith
      #
      # |@pattern
      # ∞
      #
      # |@block
      # Represents positive infinity.
      ArithPosInf.new
    when SYM_NEG_INFINITY
      # |@ nitrene.arith
      #
      # |@pattern
      # -∞
      #
      # |@block
      # Represents negative infinity.
      ArithNegInf.new
    when SYM_INDET
      # |@ nitrene.arith
      #
      # |@pattern
      # indet
      #
      # |@block
      # Represents an indeterminate result. `indet` is the result of expressions
      # such as `(+ ∞ -∞)`.
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
  def abs(a : ArithPosInf | ArithNegInf | ArithConst) : Arith
    case a
    in ArithConst  then ArithConst.new(a.value.abs)
    in ArithPosInf then ArithPosInf.new
    in ArithNegInf then ArithPosInf.new
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
    in {ArithConst, ArithPosInf}
      a.value.zero? ? ArithIndet.new : ArithPosInf.new
    in {ArithPosInf, ArithConst}
      b.value.zero? ? ArithIndet.new : ArithPosInf.new
    in {ArithConst, ArithNegInf}
      a.value.zero? ? ArithIndet.new : ArithNegInf.new
    in {ArithNegInf, ArithConst}
      b.value.zero? ? ArithIndet.new : ArithNegInf.new
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
      if b.value.exact? && b.value.zero?
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

  alias Eval = Interpreter, Term::Dict, Term -> Evaln

  # Represents an *evaluation*, the result of evaluating a term. It is
  # either another term or a special value, `Inert`.
  alias Evaln = Term | Inert

  # Signals to Nitrene that it should try other evaluators.
  defrecord Inert

  def inert : Inert
    Inert.new
  end

  # Represents a Nitrene interpreter.
  defrecord Interpreter, primitive : Eval, composite : Eval

  struct Interpreter
    DEFAULT = new(Nitrene.primitive, Nitrene.composite)
  end

  def composite : Eval
    ->Nitrene.composite(Interpreter, Term::Dict, Term)
  end

  def primitive : Eval
    ->Nitrene.primitive(Interpreter, Term::Dict, Term)
  end

  def either(a : Eval, b : Eval) : Eval
    ->(it : Interpreter, vars : Term::Dict, expr : Term) do
      value = a.call(it, vars, expr)
      if value.is_a?(Term)
        return value.as(Evaln)
      end

      b.call(it, vars, expr)
    end
  end

  def either(a : Eval, b : Eval, *cs : Eval) : Eval
    either(a, either(b, *cs))
  end

  def eval(it : Interpreter, vars : Term::Dict, expr : Term) : Term
    case evaln = evaln(it, vars, expr)
    in Term      then evaln
    in Unchanged then expr
    end
  end

  defrecord Unchanged

  def evaln(it : Interpreter, vars : Term::Dict, expr : Term) : Term | Unchanged
    evaln = it.composite.call(it, vars, expr)
    if evaln.is_a?(Term)
      return evaln
    end

    changed = false

    # Evaluate recursively.
    if expr_dict0 = expr.as_d?
      expr_dict1 = expr_dict0.transaction do |commit|
        expr_dict0.each_entry do |key, value|
          case evaln = evaln(it, vars, value)
          in Term
            commit.with(key, evaln)
          in Unchanged
          end
        end
      end

      expr = Term.of(expr_dict1)
      changed = !expr_dict0.same?(expr_dict1)
    end

    evaln = it.primitive.call(it, vars, expr)
    if evaln.is_a?(Term)
      return evaln
    end

    if value = vars[expr]?
      return value
    end

    changed ? expr : Unchanged.new
  end

  # TODO: all attention functions should end with `*`, e.g., `reduce*`, `sum*`. Their non-
  # attention counterparts (if any) should not end with it (e.g., `reduce`, `sum`).

  # See `Interpreter`.
  def composite(it : Interpreter, vars : Term::Dict, expr : Term) : Evaln
    unless expr.type.dict?
      return Inert.new
    end

    Term.case(expr, block_type: {:proc, it : Interpreter, vars : Term::Dict}) do
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

        # TODO: Is there any way at all to optimize this? Maybe dict could
        # maintain a mask of some sort, or construct it cheaply? We can't
        # just do `(attn dict dict)` because this breaks equality.
        mask = Term::Dict.build do |commit|
          dict.each_entry do |key, _|
            commit.with(key, true)
          end
        end

        Term.of(:attn, dict, mask)
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
          # [backmap pattern_ backspec_]
          next unless subexpr = subexpr.as_d?
          next unless subexpr.itemsize == 3
          head, pattern, backspec = subexpr
          next unless head == Term[:backmap]

          {pattern, backspec}
        end

        M1.backmap(backsys, matchee)
      end

      # |@ nitrene.composite.reduce
      #
      # |@pattern
      # (reduce (initial_ arg_) (fn pattern_ body_))
      #
      # |@key initial nitrene
      # An expression computing the initial value for the accumulator.
      #
      # |@key arg nitrene
      # An expression computing the dictionary to iterate over.
      #
      # |@key pattern m1.operator
      # The M1 pattern to accept pairs `(acc_ item_)`. *item* is an item
      # of *arg*. *acc* is the running value of the accumulator, starting
      # with *initial*. Items that are rejected by the pattern are skipped.
      # The captures for items that are accepted are provided to *body*.
      #
      # |@key body nitrene
      # An expression to evaluate for each pair matched by *pattern*. Captures
      # made by *pattern* are available in the body.
      #
      # |@block
      # Iterates over the items of the dictionary produced by *arg*,
      # pairing each accepted item with the running accumulator value.
      # The accumulator starts with *initial*.
      #
      # See also: `nitrene.reduce*`.
      #
      # |@example
      # The following program sums even numbers (iteratively, i.e., without
      # using attentions).
      #
      # ```wwml
      # (let xs: (1 2 3 4 5 6)
      #   (reduce (0 xs)
      #     (fn (±n m←(%pipe (mod 2) 0))
      #       (+ n m))))
      # ;; => 12
      # ```
      matchpiT %{(reduce (initialQ_ argQ_) (fn pattern_ bodyQ_))} do
        acc = eval(it, vars, initialQ)
        arg = eval(it, vars, argQ)

        unless arg = arg.as_d?
          return acc
        end

        arg.items.each do |item|
          input = Term.of(acc, item)
          next unless env = M1.match?(pattern, input)

          acc = eval(it, Term.union(vars, env), bodyQ)
        end

        Term.of(acc)
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
        Alloy.render(template,
          locals: vars,
          primitive: it.primitive,
          composite: it.composite,
        )
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

      matchpi %{(set argQ_ selector_)} do
        arg = eval(it, vars, argQ)
        unless arg = arg.as_d?
          return Term.of # empty set
        end

        result = Term::Dict.build do |commit|
          arg.items.each do |item|
            envs = M1.matches(selector, item)
            next if envs.empty?

            envs.each do |env|
              if env.empty?
                commit.with(item, true)
                next
              end

              _, value = env.ee.first
              commit.with(value, true)
            end
          end
        end

        Term.of(result)
      end

      otherwise do
        Inert.new
      end
    end
  end

  # See `Interpreter`.
  def primitive(it : Interpreter, vars : Term::Dict, expr : Term) : Evaln
    unless expr.type.dict?
      return Inert.new
    end

    Term.case(expr, block_type: {:proc, it : Interpreter, vars : Term::Dict}) do
      # |@ nitrene.add
      #
      # |@pattern
      # (+ args_*)
      #
      # |@summary
      # Addition.
      #
      # |@block
      # Returns the sum of arithmetic units in *args* (see `nitrene.arith`).
      #
      # If *args* contains no arithmetic units, the sum is zero.
      #
      # |@example
      # ```wwml
      # (+ 1 2)  ;; => 3
      # (+ ∞ -∞) ;; => indet
      # ```
      matchpi %{(+ _*)} do
        calc(expr, args: expr.items.move(1)) do |operands|
          if operands.empty?
            Term.of(0)
          else
            render(operands.reduce { |a, b| add(a, b) })
          end
        end
      end

      # |@ nitrene.sub
      #
      # |@pattern
      # (- args_*)
      #
      # |@summary
      # Subtraction.
      #
      # |@block
      # Returns the difference of arithmetic units in *args* (see `nitrene.arith`).
      #
      # - If *args* contains no arithmetic units, the difference is zero.
      # - If *args* contains one arithmetic unit, the difference is the result
      #   of negating that unit.
      # - If *args* contains two or more arithmetic units, the difference is
      #   the result of subtracting those units in the order they appear in *args*.
      #
      # |@example
      # ```wwml
      # (-)       ;; => 0
      # (- a b c) ;; => 0
      # (- 1)     ;; => -1
      # (- 1 2 3) ;; => -6
      # ```
      matchpi %{(- _*)} do
        calc(expr, args: expr.items.move(1)) do |operands|
          case operands.size
          when 0
            Term.of(0)
          when 1
            render(negate(operands.first))
          else
            render(operands.reduce { |a, b| sub(a, b) })
          end
        end
      end

      # |@ nitrene.mul
      #
      # |@pattern
      # (* args_*)
      #
      # |@summary
      # Multiplication.
      #
      # |@block
      # Returns the product of arithmetic units in *args* (see `nitrene.arith`).
      #
      # - If *args* contains no arithmetic units, returns `1`, the identity
      #   for product.
      # - If *args* contains one or more arithmetic unit, returns the product
      #   of those units.
      #
      # |@example
      # ```wwml
      # (*)           ;; => 1
      # (* 3 5)       ;; => 15
      # (* 1 2 3 4 5) ;; => 120
      # (* -∞ 0)      ;; => indet
      # ```
      matchpi %{(* _*)} do
        calc(expr, args: expr.items.move(1)) do |operands|
          case operands.size
          when 0
            Term.of(1)
          else
            render(operands.reduce { |a, b| mul(a, b) })
          end
        end
      end

      # |@ nitrene.div
      #
      # |@pattern
      # (/ args_+)
      #
      # |@summary
      # Division.
      #
      # |@block
      # Returns the quotient of one or more arithmetic unit *args*.  If there is
      # just one arg, returns `1/arg`.
      #
      # |@example
      # ```wwml
      # (/ 2)   ;; => 1/2
      # (/ 1 2) ;; => 1/2
      # ```

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

      # |@ nitrene.idiv
      #
      # |@pattern
      # (// a_ b_)
      #
      # |@summary
      # Integer division.
      #
      # |@block
      # Returns the quotient of integer division of number *a* by number *b*. If
      # either argument is not a number, or if *b* is zero, returns `0`.
      #
      # |@example
      # ```wwml
      # (// 1 2)     ;; => 0
      # (// 20 5)    ;; => 4
      # (// 10 0)    ;; => 0
      # (// 0 0)     ;; => 0
      # (// "qux" 0) ;; => 0
      # ```

      matchpiT %{(// ±a (%all ±b (%not 0)))} do
        Term.of(a // b)
      end

      matchpi %{(// _ _)} do
        Term.of(0)
      end

      # |@ nitrene.mod
      #
      # |@pattern
      # (mod a_ b_)
      #
      # |@summary
      # Modulo.
      #
      # |@block
      # Returns the remainder of division of number *a* by number *b*. If either
      # or both arguments are not a number, or if *b* is zero, returns `0`.
      #
      # |@example
      # ```wwml
      # (mod 1 2)     ;; => 1
      # (mod 20 3)    ;; => 2
      # (mod 10 0)    ;; => 0
      # (mod 0 0)     ;; => 0
      # (mod "qux" 0) ;; => 0
      # ```

      matchpiT %{(mod ±a (%all ±b (%not 0)))} do
        Term.of(a % b)
      end

      matchpi %{(mod _ _)} do
        Term.of(0)
      end

      # |@ nitrene.pow
      #
      # |@pattern
      # (** a_ b_)
      #
      # |@summary
      # Power.
      #
      # |@block
      # Raises number *a* to the power of number *b*. If either or both arguments
      # are not a number, returns `0`.
      #
      # |@example
      # ```wwml
      # (** 2 4)   ;; => 16
      # (** 2 -4)  ;; => 0.0625
      # (** 2 1/2) ;; => ≈1.4142135 (approximate!)
      # ```

      matchpiT %{(** ±a ±b)} do
        Term.of(a ** b)
      end

      matchpiT %{(** a_ b_)} do
        Term.of(0)
      end

      # |@ nitrene.sqrt
      #
      # |@pattern
      # (sqrt arg_)
      #
      # |@summary
      # Square root.
      #
      # |@block
      # Returns the approximate square root of *arg*. For negative numbers and non-
      # numbers, returns `≈NaN`.
      #
      # |@example
      # ```wwml
      # (sqrt 4) ;; => ≈2
      # (sqrt 2) ;; => ≈1.4142135
      # ```

      matchpiT %{(sqrt ±arg)} do
        Term.of(arg.sqrt)
      end

      matchpi %{(sqrt _)} do
        Term.of(Term::Num.nan)
      end

      # |@ nitrene.isqrt
      #
      # |@pattern
      # (isqrt arg_)
      #
      # |@summary
      # Integer square root.
      #
      # |@block
      # Returns the **integer** square root of *arg*.
      #
      # - For positive integers or zero, the result is exact.
      # - For negative integers and non-numbers, the result is `≈NaN`.
      # - For non-integers, the result is approximate.
      #
      # |@example
      # ```wwml
      # (isqrt 4)   ;; => 2
      # (isqrt 2)   ;; => 1
      # (isqrt ≈2)  ;; => ≈1
      # (isqrt 1/2) ;; => ≈0
      # (isqrt -1)  ;; => ≈NaN
      # ```

      matchpiT %{(isqrt ±arg)} do
        Term.of(arg.isqrt)
      end

      matchpiT %{(isqrt _)} do
        Term.of(Term::Num.nan)
      end

      # |@ nitrene.cmp
      #
      # |@pattern
      # (< args_+)
      # (<= args_+)
      # (> args_+)
      # (>= args_+)
      #
      # |@summary
      # Comparison of one or more arithmetic units.
      #
      # |@example
      # ```wwml
      # (< 1) ;; => true
      # (< 1 2) ;; => true
      # (< 1 2 3) ;; => true
      # (< 1 5 3) ;; => false
      # (>= 10 10) ;; => true
      # (>= 10 10.1) ;; => false
      #
      # ;; Importantly:
      # (>= 10 ≈10) ;; => true
      # (<= 10 ≈10) ;; => true
      #
      # ;; ... even though:
      # (= 10 ≈10) ;; => false
      # ```

      matchpi %{(< _+)} do
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

      matchpi %{(<= _+)} do
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

      matchpi %{(> _+)} do
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

      matchpi %{(>= _+)} do
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

      # |@ nitrene.equals
      #
      # |@pattern
      # (= args_+)
      #
      # |@summary
      # Term equality.
      #
      # |@block
      # Returns `true` if one or more terms are equal (When I say equal, I mean equal
      # by value. There is no other kind of equality in Wirewright, since e.g.
      # the notion of reference equality is way below our target abstraction level.)
      # If there is just one *arg*, this function returns `true`.
      #
      # |@example
      # ```wwml
      # (= 100)         ;; => true
      # (= 100 200)     ;; => false
      # (= 100 100)     ;; => true
      # (= "a" "b" "c") ;; => false
      # (= "a" "a" "a") ;; => true
      # ```
      matchpi %{(= _+)} do
        args = expr.items.move(1)
        if args.size >= 2
          ref = args.first
          Term.of(args.move(1).all? { |other| ref == other })
        else
          Term.of(true)
        end
      end

      # |@ nitrene.floor
      #
      # |@pattern
      # (floor arg_)
      #
      # |@summary
      # Rounds toward negative infinity.
      #
      # |@block
      # Rounds number *arg*s toward negative infinity. Returns `0` for all
      # other *arg*s.
      #
      # |@example
      # ```wwml
      # (floor 100.23)  ;; => 100
      # (floor -100.23) ;; => -100
      # (floor 100.5)   ;; => 100
      # (floor 100.99)  ;; => 100
      # (floor "qux")   ;; => 0
      # ```

      matchpiT %{(floor ±arg)} do
        Term.of(arg.floor)
      end

      matchpi %{(floor _)} do
        Term.of(0)
      end

      # |@ nitrene.ceil
      #
      # |@pattern
      # (ceil arg_)
      #
      # |@summary
      # Rounds toward positive infinity.
      #
      # |@block
      # Rounds number *arg*s toward positive infinity. Returns `0` for all
      # other *arg*s.
      #
      # |@example
      # ```wwml
      # (ceil 100.23)  ;; => 101
      # (ceil -100.23) ;; => -99
      # (ceil 100.5)   ;; => 101
      # (ceil 100.99)  ;; => 101
      # (ceil "qux")   ;; => 0
      # ```

      matchpiT %{(ceil ±arg)} do
        Term.of(arg.ceil)
      end

      matchpi %{(ceil _)} do
        Term.of(0)
      end

      # |@ nitrene.round
      #
      # |@pattern
      # (round arg_)
      #
      # |@summary
      # Banker's rounding.
      #
      # |@block
      # Rounds number *arg*s using Banker's rounding (round half `.5` to even).
      # Returns `0` for all other *arg*s.
      #
      # |@example
      # ```wwml
      # (round 100.23)  ;; => 100
      # (round -100.23) ;; => -100
      # (round 100.5)   ;; => 101
      # (round 101.5)   ;; => 102 (to even)
      # (round 102.5)   ;; => 102 (to even)
      # (round 103.5)   ;; => 104 (to even)
      # (round 100.99)  ;; => 101
      # (round "qux")   ;; => 0
      # ```

      matchpiT %{(round ±arg)} do
        Term.of(arg.round)
      end

      matchpi %{(round _)} do
        Term.of(0)
      end

      # |@ nitrene.exact
      #
      # |@pattern
      # (exact arg_)
      #
      # |@summary
      # Forces an exact number.
      #
      # |@block
      # Converts *arg*, a number, to an exact number. For non-number *arg*s,
      # returns `0`.
      #
      # |@example
      # ```wwml
      # (exact 100)     ;; => 100
      # (exact 100.23)  ;; => 100.23
      # (exact ≈100.23) ;; => 100.2300...
      # (exact "foo")   ;; => 0
      # ```
      #
      # There are a few unfortunate edge cases with approximate numbers. Right
      # now we handle them by returning zero:
      #
      # ```wwml
      # (exact ≈Infinity)  ;; => 0
      # (exact ≈-Infinity) ;; => 0
      # (exact ≈NaN)       ;; => 0
      # ```

      matchpiT %{(exact ±arg)} do
        result = Term::Num.exact(arg)
        if result.approx? # One of edge cases
          Term.of(0)
        else
          Term.of(result)
        end
      end

      matchpi %{(exact _)} do
        Term.of(0)
      end

      # |@ nitrene.approx
      #
      # |@pattern
      # (approx arg_)
      #
      # |@summary
      # Forces an approximate number.
      #
      # |@block
      # Converts *arg*, a number, to an approximate number. For non-number *arg*s,
      # returns `≈NaN`.
      #
      # |@example
      # ```wwml
      # (approx 100)     ;; => ≈100
      # (approx 100.23)  ;; => ≈100.23
      # (approx ≈100.23) ;; => ≈100.23
      # (approx "foo")   ;; => ≈NaN
      # ```

      matchpiT %{(approx ±arg)} do
        Term.of(Term::Num.approx(arg))
      end

      matchpiT %{(approx _)} do
        Term.of(Term::Num.nan)
      end

      # |@ nitrene.sci
      #
      # |@pattern
      # (sci mantissa_ exponent_)
      #
      # |@summary
      # Evaluates scientific notation.
      #
      # |@block
      # Converts scientific notation to decimal. WwML in particular expands e.g. `12e3`
      # to `(sci 12 3)`; evaluating such terms is why this function exists.
      #
      # Returns `0` if *mantissa*, *exponent*, or both are invalid.
      #
      # |@example
      # ```wwml
      # 12e3  ;; => 12000
      # 12e-3 ;; => 0.012
      # ```

      matchpiT %{(sci ±mantissa ±exponent)} do
        Term.of(mantissa * Term[10]**exponent)
      end

      matchpi %{(sci _ _)} do
        Term.of(0)
      end

      # |@ nitrene.nan
      #
      # |@pattern
      # (nan? arg_)
      #
      # |@summary
      # Checks for the approximate NaN.
      #
      # |@block
      # Returns `true` if *arg* is the approximate `≈NaN`.
      #
      # |@example
      # ```wwml
      # (nan? 100)       ;; => false
      # (nan? x)         ;; => false
      # (nan? ≈NaN)      ;; => true
      # (nan? ≈Infinity) ;; => false
      # ```

      matchpiT %{(nan? ±arg)} do
        Term.of(arg.nan?)
      end

      matchpiT %{(nan? _)} do
        Term.of(false)
      end

      # |@ nitrene.infinite
      #
      # |@pattern
      # (infinite? arg_)
      #
      # |@summary
      # Checks for positive and negative approximate or arithmetic infinity.
      #
      # |@block
      # Returns `true` if *arg* is a positive or negative approximate or
      # arithmetic infinity.
      #
      # Returns `false` if *arg* is invalid.
      #
      # |@example
      # ```wwml
      # (infinite? 100)        ;; => false
      # (infinite? x)          ;; => false
      # (infinite? ≈NaN)       ;; => false
      # (infinite? ≈Infinity)  ;; => true
      # (infinite? ≈-Infinity) ;; => true
      # (infinite? ∞)          ;; => true
      # (infinite? -∞)         ;; => true
      # (infinite? "qux")      ;; => false
      # ```

      matchpiT %{(infinite? ±arg)} do
        Term.of(arg.infinite?)
      end

      matchpiT %{(infinite? arg_)} do
        Term.of(arg.in?(SYM_INFINITY, SYM_NEG_INFINITY))
      end

      # |@ nitrene.sum
      #
      # |@pattern
      # (sum attn←(attn _dict _dict))
      #
      # |@key attn nitrene.attn
      #
      # |@block
      # Returns the sum of selected arithmetic units (see `nitrene.arith`).
      #
      # ```wwml
      # ;; Sum odd numbers:
      #
      # (-> (1 2 3 4 5)
      #   (attn _)
      #   (filter _ (%pipe (mod 2) 1))
      #   (sum _))
      #
      # ;; => 9
      # ```
      matchpiT %{(sum (attn data_dict mask_dict))} do
        a = ArithConst.new(Term[0])

        mask.each_entry do |key, _|
          next unless value = data[key]?
          next unless b = arith?(value)

          a = add(a, b)
        end

        render(a)
      end

      # |@ nitrene.sum
      #
      # |@pattern
      # (sum arg_)
      #
      # |@summary
      # Adds arithmetic unit items of a dict.
      #
      # |@block
      # Returns the sum of arithmetic units in *arg* (see `nitrene.arith`).
      #
      # Returns `0` if *arg* is invalid.
      #
      # |@example
      # ```wwml
      # (sum (1 2 3 4 5))             ;; => 15
      # (sum (1 2 ∞ 4 5))             ;; => ∞
      # (sum (1 2 "qux" 3 4 false 5)) ;; => 15
      # (sum "qux")                   ;; => 0
      # ```

      matchpiT %{(sum arg_dict)} do
        a = ArithConst.new(Term[0])

        arg.each_entry do |_, value|
          next unless b = arith?(value)

          a = add(a, b)
        end

        render(a)
      end

      matchpi %{(sum _)} do
        Term.of(0)
      end

      # |@ nitrene.product
      #
      # |@pattern
      # (product arg_)
      #
      # |@summary
      # Multiplies arithmetic unit items of a dict.
      #
      # |@block
      # Returns the product of arithmetic units in *arg* (see `nitrene.arith`).
      #
      # Returns `0` if *arg* is invalid.
      #
      # |@example
      # ```wwml
      # (product ())                      ;; => 1
      # (product (1 2 3 4 5))             ;; => 120
      # (product (1 2 ∞ 4 5))             ;; => ∞
      # (product (1 2 "qux" 3 4 false 5)) ;; => 120
      # (product "qux")                   ;; => 0
      # ```

      matchpiT %{(product arg_dict)} do
        a = ArithConst.new(Term[1])

        arg.each_entry do |_, value|
          next unless b = arith?(value)

          a = mul(a, b)
        end

        render(a)
      end

      matchpi %{(product _)} do
        Term.of(0)
      end

      # |@ nitrene.min
      #
      # |@summary
      # Returns the minimum among arguments or items.

      # |@ nitrene.min
      #
      # |@pattern
      # (min arg_dict)
      #
      # |@block
      # Returns the minimum arithmetic unit item in *arg*. If there are no
      # arithmetic units in arg, or if *arg* is invalid, returns `∞`.
      #
      # |@example
      # ```wwml
      # (min (1 2 3))        ;; => 1
      # (min (1 -∞ 15))      ;; => -∞
      # (min (1 "qux" -123)) ;; => -123
      # (min ("a" "b" "c"))  ;; => ∞
      # (min "qux")          ;; => ∞
      # ```

      matchpiT %{(min arg_dict)} do
        min = ArithPosInf.new

        arg.items.each do |arg|
          next unless n = arith?(arg)
          next if n.is_a?(ArithIndet)
          next unless lt?(n, min) # n < min

          min = n
        end

        render(min)
      end

      matchpi %{(min _)} do
        render(ArithPosInf.new)
      end

      # |@ nitrene.min
      #
      # |@pattern
      # (min a_ b_ cs_*)
      #
      # |@block
      # Returns the minimum arithmetic unit among *a*, *b*, *cs*. If there are no
      # arithmetic units, returns `∞`.
      #
      # |@example
      # ```wwml
      # (min 1 2 3)        ;; => 1
      # (min 1 -∞ 15)      ;; => -∞
      # (min 1 "qux" -123) ;; => -123
      # (min "a" "b" "c")  ;; => ∞
      # ```
      matchpi %{(min _ _ _*)} do
        min = ArithPosInf.new

        args = expr.items.move(1)
        args.each do |arg|
          next unless n = arith?(arg)
          next if n.is_a?(ArithIndet)
          next unless lt?(n, min) # n < min

          min = n
        end

        render(min)
      end

      # |@ nitrene.max
      #
      # |@summary
      # Returns the maximum among arguments or items.

      # |@ nitrene.max
      #
      # |@pattern
      # (max arg_dict)
      #
      # |@block
      # Returns the maximum arithmetic unit item in *arg*. If there are no
      # arithmetic units in arg, or if *arg* is invalid, returns `-∞`.
      #
      # |@example
      # ```wwml
      # (max (1 2 3))        ;; => 3
      # (max (1 -∞ 15))      ;; => 15
      # (max (1 "qux" -123)) ;; => 1
      # (max ("a" "b" "c"))  ;; => -∞
      # (max "qux")          ;; => -∞
      # ```

      matchpiT %{(max arg_dict)} do
        max = ArithNegInf.new

        arg.items.each do |arg|
          next unless n = arith?(arg)
          next if n.is_a?(ArithIndet)
          next unless lt?(max, n) # max < n

          max = n
        end

        render(max)
      end

      matchpi %{(max _)} do
        render(ArithNegInf.new)
      end

      # |@ nitrene.max
      #
      # |@pattern
      # (max a_ b_ cs_*)
      #
      # |@block
      # Returns the maximum arithmetic unit among *a*, *b*, *cs*. If there are no
      # arithmetic units, returns `-∞`.
      #
      # |@example
      # ```wwml
      # (max 1 2 3)        ;; => 3
      # (max 1 -∞ 15)      ;; => 15
      # (max 1 "qux" -123) ;; => 1
      # (max "a" "b" "c")  ;; => -∞
      # ```
      matchpi %{(max _ _ _*)} do
        max = ArithNegInf.new

        args = expr.items.move(1)
        args.each do |arg|
          next unless n = arith?(arg)
          next if n.is_a?(ArithIndet)
          next unless lt?(max, n) # max < n

          max = n
        end

        render(max)
      end

      # |@ nitrene.clamp
      #
      # |@pattern
      # (clamp ±arg ±b ..= ±e)
      #
      # |@key arg
      # The number to clamp.
      #
      # |@key b
      # Begin of the clamp range.
      #
      # |@key e
      # End of the clamp range.
      #
      # |@summary
      # Clamps a number into bounds.
      #
      # |@block
      # Clamps *arg* between *b* and *e* (inclusive).
      #
      # |@example
      # ```wwml
      # (clamp -10 0 ..= 10) ;; => 0
      # (clamp   5 0 ..= 10) ;; => 5
      # (clamp  20 0 ..= 10) ;; => 10
      # ```
      #
      # Non-numbers return 0:
      #
      # ```wwml
      # (clamp "xyz" 0 ..= 10) ;; => 0
      # (clamp 5 "xyz" ..= 10) ;; => 0
      # (clamp 5 0 ..= "xyz")  ;; => 0
      # ```
      #
      # The order of *b* and *e* in range is fixed if necessary:
      #
      # ```wwml
      # (clamp 5 10 ..= 0) ;; => 5, range fixed to 5 ..= 10
      # ```

      matchpiT %{(clamp ±arg ±b ..= ±e)} do
        lo = Math.min(b, e)
        hi = Math.max(b, e)

        if arg < lo
          Term.of(lo)
        elsif arg > hi
          Term.of(hi)
        else
          Term.of(arg)
        end
      end

      matchpi %{(clamp _ _ ..= _)} do
        Term.of(0)
      end

      # |@ nitrene.abs
      #
      # |@pattern
      # (abs arg_)
      #
      # |@summary
      # Absolute value of an arithmetic unit.
      #
      # |@block
      # Returns the absolute value of an arithmetic unit *arg*. If *arg* is not a valid
      # arithmetic unit, returns `0`.
      #
      # |@example
      # ```wwml
      # (abs 10)  ;; => 10
      # (abs -10) ;; => 10
      # (abs -∞)  ;; => ∞
      # ```

      # Fast path
      matchpiT %{(abs ±arg)} do
        Term.of(arg.abs)
      end

      matchpiT %{(abs arg_)} do
        next Term.of(0) unless a = arith?(arg)
        next Term.of(0) if a.is_a?(ArithIndet)

        render(abs(a))
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

      matchpiT %{(ml/term ml_blob)} do
        Term.of(:ok, ML.term(ml.to_string))
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

      matchpiT %{(with subject_dict key_ value_)} do
        Term.of(subject.with(key, value))
      end

      matchpiT %{(without subject_dict key_)} do
        Term.of(subject.without(key))
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

      matchpi %{(iota _)} do
        Term.of
      end

      matchpi %{(flatten arg_ ¦ -depth)}, %{(flatten arg_ ¦ depth: ∞)} do
        Term.flatten(arg, depth: nil)
      end

      matchpi %{(flatten arg_ ¦ depth_: (%number +i32))}, depth: Int32 do
        Term.flatten(arg, depth: depth)
      end

      matchpiT %{(map (attn data_dict mask_dict) head_symbol)} do |data, mask|
        data = data.transaction do |commit|
          mask.each_entry do |key, _|
            unless value = data[key]?
              mask = mask.without(key)
              next
            end

            # ?!
            rep = eval(it, vars.with(:arg, value), Term.of(head, :arg))
            commit.with(key, rep)
          end
        end

        Term.of(:attn, data, mask)
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

      # |@ nitrene.data
      #
      # |@pattern
      # (data attn←(attn _dict _dict))
      #
      # |@key attn nitrene.attn
      # The attention dictionary to extract the data from.
      #
      # |@block
      # Extracts the data part of attention (the first dictionary, i.e.,
      # `(attn data_dict _dict)`).
      #
      # `nitrene.data` keeps *all* entries regardless of the mask. This is different
      # from `nitrene.gather`, which only keeps selected entries.
      matchpi %{(data (attn data_dict _dict))} do
        data
      end

      # |@ nitrene.mask
      #
      # |@pattern
      # (mask attn←(attn _dict _dict))
      #
      # |@key attn nitrene.attn
      # The attention dictionary to extract the mask from.
      #
      # |@block
      # Extracts the mask part of attention (the second dictionary, i.e.,
      # `(attn _ mask_dict)`).
      matchpi %{(mask (attn _dict mask_dict))} do
        mask
      end

      matchpiT %{(gather (attn data_dict mask_dict))} do
        result = Term::Dict.build do |commit|
          data.items.each_with_index do |item, index|
            next unless index.in?(mask)

            commit << item
          end

          data.each_entry(in: Term::Dict.pairspart) do |key, value|
            next unless key.in?(mask)

            commit.with(key, value)
          end
        end

        Term.of(result)
      end

      matchpi %{(gather _)} do
        Term.of
      end

      # |@ nitrene.zip
      #
      # |@pattern
      # (zip attn_ attachments_dict)
      #
      # |@key attn nitrene.attn
      #
      # |@key attachments
      # A dictionary containing attachments for selected items (read sequentially) and
      # pairs (read by key).
      #
      # |@block
      # Replaces selected item and pair values with `(value attachment)`. Item
      # attachments are read sequentially from the itemspart of *attachments*.
      # Pair attachments are read by key. Leaves unselected items intact. Unselects
      # entries without a corresponding attachment.
      #
      # ```wwml
      # ;; Convert a list of digits into the corresponding base-10 number:
      #
      # (let digits: (1 2 3 4 5)
      #   (-> digits
      #     ;; Generate a sequence of increasing numbers: (0 1 2 3 4)
      #     (iota (itemsize _))
      #     ;; Convert to attention: (⏏0⏏ ⏏1⏏ ⏏2⏏ ⏏3⏏ ⏏4⏏)
      #     (attn _)
      #     ;; Convert to powers of ten: (⏏1⏏ ⏏10⏏ ⏏100⏏ ⏏1000⏏ ⏏10000⏏)
      #     (map _ (fn ±p (** 10 p)))
      #     ;; Reverse selected: (⏏10000⏏ ⏏1000⏏ ⏏100⏏ ⏏10⏏ ⏏1⏏)
      #     (reverse _)
      #     ;; Annotate with matching digits: (⏏(10000 1)⏏ ⏏(1000 2)⏏ ⏏(100 3)⏏ ⏏(10 4)⏏ ⏏(1 5)⏏)
      #     (zip _ digits)
      #     ;; Multiply: (⏏10000⏏ ⏏2000⏏ ⏏300⏏ ⏏40⏏ ⏏5⏏)
      #     (map _ (fn (±a ±b) (* a b)))
      #     ;; Sum selected: (10000 2000 300 40 5) -> 12345
      #     (sum _)))
      #
      # ;; => 12345
      # ```
      matchpi %{(zip (attn data_dict mask_dict) attachments_dict)} do |mask|
        result = data.transaction do |commit|
          cursor = attachments.items

          data.items.each_with_index do |item, index|
            next unless index.in?(mask)

            if cursor.empty?
              mask = mask.without(index)
              next
            end

            attmt = cursor.first
            cursor += 1

            commit.with(index, {item, attmt})
          end

          data.each_entry(in: Term::Dict.pairspart) do |key, value|
            next unless key.in?(mask)

            unless attmt = attachments[key]?
              mask = mask.without(key)
              next
            end

            commit.with(key, {value, attmt})
          end
        end

        Term.of(:attn, result, mask)
      end

      matchpi %{(zip _ _)} do
        Term.of(:attn, Term[], Term[])
      end

      # |@ nitrene.reverse
      #
      # |@pattern
      # (reverse attn_)
      #
      # |@key attn nitrene.attn
      #
      # |@block
      # Reverses selected items. Leaves pairs intact.
      #
      # ```wwml
      # ;; Reverse even numbers:
      #
      # (-> (1 2 3 4 5 6 7 8)
      #   ;; Convert to attention: (⏏1⏏ ⏏2⏏ ⏏3⏏ ⏏4⏏ ⏏5⏏ ⏏6⏏ ⏏7⏏ ⏏8⏏)
      #   (attn _)
      #   ;; Select even: (1 ⏏2⏏ 3 ⏏4⏏ 5 ⏏6⏏ 7 ⏏8⏏)
      #   (filter _ (%pipe (mod 2) 0))
      #   ;; Reverse selected: (1 ⏏8⏏ 3 ⏏6⏏ 5 ⏏4⏏ 7 ⏏2⏏)
      #   (reverse _)
      #   ;; Get the data part of attention: (1 8 3 6 5 4 7 2)
      #   (data _))
      #
      # ;; => (1 8 3 6 5 4 7 2)
      # ```
      matchpi %{(reverse (attn data_dict mask_dict))} do
        indices = Pf::Kit.stack_array(UInt32)

        mask.each_entry do |key, _|
          next unless index = data.index32?(key)

          indices << index
        end

        indices.reverse!

        result = data.transaction do |commit|
          n = 0
          mask.each_entry do |key, _|
            next unless index = data.index32?(key)

            commit.with(index, data[indices[n]])
            n += 1
          end
        end

        Term.of(:attn, result, mask)
      end

      matchpi %{(reverse _)} do
        Term.of(:attn, Term[], Term[])
      end

      matchpiT %{(set arg_dict)} do
        set = Term::Dict.build do |commit|
          arg.items.each do |item|
            commit.with(item, true)
          end
        end

        Term.of(set)
      end

      matchpi %{(in? (b_ ..< e_) value_)} do
        lo = arith?(b)
        continue if lo.nil? || lo.is_a?(ArithIndet)

        hi = arith?(e)
        continue if hi.nil? || hi.is_a?(ArithIndet)

        n = arith?(value)
        continue if n.nil? || n.is_a?(ArithIndet)

        eq_lo = false
        if lo.is_a?(ArithConst) && n.is_a?(ArithConst)
          eq_lo = (lo.value <=> n.value).zero?
        end

        Term.of((eq_lo || lt?(lo, n)) && lt?(n, hi))
      end

      matchpi %{(substring? haystack_string needle_string)}, haystack: String, needle: String do
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

      # TODO: ditto
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

      matchpi %{(balance text_string ⍊ nest_string unnest_string)}, text: StringView, nest: String, unnest: String do
        Term.of(balance(text, nest, unnest))
      end

      matchpi %{(rbalance text_string ⍊ nest_string unnest_string)}, text: StringView, nest: String, unnest: String do
        Term.of(rbalance(text, nest, unnest))
      end

      matchpiT %{(chr codepoint←(%number u32))} do
        begin
          chr = codepoint.chr
        rescue ArgumentError
          continue
        end

        Term.of(chr)
      end

      matchpiT %{(chr _)} do
        Term.of("")
      end

      matchpiT %{(ord arg_string)}, arg: String do
        continue if arg.empty?

        Term.of(arg[0].ord)
      end

      matchpiT %{(ord _)} do
        Term.of(:indet)
      end

      # |@ nitrene.join
      #
      # |@pattern
      # (join arg←(attn _dict _dict))
      #
      # |@key arg nitrene.attn
      #
      # |@block
      # Concatenates all selected strings in the itemspart of the attention's
      # data dict, inorder (key 0 onwards).
      #
      # ```wwml
      # (join (attn ("a" "b" "c"))) ;; => "abc"
      # ```

      matchpiT %{(join (attn data_dict mask_dict))} do
        whole = String.build do |io|
          data.items.each_with_index do |item, index|
            next unless item = item.as_s?
            next unless index.in?(mask)

            io << item.to(String)
          end
        end

        Term.of(whole)
      end

      # |@ nitrene.join
      #
      # |@pattern
      # (join arg_dict)
      #
      # |@key arg
      # The dictionary to join the items of.
      #
      # |@block
      # Concatenates all strings in the itemspart of the *arg* dictionary,
      # inorder (key 0 onwards).
      #
      # ```wwml
      # (join ("a" "b" "c")) ;; => "abc"
      # ```

      # Fast path
      matchpi %{(join (item_string))} do
        item
      end

      matchpiT %{(join arg_dict)} do
        capacity = 64u64

        # If there aren't a lot of items, let's do an extra scan to determine
        # the exact bytesize needed.
        if arg.itemsize < 64
          capacity = 0u64

          arg.items.each do |item|
            next unless item = item.as_s?

            capacity &+= item.to(String).bytesize
          end

          if capacity.zero? # Necessarily an empty string.
            return Term.of("")
          end
        end

        whole = String.build(capacity) do |io|
          arg.items.each do |item|
            next unless item = item.as_s?

            io << item.to(String)
          end
        end

        Term.of(whole)
      end

      # |@ nitrene.reduce*
      #
      # |@pattern
      # (reduce* (initial_ arg←(attn data_dict mask_dict))
      #   (fn pattern_ bodyQ_))
      #
      # |@key initial
      # The initial value for the accumulator.
      #
      # |@key arg nitrene.attn
      # The attention whose items to iterate over.
      #
      # |@key pattern m1.operator
      # The pattern to use to accept or reject pairs `(acc_ item_)`. *acc* is
      # the running value of the accumulator, and *item* is an item from *arg*.
      # Captures made by the pattern are available to *body*.
      #
      # |@key body nitrene
      # An expression to evaluate for each pair matched by *pattern*. Captures
      # made by *pattern* are available in the body.
      #
      # |@block
      # Iterates over selected values, letting each value selected by *pattern*
      # to contribute to the accumulator (starting with *initial*). Returns
      # the resulting value of the accumulator.
      #
      # ```wwml
      # ;; Sum even numbers in 0 ..< 10:
      #
      # (-> (iota 10) ;; Generate the list (0 1 ... 9)
      #     (attn _) ;; Convert to attention
      #     (filter _ (%pipe (mod 2) 0)) ;; Select even
      #     (reduce* (0 _) (fn (±m ±n) (+ m n)))) ;; Sum
      #
      # ;; => 20
      # ```
      matchpiT %{(reduce* (initial_ (attn data_dict mask_dict)) (fn pattern_ bodyQ_))} do
        acc = initial

        data.items.each_with_index do |item, index|
          next unless index.in?(mask)

          input = Term.of(acc, item)
          next unless env = M1.match?(pattern, input)

          acc = eval(it, Term.union(vars, env), bodyQ)
        end

        acc
      end

      # TODO: Remove in favor of partition/rpartition
      matchpi %{(line/stem arg_string)}, arg: StringView do
        l, _, _ = arg.partition('\n')
        Term.of(l)
      end

      # TODO: Remove in favor of partition/rpartition
      matchpi %{(line/rest arg_string)}, arg: StringView do
        _, sep, r = arg.partition('\n')
        Term.of(sep &+ r)
      end

      # TODO: Remove in favor of partition/rpartition
      matchpi %{(rline/stem arg_string)}, arg: StringView do
        _, _, r = arg.rpartition('\n')
        Term.of(r)
      end

      # TODO: Remove in favor of partition/rpartition
      matchpi %{(rline/rest arg_string)}, arg: StringView do
        l, sep, _ = arg.rpartition('\n')
        Term.of(l &+ sep)
      end

      # (run (prefix matcheeQ_) (charset prefixQ_))
      matchpi %{(prefix-run matchee_string prefix_string)}, matchee: StringView, prefix: String do |matchee|
        run = String.build do |io|
          while matchee.starts_with?(prefix)
            matchee = matchee.lskip(prefix.size)
            io << prefix
          end
        end

        Term.of(run)
      end

      matchpi %{(wrap arg_string ¦ maxwidth_: (%optional 60 (%number +i32)))}, arg: String, maxwidth: Int32 do
        Term.of(wrap(arg, maxwidth))
      end

      matchpi %{(chomp arg_string)}, arg: String do
        Term.of(arg.chomp)
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

      # |@ nitrene.classify
      #
      # |@pattern
      # (classify arg_blob)
      #
      # |@block
      # If *arg* does not have a media type set, guesses the media type using
      # PantoMIME. Returns the classified copy of *arg*.
      #
      # NOTE: To avoid any misunderstanding, this doesn't copy the underlying bytes;
      # blobs are immutable so this is never needed. Only metadata is copied.
      #
      # |@example
      # ```wwml
      # (classify ⟬fe ed be ef⟭) ;; => ⟬fe ed be ef ⁑ application/octet-stream⟭
      # ```
      #
      # `classify` preserves an existing media type, even if it is wrong:
      #
      # ```wwml
      # (classify ⟬fe ed be ef ⁑ text/plain⟭) ;; => ⟬fe ed be ef ⁑ text/plain⟭
      # ```
      matchpiT %{(classify arg_blob)} do
        Term.of(Term::Blob.classify(arg))
      end

      # |@ nitrene.unclassify
      #
      # |@pattern
      # (unclassify arg_blob)
      #
      # |@block
      # Strips the media type from *arg* if it has one.
      #
      # |@example
      # ```wwml
      # (unclassify ⟬fe ed be ef ⁑ application/octet-stream⟭) ;; => ⟬fe ed be ef⟭
      # ```
      #
      # You can use `unclassify` in combination with `classify` to force a guessed media
      # type instead of using the one provided with the blob:
      #
      # ```wwml
      # (classify (unclassify ⟬fe ed be ef ⁑ text/plain⟭))
      # ;; => ⟬fe ed be ef ⁑ application/octet-stream⟭
      #
      # ;; The above can be rewritten more neatly as:
      # (-> ⟬fe ed be ef ⁑ text/plain⟭ (unclassify _) (classify _))
      # ```
      matchpiT %{(unclassify arg_blob)} do
        Term.of(Term::Blob.unclassify(arg))
      end

      matchpiT %{(finite arg_ or: alt_)} do
        a = arith?(arg)
        a.is_a?(ArithConst) && !(a.value.infinite? || a.value.nan?) ? arg : alt
      end

      otherwise { Inert.new }
    end
  end

  # Evaluates a Nitrene expression *expr* using the default evaluation context.
  # Returns the resulting term.
  def eval(vars : Term::Dict, expr : Term) : Term
    eval(Interpreter::DEFAULT, vars, expr)
  end

  private def balance(text : StringView, nest : String, unnest : String)
    depth = 1

    text.each_before_and_after do |before, after|
      if before.ends_with?(nest)
        depth += 1
      elsif before.ends_with?(unnest)
        depth -= 1
      end

      if depth.zero?
        return before, after
      end
    end

    {text.before_begin, text}
  end

  private def rbalance(text : StringView, nest : String, unnest : String)
    depth = 1

    text.reverse_each_before_and_after do |before, after|
      if after.starts_with?(nest)
        depth += 1
      elsif after.starts_with?(unnest)
        depth -= 1
      end

      if depth.zero?
        return before, after
      end
    end

    {text, text.after_end}
  end
end
