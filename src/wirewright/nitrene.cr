module Ww::Nitrene
  extend self

  alias Out = Outcome::Accepted(Term)

  private def ok(object)
    Outcome.ok(Term.of(object))
  end

  private def ok_despite(object, *args)
    Outcome.ok_despite(Term.of(object), *args)
  end

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
  # which seems to be a good source for this kind of stuff.

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

  private def plug(dict : Term::Dict, args : Indexable(Out), keysrc, *rest)
    Outcome.accumulate do |acc|
      result = dict.transaction do |commit|
        keysrc.each_with_index do |key, index|
          value = acc.unwrap(args[index])
          commit.with(key, value)
        end
      end

      ok_despite(Term.of(:literal, result), *rest)
    end
  end

  private def calc(vars : Term::Dict, op : Term::Dict, *, filter : T.class = Arith, &) forall T
    args = Pf::Kit.stack_array(Out, 8)
    operands = Pf::Kit.stack_array(T, 8)

    # Evaluate subterms.
    subterms = op.items.move(1)
    subterms.each_with_index(offset: 1) do |subterm, index|
      arg = eval(vars, subterm).at(index)
      args << arg
    end

    # Validate arguments.
    args.map! do |arg|
      arg.bind do |value|
        if arith = arith?(value).as?(T)
          operands << arith

          ok(value)
        else
          ok_despite(value, "unrecognized argument")
        end
      end
    end

    if operands.size < args.size
      return plug(op, args, 1...op.itemsize, "one or more of the arguments not recognized")
    end

    yield operands
  end

  # Evaluates a Nitrene expression *term* using the given variables dict *vars*.
  def eval(vars : Term::Dict, term : Term) : Out
    Term.case(term) do
      matchpi %{'subterm_} do
        ok(subterm)
      end

      matchpi %{(^ _)} do
        ok(term)
      end

      matchpi %{(+)} do
        ok(0)
      end

      matchpi %{(+ _ _*)} do
        calc(vars, term.as_d) do |operands|
          ok(render(operands.reduce { |a, b| add(a, b) }))
        end
      end

      matchpi %{(- _)} do
        calc(vars, term.as_d) do |operands| # for some reason (operand) doesn't work here...
          ok(render(negate(operands.first)))
        end
      end

      matchpi %{(- _ _ _*)} do
        calc(vars, term.as_d) do |operands|
          ok(render(operands.reduce { |a, b| sub(a, b) }))
        end
      end

      matchpi %{(*)} do
        ok(1)
      end

      matchpi %{(* _ _*)} do
        calc(vars, term.as_d) do |operands|
          ok(render(operands.reduce { |a, b| mul(a, b) }))
        end
      end

      matchpi %{(/ _)} do
        calc(vars, term.as_d) do |operands|
          ok(render(div(ArithConst.new(Term[1]), operands.first)))
        end
      end

      matchpi %{(/ _ _ _*)} do
        calc(vars, term.as_d) do |operands|
          ok(render(operands.reduce { |a, b| div(a, b) }))
        end
      end

      matchpi %{(// subterm0_ subterm1_)} do
        Outcome.bind(eval(vars, subterm0).at(1), eval(vars, subterm1).at(2)) do |v, w|
          unless p = v.as_n?
            next ok_despite(Term.of(:literal, Term.morph(term, {1, v}, {2, w})), "not a number")
          end

          unless q = w.as_n?
            next ok_despite(Term.of(:literal, Term.morph(term, {1, v}, {2, w})), "not a number")
          end

          if q.zero?
            next ok_despite(Term.of(:literal, Term.morph(term, {1, v}, {2, w})), "division by zero")
          end

          ok(p // q)
        end
      end

      matchpi %{(mod subterm0_ subterm1_)} do
        Outcome.bind(eval(vars, subterm0).at(1), eval(vars, subterm1).at(2)) do |v, w|
          unless p = v.as_n?
            next ok_despite(Term.of(:literal, Term.morph(term, {1, v}, {2, w})), "not a number")
          end

          unless q = w.as_n?
            next ok_despite(Term.of(:literal, Term.morph(term, {1, v}, {2, w})), "not a number")
          end

          if q.zero?
            next ok_despite(Term.of(:literal, Term.morph(term, {1, v}, {2, w})), "division by zero")
          end

          ok(p % q)
        end
      end

      matchpi %{(< _*)} do
        calc(vars, term.as_d, filter: ArithConst | ArithPosInf | ArithNegInf) do |operands|
          result = true

          operands.each_cons_pair do |pred, succ|
            next if lt?(pred, succ)

            result = false
            break
          end

          ok(result)
        end
      end

      matchpi %{(<= _*)} do
        calc(vars, term.as_d, filter: ArithConst | ArithPosInf | ArithNegInf) do |operands|
          result = true

          operands.each_cons_pair do |pred, succ|
            next if pred == succ || lt?(pred, succ)
            next if pred.is_a?(ArithConst) && succ.is_a?(ArithConst) && (pred.value <=> succ.value).zero?

            result = false
            break
          end

          ok(result)
        end
      end

      matchpi %{(> _*)} do
        calc(vars, term.as_d, filter: ArithConst | ArithPosInf | ArithNegInf) do |operands|
          result = true

          operands.each_cons_pair do |pred, succ|
            next if lt?(succ, pred)

            result = false
            break
          end

          ok(result)
        end
      end

      matchpi %{(>= _*)} do
        calc(vars, term.as_d, filter: ArithConst | ArithPosInf | ArithNegInf) do |operands|
          result = true

          operands.each_cons_pair do |pred, succ|
            next if pred == succ || lt?(succ, pred)
            next if pred.is_a?(ArithConst) && succ.is_a?(ArithConst) && (pred.value <=> succ.value).zero?

            result = false
            break
          end

          ok(result)
        end
      end

      matchpi %{(=)} do
        ok(true)
      end

      matchpi %{(= ref_ _*)} do
        ok(term.items.move(1).all? { |other| ref == other })
      end

      matchpi %{(in-range? arg-value_ (arg-b_ ..< arg-e_))} do
        Outcome.accumulate(amend: true) do |acc| # ?!
          value = acc.unwrap(eval(vars, arg_value).at(1))
          b = acc.unwrap(eval(vars, arg_b).at(2, 0))
          e = acc.unwrap(eval(vars, arg_e).at(2, 2))

          n = arith?(value)
          if n.nil? || n.is_a?(ArithIndet)
            next ok_despite(Term.of(:literal, Term.morph(term, {1, value}, {2, 0, b}, {2, 2, e})), "not a comparable arithmetic unit").at(1)
          end

          lo = arith?(b)
          if lo.nil? || lo.is_a?(ArithIndet)
            next ok_despite(Term.of(:literal, Term.morph(term, {1, value}, {2, 0, b}, {2, 2, e})), "not a comparable arithmetic unit").at(2, 0)
          end

          hi = arith?(e)
          if hi.nil? || hi.is_a?(ArithIndet)
            next ok_despite(Term.of(:literal, Term.morph(term, {1, value}, {2, 0, b}, {2, 2, e})), "not a comparable arithmetic unit").at(2, 2)
          end

          ok((lo == n || lt?(lo, n)) && lt?(n, hi))
        end
      end

      matchpi %{(approx arg_)} do
        eval(vars, arg).at(1).bind do |v|
          if n = v.as_n?
            ok(Term::Num.approx(n))
          else
            ok_despite(Term.of(:literal, Term.morph(term, {1, v})), "not a number")
          end
        end
      end

      matchpi %{(floor arg_)} do
        eval(vars, arg).at(1).bind do |v|
          if n = v.as_n?
            ok(Term::Num.exact(n.floor))
          else
            ok_despite(Term.of(:literal, Term.morph(term, {1, v})), "not a number")
          end
        end
      end

      matchpi %{(ceil arg_)} do
        eval(vars, arg).at(1).bind do |v|
          if n = v.as_n?
            ok(Term::Num.exact(n.ceil))
          else
            ok_despite(Term.of(:literal, Term.morph(term, {1, v})), "not a number")
          end
        end
      end

      matchpi %{(round arg_)} do
        eval(vars, arg).at(1).bind do |v|
          if n = v.as_n?
            ok(Term::Num.exact(n.round))
          else
            ok_despite(Term.of(:literal, Term.morph(term, {1, v})), "not a number")
          end
        end
      end

      matchpi %{_dict} do
        Outcome.accumulate do |acc|
          result = term.transaction do |commit|
            term.each_entry do |key, value0|
              value1 = acc.unwrap(eval(vars, value0).at(key))
              commit.with(key, value1)
            end
          end

          ok(result)
        end
      end

      matchpi %{_symbol} do
        unless value = vars[term]?
          return ok_despite(term, "unrecognized symbol", term)
        end

        ok(value)
      end

      otherwise do
        ok(term)
      end
    end
  end
end
