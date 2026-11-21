module Ww::Alloy
  # :nodoc:
  defrecord Pattern,
    term : Term,
    op : M1::Op::Any,
    captures : Set(Term),
    specificity : M1::Specificity

  def Pattern.new(term : Term) : Pattern
    Pattern.new(term, M1.normal(term))
  end

  def Pattern.new(term : Term, normp : M1::Normp) : Pattern
    captures = M1.captures(normp).to_set { |(capture, _)| capture }
    specificity = M1.specificity(normp)
    op = M1.operator(normp)

    Pattern.new(term, op, captures, specificity)
  end

  # Represents a recognized Alloy template.
  alias Template = NiExpr | Var | DisplayVar | SpliceVar | NiSplice | Literal |
                   Splice | Module | Case | Cond | VarsCase | Each | Let | Site |
                   Extend | Render

  defcase NiExpr, term : Term
  defcase Var, name : Term::Sym
  defcase SpliceVar, name : Term::Sym
  defcase DisplayVar, name : Term::Sym
  defcase NiSplice, expr : NiExpr
  defcase Literal, term : Term
  defcase Splice, weight : UInt64, children : Slice(Template)
  defcase Module, bindings : Slice({Term::Sym, Term::Sym}), body : Splice
  defcase Case, expr : NiExpr, branches : Slice(CaseWhen)
  defcase VarsCase, branches : Slice(CaseWhen)
  defcase Cond, expr : NiExpr, truthy : Template, falsey : Template
  defcase CaseWhen, pattern : Pattern, body : Splice

  alias Each = EachItem | EachItemEntry | EachPairEntry | EachEntry

  defcase EachItem, iterable : NiExpr, pattern : Pattern, body : Splice
  defcase EachItemEntry, iterable : NiExpr, pattern : Pattern, body : Splice
  defcase EachPairEntry, iterable : NiExpr, pattern : Pattern, body : Splice
  defcase EachEntry, iterable : NiExpr, pattern : Pattern, body : Splice
  defcase Let, bindings : Slice({Term::Sym, NiExpr}), body : Splice

  # NOTE: *dict* is supplementary; *parts* includes all its entries. We use *dict*
  # to avoid extra work in cases such as (+ 1 ^a 2), where we only replace ^a, i.e.,
  # do only one `Term::Dict#with` call.
  defcase Site, weight : UInt64, dict : Term::Dict, parts : Hash(Term, Template)
  defcase Extend, child : Template, extension : NiExpr
  defcase Render, pattern : Pattern, subordinate : Template, body : Splice

  def Splice.new(children : Slice(Template)) : Splice
    Splice.new(children.sum(0u64) { |child| Alloy.weigh(child) }, children)
  end

  def Site.new(dict : Term::Dict, parts : Hash(Term, Template)) : Site
    # See `Site` to learn why we don't hash *dict* in.
    Site.new(parts.sum { |_, part| Alloy.weigh(part) }, dict, parts)
  end

  # :nodoc:
  def weigh(template : NiExpr | Var | DisplayVar | SpliceVar | NiSplice | Literal) : UInt64
    1u64
  end

  # :nodoc:
  def weigh(template : Splice) : UInt64
    template.weight
  end

  # :nodoc:
  def weigh(template : Module) : UInt64
    1u64 + weigh(template.body)
  end

  # :nodoc:
  def weigh(template : Case | VarsCase) : UInt64
    1u64 + template.branches.sum { |branch| weigh(branch.body) }
  end

  # :nodoc:
  def weigh(template : Cond) : UInt64
    1u64 + weigh(template.truthy) + weigh(template.falsey)
  end

  # :nodoc:
  def weigh(template : Each) : UInt64
    1u64 + weigh(template.body)
  end

  # :nodoc:
  def weigh(template : Let) : UInt64
    1u64 + weigh(template.body)
  end

  # :nodoc:
  def weigh(template : Site) : UInt64
    template.weight
  end

  # :nodoc:
  def weigh(template : Extend) : UInt64
    1u64 + weigh(template.child)
  end

  # :nodoc:
  def weigh(template : Render) : UInt64
    1u64 + weigh(template.subordinate) + weigh(template.body)
  end

  {% if flag?(:docs) %}
    # Returns the *weight* of a *template*. We use the weight for determining
    # how "heavy" a template is during caching, multiplied by some constant
    # factor. Heavier templates occupy "more" cache, which may result in
    # earlier evictions later on, even if the cache's item capacity is not
    # yet exceeded.
    def weigh(template : Template) : UInt64
    end
  {% end %}
end
