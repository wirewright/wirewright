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

  # Represents a recognized Alloy template. All templates have an associated 256-bit
  # hashcode to simplify equality checks and hashing.
  alias Template = NiExpr | Var | DisplayVar | SpliceVar | NiSplice | Literal |
                   Splice | Module | Case | Cond | VarsCase | Each | Let | Site |
                   Extend | Render

  defcase NiExpr, hashcode : Term::H256, term : Term, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase Var, hashcode : Term::H256, name : Term::Sym, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase SpliceVar, hashcode : Term::H256, name : Term::Sym, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase DisplayVar, hashcode : Term::H256, name : Term::Sym, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase NiSplice, hashcode : Term::H256, expr : NiExpr, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase Literal, hashcode : Term::H256, term : Term, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase Splice, hashcode : Term::H256, weight : UInt64, children : Slice(Template), equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase Module, hashcode : Term::H256, bindings : Slice({Term::Sym, Term::Sym}), body : Splice, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase Case, hashcode : Term::H256, expr : NiExpr, branches : Slice(CaseWhen), equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase VarsCase, hashcode : Term::H256, branches : Slice(CaseWhen), equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase Cond, hashcode : Term::H256, expr : NiExpr, truthy : Template, falsey : Template, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase CaseWhen, hashcode : Term::H256, pattern : Pattern, body : Splice, equality: :ref do
    def_equals_and_hash hashcode
  end

  alias Each = EachItem | EachItemEntry | EachPairEntry | EachEntry

  defcase EachItem, hashcode : Term::H256, iterable : NiExpr, pattern : Pattern, body : Splice, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase EachItemEntry, hashcode : Term::H256, iterable : NiExpr, pattern : Pattern, body : Splice, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase EachPairEntry, hashcode : Term::H256, iterable : NiExpr, pattern : Pattern, body : Splice, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase EachEntry, hashcode : Term::H256, iterable : NiExpr, pattern : Pattern, body : Splice, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase Let, hashcode : Term::H256, bindings : Slice({Term::Sym, NiExpr}), body : Splice, equality: :ref do
    def_equals_and_hash hashcode
  end

  # NOTE: *dict* is supplementary; *parts* includes all its entries. We use *dict*
  # to avoid extra work in cases such as (+ 1 ^a 2), where we only replace ^a, i.e.,
  # do only one `Term::Dict#with` call.
  defcase Site, hashcode : Term::H256, weight : UInt64, dict : Term::Dict, parts : Hash(Term, Template), equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase Extend, hashcode : Term::H256, child : Template, extension : NiExpr, equality: :ref do
    def_equals_and_hash hashcode
  end

  defcase Render, hashcode : Term::H256, pattern : Pattern, subordinate : Template, body : Splice, equality: :ref do
    def_equals_and_hash hashcode
  end

  private def digest256(io, arg : Class) : Nil
    arg.to_s(io)
  end

  private def digest256(io, arg : Term | Term::Any) : Nil
    io << "{"
    ML.compact(io, arg)
    io << "}"
  end

  private def digest256(io, arg : Term::H256) : Nil
    io << "<"
    arg.write(io)
    io << ">"
  end

  private def digest256(io, arg : Template | CaseWhen) : Nil
    io << "<"
    arg.hashcode.write(io)
    io << ">"
  end

  private def digest256(io, arg : Pattern) : Nil
    digest256(io, arg.term)
  end

  private def digest256(io, arg : Enumerable) : Nil
    io << "["
    arg.each_with_index do |element, index|
      io << " " if index > 0
      digest256(io, element)
    end
    io << "]"
  end

  protected def seq256(*args) : Term::H256
    Term::H256.new do |io|
      io << "("
      args.each_with_index do |arg, index|
        io << " " if index > 0
        digest256(io, arg)
      end
      io << ")"
    end
  end

  # Smart constructors.

  def NiExpr.new(term : Term) : NiExpr
    NiExpr.new(Alloy.seq256(NiExpr, term), term)
  end

  def Var.new(name : Term::Sym) : Var
    Var.new(Alloy.seq256(Var, name), name)
  end

  def DisplayVar.new(name : Term::Sym) : DisplayVar
    DisplayVar.new(Alloy.seq256(DisplayVar, name), name)
  end

  def SpliceVar.new(name : Term::Sym) : SpliceVar
    SpliceVar.new(Alloy.seq256(SpliceVar, name), name)
  end

  def NiSplice.new(expr : NiExpr) : NiSplice
    NiSplice.new(Alloy.seq256(NiSplice, expr), expr)
  end

  def Literal.new(term : Term) : Literal
    Literal.new(Alloy.seq256(Literal, term), term)
  end

  def Splice.new(children : Slice(Template)) : Splice
    Splice.new(Alloy.seq256(Splice, children), children.sum(0u64) { |child| Alloy.weigh(child) }, children)
  end

  def Module.new(bindings : Slice({Term::Sym, Term::Sym}), body : Splice) : Module
    Module.new(Alloy.seq256(Module, bindings, body), bindings, body)
  end

  def Case.new(expr : NiExpr, branches : Slice(CaseWhen)) : Case
    Case.new(Alloy.seq256(Case, expr, branches), expr, branches)
  end

  def CaseWhen.new(pattern : Pattern, body : Splice) : CaseWhen
    CaseWhen.new(Alloy.seq256(CaseWhen, pattern, body), pattern, body)
  end

  def VarsCase.new(branches : Slice(CaseWhen)) : VarsCase
    VarsCase.new(Alloy.seq256(VarsCase, branches), branches)
  end

  def Cond.new(expr : NiExpr, truthy : Template, falsey : Template) : Cond
    Cond.new(Alloy.seq256(Cond, expr, truthy, falsey), expr, truthy, falsey)
  end

  {% for cls in %w[EachItem EachItemEntry EachPairEntry EachEntry] %}
    def {{cls.id}}.new(iterable : NiExpr, pattern : Pattern, body : Splice) : {{cls.id}}
      {{cls.id}}.new(Alloy.seq256({{cls.id}}, iterable, pattern, body), iterable, pattern, body)
    end
  {% end %}

  def Let.new(bindings : Slice({Term::Sym, NiExpr}), body : Splice) : Let
    Let.new(Alloy.seq256(Let, bindings, body), bindings, body)
  end

  def Site.new(dict : Term::Dict, parts : Hash(Term, Template)) : Site
    # See `Site` to learn why we don't hash *dict* in.
    Site.new(Alloy.seq256(Site, parts), parts.sum { |_, part| Alloy.weigh(part) }, dict, parts)
  end

  def Extend.new(child : Template, extension : NiExpr) : Extend
    Extend.new(Alloy.seq256(Extend, child, extension), child, extension)
  end

  def Render.new(pattern : Pattern, subordinate : Template, body : Splice) : Render
    Render.new(Alloy.seq256(Render, pattern, subordinate, body), pattern, subordinate, body)
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
