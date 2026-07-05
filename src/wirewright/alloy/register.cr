module Ww::Alloy
  alias Ref = Term::Sym

  private def refsum?(reftab, path, components, templates : Enumerable)
    sum = nil
    valid = true

    templates.each do |template|
      register(reftab, path, components, template)
      next unless valid

      unless refs = reftab[template]?
        sum = nil
        valid = false
        next
      end

      sum ||= Set(Ref).new
      sum.concat(refs)
    end

    sum
  end

  # ^(+ x y)
  #
  # TODO: We should try to be at least a little bit smart here...
  private def register(reftab, path, components, template : NiExpr) : Nil
  end

  # ^x ^\x ^*xs
  private def register(reftab, path, components, template : Var | DisplayVar | SpliceVar) : Nil
    return if reftab.has_key?(template)

    reftab[template] = Set{template.name}
  end

  private def register(reftab, path, components, template : NiSplice) : Nil
    register(reftab, path, components, template.expr)
  end

  private def register(reftab, path, components, template : Literal) : Nil
    reftab.put_if_absent(template) { Set(Ref).new }
  end

  private def register(reftab, path, components, template : Splice) : Nil
    return if reftab.has_key?(template)
    return unless refs = refsum?(reftab, path, components, template.children)

    reftab[template] = refs
  end

  private def register(reftab, path, components, template : Module) : Nil
    return if reftab.has_key?(template)

    refs = template.bindings.to_set { |inner, _| inner }
    reftab[template] = refs
  end

  private def register(reftab, path, components, template : Case) : Nil
    return if reftab.has_key?(template)

    fanout = {template.expr}.each.chain(template.branches.each)
    return unless refs = refsum?(reftab, path, components, fanout)

    reftab[template] = refs
  end

  private def register(reftab, path, components, template : CaseWhen) : Nil
    return if reftab.has_key?(template)

    register(reftab, path, components, template.body)
    return unless refs = reftab[template.body]?

    # Shadowing.
    refs -= template.pattern.captures

    reftab[template] = refs
  end

  private def register(reftab, path, components, template : VarsCase) : Nil
    register(reftab, path, components, template.branches)
  end

  private def register(reftab, path, components, template : Cond) : Nil
    return if reftab.has_key?(template)

    fanout = {template.expr, template.truthy, template.falsey}
    return unless refs = refsum?(reftab, path, components, fanout)

    reftab[template] = refs
  end

  private def register(reftab, path, components, template : EachItem | EachItemEntry | EachPairEntry | EachEntry) : Nil
    return if reftab.has_key?(template)

    fanout = {template.iterable, template.body}
    return unless refs = refsum?(reftab, path, components, fanout)

    # Shadowing.
    refs -= template.pattern.captures

    reftab[template] = refs
  end

  private def register(reftab, path, components, template : Let) : Nil
    return if reftab.has_key?(template)

    fanout = {template.body}.each.chain(template.bindings.each.map { |_, expr| expr })
    return unless refs = refsum?(reftab, path, components, fanout)

    # Either shadowed or defined (and thus not from env.vars).
    template.bindings.each do |inner, _|
      refs.delete(inner)
    end

    reftab[template] = refs
  end

  private def register(reftab, path, components, template : Site) : Nil
    return if reftab.has_key?(template)

    register(reftab, path, components, template.parts.each_value)

    # If the head is unrecognizable or only known at runtime, we cannot
    # determine our refs, since for that we must know which components we hit,
    # and without a compile-time known head, we can't do that. For example:
    #
    #   (+ x_ y_) => ^(+ x y)
    #   (- x_ y_) => ^(- x y)
    #   (^head 1 2)
    #
    # ^head can be anything at runtime: +, -, or something else entirely. There is
    # no way to know that at compile-time, so we give up here.
    return unless head = template.parts[Term.of(0)]?
    return unless head.is_a?(Literal)

    refs = Set(Ref).new

    # If one of our children doesn't know its refs, then we also don't know
    # our refs.
    template.parts.each_value do |child|
      return unless child_refs = reftab[child]?

      refs.concat(child_refs)
    end

    components.each do |component|
      next unless component.head.nil? || component.head == head.term
      # Skip self in recursive calls. Assume recursive calls cannot contribute
      # new refs.
      next unless path.add?(component)

      begin
        register(reftab, path, components, component.template)
      ensure
        path.delete(component)
      end

      # If one of the rules we depend on doesn't know its refs, we also don't.
      return unless component_refs = reftab[component.template]?

      # Shadowing.
      component_refs -= component.pattern.captures

      refs.concat(component_refs)
    end

    reftab[template] = refs
  end

  private def register(reftab, path, components, template : Extend) : Nil
    return if reftab.has_key?(template)

    fanout = {template.child, template.extension}
    return unless refs = refsum?(reftab, path, components, fanout)

    reftab[template] = refs
  end

  private def register(reftab, path, components, template : Render) : Nil
    return if reftab.has_key?(template)

    fanout = {template.subordinate, template.body}
    return unless refs = refsum?(reftab, path, components, fanout)

    # Shadowing.
    refs -= template.pattern.captures

    reftab[template] = refs
  end

  private def register(reftab, path, components, templates : Enumerable) : Nil
    templates.each do |template|
      register(reftab, path, components, template)
    end
  end

  # Constructs a _r_eference _t_able for *unit*: a table mapping `Template`
  # or `CaseWhen` nodes to sets of references (`Ref`) they contain. If a node
  # is absent in the returned table, then the ref set is unknown; the node is
  # a "black box" and is assumed to refer to all possible references.
  def refs(unit : Unit) : Reftab
    reftab = {} of Template | CaseWhen => Set(Ref)

    path = Set(Component).new
    path.compare_by_identity

    register(reftab, path, unit.components, unit.template)

    reftab
  end
end
