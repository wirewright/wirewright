module Ww::M1
  # The depth propagation algorithm, executed by every operator in the normal
  # pattern tree during the depth propagation pass.
  private def depthp1(op : Term::Dict) : Term::Dict
    unless expr = op[:depth]?
      return op.with(:"min-depth", 0).with(:"max-depth", :"∞")
    end

    range = RangeCalc.eval(expr, Term.of(:"min-depth"), Term.of(:"max-depth"), op)
    Term.matchpi(range, %{(min_ ..= max_)}, engine: M0) do
      op.with(:"min-depth", min).with(:"max-depth", max)
    end
  end

  # Runs the depth propagation algorithm on operators in *pattern*.
  #
  # Each operator is annotated with `depth: expr_`, where *expr* is a range
  # expression evaluated against the operator and its children, using `RangeCalc`.
  # The result of the expression -- a range -- determines the operator's `min-`
  # and `max-depth`.
  #
  # If missing, `depth: (0 ..= ∞)`.
  #
  # Depths flow up, from leaves to the root. After depth propagation, all
  # operators have a set `min-depth` (`+i32`) and `max-depth` (`+i32`, inclusive;
  # or `∞`, meaning unknown, and for comparison purposes, infinite).
  #
  # Non-dict operators have their `min-depth` and `max-depth` both set to `0`,
  # meaning only depth `0` (i.e., no depth) is acceptable.
  def depthp(pattern : Normp) : Normp
    pattern
      .map { |op| Kit.ascend(op, &->depthp1(Term::Dict)) }
      .with_annotation(:depths)
  end

  # Returns the dict depth bounds accepted by *pattern*.
  def depth(pattern : Normp) : {Magnitude, Magnitude}
    unless pattern.annotations.depths?
      pattern = depthp(pattern)
    end

    pattern.unwrap do |op|
      min = op[:"min-depth"]?
      max = op[:"max-depth"]?

      {(min || Term.of(0)).to(Magnitude),
       (max.nil? || max == Term.of(:"∞")) ? Magnitude::INFINITY : max.to(Magnitude)}
    end
  end

  # The bounds propagation algorithm, executed by every operator in the normal
  # pattern tree during the bounds propagation pass.
  private def boundsp1(op : Term::Dict) : Term::Dict
    unless expr = op[:bounds]?
      return op.with(:"min-bounds", 0).with(:"max-bounds", :"∞")
    end

    range = RangeCalc.eval(expr, Term.of(:"min-bounds"), Term.of(:"max-bounds"), op)
    Term.matchpi(range, %{(min_ ..= max_)}, engine: M0) do
      op.with(:"min-bounds", min).with(:"max-bounds", max)
    end
  end

  # Runs the bounds propagation algorithm on operators in *pattern*.
  #
  # Each operator is annotated with `bounds: expr_`, where *expr* is a range
  # expression evaluated against the operator and its children, using `RangeCalc`.
  # The result of the expression -- a range -- determines the operator's `min-`
  # and `max-bounds`.
  #
  # If missing, `bounds: (0 ..= ∞)`.
  #
  # Bounds flow up, from leaves to the root. After bounds propagation, all
  # operators have a set `min-bounds` (`+i32`) and `max-bounds` (`+i32`, inclusive;
  # or `∞`, meaning unknown, and for comparison purposes, infinite).
  #
  # Non-dict operators have their `min-bounds` and `max-bounds` both set to `0`,
  # meaning only size `0` (i.e., no bounds) is acceptable.
  def boundsp(pattern : Normp) : Normp
    pattern
      .map { |op| Kit.ascend(op, &->boundsp1(Term::Dict)) }
      .with_annotation(:bounds)
  end

  # Returns the dict size bounds accepted by *normp*.
  def bounds(pattern : Normp) : {Magnitude, Magnitude}
    unless pattern.annotations.bounds?
      pattern = boundsp(pattern)
    end

    pattern.unwrap do |op|
      min = op[:"min-bounds"]?
      max = op[:"max-bounds"]?

      {(min || Term.of(0)).to(Magnitude),
       (max.nil? || max == Term.of(:"∞")) ? Magnitude::INFINITY : max.to(Magnitude)}
    end
  end

  # The literal propagation algorithm, executed by every operator in the normal
  # pattern tree during the literal propagation pass.
  private def literalp1(op : Term::Dict) : Term::Dict
    Term.case(op, engine: M0) do
      matchpi %{[%'%literal term_]}, cue: :"%literal" do
        op.with(:literals, Term.set(term))
      end

      otherwise do
        level = Term::Dict.build do |commit|
          Kit.each_member(op) do |member|
            next if member[:sealed]?
            next unless literals = member[:literals]?

            literals.each_entry do |literal, _|
              commit.with(literal, true)
            end
          end
        end

        op.with(:literals, level)
      end
    end
  end

  # Runs the literal propagation algorithm on operators in *pattern*.
  #
  # Literals flow up, from leaves to the root. After literal propagation, all
  # operators have a set `literals`. It is a dict set of literals required
  # by an operator and its non-sealed subtree.
  def literalp(pattern : Normp) : Normp
    pattern
      .map { |op| Kit.ascend(op, &->literalp1(Term::Dict)) }
      .with_annotation(:literals)
  end

  # The sketch propagation algorithm, executed by every operator in the normal
  # pattern tree during the sketch propagation pass.
  private def sketchp1(op : Term::Dict) : Term::Dict
    return op unless literals = op[:literals]?
    return op unless literals = literals.as_d?

    sketch = Term::Dict::Sketch.empty

    literals.each_entry do |literal, _|
      if dict = literal.as_d?
        sketch = Term::Dict::Sketch.union(sketch, dict.fresh_sketch)
        next
      end

      sketch = Term::Dict::Sketch.union(sketch, Term::Dict::Sketch.symbol(literal))
    end

    op.with(:sketch, sketch.bits)
  end

  # Runs the sketch propagation algorithm on operators in *pattern*.
  #
  # Sketch propagation depends on literal propagation. It currently takes
  # into account only symbol literals found in literal sets.
  #
  # Thus, `sketchp` requires *pattern* to have been processed by `literalp` first.
  def sketchp(pattern : Normp) : Normp
    assert pattern.annotations.literals?

    pattern
      .map { |op| Kit.ascend(op, &->sketchp1(Term::Dict)) }
      .with_annotation(:sketches)
  end

  # Returns the dict sketch accepted by *pattern*.
  #
  # Dictionaries matched against the pattern are expected to be supersets of
  # the returned sketch.
  def sketch(pattern : Normp) : Term::Dict::Sketch
    unless pattern.annotations.sketches?
      pattern = sketchp(pattern)
    end

    pattern.unwrap do |op|
      if sketch = op[:sketch]?
        return sketch.to(Term::Dict::Sketch)
      end

      Term::Dict::Sketch.new(0)
    end
  end

  # The capture propagation algorithm, executed by every operator in the normal
  # pattern tree during the capture propagation pass.
  private def capturesp1(op : Term::Dict) : Term::Dict
    Term.case(op, engine: M0) do
      # In things such as (%any° x_ y_ z_), the capture flow is altered a little
      # bit in that %any° asks its members for captures, and picks only captures
      # they *all* share.
      matchpi %{{¦ disjunction}} do
        intersection = nil

        Kit.each_member(op) do |member|
          Term.case(member, engine: M0) do
            matchpi %{{¦ captures_: (_*)}} do
              capture_set = captures.items.to_set

              if intersection.nil?
                intersection = capture_set
                next
              end

              intersection &= capture_set
            end

            otherwise do
              # One of our members (e.g. %any° branches) lacks captures for
              # whatever reason. Thus, abort: we can't say anything definite.
              return op
            end
          end
        end

        op.with(:captures, intersection)
      end

      otherwise do
        capture_list = Term::Dict.build do |commit|
          Kit.each_in_fanout(op) do |successor|
            Term.case(successor, engine: M0) do
              matchpi %{(%'%capture name_ ⍊ tags_: (_*))}, cue: :"%capture" do
                commit << {name, tags}
              end

              matchpi %{[%'%capture name_]}, cue: :"%capture" do
                commit << {name, Term[]}
              end

              matchpi %{{¦ captures_: (_*)}} do
                commit.concat(captures.items)
              end

              otherwise { }
            end
          end
        end

        op.with(:captures, capture_list)
      end
    end
  end

  # Runs the sketch propagation algorithm on operators in *pattern*.
  #
  # This adds the `captures` entry to every operator, holding a list of captures
  # that this operator and its members make.
  #
  # Each capture is of the form `(name_ (tags_*))`. *tags* is a list of tags
  # associated with that capture.
  #
  # A capture that hits `%any°` on its way up is stopped for inspection. Only
  # if all branches of `%any°` also make the same-named capture is it allowed
  # to proceed flowing up.
  def capturesp(pattern : Normp) : Normp
    pattern
      .map { |op| Kit.ascend(op, &->capturesp1(Term::Dict)) }
      .with_annotation(:captures)
  end

  # Returns the set of captures made in *pattern*.
  #
  # See `capturesp` for info on how captures are represented.
  def captures(pattern : Normp) : Set(Term)
    unless pattern.annotations.captures?
      pattern = capturesp(pattern)
    end

    pattern.unwrap do |op|
      next Set(Term).new unless captures = op[:captures]?
      next Set(Term).new unless dict = captures.as_d?

      dict.items.to_set
    end
  end

  # Returns the set of capture names of captures made in *pattern*.
  def capture_names(pattern : Normp) : Set(Term)
    captures(pattern).to_set { |(name, _)| name }
  end

  private def guarded1(op : Term::Dict) : Term::Dict
    Term.case(op, engine: M0) do
      matchpi %{{¦ guarded}} do
        min_depth = op[:"min-depth"]? || Term.of(0)
        max_depth = op[:"max-depth"]? || Term.of(:∞)
        min_bounds = op[:"min-bounds"]? || Term.of(0)
        max_bounds = op[:"max-bounds"]? || Term.of(:∞)
        sketch = op[:sketch]? || Term.of(0)

        if sketch == Term.of(0) && {min_depth, max_depth} == {Term.of(0), Term.of(:"∞")} && {min_bounds, max_bounds} == {Term.of(0), Term.of(:"∞")}
          return op
        end

        Term[:"%guard", op,
          "min-depth": min_depth,
          "max-depth": max_depth,
          "min-bounds": min_bounds,
          "max-bounds": max_bounds,
          "sketch": sketch,
        ]
      end

      otherwise { op }
    end
  end

  # Wraps `guarded: true` operators in *pattern* with the guard operator `%guard`.
  #
  # The result is a *guarded normal form* of the pattern: `Guardedp`. It is
  # no longer eligible for functions that want the normal form. On the other
  # hand, it becomes eligible for `operator`, `optimal`, and so on.
  def guard(pattern : Normp) : Guardedp
    opts = {
      pattern.annotations.depths?,
      pattern.annotations.bounds?,
      pattern.annotations.sketches?,
    }

    if opts.none?
      return pattern.unwrap { |op| Guardedp.new(op) }
    end

    pattern.unwrap do |op|
      rewritten = Kit.ascend(op, &->guarded1(Term::Dict))

      Guardedp.new(rewritten)
    end
  end
end
