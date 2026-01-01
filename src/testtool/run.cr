module Testtool
  alias Leaf = AlloyTest |
               MicrofoldTest |
               MLeq |
               MLpos |
               MLneg |
               BackmapEq |
               BackmapNeg |
               PatternVarEq |
               PatternEq |
               PatternPos |
               PatternNeg |
               HeadEq |
               HeadAbsent |
               BoundsEq |
               DepthEq |
               SpecificityTest |
               D7test |
               EditTest |
               TermComparison |
               ImageComparison

  # Returns `true` if an expected D7 *frame* matches *actual*.
  def d7matches?(frame : Term, actual : Term) : Bool
    Term.case(frame) do
      matchpi %{(frame content_*)} do
        content == actual
      end

      matchpi %{(frame content_* ¦ () m1)} do
        M1.probe?(content, actual)
      end

      matchpi %{end} do
        false
      end
    end
  end

  # :ditto:
  def d7matches?(frame : Term, actual : Iterator::Stop) : Bool
    frame == Term.of(:end)
  end

  defrecord D7test, seed : Term, frames : Array(Term)

  def run(test : D7test, assets, stat, complaints) : Nil
    frames = D7.frames(Rack.clf, test.seed, Rack.tspace, Rack.master)

    # Skip through seed.
    _ = frames.next

    before = test.seed

    test.frames.each do |after|
      actual = measure(stat) { frames.next }

      unless d7matches?(after, actual)
        complaints << complaint("D7 frame mismatch",
          before: before,
          after: after,
          got: actual.as?(Term) || Term.of(:end),
        )

        break
      end

      break if actual.is_a?(Iterator::Stop)

      before = actual
    end
  end

  defrecord AlloyTest, vars : Term::Dict, template : Term, expansion : Term, issues : Term::Dict

  def run(test : AlloyTest, assets, stat, complaints) : Nil
    actual, issues = measure(stat) { Alloy.render_with_issues(test.vars, test.template) }

    unless actual == test.expansion
      complaints << complaint("Alloy template expansion mismatch", expansion: actual)
    end

    issues.each do |issue|
      next if test.issues.items.any? { |detail| Term.of(issue.detail) == detail }

      complaints << complaint("Unexpected Alloy issue in template", issue: Term.of(issue.detail))
    end

    test.issues.items.each do |detail|
      next if issues.any? { |issue| Term.of(issue.detail) == detail }

      complaints << complaint("Missing Alloy issue in template", issue: detail)
    end
  end

  defrecord MicrofoldTest, variants : Array(Term), problems : Term::Dict

  def run(test : MicrofoldTest, assets, stat, complaints) : Nil
    ok = true
    renders = [] of Term

    test.variants.each do |variant|
      render, issues = measure(stat) { Microfold.render(assets.theme, variant, severity: :minor) }
      if issues.present?
        ok = false
      end

      issues.each do |issue|
        problem = Term.of(Term::Sym.new(issue.severity.to_s.underscore), issue.detail)
        next if problem.in?(test.problems.items)

        complaints << complaint("Unexected Microfold render problem", problem: problem)
      end

      renders << render
    end

    # Test expects problems but we haven't found any.
    if ok && test.problems.nonempty?
      complaints << complaint("Microfold render did not detect any problems")
    end

    reference = renders[0]
    (1...renders.size).each do |index|
      render = renders[index]
      next if render == reference

      variant = test.variants[index]

      complaints << complaint("Microfold render mismatch",
        reference: reference,
        variant: variant,
        render: render,
      )
    end
  end

  defrecord EditTest, seed : Term, msgs : Array(Term), result : Term

  def run(test : EditTest, assets, stat, complaints) : Nil
    state = test.seed
    test.msgs.each do |msg|
      state = measure(stat) { rewrite(Soma.dispatch(state, msg), assets.editR) }
    end

    return if state == test.result # ok

    complaints << complaint("editR state mismatch", expected: test.result, got: state)
  end

  def match(pattern : Term, matchee : Term) : Array(Term::Dict)
    matches = nil

    levels = {M1::O2, M1::O1, M1::O0}
    levels.each_with_index do |level, index|
      envs = M1.matches(pattern, matchee, opt: level)

      if index.zero?
        matches = envs
        next
      end

      unless matches == envs
        raise "#{level} does not match like #{levels.first}"
      end
    end

    assert matches

    # NOTE: It's very sloppy but currently we're storing backpaths in envs. Users
    # can't (at least they shouldn't) match on them or even know about them. So we
    # remove them from all envs.
    matches.map!(&.without(:"(backpaths)"))
  end

  defrecord PatternVarEq, pattern : Term, name : Term, matches : Set(Term)
  defrecord PatternEq, pattern : Term, matchee : Term, matches : Set(Term)
  defrecord PatternPos, pattern : Term, whitelist : Set(Term)
  defrecord PatternNeg, pattern : Term, blacklist : Set(Term)

  def run(test : PatternVarEq, assets, stat, complaints) : Nil
    unless matchee = assets.vars[test.name]?
      complaints << complaint("Var not found")
      return
    end

    envs = measure(stat) { match(test.pattern, matchee) }
    return if envs.to_set == test.matches # ok

    complaints << complaint("Pattern mismatch", matched: Term.of(envs))
  end

  def run(test : PatternEq, assets, stat, complaints) : Nil
    envs = measure(stat) { match(test.pattern, test.matchee) }
    return if envs.to_set == test.matches # ok

    complaints << complaint("Pattern mismatch", matched: Term.of(envs))
  end

  def run(test : PatternPos, assets, stat, complaints) : Nil
    test.whitelist.each do |item|
      envs = measure(stat) { match(test.pattern, item) }
      next unless envs.empty? # ok

      complaints << complaint("Pattern mismatch", item: item)
    end
  end

  def run(test : PatternNeg, assets, stat, complaints) : Nil
    test.blacklist.each do |item|
      envs = measure(stat) { match(test.pattern, item) }
      next if envs.empty? # ok

      complaints << complaint("Pattern was not expected to match",
        item: item,
        matched: Term.of(envs),
      )
    end
  end

  defrecord BackmapEq, pattern : Term, backspec : Term, matchee : Term, whitelist : Set(Term)
  defrecord BackmapNeg, pattern : Term, backspec : Term, blacklist : Set(Term)

  def run(test : BackmapEq, assets, stat, complaints) : Nil
    result = measure(stat) { M1.backmap?(test.pattern, test.backspec, test.matchee) }
    return if result.in?(test.whitelist) # ok

    complaints << complaint("Backmapped term is not in whitelist", result: Term.of(result || "<none>"))
  end

  def run(test : BackmapNeg, assets, stat, complaints) : Nil
    test.blacklist.each do |matchee|
      next unless result = measure(stat) { M1.backmap?(test.pattern, test.backspec, matchee) } # ok

      complaints << complaint("Backmapped term found in blacklist", result: result)
    end
  end

  enum MLentity
    Term
    Document
  end

  defrecord MLeq, sources : Array(String), entity : MLentity
  defrecord MLpos, sources : Array(String), pattern : Term, entity : MLentity
  defrecord MLneg, source : String, detail : String, entity : MLentity

  def run(test : MLeq, assets, stat, complaints) : Nil
    subjects = test.sources.compact_map do |source|
      case test.entity
      in .term?     then measure(stat) { ML.term(source) }
      in .document? then measure(stat) { ML.document(source) }
      end
    rescue e : ML::SyntaxError
      complaints << complaint("Syntax error in one of subjects", error: e)

      nil
    end

    (1...subjects.size).each do |index|
      next if subjects[0] == subjects[index] # ok

      complaints << complaint("Terms are not equal", expected: subjects[0], got: subjects[index])
    end
  end

  def run(test : MLpos, assets, stat, complaints) : Nil
    subjects = test.sources.compact_map do |source|
      case test.entity
      in .term?     then measure(stat) { ML.term(source) }
      in .document? then measure(stat) { ML.document(source) }
      end
    rescue e : ML::SyntaxError
      complaints << complaint("Syntax error in one of subjects", error: e)

      nil
    end

    subjects.each do |subject|
      next if M1.probe?(test.pattern, subject) # ok

      complaints << complaint("Term does not match pattern", term: subject)
    end
  end

  def run(test : MLneg, assets, stat, complaints) : Nil
    source = test.source.delete('⏏')

    begin
      case test.entity
      in .term?     then term = ML.term(source)
      in .document? then term = ML.document(source)
      end
    rescue e : ML::SyntaxError
      from, to = e.text.char_start, e.text.char_end

      if from == to
        actual = source.insert(from, "⏏")
      else
        actual = source.insert(to, "⏏").insert(from, "⏏")
      end

      if test.source == actual && e.detail.includes?(test.detail)
        return # ok
      end

      complaints << complaint("Reader failed incorrectly",
        "Expected detail like": Term.of(test.detail),
        "Expected error": Term.of(test.source),
        "Got detail": Term.of(e.detail),
        "Got error": Term.of(actual),
      )
    else
      complaints << complaint("Reader did not fail",
        "Expected detail like": Term.of(test.detail),
        "Expected error": Term.of(test.source),
        "Got term": term,
      )
    end
  end

  defrecord HeadEq, pattern : Term, head : Term

  def run(test : HeadEq, assets, stat, complaints) : Nil
    normp = M1.normal(test.pattern)
    head = measure(stat) { M1.head?(normp) }
    return if head == test.head # ok

    complaints << complaint("Pattern head mismatch",
      "Normal pattern": normp,
      "Got head": Term.of(head || "<none>"),
    )
  end

  defrecord HeadAbsent, patterns : Set(Term)

  def run(test : HeadAbsent, assets, stat, complaints) : Nil
    test.patterns.each do |pattern|
      normp = M1.normal(pattern)
      head = measure(stat) { M1.head?(normp) }
      next if head.nil? # ok

      complaints << complaint("Pattern has head but it was not expected to",
        "Normal pattern": normp,
        "Got head": head,
      )
    end
  end

  defrecord BoundsEq, patterns : Set(Term), bounds : Term::Dict
  defrecord DepthEq, patterns : Set(Term), depth : Term::Dict

  {% for row in { {BoundsEq, :bounds}, {DepthEq, :depth} } %}
    {% testcls, kind = row %}

    def run(test : {{testcls}}, assets, stat, complaints) : Nil
      test.patterns.each do |pattern|
        normp = M1.normal(pattern)
        range = measure(stat) { M1.{{kind.id}}(normp) }
        actual = Term.of(
          min: range[0] == Magnitude::INFINITY ? nil : range[0],
          max: range[1] == Magnitude::INFINITY ? nil : range[1],
        )

        next if test.{{kind.id}} == actual

        complaints << complaint("Pattern {{kind.id}} mismatch",
          "Pattern": pattern,
          "Normal pattern": normp,
          "Expected": Term.of(test.{{kind.id}}),
          "Got": actual,
        )
      end
    end
  {% end %}

  defrecord SpecificityTest, levels : Array(Set(Term))

  def run(test : SpecificityTest, assets, stat, complaints) : Nil
    spec_prev = nil

    test.levels.each do |level|
      spec_level = nil

      level.each do |pattern|
        normp = M1.normal(pattern)
        spec_pattern = measure(stat) { M1.specificity(normp, toplevel: true) }
        spec_level ||= spec_pattern
        next if spec_level == spec_pattern # ok

        complaints << complaint("Specificity mismatch",
          pattern: pattern,
          expected: Term.of(spec_level),
          got: Term.of(spec_pattern),
        )
      end

      next unless spec_level # empty level, ok

      unless spec_prev
        spec_prev = spec_level
        next # ok
      end

      if spec_prev < spec_level
        spec_prev = spec_level
        next # ok
      end

      complaints << complaint("Specificity levels out of order",
        "Level": Term.of(level),
        "Previous level specificity": Term.of(spec_prev),
        "Current level specificity": Term.of(spec_level),
      )
    end
  end

  record TermComparison,
    a : TermComparand,
    b : TermComparand

  def run(test : TermComparison, assets, stat, complaints) : Nil
    if measure(stat) { termcmp(test.a) == termcmp(test.b) }
      return # ok
    end

    complaints << complaint("Terms are different")
  end

  record ImageComparison,
    a : ImageComparand,
    b : ImageComparand

  def run(test : ImageComparison, assets, stat, complaints) : Nil
    lhs, rhs = measure(stat) do
      {ppmcmp(assets, test.a), ppmcmp(assets, test.b)}
    end

    return if lhs == rhs # ok

    results = { {test.a, lhs}, {test.b, rhs} }
    results.each do |(comparand, ppm)|
      next unless comparand.is_a?(ImageComparandWithTemp)

      tempdst = Path[Dir.tempdir] / "#{comparand.temp}.out.ppm"

      warn("Oops, images are different. Writing artifact to #{tempdst}")

      assets.files.write(tempdst, ppm)
    end

    complaints << complaint("Images are different")
  end
end
