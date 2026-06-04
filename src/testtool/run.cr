module Testtool
  alias Leaf = AlloyTest |
               MicrofoldTest |
               MLeq |
               MLpos |
               MLneg |
               BackmapEq |
               BackmapNeg |
               BacksysTest |
               PatternEq |
               PatternPos |
               PatternNeg |
               HeadEq |
               HeadAbsent |
               BoundsEq |
               DepthEq |
               SpecificityTest |
               CapturesEq |
               RackTest |
               EditTest |
               SceneryTest |
               TermComparison |
               ImageComparison

  enum RackComparisonResult
    Match
    Mismatch
    More

    def self.new(bool : Bool)
      bool ? Match : Mismatch
    end
  end

  # Returns `true` if an expected Rack *frame* matches *actual*.
  def rack_compare(frame : Term, actual : Term)
    Term.case(frame) do
      matchpi %{(frame content_*)} do
        RackComparisonResult.new(content == actual)
      end

      matchpi %{(frame/tail content_*)} do
        RackComparisonResult.new(actual.items.ends_with?(content.items))
      end

      matchpi %{(frame pattern_ ¦ () pattern)} do
        RackComparisonResult.new(M1.probe?(pattern, actual))
      end

      matchpi %{(frame pattern_ ¦ () pattern fast-forward)} do
        if M1.probe?(pattern, actual)
          return RackComparisonResult::Match
        end

        RackComparisonResult::More
      end

      matchpi %{terminates} do
        RackComparisonResult::More
      end

      matchpi %{end} do
        RackComparisonResult::Mismatch
      end
    end
  end

  # :ditto:
  def rack_compare(frame : Term, actual : Iterator::Stop)
    RackComparisonResult.new(frame.in?(Term.of(:end), Term.of(:terminates)))
  end

  defrecord RackTest, seed : Term, frames : Array(Term)

  def run(test : RackTest, assets, stat, complaints) : Nil
    assembler_state = Rack::Assembler.state

    frames = D7.coarse_frames(Rack.clf, test.seed,
      Rack::Tspace.pass(Rack.clf),
      Rack::Assembler.pass(Rack.clf, Rack.clf, assembler_state),
      Rack.pass(Rack.clf),
    )

    # Skip through seed.
    before = frames.next

    test.frames.each do |after|
      if before.is_a?(Iterator::Stop)
        complaints << complaint("Rack stopped producing frames but a frame was expected", expected: after)
        break
      end

      loop do
        actual = measure(stat) { frames.next }

        case rack_compare(after, actual)
        in .match?
          before = actual
          break
        in .mismatch?
          complaints << complaint("Rack frame mismatch",
            before: before.as(Term),
            after: after,
            got: actual.as?(Term) || Term.of(:end),
          )
          break
        in .more?
          before = actual
        end
      end
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

  defrecord MicrofoldTest, variants : Array(Term)

  def run(test : MicrofoldTest, assets, stat, complaints) : Nil
    unless codex = assets.mu_codex
      complaints << complaint("missing Microfold codex (did you run with `--assets-none`?)")
      return
    end

    instances = test.variants.map do |variant|
      measure(stat) { Microfold.render(codex, variant) }
    end

    return if instances.all? { |instance| instances[0].unwrap == instance.unwrap } # ok

    complaints << complaint("Microfold render mismatch",
      instances: Term.of(instances.map(&.unwrap)),
      diagnostics: Term.of(instances.map(&.diagnostics.inspect)),
    )
  end

  defrecord EditTest, seed : Term, msgs : Array(Term), result : Term

  def run(test : EditTest, assets, stat, complaints) : Nil
    unless editR = assets.editR
      complaints << complaint("missing editR (did you run with `--assets-none`?)")
      return
    end

    state = test.seed
    test.msgs.each do |msg|
      state = measure(stat) { Rho.rewrite(editR, Soma.dispatch(state, msg)) }
    end

    return if state == test.result # ok

    complaints << complaint("editR state mismatch", expected: test.result, got: state)
  end

  defrecord SceneryTest,
    path : NormalPath,
    in : Term,
    out_png : Term::Blob,
    hit : Term?,
    width : Magnitude,
    height : Magnitude,
    backdrop : Pigment::RGBA,
    microfold : Bool

  def run(test : SceneryTest, assets, stat, complaints) : Nil
    in_ruleset, in_rest = Ruleset.ruleset_and_rest(Ruleset::DEFAULT_SELECTOR, test.in)
    in_instance = Alloy.compose(in_ruleset, Term[], Alloy.template(Term[], Term.of(in_rest)))

    if test.microfold
      unless codex = assets.mu_codex
        complaints << complaint("test requires Microfold but Microfold codex is missing")
        return
      end

      scenery_in = Microfold.render(codex, in_instance).unwrap
    else
      scenery_in = in_instance
    end

    cache = Scenery::Safe.cache

    scene_out, png = measure(stat) do
      scene_out = Scenery::Safe.scene(cache, scenery_in, test.width, test.height)
      raster = Scenery::Safe.rasterize(cache, scene_out.unwrap, backdrop: test.backdrop)
      {scene_out, raster.to_png}
    end

    unless png == test.out_png
      fail_path = NormalPath["/tmp/scenery.#{test.path.stem}.fail.png"]
      complaints << complaint("in.wwml does not match out.png (writing to #{fail_path})")
      scene_out.diagnostics.each do |diagnostic|
        complaints << complaint("diagnostic: #{diagnostic.inspect}")
      end
      PathService.write(fail_path, png).wait
      return
    end

    return unless hit_doc = test.hit # ok

    hit_doc.items.each do |item|
      Term.case(item) do
        matchpi %{[hit query_ pattern_]} do
          hit_query = Scenery::HitQuery.parse(query)
          vantages = Scenery::Safe.describe(cache, scene_out.unwrap, Slice[hit_query])
          unless vantages.any? { |vantage| M1.probe?(pattern, vantage) }
            complaints << complaint("did not hit according to", query: query, pattern: pattern, vantages: Term.of(vantages))
          end
        end

        otherwise { }
      end
    end
  end

  def match(pattern : Term, matchee : Term) : Slice(Term::Dict)
    matches = nil

    levels = {M1::O2, M1::O2only, M1::O1, M1::O0}
    levels.each_with_index do |level, index|
      op = M1.operator(pattern, opt: level)
      envs = M1.matches(Term[], op, matchee)

      if index.zero?
        matches = envs
        next
      end

      unless matches == envs
        raise "#{level} does not match like #{levels.first}"
      end
    end

    matches.not_nil!
  end

  defrecord PatternEq, pattern : Term, matchee : Term, matches : Set(Term)
  defrecord PatternPos, pattern : Term, whitelist : Set(Term)
  defrecord PatternNeg, pattern : Term, blacklist : Set(Term)

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
  defrecord BacksysTest, backsys : Array({Term, Term}), seq : Array(Term)

  def run(test : BackmapEq, assets, stat, complaints) : Nil
    result = measure(stat) { M1.backmap?(test.pattern, test.backspec, test.matchee) }

    if test.whitelist.empty?
      # Reuse empty whitelist for conflict-checking, which manifests as
      # the backmap replying with the same matchee.
      return if test.matchee == result # ok
    else
      return if result.in?(test.whitelist) # ok
    end

    complaints << complaint("Backmapped term is not in whitelist", result: Term.of(result || :MISMATCH))
  end

  def run(test : BackmapNeg, assets, stat, complaints) : Nil
    test.blacklist.each do |matchee|
      next unless result = measure(stat) { M1.backmap?(test.pattern, test.backspec, matchee) } # ok

      complaints << complaint("Backmapped term found in blacklist", result: result)
    end
  end

  def run(test : BacksysTest, assets, stat, complaints) : Nil
    return unless state = test.seq.first? # ok, empty seq

    test.seq.each(within: 1..) do |expected|
      result = measure(stat) { M1.backmap(test.backsys, state) }
      unless result == expected
        complaints << complaint("backsystem frame mismatch",
          state: state,
          frame: result,
          expected: expected,
        )
        return
      end

      state = result
    end

    # ok
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

      complaints << complaint("Terms are not equal", expected: subjects[index], got: subjects[0])
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
      "Normal pattern": Term.of(normp.unwrap(&.itself)),
      "Got head": Term.of(head || :MISMATCH),
    )
  end

  defrecord HeadAbsent, patterns : Set(Term)

  def run(test : HeadAbsent, assets, stat, complaints) : Nil
    test.patterns.each do |pattern|
      normp = M1.normal(pattern)
      head = measure(stat) { M1.head?(normp) }
      next if head.nil? # ok

      complaints << complaint("Pattern has head but it was not expected to",
        "Normal pattern": Term.of(normp.unwrap(&.itself)),
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
          min: range[0] == Magnitude::INFINITY ? nil : range[0].to_i,
          max: range[1] == Magnitude::INFINITY ? nil : range[1].to_i,
        )

        next if test.{{kind.id}} == actual

        complaints << complaint("Pattern {{kind.id}} mismatch",
          "Pattern": pattern,
          "Normal pattern": Term.of(normp.unwrap(&.itself)),
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
        spec_pattern = measure(stat) { M1.specificity(normp) }
        spec_level ||= spec_pattern
        next if spec_level == spec_pattern # ok

        complaints << complaint("Specificity mismatch",
          pattern: pattern,
          expected: Term.of(spec_level.to_s),
          got: Term.of(spec_pattern.to_s),
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
        "Previous level specificity": Term.of(spec_prev.to_s),
        "Current level specificity": Term.of(spec_level.to_s),
      )
    end
  end

  defrecord CapturesEq, pattern : Term, captures : Array({Term, Term})

  def run(test : CapturesEq, assets, stat, complaints) : Nil
    normp = M1.normal(test.pattern)
    captures = measure(stat) { M1.captures(normp) }

    ok = captures.all? do |(name, tags)|
      test.captures.any? do |(candidate, tagsp)|
        next unless name == candidate

        M1.probe?(tagsp, tags)
      end
    end

    return if ok

    complaints << complaint("Pattern captures mismatch",
      "Pattern": test.pattern,
      "Normal pattern": Term.of(normp.unwrap(&.itself)),
      "Expected": Term.of(test.captures),
      "Got": Term.of(captures),
    )
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

  {% if flag?(:dwuir) %}
    def run(test : ImageComparison, assets, stat, complaints) : Nil
      unless uiR = assets.uiR
        complaints << complaint("missing uiR (did you run with `--assets-none`?)")
        return
      end

      lhs, rhs = measure(stat) do
        {ppmcmp(assets.dw, uiR, test.a), ppmcmp(assets.dw, uiR, test.b)}
      end

      return if lhs == rhs # ok

      results = { {test.a, lhs}, {test.b, rhs} }
      results.each do |(comparand, ppm)|
        next unless comparand.is_a?(ImageComparandWithTemp)

        tempdst = Path[Dir.tempdir] / "#{comparand.temp}.out.ppm"

        warn("Oops, images are different. Writing artifact to #{tempdst}")

        PathService.write(NormalPath[tempdst], Term::Blob.new(ppm)).wait
      end

      complaints << complaint("Images are different")
    end
  {% else %}
    def run(test : ImageComparison, assets, stat, complaints) : Nil
      complaints << complaint("Skipped")
    end
  {% end %}
end
