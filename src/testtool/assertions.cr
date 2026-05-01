module Testtool
  # Returns an array of assertions made in *test*.
  def assertions(test : Test) : Array(Assertion(Test))
    items = test.document.items
    items.flat_map_with_index do |item, index|
      itemsrc = test.srcmap.cd(index)

      asns = assertions(Top.new(test.path, item), itemsrc)
      asns.map { |asn| Assertion.new(loc(asn, itemsrc), test) }
    end
  end

  # Returns an array of assertions made in *comparison*.
  def assertions(comparison : Comparison) : Array(Assertion(Comparison))
    assertions(comparison.op).map { |asn| Assertion.new(asn, comparison) }
  end

  # Returns an array of assertions made in a Scenery *test*.
  def assertions(topic : SceneryGroup) : Array(Assertion(SceneryGroup))
    listing = PathService.listing(topic.path).wait.unwrap
    unless listing.is_a?(PathService::DirListing)
      raise ArgumentError.new("path is not a directory")
    end

    # Schedule reads so they're all happening simultaneously.
    test_cases = listing.entries.compact_map do |entry|
      next unless entry.is_a?(PathService::DirEntry)

      {entry.path,
       {in:  PathService.read(entry.path / "in.wwml"),
        out: PathService.read(entry.path / "out.ppm"),
        hit: PathService.read(entry.path / "hit.wwml")}}
    end

    # Wait for all reads to finish.
    test_cases = test_cases.map do |(path, test_case)|
      {path, test_case.transform { |_, promise| promise.wait }}
    end

    # Only consider ContentReading.
    test_cases = test_cases.map do |(path, test_case)|
      {path, test_case.transform do |key, result|
        case reading = result.unwrap
        in PathService::ContentReading
          reading.blob
        in PathService::DigestReading, PathService::Absent
        end
      end}
    end

    # Parse and remove nils.
    test_cases = test_cases.compact_map do |path, test_case|
      in_blob = test_case[:in]
      out_blob = test_case[:out]
      hit_blob = test_case[:hit]
      unless in_blob && out_blob
        warn("Skipping malformed test: must contain in.wwml and out.ppm", path)
        next
      end

      begin
        in_doc = ML.document(in_blob.to_string, filename: "in.wwml")
        if hit_blob
          hit_doc = ML.document(hit_blob.to_string, filename: "hit.wwml")
        end
      rescue e : ML::SyntaxError
        warn("Skipping file due to syntax error: #{e.inline}", path / (e.filename? || ""))
        next
      end

      {path: path, in: in_doc, out: out_blob, hit: hit_doc}
    end

    test_cases.flat_map do |test_case|
      Term.case(test_case[:in]) do
        matchpi %{{¦ ±width ±height backdrop_ microfold⋮ false}} do
          test = SceneryTest.new(
            path: test_case[:path],
            in: test_case[:in],
            out_ppm: test_case[:out],
            hit: test_case[:hit],
            width: width.to(Magnitude),
            height: height.to(Magnitude),
            backdrop: Pigment.rgba(backdrop),
            microfold: microfold.to(Bool),
          )

          assertions(test).map { |asn| Assertion.new(asn, topic) }
        end

        otherwise do
          warn("Invalid in.wwml, expected {¦ ±width ±height backdrop_}", test_case[:path])

          [] of Assertion(SceneryGroup)
        end
      end
    end
  end

  defrecord Failure, detail : String

  def assertions(production : Failure) : Array(AssertionNode)
    asn = ->(assets : AssertionAssets) do
      AssertionResult.new(Mmt.zero, [complaint(production.detail)])
    end

    [asn] of AssertionNode
  end

  defrecord Top, path : Path, term : Term

  def assertions(production : Top, srcmap : ML::SrcMap) : Array(AssertionNode)
    top = production.term

    Term.case(top, engine: M0) do
      # |@ testtool.decl.group
      #
      # |@pattern
      # (group children_*)
      #
      # |@key children testtool.decl
      #
      # |@block
      # Use `group` to group zero or more tests together.
      matchpi %{(group _*)} do
        children = top.items.move(1)
        children.flat_map_with_index(offset: 1) do |item, index|
          assertions(Top.new(production.path, item), srcmap.cd(index))
        end
      end

      # Everything else is a decl.
      otherwise do
        annotated(assertions(Decl.new(production.path, top), srcmap), top, srcmap)
      end
    end
  end

  defrecord Decl, path : Path, term : Term

  def assertions(production : Decl, srcmap : ML::SrcMap) : Array(AssertionNode)
    decl = production.term

    Term.case(decl, engine: M0) do
      # |@ testtool.decl.alloy
      #
      # |@pattern
      # (alloy vars_dict template_ expansion_ ¦ issues⋮ ())
      #
      # |@key vars
      # Variables to instantiate the template with.
      #
      # |@key alloy.template
      # Alloy template to instantiate.
      #
      # |@key expansion
      # Expected expansion of the template.
      #
      # |@key issues
      # Lists expected issues (strings). Issues not in this list (unexpected issues)
      # will cause your test to fail.
      #
      # |@block
      # Use `alloy` to test Alloy template expansion (Crystal-side `Alloy.render`).
      matchpiT %{[alloy vars_dict template_ expansion_]} do
        M0.schema(decl) do |s|
          s.on_mismatch { continue }

          issues = s.key(:issues, value: Term::Dict, default: Term[])
          test = AlloyTest.new(vars, template, expansion, issues)
          annotated(assertions(test), decl, srcmap)
        end
      end

      # |@ testtool.decl.µfold, testtool.decl.microfold
      #
      # |@pattern
      # (⸨µ,micro⸩fold variants_+ ¦ problems⋮ ())
      #
      # |@key variants
      # Variants whose equality should be checked.
      #
      # |@key problems
      # Expected issues (strings). Applies to all of *variants*.
      #
      # |@block
      # Use `microfold`/`µfold` to check for equality across one or more
      # Microfold variants, possibly with issues.
      matchpi %{[microfold _*]}, %{[µfold _*]} do
        M0.schema(decl) do |s|
          s.on_mismatch { continue }

          problems = s.key(:problems, value: Term::Dict, default: Term[])
          variants = decl.items.move(1)
          continue if variants.empty?

          test = MicrofoldTest.new(variants.to_a, problems)
          annotated(assertions(test), decl, srcmap)
        end
      end

      matchpi %{[µfold= _*]} do
        variants = decl.items.move(1)
        continue if variants.empty?

        test = Microfold2Test.new(variants.to_a)
        annotated(assertions(test), decl, srcmap)
      end
      # |@ testtool.decl.ml
      #
      # |@pattern
      # (ml children_*)
      #
      # |@key children testtool.ml
      #
      # |@block
      # Use `ml` to introduce zero or more WwML tests.
      matchpi %{(ml _*)} do
        assertions(decl.as_d, srcmap, offset: 1) do |item|
          MLdecl.new(production.path, item)
        end
      end

      # |@ testtool.decl.backmap
      #
      # |@pattern
      # (backmap (pattern pattern_ backspec_) children_*)
      #
      # |@key pattern m1.pattern
      # |@key backspec m1.backspec
      # |@key children testtool.pattern
      #
      # |@block
      # Use `backmap` to introduce zero or more M1 backmap tests for the given
      # *pattern* and *backspec*.
      matchpi %{(backmap (pattern_ backspec_) _*)} do
        assertions(decl.as_d, srcmap, offset: 2) do |item|
          BackmapDecl.new(production.path, pattern, backspec, item)
        end
      end

      matchpi %{(backsys defn_dict _*)} do
        backsys = defn.items.compact_map do |rule|
          Term.matchpi?(rule, %{[backmap pattern_ backspec_dict]}, engine: M0) do
            {pattern, backspec}
          end
        end

        assertions(decl.as_d, srcmap, offset: 2) do |item|
          BacksysDecl.new(production.path, backsys, item)
        end
      end

      # |@ testtool.decl.pattern
      #
      # |@pattern
      # (pattern pattern_ children_*)
      #
      # |@key pattern m1.pattern
      # |@key children testtool.backmap
      #
      # |@block
      # Use `pattern` to introduce zero or more M1 pattern tests for
      # the given *pattern*.
      matchpi %{(pattern pattern_ _*)} do
        assertions(decl.as_d, srcmap, offset: 2) do |item|
          PatternDecl.new(production.path, pattern, item)
        end
      end

      # |@ testtool.decl.head
      #
      # |@pattern
      # (head children_*)
      #
      # |@key children testtool.head
      #
      # |@block
      # Use `head` to introduce zero or more M1 pattern head tests (Crystal-
      # side `M1.head?`).
      matchpi %{(head _*)} do
        assertions(decl.as_d, srcmap, offset: 1) do |item|
          HeadDecl.new(production.path, item)
        end
      end

      # |@ testtool.decl.bounds
      #
      # |@pattern
      # (bounds children_*)
      #
      # |@key children testtool.bounds
      #
      # |@block
      # Use `bounds` to introduce zero or more M1 pattern bounds tests (Crystal-
      # side `M1.bounds`).
      matchpi %{(bounds _*)} do
        assertions(decl.as_d, srcmap, offset: 1) do |item|
          BoundsDecl.new(production.path, item)
        end
      end

      # |@ testtool.decl.depth
      #
      # |@pattern
      # (depth children_*)
      #
      # |@key children testtool.depth
      #
      # |@block
      # Use `depth` to introduce zero or more M1 pattern depth tests (Crystal-
      # side `M1.depth`)
      matchpi %{(depth _*)} do
        assertions(decl.as_d, srcmap, offset: 1) do |item|
          DepthDecl.new(production.path, item)
        end
      end

      # |@ testtool.decl.specificity
      #
      # |@pattern
      # (specificity levels_*)
      #
      # |@key levels
      # - Each level is of the form `(level items_*)`.
      # - Each *item* is an M1 pattern (`m1.pattern`).
      # - All *items* of a level are asserted to have equal specificity.
      # - Later levels are asserted to have higher specificity (they are "tighter")
      #   than those above (they are "looser").
      #
      # |@block
      # Use `specificity` to introduce an M1 specificity test (Crystal-side `M1.specificity`).
      matchpi %{(specificity _*)} do
        levels = decl.items.move(1)

        valid = levels.compact_map do |level|
          Term.matchpi?(level, %{(level _*)}, engine: M0) do
            members = level.items.move(1)
            members.to_set
          end
        end

        continue unless levels.size == valid.size # All valid

        test = SpecificityTest.new(valid)
        annotated(assertions(test), decl, srcmap)
      end

      # |@ testtool.decl.captures
      #
      # |@pattern
      # (captures children_*)
      #
      # |@key children testtool.captures
      #
      # |@block
      # Use `captures` to introduce zero or more M1 pattern capture tests (Crystal-
      # side `M1.captures`)
      matchpi %{(captures _*)} do
        assertions(decl.as_d, srcmap, offset: 1) do |item|
          CapturesDecl.new(production.path, item)
        end
      end

      # |@ testtool.decl.rack
      #
      # |@pattern
      # (rack frames_*)
      #
      # |@key frames rack.circuit
      # The expected time-sequence of circuits. The first circuit acts as
      # a "seed". The time-sequence may end with `end` to make sure rewriting
      # terminates.
      #
      # |@block
      # Use `rack` to introduce a Rack time-sequence test. This also tests D7,
      # which is used to implement Rack.
      matchpi %{(rack (frame _*) _*)} do
        seed = Term.of(decl[1].items.move(1))
        frames = decl.items.move(2)

        test = RackTest.new(seed, frames.to_a)
        annotated(assertions(test), decl, srcmap)
      end

      # |@ testtool.decl.rack/instantiate
      #
      # |@pattern
      # (rack (seed seed_*) (frame frame_*))
      #
      # |@key seed rack.circuit
      # Circuit before instantiation takes place. It must include the rules
      # for instantiation.
      #
      # |@key frame rack.circuit
      # Circuit after instantiation. For convenience, we omit rules from
      # the instance. This means you don't need to copy rules from *seed*
      # into *frame*.
      #
      # |@block
      # Use `rack/instantiate` to introduce a Rack component instantiation test.
      matchpi %{(rack/instantiate (seed _*) (frame _*))} do
        seed = Term.of(decl[1].items.move(1))
        instance = Term.of(decl[2].items.move(1))

        test = RackInstantiateTest.new(seed, instance)
        annotated(assertions(test), decl, srcmap)
      end

      # |@ testtool.decl.edit
      #
      # |@pattern
      # (edit seed_ motions←(_*) result_)
      #
      # |@key motions editR.motion
      #
      # |@block
      # Use `edit` to introduce an editR test. All motions listed in *motions*
      # are dispatched to *seed*, in sequence (not in bulk). The result is compared
      # to *result* to determine whether the test passes.
      matchpi %{(edit seed_ motions_dict result_)} do
        continue unless motions.itemsonly?

        test = EditTest.new(seed, motions.items.to_a, result)
        annotated(assertions(test), decl, srcmap)
      end

      otherwise do
        warn("Ignoring unrecognized decl: #{decl}", production.path, srcmap)

        [] of AssertionNode
      end
    end
  end

  def assertions(decl : Term::Dict, srcmap : ML::SrcMap, *, offset : Int, &) : Array(AssertionNode)
    children = decl.items.move(offset)
    children.flat_map_with_index(offset: offset) do |item, index|
      itemsrc = srcmap.cd(index)
      test = yield item

      annotated(assertions(test, itemsrc), Term.of(decl), srcmap)
    end
  end

  defrecord MLdecl, path : Path, term : Term

  def assertions(production : MLdecl, srcmap : ML::SrcMap) : Array(AssertionNode)
    decl = production.term

    {% begin %}
      Term.case(decl, engine: M0) do
        {% for row in { {"", :term}, {"doc", :document} } %}
          {% prefix, entity = row %}

          # |@ testtool.ml.=
          #
          # |@pattern
          # (= sources_string+)
          # (doc= sources_string+)
          #
          # |@block
          # Use `=` or `doc=` to check for term or document source equivalence,
          # correspondingly.
          matchpi %{({{prefix.id}}= _*)} do
            continue unless decl.itemsize >= 2

            rest = decl.items.move(1)
            continue unless rest.all?(&.type.string?)

            sources = rest.map(&.to(String))
            test = MLeq.new(sources, entity: {{entity}})

            annotated(assertions(test), decl, srcmap)
          end

          # |@ testtool.ml.+
          #
          # |@pattern
          # (+ sources_string+ pattern_)
          # (doc+ sources_string+ pattern_)
          #
          # |@key pattern m1.pattern
          #
          # |@block
          # Use `+` or `doc+` to make sure all of *sources* parsed into term or
          # document, correspondingly, match the given *pattern*.
          matchpi %{({{prefix.id}}+ _*)} do
            continue unless decl.itemsize >= 3

            rest = decl.items.move(1).grow(-1)
            continue unless rest.all?(&.type.string?)

            sources = rest.map(&.to(String))
            pattern = decl.items.last
            test = MLpos.new(sources, pattern, entity: {{entity}})

            annotated(assertions(test), decl, srcmap)
          end

          # |@ testtool.ml.-
          #
          # |@pattern
          # (- source_string detail_string)
          # (doc- source_string detail_string)
          #
          # |@key source
          # A source string, including error location or range highlighted using
          # one or two `⏏`s, correspondingly. For example, one could write `"hello⏏\q⏏"`
          # for invalid escape sequence or `[(1 2 3⏏]` for missing closing paren.
          #
          # |@key detail
          # Syntax error detail *substring*, i.e., you don't have to spell out
          # the whole message.
          #
          # |@block
          # Use `-` or `doc-` to make sure *source* fails to parse, producing
          # a syntax error including the given *detail*.
          matchpi %{({{prefix.id}}- source_string detail_string)}, source: String, detail: String do
            test = MLneg.new(source, detail, entity: {{entity}})

            annotated(assertions(test), decl, srcmap)
          end
        {% end %}

        otherwise do
          warn("Ignoring unrecognized ML decl: #{decl}", production.path, srcmap)

          [] of AssertionNode
        end
      end
    {% end %}
  end

  defrecord PatternDecl, path : Path, pattern : Term, term : Term

  def assertions(production : PatternDecl, srcmap : ML::SrcMap) : Array(AssertionNode)
    pattern, decl = production.pattern, production.term

    Term.case(decl, engine: M0) do
      # |@ testtool.pattern.resource=
      #
      # |@pattern
      # (resource= resource_ envs_*)
      #
      # |@key resource resource
      # Query to retrieve the resource.
      #
      # |@key envs
      # Zero or more expected match envs.
      #
      # |@block
      # Resource match test.
      #
      # Supported resource media types are:
      # - `application/json`
      matchpi %{(resource= resource_ _*)} do
        unless query = ResourceService.query?(resource)
          return annotated(assertions(Failure.new("invalid resource query #{resource}")), decl, srcmap)
        end

        begin
          blob = ResourceService.read_blob(query)
        rescue e : ResourceService::Error
          return annotated(assertions(Failure.new("could not load #{resource}: #{e.message}")), decl, srcmap)
        end

        case type = blob.classif.media_type
        when Term["application/json"]
          begin
            matchee = Term.of(JSON.parse(blob.to_string))
          rescue e : JSON::ParseException
            return annotated(assertions(Failure.new("invalid JSON: #{e.message}")), decl, srcmap)
          end

          matches = decl.items.move(2)
          test = PatternEq.new(pattern, matchee, matches.to_set)
          annotated(assertions(test), decl, srcmap)
        else
          annotated(assertions(Failure.new("unsupported resource media type #{type}")), decl, srcmap)
        end
      end

      # |@ testtool.pattern.=
      #
      # |@pattern
      # (= matchee_ envs_*)
      #
      # |@key name
      # Name of the variable.
      #
      # |@key envs
      # Zero or more expected match envs.
      #
      # |@block
      # Use `=` to assert that matching the current pattern produces zero
      # or more listed match envs.
      matchpi %{(= matchee_ _*)} do
        matches = decl.items.move(2)
        test = PatternEq.new(pattern, matchee, matches.to_set)

        annotated(assertions(test), decl, srcmap)
      end

      # |@ testtool.pattern.+
      #
      # |@pattern
      # (+ whitelist_*)
      #
      # |@block
      # Use `+` to assert that all matchees in *whitelist* produce one or more
      # match envs.
      matchpi %{(+ _*)} do
        whitelist = decl.items.move(1)
        test = PatternPos.new(pattern, whitelist.to_set)

        annotated(assertions(test), decl, srcmap)
      end

      # |@ testtool.pattern.-
      #
      # |@pattern
      # (- blacklist_*)
      #
      # |@block
      # Use `-` to assert that all matchees in *blacklist* produce zero match envs.
      matchpi %{(- _*)} do
        blacklist = decl.items.move(1)
        test = PatternNeg.new(pattern, blacklist.to_set)

        annotated(assertions(test), decl, srcmap)
      end

      otherwise do
        warn("Ignoring unrecognized pattern decl: #{decl}", production.path, srcmap)

        [] of AssertionNode
      end
    end
  end

  defrecord BackmapDecl, path : Path, pattern : Term, backspec : Term, term : Term

  def assertions(production : BackmapDecl, srcmap : ML::SrcMap) : Array(AssertionNode)
    decl, pattern, backspec = production.term, production.pattern, production.backspec

    Term.case(decl, engine: M0) do
      # |@ testtool.backmap.=
      #
      # |@pattern
      # (= matchee_ whitelist_*)
      #
      # |@block
      # Use `=` to assert that *matchee* matches the current pattern, and that
      # applying the current backspec to it results in one of terms from *whitelist*.
      matchpi %{(= matchee_ _*)} do
        whitelist = decl.items.move(2)
        test = BackmapEq.new(pattern, backspec, matchee, whitelist.to_set)

        annotated(assertions(test), decl, srcmap)
      end

      # |@ testtool.backmap.-
      #
      # |@pattern
      # (- blacklist_*)
      #
      # |@block
      # Use `-` to assert that backmapping is noop for all terms in *blacklist*<
      # given the current pattern and backspec.
      matchpi %{(- _*)} do
        blacklist = decl.items.move(1)
        test = BackmapNeg.new(pattern, backspec, blacklist.to_set)

        annotated(assertions(test), decl, srcmap)
      end

      otherwise do
        warn("Ignoring unrecognized backmap decl: #{decl}", production.path, srcmap)

        [] of AssertionNode
      end
    end
  end

  defrecord BacksysDecl, path : Path, backsys : Array({Term, Term}), term : Term

  def assertions(production : BacksysDecl, srcmap : ML::SrcMap) : Array(AssertionNode)
    decl, backsys = production.term, production.backsys

    Term.case(decl, engine: M0) do
      # |@ testtool.backsys.seq
      #
      # |@pattern
      # (seq frames_*)
      #
      # |@block
      # Use `seq` to assert that a sequence of *frames* is a valid evolution under
      # the current backsystem.
      matchpi %{(seq _*)} do
        seq = decl.items.move(1).to_a
        test = BacksysTest.new(backsys, seq)

        annotated(assertions(test), decl, srcmap)
      end

      otherwise do
        warn("Ignoring unrecognized backsys decl: #{decl}", production.path, srcmap)

        [] of AssertionNode
      end
    end
  end

  defrecord HeadDecl, path : Path, term : Term

  def assertions(production : HeadDecl, srcmap : ML::SrcMap) : Array(AssertionNode)
    decl = production.term

    Term.case(decl, engine: M0) do
      # |@ testtool.head.of
      #
      # |@pattern
      # (of pattern_ head_)
      #
      # |@key pattern m1.pattern
      #
      # |@block
      # Use `of` to assert that the head of the given *pattern* is *head*.
      matchpi %{(of pattern_ head_)} do
        test = HeadEq.new(pattern, head)

        annotated(assertions(test), decl, srcmap)
      end

      # |@ testtool.head.-
      #
      # |@pattern
      # (- patterns_*)
      #
      # |@key patterns m1.pattern
      #
      # |@block
      # Use `-` to assert that none of *patterns* have a head.
      matchpi %{(- _*)} do
        blacklist = decl.items.move(1)
        test = HeadAbsent.new(blacklist.to_set)

        annotated(assertions(test), decl, srcmap)
      end

      otherwise do
        warn("Ignoring unrecognized head decl: #{decl}", production.path, srcmap)

        [] of AssertionNode
      end
    end
  end

  defrecord BoundsDecl, path : Path, term : Term

  def assertions(production : BoundsDecl, srcmap : ML::SrcMap) : Array(AssertionNode)
    decl = production.term

    Term.case(decl, engine: M0) do
      # |@ testtool.bounds.of
      #
      # |@pattern
      # (of patterns_* ¦ bounds_dict)
      #
      # |@key patterns m1.pattern
      #
      # |@key bounds
      # A dictionary matching one of:
      # - `{}`, meaning bounds could not be determined.
      # - `{min: (%number +i32)}`, meaning only the lower bound is known.
      # - `{max: (%number +i32)}`, meaning only the higher bound is known.
      # - `{min: (%number +i32), max: (%number +i32)}`, meaning both bounds are known.
      #
      # |@block
      # Use `of` to assert that all of *patterns* have the given *bounds*.
      matchpiT %{(of _* ¦ bounds_dict)} do
        patterns = decl.items.move(1)
        test = BoundsEq.new(patterns.to_set, bounds)

        annotated(assertions(test), decl, srcmap)
      end

      otherwise do
        warn("Ignoring unrecognized bounds decl: #{decl}", production.path, srcmap)

        [] of AssertionNode
      end
    end
  end

  defrecord DepthDecl, path : Path, term : Term

  def assertions(production : DepthDecl, srcmap : ML::SrcMap) : Array(AssertionNode)
    decl = production.term

    Term.case(decl, engine: M0) do
      # |@ testtool.depth.of
      #
      # |@pattern
      # (of patterns_* ¦ depth_dict)
      #
      # |@key patterns m1.pattern
      #
      # |@key depth
      # A dictionary matching one of:
      # - `{}`, meaning depth could not be determined.
      # - `{min: (%number +i32)}`, meaning only the lower bound is known.
      # - `{max: (%number +i32)}`, meaning only the higher bound is known.
      # - `{min: (%number +i32), max: (%number +i32)}`, meaning both bounds are known.
      #
      # |@block
      # Use `of` to assert that all of *patterns* have the given *depth*.
      matchpiT %{(of _* ¦ depth_dict)} do
        patterns = decl.items.move(1)
        test = DepthEq.new(patterns.to_set, depth)

        annotated(assertions(test), decl, srcmap)
      end

      otherwise do
        warn("Ignoring unrecognized depth decl: #{decl}", production.path, srcmap)

        [] of AssertionNode
      end
    end
  end

  defrecord CapturesDecl, path : Path, term : Term

  def assertions(production : CapturesDecl, srcmap : ML::SrcMap) : Array(AssertionNode)
    decl = production.term

    Term.case(decl, engine: M0) do
      # |@ testtool.captures.of
      #
      # |@pattern
      # (of pattern_ (captures_*))
      #
      # |@key pattern m1.pattern
      #
      # |@key captures
      # A list of captures that *pattern* is expected to make.
      #
      # Each capture is of the form `(name_ tags-pattern_)`.
      #
      # |@block
      # Use `of` to assert that *pattern* makes the given captures.
      matchpi %{(of pattern_ captures←(_*))} do
        valid = [] of {Term, Term}

        captures.items.each do |capture|
          Term.case(capture, engine: M0) do
            matchpi %{(name_ tagsp_)} { valid << {name, tagsp} }
            otherwise { }
          end
        end

        continue unless valid.size == captures.size

        test = CapturesEq.new(pattern, valid)

        annotated(assertions(test), decl, srcmap)
      end

      otherwise do
        warn("Ignoring unrecognized captures decl: #{decl}", production.path, srcmap)

        [] of AssertionNode
      end
    end
  end

  # Bridges between assertions() and run() overloads.
  def assertions(leaf : Leaf) : Array(AssertionNode)
    asn = ->(assets : AssertionAssets) do
      mmts = Mmt.zero
      complaints = [] of Complaint

      stat = ->(mmt : Mmt) do
        mmts += mmt
        mmts
      end

      run(leaf, assets, stat, complaints)

      AssertionResult.new(mmts, complaints)
    end

    [asn] of AssertionNode
  end
end
