module Testtool
  alias ArgParse = ArgConf | ArgErr | ArgHelp

  defcase ArgConf,
    index_path : Path,
    focused : Set(Term),
    ignored : Set(Term),
    stats_path : Path?,
    interactive : Bool,
    display_assertion : Bool,
    assets : Bool

  class ArgConf
    def tests_path
      index_path.parent
    end
  end

  defrecord ArgErr, detail : String
  defrecord ArgHelp

  HELP = <<-'HELP'
  SYNOPSIS

  `testtool` lets you run the Wirewright tests.

  USAGE

    testtool [OPTIONS]

  OPTIONS

    +<tag>
      Focus topics tagged with <tag>.

    -<tag>
      Ignore topics tagged with <tag>.

    --index /path/to/index.wwml
      Changes the path to tests index.
      Default: tests/index.wwml.

    --stats
      Writes statistics (CPU and memory usage for each assertion)
      to /tmp/ww-testtool.stats.csv.

    --stats /path/to/stats.csv
      Writes statistics (CPU and memory usage for each assertion)
      to the provided path.

    --interactive, -i
      Goes through failures (if any) one-by-one instead of printing
      them all at once.

    --help, -h
      Prints this message.

    --assertion-visibility-none, -A
      Prevents the testtool from showing full assertions for each complaint. This can
      be useful if you have *a lot* of complaints, so many it's hard to scroll. Usually
      the line number is all you need.

    --assets-none
      Prevents the testtool from loading heavy assets on startup. This improves DX if
      you're working on a particular set of tests, unrelated to the heavy ones; loading
      their associated resources takes time (especially in debug builds of Wirewright).

  EXAMPLE

    $ testtool --stats stats.csv -ufold -long
    # Runs all tests except ufold and long(-running ones).
    # Writes statistics to stats.csv.
  HELP

  # Parses testtool command-line arguments provided in *argv*.
  def argparse(argv : Array(String)) : ArgParse
    index_path = Path["tests/index.wwml"]
    stats_path = nil
    focused = Set(Term).new
    ignored = Set(Term).new
    interactive = false
    display_assertion = true
    assets = true

    cursor = 0
    while cursor < argv.size
      arg = argv[cursor]
      cursor += 1

      if arg.in?("--help", "-h")
        return ArgHelp.new
      end

      if arg == "--index"
        unless successor = argv[cursor]?
          return ArgErr.new("expected a path after --index")
        end
        cursor += 1
        index_path = Path[successor]
        next
      end

      if arg == "--stats"
        if (successor = argv[cursor]?).nil? || successor.starts_with?('-')
          # nil, -, or --, doesn't matter, it's not for us, but --stats is there.
          stats_path = Path[Dir.tempdir] / Path["ww-testtool.stats.csv"]
          next
        end
        cursor += 1
        stats_path = Path[successor]
        next
      end

      if arg.in?("-i", "--interactive")
        interactive = true
        next
      end

      if arg.in?("-A", "--assertion-visibility-none")
        display_assertion = false
        next
      end

      if arg.in?("-A", "--assets-none")
        assets = false
        next
      end

      if name = arg.lchop?('+')
        focused << Term.of(Term::Sym.new(name))
        next
      end

      if name = arg.lchop?('-')
        ignored << Term.of(Term::Sym.new(name))
        next
      end
    end

    ArgConf.new(index_path, focused, ignored, stats_path, interactive, display_assertion, assets)
  end

  # Constructs a Microfold theme based on definitions from *index*, if any.
  def theme?(index : Term::Dict, base : Path) : Microfold::Theme?
    theme_path = index[:microfold, :theme]?.try(&.to?(Path))
    theme_rem = index[:microfold, :rem]?.try(&.as_n?)
    return unless theme_path && theme_rem

    log("Loading Microfold theme #{theme_path}, rem: #{theme_rem}")

    begin
      pipe(
        base / theme_path,
        ResourceService.file,
        ResourceService.read_string,
        ML.document,
        Microfold.theme(rem: theme_rem),
      )
    rescue e : ResourceService::Error
      err(e.message || "???")
    rescue e : ML::SyntaxError
      err("Syntax error in #{(base / theme_path).normalize}")
      dump(e.humanize)
    end
  end

  # Constructs an editR rewriter based on definitions from *index*, if any.
  def editR?(index : Term::Dict, base : Path) : Rewriter?
    return unless path = index[:editR, :codex]?.try(&.to?(Path))

    log("Loading editR codex at #{(base / path).normalize}")

    codex = pipe(base / path,
      ResourceService.file,
      ResourceService.read_string,
      ML.document,
    )

    Soma.editR(codex)
  end

  # Constructs a (graphics) uiR rewriter based on definitions from *index*, if any.
  def uiR?(index : Term::Dict, dw : Channel(DwUIR::Request), base : Path) : Rewriter?
    return unless path = index[:uiR, :codex]?.try(&.to?(Path))

    log("Loading uiR codex at #{(base / path).normalize}")

    ruleset = pipe(base / path,
      ResourceService.file,
      ResourceService.read_string,
      ML.document,
      Ruleset.select,
    )

    metricsR = callR do |term|
      reply = Sync::Future(Term).new
      dw.send(DwUIR::GraphicsReplyRequest.new(term, reply))
      Rewrite.one(reply.get)
    end

    Soma.uiR(metricsR, ruleset)
  end

  # Constructs `AssertionAssets` based on *conf* and contents of the index
  # file, *index*. May not run the block in case of an error.
  def assets(conf : ArgConf, index : Term::Dict, & : AssertionAssets ->) : Nil
    server = HTTP::Server.new([HTTP::StaticFileHandler.new((conf.tests_path / "public").to_s, fallthrough: false, directory_listing: false)])

    server_ctx = Fiber::ExecutionContext::Isolated.new("testtool public/ server") do
      log("HTTP server for #{conf.tests_path / "public"} started on http://127.0.0.1:9812")
      server.bind_tcp "127.0.0.1", 9812
      server.listen
      log("HTTP server for #{conf.tests_path / "public"} is down")
    end

    begin
      dw_platform = DwUIR::PvgPlatform.new
      dw_compositor = DwUIR::Compositor.new
      dw_ctx = DwUIR::Viewer::Context.new(dw_compositor, dw_platform)

      log("Starting DwUIR server")

      DwUIR.serve(dw_ctx) do |dw|
        log("DwUIR server running")

        if conf.assets
          unless theme = theme?(index, base: conf.tests_path)
            err("Microfold theme path and rem not recognized or undefined, aborting")
            return
          end

          unless editR = editR?(index, base: conf.tests_path)
            err("editR codex not recognized or undefined, aborting")
            return
          end

          unless uiR = uiR?(index, dw, base: conf.tests_path)
            err("uiR codex not recognized or undefined, aborting")
            return
          end
        end

        yield AssertionAssets.new(theme, editR, uiR, dw)
      end
    ensure
      server.close
      server_ctx.wait
    end
  end

  # Returns `true` if one of *tags* is enabled according to *conf*.
  def enabled?(conf : ArgConf, tags : Term::Dict) : Bool
    return false if tags.items.any?(&.in?(conf.ignored))

    conf.focused.empty? || tags.items.any?(&.in?(conf.focused))
  end

  # **Entrypoint of testtool.** Returns when the testtool finishes.
  def main(argv : Array(String)) : Nil
    ctx = Fiber::ExecutionContext::Isolated.new("Testtool", spawn_context: MT) do
      main(argparse(argv))

      # Let other fibers (esp. the logging fiber) finish before we exit. I'm
      # not sure if there's a better way to do this.
      sleep 1.second
    end

    ctx.wait
  end

  def main(conf : ArgConf) : Nil
    banner

    log("Reading #{conf.index_path}")

    begin
      index, indexsrc = pipe(conf.index_path,
        ResourceService.file,
        ResourceService.read_string,
        ML.document_and_srcmap(filename: conf.index_path.to_s),
      )

      index = index.as_d
    rescue e : File::Error
      err(e.message || "???")
      return
    rescue e : ML::SyntaxError
      err("Syntax error in #{conf.index_path}")
      dump(e.humanize)
      return
    end

    outline = outline(conf, index, indexsrc)

    assets(conf, index) do |assets|
      main(assets, conf, outline)
    end
  end

  def main(argp : ArgErr) : Nil
    err(argp.detail)
  end

  def main(argp : ArgHelp) : Nil
    help
  end

  def main(assets : AssertionAssets, conf : ArgConf, outline : Array(Topic)) : Nil
    log("Looking for assertions")
    tests, comparisons = assertions(outline)
    log("Found #{tests.size + comparisons.size} assertion(s): #{tests.size} test(s), #{comparisons.size} comparison(s)")

    successes = failures = 0

    log("Running #{tests.size} test(s)")
    test_results = tests.map do |asn|
      result = call(assets, asn)

      if result.complaints.empty?
        successes += 1
        display(TestSuccessPixel.new(asn.topic.color))
      else
        failures += 1
        display(TestFailurePixel.new)
      end

      result
    end

    hr

    log("Running #{comparisons.size} comparison(s)")
    comparison_results = comparisons.map do |asn|
      log("Running comparison: #{asn.topic.title}")

      result = call(assets, asn)

      if result.complaints.empty?
        successes += 1
        log("Success: #{asn.topic.title}")
      else
        failures += 1
        warn("Failure: #{asn.topic.title}")
      end

      result
    end
    hr

    if stats_path = conf.stats_path
      wstat(stats_path, tests, test_results)
    end

    mmt = test_results.sum(&.mmt) + comparison_results.sum(&.mmt)

    if conf.interactive
      display(Status.new(successes, failures, mmt))
      hr
    end

    tests.zip(test_results) do |asn, result|
      next if result.complaints.empty?

      ref = location(asn)

      display(ComplaintRef.new(ref))

      if conf.display_assertion && (term = term?(asn))
        display(AssertionReportHeader.new(term))
      end

      display(ComplaintList.new(result.complaints))
      hr

      gets if conf.interactive
    end

    comparisons.zip(comparison_results) do |asn, result|
      next if result.complaints.empty?

      ref = location(asn)

      display(ComplaintRef.new(ref))

      if conf.display_assertion
        term = term(asn)
        display(ComparisonReportHeader.new(term))
      end

      display(ComplaintList.new(result.complaints))
      hr

      gets if conf.interactive
    end

    unless conf.interactive
      display(Status.new(successes, failures, mmt))
      hr
    end
  end

  def outline(conf : ArgConf, index : Term::Dict, indexsrc : ML::SrcMap) : Array(Topic)
    outline = [] of Topic

    index.items.each_with_index do |item, index|
      _, line, col = ML::SyntaxError.lookaround(indexsrc[Tpath.value(index)])
      ref = "#{conf.index_path}:#{line}:#{col}"

      Term.case(item, engine: M0) do
        matchpi %{[test path_string]}, path: Path do |path|
          M0.schema(item) do |s|
            color = s.key(:color, value: Term, default: Term.of(:white))
            tags = s.key(:tags, value: Term::Dict, default: Term[])
            next unless enabled?(conf, tags)

            path = path.expand(conf.tests_path, expand_base: false)

            log("Reading #{path}")

            begin
              document, documentsrc = pipe(path,
                ResourceService.file,
                ResourceService.read_string,
                ML.document_and_srcmap(filename: path.to_s),
              )
            rescue e : ResourceService::Error
              warn(e.message || "???")
              next
            rescue e : ML::SyntaxError
              warn("Syntax error in #{path}")
              dump(e.humanize)
              next
            end

            rgba = Pigment.rgba(color, fallback: Pigment.named("white"))

            outline << Test.new(path, document, documentsrc, rgba, ref)
          end
        end

        matchpi %{[comparison title_string a_ b_]}, title: String do
          M0.schema(item) do |s|
            color = s.key(:color, value: Term, default: Term.of(:white))
            tags = s.key(:tags, value: Term::Dict, default: Term[])
            next unless enabled?(conf, tags)

            begin
              l = comparand(conf.tests_path, a)
              r = comparand(conf.tests_path, b)
            rescue e : ResourceService::Error
              warn("Invalid comparison: #{e.message || "???"}")
            rescue e : ArgumentError
              warn("Invalid comparison: #{e.message}")
            end

            unless op = comparison?(l, r)
              warn("No comparison exists between #{l.class} and #{r.class}")
              next
            end

            rgba = Pigment.rgba(color, fallback: Pigment.named("white"))

            outline << Comparison.new(title, op, rgba, ref, term: item)
          end
        end

        otherwise do
          warn("Ignoring invalid topic #{ref}")
        end
      end
    end

    outline
  end

  # Finds assertions in *outline*. Returns two arrays: one of `Test` assertions
  # and another of `Comparison` ones.
  def assertions(outline : Array(Topic))
    tests = [] of Assertion(Test)
    comparisons = [] of Assertion(Comparison)

    outline.each do |topic|
      asns = assertions(topic)
      asns.each do |asn|
        case asn
        in Assertion(Test)
          tests << asn
        in Assertion(Comparison)
          comparisons << asn
        end
      end
    end

    {tests, comparisons}
  end

  # Writes statistics for *tests* and their *results* to *path*.
  def wstat(path : Path, tests : Array(Assertion(Test)), results : Array(AssertionResult)) : Nil
    log("Writing stats CSV to #{path}")

    # Sort by measurement score.
    indexed = results.map_with_index { |result, index| {result, index} }
    indexed.sort_by! { |result, _| result.mmt }

    # Write file.
    blob = Term::Blob.build do |io|
      counter = IO::BytesizeCounter.new
      sink = IO::MultiWriter.new(io, counter)

      CSV.build(sink, quoting: CSV::Builder::Quoting::ALL) do |csv|
        csv.row "Location", "Time (seconds)", "Memory (bytes)"

        indexed.each do |result, index|
          asn = tests[index]
          mmt = result.mmt

          csv.row location(asn), mmt.ttotal, mmt.memtotal
        end
      end

      log("Writing #{counter.bytesize.humanize_bytes}")
    end

    PathService.write(path, blob)
  end
end
