require "./src/wirewright"

PEOPLE = Term.of(JSON.parse(File.read("data/people.json")))

module Component
  include Ω
  extend self

  def edge
    text("█")
  end

  def edge_md
    text("▊")
  end

  def edge_sm_top
    text("▖")
  end

  def edge_sm
    text("▌")
  end

  def edge_sm_bot
    text("▘")
  end

  def success(child)
    row(painted(edge, :success), child, gap: 1)
  end

  def fatal(child)
    painted(row(edge, child, gap: 1), :failure)
  end

  def error(child)
    row(painted(edge_md, :failure), child, gap: 1)
  end

  def snippet(snippet : String)
    lines = [] of Element

    snippet.each_line do |line|
      lines << row(painted(edge_sm, :dim), text(line), gap: 1)
    end

    lines.size == 1 ? lines[0] : col(lines)
  end

  def snippet(object : Term)
    snippet(ML.display(object, endl: false))
  end

  def snippet(object)
    snippet(object.to_s)
  end

  def section(caption, snippet)
    col(
      painted(text(caption), :emphasis),
      painted(edge_sm_top, :dim),
      snippet(snippet),
      painted(edge_sm_bot, :dim),
    )
  end

  def complaint(title : String, sections : Array({_, _}), *, bullet = "◉")
    points = sections.map do |caption, snippet|
      section(caption, snippet).as(Element)
    end

    padding(
      col(
        text("#{bullet} #{title}"),
        padding(col(points), pt: 1, pl: 2)
      ),
      pb: 1,
    )
  end

  def processing(item)
    Ω.row(
      Ω.text("Processing"),
      Ω.painted(Ω.text(ML.compact(item), wrap: false), :italic),
      gap: 1
    )
  end
end

alias MM = Meridium
alias Mf = Soma::Microfold
alias Cell = Char, Color ->
alias Color = Soma::DwUIR::Color

# Represents the contribution of a test to the harness.
defcase TestContrib,
  path : Path,
  text : StringView,
  dt : Time::Span,
  complaints : Array(Ω::Element)

# :nodoc:
record Test, group_id : UInt32, action : Cell -> TestContrib

# :nodoc:
class TestHarness
  def initialize(@preview : Bool, @styled : Bool)
    @contribs = [] of TestContrib
    @schedule = [] of Test
    @next_group_id = 0u32
  end

  # Constructs and yields a concurrent test harness.
  #
  # If *preview* is enabled, outputs progress preview using Termbox.
  #
  # Outputs the final report to STDOUT, using ANSI escape sequences
  # for colors and decoration if *styled* is true.
  #
  # Returns `true` if all tests succeeded, `false` otherwise.
  #
  # With the concurrent harness, all test groups are run concurrently,
  # in the order they were registered.
  def self.new(*, preview = true, styled = true, & : TestHarness ->) : Bool
    harness = new(preview, styled)
    yield harness

    harness.epilogue
  end

  # Constructs and yields a new group under this harness. The block
  # is expected to populate the group with tests. Returns whatever
  # the block returns.
  def group(color : Color, & : TestGroup ->)
    group = TestGroup.new(@next_group_id, self, color)
    @next_group_id += 1

    result = yield group

    @contribs.concat(group.@contribs)
    @schedule.concat(group.@schedule)

    result
  end

  # Schedules one or more global complaints using the block. These
  # complaints are not associated with any specific test group but
  # rather, with the testing process overall.
  #
  # - *path* is the path to file where the complaints are to be located.
  # - *text* points to the part of the file to be associated with the complaints.
  def complain(path : Path, text : StringView, & : Array(Ω::Element) ->) : Nil
    complaints = [] of Ω::Element
    yield complaints

    @contribs << TestContrib.new(path, text, 0.milliseconds, complaints)
  end

  # Captures all log messages.
  #
  # WARNING: all logging is nuked after this method returns.
  private def log_free(&) : Array(Log::Entry)
    level = ENV["LOG_LEVEL"]?.try { |v| Log::Severity.parse(v) } || Log::Severity::Debug
    logs = Log::AsyncInMemoryBackend.new(level)

    Log.builder.clear
    Log.builder.bind(source: "*", level: level, backend: logs)

    begin
      yield
    ensure
      Log.builder.unbind(source: "*", level: level, backend: logs)
    end

    logs.entries
  end

  PREVIEW_BG      = Termbox::Color.rgb(0x0A, 0x0A, 0x0A)
  PREVIEW_FG      = Termbox::Color.rgb(0xaa, 0xaa, 0xaa)
  PREVIEW_FG_DARK = Termbox::Color.rgb(0x33, 0x33, 0x33)

  private def preview(&)
    unless @preview
      yield nil
      return
    end

    Termbox.init do
      ncols = Termbox.width.to_i
      nrows = (@schedule.size/ncols).to_i

      Termbox.output_mode = :truecolor
      Termbox.clear(fg: PREVIEW_FG, bg: PREVIEW_BG)

      (0...@schedule.size).each do |index|
        y, x = index.divmod(ncols)
        Termbox.write('~', x: x, y: y,
          fg: PREVIEW_FG_DARK,
          bg: PREVIEW_BG
        )
      end

      yield({ncols, nrows})

      Termbox.write(
        "All tests complete.",
        x: 2,
        y: nrows + 2,
        fg: PREVIEW_FG | Termbox::Color::Bold,
        bg: PREVIEW_BG,
      )

      Termbox.present

      sleep 1.second
    end
  end

  private def set_cell(lock, x, y, chr, color)
    lock.synchronize do
      Termbox.write(chr, x: x, y: y,
        fg: Termbox::Color.rgb(color.r, color.g, color.b),
        bg: PREVIEW_BG
      )

      Termbox.present

      # If any worker senses Ctrl-C, it explodes the world with `abort`.
      next unless event = Termbox.peek?
      next unless event.type.key?
      next unless event.key.ctrl_c?

      Termbox.shutdown

      abort "Keyboard interrupt"
    end
  end

  private def complaint_list(errs)
    Ω.col(
      errs.map do |error|
        _, line, column = ML::SyntaxError.lookaround(error.text)

        Ω.col(
          Component.error(
            Ω.row(
              Ω.text("Unexpected result at "),
              Ω.text("#{error.path}:#{line}:#{column}", :link)
            )
          ),
          Ω.indent(Ω.col(error.complaints), by: 1),
          gap: 1
        ).as(Ω::Element)
      end
    )
  end

  # :nodoc:
  def epilogue : Bool
    captured_log_entries = log_free do
      preview do |dimensions|
        wg = WaitGroup.new
        lock_contrib = Sync::Mutex.new
        lock_termbox = Sync::Mutex.new

        base = 0
        grouped = @schedule.group_by(&.group_id)
        grouped.each do |_, members|
          workload = members.map_with_index do |test, offset|
            if dimensions
              # Preview is enabled.
              ncols, _ = dimensions
              y, x = (base + offset).divmod(ncols)
              cell = ->(chr : Char, color : Color) do
                set_cell(lock_termbox, x, y, chr, color)
              end
            else
              # Preview is disabled.
              cell = ->(chr : Char, color : Color) { }
            end

            {test, cell}
          end

          # Spawn a worker.
          wg.add

          MT.spawn do
            workload.each do |test, cell|
              contrib = test.action.call(cell)
              lock_contrib.synchronize do
                @contribs << contrib
              end
            end
          ensure
            wg.done
          end

          base += members.size
        end

        # If preview is disabled, show them at least something.
        if dimensions.nil?
          puts "Waiting for #{@schedule.size} tests to complete..."
        end

        wg.wait
      end
    end

    # Flush captured logs.
    captured_log_entries.each do |entry|
      Log::ShortFormat.format(entry, STDOUT)
      puts
    end

    puts

    # Collect data from contributions.
    oks = 0
    errs = [] of TestContrib
    duration = 0.milliseconds

    @contribs.each do |contrib|
      duration += contrib.dt

      if contrib.complaints.present?
        errs << contrib
      else
        oks += 1
      end
    end

    if errs.present?
      markup = Ω.col(
        complaint_list(errs),
        Ω.col(
          Component.fatal(Ω.text("Ran #{oks + errs.size} test case(s) in #{duration.humanize}.")),
          Ω.padding(Ω.text("Registered #{errs.size} unexpected result(s)."), pl: 2),
        ),
        gap: 1
      )
    else
      markup = Component.success(Ω.text("Ran #{oks} test case(s) in #{duration.humanize}."))
    end

    Ω.render(STDOUT, markup, styled: @styled)

    errs.empty?
  end
end

# An object that you can add related tests to.
class TestGroup
  # :nodoc:
  def initialize(@id : UInt32, @harness : TestHarness, @color : Color)
    @contribs = [] of TestContrib
    @schedule = [] of Test
  end

  private def schedule(path : Path, text : StringView, &fn : Cell -> {Time::Span, Array(Ω::Element)}) : Nil
    action = ->(cell : Cell) do
      begin
        dt, complaints = fn.call(cell)
      rescue e : Exception
        dt = 0.milliseconds
        complaints = [Component.complaint(
          title: "Test case crashed",
          bullet: "☠",
          sections: [
            {"TEST CASE", text},
            {"EXCEPTION", e.inspect_with_backtrace},
          ]
        )] of Ω::Element
      end

      if complaints.present?
        cell.call('X', Color.named("red"))
      else
        cell.call('·', @color)
      end

      TestContrib.new(path, text, dt, complaints)
    end

    @schedule << Test.new(@id, action)
  end

  # Schedules one or more general complaints using the block. These
  # complaints are not associated with any specific test in the group
  # but rather, with the group overall.
  #
  # - *path* is the path to file where the complaints are to be located.
  # - *text* points to the part of the file to be associated with the complaints.
  def complain(path : Path, text : StringView, & : Array(Ω::Element) ->) : Nil
    complaints = [] of Ω::Element
    yield complaints

    @contribs << TestContrib.new(path, text, 0.milliseconds, complaints)
  end

  # Schedules a short test. Short tests do not show a spinner.
  #
  # *fn* is the test workload itself. *fn* is expected to append complaints
  # to the array it is called with. Absence of complaints in this array
  # after *fn* is treated as success.
  def short(path : Path, text : StringView, &fn : Array(Ω::Element) ->) : Nil
    schedule(path, text) do
      complaints = [] of Ω::Element
      dt = Time.measure { fn.call(complaints) }

      {dt, complaints}
    end
  end

  LONG_TICK         = 33.milliseconds
  LONG_SPINNER      = "▌▀▐▄".chars
  LONG_SPINNER_TICK = 300.milliseconds

  # Schedules a long test. Long tests show a spinner while running.
  #
  # *fn* is the test workload itself. *fn* is expected to append complaints
  # to the array it is called with. Absence of complaints in this array
  # after *fn* is treated as success.
  def long(path : Path, text : StringView, &fn : Array(Ω::Element) ->) : Nil
    schedule(path, text) do |cell|
      result = Sync::Future({Time::Span, Array(Ω::Element)}).new

      MT.spawn do
        complaints = [] of Ω::Element
        dt = Time.measure { fn.call(complaints) }
        result.set({dt, complaints})
      rescue e : Exception
        result.fail(e)
      end

      spin = 0
      spins = LONG_SPINNER
      elapsed = 0.milliseconds

      cell.call(spins[spin], Color.named("gray"))
      spin += 1

      while true
        if response = result.get?
          dt, complaints = response
          break
        end

        if elapsed > LONG_SPINNER_TICK
          cell.call(spins[spin], Color.named("gray"))
          spin = (spin + 1) % spins.size
          elapsed = 0.milliseconds
        end

        sleep LONG_TICK
        elapsed += LONG_TICK # roughly; we don't care
      end

      {dt, complaints}
    end
  end
end

# Make sure that the pattern evaluates to the same match envs on all
# optimization levels. Lower opt levels expose very long rejection paths
# (and possibly bugs down there!) that would otherwise never be visited due
# to e.g. sketch opt that can reject at the very beginning of matching.
def matches_at_all_opt_levels(pattern, matchee)
  envsO2 = M1.matches(pattern, matchee, opt: M1::O2)
  envsO2.map!(&.without(:"(backpaths)"))

  {M1::O1, M1::O0}.each do |opt|
    envsOn = M1.matches(pattern, matchee, opt: opt)
    envsOn.map!(&.without(:"(backpaths)"))
    next if envsO2 == envsOn

    raise "optimization level #{opt} does not match like O2"
  end

  envsO2
end

# Runs a termspace test. Uses the block to compare seen and expected views
# (possibly with a side effect).
def tspace(flow : Term::Dict, & : Term, Term ->)
  counter = MM::Slot.new(0)
  tspace = MM::Tspace::InMemory.new

  conns = {} of Term => MM::Conn
  lslots = {} of {Term, Term} => MM::Slot
  rslots = {} of MM::Slot => Term
  views = Term[]
  views_lock = Mutex.new

  flow.items.each do |step|
    Term.matchpi?(step, %{(conn conn-name_symbol children_*)}) do
      conns.put_if_absent(conn_name) do
        viewcb = ->(view : MM::View) do
          views_lock.synchronize do
            views = views.with(conn_name, view.dict_multisets)
          end
        end

        conid = MM::WWID.new
        conn = MM::Conn.new(conid, tspace, &viewcb)
        conn.online
        conn.summon
        conn
      end

      children.each_item_unordered do |child|
        Term.matchpi?(child, %{[after added (%any sensor appearance) surface-name_symbol _]}) do
          key = {conn_name, surface_name}
          next if lslots.has_key?(key)

          slot = counter += 1

          lslots[key] = slot
          rslots[slot] = surface_name
        end
      end
    end
  end

  flow.items.each do |step|
    Term.matchpi?(step, %{(conn conn-name_symbol children_*)}) do
      conn = conns[conn_name]

      children.items.each do |child|
        Term.case(child) do
          matchpi %{[after added sensor surface-name_symbol pattern_]} do
            slot = lslots[{conn_name, surface_name}]

            conn.transaction &.put(slot, MM::Sensor.new(pattern, secret: child[:secret]?))
          end

          matchpi %{[after added appearance surface-name_symbol value_]} do
            slot = lslots[{conn_name, surface_name}]

            conn.transaction &.put(slot, MM::Appearance.new(value, secret: child[:secret]?))
          end

          matchpi %{[after removed surface-name_symbol]} do
            slot = lslots[{conn_name, surface_name}]

            conn.transaction &.delete(slot)
          end

          matchpi %{(view expected_)} do
            seeing = Term::Dict.build do |commit|
              view = views_lock.synchronize { views[conn_name]? } || Term[]
              view.each_entry do |slot, multiset|
                # Map numeric identities to original surface names (symbols).
                commit.with(rslots[slot.to(MM::Slot)], multiset)
              end
            end

            yield Term.of(seeing), expected
          end

          otherwise { }
        end
      end
    end
  end
end

defcase TestContext, theme : Mf::Theme, rack_basis : Term::Dict, uiR_base : Term

def test(ctx, test, path, keypath, stem, srcmap, text) : Bool
  Term.case(stem) do
    # Descend into (group _*)
    givenpi %[_* (group _*) node] { true }
    givenpi %[_* (group _*) ((group) children (_*))] { true }

    # ML tests
    begin
      # Descend into (ml _*)
      givenpi %[_* (ml _*) node] { true }
      givenpi %[_* (ml _*) ((ml) children (_*))] { true }

      # Handle `=` and `doc=`
      {% for variant in { {:"=", :term}, {:"doc=", :document} } %}
        {% op, method = variant %}

        givenpi %[_* (ml _*) ({{op.id}} sources_+) node] do
          test.short(path, text) do |complaints|
            subjects = sources.items.compact_map do |source|
              ML.{{method.id}}(source.to(String))
            rescue error : ML::SyntaxError
              complaints << Ω.col(
                Ω.text("◉ Syntax error in one of `{{op.id}}` subjects"),
                Ω.indent(Ω.text(error.humanize(styled: false)), by: 1),
                gap: 1
              )
              next
            end

            (1...subjects.size).each do |index|
              next if subjects[0] == subjects[index]

              complaints << Component.complaint(
                title: "Terms are not equal",
                sections: [
                  {"EXPECTED", subjects[0]},
                  {"GOT", subjects[index]},
                ],
              )
            end
          end

          false # no descend
        end
      {% end %}

      # Handle `-` and `doc-`
      {% for variant in { {:-, :term}, {:"doc-", :document} } %}
        {% op, method = variant %}

        givenpi %[_* (ml _*) ({{op.id}} source_string message_string) node] do
          input = source.to(String).delete('⏏')

          test.short(path,text) do |complaints|
            term = ML.{{method.id}}(input)

            complaints<< Component.complaint(
              title: "Reader did not fail",
              sections: [
                {"EXPECTED DETAIL LIKE", message},
                {"EXPECTED ERROR", source.to(String)},
                {"GOT TERM", term},
              ],
            )
          rescue error : ML::SyntaxError
            b = error.text.char_start
            e = error.text.char_end

            if b == e
              actual = input.insert(b, "⏏")
            else
              actual = input.insert(e, "⏏").insert(b, "⏏")
            end

            next if source.to(String) == actual && error.detail.includes?(message.to(String))

            complaints << Component.complaint(
              title: "Reader failed incorrectly",
              sections: [
                {"EXPECTED DETAIL LIKE",message },
                {"EXPECTED ERROR", source.to(String)},
                {"GOT DETAIL", error.detail},
                {"GOT ERROR", actual},
              ]
            )
          end

          false # no descend
        end
      {% end %}

      # Handle `+` and `doc+`
      {% for variant in { {:"+", :term}, {:"doc+", :document} } %}
        {% op, method = variant %}

        givenpi %[_* (ml _*) ({{op.id}} sources_+ pattern_) node] do
          test.short(path, text) do |complaints|
            subjects = sources.items.compact_map do |source|
              ML.{{method.id}}(source.to(String))
            rescue error : ML::SyntaxError
              complaints << Ω.col(
                Ω.text("◉ Syntax error in one of `{{op.id}}` subjects"),
                Ω.indent(Ω.text(error.humanize), by: 1),
                gap: 1
              )
              next
            end

            subjects.each do |subject|
              next if M1.probe?(pattern, subject)

              complaints << Component.complaint(
                title: "Term does not match pattern",
                sections: [{"PATTERN", pattern}, {"TERM", subject}],
              )
            end
          end

          false # no descend
        end
      {% end %}
    end

    # Backmap tests
    begin
      # Descend into backmap tests.
      givenpi %[_* (backmap (_ _) _*) node] { true }
      givenpi %[_* (backmap _*) ((backmap _) children (_*))] { true }

      givenpi %[_* (backmap (pattern_ backdict_) _*) (= matchee_ whitelist_*) node] do
        test.short(path, text) do |complaints|
          result = M1.backmapr(pattern, backdict, matchee).term?
          next if result.in?(whitelist.items.to_set)

          complaints << Component.complaint(
            title: "Backmapped term is not in whitelist",
            sections: [
              {"PATTERN", pattern},
              {"BACKDICT", backdict},
              {"MATCHEE", matchee},
              {"RESULT", result || "<none>"},
              {"WHITELIST", whitelist},
            ],
          )
        end

        false # no descend
      end

      givenpi %[_* (backmap (pattern_ backdict_) _*) (- blacklist_*) node] do
        test.short(path, text) do |complaints|
          blacklist.items.each do |matchee|
            next unless result = M1.backmapr(pattern, backdict, matchee).term?

            complaints << Component.complaint(
              title: "Backmapped term was found in blacklist",
              sections: [
                {"PATTERN", pattern},
                {"BACKDICT", backdict},
                {"MATCHEE", matchee},
                {"RESULT", result},
                {"BLACKLIST", blacklist},
              ],
            )
          end
        end

        false # no descend
      end
    end

    # Pattern tests
    begin
      # Descend into pattern tests.
      givenpi %[_* (pattern _ _*) node] { true }
      givenpi %[_* (pattern _*) ((pattern _) children (_*))] { true }

      givenpi %[_* (pattern pattern_ _*) (= matchee_ matches_*) node] do
        matchee0 = matchee

        if matchee == Term.of(:"<PEOPLE>")
          matchee = PEOPLE
        end

        test.short(path, text) do |complaints|
          envs = matches_at_all_opt_levels(pattern, matchee)

          next if matches.items.to_set == envs.to_set

          complaints << Component.complaint(
            title: "Pattern mismatch",
            sections: [
              {"PATTERN", pattern},
              {"MATCHEE", matchee0},
              {"EXPECTED", matches},
              {"GOT", Term.of(envs)},
            ],
          )
        end

        false # no descend
      end

      givenpi %[_* (pattern pattern_ _*) (+ whitelist_*) node] do
        test.short(path, text) do |complaints|
          whitelist.items.each do |matchee|
            envs = matches_at_all_opt_levels(pattern, matchee)
            next unless envs.empty?

            complaints << Component.complaint(
              title: "Pattern did not match",
              sections: [
                {"PATTERN", pattern},
                {"MATCHEE", matchee},
              ],
            )
          end
        end

        false # no descend
      end

      givenpi %[_* (pattern pattern_ _*) (- whitelist_*) node] do
        test.short(path, text) do |complaints|
          whitelist.items.each do |matchee|
            envs = matches_at_all_opt_levels(pattern, matchee)
            next if envs.empty?

            complaints << Component.complaint(
              title: "Pattern was not expected to match",
              sections: [
                {"PATTERN", pattern},
                {"MATCHEE", matchee},
                {"GOT", Term.of(envs)},
              ],
            )
          end
        end

        false # no descend
      end

      givenpi %[_* (pattern pattern_ _*) (⊆ matchee_ matchsets_*) node] do
        test.short(path, text) do |complaints|
          envs = matches_at_all_opt_levels(pattern, matchee)
          next if envs.to_set.in?(matchsets.items.map(&.items.to_set))

          complaints << Component.complaint(
            title: "Pattern match envs are not subsets of any the matchsets",
            sections: [
              {"PATTERN", pattern},
              {"MATCHEE", matchee},
              {"GOT", Term.of(envs)},
              {"MATCHSETS", Term.of(matchsets)},
            ],
          )
        end

        false # no descend
      end
    end

    # Pattern head tests
    begin
      # Descend into pattern head tests.
      givenpi %[_* (head _*) node] { true }
      givenpi %[_* (head _*) ((head) children (_*))] { true }

      givenpi %[_* (head _*) (- blacklist_*) node] do
        test.short(path, text) do |complaints|
          blacklist.items.each do |pattern|
            normp = M1.normal(pattern)
            next unless head = M1.head?(normp)

            complaints << Component.complaint(
              title: "Pattern has head but was not expected to",
              sections: [
                {"PATTERN", pattern},
                {"UNEXPECTED HEAD", head},
              ],
            )
          end
        end

        false # no descend
      end

      givenpi %[_* (head _*) (of pattern_ expectation_) node] do
        test.short(path, text) do |complaints|
          norm = M1.normal(pattern)
          head = M1.head?(norm)
          next if head == expectation

          complaints << Component.complaint(
            title: "Pattern head mismatch",
            sections: [
              {"PATTERN", pattern},
              {"EXPECTED HEAD", expectation},
              {"GOT HEAD", head || "<none>"},
            ],
          )
        end

        false # no descend
      end
    end

    # Pattern bounds and depth tests.
    begin
      {% for method in %w[bounds depth] %}
        # Descend
        givenpi %[_* ({{method.id}} _*) node] { true }
        givenpi %[_* ({{method.id}} _*) (({{method.id}}) children (_*))] { true }

        givenpi %[_* ({{method.id}} _*) (of patterns_+ ¦ lhs_) node] do
          test.short(path, text) do |complaints|
            patterns.items.each do |pattern|
              normp = M1.normal(pattern)
              range = M1.{{method.id}}(normp)
              rhs = Term.of(
                min: range[0] == Magnitude::INFINITY ? nil : range[0],
                max: range[1] == Magnitude::INFINITY ? nil : range[1],
              )

              next if lhs == rhs

              complaints << Component.complaint(
                title: "Pattern {{method.id}} mismatch",
                sections: [
                  {"PATTERN", pattern},
                  {"EXPECTED {{method.id.upcase}}", lhs},
                  {"GOT {{method.id.upcase}}", rhs},
                ],
              )
            end
          end

          false # no descend
        end
      {% end %}
    end

    # Pattern specificity tests
    givenpi %[_* (specificity levels_*) node] do
      test.short(path, text) do |complaints|
        spec_prev = nil

        levels.items.each do |level|
          Term.case(level) do
            matchpi %[(level members_*)] do
              spec_level = nil

              members.each_entry do |_, pattern|
                normp = M1.normal(pattern)
                spec_pattern = M1.specificity(normp, toplevel: true)
                spec_level ||= spec_pattern
                next if spec_level == spec_pattern

                complaints << Component.complaint(
                  title: "Specificity mismatch",
                  sections: [
                    {"PATTERN", pattern},
                    {"EXPECTED SPECIFICITY", Term.of(spec_level)},
                    {"GOT SPECIFICITY", Term.of(spec_pattern)},
                  ],
                )
              end

              next unless spec_level

              unless spec_prev
                spec_prev = spec_level
                next
              end

              if spec_prev < spec_level
                spec_prev = spec_level
                next
              end

              complaints << Component.complaint(
                title: "Specificity levels out of order",
                sections: [
                  {"LEVEL", level},
                  {"PREVIOUS LEVEL SPECIFICITY", spec_prev},
                  {"LEVEL SPECIFICITY", spec_level},
                ],
              )
            end
          end
        end
      end

      false # no descend
    end

    # Editor tests
    givenpi %[_* (editor initial_dict sequence_*) node] do
      memo = initial

      sequence.items.each do |edit|
        Term.case(edit) do
          matchpi %[(after (motions_+) expected_)] do
            # Refer specifically to this iteration's version of memo so that
            # the closure captures it rather than memo at the end of the loop.
            memo_ = memo

            test.short(path, text) do |complaints|
              state = memo_

              motions.items.each do |motion|
                state = edit(state, motion)
              end

              next if state == expected

              complaints << Component.complaint(
                title: "Editor state mismatch after motions",
                sections: [
                  {"STATE BEFORE MOTIONS", memo_},
                  {"MOTIONS", motions},
                  {"STATE AFTER MOTIONS", state},
                  {"EXPECTED STATE", expected},
                ],
              )
            end

            memo = expected
          end
        end
      end

      false # no descend
    end

    # D7 tests
    begin
      # The limit is set low by default to have faster failure. Tests should increase it if
      # they expect themselves to run longer for success. In an ideal world, instead of a limit,
      # we'd perhaps have some kind of "divergence" limit but hey; we're not in an ideal
      # world are we?
      givenpi %[_* (d7 initial_dict expected_dict ¦ limit: (%optional 128 limit←(%number +i32))) node] do
        test.short(path, text) do |complaints|
          ok, latest = D7.run?(initial.unsafe_as_d, cond: D7::Equal.new(expected.unsafe_as_d, nonshadow: true, limit: limit.to(Int32)))
          next if ok

          complaints << Component.complaint(
            title: "D7 runloop interrupted",
            sections: [
              {"CURRENT STATE", Term.of(D7.nonshadow(latest))},
              {"EXPECTED STATE", expected},
              {"RUNLOOP LIMIT", limit},
            ],
          )
        end

        false # no descend
      end

      givenpi %[_* (d7 initial_dict matches pattern_ ¦ limit: (%optional 128 limit←(%number +i32))) node] do
        test.short(path, text) do |complaints|
          ok, latest = D7.run?(initial.unsafe_as_d, cond: D7::Matches.new(pattern, nonshadow: true, limit: limit.to(Int32)))
          next if ok

          complaints << Component.complaint(
            title: "D7 runloop interrupted",
            sections: [
              {"CURRENT STATE", Term.of(D7.nonshadow(latest))},
              {"EXPECTED STATE TO MATCH", pattern},
              {"RUNLOOP LIMIT", limit},
            ],
          )
        end

        false # no descend
      end
    end

    # Meridium tests
    givenpi %[_* testcase←(tspace sequence_*) node] do
      test.short(path, text) do |complaints|
        tspace(sequence.unsafe_as_d) do |seeing, expected|
          next if seeing == expected

          complaints << Component.complaint(
            title: "Tspace view mismatch",
            sections: [
              {"TEST CASE", testcase},
              {"EXPECTED VIEW", expected},
              {"GOT VIEW", seeing},
            ],
          )

          # We cannot continue because further steps in sequence depend on
          # this one's correctness.
          break
        end
      end

      false # no descend
    end

    # Microfold tests
    givenpi %[_* testcase←(µfold variants_+ ⍊ problems⋮ ()) node] do
      test.short(path, text) do |complaints|
        any_failed = false

        results = variants.items.map do |variant|
          render, bts = Mf.render(ctx.theme, variant, severity: :minor)
          any_failed ||= bts.present?

          bts.each do |bt|
            dense = Term.of(Term::Sym.new(bt.severity.to_s.underscore), bt.detail)

            unless dense.in?(problems.items)
              complaints << Component.complaint(
                title: "Unexected Microfold render problem",
                sections: [
                  {"TEST CASE", testcase},
                  {"PROBLEM", dense},
                ],
              )
            end
          end

          render
        end

        if !any_failed && problems.items.present?
          complaints << Component.complaint(
            title: "Microfold render did not detect any problems",
            sections: [
              {"TEST CASE", testcase},
            ],
          )
        end

        expected = results[0]
        (1...results.size).each do |index|
          result = results[index]
          next if expected == result

          complaints << Component.complaint(
            title: "Microfold render mismatch",
            sections: [
              {"TEST CASE", testcase},
              {"REFERENCE RENDER", expected},
              {"GOT RENDER", result},
            ],
          )
        end
      end

      false # no descend
    end

    # Alloy tests
    givenpi %[_* testcase←(alloy vars_dict template_ expansion_ ¦ () issues_dict⋮ issues←(_*)) node] do
      test.short(path, text) do |complaints|
        actual, actual_issues = Alloy.render_with_issues(vars.unsafe_as_d, template)

        unless actual == expansion
          complaints << Component.complaint(
            title: "Alloy template expansion mismatch",
            sections: [
              {"VARS", vars},
              {"TEMPLATE", template},
              {"EXPECTED EXPANSION", expansion},
              {"GOT EXPANSION", actual},
            ],
          )
        end

        actual_issues.each do |actual_issue|
          next if issues.items.any? { |issue| Term.of(actual_issue.detail) == issue }

          complaints << Component.complaint(
            title: "Unexpected Alloy issue in template",
            sections: [
              {"VARS", vars},
              {"TEMPLATE", template},
              {"EXPANSION", expansion},
              {"ISSUE", Term.of(actual_issue.detail)},
            ],
          )
        end

        issues.items.each do |issue|
          next if actual_issues.any? { |actual_issue| Term.of(actual_issue.detail) == issue }

          complaints << Component.complaint(
            title: "Missing Alloy issue in template",
            sections: [
              {"VARS", vars},
              {"TEMPLATE", template},
              {"EXPANSION", expansion},
              {"ISSUE", issue},
            ],
          )
        end
      end

      false # no descend
    end

    # TODO: maybe notify of reachable-invalid test cases?

    otherwise do
      false # no descend
    end
  end
end

# Runs a generic test.
def test(ctx, test, path, content, srcmap)
  Term.each_itemspart_stem(content.as_d) do |keypath, stem|
    location = srcmap[keypath]
    test(ctx, test, path, keypath, stem, srcmap, location)
  end
end

def rack_image(ctx, rack : Term, needle : Term) : Soma::DwUIR::PixelRect
  files = Disk
  # These aren't thread safe so we cannot reuse them!
  platform = Soma::DwUIR::PvgPlatform.new(files)
  compositor = Soma::DwUIR::Compositor.new
  viewer_context = Soma::DwUIR::Viewer::Context.new(compositor, platform)

  image = nil

  env, retire = Rack.env(
    rack: rack,
    basis: ctx.rack_basis,
    agents: [
      Rack.uir_graphics(platform, rulebase: ctx.uiR_base),
      Rack::Image.slot(viewer_context, needle) { |pixel_rect| image = pixel_rect },
      Rack::FS.server(files),
    ] of Rack::Agent::Any,
  )

  begin
    image.not_nil!("rack did not define image `#{needle}`")
  ensure
    retire.call
  end
end

# Runs a comparison test.
def compare(ctx, test, specpath, title, a, b, text)
  Term.case({a, b}) do
    # Compares WwML source code to gzipped WwLR.
    givenpi %[(ml apath_string) (lr.gz bpath_string)] do
      expected = File.open(Path["tests"] / bpath.to(String)) do |file|
        Compress::Gzip::Reader.open(file) do |gzip|
          LR.decode(gzip)
        end
      end

      asrc = File.read(Path["tests"] / apath.to(String))

      test.long(specpath, text) do |complaints|
        actual = ML.terms(asrc, addons: ML::Addons::None)
        next if actual == expected

        complaints << Component.complaint(
          title: "#{title.to(String)} comparison test failed. Terms derived from these files are different, which is unexpected.",
          sections: [
            {"WwML", apath.to(String)},
            {"WwLR (gzipped)", bpath.to(String)},
          ]
        )
      end
    end

    givenpi %{(rack/image rackpath_string id_) (ppm imgpath_string)} do
      rack = ML.document(File.read(Path["tests"] / rackpath.to(String)))
      expected = File.open(Path["tests"] / imgpath.to(String), "rb", &.getb_to_end)

      test.long(specpath, text) do |complaints|
        actual = IO::Memory.new

        img = rack_image(ctx, rack, id)
        ppm = Soma::DwUIR::SnapFormat["ppm"]
        ppm.call(actual, img)

        next if expected.to_slice == actual.to_slice

        complaints << Component.complaint(
          title: "#{title.to(String)} comparison test failed. Terms derived from these files are different, which is unexpected.",
          sections: [
            {"DwUIR rack", rackpath.to(String)},
            {"PPM", imgpath.to(String)},
          ]
        )
      end
    end

    otherwise do
      test.complain(specpath, text) do |complaints|
        complaints << Ω.text("Comparison between #{ML.compact(a)} and #{ML.compact(b)} is not supported.")
      end
    end
  end
end

args = ARGV.dup

preview = true
if args.delete("--no-preview")
  preview = false
end

styled = true
if args.delete("--unstyled")
  styled = false
end

focused = Set(Term).new
disabled = Set(Term).new

args.each do |arg|
  if arg.prefixed_by?("-")
    disabled << Term.of(Term::Sym.new(arg[1..]))
  elsif arg.prefixed_by?("+")
    focused << Term.of(Term::Sym.new(arg[1..]))
  end
end

Ω.render(STDOUT, Ω.text("Wirewright tests tool", style: :emphasis), styled: styled)

ctx = TestContext.new(
  theme: Mf.theme(ML.document(File.read("./theme.ufold.wwml")).as_d, rem: Term[16]),
  rack_basis: ML.document(File.read("./runtime/basis.rack.wwml")).as_d,
  uiR_base: ML.document(File.read(RESOURCES / (ENV["RSET"]? || "uiR-succ8.soma.wwml"))),
)

success = TestHarness.new(preview: preview, styled: styled) do |harness|
  specpath = Path["tests"] / "index.wwml"
  specsrc = File.read("tests/index.wwml")
  spec, srcmap = ML.term_and_srcmap(specsrc, filename: specpath.to_s)
  spec.items.each_with_index do |item, index|
    text = srcmap[Term[{index}]]

    begin
      Term.case(item) do
        matchpi %[(test file_string ¦ _ tags_dict⋮ (tags_*) color_⋮ green)] do
          enabled = tags.items.any?(&.in?(focused)) || (focused.empty? && tags.items.none?(&.in?(disabled)))
          next unless enabled

          Ω.render(STDOUT, Component.processing(item), styled: styled)

          path = Path["tests"] / file.to(String)
          source = File.read(path)

          harness.group(Color.term(color)) do |test|
            begin
              test_terms, test_srcmap = ML.terms_and_srcmap(source, filename: path.to_s)
            rescue e : ML::SyntaxError
              e.humanize(STDOUT)
              break
            end

            test(ctx, test, path, test_terms, test_srcmap)
          end
        end

        matchpi %[(comparison title_string left_ right_ ¦ _ tags_dict⋮ (tags_*) color_⋮ green)] do
          enabled = tags.items.any?(&.in?(focused)) || (focused.empty? && tags.items.none?(&.in?(disabled)))
          next unless enabled

          Ω.render(STDOUT, Component.processing(item), styled: styled)

          harness.group(Color.term(color)) do |test|
            compare(ctx, test, specpath, title, left, right, text)
          end
        end

        otherwise { }
      end
    rescue e : Exception
      harness.complain(specpath, text) do |complaints|
        complaints << Component.complaint(
          title: "Spec entry crashed",
          bullet: "☠",
          sections: [
            {"SPEC ENTRY", item},
            {"EXCEPTION", e.inspect_with_backtrace},
          ]
        )
      end
    end
  end
end

if success
  exit 0
else
  exit 1
end
