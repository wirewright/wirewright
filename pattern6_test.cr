require "./src/wirewright"
require "./baz5_editor"
require "./delta7_proto2"

CASES = Dir["tests/[^-]*.test.wwml"].join('\n') { |test| File.read(test) }

PEOPLE = Term.of(JSON.parse(File.read("data/people.json")))

class Statistics
  getter ncases : Int32
  getter duration : Time::Span

  def initialize
    @ncases = 0
    @duration = 0.nanoseconds
  end

  def account
    @ncases += 1
  end

  def run(timed = true, &)
    unless timed
      return yield
    end

    start = Time.monotonic

    begin
      yield
    ensure
      elapsed = Time.monotonic - start
      @duration += elapsed
    end
  end
end

def track(ctx, pattern, &)
  failures0 = ctx.failures.size

  begin
    yield
  rescue e : Exception
    ctx.failures << Term.of(:exception, pattern, Term::Sym.new(e.class.name), e.message, e.backtrace)
  ensure
    failures1 = ctx.failures.size
    if failures0 == failures1
      print ".".colorize.green
    else
      print "X".colorize.red.bold
    end
  end
end

# TODO: split, this shouldn't be a megamethod. One strange way to do this is to
# match deep pairs, i.e. parent-child pairs (for toplevel it's symbol "toplevel")
#  I.e. (group body_*) is going to turn into toplevel (group body_*)
def process(queue, testcase, ctx)
  Term.case(testcase) do
    matchpi %[(group body_*)] do
      queue.concat(body.items.to_a.shuffle!)
    end

    matchpi %[(template name_symbol subject_ body_)] do
      if ctx.templates.has_key?(name)
        raise ArgumentError.new("template '#{name}' already exists")
      end

      ctx.templates[name] = Term.of(subject, body)
    end

    matchpi %[(instance name_symbol values_*)] do
      unless template = ctx.templates[name]?
        queue << testcase
        return
      end

      subject, body = template
      values.items.each do |value|
        queue << Term.of(body.subst(Term[].with(subject, value)))
      end
    end

    matchpi %[(backmap rule←(pattern_ backdict_) body_*)] do
      next if "-no-pattern".in?(ARGV)

      track(ctx, rule) do
        first = true
        match = ->(matchee : Term) do
          begin
            ctx.stats.run { M1.backmapr(pattern, backdict, matchee).term? }
          ensure
            if first
              ctx.stats.account
            end
            first = false
          end
        end

        body.items.each do |exp|
          Term.case(exp) do
            matchpi %[(= matchee_ whitelist_*)] do
              result = match.call(matchee)
              next if result.in?(whitelist.items.to_set)

              ctx.failures << Term.of(:mismatch, rule, exp, matchee, result)
            end

            matchpi %[(- blacklist_*)] do
              blacklist.items.each do |matchee|
                next unless result = match.call(matchee)

                ctx.failures << Term.of(:match, rule, exp, matchee, result)
              end
            end
          end
        end
      end
    end

    matchpi %[(pattern pattern_ body_*)] do
      next if "-no-pattern".in?(ARGV)

      track(ctx, pattern) do
        first = true
        match = ->(matchee : Term) do
          begin
            envs = ctx.stats.run do
              M1.matches(pattern, matchee, opt: M1::O2)
            end
            envs.map!(&.without(:"(backpaths)"))

            # Make sure that the pattern evaluates to the same stuff on all different
            # optimization levels. Lower opt levels expose deep rejection paths (and
            # possibly bugs down there!) that would otherwise never be visited due
            # to e.g. sketch opt that can reject at the very beginning of matching.
            {M1::O1, M1::O0}.each do |opt|
              next if envs == M1.matches(pattern, matchee, opt: opt).map!(&.without(:"(backpaths)"))

              raise "optimization level #{opt} does not match like O2"
            end

            envs
          ensure
            if first
              ctx.stats.account
            end
            first = false
          end
        end

        body.items.each do |exp|
          Term.case(exp) do
            matchpi %[(+ whitelist_*)] do
              whitelist.items.each do |matchee|
                envs = match.call(matchee)
                next unless envs.empty?
                ctx.failures << Term.of(:mismatch, pattern, exp, matchee)
              end
            end

            matchpi %[(- blacklist_*)] do
              blacklist.items.each do |matchee|
                envs = match.call(matchee)
                next if envs.empty?
                ctx.failures << Term.of(:match, pattern, exp, matchee, envs)
              end
            end

            matchpi %[(= $$PEOPLE matches_*)] do
              envs = match.call(PEOPLE)
              next if envs.to_set == matches.items.to_set
              ctx.failures << Term.of(:mismatch, pattern, exp, :"$$PEOPLE", envs)
            end

            matchpi %[(= matchee_ matches_*)] do
              envs = match.call(matchee)
              next if envs.to_set == matches.items.to_set
              ctx.failures << Term.of(:mismatch, pattern, exp, matchee, envs)
            end

            matchpi %[(⊆ matchee_ matchsets_*)] do
              envs = match.call(matchee)
              next if envs.to_set.in?(matchsets.items.map(&.items.to_set))
              ctx.failures << Term.of(:mismatch, pattern, exp, matchee, envs)
            end
          end
        end
      end
    end

    matchpi %[(specificity levels_*)] do
      next if "-no-pattern".in?(ARGV)

      track(ctx, testcase) do
        patterns = {} of M1::Specificity => Set(Term)

        specificity0 = nil

        levels.items.each do |level|
          Term.case(level) do
            matchpi %[(level members_*)] do
              specificity1 = nil

              members.each_entry do |_, pattern|
                normp = M1.normal(pattern)
                specificity2 = ctx.stats.run { M1.specificity(normp, toplevel: true) }
                specificity1 ||= specificity2
                next if specificity1 == specificity2

                ctx.failures << Term.of(:"mismatch/specificity", pattern, :==, specificity1, :GOT, specificity2)
              end

              next unless specificity1

              unless specificity0
                specificity0 = specificity1
                next
              end

              unless specificity0 < specificity1
                ctx.failures << Term.of(:"mismatch/specificity", level, :>, specificity0, :GOT, specificity1)
                next
              end

              specificity0 = specificity1
            end
          end
        end
      ensure
        ctx.stats.account
      end
    end

    matchpi %[(head exps_*)] do
      next if "-no-pattern".in?(ARGV)

      exps.items.each do |exp|
        Term.case(exp) do
          matchpi %[(- blacklist_*)] do
            ctx.stats.account

            track(ctx, exp) do
              blacklist.items.each do |item|
                normitem = M1.normal(item)
                next unless head = ctx.stats.run { M1.head?(normitem) }

                ctx.failures << Term.of(:"mismatch/head", item, :==, :nothing, :GOT, head)
              end
            end
          end

          matchpi %[(of lhs_ rhs_)] do
            ctx.stats.account

            track(ctx, exp) do
              normlhs = M1.normal(lhs)
              head = ctx.stats.run { M1.head?(normlhs) }

              if head.nil?
                ctx.failures << Term.of(:"mismatch/head", lhs, :==, rhs, :GOT, :nothing)
              elsif head != rhs
                ctx.failures << Term.of(:"mismatch/head", lhs, :==, rhs, :GOT, head)
              end
            end
          end
        end
      end
    end

    {% for method in %w[bounds depth] %}
      matchpi %[({{method.id}} exps_*)] do
        next if "-no-pattern".in?(ARGV)

        exps.items.each do |exp|
          Term.case(exp) do
            matchpi %[(of patterns_+ ¦ lhs_)] do
              ctx.stats.account

              track(ctx, exp) do
                patterns.items.each do |pattern|
                  normp = M1.normal(pattern)
                  range = ctx.stats.run { M1.{{method.id}}(normp) }
                  rhs = Term.of(
                    min: range[0] == Magnitude::INFINITY ? nil : range[0],
                    max: range[1] == Magnitude::INFINITY ? nil : range[1],
                  )

                  unless lhs == rhs
                    ctx.failures << Term.of(:"mismatch/{{method.id}}", pattern, :==, lhs, :GOT, rhs)
                  end
                end
              end
            end
          end
        end
      end
    {% end %}

    matchpi %[(editor initial_dict edits_*)] do
      next if "-no-editor".in?(ARGV)

      root = initial

      edits.items.each do |edit|
        Term.case(edit) do
          matchpi %[(after (motions_+) snapshot_)] do
            ctx.stats.account
            track(ctx, edit) do
              ctx.stats.run do
                motions.items.each do |motion|
                  root = edit(root, motion)
                end
              end
              unless root == snapshot
                ctx.failures << Term.of(:"mismatch/editor", root, :==, snapshot, :MOTIONS, motions)
                return # The whole test case should fail.
              end
            end
          end
        end
      end
    end

    # The limit is set low by default to have faster failure. Tests should increase it if
    # they expect themselves to run longer for success. In an ideal world, instead of a limit,
    # we'd perhaps have some kind of "divergence" limit but hey; we're not in an ideal
    # world are we?
    matchpi %[(d7 initial_dict expected_dict ¦ limit: (%optional 128 limit←(%number +i32)))] do
      next if "-no-d7".in?(ARGV)

      ctx.stats.account

      track(ctx, testcase) do
        ctx.stats.run do
          ok, latest = D7.run?(initial.unsafe_as_d, cond: D7::Equal.new(expected.unsafe_as_d, nonshadow: true, limit: limit.to(Int32)))
          unless ok
            ctx.failures << Term.of(:"d7/interrupted", D7.nonshadow(latest), :==, expected, :LIMIT, limit)
          end
        end
      end
    end

    matchpi %[(d7 initial_dict matches pattern_ ¦ limit: (%optional 128 limit←(%number +i32)))] do
      next if "-no-d7".in?(ARGV)

      ctx.stats.account

      track(ctx, testcase) do
        ctx.stats.run do
          ok, latest = D7.run?(initial.unsafe_as_d, cond: D7::Matches.new(pattern, nonshadow: true, limit: limit.to(Int32)))
          unless ok
            ctx.failures << Term.of(:"d7/interrupted", D7.nonshadow(latest), :MISMATCH, pattern, :LIMIT, limit)
          end
        end
      end
    end

    matchpi %[(tspace flow_*)] do
      next if "-no-tspace".in?(ARGV)

      ctx.stats.account

      track(ctx, testcase) do
        ctx.stats.run do
          tspace(flow.unsafe_as_d) do |seeing, expected|
            next if seeing == expected
            ctx.failures << Term.of(:"mismatch/tspace", testcase, :GOT, seeing, :EXPECTED, expected)
            break
          end
        end
      end
    end
  end
end

alias MM = Meridium

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

record TestContext,
  groups = {} of Term => Term,
  templates = {} of Term => Term,
  failures = [] of Term,
  stats = Statistics.new

# Select (isolate)d test cases.

testcases = [] of Term

spec = ML.terms(CASES)
spec.items.each do |testcase|
  Term.case(testcase) do
    match({:isolate, :"_*"}) do
      body = testcase.items.move(1)
      body.each { |item| testcases << item }
    end

    otherwise { }
  end
end

# If no (isolate)d cases, run all cases.
if testcases.empty?
  testcases = spec.items.to_a
end

testcases.shuffle!

queue = Deque(Term).new
queue.concat(testcases)

ctx = TestContext.new

while testcase = queue.shift?
  process(queue, testcase, ctx)
end

puts
puts

require "./src/dwuir2ppm"

# Run DwUIR tests
unless "-no-dwuir".in?(ARGV)
  begin
    dwuir = ML.terms(File.read("./tests/dwuir.in.wwml"))

    expected = File.open("./tests/dwuir.expected.ppm", "rb", &.getb_to_end)
    actual = IO::Memory.new

    ctx.stats.run do
      DwUIR2PPM.dwuir2ppm(actual, dwuir, 1800, 1000)
    end

    if expected.to_slice == actual.to_slice
      puts "✔️ DwUIR actual image matches expected image".colorize.green
    else
      puts "❌DwUIR actual image does not match expected image".colorize.red
      puts
      puts "  Either DwUIR does not work anymore; or the expected image is out of date."
      puts "  Use the `dwuir2ppm` tool to re-generate."
    end
  rescue e
    Log.error(exception: e)

    puts "❌Crashed while running DwUIR tests".colorize.red
  end

  puts
end

if ctx.failures.empty?
  puts "Ran #{ctx.stats.ncases} test case(s) in #{ctx.stats.duration.humanize}.".colorize.green
  exit 0
else
  puts
  ctx.failures.each_with_index do |detail, index|
    puts "- Got the following unexpected result:"
    Term.case(detail) do
      matchpi %[(exception testcase_ cls_ message_ backtrace_)] do
        print "#{cls}".colorize.red.bold, ": #{message.to(String)}\n"
        backtrace.items.join(STDOUT, '\n') do |item, io|
          io << "  " << item.to(String)
        end
        puts
        puts "Test case: #{testcase}"
      end

      otherwise do
        puts ML.display(detail)
      end
    end
  end
  puts
  puts "Ran #{ctx.stats.ncases} test case(s) in #{ctx.stats.duration.humanize}.".colorize.red
  puts "Registered #{ctx.failures.size} unexpected result(s)."
  exit 1
end
