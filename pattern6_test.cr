require "./wirewright"
require "./baz4"

CASES  = File.read("#{__DIR__}/patterns.test.wwml") + "\n" + File.read("#{__DIR__}/editor.test.wwml")
PEOPLE = Term.of(JSON.parse(File.read("#{__DIR__}/data/people.json")))

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

def track(ctx, pattern)
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
      track(ctx, rule) do
        first = true
        match = ->(matchee : Term) do
          begin
            ctx.stats.run { Term::M1.backmap?(pattern, backdict, matchee) }
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
      track(ctx, pattern) do
        first = true
        match = ->(matchee : Term) do
          begin
            envs = ctx.stats.run { Term::M1.matches(pattern, matchee) }
            envs.map!(&.without(:"(keypaths)"))
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
      track(ctx, testcase) do
        patterns = {} of Term::M1::Specificity => Set(Term)

        specificity0 = nil

        levels.items.each do |level|
          Term.case(level) do
            matchpi %[(level members_*)] do
              specificity1 = nil

              members.each_entry do |_, pattern|
                normp = Term::M1.normal(pattern)
                specificity2 = ctx.stats.run { Term::M1.specificity(normp, toplevel: true) }
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
      exps.items.each do |exp|
        Term.case(exp) do
          matchpi %[(- blacklist_*)] do
            ctx.stats.account

            track(ctx, exp) do
              blacklist.items.each do |item|
                normitem = Term::M1.normal(item)
                next unless head = ctx.stats.run { Term::M1.head?(normitem) }

                ctx.failures << Term.of(:"mismatch/head", item, :==, :nothing, :GOT, head)
              end
            end
          end

          matchpi %[(of lhs_ rhs_)] do
            ctx.stats.account

            track(ctx, exp) do
              normlhs = Term::M1.normal(lhs)
              head = ctx.stats.run { Term::M1.head?(normlhs) }

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

    matchpi %[(editor initial_dict edits_*)] do
      next if "-noed".in?(ARGV)
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
  end
end

record TestContext,
  groups = {} of Term => Term,
  templates = {} of Term => Term,
  failures = [] of Term,
  stats = Statistics.new

# Select (isolate)d test cases.

testcases = [] of Term

spec = ML.parse(CASES)
spec.items.each do |testcase|
  Term.case(testcase, engine: Term::M0) do
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

if ctx.failures.empty?
  puts "Ran #{ctx.stats.ncases} test case(s) in #{ctx.stats.duration.humanize}.".colorize.green.bold
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
  puts "Ran #{ctx.stats.ncases} test case(s) in #{ctx.stats.duration.humanize}.".colorize.red.bold
  puts "Registered #{ctx.failures.size} unexpected result(s)."
end
