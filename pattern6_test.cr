require "./wirewright"

CASES  = File.read("#{__DIR__}/patterns.test.wwml")
PEOPLE = Term.of(JSON.parse(File.read("#{__DIR__}/data/people.json")))
WORDS  = File.read_lines("./data/words.txt")

class Statistics
  getter ncases : Int32
  getter duration : Time::Span

  def initialize
    @ncases = 0
    @duration = 0.nanoseconds
  end

  def run(timed = true, accounted = true, &)
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
  ensure
    if accounted
      @ncases += 1
    end
  end
end

def catch(ctx, pattern)
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

def process(queue, testcase, ctx)
  Term.case(testcase, engine: Term::M0) do
    match({:group, :"_*"}, cue: :group) do
      body = testcase.items.move(1)
      queue.concat(body.to_a.shuffle!)
    end

    match({:template, :name_symbol, :subject_, :body_}, cue: :template) do |name, subject, body|
      if ctx.templates.has_key?(name)
        raise ArgumentError.new("template '#{name}' already exists")
      end

      ctx.templates[name] = Term.of(subject, body)
    end

    match({:instance, :name_symbol, :"_*"}, cue: :instance) do |name|
      unless template = ctx.templates[name]?
        queue << testcase
        return
      end

      subject, body = template

      values = testcase.items.move(2)
      values.each do |value|
        queue << Term.of(body.subst(Term[].with(subject, value)))
      end
    end

    match({:backmap, {:pattern_, :"_*"}, :"_*"}, cue: :backmap) do |pattern|
      rule = testcase[1]
      catch(ctx, rule) do
        layers = rule.items.move(1)
        body = testcase.items.move(2)

        first = true
        match = ->(matchee : Term) do
          result = ctx.stats.run(accounted: first) { Term::M1.backmap?(pattern, layers[0], matchee) }
          first = false
          result
        end

        body.each do |exp|
          Term.case(exp, engine: Term::M0) do
            match({:"=", :matchee_, :"_*"}, cue: :"=") do |matchee|
              result = match.call(matchee)
              unless result.in?(exp.items.move(2).to_set)
                failure = Term.of(:mismatch, rule, exp, matchee)
                failure = failure.append(result)
                ctx.failures << Term.of(failure)
              end
            end

            match({:-, :"_*"}, cue: :-) do
              matchees = exp.items.move(1)
              matchees.each do |matchee|
                if result = match.call(matchee)
                  ctx.failures << Term.of(:match, rule, exp, matchee, result)
                end
              end
            end
          end
        end
      end
    end

    # Pattern test case.
    match({:pattern, :pattern_, :"_*"}, cue: :pattern) do |pattern|
      catch(ctx, pattern) do
        first = true
        match = ->(matchee : Term) do
          envs = ctx.stats.run(accounted: first) { Term::M1.matches(pattern, matchee) }
          envs.map!(&.without(:"(keypaths)"))
          first = false
          envs
        end

        args = testcase.items.move(2)
        args.each do |arg|
          Term.case(arg, engine: Term::M0) do
            match({:+, :"_*"}, cue: :+) do
              matchees = arg.items.move(1)
              matchees.each do |matchee|
                envs = match.call(matchee)
                if envs.empty?
                  ctx.failures << Term.of(:mismatch, pattern, arg, matchee)
                end
              end
            end

            match({:-, :"_*"}, cue: :-) do
              matchees = arg.items.move(1)
              matchees.each do |matchee|
                envs = match.call(matchee)
                unless envs.empty?
                  ctx.failures << Term.of(:match, pattern, arg, matchee, envs)
                end
              end
            end

            match({:"=", :"$$PEOPLE", :"_*"}, cue: :"$$PEOPLE") do
              envs = match.call(PEOPLE)
              unless envs.to_set == arg.items.move(2).to_set
                ctx.failures << Term.of(:mismatch, pattern, arg, :"$$PEOPLE", envs)
              end
            end

            match({:"=", :matchee_, :"_*"}, cue: :"=") do |matchee|
              envs = match.call(matchee)
              unless envs.to_set == arg.items.move(2).to_set
                ctx.failures << Term.of(:mismatch, pattern, arg, matchee, envs)
              end
            end

            match({:"⊆", :matchee_, :"_*"}, cue: :"⊆") do |matchee|
              envs = match.call(matchee)
              unless envs.to_set.in?(arg.items.move(2).map(&.items.to_set))
                ctx.failures << Term.of(:mismatch, pattern, arg, matchee, envs)
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
