require "./src/wirewright"
require "./d7_codex"

clf = D7::Codex.classifier
regime = D7::Codex.regime
tspace = D7::Codex.tspace

step = ->(top : D7::Top, circuit : Term) do
  frame0 = tspace.call(clf, circuit)

  top, frame1 = D7.step(clf, regime, top, frame0, &D7::Codex.tick)
  {top, [frame0, frame1]}
end

# circuit = ML.document(<<-WWML)

#     (frag @chat
#       (chat ()
#         (queue (@front @back) ("100" "200" "100" "100" "100" "(" ")" "(+ 1 2" "qux"))
#         (feed @front (over @src))
#         (cell @src)
#         (uniq @src (pulse @srcs))
#         (ml/term @srcs (result @!terms))
#         (repr (result @!terms) (ok @terms))
#         (feed (pulse @terms) @term)
#         (cell @term)
#         (discard @term)))
#     (log (@chat [chat ((err ⍊ detail_string) _*) _*] @details)
#       ^detail)
#     (cell @details ())
#     (view (@details details_dict @view)
#       (group
#         (h1 "Most recent problems:")
#         (^each (details as detail_string)
#           (p ^detail style: "text-red-500"))))
#     (cell @view)
# WWML

# top = D7::Top.new(Term[])
# loop do
#   puts ML.display(circuit)
#   top, frames = step.call(top, circuit)
#   circuit = frames.last
#   sleep 1.second
# end

# {% skip_file %}

class TestFailure < Exception
  getter before : Term
  getter after : Term
  getter frames : Array(Term)
  getter top : D7::Top

  def initialize(@before, @after, @frames, @top)
  end
end

class TestFailure2 < Exception
  getter expected : Term
  getter got : Term

  def initialize(@expected, @got)
  end
end

class TestAborted < Exception
end

def matches?(frame : Term, actual : Term)
  Term.case(frame) do
    matchpi %{(frame content_*)} do
      content == actual
    end

    matchpi %{(frame content_* ¦ () m1)} do
      M1.probe?(content, actual)
    end
  end
end

def test_expect_frame(step, top, rest : Term, actual)
  Term.case(rest) do
    givenpi %{frame←[frame circuit_*] rest_*} do
      if matches?(frame, actual)
        return test_frames(step, top, actual, rest)
      end

      raise TestFailure2.new(circuit, actual)
    end

    otherwise do
      raise ArgumentError.new("invalid test case: matches first step but second step not consumed")
    end
  end
end

def test_seed(step, top, state, rest)
  return if rest.empty?

  Term.case(rest) do
    givenpi %{end} do
      top, frames = step.call(top, state)
      unless frames.all?(state)
        raise TestFailure.new(state, Term.of(:end), frames, top)
      end
    end

    givenpi %{(fast-forward-to pattern_) ahead_*} do
      64.times do
        if M1.probe?(pattern, state)
          return test_seed(step, top, state, ahead)
        end

        top, frames = step.call(top, state)
        state = frames.last
      end

      raise TestFailure2.new(pattern, state)
    end
  end
end

def test(step, top, frames : Term)
  Term.case(frames) do
    givenpi %{(frame circuit_*) rest_*} do
      test_frames(step, top, circuit, rest)
    end

    givenpi %{(seed circuit_*) rest_*} do
      test_seed(step, top, circuit, rest)
    end

    otherwise do
      raise ArgumentError.new("test must start with a non-m1 frame")
    end
  end
end

def test_frames(step, top : D7::Top, state : Term, rest : Term)
  return if rest.empty?

  Term.case(rest) do
    givenpi %{end} do
      top, ahead = step.call(top, state)
      unless ahead.all?(state)
        raise TestFailure.new(state, Term.of(:end), ahead, top)
      end
    end

    givenpi %{frame←[frame after_*] rest_*} do
      top, ahead = step.call(top, state)

      if matches?(frame, ahead[1])
        return test_frames(step, top, ahead[1], rest)
      end

      if matches?(frame, ahead[0])
        return test_expect_frame(step, top, rest, ahead[1])
      end

      raise TestFailure.new(state, after, ahead, top)
    end

    # Let it evolve top without us duplicating the same frame over and over.
    givenpi %{... frame←[frame after_*] rest_*} do
      32.times do
        top, ahead = step.call(top, state)
        next if ahead.all?(state)

        if matches?(frame, ahead[1])
          return test_frames(step, top, ahead[1], rest)
        end

        if matches?(frame, ahead[0])
          return test_expect_frame(step, top, rest, ahead[1])
        end

        raise TestFailure.new(state, after, ahead, top)
      end

      raise TestAborted.new("`...` limit exceeded (something not consuming events?)")
    end
  end
end

successes = 0
failures = 0
testcases = ML.document(File.read("tests/d7.test.wwml"))
dt = Time.measure do
  1.times do |i|
    pp i
    errors = [] of Exception
    testcases.items.each do |testcase|
      Term.matchpi(testcase, %{(d7 frames_*)}) do
        begin
          test(step, D7::Top.new(Term[]), frames)
        rescue e : TestAborted | TestFailure2 | TestFailure
          errors << e
          print "X".colorize.red
          failures += 1
        else
          print ".".colorize.green
          successes += 1
        end
      end
    end

    puts

    errors.each do |e|
      case e
      when TestAborted
        puts "- ABORTED WITH MESSAGE".colorize.bold
        puts e.message
      when TestFailure2
        puts "- EXPECTED".colorize.bold
        puts ML.display(e.expected)
        puts "- GOT".colorize.bold
        puts ML.display(e.got)
      when TestFailure
        puts "- BEFORE".colorize.bold
        puts ML.display(e.before)
        puts "  AFTER".colorize.bold
        puts ML.display(e.after)
        puts "  GOT #{e.frames.size} FRAME(S)".colorize.bold
        e.frames.each do |frame|
          puts "  - FRAME"
          puts ML.display(frame)
        end
        puts "  TOPLEVEL EVENTS".colorize.bold
        puts ML.display(e.top.queue)
      end
    end
  end
end

puts
if failures > 0
  puts "#{dt.total_milliseconds}ms\t+#{successes}\t-#{"#{failures}".colorize.red.bold}"
  abort
else
  puts "#{dt.total_milliseconds}ms\t+#{"#{successes}".colorize.green.bold}"
  if ARGV.includes?("--test")
    exit
  end
end
