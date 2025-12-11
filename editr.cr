require "./src/wirewright"

def editR(ruleset : Ruleset)
  exhR(absR(alloy_rulesetR(ruleset)))
end

SELECTOR = ML.term(%[(%any° [rule pattern_ template_] [backmap pattern_ backspec_])])

def editR(rulebase : Term)
  editR(Ruleset.select(SELECTOR, rulebase))
end

def dispatch(state : Term, msg : Term)
  Term.each_keypath_and_node(state) do |keypath, node|
    Term.case(node) do
      matchpi %{[I _*]} do
        state = Term.morph(state, keypath) do |cursor|
          Term.of(cursor.append(msg))
        end

        false # no descend
      end

      otherwise do
        true # descend
      end
    end
  end

  state
end

# Load codex
codex = ML.document(File.read("./codex/editR.codex.wwml"))
editR = editR(codex)

failures = [] of {expected: Term, got: Term, text: StringView}

# Load tests.
tests, srcmap = ML.document_and_srcmap(File.read("./tests/editR.test.wwml"))
state = Term.of

rwtime = 0.milliseconds

tests.items.each_with_index do |testcase, index|
  Term.case(testcase) do
    matchpi %{(edit seed_ (msgs_*) result_)} do
      state = seed
      msgs.items.each do |msg|
        rwtime += Time.measure do
          state = rewrite(dispatch(state, msg), editR)
        end
      end

      unless state == result
        print "X".colorize.red
        failures << {expected: result, got: state, text: srcmap[{index}]}
        next
      end

      print "·".colorize.green
    end
  end
end

puts
puts

path = "tests/editR.test.wwml"

failures.each do |failure|
  _, line, column = ML::SyntaxError.lookaround(failure[:text])

  puts "#{"▉ ".colorize.red}#{"#{path}:#{line}:#{column}".colorize.underline}"
  puts "  EXPECTED"
  puts ML.display(failure[:expected], maxwidth: 80)
  puts "  GOT"
  puts ML.display(failure[:got], maxwidth: 80)
  puts
end

if failures.empty?
  puts "OK #{tests.itemsize}  #{path}  #{rwtime.total_milliseconds}ms".colorize.green
else
  puts "ERR #{tests.itemsize}  #{path}  #{rwtime.total_milliseconds}ms".colorize.red
end

puts

puts <<-BANNER
#{"# editR appender".colorize.bold}
Hit Ctrl-D to quit.
BANNER

puts

seed = state
msgs = Term[]

preview = -> do
  msgs.items.reduce(seed) { |memo, msg| rewrite(dispatch(memo, msg), editR) }
end

show = ->(term : Term) do
  ed = ->(full : String, range : Range(Int32, Int32)) do
    print " ▍".colorize.dark_gray

    full.each_char_with_index do |char, index|
      if index.in?(range) || (range.size == 0 && range.begin == index)
        if char == '\n'
          print " ".colorize.back(:white).fore(:black)
          print '\n'
        else
          print char.to_s.colorize.back(:white).fore(:black)
        end
      else
        print char
      end
      if char == '\n'
        print " ▍".colorize.dark_gray
      end
    end

    if full.size.in?(range) || (range.size == 0 && range.begin == full.size)
      print " ".colorize.back(:white).fore(:black)
    end
    puts
  end

  Term.case(term) do
    matchpi %{[[I _*] (l_string * r_string) anchor←(%number +i32)]}, l: String, r: String, anchor: Int32 do
      ed.call(l + r, Math.min(anchor, l.size)...Math.max(anchor, l.size))
    end

    otherwise do
      puts ML.display(term, maxwidth: 80)
        .each_line(chomp: false)
        .map { |line| " ▍".colorize.dark_gray.to_s + line }
        .join
    end
  end
end

show.call(seed)

loop do
  print "> "
  break unless input = gets

  begin
    command = ML.terms(input)
  rescue e : ML::SyntaxError
    e.humanize(STDERR)
    next
  end

  Term.case(command) do
    givenpi %{(%any s seed)} do
      puts "Do you really want to discard #{msgs.size} message(s)? Type `s` to continue."
      next unless gets == "s"

      seed = preview.call
      msgs = Term[]

      show.call(seed)
    end

    givenpiT %{(%any s seed) line←(%number +i32) column←(%number +i32)} do
      found = false

      new_tests, new_srcmap = ML.document_and_srcmap(File.read("./tests/editR.test.wwml"))
      new_tests.items.each_with_index do |test, index|
        next unless text = new_srcmap.cd(index)[Tpath[]]?
        _, its_line, its_column = ML::SyntaxError.lookaround(text)
        next unless {line, column} == {its_line, its_column}

        Term.matchpi(test, %{(edit its-seed_ _ _)}) do
          puts "Using state as seed:"
          show.call(its_seed)

          seed = its_seed
        end

        found = true
        break
      end

      unless found
        puts "Seed not found"
      end
    end

    givenpi %{(%any c change) new-seed_} do
      unless msgs.empty?
        puts "Are you sure? Messages in the current test case will be reset. Type `c` to continue."
        next unless gets == "c"
      end

      msgs = Term[]
      seed = new_seed

      show.call(preview.call)
    end

    givenpi %{(%any u undo)} do
      if msgs.empty?
        puts "Nothing to undo"
        next
      end

      msgs = msgs.items.grow(-1).collect

      show.call(preview.call)
    end

    givenpi %{(%any p preview)} do
      show.call(preview.call)
    end

    givenpi %{(%any t testcase)} do
      result = preview.call
      testcase = Term.of(:edit, seed, msgs, result)
      show.call(testcase)
    end

    givenpi %{(%any w write)} do
      result = preview.call
      testcase = Term.of(:edit, seed, msgs, result)

      File.open("./tests/editR.test.wwml", "a") do |io|
        io.puts
        ML.display(io, testcase, maxwidth: 80)
        io.flush
      end

      puts "Wrote to disk:"
      show.call(testcase)

      seed = result
      msgs = Term[]

      show.call(preview.call)
    end

    givenpi %{(%any ws write-steps)} do
      File.open("./tests/editR.test.wwml", "a") do |io|
        result = msgs.items.reduce(seed) do |memo, msg|
          step_result = rewrite(dispatch(memo, msg), editR)
          testcase = Term.of(:edit, memo, Term[{msg}], step_result)

          io.puts
          ML.display(io, testcase, maxwidth: 80)
          io.flush

          puts "Wrote to disk:"
          show.call(testcase)

          step_result
        end

        seed = result
        msgs = Term[]
      end

      show.call(preview.call)
    end

    givenpi %{(%any r reload)} do
      codex = ML.document(File.read("./codex/editR.codex.wwml"))
      editR = editR(codex)
      puts "Reloaded codex from disk"
    end

    givenpi %{(%any ? h help)} do
      puts <<-'HELP'
      Commands:

        [s]eed
          Use the current preview as seed, discarding all messages that lead to it.

        [s]eed line←(%number +i32) column←(%number +i32)
          Use the test at line:column as seed.

        c[hange] seed_
          Changes the seed to seed_.

        u[ndo]
          Undo the last append.

        p[review]
          Show editor preview.

        t[estcase]
          Show the test case built so far.

        w[rite]
          Write the test case built so far to disk and use its result as the seed.

        w[rite-]s[tep]
          Write each message from the test case built so far as a separate test
          case. Use the last result as the seed.

        r[eload]
          Reloads the codex file.

        h[elp], ?
          Print this message.

        msg_
          Append msg_ to the current message list.
      HELP
    end

    otherwise do
      msgs = msgs.append(command)

      show.call(preview.call)
    end
  end
end
