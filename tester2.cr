require "./libtermbox2"
require "./wirewright"
require "./baz5_editor"

test = ML.term(File.read("./editor.test.wwml"))

puts "Replay editor.test.wwml"

root = ML.terms(%[("" | "" () @user)])
unless "-empty".in?(ARGV)
  Term.case(test) do
    matchpi %[(editor initial_dict edits_*)] do
      root = initial
      dt = Time.measure do
        edits.items.each_with_index do |edit, index|
          puts "Edit #{index} out of #{edits.items.size} edit(s)"
          Term.case(edit) do
            matchpi %[(after (motions_+) snapshot_)] do
              root0 = root
              motions.items.each do |motion|
                root = edit(root, motion)
              end
              unless root == snapshot
                puts ML.display(edit)
                puts ML.display(root0)
                puts ML.display(root)
                puts "Replay failed. Show step-by-step? y/".colorize.red
                unless gets == "y"
                  abort
                end
                debug(root0, motions)
                abort
              end
            end
          end
        end
      end

      puts "Replay success in #{dt.humanize}".colorize.bold.green
    end
  end
end
if "-r".in?(ARGV)
  exit 0
end

def debug(root0, motions)
  puts "Debug"
  puts ML.display(root0)
  gets

  dbgroot = root0
  motions.items.each do |motion|
    dbgroot = subsume(dbgroot, motion, Term.of(:edge, :user))
    puts "Motion #{motion}".colorize.blue.bold
    puts ML.display(dbgroot)
    gets

    staging0 = dbgroot
    dbgroot = rewrite(dbgroot, EDITR) do |kp, explanation, rewrite|
      puts "- Rewrite ------"
      puts "#{explanation}"

      staging1 = preview1(staging0, kp.keypath, rewrite)
      puts ML.display(staging0)
      puts "->"
      puts ML.display(staging1.term?)
      staging0 = staging1.term?
      gets
    end
  end

  puts ML.display(dbgroot)
  gets

  puts "No more motions to follow".colorize.red
end

root0 = root
motions = Term[]

while true
  puts ML.display(root)

  break unless command = gets

  if command == ".a"
    tail = Term.of(:after, motions, root)
    puts ML.display(tail)
    print "Append? (y/[_]) "
    if gets == "y"
      test = test.append(tail)
      root0 = root
      motions = Term[]
      File.open("./editor.test.wwml", mode: "w") do |io|
        io.puts ";; WARNING: Do not modify this file. It was generated automatically."

        ML.display(io, test)
      end
    end
    puts
    next
  elsif command == ".u" && !motions.items.empty?
    puts "Revert #{motions.items.last}. Editor potentially out of sync."
    motions = motions.items.grow(-1).collect
    puts ML.display(motions)
    next
  elsif command == ".d"
    debug(root0, motions)
    next
  end

  motion = ML.term(command)
  root = edit(root, motion)
  motions = motions.append(motion)
end
