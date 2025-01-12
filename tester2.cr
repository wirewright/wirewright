require "./libtermbox2"
require "./wirewright"
require "./foo"

# TODO: a tui

SELECTOR = ML.parse1(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_) (barrier pattern←barrier←_))])
BASE = ML.parse(File.read("#{__DIR__}/editor.soma.wwml"))
RULESET = Ruleset.select(SELECTOR, BASE, applier: Applier.new(->(term : Term) { Offspring::One.new(rec(term)) }))
REWRITER = editr(RULESET)

def apply(root : Term, motion : Term) : Term
  pipe(root,
    subsume(motion, Term.of(:edge, :user)),
    rewrite(REWRITER, cap: 64))
end


test = ML.parse1(File.read("./editor.test.wwml"))

root = ML.parse(%[("" | "" () @user)])
Term.case(test) do
  matchpi %[(editor initial_dict edits_*)] do
    root = initial
    edits.items.each do |edit|
      Term.case(edit) do
        matchpi %[(after (motions_+) snapshot_)] do
          root0 = root
          motions.items.each do |motion|
            root = apply(root, motion)
          end
          unless root == snapshot
            puts ML.display(edit)
            puts ML.display(root0)
            puts ML.display(root)
            raise "replay failed"
          end
        end
      end
    end
  end
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
    puts "Revert #{motions.items.last}"
    motions = motions.items.end.move(-1).collect
    next
  elsif command == ".d"
    puts "Debug"
    puts ML.display(root0)
    gets
    dbgroot = root0
    motions.items.each do |motion|
      dbgroot = subsume(dbgroot, motion, Term.of(:edge, :user))
      puts ML.display(dbgroot)
      gets
      dbgroot = rewrite(dbgroot, REWRITER) do |im|
        puts ML.display(im)
        gets
      end
    end
    next
  end

  motion = ML.parse1(command)
  root = apply(root, motion)
  motions = motions.append(motion)
end
