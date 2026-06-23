require "./wirewright"

module InteractiveRack
  extend self
  include Ww

  def run(args = ARGV) : Nil
    args = args.dup

    single_step = !!(args.delete("--single-step") || args.delete("-s"))
    detailed = !!(args.delete("--detailed") || args.delete("-d"))
    show_subframes = !!args.delete("--subframes")

    unless filename = args.shift?
      abort <<-'HELP'
      USAGE
        irack [OPTIONS] path/to/seed.wwml

      SYNOPSIS
        Interactive Rack. Wirewright Rack is a rewrite regime implemented using
        D7. Wirewright D7 is a symbolic physics toolkit. Wirewright is a symbolic
        physics environment. Symbolic physics is physics where instead of objects
        and geometry you have symbols and symbolic structure!

      OPTIONS
        -s, --single-step
          Wait for input before computing the next frame.
        -d, --detailed
          Output frame comments and statistics at the end of the frame.
        --subframes
          Print subframes instead of frames.

      EXAMPLE
        $ echo "(cell @x 0) (cell @y) (feed @x @y)" > /tmp/seed.wwml
        $ irack /tmp/seed.wwml
        ((cell @x 0)
         (cell @y)
         (feed @x @y))

        ((cell @x)
         (cell @y 0)
         (feed @x @y))

      HELP
    end

    begin
      source = PathService.read_string(NormalPath[filename])
    rescue e : PathService::Error
      abort "#{e.message}"
    end

    begin
      seed = ML.document(source)
    rescue e : ML::SyntaxError
      e.filename = filename
      e.humanize(STDERR)
      abort "syntax error"
    end

    circuit = seed
    assembler_state = Rack::Assembler.state
    frame_count = 0u64
    t = [] of Time::Span
    cache = GenerationalCache(Term, D7::ParseTree).new

    puts ML.display(circuit, maxwidth: 80)
    if single_step
      gets
    end

    loop do
      circuit0 = circuit

      subframes = Slice[circuit]

      dt = Time.measure do
        cache.epoch do
          subframes += Rack::Tspace.step(Rack.clf, subframes.last, Rack::Prepass, cache: cache)
        end

        cache.epoch do
          rtree = D7.parse(Rack.clf, subframes.last, cache: cache, reply: D7::ParseTree)
          wtree = rtree
          subframes += Slice[Rack::Assembler.step(Rack.clf, rtree, wtree, assembler_state)]
        end

        cache.epoch do
          subframes += Rack.step(Rack.clf, subframes.last, Rack::Prepass, cache: cache)
        end
      end

      subn = 0

      D7.fuse(Rack.clf, circuit, subframes) do |subframe|
        if show_subframes && circuit != subframe
          if detailed
            puts ";; Subframe #{subn + 1} of frame #{frame_count + 1}" # count from 1
          end

          puts ML.display(subframe, maxwidth: 80)
          puts
          if single_step
            gets
          end
          subn += 1
        end

        circuit = subframe
      end

      break if circuit0 == circuit # Quiescence

      unless show_subframes
        if detailed
          puts ";; Frame #{frame_count + 1}" # count from 1
        end

        puts ML.display(circuit, maxwidth: 80) # Show frame
        if single_step
          gets
        end
      end

      frame_count += 1
      t << dt
      if t.size >= 32
        t = [t.median]
      end
    end

    if detailed
      puts "| frame_count=#{frame_count} lowpass(median(frametime))=#{t.median.humanize}"
    end
  end
end

InteractiveRack.run
