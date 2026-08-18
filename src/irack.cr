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

    display_mask = Rack::Automaton::DisplayMask::Frame
    if show_subframes
      display_mask |= Rack::Automaton::DisplayMask::Subframe
    end

    automaton = Rack::Automaton.new(measure: detailed, display_mask: display_mask)

    circuit = seed
    frame_count = 1u64
    subframe_count = 1u64

    if detailed
      puts
      puts ";; Frame 0 (seed)"
      puts
    end

    puts ML.display(seed, maxwidth: 80)

    if single_step
      gets
    end

    loop do
      circuit, action = automaton.blocking_next(circuit)

      case action
      in Rack::Automaton::DisplaySubframe
        next unless show_subframes

        if detailed
          puts
          puts ";; Frame #{frame_count}.#{subframe_count}"
          puts
        end

        puts ML.display(action.content, maxwidth: 80)

        if single_step
          gets
        end

        subframe_count += 1
      in Rack::Automaton::DisplayFrame
        if show_subframes
          subframe_count = 1u64
          frame_count += 1
          next
        end

        if detailed
          puts ";; Frame #{frame_count}"
        end

        puts ML.display(action.content, maxwidth: 80)

        if detailed
          puts "| frame_count=#{frame_count} rec_median(frametime, 32)=#{automaton.median.humanize}"
        end

        if single_step
          gets
        end

        frame_count += 1
      in Rack::Automaton::End
        break
      end
    end
  end
end

ctx = Fiber::ExecutionContext::Isolated.new("Wirewright irack", spawn_context: Ww::MT) { InteractiveRack.run }
ctx.wait
