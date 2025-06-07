require "./wirewright"

module DwUIR2PPM
  include Ww
  include Ww::Soma::DwUIR

  extend self

  def dwuir2ppm(io, dwuir : Term, w : Int32, h : Int32)
    platform = PvgPlatform.new(Disk, Point[w, h])
    screen = PixelRect.new(0, 0, w, h)
    viewer = Viewer.new(screen, Compositor.new, platform)
    instance = Alloy.render(Term["mouse-x": 0, "mouse-y": 0, wheel: 0], dwuir) # FIXME: WTF?!?!?!
    viewer.show(instance, bg: Color.oklch(0.9, 0, 0))

    io << "P3\n"
    io << w << " " << h << "\n"
    io << "255\n"

    screen.region(screen.bounds).each_pixel_with_coords do |pixel, _, _|
      # Ignore alpha
      r, g, b, _ = pixel.rgba
      io << r << " " << g << " " << b << "\n"
    end
  end

  def run
    cases = [] of String

    Term.case(ARGV, patterns: cases) do
      matchpi(<<-WWML
      ;; Converts the DwUIR at <fin> to PPM at <fout>.
      ;;
      ;; <w> sets the width of the final image.
      ;; <h> sets the height of the final image.
      (fin_string fout_string
       (%pipe ml w←(%number 0 < (whole _) < 4096))
       (%pipe ml h←(%number 0 < (whole _) < 4096)))
      WWML
      ) do
        iw, ih = w.to(Int32), h.to(Int32)

        if File.exists?(fout.to(String))
          puts "dwuir2ppm: #{fout.to(String)} exists, overwrite? (y/)"
          unless gets == "y"
            abort "dwuir2ppm: cancelled"
          end
        end

        begin
          source = File.read(fin.to(String))
        rescue e : File::Error
          Log.fatal(exception: e)

          abort "dwuir2ppm: could not read file"
        end

        begin
          dwuir = ML.terms(source)
        rescue e : ML::SyntaxError
          e.humanize(STDERR, source)

          Log.fatal(exception: e)

          abort "dwuir2ppm: syntax error"
        end

        puts "dwuir2ppm: #{fin.to(String)}->#{fout.to(String)} (#{iw}x#{ih})"

        begin
          File.open(fout.to(String), "wb") do |io|
            io.buffer_size = 4096

            dwuir2ppm(io, dwuir, iw, ih)
          end
        rescue e : File::Error
          Log.fatal(exception: e)

          abort "dwuir2ppm: error while opening the output file"
        rescue e : IO::Error
          Log.fatal(exception: e)

          abort "dwuir2ppm: i/o error"
        end

        puts "dwuir2ppm: wrote #{fout.to(String)}"
      end

      otherwise do
        puts <<-HELP
        # Synopsis

        dwuir2ppm is a tiny tool that converts DwUIR to PPM.

        # Syntax

        #{cases.join("\n\n", &.li(bullet: "", indent: 0))}
        # Example

        $ dwuir2ppm path/to/dwuir.wwml path/to/image.ppm 1800 1000
        ...
        $ open path/to/image.ppm
        HELP
      end
    end
  end
end

{% if flag?(:dwuir2ppm) %}
  DwUIR2PPM.run
{% end %}
