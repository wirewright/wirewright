require "./src/wirewright"

include Ww::Soma::DwUIR

cases = [] of String

Term.case(ARGV, patterns: cases) do
  matchpi(<<-WWML
    (fin_string fout_string
     (%pipe ml w←(%number 0 < (whole _) < 4096))
     (%pipe ml h←(%number 0 < (whole _) < 4096)))

    ;; Converts the DwUIR at <fin> to PPM at <fout>.
    ;;
    ;; <w> sets the width of the final image.
    ;; <h> sets the height of the final image.
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
    rescue File::Error
      abort "dwuir2ppm: could not read file"
    end

    begin
      dwuir = ML.terms(source)
    rescue e : ML::SyntaxError
      e.humanize(STDERR, source)

      abort "dwuir2ppm: syntax error"
    end

    puts "dwuir2ppm: #{fin.to(String)}->#{fout.to(String)} (#{iw}x#{ih})"
    puts "dwuir2ppm: prepare frame"

    platform = PvgPlatform.new(Disk, Point[iw, ih])
    screen = PixelRect.new(0, 0, iw, ih)
    viewer = Viewer.new(screen, Compositor.new, platform)
    instance = Alloy.render(Term[x: 0, y: 0, wheel: 0], dwuir) # FIXME: ?!?!?!
    viewer.show(instance, bg: Color.oklch(0.9, 0, 0))

    puts "dwuir2ppm: frame ready"

    begin
      File.open(fout.to(String), "wb") do |io|
        io.buffer_size = 4096

        io << "P3\n"
        io << w << " " << h << "\n"
        io << "255\n"

        screen.region(screen.bounds).each_pixel_with_coords do |pixel, _, _|
          # Ignore alpha
          r, g, b, _ = pixel.rgba
          io << r << " " << g << " " << b << "\n"
        end
      end
    rescue File::Error
      abort "dwuir2ppm: failed to write file"
    end

    puts "dwuir2ppm: wrote #{fout.to(String)}"
  end

  otherwise do
    puts <<-HELP
    # Synopsis

    dwuir2ppm is a tiny tool that converts DwUIR to PPM.

    # Usage

    $ dwuir2ppm path/to/dwuir.wwml path/to/image.ppm 1800 1000
    ...
    $ open path/to/image.ppm

    # Syntax

    #{cases.join("\n\n", &.li(bullet: "", indent: 0, strip_first: true))}

    HELP
  end
end
