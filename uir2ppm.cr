require "./src/wirewright"
require "./uir-succ"

module UIR2PPM
  include Ww
  include Ww::Soma::DwUIR

  extend self

  struct Renderer
    SELECTOR = ML.term %{(rule pattern_ template_)}

    def initialize(@spec : Term)
      @ruleset = Ruleset.select(SELECTOR, @spec)
    end

    def call(spec : Term, view : Term) : {Renderer, Term}
      if @spec == spec
        {self, UIR2PPM.render(@ruleset, view)}
      else
        rr = Renderer.new(spec)
        rr.call(spec, view)
      end
    end
  end

  protected def renderer(spec : Term)
    Renderer.new(spec)
  end

  protected def render(ruleset : Ruleset, view : Term)
    unless view.type.dict? # base case
      return view
    end

    responses = ruleset.responses(view)
    responses.each do |response|
      pr, rule = response

      case pr
      in Pr::One  then env = pr.env
      in Pr::Many then env = pr.envs[0]
      end

      unless rule.is_a?(Rule::Template)
        raise "render: unsupported rule type"
      end

      instance = Alloy.render(env, rule.body)

      if view == instance # base case
        return instance
      end

      return render(ruleset, instance)
    end

    view = Term::Dict.build do |commit|
      view.each_entry do |key, value|
        commit.with(key, render(ruleset, value))
      end
    end

    Term.of(view)
  end

  def uir2ppm(io, input : Term)
    w = h = nil
    fill0 = Color.named("white")

    Term.case(input) do
      matchpi %[{¦ content-w: cw←(%number +i32)}] do
        w = cw.to(Int32)
        continue
      end

      matchpi %[{¦ content-h: ch←(%number +i32)}] do
        h = ch.to(Int32)
        continue
      end

      matchpi %[{¦ fill_}] do
        fill0 = Color.term(fill)
        continue
      end

      otherwise { }
    end

    unless w && h
      abort "unable to determine width and height, please set content-w: and content-h:"
    end

    rr = renderer(input)

    view = Term::Dict.build do |commit|
      input.items.each do |item|
        next if M1.probe?(Renderer::SELECTOR, item)

        commit << item
      end
    end

    rr, template = rr.call(input, Term.of(view | input.pairspart))

    platform = PvgPlatform.new(Disk, Point[w, h])
    screen = PixelRect.new(0, 0, w, h)
    viewer = Viewer.new(screen, Compositor.new, platform)
    uiR = Soma::UIR.rewriter(platform)

    env = Term[uir2ppm: true, frame: 0, page: 0, src: "", ui: false]

    instance, complaints = Alloy.render_with_complaints(env, template)
    unless complaints.empty?
      complaints.each do |complaint|
        puts complaint
      end
      abort "template error"
    end

    instance = rewrite(instance, uiR)

    viewer.show(instance, bg: fill0)

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
      ;; Converts UIR at <fin> to PPM at <fout>.
      (fin_string fout_string)
      WWML
      ) do
        if File.exists?(fout.to(String))
          puts "uir2ppm: #{fout.to(String)} exists, overwrite? (y/)"
          unless gets == "y"
            abort "uir2ppm: cancelled"
          end
        end

        begin
          source = File.read(fin.to(String))
        rescue e : File::Error
          Log.fatal(exception: e)

          abort "uir2ppm: could not read file"
        end

        begin
          uir = ML.terms(source)
        rescue e : ML::SyntaxError
          e.humanize(STDERR, source)

          Log.fatal(exception: e)

          abort "uir2ppm: syntax error"
        end

        puts "uir2ppm: #{fin.to(String)}->#{fout.to(String)}"

        begin
          File.open(fout.to(String), "wb") do |io|
            io.buffer_size = 4096

            uir2ppm(io, uir)
          end
        rescue e : File::Error
          Log.fatal(exception: e)

          abort "uir2ppm: error while opening the output file"
        rescue e : IO::Error
          Log.fatal(exception: e)

          abort "uir2ppm: i/o error"
        end

        puts "uir2ppm: wrote #{fout.to(String)}"
      end

      otherwise do
        puts <<-HELP
        # Synopsis

        uir2ppm is a tiny tool that converts UIR to PPM.

        # Syntax

        #{cases.join("\n\n", &.li(bullet: "", indent: 0))}
        # Example

        $ uir2ppm path/to/uir.wwml path/to/image.ppm 1800 1000
        ...
        $ open path/to/image.ppm
        HELP
      end
    end
  end
end

{% if flag?(:uir2ppm) %}
  UIR2PPM.run
{% end %}
