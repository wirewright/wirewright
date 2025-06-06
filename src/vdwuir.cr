require "./wirewright"
require "crsfml"

module VDwUIR
  extend self

  include Ww
  include Ww::Soma

  record FileData, string : String, modt : Time

  @@lock = Sync::Mutex.new
  @@olds = {} of Path => FileData
  @@news = {} of Path => FileData

  MT.spawn do
    loop do
      select
      when timeout(300.milliseconds)
        @@lock.synchronize do
          @@olds.select! do |path, data|
            modt1 = File.info(path).modification_time

            if data.modt == modt1
              true # Keep in olds, file did not change.
            else
              @@news[path] = FileData.new(File.read(path), modt1)

              false # Remove from olds, file changed.
            end
          end
        end
      end
    end
  end

  def rendezvous(path : Path, & : String ->)
    source = @@lock.synchronize do
      if data = @@news.delete(path)
        @@olds[path] = data
        next data.string
      end

      # If it's not in news but in olds, then we detected no change yet.
      return if @@olds.has_key?(path)

      # If it's in neither we need to register a new file.
      @@olds[path] = data = FileData.new(File.read(path), File.info(path).modification_time)

      data.string
    end

    yield source
  end

  def open(path : Path, initial : Term, title0 : String, w0 : Int32, h0 : Int32)
    files = Disk

    buffer = Slice(UInt32).new(w0 * h0)
    screen = DwUIR::PixelRect.new(0, 0, w0, h0)
    viewer = DwUIR::Viewer.new(screen, DwUIR::Compositor.new, DwUIR::PvgPlatform.new(files, DwUIR::Point[w0, h0]))
    texture = SF::Texture.new(w0, h0)

    window = SF::RenderWindow.new(SF::VideoMode.new(w0, h0), title: title0)
    window.framerate_limit = 60

    vars = Term["mouse-x": 0, "mouse-y": 0, "wheel": 0, w: w0, h: h0]
    showing = {initial, vars}
    rejected = nil

    while window.open?
      while event = window.poll_event
        case event
        when SF::Event::Closed then window.close
        when SF::Event::MouseWheelScrolled
          vars = vars.morph({:wheel, vars[:wheel] + event.delta})
        when SF::Event::MouseMoved
          vars = vars.morph({:"mouse-x", event.x}, {:"mouse-y", event.y})
        when SF::Event::Resized
          window.view = SF::View.new(SF.float_rect(0, 0, event.width, event.height))
          vars = vars.morph({:w, event.width}, {:h, event.height})
        end
      end

      candidate, _ = showing

      rendezvous(path) do |source|
        begin
          candidate = ML.terms(source)
        rescue e : ML::SyntaxError
          e.humanize(STDOUT, source)
        end
      end

      if rejected != {candidate, vars} && showing != {candidate, vars}
        instance, complaints = Alloy.render_with_complaints(vars, candidate)

        unless complaints.empty?
          complaints.each do |complaint|
            puts complaint
          end
          rejected = {candidate, vars}
        end

        if complaints.empty?
          Term.case(instance) do
            matchpi %[{¦ bg_⋮ white}] do
              showing = {candidate, vars}

              dmgrects = viewer.show(instance, bg: DwUIR::Color.term(bg))
              dmgrects.each do |rect|
                region = screen.region(rect)
                region.each_pixel_with_coords do |pixel, i, j|
                  buffer[w0 * j + i] = pixel.rgba_le
                end
              end

              texture.update(buffer.to_unsafe.as(UInt8*), w0, h0, 0, 0)
            end
          end
        end
      end

      window.clear(SF::Color::White)
      window.draw(SF::Sprite.new(texture))
      window.display

      Fiber.yield
    end
  end

  def open(path : Path, term : Term)
    Term.case(term) do
      matchpi %[{¦ title_string initial-w: w←(%number 0 < _ <= 4096) initial-h: h←(%number 0 < _ <= 4096)}] do
        instance, complaints = Alloy.render_with_complaints(Term[], term)
        unless complaints.empty?
          complaints.each do |complaint|
            puts complaint
          end

          abort "cannot continue: no fallback state"
        end

        open(path, instance, title.to(String), w.to(Int32), h.to(Int32))
      end

      otherwise do
        abort "invalid toplevel term, expected title, initial-w, initial-h"
      end
    end
  end

  def run
    Term.case(ARGV) do
      matchpi %{("open" file_string)} do
        path = Path[file.to(String)]

        open(path, Term.of(title: "Untitled", "initial-w": 800, "initial-h": 600))
      end

      otherwise do
        puts <<-HELP
        SYNOPSIS

        `vdwuir` lets you render a WwML file containing DwUIR with live-reloading.
        Alloy templating is supported as well.

        The following Alloy variables are exposed:

          - `mouse-x`: the current X coordinate of the mouse
          - `mouse-y`: the current Y coordinate of the mouse
          - `wheel`: the current value of a counter that increments by wheel delta
          - `w`: the current window width
          - `h`: the current window height

        USAGE

          vdwuir open <file>
            Displays <file> with live-reloading on save.

        HELP
      end
    end
  end
end

VDwUIR.run
