require "./wirewright"

module InteractiveRack
  extend self
  include Ww

  def run(args = ARGV) : Nil
    args = args.dup

    single_step = !!(args.delete("--single-step") || args.delete("-s"))

    unless filename = args.shift?
      abort "usage: irack [-s|--single-step] path/to/seed.wwml"
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

    assembler_state = Rack::Assembler.state

    frames = D7.coarse_frames(Rack.clf, seed,
      Rack::Tspace.pass(Rack.clf),
      Rack::Assembler.pass(Rack.clf, Rack.clf, assembler_state),
      Rack.pass(Rack.clf),
    )

    frames.each do |frame|
      puts ML.display(frame)

      if single_step
        gets
      end
    end
  end
end

InteractiveRack.run
