require "./wirewright"
require "./wirewright/frontend/rack"

help = <<-WWML
USAGE

ww <frontend> [args...]

FRONTENDS

  rack [path/to/your/initial.rack.wwml]
    Wirewright Rack is a general-purpose tool to make Wirewright subsystems
    talk to each other.

    $ ww rack tests/dwuir.rack.wwml
    # Opens the terminal UI for Wirewright Rack, automatically running
    # (rack load "tests/dwuir.rack.wwml") for you.

    $ ww rack
    # Opens the terminal UI for Wirewright Rack, with no rack loaded.

  rack --headless
    Starts a Wirewright Rack server process. Communication is done over
    STDIN/STDOUT.
WWML

args = ARGV.dup

unless frontend = args.shift?
  abort help
end

case {frontend, args.size}
when {"rack", 0..1}
  if args[0]? == "--headless"
    Frontend::Rack::Server.run
    exit
  end

  unless exepath = Process.executable_path
    abort "could not determine executable path for the process"
  end

  server = Process.new(exepath,
    args: ["rack", "--headless"],
    env: {"LOG_LEVEL" => "WARN"},
    input: Process::Redirect::Pipe,
    output: Process::Redirect::Pipe,
    error: Process::Redirect::Inherit,
  )

  Frontend::Rack::Client.run(server, args[0]?.try { |arg| Path[arg] })
else
  abort help
end
