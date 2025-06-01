require "./wirewright"

def crystal_from_conf(conf : Term::Dict, rest : Term::Dict, envvars : Term::Dict, & : Array(String) ->)
  unless envvars.empty?
    env = {} of String => String
    envvars.each_entry do |key, value|
      env[key.to(String)] = value.to(String)
    end
  end

  # Construct the base args.
  args = [] of String
  yield args

  # Append compile-time flags.
  if flags = conf[:flags]?
    flags.each_entry do |flag, _|
      args << "-D#{flag.to(String)}"
    end
  end

  # Append link directories.
  if linkdirs = conf[:linkdirs]?
    args << "--link-flags"

    link_flags = String.build do |io|
      io << "\""
      linkdirs.ee.join(io, " ") do |(linkdir, _)|
        io << "-L" << Dir.current << "/" << linkdir.to(String)
      end
      io << "\""
    end

    args << link_flags
  end

  # Append executable arguments.
  unless rest.empty?
    args << "--"
    rest.items.each do |arg|
      args << arg.to(String)
    end
  end

  STDOUT << "dev: "

  if env
    env.each do |k, v|
      STDOUT << k << "=" << v
    end
    STDOUT << " "
  end

  STDOUT << "crystal "

  args.join(STDOUT, " ")

  STDOUT.puts

  Process.run("crystal", args, env, error: Process::Redirect::Inherit, output: Process::Redirect::Inherit)
end

# We're communicating with past versions of the dev tool. Hence the use
# of the word "protocol".
PROTOCOL = "dev0"

state0 = Term[protocol: PROTOCOL]
if File.exists?("./dev.state.wwml")
  statesrc = File.read("./dev.state.wwml")
  state0 = ML.dict(statesrc)

  unless state0[:protocol]? == Term[PROTOCOL]
    abort "error: corrupted or incompatible state"
  end
end

state = state0

Term.case(ARGV) do
  matchpi %{("p")}, %{("preset")} do
    if preset = state[:preset]?
      puts "exists #{preset.to(String)}"
    else
      puts "missing"
    end
  end

  matchpi %{("g")}, %{("go")} do
    if presets = state[:presets]?
      presets.each_entry_ord do |preset, _|
        puts "- #{preset.to(String)}"
      end
    end
  end

  matchpi %{((%any "g" "go") preset_string)} do
    unless state[:presets, preset]?
      state = state.morph({:presets, preset, Term[]})
    end

    state = state.morph({:preset, preset})
  end

  matchpi %{((%any "g" "go") prototype_string preset_string)} do
    unless state[:presets, preset]?
      unless protoconf = state[:presets, prototype]?
        abort "error: preset #{prototype} does not exist"
      end

      state = state.morph({:presets, preset, protoconf})
    end

    state = state.morph({:preset, preset})
  end

  matchpi %{("rm" preset_string)} do
    unless state[:presets, preset]?
      abort "error: preset #{preset} does not exist"
    end

    state = state.morph({:presets, preset, nil}, {:preset, nil})
  end

  matchpi %{("src" path_string)} do
    unless preset = state[:preset]?
      abort "error: no current preset"
    end

    state = state.morph({:presets, preset, :source, path})
  end

  matchpi %{("log" level_string)} do
    unless preset = state[:preset]?
      abort "error: no current preset"
    end

    state = state.morph({:presets, preset, :runvars, "LOG_LEVEL", level})
  end

  matchpi %{("set" runvar_string value_string)} do
    unless preset = state[:preset]?
      abort "error: no current preset"
    end

    state = state.morph({:presets, preset, :runvars, runvar, value})
  end

  matchpi %{("linkdir" dirpath_string)} do
    unless preset = state[:preset]?
      abort "error: no current preset"
    end

    state = state.morph({:presets, preset, :linkdirs, dirpath, true})
  end

  matchpi %{("flag" flag_string)} do
    unless preset = state[:preset]?
      abort "error: no current preset"
    end

    state = state.morph({:presets, preset, :flags, flag, true})
  end

  matchpi %{("unflag" flag_string)} do
    unless preset = state[:preset]?
      abort "error: no current preset"
    end

    state = state.morph({:presets, preset, :flags, flag, nil})
  end

  matchpi %{((%any "r" "run") (%optional debug mode←(%any debug "--release")) rest_string*)} do
    unless preset = state[:preset]?
      abort "error: no current preset"
    end

    conf = state[:presets, preset].as_d

    unless source = conf[:source]?
      abort "error: preset is missing a source file, use `dev src` to add a source file"
    end

    crystal_from_conf(conf, rest.as_d, envvars: (conf[:runvars]? || Term[]).as_d) do |args|
      args << "run" << source.to(String) << "--progress" << "--error-trace"

      unless mode == Term[:debug]
        args << mode.to(String)
      end
    end
  end

  # NOTE: for some reason building e.g. soma with `--debug` causes a codegen bug.
  # I suppose building without `--release` is not the same as building with `--debug`...
  matchpi(<<-WWML
  ((%any "b" "build")
   (%optional release mode←(%any release "--debug"))
   (%optional auto target←(%any° auto _string)))
  WWML
  ) do
    unless preset = state[:preset]?
      abort "error: no current preset"
    end

    conf = state[:presets, preset].as_d

    unless source = conf[:source]?
      abort "error: preset is missing a source file, use `dev src` to add a source file"
    end

    crystal_from_conf(conf, rest: Term[], envvars: Term[]) do |args|
      args << "build" << source.to(String) << "--progress" << "--error-trace"

      unless target == Term[:auto]
        args << "-o" << target.to(String)
      end

      if mode == Term[:release]
        args << "--release"
      end
    end
  end

  matchpi %{("x")}, %{("explain")} do
    unless preset = state[:preset]?
      abort "error: no current preset"
    end

    conf = state[:presets, preset].as_d

    puts "go #{preset.to(String)}"

    if source = conf[:source]?
      puts "src #{source.to(String)}"
    end

    if flags = conf[:flags]?
      flags.each_entry do |flag, _|
        puts "flag #{flag.to(String)}"
      end
    end

    if linkdirs = conf[:linkdirs]?
      linkdirs.each_entry do |linkdir, _|
        puts "linkdir #{linkdir.to(String)}"
      end
    end

    if runvars = conf[:runvars]?
      runvars.each_entry do |runvar, value|
        if runvar == Term["LOG_LEVEL"]
          puts "log #{value.to(String)}"
        else
          puts "set #{runvar.to(String)} #{value.to(String)}"
        end
      end
    end
  end

  otherwise do
    puts <<-BANNER
    SYNOPSIS

    `dev` is a tiny command line tool for working with the Wirewright repo.

    Its state is stored on the disk, in the dev.state.wwml file.

    USAGE

      dev g|go
        Prints all available presets.

      dev g|go <preset>
        Creates <preset> if it does not exist, and makes it the current.

      dev g|go <prototype> <preset>
        Creates a copy of <prototype> under the name <preset> if it does not
        exist, and makes it the current.

      dev rm <preset>
        Removes <preset>.

      dev p|preset
        Prints the current preset.

      dev src <path/to/file.cr>
        Sets <file> to be the source file.
          $ dev src soma6.cr

      dev log <log level>
        Sets the log level used for `run`.
          $ dev log debug
          $ dev log warn

      dev set <var> <value>
        Sets a runtime environment variable during `run`.
          $ dev set CRYSTAL_WORKERS 16

      dev flag <comptime flag>
        Adds a compile-time flag.
          $ dev flag preview_mt
          $ dev flag newsoma

      dev unflag <comptime flag>
        Removes a compile-time flag.
          $ dev unflag newsoma

      dev linkdir <dirpath>
        Adds a link flag: -L/path/to/working-dir/<dirpath>

      dev r|run [--release] [...args]
        Builds and runs the source file in debug mode. The `--release` switch can
        be used to build in release mode instead.

      dev b|build [--debug] [executable-name]
        Builds the source file in release mode. The `--debug` switch can be used
        to build in debug mode instead.

      dev x|explain
        Lists the commands to re-create the current preset. This is most useful
        as a form of "ls" -- if you want to make sure your changes were added to
        the state.

    EXAMPLE

      $ dev go mt foo
      $ dev log debug
      $ dev src foo.cr
      $ dev run
      ...
      # Make changes
      $ dev build
      ...
      $ ./foo
      ...
      $ dev rm foo

    BANNER
  end
end

unless state0 == state
  File.open("./dev.state.wwml", "w") do |io|
    io.puts ";; NOTE: this file stores the state for the `dev` tool. Prefer to use"
    io.puts ";; the `dev` tool to modify this file."
    io.puts

    state.each_entry_ord do |key, value|
      ML.compact(io, key)
      io.puts ":"
      ML.display(io, value)
      io.puts
    end
  end
end
