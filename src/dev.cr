require "./wirewright"

module DevTool
  extend self
  include Ww

  # :nodoc:
  DEVPATH = Path[Dir.current]

  # :nodoc:
  SRC_BASE = "base.dev.wwml"

  # :nodoc:
  SRC_ACTIVE = "active.dev.wwml"

  # :nodoc:
  #
  # We're communicating with the past versions of the dev tool, hence
  # the name "protocol".
  PROTOCOL = "dev 0"

  # :nodoc:
  HELP = <<-HELP
  SYNOPSIS

  `dev` is a tiny command line tool for working with the Wirewright repo.

  Its state is stored on the disk, in two files:

    * #{SRC_ACTIVE}: gitignored current state. This is the file you make
      changes to with this tool.

    * #{SRC_BASE}: git-tracked base state. It is initially copied to produce
      dev.active.wwml. You can sync individual presets from `dev.active.wwml`
      to `#{SRC_BASE}` using the `dev sync` command.

  USAGE

    dev g|go
      Prints all available presets.

    dev g|go <preset>
      Creates <preset> if it does not exist, and makes it the current.

    dev g|go <prototype> <preset>
      Creates a copy of <prototype> under the name <preset> if it does not
      exist, and makes it the current.

    dev rm <preset>
      Removes <preset> from active and base states.

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

    dev sync
      Writes active preset to base state.

    dev r|run [--release] [...args]
      Builds and runs the source file in debug mode. The `--release` switch can
      be used to build in release mode instead.

    dev b|build [--debug] [--native] [executable-name]
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

  HELP

  private def note(msg : String) : Nil
    print "dev: ".colorize.bold.green
    puts msg
  end

  private def fatal(msg : String) : NoReturn
    print "dev: ".colorize.bold.red
    abort msg
  end

  private def display(io, state : Term::Dict)
    io.puts ";; NOTE: this file is part of the state for the `dev` tool. Prefer to use"
    io.puts ";; the `dev` tool to modify this file."
    io.puts

    state.each_entry_ord do |key, value|
      ML.compact(io, key)
      io.puts ":"
      ML.display(io, value)
      io.puts
    end
  end

  # Yields the current state of the dev tool. Writes to disk the state that
  # the block returns (assumed to be modified current state).
  private def state(& : Term::Dict -> Term::Dict) : Nil
    begin
      basesrc = File.read(DEVPATH / SRC_BASE)

      begin
        base = ML.dict(basesrc, filename: (DEVPATH / SRC_BASE).to_s)
      rescue e : ML::SyntaxError
        e.humanize(STDERR)

        fatal "invalid or malformed state: #{DEVPATH / SRC_BASE}"
      end
    rescue File::Error
    end

    begin
      activesrc = File.read(DEVPATH / SRC_ACTIVE)

      begin
        active = ML.dict(activesrc, filename: (DEVPATH / SRC_ACTIVE).to_s)
      rescue e : ML::SyntaxError
        e.humanize(STDERR)

        fatal "invalid or malformed state: #{DEVPATH / SRC_ACTIVE}"
      end
    rescue File::Error
    end

    # Initialize active from base.
    if base && active.nil?
      active = base
    end

    base ||= Term[protocol: PROTOCOL]
    active ||= Term[protocol: PROTOCOL]

    unless {base[:protocol]?, active[:protocol]?}.all?(Term[PROTOCOL])
      fatal "incompatible protocol: expected #{PROTOCOL}"
    end

    state0 = Term[base: base, active: active]
    state1 = yield state0

    begin
      base1 = state1[:base]?.try(&.as_d?)

      unless state0[:base]? == base1
        if base1
          File.open(DEVPATH / SRC_BASE, "w") { |io| display(io, base1) }
        else
          File.delete(DEVPATH / SRC_BASE)
        end
      end

      active1 = state1[:active]?.try(&.as_d?)

      unless state0[:active]? == active1
        if active1
          File.open(DEVPATH / SRC_ACTIVE, "w") { |io| display(io, active1) }
        else
          File.delete(DEVPATH / SRC_ACTIVE)
        end
      end
    rescue e : File::Error
      fatal "error syncing changes to disk: #{e.message}"
    end
  end

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

    command = String.build do |io|
      if env
        env.join(io, " ") do |(k, v)|
          io << k << "=" << v
        end
        io << " "
      end

      io << "crystal "

      args.join(io, " ")
    end

    note command

    Process.run("crystal", args, env, input: Process::Redirect::Inherit, error: Process::Redirect::Inherit, output: Process::Redirect::Inherit)
  end

  private def with_active_preset(state : Term::Dict, & : Term ->)
    unless preset = state[:active, :preset]?
      fatal "no active preset, use `dev go` to set"
    end

    yield preset
  end

  private def with_active_preset_and_conf(state : Term::Dict, & : Term, Term::Dict ->)
    with_active_preset(state) do |preset|
      unless (conf = state[:active, :presets, preset]?) && conf.type.dict?
        fatal "malformed preset"
      end

      yield preset, conf.unsafe_as_d
    end
  end

  def advance(state0 : Term::Dict, command : Term) : Term::Dict
    state = state0

    Term.case(command) do
      matchpi %{((%any "p" "preset"))} do
        if preset = state[:active, :preset]?
          puts preset.to(String)
        end
      end

      matchpi %{((%any "g" "go"))} do
        active = state[:active, :preset]?

        if presets = state[:active, :presets]?
          presets.each_entry_ord do |preset, _|
            if active == preset
              print "> "
            else
              print "  "
            end

            puts preset.to(String)
          end
        end
      end

      matchpi %{((%any "g" "go") preset_string)} do
        unless state[:active, :presets, preset]?
          state = Term.morph(state, {:active, :presets, preset, Term[]})
        end

        state = Term.morph(state, {:active, :preset, preset})
      end

      matchpi %{((%any "g" "go") prototype_string preset_string)} do
        unless state[:active, :presets, preset]?
          unless conf = state[:active, :presets, prototype]?
            fatal "prototype #{prototype} does not exist"
          end

          state = Term.morph(state, {:active, :presets, preset, conf})
        end

        state = Term.morph(state, {:active, :preset, preset})
      end

      matchpi %{("rm" preset_string)} do
        unless state[:active, :presets, preset]?
          fatal "preset #{preset} does not exist"
        end

        # Exit preset if it's the one being removed.
        if state[:active, :preset]? == preset
          state = Term.morph(state, {:active, :preset, nil})
        end

        state = Term.morph(state,
          {:active, :presets, preset, nil},
          {:base, :presets, preset, nil},
        )
      end

      matchpi %{("src" path_string)} do
        with_active_preset(state) do |preset|
          state = Term.morph(state, {:active, :presets, preset, :source, path})
        end
      end

      matchpi %{("log" level_string)} do
        with_active_preset(state) do |preset|
          state = Term.morph(state, {:active, :presets, preset, :runvars, "LOG_LEVEL", level})
        end
      end

      matchpi %{("set" runvar_string value_string)} do
        with_active_preset(state) do |preset|
          state = Term.morph(state, {:active, :presets, preset, :runvars, runvar, value})
        end
      end

      matchpi %{("linkdir" dirpath_string)} do
        with_active_preset(state) do |preset|
          state = Term.morph(state, {:active, :presets, preset, :linkdirs, dirpath, true})
        end
      end

      matchpi %{("flag" flag_string)} do
        with_active_preset(state) do |preset|
          state = Term.morph(state, {:active, :presets, preset, :flags, flag, true})
        end
      end

      matchpi %{("unflag" flag_string)} do
        with_active_preset(state) do |preset|
          state = Term.morph(state, {:active, :presets, preset, :flags, flag, nil})
        end
      end

      matchpi %{("x")}, %{("explain")} do
        with_active_preset_and_conf(state) do |preset, conf|
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
      end

      matchpi %{("sync")} do
        with_active_preset(state) do |preset|
          state = Term.morph(state, {:base, :presets, preset, state[:active, :presets, preset]?})
        end
      end

      matchpi(<<-WWML
      ((%any "r" "run")
       (%optional unset mode←(%any unset "--release"))
       (%plural/max rest type: _string))
      WWML
      ) do |rest|
        with_active_preset_and_conf(state) do |preset, conf|
          unless source = conf[:source]?
            fatal "preset is missing a source file, use `dev src` to add a source file"
          end

          crystal_from_conf(conf, rest.as_d, envvars: (conf[:runvars]? || Term[]).as_d) do |args|
            args << "run" << source.to(String) << "--progress" << "--error-trace"

            unless mode == Term[:unset]
              args << mode.to(String)
            end
          end
        end
      end

      matchp(<<-WWML
      ((%any "b" "build")
       (%many options_ (%any° flag←(%any "--debug" "--native") target_string) min: 0))
      WWML
      ) do |options|
        with_active_preset_and_conf(state) do |preset, conf|
          unless source = conf[:source]?
            fatal "preset is missing a source file, use `dev src` to add a source file"
          end

          crystal_from_conf(conf, rest: Term[], envvars: Term[]) do |args|
            args << "build" << source.to(String) << "--progress" << "--error-trace"

            release = true

            options.items.each do |option|
              if target = option[:target]?
                args << "-o" << target.to(String)
              end

              # NOTE: for some reason building e.g. soma with `--debug` causes a Crystal codegen bug.
              # I suppose building without `--release` is not the same as building with `--debug`...
              if option == Term[flag: "--debug"]
                release = false
              end

              if option == Term[flag: "--native"]
                args << "--mcpu" << "native"
              end
            end

            args << "--release" if release
          end
        end
      end

      otherwise do
        puts HELP
      end
    end

    state
  end

  def run : Nil
    state do |state|
      advance(state, Term.of(ARGV))
    end
  end
end

DevTool.run
