# Racks are networks of devices connected by hyperedges.
#
# Each device has a piece of immutable state. The rack is used to evolve
# a *workspace* and a *state map*, the latter being a map of immutable
# device states. Each time step results in a new, modified workspace, and
# a new, modified state map.
#
# Immutable state is used to maintain Crystal objects, mostly for cache.
#
# Workspaces are used for inference, and as a communication medium for
# the rack.
#
# Compared to something like D7, an important assumption with racks is
# that racks must not change throughout evolution and inference. They
# remain a static asset.
#
# Devices copying data from the workspace to the states map is known as
# *exchange*, and is done once after each inference loop. During inference,
# the rack tells/learns as much as it can; saturating the workspace with
# info if possible. Finally, during exchange, each device modifies its Crystal-
# side state based on the workspace; effectively, copying info from the workspace
# to the state.
#
# Racks either interact with, or are observed by *agents*.
#
# The inference- and-exhcange process may interact with Crystal code
# through `Agent::Peer`, which has access to both of states and
# the workspace.
#
# *Narrator* agents are pure observers when it comes to the environment.
# All they see is state transitions; they are also given some info about
# rack contents and the device(s) involved. Narrators may maintain side
# state, which lets them e.g. manage windows, draw images, print messages
# and so on based on effectively assertions made in device state. Narrators
# are triggered on each and every state transition.
#
# Racks are designed to be the primary interface to Wirewright-as-a-system;
# so, a *general-purpose* interface. There's also µsoma which is a GUI to
# a subset of Wirewright.
module Ww::Rack
  extend self

  private alias DwUIR = Soma::DwUIR

  # Represents the address of a device in the rack. We use indices of devices
  # in the rack as device addresses.
  alias DeviceAddr = Int32

  # Maps device addresses to the corresponding Crystal-side state, if any.
  #
  # See also: `DeviceAddr`, `State`.
  struct StateMap
    # Returns the family id of this state map.
    #
    # Family id is kept unchanged after `assoc`s and `map`s.
    getter family : UInt32

    # :nodoc:
    def initialize(@family : UInt32, @map : Pf::Map(Int32, State::Any))
    end

    @@counter = Atomic(UInt32).new(0u32)

    # :nodoc:
    def self.build(&)
      map = Pf::Map(Int32, State::Any).transaction { |commit| yield commit }

      new(@@counter.add(1, :relaxed), map)
    end

    # Returns the state of a device with the given address *addr*. Returns
    # `nil` if such a device does not exist.
    def []?(addr : DeviceAddr) : State::Any?
      @map[addr]?
    end

    # Returns the state of a device with the given address *addr*. Raises
    # `KeyError` if such a device does not exist.
    def [](addr : DeviceAddr) : State::Any
      self[addr]? || raise KeyError.new
    end

    # Yields addresses and device states in this map.
    def each(& : DeviceAddr, State::Any ->) : Nil
      @map.each { |addr, state| yield addr, state }
    end

    # Yields only addresses and device states of type `T`.
    def each(cls : T.class, & : DeviceAddr, T ->) : Nil forall T
      each do |addr, state|
        next unless state.is_a?(T)
        yield addr, state
      end
    end

    # Returns an iterator over addresses and device states in this map.
    def each : Iterator({DeviceAddr, State::Any})
      (0...@map.size).each.map { |n| @map.nth?(n) || raise IndexError.new }
    end

    # Transforms device states using the block. The block can return `nil`
    # for cheap skip/ignore. Returns a modified copy of this map.
    def map(& : DeviceAddr, State::Any -> State::Any?) : StateMap
      map1 = @map.transaction do |commit|
        @map.each do |addr, state0|
          next unless state1 = yield addr, state0

          commit.assoc(addr, state1)
        end
      end

      StateMap.new(@family, map1)
    end

    # Transforms only device states of type `T` using the block. The block can
    # return `nil` for cheap skip/ignore. Returns a modified copy of this map.
    def map(cls : T.class, & : DeviceAddr, T -> State::Any?) : StateMap forall T
      map do |addr, state0|
        next unless state0.is_a?(T)
        yield addr, state0
      end
    end

    # Sets the state of a device with the given address *addr* to *state*.
    # Returns a modified copy of this map.
    #
    # Raises `KeyError` if a device with the given *addr* does not exist.
    def assoc(addr : DeviceAddr, state : State::Any) : StateMap
      map1 = @map.assoc(addr, state)

      if map1.size > @map.size
        raise KeyError.new("device address #{addr} is invalid")
      end

      StateMap.new(@family, map1)
    end
  end

  # *Devices* act as bridges between a *workspace* and Crystal-side *device states*.
  # Most devices have a corresponding `State`. During *exchange*, devices are able
  # to move data from the active workspace (subject to e.g. inference rules) to the
  # active Crystal-readable and writable `StateMap` -- and vice versa, since during
  # *exchange*, devices may also copy data from the state map back into the active
  # workspace if such a demand is found.
  #
  # Devices may also point each other to the values of their state. They
  # usually do that by computing to `(handle device-addr_)`. This is useful
  # in cases like caching a Microfold theme (which is a Crystal object
  # that is reasonably expensive to construct frequently).
  #
  # Sometimes device states are used to "share" some rack-global object, such
  # as process arguments.
  #
  # NOTE: states are immutable as is a `StateMap`. Instead of advancing individual
  # states or mutating them, we advance entire `StateMap`s instead. This does not
  # mean that member objects are required to be immutable; however, you are highly
  # recommended to avoid mutation for ease of reasoning. Mutation should be kept
  # well outside device states; and must occur through observation of such states
  # and consequent feedback through `StateMap` advancement.
  module State
    alias Any = Source::Any | Cell::Any | Ruleset::Any | MuTheme::Any | MuRender::Any | Ticker::Any | UIR::Any | Window::Any | Console::Any | Image::Any | ProcessArgs::Any | ProcessEnv::Any | ML::Any | Alloy::Any | Log::Any

    # Transient states have their lifetime equal to the lifetime of the active
    # workspace. When the active workspace is expended, all transient states
    # will be converted into their stable forms. This is a shorthand/convenience
    # to simplify automatic cleanup of certain states after the current workspace
    # retires. Transients underpin behavior such as logging (e.g. `log`, `note` devices).
    #
    # Without transients and their decay, one would only see different messages,
    # which may not be desired if one is using e.g. a ticker printing the same message.
    # Thanks to transient state decay, after each workspace, the message is effectively
    # forgotten; and observers may print again.
    module Transient
      # Returns the stable form of this state.
      abstract def stable
    end

    # :nodoc:
    #
    # Shorthand. Unfortunately macros cannot do this due to the use of `alias`;
    # Crystal appears to assess aliases during macro expansion which makes multi-level
    # macros that generate types incapable of participating in aliases/as alias union
    # member types.
    module TransientPrior(Stable)
      include Transient

      def stable
        Stable.new
      end
    end

    # Associated with an `ml` device.
    module ML
      alias Any = Ok | Err

      record Ok
      record Err, exception : ::Ww::ML::SyntaxError { include TransientPrior(Ok) }
    end

    # Associated with an `alloy/template` or `alloy/view` device.
    module Alloy
      alias Any = Ok | TemplateIssues | ViewIssues

      record Ok

      record TemplateIssues, vars : Term::Dict, template : Term, issues : Array(Issue::Backtrace) do
        include TransientPrior(Ok)
      end

      record ViewIssues, ruleset : ::Ruleset, view : Term, issues : Array(Issue::Backtrace) do
        include TransientPrior(Ok)
      end
    end

    # Associated with an `src` device.
    module Source
      alias Any = FileLoaded | FilePending | BadQuery | None

      record None

      record FileLoaded, path : Path, dst : Term, content : String, instant : Time
      record FilePending, path : Path, dst : Term

      record BadQuery, query : Term { include TransientPrior(None) }
    end

    # Associated with a `latest` device.
    module Cell
      alias Any = Some | None

      record Some, value : Term
      record None
    end

    # Associated with a `m1/ruleset` device.
    module Ruleset
      alias Any = Some | None

      record Some, rules : ::Ruleset
      record None
    end

    # Associated with a `microfold/theme` device.
    module MuTheme
      alias Any = Some | None

      record Some, theme : Soma::Microfold::Theme
      record None
    end

    # Associated with a `microfold` device.
    module MuRender
      alias Any = Ok | Issues

      record Ok
      record Issues, node : Term, issues : Array(Issue::Backtrace) do
        include TransientPrior(Ok)
      end
    end

    # Associated with a `ticker` device.
    module Ticker
      alias Any = Running | NotRunning
      alias NotRunning = BadPeriodSpec | None

      record None, id : Int32
      record Running, id : Int32, period : Time::Span, query : Term::Dict
      record BadPeriodSpec, id : Int32, spec : Term do
        include Transient

        def stable
          None.new(id)
        end
      end
    end

    # Associated with a `uir` device.
    #
    # TODO: This is a temporary hack until rewriter circuits are there.
    module UIR
      alias Any = Pending | None

      record Pending, uir : Term, dst : Term
      record None
    end

    # Associated with a `dwuir/window` device.
    module Window
      alias Any = Open | NotOpen
      alias NotOpen = Closed | None

      record Open, id : Int32, spec : Term, events : Term?
      record Closed, id : Int32, spec : Term, events : Term?
      record None, id : Int32, events : Term?
    end

    # Associated with a `dwuir/console` device.
    module Console
      alias Any = Open | NotOpen
      alias NotOpen = Closed | None

      record Open, id : Int32, spec : Term, events : Term?
      record Closed, id : Int32, spec : Term, events : Term?
      record None, id : Int32, events : Term?
    end

    # Associated with a `dwuir/image` device.
    module Image
      alias Any = File | InMemory | BadSpec | BadTarget | None

      record None

      record File, version : UInt32, conf : DwUIR::ShowConf, path : Path
      record InMemory, version : UInt32, conf : DwUIR::ShowConf, id : Term

      record BadSpec, spec : Term { include TransientPrior(None) }
      record BadTarget, target : Term { include TransientPrior(None) }
    end

    # Associated with an `args` device.
    module ProcessArgs
      alias Any = None | Some

      record None
      record Some, args : Array(String)
    end

    # Associated with an `env` device.
    module ProcessEnv
      alias Any = None | Some

      record None
      record Some, env : Hash(String, String)
    end

    # Associated with a logging device (e.g. `log`, `note`, `err`).
    module Log
      alias Any = NoMessage | Message

      enum Style
        Comment
        Note
        Warning
        Error
      end

      record NoMessage
      record Message, style : Style, term : Term { include TransientPrior(NoMessage) }
    end
  end

  # Calls *fn* with keypaths and terms of edge literals found in *device*.
  #
  # Apart from ignoring the value of a `const` device, nothing smart is done
  # here; and edges are found on a purely literal basis (i.e. `(edge _)`).
  #
  # NOTE: the keypath array is reused. Make sure to make a copy if you want
  # to store it.
  private def each_edge(device : Term, &fn : ThinArray(Term), Term ->) : Nil
    # Special-case (const @_ _) so that we don't descend into its value.
    Term.matchpi?(device, %{[const @edge_ _]}) do
      fn.call(ThinArray{Term.of(1)}, edge)
      return
    end

    Term.each_keypath_and_node(device) do |keypath, node|
      unless ML.edge?(node)
        next true # descend
      end

      fn.call(keypath, node)

      false # no descend
    end
  end

  # Replaces edges found in *device* using *fn*. See `each_edge`.
  private def map_edge(device : Term, &fn : Term -> Term) : Term
    unless device0 = device.as_d?
      return device
    end

    # We have to do this variable dance to make sure only non-nil is
    # captured by the each_edge closure.
    device1 = device0
    each_edge(device) do |keypath, edge|
      device1 = device1.where(keypath.to_readonly_slice, eq: fn.call(edge))
    end

    Term.of(device1)
  end

  private alias Scope = ThinArray(Int32)

  # Resolves `module`s in *rack* by annotating edges with scope ids, and
  # connecting edges from different scopes with a `link` device. Returns
  # a modified copy of *rack*.
  #
  # For example, the following rack:
  #
  # ```wwml
  # (module {@x: @x-outer}
  #   (const @x "X defined inside")
  #   (note @x-outer)
  #   (note @x))
  #
  # (const @x "X defined outside")
  # ```
  #
  # ... flattens into:
  #
  # ```wwml
  # (link @(0 x-outer) @(1 x))
  # (const @(1 x) "X defined inside")
  # (note @(1 x-outer))
  # (note @(1 x))
  # (const @(0 x) "X defined outside")
  # ```
  #
  # Note how all edges are now tagged with the scope id; and explicit `link`s
  # are present between equal but differently scoped edges. Note also, how
  # the result of flattening is always a flat list of devices; hence the name.
  private def flatten(rack rack0 : Term) : Term
    scope = Scope.new
    scopes = {} of Scope => Int32

    rack1 = Term::Dict.build do |commit|
      emit = ->(device : Term) { commit << device }
      resolve = ->(scope : ThinArray(Int32), edge : Term) do
        Term.case(edge) do
          matchpi %{(%'edge name_)} do
            unless id = scopes[scope]?
              # `scopes` is append-only so .size is an OK thing to do to get unique ids.
              id = scopes.size
              scopes[scope.dup] = id
            end

            Term.of(:edge, {id, name})
          end

          otherwise { edge }
        end
      end

      rack0.items.each_with_index do |device, device_id|
        flatten(scope, device, device_id, resolve, emit)
      end
    end

    Term.of(rack1)
  end

  private def flatten(scope : Scope, device : Term, device_id : Int32, resolve, emit) : Nil
    Term.case(device) do
      # |@ rack.device.module
      #
      # |@block
      # A module introduces a new, fully isolated scope. Bindings can be "imported" from
      # an outer scope using *bindings*.
      # |@endblock
      #
      # |@key bindings -- Edge-to-edge associations from the module's exterior
      # to its interior scope.
      #
      # |@key children rack.device -- Devices that should be scoped under the module.
      matchpi %{[module bindings_dict children_*]} do
        exterior = [] of Term
        interior = [] of Term

        bindings.each_entry do |key, value|
          unless ML.edge?(key) && ML.edge?(value)
            Log.warn { "ignoring invalid key-value pair `#{ML.compact(key)}: #{ML.compact(value)}` in module: expected `@_: @_`" }
            next
          end

          # Resolve value (the right-hand side of the binding) in the outer scope,
          # producing an exterior edge.
          exterior << resolve.call(scope, value)

          scope.push(device_id) do
            # Resolve key (the left-hand side of the binding) in the inner scope,
            # producing an interior edge.
            interior << resolve.call(scope, key)
          end
        end

        # Link exterior and interior edges using the `link` device.
        exterior.zip(interior) do |a, b|
          emit.call(Term.of(:link, a, b))
        end

        # Proceed into the module's children.
        scope.push(device_id) do
          children.items.each_with_index do |child, child_id|
            flatten(scope, child, child_id, resolve, emit)
          end
        end
      end

      otherwise do
        device1 = map_edge(device) { |edge| resolve.call(scope, edge) }

        emit.call(device1)
      end
    end
  end

  # Returns an instantiated copy of *rack* and the inference ruleset
  # extracted from *basis*.
  #
  # *basis* is assumed to be a document dict, containing at least two sections:
  # `inference` with inference rules selectable using *selector*; and `shorthands`
  # with shorthand rules selectable using *selector*.
  private def instance(rack : Term, basis : Term::Dict, *, selector : Term = ML.term(%{[rule pattern_ template_]})) : {Term, Ruleset}
    rack, inference, shorthands = separate(rack, selector, basis)

    {Alloy.render(shorthands, rack), inference}
  end

  # :nodoc:
  #
  # Optimize inference rule lookup by keying the inference ruleset on
  # the device's name. Only rules mentioning the current device's name
  # are going to be tried.
  module InferenceKey
    extend PatternSet::Key

    def self.of_pattern?(pattern : Term, normp : Term) : Term?
      Term.case(pattern) do
        matchpi %{((%'%partition (name_symbol _*) _) _)} do
          name
        end

        otherwise do
          Log.debug do
            "no key on rule pattern #{ML.compact(pattern)}: expected pattern of the form `((%'%partition (name_symbol _*) _) _)`"
          end
        end
      end
    end

    def self.of_matchee?(matchee : Term) : Term?
      return unless dict = matchee.as_d?
      return unless dict.size == 2

      fst = dict[0]
      return unless fst = fst.as_d?
      return unless fst.itemsize > 0

      fst[0]?
    end
  end

  private def separate(rack : Term, selector : Term, basis : Term::Dict) : {Term, Ruleset, Ruleset}
    Term.matchpi(basis, %[{¦ inference⋮ {} shorthands⋮ {}}]) do |shorthands|
      # Extend shorthands by appending whatever rules we find in
      # the rack itself.
      shorthands = shorthands.transaction do |commit|
        commit.selected(rack.items) { |item| M1.probe?(selector, item) }
      end

      extras = Term::Dict.build do |commit|
        commit.rejected(rack.items) { |item| M1.probe?(selector, item) }
      end

      {Term.of(extras),
       Ruleset.select(selector, Term.of(inference), key: InferenceKey),
       Ruleset.select(selector, Term.of(shorthands))}
    end
  end

  # A rack index is a data structure built from a rack term, and designed
  # to query it efficiently.
  struct Index
    def initialize(@devices : Slice(Term), @clusters : Hash(Term, Slice(Term)))
    end

    # Returns the device at the given device address *addr*.
    def device?(addr : DeviceAddr) : Term?
      @devices[addr]
    end

    # Returns the device at the given device address *addr*.
    def device(addr : DeviceAddr) : Term
      device?(addr) || raise KeyError.new("invalid device address #{addr}")
    end

    # Yields devices and their device addresses.
    def each_device_with_addr(& : Term, DeviceAddr ->) : Nil
      @devices.each_with_index { |device, addr| yield device, addr }
    end

    # Yields devices connected to the given *edge*.
    def each_device_at_edge(edge : Term, & : Term ->) : Nil
      return unless cluster = @clusters[edge]?

      cluster.each do |device|
        yield device
      end
    end

    # Yields all edges referenced in the rack.
    def each_edge(& : Term ->) : Nil
      @clusters.each { |edge, _| yield edge }
    end
  end

  # Constructs an index for the given *rack*.
  private def index(rack : Term) : Index
    unless dict = rack.as_d?
      raise ArgumentError.new("invalid rack term, expected a dict")
    end

    # Even though we can access/iterate over *rack*'s items directly this
    # won't be as efficient as slice iteration/access even after extreme
    # optimizations; which we don't do at the moment.
    devices = Slice(Term).new(dict.itemsize) { |index| dict[index] }

    clusters = {} of Term => Slice(Term)

    rack.items.each do |device|
      each_edge(device) do |_, edge|
        # NOTE: We assume here that clusters are small enough it's cheap to copy
        # the slice on each append.
        clusters[edge] = (clusters[edge]? || Slice(Term).empty).append(device)
      end
    end

    Index.new(devices, clusters)
  end

  # Performs the initial inspection of *rack*: constructs the corresponding
  # `State` objects for devices that need them, and distributes references
  # to parts of the running context (provided in the named arguments) among
  # the devices. Returns the resulting state map and an initial workspace to
  # send to the rack to "ignite" it (may be empty, in which case "ignition"
  # must necessarily be initiated from the outside).
  private def boot(index : Index) : {StateMap, Term::Dict}
    query = Term[]
    states = StateMap.build do |commit|
      index.each_device_with_addr do |device, device_addr|
        Term.case(device) do
          matchpi %{[src @srcs_ @contents_]} do
            query = query.with(srcs, :"?")
            commit.assoc(device_addr, State::Source::None.new)
          end

          matchpi %{[ml @_ @_]} do
            commit.assoc(device_addr, State::ML::Ok.new)
          end

          matchpi %{[dwuir/window @_ @events_]} do
            commit.assoc(device_addr, State::Window::None.new(device_addr, events))
          end

          matchpi %{[dwuir/window @_]} do
            commit.assoc(device_addr, State::Window::None.new(device_addr, events: nil))
          end

          matchpi %{[dwuir/console @_ @events_]} do
            commit.assoc(device_addr, State::Console::None.new(device_addr, events))
          end

          matchpi %{[dwuir/console @_]} do
            commit.assoc(device_addr, State::Console::None.new(device_addr, events: nil))
          end

          matchpi %{[dwuir/image @_ @_]} do
            commit.assoc(device_addr, State::Image::None.new)
          end

          matchpi %{[microfold/theme (@_ @_) @_]} do
            commit.assoc(device_addr, State::MuTheme::None.new)
          end

          matchpi %{[microfold (@_ @_) @_]} do
            commit.assoc(device_addr, State::MuRender::Ok.new)
          end

          matchpi %{[m1/ruleset (@_ @_) @_]} do
            commit.assoc(device_addr, State::Ruleset::None.new)
          end

          matchpi %{[alloy/template (@_ @_) @_]}, %{[alloy/view (@_ @_) @_]} do
            commit.assoc(device_addr, State::Alloy::Ok.new)
          end

          matchpi %{[latest @_ @_]} do
            commit.assoc(device_addr, State::Cell::None.new)
          end

          matchpi %{[uir @_ @_]} do
            commit.assoc(device_addr, State::UIR::None.new)
          end

          matchpi %{[ticker @specs_ @_]} do
            query = query.with(specs, :"?")
            commit.assoc(device_addr, State::Ticker::None.new(device_addr))
          end

          # |@ rack.device.args
          #
          # |@block
          # Gives access to the argument list of the rack's process.
          # |@endblock
          #
          # |@key arglist -- An edge to receive itemsonly dicts of arguments;
          # each argument in the dict is a string.
          matchpi %{[args @arglist_]} do
            commit.assoc(device_addr, State::ProcessArgs::None.new)
          end

          # |@ rack.device.env
          #
          # |@block
          # Gives access to the environment variables of the rack's process.
          # |@endblock
          #
          # |@key envvars -- An edge to receive a pairsonly dict containing environment
          # variables; each key and value in the map is a string.
          matchpi %{[env @envvars_]} do
            commit.assoc(device_addr, State::ProcessEnv::None.new)
          end

          matchpi %{[log @edge_]}, %{[note @edge_]}, %{[err @edge_]}, %{[warn @edge_]} do
            commit.assoc(device_addr, State::Log::NoMessage.new)
          end

          otherwise { }
        end
      end
    end

    {states, query}
  end

  # Runs *inference* on a single device, letting it contribute to *workspace*.
  private def propagate?(device : Term, workspace : Term::Dict, inference : Ruleset) : Term?
    # FIXME: this is a hack. combined must be implemented with M1 patterns just
    # like the other devices, but the parts of M1 this will need (%items, %entries)
    # are very buggy right now.
    Term.case({device, workspace}) do
      # If any edge is known, then sink must be known.
      givenpi %{[combined edges_dict @sink_] (%-value sink)} do
        edges.each_entry do |_, edge|
          next unless value = workspace[edge]?

          Term.matchpi?(value, %{(currently _)}) do
            return Term.of(:proposal, sink, :"?")
          end
        end
      end

      # If sink is unknown, then all edges must be known.
      givenpi %{[combined edges_dict @sink_] (%value sink ?)} do
        map = Term[]

        edges.each_entry do |key, edge|
          unless value = workspace[edge]?
            return Term.of(:proposal, edge, :"?")
          end

          Term.matchpi?(value, %{(currently value_)}) do
            map = map.with(key, value)
          end
        end

        return edges.size == map.size ? Term.of(:proposal, sink, {:currently, map}) : nil
      end

      otherwise { }
    end

    return unless response = inference.call?(Term.of(device, workspace))

    pr, rule = response

    unless pr.is_a?(Pr::One) && rule.is_a?(Rule::Template)
      Log.error { "expected a response from a template rule with a pattern that emits a single match env, but something else responded instead" }
      return
    end

    Alloy.render(pr.env, rule.body)
  end

  # Parses a period specification *spec*.
  private def period?(spec : Term) : Time::Span?
    Term.case(spec) do
      # |@ rack.period.seconds
      #
      # |@block
      # Use one of the following forms to specify a period in seconds.
      # |@endblock
      matchpi %{(±n second)}, %{(±n seconds)}, %{(±n s)} do
        n.to(Float32).seconds
      end

      # |@ rack.period.milliseconds
      #
      # |@block
      # Use one of the following forms to specify a period in milliseconds.
      # |@endblock
      matchpi %{(±n millisecond)}, %{(±n milliseconds)}, %{(±n ms)} do
        n.to(Float32).milliseconds
      end

      otherwise { }
    end
  end

  # :nodoc:
  alias ExchangeSeenSet = Set({DeviceAddr, UInt32})

  # :nodoc:
  struct ExchangeSession
    include Term::CaseSession

    def initialize(@seen : ExchangeSeenSet, @device_addr : DeviceAddr)
    end

    def enter?(pattern_id : UInt32) : Bool
      !{@device_addr, pattern_id}.in?(@seen)
    end

    def matched(pattern_id : UInt32) : Nil
      @seen << {@device_addr, pattern_id}
    end
  end

  # During exchange, data is copied from *workspace* to *states*, and vice versa,
  # based on demands in *workspace*.
  private def exchange(workspace0 : Term::Dict, index : Index, states states0 : StateMap, seen : ExchangeSeenSet) : {Term::Dict, StateMap}
    states1 = states0
    workspace1 = workspace0

    index.each_device_with_addr do |device, device_addr|
      next unless state0 = states0[device_addr]?

      state1 = state0

      Term.case({device, workspace0}, session: ExchangeSession.new(seen, device_addr)) do
        {% for conf, index in { {"(%optional term term)", :term}, {"terms", :terms}, {"document", :document} } %}
          {% entity, method = conf %}

          givenpi %{(ml @srcs_ @terms_ ⍊ entity: {{entity.id}}) (%all (%value srcs (currently src_string)) (%value terms ?))} do
            assert state0.is_a?(State::ML::Any)

            case state0
            in State::ML::Err
              unreachable
            in State::ML::Ok
              begin
                term = ML.{{method.id}}(src.to(String))
                state1 = State::ML::Ok.new
                workspace1 = workspace1.with(terms, {:currently, term})
              rescue e : ML::SyntaxError
                state1 = State::ML::Err.new(e)
              end
            end
          end
        {% end %}

        givenpi %{[src @queries_ @contents_] (%all (%-value queries) (%value contents ?))} do
          assert state0.is_a?(State::Source::Any)

          case state0
          in State::Source::BadQuery
            unreachable
          in State::Source::None
            # Propose to infer query.
            workspace1 = workspace1.with(queries, :"?")
          in State::Source::FilePending
          in State::Source::FileLoaded
            workspace1 = workspace1.with(contents, {:currently, state0.content})
          end
        end

        givenpi %{[src @queries_ @contents_] (%all (%value queries (currently query_)) (%value contents ?))} do
          assert state0.is_a?(State::Source::Any)

          Term.case(query) do
            matchpi %{(file filename_string)} do
              path = Path[filename.to(String)].normalize

              case state0
              in State::Source::BadQuery
                unreachable
              in State::Source::None, State::Source::FilePending
              in State::Source::FileLoaded
                if state0.path == path
                  workspace1 = workspace1.with(contents, {:currently, state0.content})
                  next
                end
              end

              state1 = State::Source::FilePending.new(path, contents)
            end

            otherwise do
              state1 = State::Source::BadQuery.new(query)
            end
          end
        end

        givenpi %{[microfold/theme (@documents_ @rems_) @themes_] (%all (%-value documents) (%-value rems) (%value themes ?))} do
          assert state0.is_a?(State::MuTheme::Any)

          case state0
          in State::MuTheme::None
            workspace1 = workspace1.with(rems, :"?").with(documents, :"?")
          in State::MuTheme::Some
            workspace1 = workspace1.with(themes, {:currently, {:handle, device_addr}})
          end
        end

        givenpi %{[microfold/theme (@documents_ @rems_) @themes_] (%all (%value documents (currently document_dict)) (%value rems (currently ±rem)) (%value themes ?))} do
          assert state0.is_a?(State::MuTheme::Any)

          theme = Soma::Microfold.theme(document, rem.unsafe_as_n)
          state1 = State::MuTheme::Some.new(theme)
          workspace1 = workspace1.with(themes, {:currently, {:handle, device_addr}})
        end

        givenpi %{[microfold (@themes_ @nodes_) @uirs_] (%all (%value themes (currently (handle ownerT←(%number +i32)))) (%value nodes (currently node_)) (%value uirs ?))} do
          assert state0.is_a?(State::MuRender::Any)

          owner = ownerT.to(DeviceAddr)

          unless theme_state = states0[owner]?.as?(State::MuTheme::Some)
            Log.debug { "ignoring invalid Microfold theme owner device id: `#{owner}`" }
            next
          end

          uir, issues = Soma::Microfold.render(theme_state.theme, node)

          if issues.present?
            state1 = State::MuRender::Issues.new(node, issues)
          else
            state1 = State::MuRender::Ok.new
          end

          workspace1 = workspace1.with(uirs, {:currently, uir})
        end

        givenpi %{[m1/ruleset (@bases_ @selectors_) @rulesets_] (%all (%-value rulesets) (%-value selectors) (%value bases ?))} do
          assert state0.is_a?(State::Ruleset::Any)

          case state0
          in State::Ruleset::None
            workspace1 = workspace1.with(rulesets, :"?")
          in State::Ruleset::Some
            workspace1 = workspace1.with(rulesets, {:currently, {:handle, device_addr}})
          end
        end

        givenpi %{[m1/ruleset (@bases_ @selectors_) @rulesets_] (%all (%value bases (currently base_dict)) (%value selectors (currently selector_)) (%value rulesets ?))} do
          assert state0.is_a?(State::Ruleset::Any)

          ruleset = Ruleset.select(selector, base)
          state1 = State::Ruleset::Some.new(ruleset)
          workspace1 = workspace1.with(rulesets, {:currently, {:handle, device_addr}})
        end

        givenpi %{[alloy/template (@envs_ @templates_) @instances_] (%all (%value envs (currently env_dict)) (%value templates (currently template_)) (%value instances ?))} do
          assert state0.is_a?(State::Alloy::Any)

          instance, issues = Alloy.render_with_issues(env.unsafe_as_d, template)

          if issues.present?
            state1 = State::Alloy::TemplateIssues.new(env.unsafe_as_d, template, issues)
          else
            state1 = State::Alloy::Ok.new
          end

          workspace1 = workspace1.with(instances, {:currently, instance})
        end

        givenpi %{[alloy/view (@rulesets_ @templates_) @instances_] (%all (%value rulesets (currently (handle ownerT←(%number +i32)))) (%value templates (currently template_)) (%value instances ?))} do
          assert state0.is_a?(State::Alloy::Any)

          owner = ownerT.to(DeviceAddr)

          unless ruleset_state = states0[owner]?.as?(State::Ruleset::Some)
            Log.debug { "ignoring invalid ruleset owner device id: `#{owner}`" }
            next
          end

          instance, issues = Alloy.render_with_issues(ruleset_state.rules, template)

          if issues.present?
            state1 = State::Alloy::ViewIssues.new(ruleset_state.rules, template, issues)
          else
            state1 = State::Alloy::Ok.new
          end

          workspace1 = workspace1.with(instances, {:currently, instance})
        end

        givenpi %{[uir @uirs_ @dwuirs_] (%all (%value uirs (currently uir_)) (%value dwuirs ?))} do
          assert state0.is_a?(State::UIR::Any)

          case state0
          in State::UIR::None, State::UIR::Pending
            state1 = State::UIR::Pending.new(uir, dwuirs)
          end
        end

        # |@ rack.device.latest
        #
        # |@block
        # Use `latest` to memoize *terms*, effectively blocking pull computation
        # on *memos*.
        # |@endblock
        begin
          givenpi %{[latest @terms_ @memos_] (%value terms (currently term_))} do
            assert state0.is_a?(State::Cell::Any)

            state1 = State::Cell::Some.new(term)
            workspace1 = workspace1.with(memos, {:currently, term})
          end

          givenpi %{[latest @terms_ @memos_] (%all (%-value terms) (%value memos ?))} do
            assert state0.is_a?(State::Cell::Any)

            case state0
            in State::Cell::None
              workspace1 = workspace1.with(terms, :"?")
            in State::Cell::Some
              workspace1 = workspace1.with(memos, {:currently, state0.value})
            end
          end
        end

        # |@ rack.device.dwuir/window
        #
        # |@pattern
        # [dwuir/window @specs_ @events_]
        #
        # |@pattern
        # [dwuir/window @specs_]
        #
        # |@block
        # Use `dwuir/window` to display an image of DwUIR *specs* in an OS window.
        # Events from the window will be assigned to *events*.
        # |@endblock
        #
        # |@key specs soma.dwuir.window.os -- Window spec.
        #
        # |@key events soma.dwuir.window.event -- Edge for events.
        givenpi(
          %{[dwuir/window @specs_ @_] (%value specs (currently spec_))},
          %{[dwuir/window @specs_] (%value specs (currently spec_))},
        ) do
          assert state0.is_a?(State::Window::Any)

          case state0
          in State::Window::None
            state1 = State::Window::Open.new(state0.id, spec, state0.events)
          in State::Window::Open,
             State::Window::Closed
            state1 = state0.copy_with(spec: spec)
          end
        end

        # |@ rack.device.dwuir/console
        #
        # |@pattern
        # [dwuir/console @specs_ @events_]
        #
        # |@pattern
        # [dwuir/console @specs_]
        #
        # |@block
        # Use `dwuir/console` to display a subset of DwUIR, called textual DwUIR,
        # in the terminal. Events from the terminal will be assigned to *events*.
        #
        # NOTE: even though there can be any number of `dwuir/console` devices,
        # whether multiple console windows/tabs are going to be shown in the terminal
        # depends completely on the multiplexing ability of the client that is
        # handling `dwuir/console`.
        # |@endblock
        #
        # |@key specs soma.dwuir.window.console -- Window spec.
        #
        # |@key events soma.dwuir.window.event -- Edge for events.
        givenpi(
          %{[dwuir/console @specs_ @_] (%value specs (currently spec_))},
          %{[dwuir/console @specs_] (%value specs (currently spec_))},
        ) do
          assert state0.is_a?(State::Console::Any)

          case state0
          in State::Console::None
            state1 = State::Console::Open.new(state0.id, spec, state0.events)
          in State::Console::Open,
             State::Console::Closed
            state1 = state0.copy_with(spec: spec)
          end
        end

        givenpi %{[dwuir/image @specs_ @targets_] (%all (%value specs (currently spec_)) (%value targets (currently target_)))} do
          assert state0.is_a?(State::Image::Any)

          # |@ rack.device.dwuir/image.spec
          #
          # |@block
          # Image specifications are a subset of window specifications for convenience.
          # If you have a well-defined DwUIR window spec, you will be able to capture
          # an image of it without any further work.
          # |@endblock
          #
          # |@key width -- Determines the width of the resulting image, in pixels.
          #
          # |@key height -- Determines the height of the resulting image, in pixels.
          #
          # |@key backdrop soma.dwuir.color -- Determines the background (clear)
          # color of the resulting image.
          conf = Term.matchpi?(spec, %{(window content_* ⍊ width_: (%number +i16) height_: (%number +i16) backdrop_⋮ white)}) do
            DwUIR::ShowConf.new(
              width: width.to(Int32),
              height: height.to(Int32),
              backdrop: DwUIR::Color.term(backdrop, fallback: DwUIR::Color.named("white")),
              content: content,
            )
          end

          unless conf
            state1 = State::Image::BadSpec.new(spec)
            next
          end

          case state0
          in State::Image::None
            version1 = 0u32
          in State::Image::File
            # NOTE: we currently force redraw even if spec did not change -- we
            # don't know what happened on the disk, maybe the file was removed and
            # the user wants to re-create it. Thus simply bump up version to force
            # a redraw.
            version1 = state0.version + 1
          in State::Image::InMemory
            next if state0.conf == conf

            version1 = state0.version + 1
          in State::Image::BadSpec,
             State::Image::BadTarget
            unreachable
          end

          Term.case(target) do
            # |@ rack.device.dwuir/image.target.file
            #
            # |@block
            # Use `(file _)` to write an image to a file with the given *filename*.
            # The image's format will be determined from the file extension. For
            # example, `(file "path/to/foo.ppm")` will write a PPM image; similarly,
            # `(file "path/to/bar.png")` will write a PNG image.
            # |@endblock
            matchpi %{(file filename_string)} do
              path = Path[filename.to(String)].normalize
              state1 = State::Image::File.new(version1, conf, path)
            end

            # |@ rack.device.dwuir/image.target.memory
            #
            # |@block
            # Use `memory` to write an image to an in-memory buffer. This is mainly
            # used for comparison testing when Wirewright is developed; the buffer is
            # only reachable from the Crystal side.
            # |@endblock
            matchpi %{(memory id_)} do
              state1 = State::Image::InMemory.new(version1, conf, id)
            end

            otherwise do
              state1 = State::Image::BadTarget.new(target)
            end
          end
        end

        # |@ rack.device.ticker
        #
        # |@pattern
        # [ticker @specs_ @ticks_]
        #
        # |@block
        # Use a `ticker` device to schedule periodic `true` messages at *ticks*.
        # |@endblock
        #
        # |@key specs rack.period -- Specifies the period with which to
        # schedule the ticks.
        #
        # |@key ticks -- Will be `true` on each tick.
        givenpi %{[ticker @specs_ @ticks_] (%value specs (currently spec_))} do
          assert state0.is_a?(State::Ticker::Any)

          if period = period?(spec)
            state1 = State::Ticker::Running.new(state0.id, period, Term.entries({ticks, {:currently, true}}))
          else
            state1 = State::Ticker::BadPeriodSpec.new(state0.id, spec)
          end
        end

        # |@ rack.device.log
        #
        # |@pattern
        # [log @msgs_]
        #
        # |@block
        # Use a `log` device to print the value at *msgs* to console using
        # the `comment` style.
        # |@endblock
        givenpi %{[log @msgs_] (%value msgs (currently msg_))} do
          assert state0.is_a?(State::Log::Any)
          next unless state0.is_a?(State::Log::NoMessage)

          state1 = State::Log::Message.new(:comment, msg)
        end

        # |@ rack.device.note
        #
        # |@pattern
        # [note @msgs_]
        #
        # |@block
        # Use a `note` device to print the value at *msgs* to console using
        # the `note` style.
        # |@endblock
        givenpi %{[note @msgs_] (%value msgs (currently msg_))} do
          assert state0.is_a?(State::Log::Any)
          next unless state0.is_a?(State::Log::NoMessage)

          state1 = State::Log::Message.new(:note, msg)
        end

        # |@ rack.device.warn
        #
        # |@pattern
        # [warn @msgs_]
        #
        # |@block
        # Use a `warn` device to print the value at *msgs* to console using
        # the `warning` style.
        # |@endblock
        givenpi %{[warn @msgs_] (%value msgs (currently msg_))} do
          assert state0.is_a?(State::Log::Any)
          next unless state0.is_a?(State::Log::NoMessage)

          state1 = State::Log::Message.new(:warning, msg)
        end

        # |@ rack.device.err
        #
        # |@pattern
        # [err @msgs_]
        #
        # |@block
        # Use an `err` device to print the value at *msgs* to console using
        # the `error` style.
        # |@endblock
        givenpi %{[err @msgs_] (%value msgs (currently msg_))} do
          assert state0.is_a?(State::Log::Any)
          next unless state0.is_a?(State::Log::NoMessage)

          state1 = State::Log::Message.new(:error, msg)
        end

        otherwise { }
      end

      states1 = states1.assoc(device_addr, state1)
    end

    {workspace1, states1}
  end

  # Runs the inference loop on *workspace1*, using devices in *index*.
  #
  # Inference is delta-based here; thus the need for *workspace0*, to compute
  # the initial delta. *workspace0* can be empty, in which case all edges in
  # *workspace1* will be considered new.
  #
  # Returns the final workspace and a boolean indicating whether it is different
  # from *workspace1*.
  private def infer(workspace0 : Term::Dict, workspace1 : Term::Dict, index : Index, inference : Ruleset) : {Term::Dict, Bool}
    damaged = Deque(Term).new

    workspace1.each_entry do |edge, value|
      next if workspace0[edge]? == value

      damaged << edge
    end

    if damaged.empty?
      return workspace1, false
    end

    workspace0 = workspace1

    while edge = damaged.shift?
      index.each_device_at_edge(edge) do |device|
        next unless response = propagate?(device, workspace0, inference)

        # NOTE: This is a hot path, we cannot afford a Term.case containing
        # expensive backtracking here despite the niceties.
        unless proposal = proposal?(response)
          raise ArgumentError.new("invalid response term `#{ML.compact(response)}`")
        end

        target, keypath, value = proposal

        # Execute stateless native calls.
        Term.matchpi?(value, %{(native form_)}) do
          if result = call1(form)
            value = Term.of(:currently, result)
          else
            value = Term.of(:"?")
          end
        end

        workspace1, changed = commit(workspace1, keypath, value)
        next unless changed

        damaged << target
      end
    end

    {workspace1, true}
  end

  # :nodoc:
  #
  # Equivalent to the pattern `(proposal (%group keypath_ @target_ _*) value_)`.
  def proposal?(term : Term) : {Term, Term::Dict, Term}?
    return unless dict = term.as_d?
    return unless dict.itemsize >= 3

    items = dict.items

    target = items[1]
    return unless ML.edge?(target)

    keypath = items.move(1).grow(-1).collect
    value = items[-1]

    {target, keypath, value}
  end

  private def call1(form : Term)
    Term.of_case(form) do
      matchpi %{(m1.match pattern_ matchee_)} do
        M1.match?(pattern, matchee)
      end

      matchpi %{(m1.matches pattern_ matchee_)} do
        matches = M1.matches(pattern, matchee)
        matches.present? ? matches : nil
      end

      matchpi %{(m1.backmap pattern_ backspec_ matchee_)} do
        M1.backmap?(pattern, backspec, matchee)
      end

      otherwise { }
    end
  end

  # :nodoc:
  def commit(workspace : Term::Dict, keypath : Term::Dict, value : Term) : {Term::Dict, Bool}
    case current = workspace.follow?(keypath.items)
    when .nil?, Term.of(:"?")
      # Defined or computed
      workspace = workspace.where(keypath.items, eq: value)
      {workspace, current != value}
    when value # Consensus
      {workspace, false}
    else
      Log.error { "ignoring device commit due to conflict over `#{keypath.items.join(':')}`: `#{ML.compact(value)}` proposed over existing #{ML.compact(current)}" }
      {workspace, false}
    end
  end

  # Extension points for different roles in an environment.
  module Agent
    alias Any = Narrator | Peer

    # Peers may modify the environment's state map and issue proposals.
    #
    # In other words, they may act both as *initiators* of workspace processing
    # and as participants in it.
    struct Peer
      def initialize(&@fn : StateMap, Index -> {StateMap, Array(Term)})
      end

      delegate :call, to: @fn
    end

    # Narrators may comment on state transitions in the state map, e.g.
    # file-none transitioning to file-pending (such a transition may be
    # commented as "loading file").
    #
    # **Narrators are guaranteed to be called with states that compare
    # not equal.** `==` is used to check for equality.
    #
    # Narrators are "orthogonal" to the environment and cannot affect it
    # in any way. However, they may maintain their own, internal state
    # based on their observation, and thus, accomplish tasks like window
    # management (see `Rack::WM`), scheduling (see `Rack.scheduler`),
    # imaging (see `Rack::Image`), etc.
    struct Narrator
      def initialize(&@fn : Env, DeviceAddr, Term, State::Any, State::Any ->)
      end

      delegate :call, to: @fn
    end
  end

  # A client is located outside of an environment, and may initiate processing
  # by sending some workspaces or submitting some state maps.
  alias Client = Env ->

  class Env
    # Returns the current state map.
    getter states : StateMap

    # Returns an index of the rack associated with this environment.
    getter index : Index

    def initialize(@index : Index, @states : StateMap, @inference : Ruleset)
      @peers = [] of Agent::Peer
      @narrators = [] of Agent::Narrator
    end

    {% for word in %w[narrator peer] %}
      # :nodoc:
      def <<(agent : Agent::{{word.id.titleize}}) : self
        @{{word.id}}s << agent

        self
      end
    {% end %}

    {% if flag?(:docs) %}
      # Adds *agent* to this environment.
      def <<(agent : Agent::Any) : self
      end
    {% end %}

    # Adds all of *agents* to this environment.
    def <<(agents : Enumerable) : self
      agents.each { |agent| self << agent }

      self
    end

    private def narrate(states0 : StateMap, states1 : StateMap, &)
      states1.each do |device_addr, state1|
        state0 = states0[device_addr]
        next if state0 == state1
        yield device_addr, @index.device(device_addr), state0, state1
      end
    end

    private def narrate(states0 : StateMap, states1 : StateMap)
      narrate(states0, states1) do |device_addr, device, state0, state1|
        @narrators.each &.call(self, device_addr, device, state0, state1)
      end
    end

    private def submit0(states1 : StateMap)
      narrate(@states, states1)
      @states = states1
      narrate(states1, @states)
    end

    private def review(workspace : Term::Dict = Term[]) : Nil
      changed = false

      @peers.each do |agent|
        states1, proposals = agent.call(@states, @index)

        narrate(@states, states1)
        @states = states1

        proposals.each do |proposal|
          _, keypath, value = Rack.proposal?(proposal) || next
          workspace, wrote = Rack.commit(workspace, keypath, value)
          next unless wrote
          changed = true
        end
      end

      return unless changed

      send(workspace)
    end

    # Submits a new state map *states*.
    def submit(states : StateMap) : Nil
      unless @states.family == states.family
        raise ArgumentError.new("submitted state map is unrelated to one used by the environment")
      end

      submit0(states)

      review
    end

    # Shorthand for `StateMap#each`.
    def each(*args, **kwargs, &) : Nil
      @states.each(*args, **kwargs) { |*yargs| yield *yargs }
    end

    # Shorthand for `submit` of `StateMap#map`.
    def map(*args, **kwargs, &) : Nil
      submit(@states.map(*args, **kwargs) { |*yargs| yield *yargs })
    end

    # Sends *workspace* to the environment. Agents and the rack of this environment
    # will communicate through *workspace* and `states`, filling them with
    # information until fixpoint.
    #
    # Here on the Crystal side, you are expected to inspect `states` at some point
    # after calling this method.
    #
    # Workspace dicts (or at least the part of them that is meaningful to the rack)
    # consist of pairs of *one of the following two forms*:
    #
    # - `@edge_: (currently value_)`, which restricts the value at edge to *value*.
    # - `@edge_: ?`, which means the value at edge must be determined by the rack
    #   and/or the agents.
    #
    # Any other pair is ignored by the rack.
    #
    # Edges not mentioned and not reachable through mentioned ones, will not be
    # inspected, recomputed, or touched in any other way.
    #
    # Returns the resulting workspace for possible inspection by the caller.
    def send(workspace workspace0 : Term::Dict) : Term::Dict
      workspace1, _ = Rack.cycle(@index, @inference, workspace0, @states) do |states1|
        submit0(states1)

        # Let other fibers breathe some.
        Fiber.yield

        @states
      end

      review(workspace1)

      workspace1
    end
  end

  # :nodoc:
  def cycle(index : Index, inference : Ruleset, workspace1 : Term::Dict, states : StateMap, & : StateMap -> StateMap) : {Term::Dict, StateMap}
    seen = ExchangeSeenSet.new
    workspace0 = Term[]

    loop do
      workspace2, changed = infer(workspace0, workspace1, index, inference)
      break unless changed

      workspace0 = workspace1
      workspace1 = workspace2

      workspace1, states = exchange(workspace1, index, states, seen)
      states = yield states
    end

    states = yield decay(states)

    {workspace1, states}
  end

  # Replaces `State::Transient` states in *states* with their successors.
  # Returns a modified copy of *states*.
  private def decay(states : StateMap) : StateMap
    states.map(State::Transient) { |_, state0| state0.stable }
  end

  # **Entry point to Rack**: Constructs an environment for *rack*.
  #
  # - *rack* is the rack, a dict of `rack.device`s.
  # - *basis* is a document containing inference (section `inference`) and
  #   shorthand (section `shorthand`) rules. You most likely want to parse
  #   and give `runtime/basis.rack.wwml` as *basis*.
  # - *agents* is an array of agents the environment should be populated with.
  #
  # If possible, this function will also "ignite" the rack, meaning it may
  # not return as quickly as one would expect from a constructor.
  #
  # Returns a proc to "retire" the environment.
  #
  # NOTE: You must call the retire proc for cleanup if necessary (e.g. before
  # replacing with another env). Otherwise, there is the possibility of "zombie"
  # OS state remaining after the returned environment's disappearance -- since
  # with another env (or no env), we'll no longer have any way of reaching
  # the corresponding management code.
  #
  # NOTE: The retire proc can be called multiple times, although it would be
  # strange for you to do that. It simply transitions the rack into the after-
  # boot state, letting agents & devices in the rack handle that as they may.
  #
  # WARNING: Rack environments and most agents (e.g. `WM`) are not expected to
  # run in a concurrent setting; and are thus thread-unsafe. Make sure to
  # construct an env per fiber.
  def env(rack : Term, basis : Term::Dict, agents : Array(Agent::Any)) : {Env, (->)}
    rack, inference = instance(rack, basis)
    rack = flatten(rack)
    index = index(rack)
    states0, query = boot(index)

    env = Env.new(index, states0, inference)
    env << agents
    env.send(query)

    {env, -> { env.submit(states0) }}
  end

  # Traverses the device/edge graph encoded in *index* according to a
  # sequence of pattern-capture name steps given in *qpath*.
  #
  # A *qpath* is an alternating sequence of patterns and capture names. At
  # each step, the algorithm finds a device that matches the current pattern,
  # then uses the corresponding capture name to obtain the next edge to follow.
  # This continues until the final pattern in the path is reached.
  #
  # Captures made in previous patterns are available in subsequent ones
  # to enable constraints.
  #
  # Returns the match env of the final pattern if the full path can be followed.
  # Returns `nil` otherwise.
  #
  # Raises `ArgumentError` if a pattern in *qpath* matched but is missing
  # the corresponding capture; or if the capture value does not resolve to an edge.
  #
  # ```wwml
  # ;; The following qpath can be used to find e.g. the file from which nodes
  # ;; coming to [microfold (@_ @nodes_) @_] originate. Setting *origin* = <nodes>
  # ;; and following this qpath will give one a match env containing path: "path/to/file":
  # [ml @srcs_ @_] srcs [src @specs_ @_] specs [const @specs_ (file path_string)]
  # ```
  def query?(index : Index, origin : Term, qpath : Term::Dict) : Term::Dict?
    query?(index, Set(Term).new, Term[], origin, qpath.items)
  end

  private def query?(index : Index, seen : Set(Term), env0 : Term::Dict, edge : Term, qpath : Term::Dict::ItemsView) : Term::Dict?
    follow?(index, seen, edge) do |device|
      next unless env1 = M1.match?(qpath.first, device, env: env0)

      qpath = qpath.move(1)

      # Nothing ahead, we're at the end.
      if qpath.empty?
        next env1
      end

      # If something is ahead, treat it as a key.
      unless successor = env1[qpath.first]
        raise ArgumentError.new("match env does not contain capture #{qpath.first}")
      end

      unless ML.edge?(successor)
        raise ArgumentError.new("term captured by #{qpath.first} is not an edge")
      end

      query?(index, seen, env1, successor, qpath.move(1))
    end
  end

  private def follow?(index : Index, seen : Set(Term), edge : Term, &fn : Term -> T?) : T? forall T
    return unless seen.add?(edge)

    index.each_device_at_edge(edge) do |device|
      Term.case(device) do
        # Follow links.
        matchpi %{[link @a_ @b_]} do
          if edge == a
            next unless object = follow?(index, seen, b, &fn)
          elsif edge == b
            next unless object = follow?(index, seen, a, &fn)
          end

          return object
        end

        # Follow `latest`.
        matchpi %{[latest @pred_ @sink_]} do
          next unless edge == sink
          next unless object = follow?(index, seen, pred, &fn)
          return object
        end

        otherwise do
          next unless object = fn.call(device)
          return object
        end
      end
    end
  end
end

require "./rack/agents"
require "./rack/server"
