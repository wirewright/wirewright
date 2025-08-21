# Racks are networks of devices wired together by logical edges.
#
# Each device has an immutable state, and the rack as a whole evolves by
# applying inference rules and exchanging proposals in a workspace until
# a consistent state is reached.
#
# Rather than direct mutation, all updates flow through this exchange-and-
# inference process.
#
# Racks are similar in spirit to logic circuits and networks of constraints.
#
# Racks are designed to be the primary interface to Wirewright-as-a-system;
# so, a *general-purpose* interface. There's also µsoma which is a GUI to
# a subset of Wirewright.
module Ww::Rack
  extend self

  private alias D = Soma::DwUIR

  # Maps device addresses (indices into the rack) to the corresponding
  # Crystal-side state, if any. See also: `State`.
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
    def []?(addr : Int32) : State::Any?
      @map[addr]?
    end

    # Returns the state of a device with the given address *addr*. Raises
    # `KeyError` if such a device does not exist.
    def [](addr : Int32) : State::Any
      self[addr]? || raise KeyError.new
    end

    # Yields addresses and states of devices in this map.
    def each(& : Int32, State::Any ->) : Nil
      @map.each { |addr, state| yield addr, state }
    end

    # Returns an iterator over addresses and states of devices in this map.
    def each : Iterator({Int32, State::Any})
      (0...@map.size).each.map { |n| @map.nth?(n) || raise IndexError.new }
    end

    # Transforms states of devices in this map using the block. The block can
    # return `nil` for cheap skip/ignore. Returns a modified copy of this map.
    def map(& : Int32, State::Any -> State::Any?) : StateMap
      map1 = @map.transaction do |commit|
        @map.each do |addr, state0|
          next unless state1 = yield addr, state0

          commit.assoc(addr, state1)
        end
      end

      StateMap.new(@family, map1)
    end

    # Sets the state of a device with the given address *addr* to *state*.
    # Returns a modified copy of this map.
    #
    # Raises `KeyError` if a device with the given *addr* does not exist.
    def assoc(addr : Int32, state : State::Any) : StateMap
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
    alias Any = Source::Any | Cell::Any | Ruleset::Any | MuTheme::Any | MuRender::Any | Ticker::Any | UIR::Any | Window::Any | Image::Any | ProcessArgs::Any | ProcessEnv::Any | ML::Any | Alloy::Any | Log::Any

    # Transient states have their lifetime equal to the lifetime of the active
    # workspace. When the active workspace is expended, all transient states
    # will be converted into their stable forms. This is a shorthand to simplify
    # automatic cleanup of certain states after the current workspace retires.
    module Transient
      # Returns the stable form of this state.
      abstract def decayed
    end

    # :nodoc:
    #
    # Shorthand. Unfortunately macros cannot do this due to the use of `alias`;
    # Crystal appears to assess aliases during macro expansion which makes multi-level
    # macros that generate types incapable of participating in aliases/as alias union
    # member types.
    module TransientPrior(Stable)
      include Transient

      def decayed
        Stable.new
      end
    end

    # Associated with an `ml` device.
    module ML
      alias Any = Ok | Err

      record Ok
      record Err, e : ::Ww::ML::SyntaxError { include TransientPrior(Ok) }
    end

    # Associated with an `alloy/template` or `alloy/document` device.
    module Alloy
      alias Any = Ok | Issues

      record Ok
      record Issues, vars : Term::Dict, template : Term, complaints : Array(String) do
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
      record Issues, node : Term, backtraces : Array(Issue::Backtrace) do
        include TransientPrior(Ok)
      end
    end

    # Associated with a `ticker` device.
    module Ticker
      alias Any = ReplacedBy | Running | NotRunning
      alias NotRunning = Pending | BadPeriodSpec | None

      record None

      record ReplacedBy, current : Running, succ : NotRunning
      record Running, period : Time::Span, cancel : ->
      record Pending, period : Time::Span, query : Term::Dict

      record BadPeriodSpec, spec : Term { include TransientPrior(None) }
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

      record Open, window : D::Window::Any, spec : Term, events : Term?
      record Closed, spec : Term, events : Term?
      record None, events : Term?
    end

    # Associated with a `dwuir/image` device.
    module Image
      alias Any = InMemory | FilePending | InMemoryPending | BadSpec | BadTarget | None

      record None

      record InMemory, conf : D::ShowConf, id : Term, data : D::PixelRect

      record FilePending, conf : D::ShowConf, path : Path
      record InMemoryPending, conf : D::ShowConf, id : Term

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
  # NOTE: the keypath stack is reused. Make sure to make a copy if you want
  # to store it.
  private def each_edge(device : Term, &fn : Stack(Term), Term ->) : Nil
    # Special-case (const @_ _) so that we don't descend into its value.
    Term.matchpi?(device, %{[const @edge_ _]}) do
      fn.call(Stack{Term.of(1)}, edge)
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

  private alias Scope = Stack(Int32)

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
      resolve = ->(scope : Stack(Int32), edge : Term) do
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
  # extracted from *base*.
  #
  # *base* is assumed to be a document dict, containing at least two sections:
  # `inference` with inference rules selectable using *selector*; and `shorthands`
  # with shorthand rules selectable using *selector*.
  private def instance(rack : Term, base : Term::Dict, *, selector : Term = ML.term(%{[rule pattern_ template_]})) : {Term, Ruleset}
    rack, inference, shorthands = separate(rack, selector, base)

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

  private def separate(rack : Term, selector : Term, base : Term::Dict) : {Term, Ruleset, Ruleset}
    Term.matchpi(base, %[{¦ inference⋮ {} shorthands⋮ {}}]) do |shorthands|
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
    def device(addr : Int32) : Term
      @devices[addr]
    end

    # Yields devices and their device addresses.
    def each_device_with_addr(& : Term, Int32 ->) : Nil
      @devices.each_with_index { |device, addr| yield device, addr }
    end

    # Yields devices connected to the given *edge*.
    def each_device_at_edge(edge : Term, & : Term ->) : Nil
      return unless cluster = @clusters[edge]?

      cluster.each do |device|
        yield device
      end
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
            commit.assoc(device_addr, State::Window::None.new(events))
          end

          matchpi %{[dwuir/window @_]} do
            commit.assoc(device_addr, State::Window::None.new(events: nil))
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

          matchpi %{[alloy/template (@_ @_) @_]}, %{[alloy/document (@_ @_) @_]} do
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
            commit.assoc(device_addr, State::Ticker::None.new)
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
  alias ExchangeSeenSet = Set({Int32, UInt32})

  # :nodoc:
  struct ExchangeSession
    include Term::CaseSession

    def initialize(@seen : ExchangeSeenSet, @device_addr : Int32)
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
  private def exchange(workspace : Term::Dict, index : Index, states states0 : StateMap, seen : ExchangeSeenSet) : {Term::Dict, StateMap}
    states1 = states0

    index.each_device_with_addr do |device, device_addr|
      next unless state0 = states0[device_addr]?

      state1 = state0

      Term.case({device, workspace}, session: ExchangeSession.new(seen, device_addr)) do
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
                workspace = workspace.with(terms, {:currently, term})
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
            workspace = workspace.with(queries, :"?")
          in State::Source::FilePending
          in State::Source::FileLoaded
            workspace = workspace.with(contents, {:currently, state0.content})
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
                  workspace = workspace.with(contents, {:currently, state0.content})
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
            workspace = workspace.with(rems, :"?").with(documents, :"?")
          in State::MuTheme::Some
            workspace = workspace.with(themes, {:currently, {:handle, device_addr}})
          end
        end

        givenpi %{[microfold/theme (@documents_ @rems_) @themes_] (%all (%value documents (currently document_dict)) (%value rems (currently ±rem)) (%value themes ?)))} do
          assert state0.is_a?(State::MuTheme::Any)

          theme = Soma::Microfold.theme(document.unsafe_as_d, rem.unsafe_as_n)
          state1 = State::MuTheme::Some.new(theme)
          workspace = workspace.with(themes, {:currently, {:handle, device_addr}})
        end

        givenpi %{[microfold (@themes_ @nodes_) @uirs_] (%all (%value themes (currently (handle ownerT←(%number +i32)))) (%value nodes (currently node_)) (%value uirs ?))} do
          assert state0.is_a?(State::MuRender::Any)

          owner = ownerT.to(Int32)

          unless theme_state = states0[owner]?.as?(State::MuTheme::Some)
            Log.debug { "ignoring invalid Microfold theme owner device id: `#{owner}`" }
            next
          end

          uir, backtraces = Soma::Microfold.render(theme_state.theme, node)

          if backtraces.present?
            state1 = State::MuRender::Issues.new(node, backtraces)
          else
            state1 = State::MuRender::Ok.new
          end

          workspace = workspace.with(uirs, {:currently, uir})
        end

        givenpi %{[m1/ruleset (@bases_ @selectors_) @rulesets_] (%all (%-value rulesets) (%-value selectors) (%value bases ?))} do
          assert state0.is_a?(State::Ruleset::Any)

          case state0
          in State::Ruleset::None
            workspace = workspace.with(rulesets, :"?")
          in State::Ruleset::Some
            workspace = workspace.with(rulesets, {:currently, {:handle, device_addr}})
          end
        end

        givenpi %{[m1/ruleset (@bases_ @selectors_) @rulesets_] (%all (%value bases (currently base_dict)) (%value selectors (currently selector_)) (%value rulesets ?)))} do
          assert state0.is_a?(State::Ruleset::Any)

          ruleset = Ruleset.select(selector, base)
          state1 = State::Ruleset::Some.new(ruleset)
          workspace = workspace.with(rulesets, {:currently, {:handle, device_addr}})
        end

        givenpi %{[alloy/template (@envs_ @templates_) @instances_] (%all (%value envs (currently env_dict)) (%value templates (currently template_)) (%value instances ?))} do
          assert state0.is_a?(State::Alloy::Any)

          instance, complaints = Alloy.render_with_complaints(env.unsafe_as_d, template)

          if complaints.present?
            state1 = State::Alloy::Issues.new(env.unsafe_as_d, template, complaints.to_a)
          else
            state1 = State::Alloy::Ok.new
          end

          workspace = workspace.with(instances, {:currently, instance})
        end

        givenpi %{[alloy/document (@rulesets_ @templates_) @instances_] (%all (%value rulesets (currently (handle ownerT←(%number +i32)))) (%value templates (currently template_)) (%value instances ?))} do
          assert state0.is_a?(State::Alloy::Any)

          owner = ownerT.to(Int32)

          unless ruleset_state = states0[owner]?.as?(State::Ruleset::Some)
            Log.debug { "ignoring invalid ruleset owner device id: `#{owner}`" }
            next
          end

          instance = Alloy.render(ruleset_state.rules, template)

          # TODO: render() currently does not produce complaints; it should, but that's
          # a TODO. So we always resolve to State::Alloy::Ok for now.
          state1 = State::Alloy::Ok.new
          workspace = workspace.with(instances, {:currently, instance})
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
            workspace = workspace.with(memos, {:currently, term})
          end

          givenpi %{[latest @terms_ @memos_] (%all (%-value terms) (%value memos ?))} do
            assert state0.is_a?(State::Cell::Any)

            case state0
            in State::Cell::None
              workspace = workspace.with(terms, :"?")
            in State::Cell::Some
              workspace = workspace.with(memos, {:currently, state0.value})
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
        # |@key specs soma.dwuir.window -- Window spec.
        #
        # |@key events soma.dwuir.window.event -- Edge for events.
        givenpi(
          %{[dwuir/window @specs_ @_] (%value specs (currently spec_))},
          %{[dwuir/window @specs_] (%value specs (currently spec_))},
        ) do
          assert state0.is_a?(State::Window::Any)

          case state0
          in State::Window::None
            state1 = State::Window::Open.new(D::Window::None.new, spec, state0.events)
          in State::Window::Open, State::Window::Closed
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
          conf = Term.matchpi?(spec, %{(window content_ ⍊ width_: (%number +i16) height_: (%number +i16) backdrop_⋮ white)}) do
            D::ShowConf.new(
              width: width.to(Int32),
              height: height.to(Int32),
              backdrop: D::Color.term(backdrop, fallback: D::Color.named("white")),
              content: content,
            )
          end

          unless conf
            state1 = State::Image::BadSpec.new(spec)
            next
          end

          case state0
          in State::Image::None
          in State::Image::FilePending,
             State::Image::InMemoryPending,
             State::Image::BadSpec,
             State::Image::BadTarget
            next
          in State::Image::InMemory
            next if state0.conf == conf
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
              state1 = State::Image::FilePending.new(conf, path)
            end

            # |@ rack.device.dwuir/image.target.file
            #
            # |@block
            # Use `memory` to write an image to an in-memory buffer. This is mainly
            # used for comparison testing when Wirewright is developed; the buffer is
            # only reachable from the Crystal side.
            # |@endblock
            matchpi %{(memory id_)} do
              state1 = State::Image::InMemoryPending.new(conf, id)
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
            succ = State::Ticker::Pending.new(period, Term.entries({ticks, {:currently, true}}))
          else
            succ = State::Ticker::BadPeriodSpec.new(spec)
          end

          case state0
          in State::Ticker::NotRunning
            state1 = succ
          in State::Ticker::Running
            state1 = State::Ticker::ReplacedBy.new(state0, succ)
          in State::Ticker::ReplacedBy
            state1 = state0.copy_with(succ: succ)
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
        givenpi %{[err @msg_] (%value msgs (currently msg_))} do
          assert state0.is_a?(State::Log::Any)
          next unless state0.is_a?(State::Log::NoMessage)

          state1 = State::Log::Message.new(:error, msg)
        end

        otherwise { }
      end

      states1 = states1.assoc(device_addr, state1)
    end

    {workspace, states1}
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
    alias Any = Seeder | Narrator | Server | Peer

    # Seeders run before anything else in an environment, once.
    struct Seeder
      def initialize(&@fn : StateMap -> StateMap)
      end

      delegate :call, to: @fn
    end

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
    # Narrators are "orthogonal" to the environment and cannot affect it
    # in any way.
    struct Narrator
      def initialize(&@fn : Index, StateMap, StateMap ->)
      end

      delegate :call, to: @fn
    end

    # Servers modify some states in the state map -- effectively "serving"
    # some functionality to the workspace/devices.
    #
    # Servers cannot initiate workspace processing; they can only participate
    # in workspace processing initiated by something else.
    struct Server
      def initialize(&@fn : StateMap -> StateMap)
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
      @seeders = [] of Agent::Seeder
      @peers = [] of Agent::Peer
      @servers = [] of Agent::Server
      @narrators = [] of Agent::Narrator
    end

    {% for word in %w[seeder server narrator peer] %}
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

    private def submit0(states1 : StateMap)
      @narrators.each &.call(@index, @states, states1)
      @states = states1
      @servers.each { |r| @states = r.call(@states) }
      @narrators.each &.call(@index, states1, @states)
    end

    private def review(workspace : Term::Dict = Term[]) : Nil
      changed = false

      @peers.each do |agent|
        states1, proposals = agent.call(@states, @index)

        @narrators.each &.call(@index, @states, states1)
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
    def send(workspace workspace0 : Term::Dict) : self
      unless @seeders.empty?
        states0 = @states
        @seeders.each do |agent|
          @states = agent.call(@states)
        end
        @narrators.each &.call(@index, states0, @states)
        @seeders.clear
      end

      workspace1, _ = Rack.cycle(@index, @inference, workspace0, @states) do |states1|
        submit0(states1)

        # Let other fibers breathe some.
        Fiber.yield

        @states
      end

      review(workspace1)

      self
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
    states.map do |device_addr, state0|
      next unless state0.is_a?(State::Transient)

      state0.decayed
    end
  end

  # **Entry point to Rack**: Constructs an environment for *rack*.
  #
  # - *rack* is the rack, a dict of `rack.device`s.
  # - *base* is a document containing inference (section `inference`) and
  #   shorthand (section `shorthand`) rules. You most likely want to parse
  #   and give `runtime/base.rack.wwml` as *base*.
  # - *agents* is an array of agents the environment should be populated with.
  #
  # If possible, this function will also "ignite" the rack, meaning it may
  # not return as quickly as one would expect from a constructor.
  def env(rack : Term, base : Term::Dict, agents : Array(Agent::Any)) : Env
    rack, inference = instance(rack, base)
    rack = flatten(rack)
    index = index(rack)
    states, query = boot(index)

    env = Env.new(index, states, inference)
    env << agents
    env.send(query)
  end

  # Constructs a server agent that handles images (in-memory and file-backed).
  #
  # `dwuir/image` devices depend on this agent's presence (otherwise, they
  # are going to be ignored).
  def snap(ctx : D::Viewer::Context) : Agent::Server
    Agent::Server.new do |states|
      states.map do |device_addr, state0|
        case state0
        when State::Image::FilePending
          begin
            D.snap(ctx, state0.conf, state0.path)
          rescue e : D::SnapError
            Log.error(exception: e) { e.message }
          end

          State::Image::None.new
        when State::Image::InMemoryPending
          data = D.show(ctx, state0.conf)

          State::Image::InMemory.new(state0.conf, state0.id, data)
        else
          next
        end
      end
    end
  end

  # Constructs a server agent that schedules periodic ticking using *fn*.
  #
  # - The first argument of *fn* is period (e.g. every `100.milliseconds`).
  # - The second argument is a query dict that should be sent (`Env#send`)
  #   to the environment on each tick.
  #
  # `ticker` devices depend on this agent's presence (otherwise, they are
  # going to be ignored).
  def schedule(&fn : Time::Span, Term::Dict -> (->)) : Agent::Server
    Agent::Server.new do |states|
      states.map do |device_addr, state0|
        if state0.is_a?(State::Ticker::ReplacedBy)
          state0.current.cancel.call
          state0 = state0.succ
        end

        next unless state0.is_a?(State::Ticker::Pending)

        cancel = fn.call(state0.period, state0.query)

        State::Ticker::Running.new(state0.period, cancel)
      end
    end
  end

  # Constructs an agent that finds pending files in the state map, and proposes
  # their content to the workspace (caching it in the state map for future reference).
  def file_server(files : FileServer) : Agent::Peer
    Agent::Peer.new do |states0, index|
      proposals = [] of Term

      states1 = states0.map do |device_addr, state0|
        next unless state0.is_a?(State::Source::FilePending)

        device = index.device(device_addr)

        begin
          data = files.read(state0.path)
        rescue e : FileServerError
          Log.debug(exception: e) { "could not read file #{state0.path}" }
          next
        end

        content = String.new(data)

        proposals << Term.of(:proposal, state0.dst, {:currently, content})

        State::Source::FileLoaded.new(state0.path, state0.dst, content, instant: Time.local)
      end

      {states1, proposals}
    end
  end

  private def mkuiR(platform : D::Platform)
    cache = SyncCache(Term, Rewrite::Any).new(capacity: 2**16, preallocate: true)

    base_main = File.read(RESOURCES / (ENV["RSET"]? || "uiR-succ8.soma.wwml"))

    onceR = callR(PRIMITIVES)

    # First rewrite entries, then rewrite self.
    set, exhevalR = recR
    set.call chainR(entriesR(exhevalR), onceR)

    evalR = dfsR(
      switchR(
        { %[($ rewritee_)], exhevalR },
        { %[($once rewritee_)], onceR },
      )
    )

    set_backmapr, rec_backmapr = recR

    refR = dfsR(
      switchR(
        { %[($my rewritee←($ _))], chainR(rec_backmapr, envR(Term.of(:"$my"))) },
        { %[($my rewritee_)], envR(Term.of(:"$my")) },
        { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
        { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
      )
    )

    backmapR = set_backmapr.call chainR(refR, evalR)

    selector = ML.term(%[(%any° [rule pattern_ template_] [backmap pattern_ backspec_])])

    dwuirR = callR do |term|
      Rewrite.one(Soma::DwUIR.reply(platform, term))
    end

    # recursive exhR
    set_main, rec_main = recR
    set_main.call(memoR(cache, exhR(choiceR(
      itemsR(rec_main),
      chainR(
        rulesetR(Ruleset.select(selector, ML.terms(base_main)), noR, backmapR, noR),
        dwuirR,
      ),
    ))))
  end

  # Constructs an agent that finds and handles requests for UIR rewriting using
  # the uiR rewriter.
  #
  # TODO: this is a hack. uiR is no different from any other *rewriter circuit*,
  # but we do not have them implemented at the moment.
  def uir(platform : D::Platform) : Agent::Peer
    uiR = mkuiR(platform)

    Agent::Peer.new do |states0, _|
      proposals = [] of Term

      states1 = states0.map do |device_addr, state0|
        next unless state0.is_a?(State::UIR::Pending)

        dwuir = rewrite(state0.uir, uiR)
        proposals << Term.of(:proposal, state0.dst, {:currently, dwuir})

        State::UIR::None.new
      end

      {states1, proposals}
    end
  end

  # Constructs an agent that finds and handles requests for process arguments
  # and environment.
  #
  # `args` and `env` devices depend on this agent's presence (otherwise they
  # are going to be ignored)
  def process(*, args : Array(String), env : Hash(String, String)) : Agent::Provider
    Agent::Provider.new do |states|
      states.map do |state0|
        case state0
        when State::ProcessArgs::None
          State::ProcessArgs::Some.new(args)
        when State::ProcessEnv::None
          State::ProcessEnv::Some.new(env)
        end
      end
    end
  end

  # Constructs an environment client that performs event polling on open windows,
  # handles window closure, and redraws window content based on state content.
  #
  # The returned client must be called periodically with an environment so that it
  # can send it some events. How often (and whether) this happens depends
  # on the caller.
  #
  # `dwuir/window` devices depend on this client's presence (otherwise they are
  # going to be ignored).
  def wm(ctx : D::Window::Context) : Client
    Client.new do |env|
      # Handle input events.
      survived = D::Window.poll(open_window_set(env.states)) do |target, event|
        env.states.each do |device_addr, state|
          next unless state.is_a?(State::Window::Open)
          next unless events = state.events

          query = Term.entries({events, {:currently, event}})

          env.send(query)
        end
      end

      # Handle window closure.
      states1 = env.states.map do |device_addr, state0|
        next unless state0.is_a?(State::Window::Open)
        next if state0.window.is_a?(D::Window::None)
        next if state0.window.in?(survived)

        State::Window::Closed.new(state0.spec, state0.events)
      end
      env.submit(states1)

      states1 = env.states.map do |device_addr, state0|
        next unless state0.is_a?(State::Window::Open)

        window1 = D::Window.next(ctx, state0.window, state0.spec)
        D::Window.present(window1)

        state0.copy_with(window: window1)
      end
      env.submit(states1)
    end
  end

  private def open_window_set(states : StateMap) : Set(D::Window::Some)
    states.each
      .map { |_, state| state }
      .select(State::Window::Open)
      .map(&.window)
      .select(D::Window::Some)
      .to_set
  end

  # Constructs an environment client that performs a watch step for file-
  # backed `src` devices against *files*. This client will send appropriate
  # queries to the environment when a file dependency is created, removed,
  # or modified.
  def file_monitor(files : FileServer) : Client
    Client.new do |env|
      alert = Set(Int32).new

      states0 = states1 = env.states
      states0.each do |device_addr, state0|
        case state0
        when State::Source::FilePending
        when State::Source::FileLoaded
          t0 = state0.instant
        else
          next
        end

        path = state0.path

        loop do
          t1 = files.modification_time?(path)

          case {t0, t1}
          in {nil, nil}
            # Did not and does not exist.
            state1 = state0
            break
          in {_, nil}
            # Removed.
            state1 = State::Source::FilePending.new(path, state0.dst)
            states1 = states1.assoc(device_addr, state1)
            alert << device_addr
            break
          in {nil, _}
            # Created.
          in {_, _}
            # Exists.
            break if t0 == t1
          end

          # Modified.
          begin
            content = files.read(path)
          rescue FileServerError
            t1 = nil
            next
          end

          state1 = State::Source::FileLoaded.new(path, state0.dst, String.new(content), t1)
          states1 = states1.assoc(device_addr, state1)
          alert << device_addr
          break
        end
      end

      env.submit(states1)

      next if alert.empty?

      query = Term[]

      alert.each do |device_addr|
        state0, state1 = states0[device_addr], states1[device_addr]

        case {state0, state1}
        when {State::Source::FileLoaded, State::Source::FilePending}
          # Removed
          query = query.with(state1.dst, :"?")
        when {State::Source::FileLoaded, State::Source::FileLoaded}, # Modified
             {State::Source::FilePending, State::Source::FileLoaded} # Created
          query = query.with(state1.dst, {:currently, state1.content})
        end
      end

      env.send(query)
    end
  end
end
