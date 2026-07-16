# A high-level API for `Rack`.
#
# High-level usage:
#
# ```
# seed = ML.document(<<-WWML)
# (cell @x 0)
# (cell @y)
# (feed @x @y @x)
# WWML
#
# automaton = Rack::Automaton.new
# automaton.frames(seed).each do |frame|
#   puts ML.display(frame)
#   sleep 1.second
# end
#
# # Teardown: show an empty circuit to the automaton.
# automaton.next(Term.of)
# ```
#
# Low-level usage:
#
# ```
# seed = ML.document(<<-WWML)
# (cell @x 0)
# (cell @y)
# (feed @x @y @x)
# WWML
#
# automaton = Rack::Automaton.new
# circuit = seed
#
# loop do
#   circuit, action = automaton.blocking_next(circuit)
#
#   case action
#   in Rack::Automaton::DisplayFrame
#     puts ML.display(action.content)
#   in Rack::Automaton::DisplaySubframe
#   in Rack::Automaton::End
#     break
#   end
# end
#
# # Teardown: show an empty circuit to the automaton.
# automaton.next(Term.of)
# ```
class Ww::Rack::Automaton
  # Actions returned by `next`.
  alias Action = DisplayAction | Wait | Again | End

  # `next` wants you to wait until some asynchronous computation it scheduled
  # is finished. The circuit itself is quiescent; no more "logical" progress
  # can be made. It is your responsibility to determine how (and whether) to
  # wait. `Automaton` will signal completion on the alarm you pass to `Automaton.new`.
  # The simplest way is, therefore, to simply call `BlockingSignal#wait`.
  defrecord Wait

  # `next` tells you to call it again.
  defrecord Again

  # `next` tells you that there are no more frames; the circuit reached
  # *quiescence*. Unless it is perturbed by the outside world somehow,
  # its evolution is finished.
  defrecord End

  # Actions related to displaying a circuit.
  alias DisplayAction = DisplayFrame | DisplaySubframe

  # `next` recommends you to display a view of the circuit with frame
  # granularity, with the given *content*.
  defrecord DisplayFrame, content : Term

  # `next` recommends you to display a view of the circuit with subframe
  # granularity, with the given *content*.
  defrecord DisplaySubframe, content : Term

  @[Flags]
  enum DisplayMask
    # Enables the emission of `DisplayFrame` actions.
    Frame

    # Enables the emission of `DisplaySubframe` actions.
    Subframe
  end

  class Epoch
    def initialize(@alarm : BlockingSignal)
      @value = Atomic(UInt64).new(0u64)
    end

    def get : UInt64
      @value.get(:relaxed)
    end

    def call : Nil
      @value.add(1, :relaxed)
      @alarm.call
    end

    def wait(epoch : UInt64) : UInt64
      @alarm.wait(epoch)
    end
  end

  # Constructs a Rack automaton.
  #
  # - *parser* is the parser to use to parse circuits.
  # - *alarm* will be passed to asynchronous subsystems of `Rack` so that they
  #   can notify you that some piece of asynchronous work has completed.
  # - *measure* enables or disables frame time measurement (see also: `median`;
  #   disabled by default).
  # - *mask* specifies the `DisplayAction` classes to emit.
  def initialize(
    @parser : D7::Parser, *,
    @alarm = BlockingSignal.new,
    @measure : Bool = false,
    @display_mask : DisplayMask = DisplayMask::Frame,
  )
    # Automaton state.
    @epoch = Epoch.new(@alarm)
    @display = Deque(DisplayAction).new

    # Auxiliary state.
    @alarm_epoch = 0u64
    @t = [] of Time::Span
    @first = false

    # Subsystem state.
    @parser_state = Parser.state(@epoch)
    @database_state = Database.state(@epoch)
    @extrinsic_state = Extrinsics.state(@epoch)
    @assembler_state = Assembler.state
  end

  # Constructs an automaton using the standard classifier `Rack.clf`.
  #
  # See the other overload to learn about *kwargs*.
  def self.new(**kwargs) : Automaton
    new(D7::Parser.new(Rack.clf), **kwargs)
  end

  private def measure(&) : Nil
    unless @measure
      yield
      return
    end

    dt = Time.measure do
      yield
    end

    if @t.size < 32
      @t << dt
      return
    end

    @t[1] = @t.median
    @t.shift
    @t << dt
  end

  def epoch : UInt64
    @epoch.get
  end

  # Returns `true` if the underlying asynchronous subsystems of Rack are busy.
  # This often determines whether the circuit truly reached quiescence, or is
  # just "asynchronously busy".
  def pending? : Bool
    Parser.pending?(@parser_state) || Extrinsics.pending?(@extrinsic_state) || Database.pending?(@database_state)
  end

  private def step(subframes, frames, circuit : Term, prepass, library) : Nil
    subframes << circuit

    subframes.concat(Assembler.step(@assembler_state, @parser, library, subframes.last))
    frames << subframes.last

    subframes.concat(Tspace.step(@parser, subframes.last, prepass))
    frames << subframes.last

    # TODO: the following step()s must eventually contribute to the same hypergraph
    # instead of being staged like they are here. The subsystem nodes such as `parser`
    # or `rewriter` are conceptually members of the Rack step; not distinct steps
    # such as `Tspace` or `Assembler`.

    subframes.concat(Parser.step(@parser_state, @parser, subframes.last, prepass))
    frames << subframes.last

    subframes.concat(Extrinsics.step(@extrinsic_state, @parser, subframes.last, prepass))
    frames << subframes.last

    subframes.concat(Database.step(@database_state, @parser, subframes.last, prepass))
    frames << subframes.last

    subframes.concat(Rack.step(@parser, subframes.last, prepass))
    frames << subframes.last
  end

  # Advances the automaton by one abstract step by evolving *circuit*. Returns
  # the evolved circuit (can be the same as *circuit*) and an action for you
  # to run.
  def next(circuit : Term, prepass = Rack::Prepass, library = Assembler::RuleLibrary.empty) : {Term, Action}
    # Exhaust display items.
    if item = @display.shift?
      return circuit, item
    end

    # Calculate frames and subframes.
    frames = Pf::Kit.stack_array(Term, 4)
    subframes = Pf::Kit.stack_array(Term, 8)

    measure do
      step(subframes, frames, circuit, prepass, library)
    end

    fused = Pf::Kit.stack_array(Term, 8)

    # Commit.
    circuit0 = circuit
    circuit = frames.last

    # Fuse subframes.
    pass do
      next unless @display_mask.subframe?

      seen = circuit0

      D7.fuse(@parser, seen, subframes.to_readonly_slice) do |subframe|
        next if seen == subframe

        fused << subframe
        seen = subframe
      end

      fused.each do |content|
        @display << DisplaySubframe.new(content)
      end
    end

    fused.clear

    # Fuse frames.
    pass do
      next unless @display_mask.frame?

      seen = circuit0

      D7.fuse(@parser, seen, frames.to_readonly_slice) do |frame|
        next if seen == frame

        fused << frame
        seen = frame
      end

      fused.each do |content|
        @display << DisplayFrame.new(content)
      end
    end

    if item = @display.shift?
      return circuit, item
    end

    unless frames.all? { |frame| circuit0 == frame }
      return circuit, Again.new
    end

    # If there's nothing new to display, this means we're probably quiescent.
    # Check if any asynchronous work is in progress.

    if pending?
      return circuit, Wait.new
    end

    # If nothing is pending (no asynchronous work in progress), this means
    # we're done -- we've indeed reached quiescence.
    {circuit, End.new}
  end

  # The same as `next`, but subtracts control actions (`Again`, `Wait`, etc.)
  # by executing them. This lets you handle only "meaningful" actions such as
  # `DisplayAction` at the cost of a possibly nonterminating, blocking call.
  def blocking_next(circuit : Term, *args, **kwargs) : {Term, DisplayAction | End}
    loop do
      circuit, action = self.next(circuit, *args, **kwargs)

      case action
      in Again
      in DisplayAction, End
        return circuit, action
      in Wait
        @alarm_epoch = @epoch.wait(@alarm_epoch)
      end
    end
  end

  def next(circuit : Term, **kwargs, &) : Term
    circuit, action = self.next(circuit, **kwargs)

    yield action
    @display.each { |action| yield action }
    @display.clear

    circuit
  end

  def next_frames(circuit circuit0 : Term, **kwargs) : Slice(Term)
    frames = Slice[circuit0]

    circuit1 = self.next(circuit0, **kwargs) do |action|
      case action
      in Again
      in DisplayFrame
        frames = frames.append(action.content)
      in DisplaySubframe
      in End
      in Wait
      end
    end

    frames.append(circuit1)
  end

  private class FrameIterator
    include Iterator(Term)

    def initialize(@automaton : Automaton, seed : Term)
      @circuit = seed
    end

    def next
      loop do
        @circuit, action = @automaton.blocking_next(@circuit)

        case action
        in Again
        in DisplaySubframe
        in DisplayFrame
          return action.content
        in End
          return Iterator.stop
        end
      end
    end
  end

  # Returns an iterator over frame contents (i.e., `DisplayFrame#content`)
  # throughout the evolution of *seed*.
  #
  # NOTE: The iterator is not guaranteed to terminate; this depends entirely
  # on *seed*. For example, oscillators will not terminate.
  #
  # NOTE: Certain configurations of the automaton (such as automata constructed
  # with `DisplayMask::None`) will not return even on `Iterator#next` on infinite
  # *seed*s (e.g. an oscillator), because such configurations assume completion;
  # so for *seeds* that cannot complete, they will basically cause a busy-loop
  # in `Iterator#next`.
  def frames(seed : Term) : Iterator(Term)
    FrameIterator.new(self, seed)
  end

  # Returns the recursive median of frametime (buffer capacity 32). This
  # measurement only includes `step` (e.g. `Rack.step`), and not e.g.
  # `D7.fuse`.
  #
  # If no measurements were taken, returns the zero time span.
  #
  # NOTE: You must opt into frame time measurement by constructing an automaton
  # with `measure: true`. See also: `Automaton.new`.
  def median : Time::Span
    if @t.empty?
      return Time::Span::ZERO
    end

    @t.median
  end
end
