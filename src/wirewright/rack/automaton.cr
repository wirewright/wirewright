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
    # Use a separate parser for `D7.fuse` so that the main parser's
    # cache is not erased by past frames.
    @fuse_parser = D7::Parser.new(@parser.clf)

    # Automaton state.
    @epoch = Epoch.new(@alarm)
    @display = Deque(DisplayAction).new

    # Auxiliary state.
    @alarm_epoch = 0u64
    @t = [] of Time::Span
    @first = false

    # Subsystem state.
    @tspace_state = Tspace.state
    @parser_state = Parser.state(@epoch)
    @database_state = Database.state(@epoch)
    @extrinsic_state = Extrinsics.state(@epoch)
    @websocket_state = WebSocket.state(@epoch)
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
    Parser.pending?(@parser_state) || Extrinsics.pending?(@extrinsic_state) ||
      Database.pending?(@database_state) || WebSocket.pending?(@websocket_state)
  end

  # FIXME: this method is a mess
  private def step(subframes, frames, circuit : Term, prepass, library) : Nil
    # HACK: This is a "shadow step" to make sure rigs execute in the same tick
    # invisibly from the main Rack pass. From the latter's point of view, rigs
    # are immediate.
    #
    # NOTE: this here is supposed to be executed only before the very first step of a circuit.
    # However, with Automaton, there is in fact no such thing as a "very first step". You show
    # it one circuit this tick, and another one the next tick. Or it could be the same, but
    # evolved circuit. The point being, there's no distinction. So we have to "fix" the circuit
    # at the beginning always. Hopefully this will be optimized later, there are ways to do that.
    subframes << Rack.rig_step(@parser, circuit, prepass).last

    pass do
      input = subframes.last
      subframes.concat(Assembler.step(@assembler_state, @parser, library, input))
      output = subframes.last
      unless input == output
        frames << output
      end
    end

    pass do
      input = subframes.last
      subframes.concat(Tspace.step(@tspace_state, @parser, subframes.last, prepass))
      output = subframes.last
      unless input == output
        frames << output
      end
    end

    subframes.concat(Extrinsics.step(@extrinsic_state) do |extrinsics|
      Database.step(@database_state) do |database|
        Parser.step(@parser_state) do |parser|
          WebSocket.step(@websocket_state) do |web_socket|
            Supervisor.step do |supervisor|
              D7.step(@parser, subframes.last) do |hg|
                prepass.call(hg) do |hg|
                  proposals = [] of D7::Patch

                  extrinsics.propose(hg, proposals)
                  parser.propose(hg, proposals)
                  database.propose(hg, proposals)
                  web_socket.propose(hg, proposals)
                  supervisor.propose(hg, proposals)
                  Rack.propose(hg, proposals)

                  D7::Regime.merge(hg, proposals)
                end
              end
            end
          end
        end
      end
    end)

    # Execute rig step again to fix inconsistencies.
    frames << Rack.rig_step(@parser, subframes.last, prepass).last
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

      D7.fuse(@fuse_parser, seen, subframes.to_readonly_slice) do |subframe|
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

      D7.fuse(@fuse_parser, seen, frames.to_readonly_slice) do |frame|
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

  def next_subframes(circuit circuit0 : Term, **kwargs) : Slice(Term)
    subframes = Slice[circuit0]

    circuit1 = self.next(circuit0, **kwargs) do |action|
      case action
      in Again
      in DisplayFrame
      in DisplaySubframe
        subframes = subframes.append(action.content)
      in End
      in Wait
      end
    end

    subframes.append(circuit1)
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
