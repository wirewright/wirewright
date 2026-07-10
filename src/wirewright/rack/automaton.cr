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
# frames = Rack::Automaton.frames(seed)
# frames.each do |frame|
#   puts ML.display(frame)
#   sleep 1.second
# end
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
# machine = Rack::Automaton.new
# circuit = seed
#
# loop do
#   circuit, action = machine.blocking_next(circuit)
#
#   case action
#   in Rack::Automaton::DisplayFrame
#     puts ML.display(action.content)
#   in Rack::Automaton::DisplaySubframe
#   in Rack::Automaton::End
#     break
#   end
# end
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

  # Constructs a Rack machine.
  #
  # - *parser* is the parser to use to parse circuits.
  # - *alarm* will be passed to asynchronous components of `Rack` so that they
  #   can notify you that some piece of asynchronous work was completed.
  # - *measure* enables or disables frame time measurement (see also: `median`;
  #   disabled by default).
  # - *mask* specifies the `DisplayAction` classes to emit.
  def initialize(
    @parser : D7::Parser, *,
    @alarm = BlockingSignal.new,
    @measure : Bool = false,
    @display_mask : DisplayMask = DisplayMask::Frame,
  )
    @alarm_epoch = 0u64
    @display = Deque(DisplayAction).new
    @parser_state = Parser.state(@alarm)
    @assembler_state = Assembler.state
    @t = [] of Time::Span
    @first = false
  end

  # Constructs a machine using the standard classifier `Rack.clf`.
  #
  # See the other overload to learn about *kwargs*.
  def self.new(**kwargs) : Automaton
    new(D7::Parser.new(Rack.clf), **kwargs)
  end

  # A shorthand for `new.frames(seed)`.
  def self.frames(seed : Term) : Iterator(Term)
    new.frames(seed)
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

  # Advances the machine by one abstract step by evolving *circuit*. Returns
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

    subframes << circuit

    measure do
      subframes.concat(Assembler.step(@assembler_state, @parser, library, subframes.last))
      frames << subframes.last

      subframes.concat(Tspace.step(@parser, subframes.last, prepass))
      frames << subframes.last

      subframes.concat(Parser.step(@parser_state, @parser, subframes.last, prepass))
      frames << subframes.last

      subframes.concat(Rack.step(@parser, subframes.last, prepass))
      frames << subframes.last
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

    unless circuit0 == circuit
      return circuit, Again.new
    end

    # If there's nothing new to display, this means we're probably quiescent.
    # Check if any asynchronous work is in progress.

    # Make `pending?` queries.
    pending = Parser.pending?(@parser_state)

    if pending
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
        @alarm_epoch = @alarm.wait(@alarm_epoch)
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

    def initialize(@machine : Automaton, seed : Term)
      @circuit = seed
    end

    def next
      loop do
        @circuit, action = @machine.blocking_next(@circuit)

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
  # NOTE: Certain configurations of the machine (such as machines constructed
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
  # NOTE: You must opt into frame time measurement by constructing a machine
  # with `measure: true`. See also: `Automaton.new`.
  def median : Time::Span
    if @t.empty?
      return Time::Span::ZERO
    end

    @t.median
  end
end
