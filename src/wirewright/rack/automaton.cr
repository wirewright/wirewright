module Ww::Rack
  alias Propose = D7::Hypergraph, Array(D7::Patch) ->
end

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
# automaton.blocking_next(Term.of)
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
# automaton.blocking_next(Term.of)
# ```
class Ww::Rack::Automaton
  # Actions returned by `next`.
  alias Action = DisplayAction | Wait | Again | End

  # `next` wants you to wait until some asynchronous computation it scheduled
  # is finished. The circuit itself is quiescent; no more "logical" progress
  # can be made. It is your responsibility to determine how (and whether) to
  # wait. `Automaton` will signal completion on the alarm you pass to `Automaton.new`.
  # The simplest way is, therefore, to simply call `BlockingSignal#wait`.
  defrecord Wait, deadline : Time::Instant?, smart: true

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

    def wait_until(epoch : UInt64, timeout : Time::Span) : UInt64
      @alarm.wait_until(epoch, timeout)
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
    @fs_state = FS.state(@epoch)
    @tspace_state = Tspace.state
    @parser_state = Parser.state(@epoch)
    @database_state = Database.state(@epoch)
    @extrinsic_state = Extrinsics.state(@epoch)
    @accord_state = Accord.state(@epoch)
    @assembler_state = Assembler.state
    @rewriter_state = Rewriter.state(@epoch)
    @backsys_state = Backsys.state
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

  # Returns `true` if the underlying asynchronous subsystems are busy. This
  # tells whether the circuit truly reached quiescence, or is just waiting
  # for asynchronous work to finish.
  #
  # See also: `deadline?`.
  def pending? : Bool
    Parser.pending?(@parser_state) || Extrinsics.pending?(@extrinsic_state) ||
      Database.pending?(@database_state) || Accord.pending?(@accord_state) ||
      FS.pending?(@fs_state) || Rewriter.pending?(@rewriter_state)
  end

  # Returns the smallest deadline among deadlines for asynchronous subsystems.
  # The returned deadline is not guaranteed to be in the future. The caller
  # is expected to timeout their `pending?` wait when the returned deadline
  # is reached.
  #
  # Some subsystems need to wake up periodically without any extrinsic reason.
  # Such subsystems declare a `deadline?`. The caller (often the interpreter
  # of `Wait`) is the expected to wait until that instant.
  #
  # NOTE: The deadline will not necessarily be followed to the microsecond.
  # `Automaton` only guarantees that it will wait if it can, and wake up
  # after or at the deadline. No further guarantees as to how soon that will
  # happen are given.
  def deadline? : Time::Instant?
    Accord.deadline?(@accord_state)
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
    subframes << Rack.manipulate(@parser, circuit, prepass).last

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
      subframes.concat(Tspace.step(@tspace_state, @parser, input, prepass))
      output = subframes.last
      unless input == output
        frames << output
      end
    end

    subframes.concat(Extrinsics.step(@extrinsic_state) do |extrinsics|
      Database.step(@database_state) do |database|
        Parser.step(@parser_state) do |parser|
          Accord.step(@accord_state) do |accord|
            Supervisor.step do |supervisor|
              FS.step(@fs_state) do |fs|
                Rewriter.step(@rewriter_state) do |rewriter|
                  Backsys.step(@backsys_state) do |backsys|
                    Misc.step do |misc|
                      D7.step(@parser, subframes.last) do |hg|
                        prepass.call(hg) do |hg|
                          proposals = [] of D7::Patch

                          extrinsics.call(hg, proposals)
                          parser.call(hg, proposals)
                          database.call(hg, proposals)
                          accord.call(hg, proposals)
                          supervisor.call(hg, proposals)
                          fs.call(hg, proposals)
                          rewriter.call(hg, proposals)
                          backsys.call(hg, proposals)
                          misc.call(hg, proposals)

                          D7.merge(hg, proposals)
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end)

    # Execute manipulate again to fix inconsistencies.
    frames << Rack.manipulate(@parser, subframes.last, prepass).last
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

    # Commit.
    circuit0 = circuit
    circuit = frames.last

    buffer = Pf::Kit.stack_array(Term, 8)

    # Deduplicate *subframes*. Deduplication => less work for `D7.fuse`, which
    # is quite expensive.
    pass do
      next unless @display_mask.subframe?

      subframes.each do |frame|
        next if buffer.last? == frame

        buffer << frame
      end

      subframes.clear
      subframes.concat(buffer)
      buffer.clear
    end

    # Deduplicate *frames* in a similar way.
    pass do
      next unless @display_mask.frame?

      frames.each do |frame|
        next if buffer.last? == frame

        buffer << frame
      end

      frames.clear
      frames.concat(buffer)
      buffer.clear
    end

    # Fuse subframes.
    pass do
      next unless @display_mask.subframe?

      seen = circuit0

      D7.fuse(@fuse_parser, seen, subframes) do |subframe|
        next if seen == subframe

        buffer << subframe
        seen = subframe
      end

      buffer.each { |content| @display << DisplaySubframe.new(content) }
      buffer.clear
    end

    # Fuse frames.
    pass do
      next unless @display_mask.frame?

      seen = circuit0

      D7.fuse(@fuse_parser, seen, frames) do |frame|
        next if seen == frame

        buffer << frame
        seen = frame
      end

      buffer.each { |content| @display << DisplayFrame.new(content) }
      buffer.clear
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
      return circuit, Wait.new(deadline?)
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
        if deadline = action.deadline?
          wait(deadline)
        else
          @alarm_epoch = @epoch.wait(@alarm_epoch)
        end
      end
    end
  end

  def wait(deadline)
    timeout = deadline - Time.instant
    @alarm_epoch = @epoch.wait_until(@alarm_epoch, timeout)
  end

  def next_frame(seed : Term) : {Term, Term?}
    loop do
      result, action = blocking_next(seed)

      case action
      in DisplaySubframe
      in DisplayFrame
        return result, action.content
      in End
        return result, nil
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
