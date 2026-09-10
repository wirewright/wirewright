# Implements time-related nodes: `rack.sequencer`, `rack.ticker`, `rack.lfo`, etc.
module ::Ww::Rack::Timekeeper
  extend self

  # :nodoc:
  defcase State,
    origin : Time::Instant,
    checkpoint : Time::Span,
    periods : Set(Time::Span)

  class State
    setter checkpoint
  end

  def state : State
    origin = Time.instant
    checkpoint = Time::Span.zero
    periods = Set(Time::Span).new
    State.new(origin, checkpoint, periods)
  end

  def pending?(state : State) : Bool
    state.periods.present?
  end

  def deadline?(state : State) : Time::Instant?
    now = Time.instant
    elapsed = now - state.origin

    state.periods.min_of? do |period|
      if period.zero?
        now
      else
        n = (elapsed / period).floor.to_i64 + 1
        state.origin + period * n
      end
    end
  end

  defrecord StepContext,
    checkpoint : Time::Span,
    now : Time::Span,
    periods : Set(Time::Span)

  def step(state : State, & : Propose -> T) : T forall T
    now = Time.instant - state.origin
    seen_periods = Set(Time::Span).new

    propose = Propose.new do |hg, proposals|
      ctx = StepContext.new(state.checkpoint, now, seen_periods)
      propose(ctx, hg, proposals)
    end

    result = yield propose

    state.checkpoint = now
    state.periods.clear
    state.periods.concat(seen_periods)

    result
  end

  private def propose(ctx : StepContext, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :sequencer, :ticker, :lfo) do |node|
      nodeQ = node.term

      Term.case(nodeQ) do
        matchpi %{[sequencer periodQ_ _+]} do
          next unless period = DurationLanguage.decode?(periodQ)

          ctx.periods << period

          crossings = crossingcnt(ctx.checkpoint, ctx.now, period)

          # The (> _) term is called the *needle*. A sequencer can have zero or more
          # needles. A needle is moved forward, with wraparound, for each crossing.
          crossings.times do
            successor = nodeQ.transaction do |commit|
              steps = nodeQ.items.move(2)
              steps.each_with_index do |step, index|
                pred = steps[(index - 1) % steps.size]

                # If this step was active, then disable it.
                Term.matchpi?(step, %{(> term_)}) do
                  step = term
                end

                # If this step had an active predecessor, then enable it.
                Term.matchpi?(pred, %{(> _)}) do
                  step = Term.of(:>, step)
                end

                key = 2 + index
                commit.with(key, step)
              end
            end

            nodeQ = Term.of(successor)
          end

          D7.replace(node, nodeQ)
        end

        matchpiT %{[ticker (%layer periodQ_ {| step⋮ 1}) ±state]} do
          next unless period = DurationLanguage.decode?(periodQ)

          ctx.periods << period

          crossings = crossingcnt(ctx.checkpoint, ctx.now, period)
          next if crossings.zero? # Fast path out

          D7.patch(node, {2, state + step*Term[crossings]})
        end

        matchpi %{[lfo (periodQ_ waveform_) _?]} do
          next unless period = DurationLanguage.decode?(periodQ)

          ctx.periods << period

          crossings = crossingcnt(ctx.checkpoint, ctx.now, period)
          next if crossings.zero? # Fast path out

          next unless output = wavef?(waveform, ctx.now)

          D7.patch(node, {2, output})
        end

        # debounce?

        otherwise { }
      end
    end
  end

  private def crossingcnt(checkpoint : Time::Span, now : Time::Span, period : Time::Span) : Int64
    if period.zero?
      # Zero period should make it always cross.
      1i64
    else
      previous = (checkpoint / period).floor.to_i64
      current = (now / period).floor.to_i64
      current - previous
    end
  end

  private def wavef?(waveform : Term, now : Time::Span) : Float64?
    Term.case(waveform) do
      # |@ rack.lfo.waveform
      #
      # |@pattern
      # (sin freq_ ⍊ offset⋮ 1/2 amplitude⋮ 1/2)
      #
      # |@key freq rack.lfo.freq
      # The frequency of the wave.
      #
      # |@key offset
      # The offset of the wave.
      #
      # |@key amplitude
      # The amplitude of the wave.
      #
      # |@block
      # Defines a sine waveform.
      #
      # |@example
      # ```wwml
      # (sin (440 Hz))
      # (sin (1 Hz) offset: 0 amplitude: 1)
      # ```
      matchpi(
        %{(sin freqQ_ ⍊ offset⋮ 1/2 amplitude⋮ 1/2)},
        offset: Float64,
        amplitude: Float64,
      ) do
        next unless freq = freq?(freqQ)

        # Calculate how many cycles have passed.
        cycles = now.total_seconds * freq
        # Take the fractional part, e.g. 1.⏏23⏏, telling us how far we are
        # into the current cycle.
        phase = cycles - cycles.floor
        offset + amplitude*Math.sin(Math::TAU * phase)
      end

      # |@ rack.lfo.waveform
      #
      # |@pattern
      # (saw freq_ ⍊ direction_: (%optional up (%any up dn)))
      #
      # |@key freq rack.freq
      # The frequency of the wave.
      #
      # |@key direction
      # Whether the saw should be rising (`up`) or falling (`dn`).
      #
      # |@block
      # Defines a sawtooth waveform.
      #
      # |@example
      # ```wwml
      # (saw (10 Hz))
      # (saw (10 Hz) direction: dn)
      # ```

      matchpi %{(saw freqQ_ ⍊ direction: (%optional up up))} do
        next unless freq = freq?(freqQ)

        cycles = now.total_seconds * freq
        phase = cycles - cycles.floor
        phase # Ramp is just phase
      end

      matchpi %{(saw freqQ_ ⍊ direction: dn)} do
        next unless freq = freq?(freqQ)

        cycles = now.total_seconds * freq
        phase = cycles - cycles.floor
        1.0f64 - phase
      end

      # |@ rack.lfo.waveform
      #
      # |@pattern
      # [tri freq_]
      #
      # |@key freq rack.freq
      # The frequency of the wave.
      #
      # |@block
      # Defines a triangle waveform.
      #
      # |@example
      # ```wwml
      # (tri (10 Hz))
      # ```
      matchpi %{[tri freqQ_]} do
        next unless freq = freq?(freqQ)

        cycles = now.total_seconds * freq
        phase = cycles - cycles.floor
        1.0f64 - (2*phase - 1.0f64).abs
      end

      # |@ rack.lfo.waveform
      #
      # |@pattern
      # (pulse freq_ ⍊ duty⋮ 1/2)
      #
      # |@key freq rack.freq
      # The frequency of the wave.
      #
      # |@key duty
      # The fraction of each cycle spent in the high state.
      #
      # |@block
      # Defines a pulse waveform.
      #
      # |@example
      # ```wwml
      # (pulse (1 Hz))
      # (pulse (10 Hz) duty: 1/5)
      # ```
      matchpi %{(pulse freqQ_ ⍊ duty⋮ 1/2)}, duty: Float64 do
        next unless freq = freq?(freqQ)

        cycles = now.total_seconds * freq
        phase = cycles - cycles.floor
        phase < duty ? 0.0f64 : 1.0f64
      end

      otherwise { }
    end
  end

  private def freq?(term : Term) : Float64?
    Term.case(term) do
      # |@ rack.lfo.freq
      #
      # |@pattern
      # (±freq hz)
      # (±freq Hz)
      #
      # |@block
      # Specifies a frequency in hertz.
      #
      # |@example
      # ```wwml
      # (440 Hz)
      # ```
      matchpi %{(±freq hz)}, %{(±freq Hz)} do
        freq.to(Float64)
      end

      # |@ rack.lfo.freq
      #
      # |@pattern
      # (±freq khz)
      # (±freq kHz)
      #
      # |@block
      # Specifies a frequency in kilohertz.
      #
      # |@example
      # ```wwml
      # (10 kHz)
      # ```
      matchpi %{(±freq khz)}, %{(±freq kHz)} do
        freq.to(Float64) * 1000
      end

      otherwise { }
    end
  end
end
