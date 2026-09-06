module MuSoma
  alias UpdateInput = UpdateFocus | UpdateKeyboardState

  defrecord UpdateFocus, addr : D7::NodeAddr, focus : Term
  defrecord UpdateKeyboardState, addr : D7::NodeAddr, keys : Pf::Set(Term)

  alias Input = InputModel | KeyboardModel

  defrecord InputModel, focus : InputFocus, keys : Pf::Set(Term), copying: true
  defrecord KeyboardModel, focus : InputFocus, keys : Pf::Set(Term), copying: true

  enum InputFocus
    # The input is currently inactive, can receive focus, can give focus.
    Inactive

    # The input is currently active, can receive focus, can give focus.
    Active

    # The input is currently inactive, politely asks everyone to blur,
    # and wants to receive focus.
    Wanted

    # The input is currently active, cannot give focus, does not seize focus.
    AlwaysActive

    # The input is currently inactive, cannot receive focus.
    AlwaysInactive

    # Seizes control of all input. Refuses all `Wanted` requests while active.
    # Blurs all active inputs. Does not affect `AlwaysActive` inputs (they remain
    # active even if focus is seized).
    Seize

    def self.parse?(term : Term) : InputFocus?
      case term
      when Term.of(false)     then Inactive
      when Term.of(true)      then Active
      when Term.of(:wanted)   then Wanted
      when Term.of(:always)   then AlwaysActive
      when Term.of(:disabled) then AlwaysInactive
      when Term.of(:seize)    then Seize
      end
    end

    def term : Term
      case self
      in .inactive?        then Term.of(false)
      in .active?          then Term.of(true)
      in .wanted?          then Term.of(:wanted)
      in .always_active?   then Term.of(:always)
      in .always_inactive? then Term.of(:disabled)
      in .seize?           then Term.of(:seize)
      end
    end

    def focused? : Bool
      active? || always_active? || seize?
    end
  end

  struct InputExchange
    # :nodoc:
    def initialize(@inputs : Slice({D7::NodeAddr, Input}))
    end

    def initialize
      @inputs = Slice({D7::NodeAddr, Input}).empty
    end

    def register(addr : D7::NodeAddr, model : Input) : InputExchange
      InputExchange.new(@inputs.append({addr, model}))
    end

    def active? : Bool
      @inputs.any? { |_, input| input.focus.active? }
    end

    private def rotate(n : Int32) : InputExchange
      bits = [] of Bool

      @inputs.each do |_, input|
        case input.focus
        in .inactive?
          bits << false
        in .active?
          bits << true
        in .wanted?, .always_active?, .always_inactive?
        in .seize?
          return self
        end
      end

      if bits.present? && bits.all?(false)
        if n.negative?
          bits[0] = true
        else
          bits[-1] = true
        end
      else
        bits.rotate!(n)
      end

      inputs1 = @inputs.to_readonly_slice do |(addr, input)|
        case input.focus
        in .inactive?, .active?
          if bits.shift
            {addr, input.copy_with(focus: InputFocus::Active)}
          else
            {addr, input.copy_with(focus: InputFocus::Inactive)}
          end
        in .wanted?, .always_active?, .always_inactive?
          {addr, input}
        in .seize?
          unreachable
        end
      end

      InputExchange.new(inputs1)
    end

    def backward : InputExchange
      rotate(1)
    end

    def forward : InputExchange
      rotate(-1)
    end

    def blur : InputExchange
      inputs1 = @inputs.to_readonly_slice do |(addr, input)|
        case input.focus
        in .inactive?, .wanted?, .always_active?, .always_inactive?, .seize?
          {addr, input}
        in .active?
          {addr, input.copy_with(focus: InputFocus::Inactive)}
        end
      end

      InputExchange.new(inputs1)
    end

    def step : InputExchange
      wanted = false
      seized = false

      @inputs.each do |_, input|
        case input.focus
        in .inactive?, .always_active?, .always_inactive?, .active?
        in .seize?
          seized = true
        in .wanted?
          wanted = true
        end
      end

      inputs1 = @inputs.to_readonly_slice do |(addr, input)|
        case input.focus
        in .inactive?, .always_active?, .always_inactive?, .seize?
          {addr, input}
        in .wanted?
          if seized
            # Decline
            {addr, input.copy_with(focus: InputFocus::Inactive)}
          else
            # Accept
            {addr, input.copy_with(focus: InputFocus::Active)}
          end
        in .active?
          if seized || wanted
            {addr, input.copy_with(focus: InputFocus::Inactive)}
          else
            {addr, input}
          end
        end
      end

      InputExchange.new(inputs1)
    end

    def self.sync(exchange0 : InputExchange, exchange1 : InputExchange, input : InputTransition, keyboard : InputTransition, & : UpdateInput ->) : Nil
      assert exchange0.@inputs.size == exchange1.@inputs.size

      exchange0.@inputs.zip(exchange1.@inputs) do |(addr0, model0), (addr1, model1)|
        assert addr0 == addr1

        addr = addr1

        # Notify nodes of focus change.
        unless model0.focus == model1.focus
          yield UpdateFocus.new(addr, model1.focus.term)
        end

        # Ask nodes which lost focus to clear keys.
        unless model1.focus.focused?
          next if model1.keys.empty?
          yield UpdateKeyboardState.new(addr, Pf::Set(Term).new)
          next
        end

        # Ask nodes which gained / have focus to update keys.
        case model1
        in InputModel    then authority = input
        in KeyboardModel then authority = keyboard
        end

        keys = authority.update(model1.keys)
        next if model0.keys == keys

        yield UpdateKeyboardState.new(addr, keys)
      end
    end
  end

  class InputTransition
    # :nodoc:
    def initialize(
      @pred : Pf::Set(Term),
      @succ : Pf::Set(Term),
      @handled : Pf::Set(Term),
    )
    end

    def self.new : InputTransition
      new(
        pred: Pf::Set(Term).new,
        succ: Pf::Set(Term).new,
        handled: Pf::Set(Term).new,
      )
    end

    def pressed : Pf::Set(Term)
      (@succ - @handled) - @pred
    end

    def released : Pf::Set(Term)
      @pred - (@succ - @handled)
    end

    # FIXME: Is this necessary? Can't we simply remove released keys and
    # add pressed ones?
    def update(target : Pf::Set(Term)) : Pf::Set(Term)
      discarded = @pred - target
      (target - released) + ((@succ - @handled) - discarded)
    end

    def to(succ : Pf::Set(Term)) : InputTransition
      # Check which keys were released and un-handle them.
      released = succ - @succ

      InputTransition.new(@succ, succ, @handled - released)
    end

    def hold : InputTransition
      InputTransition.new(@succ, @succ, @handled)
    end

    def handle(key : Term) : InputTransition
      InputTransition.new(@pred, @succ, @handled.add(key))
    end

    def handle(mods : Enumerable(Term) | Tuple, key : Term, &) : InputTransition
      # Already handled this key. It must be released first.
      if key.in?(@handled)
        return self
      end

      # Modifier must be pressed *before* key is pressed, and must be held
      # alongside the key for the combo to trigger.
      unless mods.all? { |mod| mod.in?(@pred) && mod.in?(@succ) } && key.in?(@succ)
        return self
      end

      yield

      InputTransition.new(@pred, @succ, @handled.add(key))
    end

    def handle(key : Term, &) : InputTransition
      handle(Tuple.new, key) { yield }
    end

    def term : Term
      buffer = Pf::Kit.stack_array(Term)
      buffer.reserve(@succ.size)
      @succ.each do |key|
        buffer << key
      end
      buffer.sort! { |a, b| Term.compare(a, b) }

      Term.of(buffer)
    end
  end

  class InputTransition
    # :nodoc:
    def initialize(
      @pred : Pf::Set(Term),
      @succ : Pf::Set(Term),
      @handled : Pf::Set(Term),
    )
    end

    def self.new : InputTransition
      new(
        pred: Pf::Set(Term).new,
        succ: Pf::Set(Term).new,
        handled: Pf::Set(Term).new,
      )
    end

    def pressed : Pf::Set(Term)
      (@succ - @handled) - @pred
    end

    def released : Pf::Set(Term)
      @pred - (@succ - @handled)
    end

    # FIXME: Is this necessary? Can't we simply remove released keys and
    # add pressed ones?
    def update(target : Pf::Set(Term)) : Pf::Set(Term)
      discarded = @pred - target
      (target - released) + ((@succ - @handled) - discarded)
    end

    def to(succ : Pf::Set(Term)) : InputTransition
      # Check which keys were released and un-handle them.
      released = succ - @succ

      InputTransition.new(@succ, succ, @handled - released)
    end

    def hold : InputTransition
      InputTransition.new(@succ, @succ, @handled)
    end

    def handle(mods : Enumerable(Term) | Tuple, key : Term, &) : InputTransition
      # Already handled this key. It must be released first.
      if key.in?(@handled)
        return self
      end

      # Modifier must be pressed *before* key is pressed, and must be held
      # alongside the key for the combo to trigger.
      unless mods.all? { |mod| mod.in?(@pred) && mod.in?(@succ) } && key.in?(@succ)
        return self
      end

      yield

      InputTransition.new(@pred, @succ, @handled.add(key))
    end

    def handle(key : Term, &) : InputTransition
      handle(Tuple.new, key) { yield }
    end

    def term : Term
      buffer = Pf::Kit.stack_array(Term)
      buffer.reserve(@succ.size)
      @succ.each do |key|
        buffer << key
      end
      buffer.sort! { |a, b| Term.compare(a, b) }

      Term.of(buffer)
    end
  end
end
