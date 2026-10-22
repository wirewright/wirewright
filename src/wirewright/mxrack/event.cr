module Ww::MxRack
  alias Event = ResourceChanged | MousePlugged | MouseUnplugged | WindowReceived

  defrecord ResourceChanged
  defrecord MousePlugged, mouse_id : UInt64
  defrecord MouseUnplugged, mouse_id : UInt64
  defrecord WindowReceived, session_id : UInt64, message : WindowMessage | MouseMessage | KeyboardMessage

  alias WindowMessage = WindowSizeChanged | WindowHoverChanged | WindowFocusChanged | WindowFormChanged

  defrecord WindowSizeChanged, width : UInt32, height : UInt32
  defrecord WindowHoverChanged, hover : Bool
  defrecord WindowFocusChanged, focus : Bool
  defrecord WindowFormChanged, form : WindowForm

  alias MouseMessage = MouseMoved | MouseButtonPressed | MouseButtonReleased

  defrecord MouseMoved, mouse_id : UInt64, x : Magnitude, y : Magnitude
  defrecord MouseButtonPressed, mouse_id : UInt64, button : Term, clicks : UInt32
  defrecord MouseButtonReleased, mouse_id : UInt64, button : Term

  alias KeyboardMessage = KeyPressed | KeyReleased | RunePressed | RuneReleased

  defrecord KeyPressed, keyboard_id : UInt64, key : Term
  defrecord KeyReleased, keyboard_id : UInt64, key : Term
  defrecord RunePressed, rune : Term
  defrecord RuneReleased, rune : Term

  class EventQueue
    def initialize(@alert : BlockingSignal)
      @queue = Deque(Event).new
      @lock = Sync::Mutex.new
    end

    def empty? : Bool
      @lock.synchronize { @queue.empty? }
    end

    def <<(event : Event) : self
      @lock.synchronize { @queue << event }
      @alert.call
      self
    end

    def shift? : Event?
      @lock.synchronize { @queue.shift? }
    end
  end
end
