lib LibSDL
  fun set_window_resizable = SDL_SetWindowResizable(window : Window*, resizable : Bool) : Bool
  fun get_window_id = SDL_GetWindowID(window : Window*) : UInt32
end

class SDL::Window
  alias Event = LibSDL::WindowEventID

  def id : UInt32
    LibSDL.get_window_id(@window)
  end

  def resizable=(value : Bool) : Bool
    LibSDL.set_window_resizable(@window, value)

    value
  end
end
