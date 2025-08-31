require "sdl"
require "./sdl/patches"
require "./sdl/window"

SDL.set_hint("SDL_NO_SIGNAL_HANDLERS", "1")
SDL.set_hint("SDL_QUIT_ON_LAST_WINDOW_CLOSE", "0")
SDL.init(SDL::Init::VIDEO)
at_exit { SDL.quit }
