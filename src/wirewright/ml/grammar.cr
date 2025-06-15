class Ww::ML::Grammar
  # :nodoc:
  macro rule(name, result, &)
    getter({{name.id}} : P::Pi(State -> { {{result}}, State } | P::Err)) do
      {{yield}}
    end
  end

  # :nodoc:
  macro rule(name, &)
    getter({{name.id}} : P::Pi(State -> { P::Ok, State } | P::Err)) do
      {{yield}}
    end
  end

  struct State
    def initialize(@feed : StringView)
    end

    private def_change

    def loc : Int32
      @feed.byte_start
    end

    def current_char? : Char?
      @feed.first_char?
    end

    def at_end? : Bool
      @feed.empty?
    end

    def ahead?(chars : String | StringView) : Bool
      @feed.starts_with?(chars)
    end

    def skip(bytesize : Int32) : State
      change(feed: @feed.skip(bytesize))
    end

    def skip(stopword : String? = nil, & : Char -> Bool) : State
      change(feed: @feed.skip(stopword) { |ch| yield ch })
    end

    def view_upto(ahead : State) : StringView
      ahead.@feed - @feed
    end
  end

  alias P = Parselet(State)
end

require "./grammar/escape"
