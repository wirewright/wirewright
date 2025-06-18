module Ww::ML::Grammar
  extend self

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

    def skip(lquote : String?, rquote : String?, & : Char -> Bool) : State
      change(feed: @feed.skip(lquote, rquote) { |ch| yield ch })
    end

    def view_upto(ahead : State) : StringView
      ahead.@feed - @feed
    end
  end

  alias P = Parselet(State)
end

require "./grammar/symbol"
require "./grammar/boolean"
require "./grammar/number"
require "./grammar/escape"
require "./grammar/string"
require "./grammar/pairspattern"
