module Rtk
  extend self

  alias R = Char::Reader*

  class Error < Exception
  end

  # TODO: line, column
  def err(r, message : String = "parse error")
    raise Error.new(message)
  end

  def peek?(r, & : Char -> Bool) : Bool
    yield r.value.current_char
  end

  def peek?(r, charset : String | StringView) : Bool
    peek?(r, &.in?(charset))
  end

  def peek?(r, charset : Char) : Bool
    peek?(r) { |char| char == charset }
  end

  def peeksequ?(r, sequ : String | StringView) : Bool
    save(r) do
      sequ.each_char do |char|
        return false unless peek?(r, char)
        advance(r)
      end
    end

    true
  end

  def advance(r)
    char = r.value.current_char
    if at_end?(r)
      err(r, "unexpected end-of-input")
    end
    r.value.next_char
    char
  end

  def at_end?(r)
    !r.value.has_next?
  end

  def capture_view(r, &)
    b = r.value.pos
    yield
    e = r.value.pos

    StringView.new(r.value.string, b, e)
  end

  def capture(r, io, &)
    io << capture_view(r) { yield }
  end

  def capture(r, &) : String
    String.build do |io|
      capture(r, io) { yield }
    end
  end

  def save(r, &)
    pos = r.value.pos
    begin
      yield
    ensure
      r.value.pos = pos
    end
  end

  def expect(r, charset, *, error = "expected one of #{charset.inspect}")
    unless peek?(r, charset)
      err(r, "#{error}")
    end

    advance(r)
  end

  def expectsequ(r, sequ : String | StringView)
    sequ.each_char do |char|
      expect(r, char, error: "expected #{sequ}")
    end
  end

  def skip(r, charset)
    while peek?(r, charset)
      advance(r)
    end
  end

  def thru(r, & : Char -> Bool)
    while peek?(r) { |char| yield char }
      advance(r)
    end
  end

  def skip_to(r, charset)
    until peek?(r, charset)
      advance(r)
    end
  end

  def column(r) : Int32
    save(r) do
      column = 0
      r.value.reverse_each do |char|
        break if char == '\n'
        column += 1
      end
      column
    end
  end
end
