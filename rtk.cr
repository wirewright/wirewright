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
    charset = charset.to_s

    peek?(r, &.in_set?(charset))
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

  def skip?(r, arg)
    if peek?(r, arg)
      advance(r)

      true
    else
      false
    end
  end

  def at_start?(r)
    !r.value.has_previous?
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

  # Expects the block to return true or truthy. Otherwise rolls back.
  def txn(r, &)
    v0 = r.value
    begin
      res = yield
    ensure
      unless res
        r.value = v0
      end
    end
  end

  def save(r, &)
    txn(r) { return yield }
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

  def thru(r, arg)
    while peek?(r, arg)
      advance(r)
    end
  end

  def past?(r, arg)
    if peek?(r, arg)
      advance(r)
      true
    else
      false
    end
  end

  def pastsequ?(r, sequ)
    if peeksequ?(r, sequ)
      sequ.bytesize.times { r.value.next_char }
      true
    else
      false
    end
  end

  def skip_to(r, charset)
    until peek?(r, charset)
      advance(r)
    end
  end

  def bytespan(r, &) : Int32
    p0 = r.value.pos
    yield
    p1 = r.value.pos
    p1 - p0
  end
end
