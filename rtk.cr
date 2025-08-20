module Rtk
  extend self

  alias R = Char::Reader*

  class Error < Exception
  end

  # TODO: line, column
  def err(r, message : String = "parse error")
    raise Error.new(message)
  end

  def chr(r)
    r.value.current_char
  end

  def ahead?(r, & : Char -> Bool) : Bool
    yield chr(r)
  end

  def ahead?(r, charset : String | StringView) : Bool
    charset = charset.to_s

    ahead?(r, &.in_set?(charset))
  end

  def ahead?(r, charset : Char) : Bool
    ahead?(r) { |char| char == charset }
  end

  def aheadsequ?(r, sequ : String | StringView) : Bool
    save(r) do
      sequ.each_char do |char|
        return false unless ahead?(r, char)
        forward(r)
      end
    end

    true
  end

  def forward(r) : Bool
    char = chr(r)
    if at_end?(r)
      return false
    end
    r.value.next_char
    true
  end

  def skip?(r, arg)
    if ahead?(r, arg)
      forward(r)

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

  def view(r, &)
    b = r.value.pos
    yield
    e = r.value.pos

    StringView.new(r.value.string, b, e, r.value.string.single_byte_optimizable?)
  end

  def view2(r, &)
    b = r.value.pos
    result = yield
    e = r.value.pos

    {result, StringView.new(r.value.string, b, e, r.value.string.single_byte_optimizable?)}
  end

  def capture(r, io, &)
    io << view(r) { yield }
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
    unless ahead?(r, charset)
      err(r, "#{error}")
    end

    forward(r)
  end

  def expectsequ(r, sequ : String | StringView)
    sequ.each_char do |char|
      expect(r, char, error: "expected #{sequ}")
    end
  end

  def skip(r, charset)
    while ahead?(r, charset)
      forward(r)
    end
  end

  def thru(r, & : Char -> Bool)
    while ahead?(r) { |char| yield char }
      forward(r)
    end
  end

  def thru(r, arg)
    while ahead?(r, arg)
      forward(r)
    end
  end

  def past?(r, arg)
    if ahead?(r, arg)
      forward(r)
      true
    else
      false
    end
  end

  def pastsequ?(r, sequ)
    if aheadsequ?(r, sequ)
      sequ.each_char do |chr|
        break if chr == '\0'

        r.value.next_char
      end

      true
    else
      false
    end
  end

  def skip_to(r, charset) : Bool
    loop do
      return true if ahead?(r, charset)
      return false unless forward(r)
    end
  end

  def bytespan(r, &) : Int32
    p0 = r.value.pos
    yield
    p1 = r.value.pos
    p1 - p0
  end

  def pos(r)
    r.value.pos
  end

  def ahead0(r)
    Rtk.view(r) { }
  end

  def ahead1(r)
    Rtk.save(r) { Rtk.view(r) { Rtk.forward(r) } }
  end

  def rest(r)
    b = r.value.pos
    e = r.value.max_pos

    StringView.new(r.value.string, b, e, r.value.string.single_byte_optimizable?)
  end

  def hexdigit?(r) : Int32?
    return unless Rtk.ahead?(r, "0-9a-fA-F")

    char = Rtk.view(r) { Rtk.forward(r) }
    char[0].to_i(base: 16)
  end
end

struct Char::Reader
  def max_pos : Int32
    @string.bytesize
  end
end
