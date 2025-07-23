module Ww::Soma::DwUIR
  # Includers are immutable & persistent objects that allow you to "write" using
  # a font face. Pencils track their location through this "typing".
  #
  # The fact that they're persistent lets you "speculate" trivially and very cheaply --
  # you can simply try to type your strings and see where the pencil lands, and make
  # decisions based on that (e.g. commit or roll back -- as simple as storing
  # the previous pencil in a variable).
  module IPencil
    # Returns the location of this pencil's tip. The tip is located at the bottom-
    # left corner of the "box" where the next character will be put.
    #
    # Note that the "bottom" of this "box" is the baseline of the text; it is not
    # the line height.
    #
    # Use `origin` if you want to refer to the top-left corner.
    abstract def tip : Point

    # Returns the location of this pencil's tip specifically for character *ch*. This
    # takes into account the kerning computed from the previous character that this
    # pencil wrote and *ch*.
    abstract def tip(ch : Char) : Point

    # Returns the top-left corner of the "box" where the next character will be put.
    abstract def origin : Point

    # Measures the width of *object* by writing it using this pencil.
    #
    # NOTE: This method will raise `ArgumentError` if *object* contains newlines.
    def measure(object) : Float32
      pencil1 = after_writing(object)

      unless pencil1.tip.y == tip.y
        raise ArgumentError.new
      end

      (pencil1.tip.x - tip.x).ceil
    end

    # Returns the bounding box of *object* with respect to this pencil.
    #
    # NOTE: This method will raise `ArgumentError` if *object* contains newlines.
    def after_writing_with_bounds(object) : {IPencil, Rect}
      pencil1 = after_writing(object)

      unless pencil1.tip.y == tip.y
        raise ArgumentError.new
      end

      {pencil1, Rect.new(origin, pencil1.origin + Point[0, line_height])}
    end

    # Measures and returns the line height for this pencil.
    def line_height : Float32
      pencil1 = after_writing('\n')
      (pencil1.tip.y - tip.y).ceil
    end

    # Measures and returns the width of the ASCII whitespace character for
    # this pencil.
    def space_width : Float32
      measure(' ')
    end

    # Returns the location of this pencil's tip.
    #
    # See also: `tip`.
    def to_point : Point
      tip
    end

    # Returns a copy of this pencil after writing *ch* at this pencil's tip.
    abstract def after_writing(ch : Char) : IPencil

    # Returns a copy of this pencil after writing *string* at this pencil's tip.
    def after_writing(string : String | StringView) : IPencil
      pencil = self
      string.each_char do |ch|
        pencil = pencil.after_writing(ch)
      end
      pencil
    end
  end

  # A generic way to ask something -- usually a "pencil server" -- to return
  # a pencil with the given configuration.
  #
  # - *font* specifies an absolute path to the font file of one of the types
  #   supported by the "pencil server".
  # - *size* specifies the font's size.
  # - *leading* specifies the line height (named after Tailwind).
  # - *tracking* specifies the letter spacing (named after Tailwind).
  record PencilRequest,
    font : Path,
    size : Float32,
    leading : Magn = Magn.rel(1.0),
    tracking : Magn = Magn.rel(0.0)

  alias PencilServer = PencilRequest -> IPencil
end
