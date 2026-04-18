module Ww::Scenery
  # Represents a `Magnitude` that should be interpreted as a unit of some `Kind`.
  #
  # Note that this isn't about units in general, like apples or bananas, but graphical
  # units (such as pixels), and a special kind of unit `Kind::Relative`; hence
  # the namespacing under `Scenery`: `Unit` is a Scenery-specific thing.
  struct Unit
    # TODO: Em? Rem? Cm? Mm?
    enum Kind
      # See `scenery.unit.px`.
      Px

      # See `scenery.unit.rel`. Relative values are always mapped to
      # the unit range even if the user enters e.g. `(55 %)` -- i.e.,
      # we'd store `0.55` *not* `55`.
      Relative
    end

    # Returns what kind of unit this is.
    getter kind : Kind

    # Returns the magnitude.
    getter magn : Magnitude

    # :nodoc:
    def initialize(@kind, @magn)
    end

    # Constructs a *relative* unit with the given magnitude.
    def self.rel(magn : Magnitude) : Unit
      new(:relative, magn)
    end

    # Recognizes the unit expressed in *term*, treating bare numbers as
    # relative. If recognition fails, returns *fallback*.
    def self.rel(term : Term, fallback : Unit) : Unit
      if term.type.number?
        return rel(term.to(Magnitude))
      end

      recognize(term, fallback)
    end

    # Construct a pixels unit with the given magnitude.
    def self.px(magn : Magnitude) : Unit
      new(:px, magn)
    end

    # Recognizes the unit expressed in *term*, treating bare numbers as pixel
    # values. If recognition fails, returns *fallback*.
    def self.px(term : Term, fallback : Unit)
      if term.type.number?
        return px(term.to(Magnitude))
      end

      recognize(term, fallback)
    end

    # Recognizes the unit expressed in *term*. If recognition fails, returns *fallback*.
    # See also: `scenery.unit`.
    def self.recognize(term : Term, fallback : Unit) : Unit
      Term.case(term) do
        # |@ scenery.unit.rel
        #
        # |@pattern
        # (* ±magn)
        #
        # |@key magn
        # Magnitude. The value is not bounded, but normally it is in the unit
        # range 0-1. Occasionally you may want it slightly below or above.
        #
        # |@block
        # Expresses a *relative* value. Relative values work like `%` in CSS. Their
        # meaning is context-dependent, and is usually explicitly pointed out in
        # the docs (as in, *relative to what?*)
        matchpi %{(* ±magn)} do
          rel(magn.to(Magnitude))
        end

        # |@ scenery.unit.rel
        #
        # |@pattern
        # (±magn %)
        #
        # |@key magn
        # Magnitude. The value is not bounded, but normally it is in the range
        # range 0-100. Occasionally you may want it slightly below or above
        # (e.g. `(105 %)`, `(230 %)` etc.)
        matchpi %{(±magn %)} do
          rel(magn.to(Magnitude) / 100)
        end

        # |@ scenery.unit.px
        #
        # |@pattern
        # (±magn px)
        #
        # |@key magn
        # Magnitude. The value is not bounded, but may be clamped downstream by
        # the implementation.
        #
        # |@block
        # Expresses a value in pixels.
        matchpi %{(±magn px)} do
          px(magn.to(Magnitude))
        end

        otherwise { fallback }
      end
    end

    # Tries to resolve this unit without knowing the base value.
    #
    # This can be used if calculating the base value is expensive; if the unit is
    # absolute (e.g. pixels), you'd be able to skip the calculation.
    def resolve? : Magnitude?
      case @kind
      in .relative?
      in .px? then @magn
      end
    end

    # Resolves this unit using the given *base* value.
    def resolve(base : Magnitude) : Magnitude
      case @kind
      in .relative? then @magn * base
      in .px?       then @magn
      end
    end

    def inspect(io)
      case @kind
      in .relative? then io << "rel(" << @magn << ")"
      in .px?       then io << "(" << @magn << " px)"
      end
    end
  end
end
