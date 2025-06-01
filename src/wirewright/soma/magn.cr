module Ww::Soma
  # |@ soma.dwuir.magnitude
  #
  # |@block
  # There are currently two kinds of supported magnitudes:
  #
  #   - Absolute. This usually means pixels. Use `(= <value>)` to force.
  #   - Relative. Interpreted relatively to some node property. Usually
  #     that property is some kind of size: width, height, text size, etc.
  #     Use `(* <value>)` to force.
  #
  # Each node may define raw number values to stand for an absolute or relative
  # magnitude (e.g. whether `1.5` is `(= 1.5)` or `(* 1.5)`). The node should tell
  # about that in its documentation.
  # |@endblock

  # Represents a *magnitude* of some `Kind`. *value* is interpreted depending
  # on the kind of the magnitude.
  record Magn, kind : Kind, value : Float32 do
    enum Kind
      Absolute
      Relative
    end

    # Constructs a relative magnitude with *value*
    def self.rel(value : Float32) : Magn
      new(:relative, value)
    end

    # Constructs an absolute magnitude with *value*
    def self.abs(value : Float32) : Magn
      new(:absolute, value)
    end

    # Constructs a `Magn` object from the given *term*. Raw numbers are treated
    # as **absolute** values. *fallback* is returned when *term* cannot be parsed.
    def self.abst(term : Term, fallback : Magn)
      if term.type.number? # Fast path
        return abs(term.to(Float32))
      end

      Term.case(term) do
        matchpi %{(* value_number)} { rel(value.to(Float32)) }
        matchpi %{(= value_number)} { abs(value.to(Float32)) }
        otherwise { fallback }
      end
    end

    # Constructs a `Magn` object from the given *term*. Raw numbers are treated
    # as **relative** values. *fallback* is returned when *term* cannot be parsed.
    def self.relt(term : Term, fallback : Magn) : Magn
      if term.type.number? # Fast path
        return rel(term.to(Float32))
      end

      Term.case(term) do
        matchpi %{(* value_number)} { rel(value.to(Float32)) }
        matchpi %{(= value_number)} { abs(value.to(Float32)) }
        otherwise { fallback }
      end
    end

    # Resolves this magnitude using *base*.
    def resolve(base : Float32) : Float32
      case kind
      in .relative? then value * base
      in .absolute? then value
      end
    end
  end
end
