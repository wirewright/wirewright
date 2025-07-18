# Alpha-48 is a reduced version of base-62, omitting visually ambiguous symbols
# and digits. This is useful for generating short and always-valid-as-WwML
# identifiers. Alpha-48 is used in rule (`◇`) and block (`▢`) ids.
module Ww::Alpha48
  extend self

  # :nodoc:
  TABLE = Slice[
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "m", "n", "o", "p",
    "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E",
    "F", "G", "H", "J", "K", "L", "M", "N", "P", "Q", "R", "T", "U", "V", "W",
    "X", "Y", "Z", "ba", "bb", "bc", "bd", "be", "bf", "bg", "bh", "bi", "bj",
    "bk", "bm", "bn", "bo", "bp", "bq", "br", "bs", "bt", "bu", "bv", "bw",
    "bx", "by", "bz", "bA", "bB", "bC", "bD", "bE", "bF", "bG", "bH", "bJ",
    "bK", "bL", "bM", "bN", "bP", "bQ", "bR", "bT", "bU", "bV", "bW", "bX",
    "bY", "bZ", "ca", "cb", "cc", "cd", "ce", "cf", "cg", "ch", "ci", "cj",
    "ck", "cm", "cn", "co", "cp", "cq", "cr", "cs", "ct", "cu", "cv", "cw",
    "cx", "cy", "cz", "cA", "cB", "cC", "cD", "cE", "cF", "cG", "cH", "cJ",
    "cK", "cL", "cM", "cN", "cP", "cQ", "cR", "cT", "cU", "cV", "cW", "cX",
    "cY", "cZ", "da", "db", "dc", "dd", "de", "df", "dg", "dh", "di", "dj",
    "dk", "dm", "dn", "do", "dp", "dq", "dr", "ds", "dt", "du", "dv", "dw",
    "dx", "dy", "dz", "dA", "dB", "dC", "dD", "dE", "dF", "dG", "dH", "dJ",
    "dK", "dL", "dM", "dN", "dP", "dQ", "dR", "dT", "dU", "dV", "dW", "dX",
    "dY", "dZ", "ea", "eb", "ec", "ed", "ee", "ef", "eg", "eh", "ei", "ej",
    "ek", "em", "en", "eo", "ep", "eq", "er", "es", "et", "eu", "ev", "ew",
    "ex", "ey", "ez", "eA", "eB", "eC", "eD", "eE", "eF", "eG", "eH", "eJ",
    "eK", "eL", "eM", "eN", "eP", "eQ", "eR", "eT", "eU", "eV", "eW", "eX",
    "eY", "eZ", "fa", "fb", "fc", "fd", "fe", "ff", "fg", "fh", "fi", "fj",
    "fk", "fm", "fn", "fo", "fp", "fq",
  ]

  # Appends the base alpha-48 representation of *object* to *io*.
  def encode(io, object) : Nil
    bytesize = (object.bit_length + 7) // 8

    until bytesize.zero?
      io << TABLE.unsafe_fetch((object >> bytesize) & 0xff)
      bytesize -= 1
    end
  end

  # Returns the base alpha-48 representation of *object* as a string.
  def encode(object) : String
    String.build { |io| encode(io, object) }
  end
end
