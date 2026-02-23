# Alpha-48 is a reduced version of base-62, omitting visually ambiguous symbols
# and all of digits (to avoid tricky starts-with-digit situations).
#
# Base alpha-48 is useful for generating identifiers that are short and always
# valid when treated as WwML.
#
# Base alpha-48 is used in rule (`◇`) and block (`▢`) ids.
module Ww::Alpha48
  extend self

  # :nodoc:
  #
  # Maps byte 0-255 to the corresponding sequence of digits in base alpha-48.
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

  def encode(io, object : UInt8) : Nil
    io << TABLE.unsafe_fetch(object)
  end

  def encode(io, object : Bytes) : Nil
    object.each { |byte| encode(io, byte) }
  end

  # Appends the base alpha-48 representation of *object* to *io* (Big Endian).
  def encode(io, object : Int32 | UInt32) : Nil
    bytesbuf = uninitialized UInt8[4]
    bytes = bytesbuf.to_slice
    IO::ByteFormat::BigEndian.encode(object, bytes)

    # Skip leading zeros.
    while bytes.size > 1 && bytes.first == 0
      bytes += 1
    end

    encode(io, bytes)
  end

  # :ditto:
  def encode(io, object : Int64 | UInt64) : Nil
    bytesbuf = uninitialized UInt8[8]
    bytes = bytesbuf.to_slice
    IO::ByteFormat::BigEndian.encode(object, bytes.to_slice)

    # Skip leading zeros.
    while bytes.size > 1 && bytes.first == 0
      bytes += 1
    end

    encode(io, bytes)
  end

  # Returns the base alpha-48 representation of *object* as a string.
  def encode(object) : String
    String.build { |io| encode(io, object) }
  end
end
