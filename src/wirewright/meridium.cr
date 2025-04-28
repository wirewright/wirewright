# Meridium is a subsystem of Wirewright responsible for interacting with
# the outside world via the network. It is the general abstraction Wirewright
# employs for networking.
#
# Clients and servers of the termspace, internal emergent entities such as
# `Xgraph`, surfaces (`Sensor`s & `Appearance`s), and lots of other network-
# related things are within the domain of Meridium.
module Ww::Meridium
  extend self

  # Implementations can capture atoms coming from various sources such as `Utrie`,
  # `Xgraph`, etc.
  module IAtomAppend
    abstract def <<(atom : Atom)
  end

  # Implementations can be queried about the presence of `Atom`s.
  module IAtomsPresent
    # Returns a BitList indicating whether each atom, rendered from *objects*
    # via the block, exists.
    #
    # Guarantees the resulting bit list matches the size of *objects*.
    abstract def present?(objects : Enumerable(T), & : T -> Atom | Enumerable(Atom)) : BitList forall T
  end

  # :nodoc:
  ST = Fiber::ExecutionContext::SingleThreaded.new("Meridium single-threaded")

  # :nodoc:
  MT = Fiber::ExecutionContext::MultiThreaded.new("Meridium multi-threaded", System.cpu_count.to_i)

  # This enum acts as a "scope", protecting atoms of various Meridium entities
  # from clashes.
  enum Entity : UInt8
    Utrie
    Xgraph
    SensorRegistry
    AppearanceRegistry
  end

  # The following are internal "hash step" functions. We need to make sure
  # they're efficient. One particular thing I've found out is BLAKE3 likes
  # one update() call and that's it; a sequence of update()s appears to cost
  # more than just one update(). So in these h() methods we often have a scratch
  # buffer where we copy all inputs and then pass that to BLAKE3 through update().
  # This partly explains the clumsiness & the amount of similarly looking defs.

  private def h0 : Atom
    scratch = uninitialized UInt8[Atom::BYTESIZE]

    hasher = Atom::HASHER.new
    hasher.final(scratch.to_slice)

    Atom.of(scratch.to_slice)
  end

  # :nodoc:
  NULL_HASH_ATOM = h0

  # :nodoc:
  def h(hasherptr) : Atom
    NULL_HASH_ATOM
  end

  # :nodoc:
  def h(hasherptr, a : Nil) : Atom
    h(hasherptr)
  end

  # :nodoc:
  def h(hasherptr, a : Entity) : Atom
    scratch = uninitialized UInt8[Atom::BYTESIZE]

    hasherptr.value.reset
    hasherptr.value.update(a.value)
    hasherptr.value.final(scratch.to_slice)

    Atom.of(scratch.to_slice)
  end

  # :nodoc:
  def h(hasherptr, a : Term) : Atom
    scratch = uninitialized UInt8[Atom::BYTESIZE]

    hasherptr.value.reset

    io = IO::ByteStream.new { |slice| hasherptr.value.update(slice) }
    ML.compact(io, a)

    hasherptr.value.final(scratch.to_slice)

    Atom.of(scratch.to_slice)
  end

  # :nodoc:
  def h(hasherptr, a : Atom, b : Bytes) : Atom
    scratch = uninitialized UInt8[Atom::BYTESIZE]

    a.copy_hash_to(scratch.to_slice)

    hasherptr.value.reset
    hasherptr.value.update(scratch.to_slice)
    hasherptr.value.update(b)
    hasherptr.value.final(scratch.to_slice)

    Atom.of(scratch.to_slice)
  end

  # :nodoc:
  def h(hasherptr, a : Atom, b : Atom) : Atom
    {% begin %}
      scratch = uninitialized UInt8[{{Atom::BYTESIZE * 2}}]

      a.copy_hash_to(scratch.to_slice[0, Atom::BYTESIZE])
      b.copy_hash_to(scratch.to_slice[Atom::BYTESIZE, Atom::BYTESIZE])

      hasherptr.value.reset
      hasherptr.value.update(scratch.to_slice)
      hasherptr.value.final(scratch.to_slice)

      Atom.of(scratch.to_slice[0, Atom::BYTESIZE])
    {% end %}
  end

  # :nodoc:
  def h(hasherptr, a : Atom, b : UInt8) : Atom
    {% begin %}
      scratch = uninitialized UInt8[{{Atom::BYTESIZE + 1}}]

      a.copy_hash_to(scratch.to_slice[0, Atom::BYTESIZE])
      scratch[-1] = b

      hasherptr.value.reset
      hasherptr.value.update(scratch.to_slice)
      hasherptr.value.final(scratch.to_slice[0, Atom::BYTESIZE])

      Atom.of(scratch.to_slice[0, Atom::BYTESIZE])
    {% end %}
  end

  # :nodoc:
  def h(hasherptr, a : UInt8, b : Atom) : Atom
    {% begin %}
      scratch = uninitialized UInt8[{{Atom::BYTESIZE + 1}}]

      scratch[0] = a
      b.copy_hash_to(scratch.to_slice[1, Atom::BYTESIZE])

      hasherptr.value.reset
      hasherptr.value.update(scratch.to_slice)
      hasherptr.value.final(scratch.to_slice[0, Atom::BYTESIZE])

      Atom.of(scratch.to_slice[0, Atom::BYTESIZE])
    {% end %}
  end

  # :nodoc:
  def h(hasherptr, a : Atom, b : Ubase::Any) : Atom
    scratch = uninitialized UInt8[Atom::BYTESIZE]

    a.copy_hash_to(scratch.to_slice)

    hasherptr.value.reset
    hasherptr.value.update(scratch.to_slice)

    Ubase.update(hasherptr, b)

    hasherptr.value.final(scratch.to_slice)

    Atom.of(scratch.to_slice)
  end

  # :nodoc:
  def h(hasherptr, a : Atom, b : Nil) : Atom
    h(hasherptr, a, h(hasherptr))
  end

  # :nodoc:
  def h(hasherptr, entity : Entity, a, b) : Atom
    h(hasherptr, h(hasherptr, entity.value, a), b)
  end

  # :nodoc:
  def secret_to_bytes(secret : Term) : Bytes
    io = IO::Memory.new
    io.write_byte(1)

    ML.compact(io, secret)

    io.to_slice
  end

  # :nodoc:
  def secret_to_bytes(secret : Nil) : Bytes
    Bytes[0]
  end
end

require "./meridium/atom"
require "./meridium/wwid"
require "./meridium/bytes_multimap"
require "./meridium/ubase"
require "./meridium/utrie"
require "./meridium/xgraph"
require "./meridium/sensor_registry"
require "./meridium/appearance_registry"
require "./meridium/surface"
