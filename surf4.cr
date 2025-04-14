require "./src/wirewright"
require "compress/gzip"
require "log"
require "./surf_common"
require "digest"
require "bit_array"
require "brotli"

Log.setup_from_env(default_level: :warn)

module IAtom
  abstract def reinsert
  abstract def burn : Nil
end

alias AtomArray = Array(IAtom)

module ISet(T)
  abstract def includes?(identity : T) : Bool
  abstract def add(identity : T, atoms : AtomArray) : Nil
  abstract def size? : Int32?

  def subset(cls : St.class) forall St
    SubSet(T, St).new(self)
  end
end

module IMultiset(T)
  include ISet(T)
end

module IChat(M)
  alias Unsubscribe = ->

  abstract def subscribe(address : Label, &recv : M ->) : Unsubscribe
  abstract def send(to receiver : Label, message : M) : Nil
end

class SyncInMemoryChat(M)
  include IChat(M)

  @subscribers = {} of Label => Set(M ->)
  @lock = Mutex.new

  def subscribe(address : Label, &recv : M ->) : Unsubscribe
    @lock.synchronize do
      recvs = @subscribers.put_if_absent(address) { Set(M ->).new }
      recvs << recv
    end

    Unsubscribe.new do
      @lock.synchronize do
        next unless recvs = @subscribers[address]?
        next unless recvs.delete(recv)
        next unless recvs.empty?

        @subscribers.delete(address)
      end
    end
  end

  def send(to receiver : Label, message : M) : Nil
    recvs = @lock.synchronize do
      # Copy receiver procs (if any) so that we can call them outside of the lock,
      # and so that they're "frozen in time".
      @subscribers[receiver]?.try(&.dup)
    end

    return unless recvs

    recvs.each &.call(message)
  end
end

struct SubSet(T, St)
  include ISet(St)

  def initialize(@set : ISet(T))
  end

  # WARNING: delegates to the backing set; this method **does not** return
  # the amount of identities of type `St`!
  def size? : Int32?
    @set.size?
  end

  def includes?(identity : St) : Bool
    @set.includes?(identity.as(T))
  end

  def add(identity : St, atoms : AtomArray) : Nil
    @set.add(identity.as(T), atoms)
  end
end

class SyncInMemoryMultiset(T)
  include IMultiset(T)

  struct Atom(T)
    include IAtom

    def initialize(@set : SyncInMemoryMultiset(T), @identity : T)
    end

    def reinsert
      @set.reinsert(@identity)
    end

    def burn : Nil
      @set.delete(@identity)
    end

    def inspect(io)
      io << "atom(" << @identity << ")"
    end
  end

  @set = {} of T => UInt32
  @lock = Mutex.new

  def includes?(identity : T) : Bool
    @lock.synchronize { @set.has_key?(identity) }
  end

  def size? : Int32?
    @set.size
  end

  def add(identity : T, atoms : AtomArray) : Nil
    @lock.synchronize do
      @set[identity] = (@set[identity]? || 0u32) + 1
    end

    atoms << Atom.new(self, identity)
  end

  protected def reinsert(identity : T) : Nil
    @lock.synchronize do
      @set.put_if_absent(identity, 1)
    end
  end

  protected def delete(identity : T) : Nil
    @lock.synchronize do
      return unless refcount = @set[identity]?

      if refcount == 1
        @set.delete(identity)
      else
        @set[identity] = refcount - 1
      end
    end
  end

  def pretty_print(pp)
    @lock.synchronize do
      pp.list("{", @set, "}") do |item|
        {% if T == ::Bytes %}
          pp.text(Base64.strict_encode(item))
        {% else %}
          item.pretty_print(pp)
        {% end %}
      end
    end
  end
end

class SyncInMemorySet(T)
  include ISet(T)

  struct Atom(T)
    include IAtom

    def initialize(@set : SyncInMemorySet(T), @identity : T)
    end

    def reinsert
      @set.reinsert(@identity)
    end

    def burn : Nil
      @set.delete(@identity)
    end

    def inspect(io)
      io << "atom(" << @identity << ")"
    end
  end

  @set = Set(T).new
  @lock = Mutex.new

  def includes?(identity : T) : Bool
    @lock.synchronize { @set.includes?(identity) }
  end

  def size? : Int32?
    @set.size
  end

  def add(identity : T, atoms : AtomArray) : Nil
    @lock.synchronize { @set << identity }

    atoms << Atom.new(self, identity)
  end

  protected def reinsert(identity : T) : Nil
    @lock.synchronize { @set << identity }
  end

  protected def delete(identity : T) : Nil
    @lock.synchronize { @set.delete(identity) }
  end

  def pretty_print(pp)
    @lock.synchronize do
      pp.list("{", @set, "}") do |item|
        {% if T == ::Bytes %}
          pp.text(Base64.strict_encode(item))
        {% else %}
          item.pretty_print(pp)
        {% end %}
      end
    end
  end
end


alias Fingerprint = Bytes

DIGEST_ALG           = Digest::SHA256
FINGERPRINT_BYTESIZE = 32

record Label, value : UInt128 do
  include Comparable(Label)

  class_getter zero = Label.new(0u128)

  def <=>(other : Label)
    value <=> other.value
  end

  # Returns the Big Endian encoding of the raw label value.
  def to_slice_be : Bytes
    slice = Bytes.new(value.byte_size)

    append_be(slice)

    slice
  end

  def append_be(dst : Bytes) : Bytes
    IO::ByteFormat::BigEndian.encode(value, dst)

    dst + Label.bytesize
  end

  def self.bytesize
    16
  end

  def self.from_slice_be?(bytes : Bytes) : Label?
    return unless bytes.size == Label.bytesize

    new(IO::ByteFormat::BigEndian.decode(UInt128, bytes))
  end

  def complete(digit, *, base, index)
    Label.new(value &+ (digit &* base**index))
  end

  def each_prefix_with_index(*, base, max, &)
    state = 0u128

    # NOTE: since we're using UUIDs (ish) as opposed to a counter, we don't
    # have the "long zeros prefix" problem where we store lots of zeros redundantly.
    # So we don't have to think about using a variable length encoding.
    (0...max).reverse_each do |index|
      digit = (value // (base ** index)) % base
      state &+= digit &* base**index
      yield Label.new(state), index.to_u8
    end
  end

  def inspect(io)
    io << "#'"
    value.to_s(io, base: 62, precision: 22)
  end

  def to_s(io)
    inspect(io)
  end

  # Reference: https://crypto.stackexchange.com/questions/109848/what-is-the-fastest-stable-128-bit-non-cryptographic-hash-function#comment235778_109848
  # Reference: https://crypto.stackexchange.com/questions/109848/what-is-the-fastest-stable-128-bit-non-cryptographic-hash-function#comment235789_109848
  def hash(hasher)
    a = (@value >> 64).to_u64
    b = (@value << 64 >> 64).to_u64

    (a.rotate_left(1) &+ b).hash(hasher)
  end
end

alias LabelGenerator = (-> Label) | ILabelGenerator

module ILabelGenerator
  abstract def call : Label
end

# An extremely simple globally unique id source.
#
# - The first 64 bits are used for nanoseconds since the Unix epoch.
# - The second 64 bits are randomness.
#
# Time comes first (most significant) to allow for better packing, since
# prefix digits are likely to be very similar if not exactly the same.
#
# An obvious problem is maliciously rolling the time back. However, we still
# have randomness to protect us in terms of uniqueness. The time component
# is used to check if one surface (e.g. a sensor) was added before another.
# Let's say if a sensor S sees an appearance A was added after S was inserted,
# then S won't report A and instead rely on A finding S. So if the time
# component of S or A is incorrect, S will see A when it shouldn't have --
# not a *huge* problem since A is matching S anyway, and has a lot of filtering
# to go through.
#
# TODO: use something more battle-tested
struct WWID
  extend ILabelGenerator

  def self.call : Label
    order = Time.utc.to_unix_ns.to_u128
    randomness = Random::Secure.rand(UInt64)

    Label.new((order << 64) | randomness)
  end
end

alias Strand = Slice(Ubase::Any)
alias StrandList = Slice(Strand)
alias BranchList = Slice(StrandList)

# Ubases are tiny gate-keeper nodes for `Utrie`.
#
# Arbitrary M1 patterns are broken down into `BranchList` (so DNF, which has
# terrible scaling characteristics but still works!) Each branch in the branch
# list is a `StrandList` (so a conjunction of strands; we're DNF, remember?)
# A strand is a sequence of Ubases that create, in effect, a "chain of filters".
# Each Ubase, then, is such a filter. E.g. `IsNum` filters number terms; `Literal`
# filters literal matches. The `At` Ubase, on the other hand, is interesting
# because its output is different from its input.
#
# See `Utrie` to learn more.
module Ubase
  alias Any = At | Trunk | IsSym | IsStr | IsNum | IsBool | IsDict | Literal

  # Passes a dictionary term's value for *key* forward.
  record At, key : Term

  # Positioned at the beginning of all valid strands. Relied upon by
  # match-any patterns such as `_` or `x_`, since they aren't matching
  # anything in particular (so `Trunk` is a NOP in terms of filtering
  # or transformation).
  record Trunk

  # Passes only symbol terms forward.
  record IsSym do
    def self.code
      Bytes[0]
    end
  end

  # Passes only string terms forward.
  record IsStr do
    def self.code
      Bytes[1]
    end
  end

  # Passes only number terms forward.
  record IsNum do
    def self.code
      Bytes[2]
    end
  end

  # Passes only boolean terms forward.
  record IsBool do
    def self.code
      Bytes[3]
    end
  end

  # Passes only dictionary terms forward.
  record IsDict do
    def self.code
      Bytes[4]
    end
  end

  # Passes foward only terms that match *value* exactly.
  record Literal, value : Term

  # Returns the is-type `Ubase` (e.g. `IsNum`) that corresponds to the given
  # `TermType` *type*.
  #
  # Raises `ArgumentError` if *type* is `TermType::Any`.
  def self.from(type : TermType) : Ubase::Any
    case type
    in .any?     then raise ArgumentError.new
    in .boolean? then IsBool.new
    in .number?  then IsNum.new
    in .string?  then IsStr.new
    in .symbol?  then IsSym.new
    in .dict?    then IsDict.new
    end
  end

  def self.update(digest, base : Trunk)
    digest.update(Bytes[1])
  end

  def self.update(digest, base : IsSym)
    digest.update(Bytes[2])
  end

  def self.update(digest, base : IsStr)
    digest.update(Bytes[3])
  end

  def self.update(digest, base : IsNum)
    digest.update(Bytes[4])
  end

  def self.update(digest, base : IsBool)
    digest.update(Bytes[5])
  end

  def self.update(digest, base : IsDict)
    digest.update(Bytes[6])
  end

  def self.update(digest, base : Literal)
    digest.update(Bytes[7])

    io = IO::Digest.new(IO.empty, digest, mode: :write)

    ML.compact(io, base.value)
  end

  def self.update(digest, base : At)
    digest.update(Bytes[8])

    io = IO::Digest.new(IO.empty, digest, mode: :write)

    ML.compact(io, base.key)
  end
end

# Encode to / decode from terms

struct ::Ww::Term
  # WARNING: both encode and decode MUST be compatible with M1's normal form,
  # since we're feeding the normal form directly to `decode` to obtain
  # the corresponding Ubases occasionally.

  def self.encode(src : Ubase::At) : Term
    Term.of(:"%value", {:"%literal", src.key})
  end

  def self.decode?(dst : Ubase::At.class, term : Term) : Ubase::At?
    Term.matchpi?(term, %{(%'%value (%'%literal key_))}) do
      Ubase::At.new(key)
    end
  end

  # :nodoc:
  #
  # NOTE: To retain compatibility with M1's normal form we encode/decode
  # Trunk as %any.
  ENCODED_TRUNK = Term.of({:"%any"})

  # :nodoc:
  ENCODED_IS_SYM = Term.of({:"%symbol"})

  # :nodoc:
  ENCODED_IS_NUM = Term.of({:"%number", :_})

  # :nodoc:
  ENCODED_IS_STR = Term.of({:"%string"})

  # :nodoc:
  ENCODED_IS_DICT = Term.of({:"%dict"})

  # :nodoc:
  ENCODED_IS_BOOL = Term.of({:"%boolean"})

  {% for base in %w(Trunk IsSym IsNum IsStr IsDict IsBool) %}
    def self.encode(src : Ubase::{{base.id}}) : Term
      ENCODED_{{base.underscore.upcase.id}}
    end

    def self.decode?(dst : Ubase::{{base.id}}.class, term : Term) : Ubase::{{base.id}}?
      if term == ENCODED_{{base.underscore.upcase.id}}
        return Ubase::{{base.id}}.new
      end
    end
  {% end %}

  def self.encode(src : Ubase::Literal) : Term
    Term.of(:"%literal", src.value)
  end

  def self.decode?(dst : Ubase::Literal.class, term : Term) : Ubase::Literal?
    Term.matchpi?(term, %{(%'%literal value_)}) do
      Ubase::Literal.new(value)
    end
  end
end

# TODO: better impl like `Ttrie`
struct Utrie
  record Atom, prefix : Fingerprint, base : Ubase::Any do
    def update(digest : Digest) : Nil
      digest.update(prefix)

      Ubase.update(digest, base)
    end

    def inspect(io)
      io << "UtrieAtom["
      Base64.strict_encode(prefix, io)
      io << ", base="
      base.inspect(io)
      io << ")"
    end
  end

  def initialize(@set : ISet(Atom))
  end

  def mount(strand : Strand, atoms : AtomArray) : Fingerprint
    unless strand[0]? == Ubase::Trunk.new
      raise ArgumentError.new("expected a nonempty strand that starts with Trunk")
    end

    digest = DIGEST_ALG.new
    digest.update(Bytes[0]) # FIXME: ?!

    strand.each do |base|
      @set.add(Atom.new(prefix: digest.dup.final, base: base), atoms)

      Ubase.update(digest, base)
    end

    digest.final
  end

  private def advance(state : Atom, base : Ubase::Any) : Atom
    state.copy_with(prefix: state.strand, base: base)
  end

  {% for type, base in {Term::Num => Ubase::IsNum, Term::Str => Ubase::IsStr, Term::Sym => Ubase::IsSym, Term::Boolean => Ubase::IsBool} %}
    private def query(digest, prefix, term : {{type}}, sink)
      return unless @set.includes?(Atom.new(prefix, base: {{base}}.new))

      # If type base is present in the set we append it to `digest` state
      # and call sink with it.
      Ubase.update(digest, {{base}}.new)

      prefix = digest.dup.final

      sink.call(prefix)

      return unless @set.includes?(Atom.new(prefix, base: Ubase::Literal.new(term.upcast)))

      # If literal base is present in the set we append it to `digest` state
      # and call sink with it.
      Ubase.update(digest, Ubase::Literal.new(term.upcast))

      prefix = digest.dup.final

      sink.call(prefix)
    end
  {% end %}

  # NOTE: dictionaries must be normalized to IsDict - At(), even literal ones.
  # We do not handle Literal(dict).
  private def query(digest, prefix, term : Term::Dict, sink) : Nil
    return unless @set.includes?(Atom.new(prefix, base: Ubase::IsDict.new))

    # If type base is present in the set we append it to `digest` state
    # and call sink with it.
    Ubase.update(digest, Ubase::IsDict.new)

    prefix = digest.dup.final

    sink.call(prefix)

    term.each_entry do |key, value|
      next unless @set.includes?(Atom.new(prefix, base: Ubase::At.new(key)))

      subdigest = digest.dup

      # If "at" base is present in the set we append it to `digest` state
      # and call sink with it.
      Ubase.update(subdigest, Ubase::At.new(key))

      subprefix = subdigest.dup.final

      sink.call(subprefix)

      query(subdigest, subprefix, value.downcast, sink)
    end
  end

  private def query(term, sink) : Nil
    digest = DIGEST_ALG.new
    digest.update(Bytes[0])

    prefix = digest.dup.final

    state = Atom.new(prefix, base: Ubase::Trunk.new)
    return unless @set.includes?(state)

    # Append base to prefix to obtain new prefix in the `digest` state.
    # Call sink with it.
    Ubase.update(digest, state.base)

    prefix = digest.dup.final

    sink.call(prefix)

    query(digest, prefix, term.downcast, sink)
  end

  def query(term : Term, &sink : Fingerprint ->) : Nil
    query(term, sink)
  end
end

struct Xtrie
  # Represents a connection between two "facts".
  #
  # Its fingerprint is a higher-order "fact"; that is, the combination of
  # the two basic "facts" *a* and *b*.
  record Atom, a : Fingerprint, b : Fingerprint do
    def update(digest : Digest) : Nil
      digest.update(a)
      digest.update(b)
    end

    def inspect(io)
      io << "XtrieAtom["
      Base64.strict_encode(a, io)
      io << ", "
      Base64.strict_encode(b, io)
      io << "]"
    end
  end

  def initialize(@set : ISet(Atom))
  end

  # Mounts an Xtrie rule (an *xrule*). Returns the fingerprint of the xrule.
  # See also: `Xtrie`.
  #
  # NOTE: *xrule* must be pre-sorted ascending. You lose ownership of *xrule*
  # until this method returns.
  def mount(xrule : Deque(Fingerprint), atoms : AtomArray) : Fingerprint
    if xrule.empty?
      raise ArgumentError.new
    end

    digest = DIGEST_ALG.new

    while xrule.size > 1
      a = xrule.shift
      b = xrule.shift

      @set.add(Atom.new(a, b), atoms)

      digest.reset
      digest.update(a)
      digest.update(b)

      xrule << digest.final
    end

    xrule[0]
  end

  private def conjs(digest, vertices, sink) : Nil
    while a = vertices.shift?
      sink.call(a)

      (0...vertices.size).each do |i|
        b = vertices.unsafe_fetch(i)
        next unless @set.includes?(Atom.new(a, b))

        digest.reset
        digest.update(a)
        digest.update(b)

        vertices << digest.final
      end
    end
  end

  # Calls *sink* with all mounted conjunction vertices whose corresponding
  # conjunctions are satisfied by *vertices*.
  #
  # NOTE: *vertices* must be pre-sorted ascending. You lose ownership of *vertices*
  # until this method returns.
  def conjs(vertices : Deque(Fingerprint), &sink : Fingerprint ->) : Nil
    digest = DIGEST_ALG.new

    conjs(digest, vertices, sink)
  end
end

module Ttrie
  # Converts an arbitrary *strand* (keypath plus value) into the corresponding
  # array of fingerprints. Each fingerprint represents a step (hence the name
  # of this method) in the path that leads closer and closer to the value;
  # the final fingerprint is that of the value itself.
  def self.steps(strand : Enumerable(Term)) : Array(Fingerprint)
    tip = nil
    steps = [] of Fingerprint

    strand.each do |term|
      if tip
        steps << Ubase::IsDict.code
        steps << ML.compact(tip).to_slice
      end

      tip = term
    end

    if tip
      type = Ubase.from(tip.type)
      steps << type.class.code
      steps << ML.compact(tip).to_slice
    end

    steps
  end

  # Converts a Ubase *query* into a path prefix.
  def self.steps(query : Indexable(Ubase::Any)) : Array(Fingerprint)
    unless query[0]? == Ubase::Trunk.new
      raise "expected a nonempty query that starts with Trunk"
    end

    path = [] of Fingerprint
    state = :term

    (1...query.size).each do |index|
      base = query[index]

      case state
      when :term
        case base
        when Ubase::IsNum, Ubase::IsStr, Ubase::IsSym, Ubase::IsBool
          state = :type
          path << base.class.code
        when Ubase::IsDict
          state = :dict
          path << base.class.code
        else
          raise ArgumentError.new("malformed query: expected type of term")
        end
      when :type
        case base
        when Ubase::Literal
          path << ML.compact(base.value).to_slice
          state = :end
        else
          raise ArgumentError.new("malformed query: expected Literal after type")
        end
      when :dict
        case base
        when Ubase::At
          path << ML.compact(base.key).to_slice
          state = :term
        else
          raise ArgumentError.new("malformed query: expected At after IsDict")
        end
      when :end
        raise ArgumentError.new("unexpected trailing base: #{base}")
      else
        unreachable
      end
    end

    path
  end
end

struct BytesMultimap(I)
  Log = ::Log.for(self)

  def initialize(@set : ISet(I))
  end

  def mount(byteslice : Bytes, atoms : AtomArray, *, start = 0)
    reader = BitReader.new(byteslice[start..])

    # MutBitWriter is stupid so we have to do this.
    if start.zero?
      writer = MutBitWriter.new
    else
      writer = MutBitWriter.new(byteslice[...start])
    end

    while true
      # Consume one base-4 digit.
      bit0 = reader.consume? || break
      bit1 = reader.consume? || 0u8

      @set.add(I.new(*writer.progress, (bit0 << 1) | bit1), atoms)

      writer << bit0
      writer << bit1
    end
  end

  # Yields possible completions of *prefix* to the block.
  #
  # *prefix* can be empty to yield all values in the multimap.
  def complete(prefix : Bytes, & : Bytes ->)
    workspace = Deque{MutBitWriter.new(prefix)}
    digits = { {0u8, 0u8}, {0u8, 1u8}, {1u8, 0u8}, {1u8, 1u8} }

    while writer0 = workspace.shift?
      answers = digits.map do |(bit0, bit1)|
        if @set.includes?(I.new(*writer0.progress, (bit0 << 1) | bit1))
          {bit0, bit1}
        end
      end

      case answers.count { |item| !item.nil? }
      when 0 # All nil
        yield writer0.final
      when 1 # One non-nil
        answers.each do |answer|
          next unless answer

          # This will only run once
          bit0, bit1 = answer
          writer0 << bit0
          writer0 << bit1
          workspace << writer0
        end
      else # Many non-nil
        last = nil

        answers.each do |last1|
          next unless last1

          last0 = last
          last = last1
          next unless last0

          bit0, bit1 = last0
          writer1 = writer0
          writer1.detach
          writer1 << bit0
          writer1 << bit1
          workspace << writer1
        end

        expect last

        # Reuse writer0 for last answer (e.g. if there were two we'll make just
        # one copy of writer0).
        bit0, bit1 = last
        writer0 << bit0
        writer0 << bit1
        workspace << writer0
      end
    end
  end

  # Returns one possible completion of *prefix*. If *prefix* cannot be completed,
  # will return *prefix* itself (i.e. no completion).
  def complete1(prefix : Bytes) : Bytes
    writer = MutBitWriter.new(prefix)
    digits = { {0u8, 0u8}, {0u8, 1u8}, {1u8, 0u8}, {1u8, 1u8} }

    while true
      completed = false

      digits.each do |(bit0, bit1)|
        next unless @set.includes?(I.new(*writer.progress, (bit0 << 1) | bit1))

        writer << bit0
        writer << bit1
        completed = true

        break
      end

      unless completed
        return writer.final
      end
    end
  end
end

module BytesMultimap::Atom
  abstract def prefix : Bytes

  # 0..64
  abstract def byte : UInt8

  # 0..3
  abstract def cursor : UInt8

  # 0..3
  abstract def digit : UInt8

  def update(digest : Digest) : Nil
    digest.update(prefix)
    digest.update(Bytes[byte, cursor, digit])
  end
end

# One-to-many map for decoding a conjunction vertex fingerprint into unique sensor
# ids that are bound to it.
struct SensorMultimap
  Log = ::Log.for(self)

  record Atom, prefix : Bytes, byte : UInt8, cursor : UInt8, digit : UInt8 do
    include BytesMultimap::Atom

    def inspect(io)
      io << "SensorAtom["
      Base64.strict_encode(prefix, io)
      io << "|" << byte.to_s(2, precision: 8) << "/" << cursor << "^" << digit << "]"
    end
  end

  def initialize(@set : ISet(Atom))
  end

  private def bytesize : Int32
    FINGERPRINT_BYTESIZE + Label.bytesize + sizeof(Checksum)
  end

  # Binds *sensor* to the given conjunction vertex fingerprint *conjv*.
  def bind(conjv : Fingerprint, sensor : Label, atoms : AtomArray) : Nil
    # Just jam them together into one big byteslice. Include a checksum just to
    # be sure; it's not that expensive and we're going to ship higher quality to
    # our callers.
    byteslice0 = byteslice = Bytes.new(bytesize)

    byteslice.copy_from(conjv)
    byteslice += conjv.size

    byteslice = sensor.append_be(byteslice)

    # Compute checksum.
    checksum = Digest::CRC32.checksum(byteslice0[...-sizeof(Checksum)])

    IO::ByteFormat::BigEndian.encode(checksum, byteslice)
    byteslice += sizeof(Checksum)

    multimap = BytesMultimap.new(@set)
    multimap.mount(byteslice0, atoms)
  end

  # Yields sensors bound to the given conjunction vertex fingerprint *conjv*.
  def each_sensor(conjv : Fingerprint, &sink : Label ->) : Nil
    multimap = BytesMultimap.new(@set)
    multimap.complete(conjv) do |row|
      unless row.size == bytesize
        # Do not spam log messages if conjv doesn't have any sensors attached.
        #
        # NOTE: #complete is additive, it won't change bytes in `row`, so a size
        # check is enough.
        next if row.size == conjv.size

        Log.warn { "reject row: size mismatch (#{row.size} != #{bytesize})" }
        next
      end

      # Verify checksum (0 stands for "original")
      checksum0 = IO::ByteFormat::BigEndian.decode(Checksum, row[-sizeof(Checksum)..])
      checksum1 = Digest::CRC32.checksum(row[...-sizeof(Checksum)])

      unless checksum0 == checksum1
        Log.warn { "reject row: checksum mismatch: #{checksum0} (its) != #{checksum1} (my)" }
        next
      end

      label_offset = FINGERPRINT_BYTESIZE

      unless sensor = Label.from_slice_be?(row[label_offset, Label.bytesize])
        Log.warn { "reject row: bad label: #{row.hexstring}" }
        next
      end

      yield sensor
    end
  end
end

struct AppearanceMultimap
  Log = ::Log.for(self)

  record Atom, prefix : Bytes, byte : UInt8, cursor : UInt8, digit : UInt8 do
    include BytesMultimap::Atom

    def inspect(io)
      io << "AppearanceAtom["
      Base64.strict_encode(prefix, io)
      io << "|" << byte.to_s(2, precision: 8) << "/" << cursor << "^" << digit << "]"
    end
  end

  def initialize(@set : ISet(Atom))
  end

  # The row layout is as follows:
  #
  # <steps: N bytes> <appearance id: 16 bytes> <checksum: 4 bytes>
  #
  # We are only interested in the last two fields, in fact.

  def bind(steps : Array(Fingerprint), endpoint : Label, atoms : AtomArray)
    row0 = row = Bytes.new(steps.sum(&.size) + Label.bytesize + sizeof(Checksum))

    # - We then concatenate all steps in the path to get one big lump of steps.
    # - We simultaneously compute the CRC32 checksum of the steps.
    steps.each do |step|
      row.copy_from(step)
      row += step.size
    end

    # Append endpoint.
    row = endpoint.append_be(row)

    # Calculate checksum.
    checksum = Digest::CRC32.checksum(row0[...-sizeof(Checksum)])

    # Append checksum.
    IO::ByteFormat::BigEndian.encode(checksum, row)

    multimap = BytesMultimap.new(@set)
    multimap.mount(row0, atoms)
  end

  def each_appearance(steps : Array(Fingerprint), & : Label ->)
    prefix0 = prefix = Bytes.new(steps.sum(&.size))

    steps.each do |step|
      prefix.copy_from(step)
      prefix += step.size
    end

    multimap = BytesMultimap(Atom).new(@set)
    multimap.complete(prefix0) do |row|
      if row.size < Label.bytesize + sizeof(Checksum)
        # Do not spam log messages if the multimap is empty and someone is trying to
        # query it.
        next if row.size == prefix0.size

        Log.warn { "reject row: size mismatch (#{row.size} < #{Label.bytesize + sizeof(Checksum)})" }
        next
      end

      # Verify checksum (0 stands for "original")
      checksum0 = IO::ByteFormat::BigEndian.decode(Checksum, row[-sizeof(Checksum)..])
      checksum1 = Digest::CRC32.checksum(row[...-sizeof(Checksum)])

      unless checksum0 == checksum1
        Log.warn { "reject row: checksum mismatch: #{checksum0} (its) != #{checksum1} (my)" }
        next
      end

      unless id = Label.from_slice_be?(row[-sizeof(Checksum) - Label.bytesize...-sizeof(Checksum)])
        Log.warn { "reject row: bad label: #{row.hexstring}" }
        next
      end

      yield id
    end
  end
end

struct StrandSet
  record Atom, fingerprint : Fingerprint do
    def update(digest : Digest) : Nil
      digest.update(fingerprint)
    end

    def inspect(io)
      io << "Strand["
      Base64.strict_encode(fingerprint, io)
      io << "]"
    end
  end

  def initialize(@set : ISet(Atom))
  end

  def mount(fingerprint : Fingerprint, atoms : AtomArray) : Nil
    @set.add(Atom.new(fingerprint), atoms)
  end

  def strand?(fingerprint : Fingerprint) : Bool
    @set.includes?(Atom.new(fingerprint))
  end
end

# Tbase (short for *termbase*, whatever that is supposed to mean...) is
# an internal object responsible for orchestrating objects that are even
# more internal (such as `Utrie`, `Xgraph`, `Ttrie`, and so on).
#
# This "orchestration" results in the emergence of *sensors* and *appearances*,
# conceptually grouped into *surfaces*; but througout the operation of Tbase
# referred to as *subjects* as well. With the help of Tbase, you can talk about
# them without having to remember they're just a ton of key-value pairs.
#
# It's like atoms and chairs -- Tbase creates the illusion of "chairs" while
# they are just atoms; and there's something else even more internal (e.g. `Xgraph`)
# that creates a similar illusion of atoms for `Tbase` itself, while what
# they really are is collections of quarks and electrons etc., and so on.
struct Tbase
  alias Atom = Utrie::Atom | Xtrie::Atom | StrandSet::Atom | SensorMultimap::Atom | AppearanceMultimap::Atom

  # Data that `Tbase` needs to know about a sensor.
  record Sensor, id : Label, strands : StrandList do
    # Calls *fn* with each sensor in *pattern*.
    #
    # An arbitrary M1 *pattern* can contain branches (e.g. `%any`) so it is considered
    # to contain multiple sensors.
    def self.each(fresh : LabelGenerator, pattern : Term, &fn : Sensor ->) : Nil
      skeleton = pipe(pattern, M1.normal, M1.skeleton)

      strands = [] of Strand

      M1.branches(skeleton) do |branch|
        M1.strands(branch) do |strand|
          strands << strand.items.to_readonly_slice { |base| Term.decode(Ubase::Any, base) }
        end

        sensor = new(fresh.call, strands.to_readonly_slice(&.itself))

        fn.call(sensor)

        strands.clear
      end
    end
  end

  # Data that `Tbase` needs to know about an appearance.
  record Appearance, id : Label, value : Term

  alias Subject = Sensor | Appearance

  def initialize(@set : ISet(Atom))
  end

  # Constructs a `Utrie` view of this termbase's backing set.
  def utrie : Utrie
    Utrie.new(@set.subset(Utrie::Atom))
  end

  # Constructs an `Xtrie` view of this termbase's backing set.
  def xtrie : Xtrie
    Xtrie.new(@set.subset(Xtrie::Atom))
  end

  # Constructs a `StrandSet` view of this termbase's backing set.
  def strands : StrandSet
    StrandSet.new(@set.subset(StrandSet::Atom))
  end

  # Constructs a `SensorMultimap` view of this termbase's backing set.
  def sensors : SensorMultimap
    SensorMultimap.new(@set.subset(SensorMultimap::Atom))
  end

  # Constructs an `AppearanceMultimap` view of this termbase's backing set.
  def appearances : AppearanceMultimap
    AppearanceMultimap.new(@set.subset(AppearanceMultimap::Atom))
  end

  # Inserts a sensor *subject* into this termbase.
  def mount(subject : Sensor, atoms : AtomArray) : Nil
    endpoints = Deque(Fingerprint).new

    subject.strands.each do |strand|
      endpoint = utrie.mount(strand, atoms)
      endpoints << endpoint
      strands.mount(endpoint, atoms)
    end

    # Sort fingerprints ascending (fingerprints are hash digests in this case).
    endpoints.unstable_sort!

    conjv = xtrie.mount(endpoints, atoms)

    sensors.bind(conjv, subject.id, atoms)
  end

  # Inserts an appearance *subject* into this termbase.
  def mount(subject : Appearance, atoms : AtomArray) : Nil
    Term.each_keypath_and_leaf(subject.value) do |keypath, leaf|
      keypath.push(leaf) do
        # Now we will append the entry byteslice to the appearance multimap. Anyone
        # will be able to "guess their way" through the multimap to obtain the entry
        # slice -- and by the end of it, they would've done the pattern matching &
        # would be holding an appearance id & the CRC32 checksum in their hands.
        appearances.bind(Ttrie.steps(keypath), subject.id, atoms)
      end

      true # continue
    end
  end

  # Calls *sink* with each appearance complement of *subject*.
  #
  # For a sensor, its appearance complements are appearances that the sensor
  # is excited by.
  def each_complement(subject : Sensor, &sink : Label ->) : Nil
    hitsets = [] of Set(Label)

    # NOTE: Unfortunately, the state of the algorithm right now is that we'd have
    # to download all strands and intersect them. This means that unlike sensors,
    # which can make use of e.g. type / discriminator fields, appearances won't
    # be able to do that; so if you have a field whose value is only found on one
    # appearance, and then more fields that are found on e.g. hundreds of thousands
    # of appearances, we'd be forced to download all those hundreds of thousands of
    # appearances (their ids) and do an intersection to find out only one of them
    # matches, based solely on the discriminator field.
    #
    # Stupid, but this is somehow fundamentally related to CNF/DNF. Appearances
    # are disjunctive; the presence of any number of matching fields is enough to
    # trigger an appearance. Sensors, on the other hand, are conjunctive; all fields
    # that a sensor matches must be present. So we'd be forced to index all possible
    # endpoint subsets for an appearance (combinations of field presences); which explodes
    # really really quickly, like, past 5 or something. I feel this is deeply related
    # to a similar explosion we see in CNF->DNF.

    subject.strands.each do |strand|
      hits = Set(Label).new

      # This is where the "download" described above may occur.
      appearances.each_appearance(Ttrie.steps(strand)) do |hit|
        hits << hit
      end

      return if hits.empty?

      hitsets << hits
    end

    return unless hitsets.size == subject.strands.size # Sanity

    hitsets.unstable_sort_by!(&.size)
    hitsets[0].each do |candidate|
      # Make sure the candidate is in all sets (matches all strands of the sensor).
      next unless (1...hitsets.size).all? { |index| candidate.in?(hitsets[index]) }

      sink.call(candidate)
    end
  end

  # Calls *sink* with each sensor complement of *subject*.
  #
  # For an appearance, its sensor complements are sensors that the appearance excites.
  def each_complement(subject : Appearance, &sink : Label ->) : Nil
    hits = Deque(Fingerprint).new

    utrie.query(subject.value) do |hit|
      next unless strands.strand?(hit)

      hits << hit
    end

    hits.unstable_sort!

    xtrie.conjs(hits) do |conjv|
      sensors.each_sensor(conjv, &sink)
    end
  end
end

alias Slot = UInt32
alias Checksum = UInt32

record SensorInfo, conid : Label, grpid : Label, slot : Slot

struct SensorRegistry
  Log = ::Log.for(self)

  record Atom, prefix : Bytes, byte : UInt8, cursor : UInt8, digit : UInt8 do
    include BytesMultimap::Atom

    def inspect(io)
      io << "SensorRegistryAtom["
      Base64.strict_encode(prefix, io)
      io << "|" << byte.to_s(2, precision: 8) << "/" << cursor << "^" << digit << "]"
    end
  end

  def initialize(@set : ISet(Atom))
  end

  # SensorInfo row format is as follows:
  #
  # frag   <instant> <secret> : <conid> <grpid> <slot> <checksum>
  # bytes      16      0..       16       16     4           4
  #
  # Checksum is calculated for the entire row, i.e. for instant and secret
  # as well. This way, both sides can have some certainty & proceed with their
  # doings more or less confidently.

  private def bytesize(secret : Bytes) : Int32
    {Label.bytesize, secret.size, Label.bytesize*2, sizeof(Slot), sizeof(Checksum)}.sum
  end

  private def register(instant : Label,
                       secret : Bytes,
                       conid : Label,
                       grpid : Label,
                       slot : Slot,
                       atoms : AtomArray) : Nil
    row = cursor = Bytes.new(bytesize(secret))

    # Append fields.
    cursor = instant.append_be(cursor)
    cursor.copy_from(secret)
    cursor += secret.size
    cursor = conid.append_be(cursor)
    cursor = grpid.append_be(cursor)

    IO::ByteFormat::BigEndian.encode(slot, cursor)
    cursor += sizeof(Slot)

    checksum = Digest::CRC32.checksum(row[...-sizeof(Checksum)])

    IO::ByteFormat::BigEndian.encode(checksum, cursor)
    cursor += sizeof(Checksum)

    # NOTE: that we specify an explicit 'start' here is an important safety precaution!
    # Otherwise anybody who knows the algorithm can run `BytesMultimap#complete` and
    # voila -- all secrets and instants that we're desperately trying to hide, neatly
    # completed to the last letter!
    #
    # By offsetting the start, we *require* the querying side to know at least *that*
    # about the thing that it is querying -- the set simply doesn't store anything
    # before the start. The querying side mustn't be able to simply complete nothingness
    # up to everyone's data; like it can with e.g. `AppearanceMultimap` (where that is
    # done by design).
    multimap = BytesMultimap.new(@set)
    multimap.mount(row, atoms, start: Label.bytesize + secret.size)
  end

  private def query?(instant : Label, secret : Bytes) : SensorInfo?
    prefix = cursor = Bytes.new({Label.bytesize, secret.size}.sum)

    # Copy data into the prefix.
    cursor = instant.append_be(cursor)
    cursor.copy_from(secret)
    cursor += secret.size

    multimap = BytesMultimap.new(@set)

    row = multimap.complete1(prefix)

    return if row.size == prefix.size

    unless row.size == bytesize(secret)
      Log.warn { "reject row: size mismatch (#{row.size} != #{bytesize(secret)})" }
      return
    end

    # Verify checksum (0 stands for "original")
    checksum0 = IO::ByteFormat::BigEndian.decode(Checksum, row[-sizeof(Checksum)..-1])
    checksum1 = Digest::CRC32.checksum(row[...-sizeof(Checksum)])

    unless checksum0 == checksum1
      Log.warn { "reject row: checksum mismatch: #{checksum0} (its) != #{checksum1} (my)" }
      return
    end

    offset_conid = prefix.size
    offset_grpid = offset_conid + Label.bytesize
    offset_slot = offset_grpid + Label.bytesize

    unless conid = Label.from_slice_be?(row[offset_conid, Label.bytesize])
      Log.warn { "reject row: bad conid label: #{row.hexstring}" }
      return
    end

    unless grpid = Label.from_slice_be?(row[offset_grpid, Label.bytesize])
      Log.warn { "reject row: bad grpid label: #{row.hexstring}" }
      return
    end

    unless conid < grpid < instant
      Log.warn { "reject row: label constraint violated: #{conid} < #{grpid} < #{instant}" }
      return
    end

    slot = IO::ByteFormat::BigEndian.decode(Slot, row[offset_slot, sizeof(Slot)])

    SensorInfo.new(conid, grpid, slot)
  end

  # WARNING: the caller guarantees *instant* will never be registered again.
  def register(instant : Label, secret : Term?, info : SensorInfo, atoms : AtomArray) : Nil
    ml_secret = secret ? ML.compact(secret) : ""

    register(instant, ml_secret.to_slice, info.conid, info.grpid, info.slot, atoms)
  end

  def query?(instant : Label, secret : Term?) : SensorInfo?
    ml_secret = secret ? ML.compact(secret) : ""

    query?(instant, ml_secret.to_slice)
  end
end

record AppearanceInfo, conid : Label, slot : Slot, value : Term

struct AppearanceRegistry
  Log = ::Log.for(self)

  record Atom, prefix : Bytes, byte : UInt8, cursor : UInt8, digit : UInt8 do
    include BytesMultimap::Atom

    def inspect(io)
      io << "AppearanceRegistryAtom["
      Base64.strict_encode(prefix, io)
      io << "|" << byte.to_s(2, precision: 8) << "/" << cursor << "^" << digit << "]"
    end
  end

  @[Flags]
  enum RowFlags : UInt8
    ValueCompressed
  end

  def initialize(@set : ISet(Atom))
  end

  # AppearanceInfo row format is as follows:
  #
  # frag   <instant> <secret> : <conid> <slot>  <flags> <value payload> <checksum>
  # bytes      16         0..       16      4      1          1..          4
  #
  # Checksum is calculated for the entire row, i.e. for instant and secret
  # as well. This way, both sides can have some certainty & proceed with their
  # doings more or less confidently.

  private def bytesize(secret : Bytes) : Int32
    {Label.bytesize, secret.size, Label.bytesize, sizeof(Slot), sizeof(RowFlags), sizeof(Checksum)}.sum
  end

  private def bytesize(secret : Bytes, value : Bytes) : Int32
    bytesize(secret) + value.size
  end

  private def register(instant : Label,
                       secret : Bytes,
                       conid : Label,
                       slot : Slot,
                       value : Bytes,
                       flags : RowFlags,
                       atoms : AtomArray) : Nil
    row = cursor = Bytes.new(bytesize(secret, value))

    # Append fields.
    cursor = instant.append_be(cursor)
    cursor.copy_from(secret)
    cursor += secret.size
    cursor = conid.append_be(cursor)

    IO::ByteFormat::BigEndian.encode(slot, cursor)
    cursor += sizeof(Slot)

    IO::ByteFormat::BigEndian.encode(flags.value, cursor)
    cursor += sizeof(RowFlags)

    cursor.copy_from(value)
    cursor += value.size

    checksum = Digest::CRC32.checksum(row[...-sizeof(Checksum)])

    IO::ByteFormat::BigEndian.encode(checksum, cursor)
    cursor += sizeof(Checksum)

    # See `SensorMultimap` to learn why we need to set `start` here.
    multimap = BytesMultimap.new(@set)
    multimap.mount(row, atoms, start: Label.bytesize + secret.size)
  end

  private def query?(instant : Label, secret : Bytes) : AppearanceInfo?
    prefix = cursor = Bytes.new({Label.bytesize, secret.size}.sum)

    # Copy known fields to obtain row prefix.
    cursor = instant.append_be(cursor)
    cursor.copy_from(secret)
    cursor += secret.size

    multimap = BytesMultimap.new(@set)
    row = multimap.complete1(prefix)
    return if row.size == prefix.size

    if row.size <= bytesize(secret)
      Log.warn { "reject row: size constraint violated: #{row.size} <= #{bytesize(secret)}" }
      return
    end

    # Verify checksum (0 stands for "original")
    checksum0 = IO::ByteFormat::BigEndian.decode(Checksum, row[-sizeof(Checksum)..-1])
    checksum1 = Digest::CRC32.checksum(row[...-sizeof(Checksum)])

    unless checksum0 == checksum1
      Log.warn { "reject row: checksum mismatch: #{checksum0} (its) != #{checksum1} (my)" }
      return
    end

    offset_conid = prefix.size
    offset_slot = offset_conid + Label.bytesize
    offset_flags = offset_slot + sizeof(Slot)
    offset_value = offset_flags + sizeof(RowFlags)

    unless conid = Label.from_slice_be?(row[offset_conid, Label.bytesize])
      Log.warn { "reject row: bad conid label: #{row.hexstring}" }
      return
    end

    unless conid < instant
      Log.warn { "reject row: label constraint violated: #{conid} < #{instant}" }
      return
    end

    slot = IO::ByteFormat::BigEndian.decode(Slot, row[offset_slot, sizeof(Slot)])
    flags_value = IO::ByteFormat::BigEndian.decode(typeof(RowFlags::All.value), row[offset_flags, sizeof(RowFlags)])

    unless flags = RowFlags.from_value?(flags_value)
      Log.warn { "reject row: invalid row flags: #{flags_value}" }
      return
    end

    ml_value_bytes = row[offset_value...-sizeof(Checksum)]

    if flags.value_compressed?
      buffer = IO::Memory.new(ml_value_bytes)

      begin
        ml_value = Compress::Brotli::Reader.open(buffer, &.gets_to_end)
      rescue e : Compress::Brotli::BrotliError
        Log.warn(exception: e) { "reject row: compression error" }
        return
      end
    else
      ml_value = String.new(ml_value_bytes)
    end

    begin
      value = ML.term(ml_value)
    rescue ML::SyntaxError
      Log.warn { "reject row: cannot parse value: `#{ml_value}`" }
      return
    end

    AppearanceInfo.new(conid, slot, value)
  end

  # WARNING: the caller guarantees *instant* will never be registered again.
  def register(instant : Label, secret : Term?, info : AppearanceInfo, atoms : AtomArray) : Nil
    ml_secret = secret ? ML.compact(secret) : ""

    flags = RowFlags::None

    # Do not compress the value if it is very small.
    if ML.compact_bytesize(info.value) <= 64
      ml_value = ML.compact(info.value).to_slice
    else
      buffer = IO::Memory.new

      # NOTE: we really really do not care about CPU right now, the querying side
      # will have to **guess** all those bazillion bytes; we must compress as much
      # as we can.
      Compress::Brotli::Writer.open(buffer) do |gz|
        ML.compact(gz, info.value)
      end

      ml_value = buffer.to_slice
      flags |= RowFlags::ValueCompressed
    end

    register(instant, ml_secret.to_slice, info.conid, info.slot, ml_value, flags, atoms)
  end

  def query?(instant : Label, secret : Term?) : AppearanceInfo?
    ml_secret = secret ? ML.compact(secret) : ""

    query?(instant, ml_secret.to_slice)
  end
end

struct Tspace
  alias Atom = Tbase::Atom | SensorRegistry::Atom | AppearanceRegistry::Atom

  alias Surface = Sensor | Appearance

  # Sensor and Appearance are quite heavy beasts at this point so we're hiding
  # them behind a reference.

  defcase Sensor,
    conid : Label,
    slot : Slot,
    grpid : Label,
    secret : Term?,
    pattern : Term,
    subjects : Array(Tbase::Sensor)

  class Sensor
    def self.new(fresh : LabelGenerator,
                 conid : Label,
                 slot : Slot,
                 grpid : Label,
                 secret : Term?,
                 pattern : Term) : Sensor
      subjects = [] of Tbase::Sensor

      Tbase::Sensor.each(fresh, pattern) do |subject|
        subjects << subject
      end

      new(conid, slot, grpid, secret, pattern, subjects)
    end

    def info : SensorInfo
      SensorInfo.new(conid, grpid, slot)
    end

    # Two sensors are hashed and compared by their grpid's.
    def_equals_and_hash grpid
  end

  defcase Appearance,
    conid : Label,
    slot : Slot,
    secret : Term?,
    subject : Tbase::Appearance

  class Appearance
    def self.new(conid : Label, slot : Slot, instant : Label, secret : Term?, value : Term)
      new(conid, slot, secret, Tbase::Appearance.new(instant, value))
    end

    def instant : Label
      subject.id
    end

    def value : Term
      subject.value
    end

    def info : AppearanceInfo
      AppearanceInfo.new(conid, slot, subject.value)
    end

    # Two appearances are hashed and compared by their instants.
    def_equals_and_hash instant
  end

  def initialize(@set : ISet(Atom))
  end

  def tbase : Tbase
    Tbase.new(@set.subset(Tbase::Atom))
  end

  def sensors : SensorRegistry
    SensorRegistry.new(@set.subset(SensorRegistry::Atom))
  end

  def appearances : AppearanceRegistry
    AppearanceRegistry.new(@set.subset(AppearanceRegistry::Atom))
  end

  # NOTE: we register the surface in its corresponding registry first so that it
  # has a chance to be reachable at any point throughout the mounting process.

  def summon(surface : Sensor, atoms : AtomArray) : Nil
    surface.subjects.each do |subject|
      # Point all instant activations back to the same sensor slot, conid, and grpid.
      sensors.register(subject.id, surface.secret, surface.info, atoms)

      tbase.mount(subject, atoms)
    end
  end

  def summon(surface : Appearance, atoms : AtomArray) : Nil
    subject = surface.subject

    appearances.register(subject.id, surface.secret, surface.info, atoms)

    tbase.mount(subject, atoms)
  end

  def each_complement(surface : Sensor, &sink : Label, AppearanceInfo ->)
    surface.subjects.each do |subject|
      tbase.each_complement(subject) do |instant|
        next unless info = appearances.query?(instant, surface.secret)

        sink.call(instant, info)
      end
    end
  end

  def each_complement(surface : Appearance, &sink : Label, SensorInfo ->)
    tbase.each_complement(surface.subject) do |instant|
      next unless info = sensors.query?(instant, surface.secret)

      sink.call(instant, info)
    end
  end
end

class TspaceDigestSet
  include ISet(Tspace::Atom)

  enum Scope : UInt8
    Utrie
    StrandSet
    Xtrie
    SensorMultimap
    AppearanceMultimap
    SensorRegistry
    AppearanceRegistry

    def self.of(atom : Tspace::Atom) : self
      case atom
      in ::Utrie::Atom              then Utrie
      in ::StrandSet::Atom          then StrandSet
      in ::Xtrie::Atom              then Xtrie
      in ::SensorMultimap::Atom     then SensorMultimap
      in ::AppearanceMultimap::Atom then AppearanceMultimap
      in ::SensorRegistry::Atom     then SensorRegistry
      in ::AppearanceRegistry::Atom then AppearanceRegistry
      end
    end
  end

  @digest : Digest

  def initialize(@set : ISet(Bytes), algorithm : Digest.class = Digest::SHA256)
    @digest = algorithm.new
  end

  private def digest(scope : Scope, atom) : Bytes
    @digest.reset
    @digest.update(Bytes[scope.value]) # FIXME: ?!
    atom.update(@digest)
    @digest.final
  end

  def includes?(identity : Tspace::Atom) : Bool
    @set.includes?(digest(Scope.of(identity), identity))
  end

  def size? : Int32?
    @set.size?
  end

  def add(identity : Tspace::Atom, atoms : AtomArray) : Nil
    @set.add(digest(Scope.of(identity), identity), atoms)
  end
end

defcase Activation, kind : Kind, sensor : SensorInfo, instant : Label, appearance : AppearanceInfo do
  enum Kind : UInt8
    StimulusPresence
    StimulusAbsence
  end

  def inspect(io)
    io << kind << "["
    sensor.inspect(io)
    io << ": "
    appearance.inspect(io)
    io << " at "
    instant.inspect(io)
    io << "]"
  end

  def to_s(io)
    inspect(io)
  end
end

# Keepalive objects are companion objects of `Tconn` that manage the insertion
# and removal of atoms.
module Keepalive
  abstract def schedule(tid : Label, atoms : AtomArray) : Nil
  abstract def cancel : Nil
  abstract def cancel(tid : Label) : Nil
end

class Keepalive::None
  include Keepalive

  def initialize
    @atoms = {} of Label => AtomArray
  end

  def schedule(tid : Label, atoms : AtomArray) : Nil
    unless @atoms.put?(tid, atoms)
      Log.warn { "attempt to overwrite an existing task: #{tid}" }
      return
    end
  end

  def cancel : Nil
    @atoms.each { |_, atoms| atoms.each(&.burn) }
    @atoms.clear
  end

  def cancel(tid : Label) : Nil
    unless atoms = @atoms.delete(tid)
      Log.warn { "attempt to remove a task that does not exist: #{tid}" }
      return
    end

    atoms.each(&.burn)
  end
end

# With continuous keepalive, each atom is assigned to a millisecond in the user-
# specified interval. When it's the millisecond's turn the atoms assigned to that
# millisecond are reinserted.
class Keepalive::Continuous
  include Keepalive

  Log = ::Log.for(self)

  # NOTE: For some reason making this a struct triggers a SEGFAULT at runtime.
  defcase Reinsert, tid : Label, atom : IAtom

  # :nodoc:
  #
  # Continuous keepalive assigns each task to a millisecond bucket within
  # the specified interval based on the task's hash.
  #
  # A `Millisecond` is such a bucket. It is persistent & immutable.
  struct Millisecond
    getter ord : UInt32

    def initialize(@ord : UInt32, @tasks = Pf::Set(Reinsert).new)
    end

    private def_change

    def empty? : Bool
      @tasks.empty?
    end

    def same?(other : Millisecond) : Bool
      @tasks.same?(other.@tasks)
    end

    def each(& : Reinsert ->) : Nil
      @tasks.each { |task| yield task }
    end

    def with(task : Reinsert) : Millisecond
      change(tasks: @tasks.add(task))
    end

    def without(task : Reinsert) : Millisecond
      change(tasks: @tasks.delete(task))
    end
  end

  # :nodoc:
  #
  # Millisecond buckets are put in a crude immutable & persistent BST, Schedule,
  # ordered by their ordinal (i.e. the millisecond they're at in the interval).
  #
  # There is no need to balance anything since the position is determined by
  # the hash function, which should be reasonably random to balance everything
  # out itself.
  abstract class Schedule
    EMPTY = Empty.new

    def empty? : Bool
      same?(EMPTY)
    end

    def each(&fn : Millisecond ->) : Nil
      each(fn)
    end
  end

  defcase Schedule::Empty < Schedule do
    def size : Int32
      0
    end

    def each(fn : Millisecond ->) : Nil
    end

    def with(ord : UInt32, task : Reinsert) : Schedule
      Node.new(EMPTY, Millisecond.new(ord).with(task), 1, EMPTY)
    end

    def without(ord : UInt32, task : Reinsert) : Schedule
      self
    end
  end

  defcase Schedule::Node < Schedule, l : Schedule, ms : Millisecond, size : Int32, r : Schedule do
    def each(fn : Millisecond ->) : Nil
      l.each(fn)
      fn.call(ms)
      r.each(fn)
    end

    def with(ord : UInt32, task : Reinsert) : Schedule
      if ord == ms.ord
        ms1 = ms.with(task)
        ms1.same?(ms) ? self : copy_with(ms: ms1, size: size + 1)
      elsif ord < ms.ord
        l1 = l.with(ord, task)
        l1.same?(l) ? self : copy_with(l: l1, size: size + 1)
      else # ord > ms.ord
        r1 = r.with(ord, task)
        r1.same?(r) ? self : copy_with(r: r1, size: size + 1)
      end
    end

    def without(ord : UInt32, task : Reinsert) : Schedule
      if ord == ms.ord
        ms1 = ms.without(task)

        if ms1.same?(ms)
          self
        elsif ms1.empty? && l.empty?
          r
        elsif ms1.empty? && r.empty?
          l
        else
          copy_with(ms: ms1, size: size - 1)
        end
      elsif ord < ms.ord
        l1 = l.without(ord, task)
        l1.same?(l) ? self : copy_with(l: l1, size: size - 1)
      else # ord > ms.ord
        r1 = r.without(ord, task)
        r1.same?(r) ? self : copy_with(r: r1, size: size - 1)
      end
    end
  end

  @reinserts : Schedule

  def initialize(@interval : Time::Span)
    @fibers = Fiber::ExecutionContext::SingleThreaded.new("Continuous Keepalive")

    # These instance variables are protected by the lock. They are modified by
    # the keepalive mainloop fiber once per interval, and by the caller randomly.
    @running = false
    @reinserts = Schedule::EMPTY
    @removals = [] of AtomArray
    @lock = Mutex.new

    # These instance variables are only modified by the caller. They are not
    # protected by anything.
    @tasks = {} of Label => AtomArray
  end

  # Returns the size of the keepalive interval (no. of milliseconds).
  private def size : Int32
    @interval.total_milliseconds.to_i
  end

  private def mainloop : Nil
    Log.debug { "keepalive mainloop started" }

    while true
      reinserts, removals = @lock.synchronize do
        if @reinserts.empty? && @removals.empty?
          @running = false
          Log.debug { "mainloop was stopped: nothing more to do" }
          return
        end

        @removals, tmp = [] of AtomArray, @removals

        {@reinserts, tmp}
      end

      Log.debug { "copied schedule with #{reinserts.size} reinsert(s)" }

      if removals.size > 0
        Log.trace { "copied schedule with #{removals.size} atom group(s)" }

        removals.each do |atoms|
          atoms.each(&.burn)
        end

        Log.trace { "burned atoms from #{removals.size} atom group(s)" }
      end

      ord0 = 0

      reinserts.each do |ms|
        dt = (ms.ord - ord0).milliseconds

        if dt > 100.milliseconds
          Log.trace { "sleep for #{dt} until reinsert" }
        end

        sleep dt

        ms.each(&.atom.reinsert)

        ord0 = ms.ord
      end

      Log.debug { "completed #{reinserts.size} reinsert(s)" }

      dt = @interval - ord0.milliseconds

      if dt > 100.milliseconds
        Log.trace { "sleep for #{dt} until next round" }
      end

      sleep dt
    end
  end

  private def run_unless_running : Nil
    @lock.synchronize do
      return if @running

      @running = true
      @fibers.spawn { mainloop }
    end
  end

  def schedule(tid : Label, atoms : AtomArray) : Nil
    unless @tasks.put?(tid, atoms)
      Log.warn { "attempt to overwrite an existing task: #{tid}" }
      return
    end

    atoms.each do |atom|
      task = Reinsert.new(tid, atom)
      needle = (task.hash % size).to_u32

      @lock.synchronize do
        @reinserts = @reinserts.with(needle, task)
      end
    end

    run_unless_running
  end

  def cancel : Nil
    @lock.synchronize { @reinserts = Schedule::EMPTY }

    @tasks.each_value do |atoms|
      @lock.synchronize { @removals << atoms }
    end

    @tasks.clear

    run_unless_running
  end

  def cancel(tid : Label) : Nil
    unless atoms = @tasks.delete(tid)
      Log.warn { "attempt to remove a task that does not exist: #{tid}" }
      return
    end

    atoms.each do |atom|
      task = Reinsert.new(tid, atom)
      needle = (task.hash % size).to_u32

      @lock.synchronize do
        @reinserts = @reinserts.without(needle, task)
      end
    end

    @lock.synchronize { @removals << atoms }

    run_unless_running
  end
end

# Includers are companion objects of `Tconn` responsible for scheduling and
# executing *relook* for sensors.
#
# *relook* is necessary to detect appearances that "died unexpectedly", or are
# otherwise unable to report about their demise. Relook, which is basically "close
# your eyes and open them again, noting what changed" -- but for sensors; solves
# this issue.
#
# Includers of `Relook` are internal objects that you most likely should not be
# creating manually. See `Tconn::Sensor` for a user-facing interface to `Relook`.
module Relook
  # Schedules a relook task for *sensor* surface on *conn*'s *slot*. *period*
  # is the period of the relook task.
  abstract def schedule(conn : Tconn, slot : Slot, sensor : Tspace::Sensor, period : Time::Span) : Nil

  # Issues a targeted close of *sensor*'s relook task. Noop if such a task
  # was not scheduled.
  abstract def cancel(sensor : Tspace::Sensor) : Nil

  # Cancels all relook tasks. This method may only be called once. This relook
  # object will be unusable after that.
  abstract def cancel : Nil
end

# Noop relook implementation.
class Relook::None
  include Relook

  def schedule(conn : Tconn, slot : Slot, sensor : Tspace::Sensor, period : Time::Span) : Nil
  end

  def cancel(sensor : Tspace::Sensor) : Nil
  end

  def cancel : Nil
  end
end

# Periodic relook implementation. Spawns a fiber per sensor, which calls
# `Tconn#relook` for that sensor periodically.
class Relook::Periodic
  include Relook

  Log = ::Log.for(self)

  def initialize
    @fibers = Fiber::ExecutionContext::SingleThreaded.new("periodic relook")
    @cancel = Channel(Nil).new

    @stopswitches = {} of Tspace::Sensor => Channel(Nil)
    @lock = Mutex.new
  end

  def schedule(conn : Tconn, slot : Slot, sensor : Tspace::Sensor, period : Time::Span) : Nil
    stopswitch = Channel(Nil).new

    @lock.synchronize do
      @stopswitches[sensor] = stopswitch
    end

    @fibers.spawn do
      Log.trace { "started relook fiber for #{sensor} with period=#{period}" }

      while true
        select
        when @cancel.receive?
          Log.trace { "stopped relook fiber for #{sensor} due to global cancel" }
          break
        when stopswitch.receive?
          Log.trace { "stopped relook fiber for #{sensor} due to directed cancel" }
          break
        when timeout(period)
          unless conn.relook?(slot, sensor)
            Log.trace { "stopped relook fiber for #{sensor} due to failed relook" }
            break
          end
        end
      end
    ensure
      @lock.synchronize { @stopswitches.delete(sensor) }
    end
  end

  def cancel(sensor : Tspace::Sensor) : Nil
    return unless chan = @lock.synchronize { @stopswitches[sensor]? }

    chan.close
  end

  def cancel : Nil
    @cancel.close
  end
end

class Tstimuli
  record Owner, conid : Label, slot : Slot
  defcase Instance, instant : Label, value : Term, matches : Array(Term::Dict)

  getter grpid : Label

  def initialize(@pattern : Term, @grpid : Label, @stimuli = Pf::Map(Owner, Instance).new)
  end

  private def_change

  def absent? : Bool
    @stimuli.empty?
  end

  def after_present(instant : Label, stimulus : AppearanceInfo) : {Tstimuli, Bool}
    owner = Owner.new(stimulus.conid, stimulus.slot)

    # If we do not know about the owner already, then simply create the corresponding
    # entry and move on.
    unless instance0 = @stimuli[owner]?
      matches = M1.matches(@pattern, stimulus.value)
      if matches.empty?
        return self, false
      end

      instance1 = Instance.new(instant, stimulus.value, matches)
      stimuli1 = @stimuli.assoc(owner, instance1)

      return change(stimuli: stimuli1), true
    end

    # If we do know about the owner, make sure the new stimulus is actually new,
    # that is, its instant succedes the instant of the stimulus we already see.

    if instant < instance0.instant # If instant is older than seen, ignore.
      Log.debug { "ignore older stimulus presence: #{instant} (its) < #{instance0.instant} (my)" }
      return self, false
    end

    if instant == instance0.instant # If duplicate, ignore as well.
      # Do a quiet ignore if values are equal. Do a loud ignore if they're not,
      # this shouldn't happen.
      unless stimulus.value == instance0.value
        Log.warn { "ignore duplicate instant with different values: #{instant}, #{stimulus.value} (its) == #{instance0.instant}, #{instance0.value} (my)" }
      end

      return self, false
    end

    # instant > instance0.instant

    # Retire old instance and construct a new one. Update the stimuli map and
    # move on. If stimuli are equal, return no change (but update the stimuli
    # map with the new instant!)

    matches = M1.matches(@pattern, stimulus.value)
    if matches.empty?
      return change(stimuli: @stimuli.dissoc(owner)), true
    end

    instance1 = Instance.new(instant, stimulus.value, matches)
    stimuli1 = @stimuli.assoc(owner, instance1)

    {change(stimuli: stimuli1), instance0.matches != instance1.matches}
  end

  def after_absent(instant : Label, stimulus : AppearanceInfo) : {Tstimuli, Bool}
    owner = Owner.new(stimulus.conid, stimulus.slot)

    # Humph?
    unless instance = @stimuli[owner]?
      Log.debug { "received stimulus absence from unknown source: #{owner}" }
      return self, false
    end

    # Make sure the absence we're about to see is newer (we're outdated &
    # have skipped some presences) or equal (we're up-to-date and the appearance
    # was removed).
    if instant < instance.instant
      Log.debug { "ignore older stimulus absence: #{instant} (its) < #{instance.instant} (my)" }
      return self, false
    end

    # instant >= instance0.instant

    # Removal can hardly be harmful at this point so we don't do any
    # additional checks.
    {change(stimuli: @stimuli.dissoc(owner)), true}
  end

  def dict_multiset : Term::Dict
    Term::Dict.build do |commit|
      @stimuli.each do |_, stimulus|
        stimulus.matches.each do |env|
          commit.with(env, (commit[env]? || 0) + 1)
        end
      end
    end
  end
end

class Tview
  EMPTY = new(view: Pf::Map(Slot, Tstimuli).new)

  protected def initialize(@view : Pf::Map(Slot, Tstimuli))
  end

  private def_change

  def without(slot : Slot) : {Tview, Bool}
    view0 = @view
    view1 = @view.dissoc(slot)
    view0.same?(view1) ? {self, false} : {change(view: view1), true}
  end

  def next(pattern : Term, act : Activation) : {Tview, Bool}
    sensor = act.sensor
    stimulus = act.appearance

    view0 = @view[sensor.slot]? || Tstimuli.new(pattern, sensor.grpid)

    # Make sure it still belongs to the same grpid.
    unless view0.grpid == sensor.grpid
      return self, false
    end

    case act.kind
    in .stimulus_presence?
      view1, changed = view0.after_present(act.instant, stimulus)
    in .stimulus_absence?
      view1, changed = view0.after_absent(act.instant, stimulus)
    end

    # Make sure there truly was a change before committing to the map.
    if view0.same?(view1)
      return self, false
    end

    {change(view: @view.assoc(sensor.slot, view1)), changed}
  end

  def dict_multisets : Term::Dict
    Term::Dict.build do |commit|
      @view.each do |slot, stimuli|
        next if stimuli.absent?

        commit.with(slot, stimuli.dict_multiset)
      end
    end
  end
end

# WARNING: only the internals of `Tconn` that deal with `IChat` are thread-safe;
# nothing else is thread-safe. Create a `Tconn` per thread/fiber. Do not use
# the same `Tconn` from different threads/fibers. This will not work and will
# lead to cryptic bugs.
class Tconn
  alias Observer = Tview ->

  Log = ::Log.for(self)

  alias Spec = Sensor | Appearance

  record Sensor, pattern : Term, secret : Term? = nil, relook : Time::Span? = nil do
    def inspect(io)
      io << "Sensor["
      ML.compact(io, pattern)
      if secret_ = secret
        io << "::"
        ML.compact(io, secret_)
      else
        io << ":public"
      end
      io << "]"
    end
  end

  record Appearance, value : Term, secret : Term? = nil do
    def inspect(io)
      io << "Appearance["
      ML.compact(io, value)
      if secret_ = secret
        io << "::"
        ML.compact(io, secret_)
      else
        io << ":public"
      end
      io << "]"
    end
  end

  @conid : Label
  @view : Tview
  @unsubscribe : IChat::Unsubscribe

  def initialize(@set : ISet(Tspace::Atom),
                 @chat : IChat(Activation),
                 @observer : Observer,
                 @fresh : LabelGenerator = WWID,
                 @keepalive : Keepalive = Keepalive::None.new,
                 @relook : Relook = Relook::None.new)
    @conid = @fresh.call
    @unsubscribe = @chat.subscribe(@conid, &->receive(Activation))

    @view = Tview::EMPTY
    @surfaces = {} of Slot => Tspace::Surface
    @lock = Mutex.new
  end

  # Convenience method to construct a `Sensor` surface.
  def self.sensor(*args, **kwargs) : Sensor
    Sensor.new(*args, **kwargs)
  end

  # Convenience method to construct an `Appearance` surface.
  def self.appearance(*args, **kwargs) : Appearance
    Appearance.new(*args, **kwargs)
  end

  # Returns the `Tspace` object with which this map connection works.
  def tspace : Tspace
    Tspace.new(@set)
  end

  # Closes this connection and terminates all fibers associated with it.
  #
  # This method can only be called once; the connection will be in an expended
  # state afterwards. Thus after calling this method, you should dispose of
  # the connection as soon as possible.
  def close : Nil
    @lock.synchronize do
      @surfaces.clear
      @view = Tview::EMPTY
      @observer.call(@view)
    end

    @unsubscribe.call
    @keepalive.cancel
    @relook.cancel
  end

  # :nodoc:
  #
  # This is an internal method called by the `Relook` companion of this
  # connection when it wants the connection to "refresh" a specific
  # sensor's view.
  def relook?(slot : Slot, sensor : Tspace::Sensor) : Bool
    clear(slot, sensor)
    refresh(sensor, as: :stimulus_presence)

    true
  end

  # Updates or inserts *spec* at the given *slot*. Returns *spec*.
  def []=(slot : Slot, spec : Spec) : Spec
    grpid = @fresh.call

    if surface0 = unregister?(slot)
      Log.trace { "#{@conid}: iseq(#{grpid}): unregistered previous value @#{slot}, #{surface0}" }

      case surface0
      in Tspace::Sensor
        @keepalive.cancel(surface0.grpid)
        @relook.cancel(surface0)
      in Tspace::Appearance
        @keepalive.cancel(surface0.instant)
      end

      Log.trace { "#{@conid}: iseq(#{grpid}): canceled keepalive&/relook of #{surface0}" }
    end

    case spec
    in Sensor
      surface1 = Tspace::Sensor.new(@fresh, @conid, slot, grpid, spec.secret, spec.pattern)
    in Appearance
      surface1 = Tspace::Appearance.new(@conid, slot, grpid, spec.secret, spec.value)
    end

    register(slot, surface1)

    Log.trace { "#{@conid}: iseq(#{grpid}): registered" }

    # Now summon the surface. Assume it is thread-safe to do that.
    atoms = AtomArray.new
    tspace.summon(surface1, atoms)

    Log.trace { "#{@conid}: iseq(#{grpid}): summoned" }

    # Finally schedule keepalive for the atoms that constitute the surface.
    # Assume keepalive scheduling logic is thread-safe.
    @keepalive.schedule(grpid, atoms)

    Log.trace { "#{@conid}: iseq(#{grpid}): scheduled keepalive" }

    # If we were inserting a sensor, schedule relook as well, provided the sensor
    # has a relook period set.
    if spec.is_a?(Sensor) && (period = spec.relook)
      @relook.schedule(self, slot, surface1, period)

      Log.trace { "#{@conid}: iseq(#{grpid}): scheduled relook" }
    end

    # If we've managed to remove an appearance, make sure to notify anybody
    # interested in the appearance's demise.
    #
    # A known problem is sensors connecting after we've canceled keepalive of
    # the appearance, but before our atoms start to decay; thus sensors will
    # see the "corpse" as "lively enough".
    #
    # This is the reason why we have periodic, forced sensor refresh (or its
    # alternative, blast sensors, which are stateless & thus cannot perceive
    # absence & leak).
    if surface0.is_a?(Tspace::Appearance)
      refresh(surface0, as: :stimulus_absence)
    end

    refresh(surface1, as: :stimulus_presence)

    spec
  end

  # Removes the surface assigned to *slot*.
  def delete(slot : Slot) : Nil
    return unless surface = unregister?(slot)

    Log.trace { "#{@conid}: dseq: unregistered previous value @#{slot}, #{surface}" }

    case surface
    in Tspace::Sensor
      @keepalive.cancel(surface.grpid)
      @relook.cancel(surface)
    in Tspace::Appearance
      @keepalive.cancel(surface.instant)
      refresh(surface, as: :stimulus_absence)
    end

    Log.trace { "#{@conid}: dseq: canceled keepalive&/relook of #{surface}" }
  end

  # This method is called by `IChat` when it receives a message directed
  # at this connection; or at insertion through self-activation.
  private def receive(act : Activation) : Nil
    activate(act)
  end

  private def refresh(surface : Tspace::Sensor, *, as kind : Activation::Kind) : Nil
    Log.trace { "#{@conid}: refresh surface @#{surface.slot}" }

    tspace.each_complement(surface) do |instant, appearance|
      Log.debug { "#{@conid}: self-directed #{kind} from complement appearance surface #{instant}" }

      receive(Activation.new(kind, surface.info, instant, appearance))
    end

    Log.trace { "#{@conid}: end refresh surface @#{surface.slot}" }
  end

  private def refresh(surface : Tspace::Appearance, *, as kind : Activation::Kind) : Nil
    Log.trace { "#{@conid}: refresh surface @#{surface.slot}" }

    tspace.each_complement(surface) do |instant, sensor|
      Log.debug { "#{@conid}: send #{kind} to complement sensor surface #{instant}" }

      # Assume `IChat#send` is thread-safe.
      @chat.send(sensor.conid, Activation.new(kind, sensor, surface.instant, surface.info))
    end

    Log.trace { "#{@conid}: end refresh surface @#{surface.slot}" }
  end

  private def unregister?(slot : Slot) : Tspace::Surface?
    @lock.synchronize do
      return unless surface = @surfaces.delete(slot)

      # Do not forget to update the view -- remove the sensor from there.
      # Importantly we must do this before responding. Otherwise nasty
      # out-of-order could mess things up and leave staleness.
      if surface.is_a?(Tspace::Sensor)
        @view, changed = @view.without(slot)
        if changed
          @observer.call(@view)
        end
      end

      surface
    end
  end

  private def register(slot : Slot, surface : Tspace::Surface) : Nil
    @lock.synchronize { @surfaces[slot] = surface }
  end

  private def clear(slot : Slot, sensor : Tspace::Sensor) : Nil
    @lock.synchronize do
      # Make sure the surface is still there and it's the same sensor.
      surface = @surfaces[slot]?

      unless surface && surface == sensor
        Log.debug { "cancel clear on @#{slot}: surface absent or different" }
        return
      end

      # Remove the sensor's view & call the observer if something changed.
      @view, changed = @view.without(slot)
      if changed
        @observer.call(@view)
      end
    end
  end

  private def activate(act : Activation)
    @lock.synchronize do
      # Make sure we actually own the sensor.
      unless act.sensor.conid == @conid
        Log.debug { "cancel activation on @#{act.sensor.slot}: excited sensor not owned by self" }
        return
      end

      # Make sure the surface is still there, is a sensor, and it's the same
      # sensor that is set to be the receiver of the activation.
      surface = @surfaces[act.sensor.slot]?

      unless surface && surface.is_a?(Tspace::Sensor) && surface.grpid == act.sensor.grpid
        Log.debug { "cancel activation on @#{act.sensor.slot}: surface absent or different" }
        return
      end

      # If everything checks out, update the view & call the observer.
      @view, changed = @view.next(surface.pattern, act)
      if changed
        @observer.call(@view)
      end
    end
  end
end

# set = SyncInMemoryMultiset(Tspace::Atom).new
# chat = SyncInMemoryChat(Activation).new
# map = Tconn.new(set, chat, ->(v : Tview) { pp v }, relook: Relook::Periodic.new)
# map[0] = Tconn.sensor(ML.term(%{((%any + -) x_number y_number)}), relook: 10.seconds)
# sleep 3.seconds
# map[1] = Tconn.appearance(Term.of(:+, 1, 2))
# sleep 3.seconds
# map.delete(1)
# sleep 3.seconds
# map.delete(0)

# sleep
{% skip_file %}

# With brotli, atom cost = 1 953 287

patterns = ML.terms File.read("./patterns.test.wwml")

set = TspaceDigestSet.new(SyncInMemorySet(Bytes).new)
chat = SyncInMemoryChat(Activation).new
map = Tconn.new(set, chat, ->(v : Tview) { pp v })
puts "Begin"
map[0] = Tconn.appearance(ML.term %{(+ 1 2)})
map[1] = Tconn.appearance(patterns)

puts "Atom cost ="
puts set.size?

while (print "> "; i = gets)
  map[2] = Tconn.sensor(ML.term i)
end

{% skip_file %}

set = SyncInMemorySet(Tspace::Atom).new
chat = SyncInMemoryChat(Activation).new
map = Tconn.new(set, chat, ->(v : Tview) { pp v })
map[0] = Tconn.sensor(ML.term %{((%any + -) x_number y_number)})
map[1] = Tconn.appearance(ML.term %{(+ 1 2)})
map[2] = Tconn.appearance(ML.term %{(- 1 2)})

sleep 20.seconds

map.delete(1)

sleep 20.seconds

map[3] = Tconn.sensor(ML.term %{(- x_ y_)})

sleep 20.seconds

map.close

sleep

{% skip_file %}

set = TspaceDigestSet.new(SyncInMemoryMultiset(Bytes).new)
blueprint = Tconn::Blueprint.new(
  set: set,
  chat: SyncInMemoryChat(Activation).new,
  sink: ->(ov : Tview) { pp ov.dict_multisets },
)

map = Tconn.new(blueprint)
map[0] = Tconn.sensor(ML.term %{((%any + -) x_number y_number)})
map[1] = Tconn.appearance(ML.term %{(+ 1 2)})
map[1] = Tconn.appearance(ML.term %{(- 3 4)})
map.delete(0)
map[0] = Tconn.sensor(ML.term %{(+ x_ y_)})
map[1] = Tconn.appearance(ML.term %{(+ 100 200)})
map.delete(0)
map.delete(1)
sleep 1.second
pp set.size?

# TODO: thread safety of Tconn<>IChat, Tconn<>Relook
# TODO: Tsetconn
# TODO: implement UnbufferedSet(IRemoteSet) < ISet
# TODO: implement BufferedSet(IRemoteSet) < ISet
# TODO: implement basic string set & chat client < IRemoteSet; server to start working
#   on remote stuff in D7/soma. p2p can wait.
# TODO: use this in soma
# TODO: move ready stuff to src/, replace/remove old files
#
# TODO: ensure each Tconn has its own, unique Keepalive
# TODO: compute Keepalive interval based on the number of atoms. The more
#  atoms the longer the period, so that keepalive is spread out. If we have
#  millions of atoms it's better to have minutes-long interval so that we don't
#  overwhelm the network/etc.
#
# TODO: implement "lightweight" or "stateless" sensors that do not commit themselves
#   to the view. Such sensors are provided as a way to disable periodic refresh. In
#   D7 they're going to be e.g.:
#      (sensor (in tspace blasting x to @xs) x_number)
#      (log @xs in ())
#   Such sensors are unable to perceive absence.
#
#
# TODO: reduce atom cost of Utrie (remove Trunk etc.)
# TODO: reduce atom cost of appearanceinfo
#   * use ML.compactf -> pretty printer which is much smarter & has a chance
#     of emitting something shorter than what ML.compact would. E.g. `%partition`
#     and so on.
# TODO: experiment with unstructured p2p multiset impl/proto, simulate stuff
#    * ant colony inspired "scout message" routing, neighborhood mapping,
#      deep exploration
#    * I have a feeling it would work, but how to build a multiset on top of that
#      with enough guarantees to make it practical?!
#    * stuff should be as simple as possible. single threaded core. no fanciness
#      on the implementation. protocol as simple as possible. p2p stuff is unbelievably
#      hard. try to keep it simple,stupid.
# TODO: proof of work cost for atoms and activations once p2p stuff works.

# sleep 1.second
# pp set
# map[2] = Tconn.appearance(ML.term %{(+ 1 2)})
# map[3] = Tconn.appearance(ML.term %{(+ 1 "qux")})
# map[4] = Tconn.appearance(ML.term %{(- 3 4)})

# Tset

# TsyncMap
# TsyncSet

# conid = WWID.call
# set = MySet(Tspace::Atom).new
# tspace = Tspace.new(set)
# atoms = AtomArray.new
# tspace.summon((a = Tspace::Appearance.new(conid, 0, WWID.call, nil, Term.of(:+, 1, 2))), atoms)
# tspace.summon((b = Tspace::Sensor.new(conid, 1, WWID.call, nil, Term.of({:"%any", :+, :-}, :x_number, :y_number))), atoms)
# tspace.summon((c = Tspace::Appearance.new(conid, 2, WWID.call, nil, Term.of(:-, 3, 4))), atoms)
# tspace.summon((d = Tspace::Sensor.new(conid, 3, WWID.call, nil, Term.of(:_symbol, :x_, :y_))), atoms)
# puts "Complements of #{a.slot}"
# tspace.each_complement(a) do |instant, sensor|
#   pp sensor.slot
# end
# puts "Complements of #{b.slot}"
# tspace.each_complement(b) do |instant, appearance|
#   pp appearance.slot
# end
# puts "Complements of #{c.slot}"
# tspace.each_complement(c) do |instant, sensor|
#   pp sensor.slot
# end
# puts "Complements of #{d.slot}"
# tspace.each_complement(d) do |instant, appearance|
#   pp appearance.slot
# end
# pp set.@set.size

# sleep 1.second
# pp set
# conid = WWID.call
# grpid = WWID.call
# sid1 = WWID.call
# sid2 = WWID.call
# set = MySet(AppearanceRegistry::Atom).new
# atoms = AtomArray.new
# areg = AppearanceRegistry.new(set)
# sin1 = AppearanceInfo.new(conid, 123u32, Term.of(:+, 1, 2))
# sin2 = AppearanceInfo.new(conid, 456u32, Term.of(:count, 0, "Hello World"))
# areg.register(sid1, nil, sin1, atoms)
# areg.register(sid2, Term.of(:passw0rd), sin2, atoms)
# pp! areg.query?(sid1, nil)
# pp! areg.query?(sid2, nil)
# pp! areg.query?(sid2, Term.of(:passw0rd))
# pp set.@set.size

# puts ML.compact(Term.of(:+, 1, 2))
# puts ML.compact(Term.of(:count, 0, "Hello World"))
# pp sin
# pp sout

# aid = WWID.call
# # sid = WWID.call
# tset = MySet(Tbase::Atom).new
# tbase = Tbase.new(tset)
# atoms = AtomArray.new

# # people = File.read("data/people.json")
# # json = JSON.parse(people)
# # term = Term.of(json)
# # term.each_entry do |_, value|
# #   tbase.mount(Tbase::Appearance.new(WWID.call, value), atoms)
# # end

# # require "benchmark"

# # sensor0 = nil
# # Tbase::Sensor.each(WWID, Term.of({"isActive" => false})) do |sensor|
# #   sensor0 = sensor
# # end

# # n = 0
# # Benchmark.ips do |x|
# #   x.report("is active") do
# #     n = 0
# #     tbase.each_complement(sensor0.not_nil!) do |appearance|
# #       n  += 1
# #     end
# #   end
# # end

# # pp n

# tbase.mount(app = Tbase::Appearance.new(aid, Term.of(:+, 1, 2)), atoms)
# tbase.mount(app1 = Tbase::Appearance.new(WWID.call, Term.of(:+, 100, 200)), atoms)
# sensor0 = nil
# Tbase::Sensor.each(WWID, Term.of(:+, :x_number, :y_number)) do |sensor|
#   sensor0 = sensor
#   tbase.mount(sensor, atoms)
# end

# # bytes = Digest::SHA256.digest("Hello World")

# # reader = BitReader.new(bytes)
# # while true
# #   # Consume base-4 digit.
# #   _bit0 = reader.consume? || break
# #   _bit1 = reader.consume? || 0u8

# #   reader.progress
# # end

# puts "Appearance complements"
# tbase.each_complement(app) do |sensor|
#   pp sensor
# end

# puts "Sensor complements"
# tbase.each_complement(sensor0.not_nil!) do |appearance|
#   pp appearance
# end

# pp tset
# pp tset.@set.size

# Data that `Tbase` needs to know about a sensor.
# record Sensor, id : Label, strands : StrandList do
#   # Calls *fn* with each sensor in *pattern*.
#   #
#   # An arbitrary M1 *pattern* can contain branches (e.g. `%any`) so it is considered
#   # to contain multiple sensors.
#   def self.each(fresh : LabelGenerator, pattern : Term, &fn : Sensor ->) : Nil
#     skeleton = pipe(pattern, M1.normal, M1.skeleton)

#     strands = [] of Strand

#     M1.branches(skeleton) do |branch|
#       M1.strands(branch) do |strand|
#         strands << strand.items.to_readonly_slice { |base| Term.decode(Ubase::Any, base) }
#       end

#       sensor = new(fresh.call, strands.to_readonly_slice(&.itself))

#       fn.call(sensor)

#       strands.clear
#     end
#   end
# end

{% skip_file %}
ref = WWID.call

aset = MySet(AppearanceMultimap::Identity).new
# eset = MySet(Etrace::Identity).new

amap = AppearanceMultimap.new(aset)
# etrace = Etrace.new(eset)

# etrace.mount(ref, [Digest::SHA256.digest("hello"), x = Digest::SHA256.digest("world")], atoms)
# etrace.mount(ref, [Digest::SHA256.digest("hello"), y = Digest::SHA256.digest("boo")], atoms)
# etrace.mount(ref, [Digest::SHA256.digest("foobar"), z = Digest::SHA256.digest("baz")], atoms)

# etrace.walk(ref, Digest::SHA256.digest("hello")) do |succ|
#   pp succ.hexstring
# end

people = File.read("data/people.json")
json = JSON.parse(people)
term = Term.of(json)

# map = {} of Label => Term
term.each_entry do |_, value|
  # value = Term.of(:+, 100, 200)
  id = WWID.call
  atoms = AtomArray.new

  Term.each_keypath_and_leaf(value) do |keypath, leaf|
    keypath.push(leaf) do
      entry = Ttrie.encode(Ttrie.steps(keypath), id: id)

      # Now we will append the entry byteslice to the appearance multimap. Anyone
      # will be able to "guess their way" through the multimap to obtain the entry
      # slice -- and by the end of it, they would've done the pattern matching &
      # would be holding an appearance id & the CRC32 checksum in their hands.
      amap.append(ref, entry, atoms)
    end

    true # continue
  end
end

query = {Ubase::Trunk.new, Ubase::IsDict.new, Ubase::At.new(Term.of("isActive")), Ubase::IsBool.new, Ubase::Literal.new(Term.of(true))}
query_prefix = Ttrie.prefix(Ttrie.steps(query))
amap.each_appearance(ref, query_prefix) do |appearance_id|
  pp appearance_id
end

#   map[aid] = value
# end
# require "benchmark"

# a = b = c = 0
# Benchmark.ips do |x|
#   x.report("query all") do
#     if id0 = ttrie.query?(ref, {Ubase::Trunk.new})
#       hits = Set(Label).new
#       etrace.walk(ref, id0) do |cid|
#         amap.decode(ref, cid) do |hit|
#           hits << hit
#         end
#       end
#       a = hits.size
#     end
#   end

#   x.report("query active") do
#     if id1 = ttrie.query?(ref, {Ubase::Trunk.new, Ubase::IsDict.new, Ubase::At.new(Term.of("isActive")), Ubase::IsBool.new, Ubase::Literal.new(Term.of(true))})
#       hits = Set(Label).new
#       etrace.walk(ref, id1) do |cid|
#         amap.decode(ref, cid) do |hit|
#           hits << hit
#         end
#       end
#       b = hits.size
#     end
#   end
#   x.report("query age 27") do
#     if id2 = ttrie.query?(ref, {Ubase::Trunk.new, Ubase::IsDict.new, Ubase::At.new(Term.of("age")), Ubase::IsNum.new, Ubase::Literal.new(Term.of(27))})
#       hits = Set(Label).new
#       etrace.walk(ref, id2) do |cid|
#         amap.decode(ref, cid) do |hit|
#           hits << hit
#         end
#       end
#       c = hits.size
#     end
#   end
# end

# pp a
# pp b
# pp c

# if id = ttrie.query?(ref, {Ubase::Trunk.new, Ubase::IsDict.new, Ubase::At.new(Term.of("isActive")), Ubase::IsBool.new, Ubase::Literal.new(Term.of(true))})
#   hits = Set(Label).new
#   etrace.walk(ref, id) do |cid|
#     amap.decode(ref, cid) do |hit|
#       hits << hit
#     end
#   end
#   hits.each do |hit|
#     pp map[hit]
#   end
# end

# if id = ttrie.query?(ref, {Ubase::Trunk.new, Ubase::IsDict.new, Ubase::At.new(Term.of("age")), Ubase::IsNum.new, Ubase::Literal.new(Term.of(27))})
#   hits = Set(Label).new
#   etrace.walk(ref, id) do |cid|
#     amap.decode(ref, cid) do |hit|
#       hits << hit
#     end
#   end
#   hits.each do |hit|
#     pp map[hit]
#   end
# end

# uset = MySet(Utrie::Identity).new
# utrie = Utrie.new(uset)
# xset = MySet(Xtrie::Identity).new
# xtrie = Xtrie.new(xset)

# Sensor.each(WWID, ML.term %{((%any + -) x_number y_number)}) do |sensor|
#   endpoints = Deque(Bytes).new
#   sensor.strands.each do |strand|
#     endpoints << utrie.mount(ref, strand, atoms)
#   end

#   endpoints.unstable_sort!
#   id = xtrie.mount(ref, endpoints, atoms)
#   puts "Sensor id: #{id.hexstring}"
# end

# # base = Ubase::Literal.new(Term.of(456))
# # io = IO::Digest.new(IO.empty, Digest::Blake3.new)

# # Ubase.update(io, base)

# # pp io.hexfinal

# hits = Deque(Bytes).new
# utrie.query(ref, Term.of(:-, 1, 2)) do |hit|
#   hits << hit
# end

# hits.unstable_sort!
# puts "#{hits.join(' ', &.hexstring)}"

# xtrie.conjs(ref, hits) do |conjv|
#   puts "Hit: #{conjv.hexstring}"
# end
