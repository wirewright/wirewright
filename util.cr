macro defrecord(name, *properties, includes = [] of ::NoReturn, copying = false, smart = false)
  struct {{name.id}}
    {% for dep in includes %}
      include {{dep}}
    {% end %}

    {% for property in properties %}
      {% if property.is_a?(Assign) %}
        getter {{property.target.id}}
      {% elsif property.is_a?(TypeDeclaration) %}
        {% if smart && property.type.resolve.nilable? %}
          getter? {{property}}
        {% else %}
          getter {{property}}
        {% end %}
      {% else %}
        getter :{{property.id}}
      {% end %}
    {% end %}

    def initialize({{ properties.map { |field| "@#{field.id}".id }.splat }})
      {{yield}}
    end

    # :nodoc:
    #
    # This lets us let Crystal infer generic args which is very useful.
    def self.__copy_with(*args, **kwargs)
      \{%begin %}
        \{{@type.name(generic_args: false)}}.new(*args, **kwargs)
      \{% end %}
    end

    {% if copying %}
      def copy_with({{
                      properties.map do |property|
                        if property.is_a?(Assign)
                          "#{property.target.id} _#{property.target.id} = @#{property.target.id}".id
                        elsif property.is_a?(TypeDeclaration)
                          "#{property.var.id} _#{property.var.id} = @#{property.var.id}".id
                        else
                          "#{property.id} _#{property.id} = @#{property.id}".id
                        end
                      end.splat
                    }})
        self.class.__copy_with({{
                         properties.map do |property|
                           if property.is_a?(Assign)
                             "_#{property.target.id}".id
                           elsif property.is_a?(TypeDeclaration)
                             "_#{property.var.id}".id
                           else
                             "_#{property.id}".id
                           end
                         end.splat
                       }})
      end
    {% end %}
  end
end

annotation DefcaseField
end

macro defcase(cls, *properties, inherit = false, equality = :value, caches_hash = false, copying = true, mutation = false, &)
  {% unless equality == :value || equality == :ref %}
    {% raise "equality must be :value or :ref"%}
  {% end %}

  {% header = "".id %}
  {% if inherit && @type.module? %}
    {% header = "include #{@type.id}".id %}
  {% elsif inherit && @type.class? %}
    {% cls = "#{cls.id} < #{@type.id}".id %}
  {% end %}

  {%
    names = properties.map do |typedecl|
      if typedecl.is_a?(Assign)
        typedecl.target.id
      elsif typedecl.is_a?(TypeDeclaration)
        typedecl.var.id
      else
        typedecl.id
      end
    end
  %}

  class {{cls}}
    {{header}}

    {% for name in names %}
      @[::DefcaseField]
      def {{name}}
        @{{name.id}}
      end

      {% if mutation %}
        def {{name}}=(object)
          @{{name.id}} = object
        end
      {% end %}
    {% end %}

    def initialize({{properties.map { |typedecl| "@#{typedecl}".id }.splat}})
      {% if caches_hash %}
        _ = hash # Run for the side effect of initializing @hash
      {% end %}
    end

    # :nodoc:
    #
    # This lets us let Crystal infer generic args which is very useful.
    def self.__copy_with(*args, **kwargs)
      \{%begin %}
        \{{@type.name(generic_args: false)}}.new(*args, **kwargs)
      \{% end %}
    end

    {% if copying %}
      def copy_with({{
                      properties.map do |property|
                        if property.is_a?(Assign)
                          "#{property.target.id} _#{property.target.id} = @#{property.target.id}".id
                        elsif property.is_a?(TypeDeclaration)
                          "#{property.var.id} _#{property.var.id} = @#{property.var.id}".id
                        else
                          "#{property.id} _#{property.id} = @#{property.id}".id
                        end
                      end.splat
                    }})
        self.class.__copy_with({{
                         properties.map do |property|
                           if property.is_a?(Assign)
                             "_#{property.target.id}".id
                           elsif property.is_a?(TypeDeclaration)
                             "_#{property.var.id}".id
                           else
                             "_#{property.id}".id
                           end
                         end.splat
                       }})
      end
    {% end %}

    {% if equality == :value %}
      def_equals_and_hash {{names.map { |name| "@#{name}".id }.splat}}
    {% end %}

    {% if caches_hash && equality == :value %}
      @_hash : Atomic(UInt64) = Atomic.new(0u64)

      def hash(hasher)
        hash = @_hash.get(:acquire)

        # NOTE: If the hash alg itself emits zero then we'll always recalculate,
        # but that's practically impossible so that's fine.
        if hash == 0u64
          hash = previous_def(Crystal::Hasher.new).result
          @_hash.set(hash, :release)
        end

        # Unfortunately there doesn't seem to be an easier way to combine our
        # cached hash with what *hasher* is accumulating right now.
        hash.hash(hasher)
      end
    {% end %}

    {{yield}}
  end
end

class ::UnreachableException < Exception
end

macro unreachable(detail = "unreachable")
  raise ::UnreachableException.new({{detail}})
end

def pass(*args, &)
  yield *args
end

class Array(T)
  def average(& : T -> Number)
    return 0.0 if empty?

    sum { |n| yield n } / size
  end

  # WARNING: the caller must guarantee/have guarantees that this array won't ever
  # be modified.
  def to_readonly_slice : Slice(T)
    Slice.new(to_unsafe, size, read_only: true)
  end

  def concat(other : Indexable(U), & : U -> T) forall U
    resize_if_cant_insert(other.size)

    other.each do |el|
      @buffer[@size] = yield el
      @size += 1
    end
  end
end

# Defines a `copy_with` method which functions like `#copy_with` of records.
macro def_copy_with
  {% verbatim do %}
    def copy_with(**kwargs) : self
      {% begin %}
        {{@type}}.new(
          {% for var in @type.instance_vars %}
            {{var.id}}: kwargs.fetch({{var.symbolize}}, @{{var.id}}).as({{var.type}}),
          {% end %}
        )
      {% end %}
    end
  {% end %}
end

struct Set(T)
  def reject!(&)
    @hash.reject! { |k, _| yield k }

    self
  end
end

struct Pf::Map(K, V)
  def extend(key : K, default : V, & : V -> V) : Map(K, V)
    assoc(key, yield self[key]? || default)
  end
end

module Enumerable(T)
  # FIXME: Maybe more idiomatically: first_of? / last_of?

  def leftmost?(& : T -> U?) : U? forall U
    each do |object0|
      next unless object1 = yield object0
      return object1
    end
  end

  def leftmost?(cls : U.class) : U? forall U
    each do |object|
      return object if object.is_a?(U)
    end
  end

  def rightmost?(cls : U.class) : U? forall U
    reverse_each do |object|
      return object if object.is_a?(U)
    end
  end

  def rightmost?(cls, *classes)
    rightmost?(cls) || rightmost?(*classes)
  end
end

struct ::Tuple(*T)
  def self.flatten
    Tuple.new
  end

  def self.flatten(object)
    {object}
  end

  def self.flatten(object : Tuple)
    flatten(*object)
  end

  def self.flatten(object, *objects)
    flatten(object) + flatten(*objects)
  end

  def self.compact
    Tuple.new
  end

  def self.compact(object : Nil, *objects)
    compact(*objects)
  end

  def self.compact(object, *objects)
    {object} + compact(*objects)
  end

  def flatten
    Tuple.flatten(self)
  end

  def flatten1
    Tuple.flatten1(*self)
  end

  def self.flatten1(x : Tuple, *xs)
    x + flatten1(xs)
  end

  def self.flatten1(x, *xs)
    {x} + flatten1(xs)
  end

  def self.flatten1(x : Tuple)
    x
  end

  def self.flatten1(x)
    {x}
  end

  def compact
    Tuple.compact(*self)
  end

  def partition(pivot : U.class) forall U
    {% begin %}
      {% index = nil %}
      {% for i in 0...T.size %}
        {% if index.nil? && T[i] == U %}
          {% index = i %}
        {% end %}
      {% end %}

      {% if index %}
        {self[...{{index}}], self[{{index}}..]
      {% else %}
        {self, ::Tuple.new}
      {% end %}
    {% end %}
  end

  def x
    self[0]
  end

  def y
    self[1]
  end
end

struct ::NamedTuple
  def transform(&)
    {% if T.size == 0 %}
      self
    {% else %}
      {
        {% for key in T %}
          {{key.id}}: (yield {{key.symbolize}}, self[{{key.symbolize}}]),
        {% end %}
      }
    {% end %}
  end
end

module Enumerable(T)
  def select(*types : *U) forall U
    {% begin %}
      {% for cls, i in U %}
        %acc{i} = [] of {{cls.instance}}
      {% end %}

      each do |object|
        case object
        {% for cls, i in U %}
        when {{cls.instance}}
          %acc{i} << object
        {% end %}
        end
      end

      { {% for cls, i in U %} %acc{i}, {% end %} }
    {% end %}
  end

  def partition(*types : *U) forall U
    {% begin %}
      {% for cls, i in U %}
        %acc{i} = [] of {{cls.instance}}
      {% end %}

      %rest = [] of T

      each do |object|
        case object
        {% for cls, i in U %}
        when {{cls.instance}}
          %acc{i} << object
        {% end %}
        else
          %rest << object
        end
      end

      { {% for cls, i in U %} %acc{i}, {% end %} %rest }
    {% end %}
  end
end

abstract struct Int
  def self.get(x : Int, *, bit_start : Int, bit_size : Int) : Int
    new((x >> bit_start) & mask(bit_size))
  end

  def self.put(x : Int, value : Int, *, bit_start : Int, bit_size : Int) : Int
    x &= ~(UInt64.mask(bit_size: bit_size) << bit_start)
    x |= value << bit_start
    x
  end

  def self.mask(bit_size : Int)
    (new(1u64) << bit_size) &- 1
  end

  # Reference: https://mostlymangling.blogspot.com/2019/01/better-stronger-mixer-and-test-procedure.html
  # See also: https://jonkagstrom.com/bit-mixer-construction/
  def self.mix(x : UInt64) : UInt64
    x ^= x.rotate_right(25) ^ x.rotate_right(50)
    x &*= 0xA24BAED4963EE407u64
    x ^= x.rotate_right(24) ^ x.rotate_right(49)
    x &*= 0x9FB21C651E98DF25u64
    x ^ (x >> 28)
  end

  def self.mix(x : UInt64, y : UInt64) : UInt64
    Int.mix(x ^ y.rotate_left(5))
  end

  def bit_set?(index)
    !bit(index).zero?
  end

  def each_bit(&)
    if zero?
      yield 0
      return
    end

    each_bit_with_index { |bit, _| yield bit }
  end

  def each_bit_with_index(&)
    (0...bit_length).each do |index|
      yield bit(index).to_u8, index.to_u8
    end
  end

  def self.each_digit(object, *, base = 10, &) : Nil
    assert base >= 1
    assert object.zero? || object.positive?

    if object.zero?
      yield object
      return
    end

    n = object

    until n.zero?
      yield n % base

      n //= base
    end
  end

  def each_digit(base = 10, &) : Nil
    Int.each_digit(self, base: base) do |digit|
      yield digit
    end
  end

  def each_digit_with_index(base = 10, &)
    index = 0
    each_digit(base) do |digit|
      yield digit, index
      index += 1
    end
  end

  def reverse_each_digit(base = 10, &)
    n = self

    dvsr = 1
    while dvsr <= n//base
      dvsr *= base
    end

    until dvsr == 0
      yield n // dvsr
      n %= dvsr
      dvsr //= base
    end
  end

  def reverse_each_digit_with_index(base = 10, &)
    index = 0
    reverse_each_digit(base) do |digit|
      yield digit, index
      index += 1
    end
  end

  def reverse(base = 10) : Int
    p = self
    q = self.class.zero

    until p.zero?
      q = q * base + p % base
      p //= base
    end

    q
  end

  def self.bit_size
    {% begin %}
      {% table = {UInt8   => 8,
                  Int8    => 8,
                  UInt16  => 16,
                  Int16   => 16,
                  UInt32  => 32,
                  Int32   => 32,
                  UInt64  => 64,
                  Int64   => 64,
                  UInt128 => 128,
                  Int128  => 128} %}

      {{table[@type] || @type.raise "unsupported number type"}}
    {% end %}
  end

  def self.bytesize
    bit_size//8
  end

  def bytesize
    self.class.bytesize
  end

  def nonzero?
    !zero?
  end

  # Yields bit indices of set bits. Iteration order: LSB to MSB.
  #
  # Reference: https://lemire.me/blog/2018/02/21/iterating-over-set-bits-quickly/
  def each_set_bit(&)
    bitset = self

    loop do
      break if bitset.zero?

      t = bitset & &-bitset
      r = bitset.trailing_zeros_count
      yield r

      bitset ^= t
    end
  end
end

struct ::BigInt < Int
  def each_prime_factor(& : self ->) : Nil
    n = self
    divisor = 2.to_big_i

    while divisor <= n
      if n.divisible_by?(divisor)
        yield divisor
        n //= divisor
      else
        divisor += 1
      end
    end
  end
end

class IO::Empty < IO
  INSTANCE = new

  def read(slice : Bytes)
    0
  end

  def write(slice : Bytes) : Nil
  end
end

class IO::BytesizeCounter < IO
  # Returns the number of bytes written to this IO.
  getter bytesize = 0

  def read(slice : Bytes)
    0
  end

  def write(slice : Bytes) : Nil
    @bytesize += slice.size
  end
end

class IO
  def self.empty
    Empty::INSTANCE
  end
end

class IO::Memory
  def to_readonly_slice : Bytes
    Slice.new(@buffer, @bytesize, read_only: true)
  end
end

class IO::BufferedWriteDigest < IO
  def initialize(@buffer : Bytes, @digest : ::Digest)
    @size = 0
  end

  def read(slice : Bytes) : Int32
    0
  end

  def flush : Nil
    @digest.update(@buffer.trim(@size))
    @size = 0
  end

  def write(slice : Bytes) : Nil
    if @size + slice.size > @buffer.size
      flush
    end

    # If slice is larger than buffer size, update the digest immediately
    # without buffering.
    if @size + slice.size > @buffer.size
      assert @size.zero?
      @digest.update(slice)
      return
    end

    # Otherwise, buffer.
    slice.copy_to(@buffer + @size)
    @size += slice.size
  end
end

class IO::Digest128 < IO
  def initialize(@buffer : Bytes)
    @digest = LibXXH128.create_state
    assert @digest, "could not create XXH128 digest state"
    assert LibXXH128.reset(@digest, seed: 144115188075855811u64).ok? # just a random prime for no reason

    @size = 0
  end

  def finalize
    LibXXH128.free_state(@digest)
  end

  def read(slice : Bytes) : Int32
    0
  end

  def flush : Nil
    assert LibXXH128.update(@digest, @buffer, @size).ok?
    @size = 0
  end

  def write(slice : Bytes) : Nil
    if @size + slice.size > @buffer.size
      flush
    end

    # If slice is larger than buffer size, update the digest immediately
    # without buffering.
    if @size + slice.size > @buffer.size
      assert @size.zero?
      assert LibXXH128.update(@digest, slice, slice.size).ok?
      return
    end

    # Otherwise, buffer.
    slice.copy_to(@buffer + @size)
    @size += slice.size
  end

  def digest : UInt128
    digest = LibXXH128.digest(@digest)
    (digest.high64.to_u128 << 64) | digest.low64
  end
end

module TextWrap
  extend self

  private def scan(text : StringView, maxw : Int, maxh : Int, &) : Nil
    w = 0
    h = 1
    wsidx = wsx = nil

    text.each_line do |line|
      line.each_char_with_abs_byte_index do |char, byte_index|
        if w + 1 > maxw
          next if char == ' ' # Ignore whitespace that we can't fit.

          # Max height exceeded.
          if h > maxh
            yield :trunc_before, byte_index
            return
          end

          # Reset width as if we're on a new line.

          if wsidx && wsx # Whitespace available on the line
            yield :br_at, wsidx
            w = w - wsx
          else # No whitespace on the line
            yield :br_before, byte_index
            w = 1
          end

          wsidx = wsx = nil
          h += 1
          next
        end

        if char == ' '
          wsx = w
          wsidx = byte_index
          w += 1
          next
        end

        w += 1
      end

      # EOI, no trailing newline
      return unless line.ends_with?('\n')

      # Newline or trailing newline.
      w = 0
      wsidx = nil

      # Max height exceeded
      if h > maxh
        yield :trunc_before, line.byte_end - 1
        return
      end

      h += 1
    end
  end

  def wrap(io : IO, text : StringView, maxw : Int, maxh : Int, *, ellipsis : String = "…") : Nil
    cursor = 0
    truncated = false

    scan(text, maxw, maxh) do |action, index|
      prefix = StringView.new(text.@trunk, byte_start: cursor.to_u32, byte_end: index.to_u32) # FIXME
      cursor += prefix.bytesize

      io << prefix

      case action
      when :br_at
        io << '\n'
        cursor += 1 # Skip index-th char
      when :br_before
        io << '\n'
      when :trunc_before
        truncated = true

        # Text was truncated. Replace N last chars with ellipsis to indicate
        # truncation, if possible. If ellipsis does not fit, do not insert it.
        if io.bytesize >= ellipsis.bytesize
          io.back(ellipsis.bytesize)
          io << ellipsis
        end

        break
      end
    end

    return if truncated

    remaining = StringView.new(text.@trunk, byte_start: cursor.to_u32, byte_end: text.bytesize.to_u32) # FIXME
    io << remaining
  end

  def wrap(text : StringView, maxw : Int, maxh : Int, **kwargs)
    String.build((text.bytesize * 1.33).to_i) do |io|
      wrap(io, text, maxw, maxh, **kwargs)
    end
  end

  def wrap(text : String, maxw : Int, maxh : Int, **kwargs)
    wrap(text.view, maxw, maxh, **kwargs)
  end
end

def wrap(io : IO, text : String, maxw : Int = 60, maxh = Int32::MAX, **kwargs)
  TextWrap.wrap(io, text, maxw, maxh, **kwargs)
end

def wrap(text : String, maxw : Int = 60, maxh = Int32::MAX, **kwargs)
  TextWrap.wrap(text, maxw, maxh, **kwargs)
end

class ::Hash
  def inverted_index : Hash(V, Array(K))
    hash = {} of K => Array(V)
    each do |key, value|
      bucket = hash.put_if_absent(key) { [] of V }
      bucket << value
    end
    hash
  end

  defrecord DiffAdded(K), key : K
  defrecord DiffRemoved(K, V), key : K, value : V

  def diff(keys : Set(K), & : DiffAdded(K) | DiffRemoved(K, V) ->)
    added = Pf::Kit.stack_array(DiffAdded(K), 8)
    removed = Pf::Kit.stack_array(DiffRemoved(K, V), 8)

    each do |key, value|
      next if key.in?(keys)

      removed << DiffRemoved(K, V).new(key, value)
    end

    keys.each do |key|
      next if has_key?(key)

      added << DiffAdded(K).new(key)
    end

    removed.each do |action|
      yield action
    end

    added.each do |action|
      yield action
    end
  end

  def to_readonly_slice(& : {K, V} -> T) : Slice(T) forall T
    ptr = Pointer(T).malloc(size)
    each_with_index do |(key, value), index|
      ptr[index] = yield({key, value})
    end
    Slice.new(ptr, size, read_only: true)
  end

  def try_update(key : K, & : V -> V) : Nil
    return unless entry_index = find_entry_with_index(key)

    entry, index = entry_index
    set_entry(index, Entry(K, V).new(entry.hash, entry.key, yield entry.value))
  end

  def transform(key : K, default_value : V, & : V -> V) : V
    if entry_index = find_entry_with_index(key)
      entry, index = entry_index
      set_entry(index, Entry(K, V).new(entry.hash, entry.key, yield entry.value))
      entry.value
    else
      upsert(key, default_value)
      default_value
    end
  end

  def replace?(key : K, & : V? -> V) : V
    if entry_index = find_entry_with_index(key)
      entry, index = entry_index
      set_entry(index, Entry(K, V).new(entry.hash, entry.key, yield entry.value))
      entry.value
    else
      upsert(key, value = yield nil)
      value
    end
  end

  def update(key : K, default_value : V, & : V -> V) : V
    if entry_index = find_entry_with_index(key)
      entry, index = entry_index
      set_entry(index, Entry(K, V).new(entry.hash, entry.key, yield entry.value))
      entry.value
    else
      upsert(key, yield default_value)
      default_value
    end
  end

  def update?(key : K, default_value : V, & : V -> V?) : Nil
    if entry_index = find_entry_with_index(key)
      entry, index = entry_index
      if new_value = yield entry.value
        set_entry(index, Entry(K, V).new(entry.hash, entry.key, new_value))
      else
        delete_entry_and_update_counts(index)
      end
    else
      new_value = yield default_value
      unless new_value.nil?
        upsert(key, new_value)
      end
    end
  end

  def put?(key : K, value : V) : Bool
    result = false
    put_if_absent(key) do
      result = true
      value
    end
    result
  end

  def put_if_absent(key, *keys, &)
    value = put_if_absent(key) { V.new }
    value.put_if_absent(*keys) { yield }
  end
end

struct Char
  def single_byte? : Bool
    0 <= ord <= 0xff
  end

  def view : StringView
    to_s.view
  end

  def each_char(& : Char ->) : Nil
    yield self
  end

  def ===(other : StringView)
    other === self
  end

  def hexdigit? : Int32?
    case self
    when '0'..'9' then self - '0'
    when 'a'..'f' then (self - 'a') + 10
    when 'A'..'F' then (self - 'A') + 10
    end
  end

  # Reference: https://github.com/rakudo/rakudo/blob/6b47541e27a4a0bcc9bbc07cdbf944b7174cc01c/src/Raku/ast/regex.rakumod#L715
  def hspace? : Bool
    ord.in?(0x09, 0x20, 0xa0, 0x1680, 0x180e, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008, 0x2009, 0x200a, 0x202f, 0x205f, 0x3000)
  end

  # Reference: https://github.com/rakudo/rakudo/blob/6b47541e27a4a0bcc9bbc07cdbf944b7174cc01c/src/Raku/ast/regex.rakumod#L793
  def vspace? : Bool
    ord.in?(0x0a, 0x0b, 0x0c, 0x0d, 0x85, 0x2028, 0x2029)
  end
end

class String
  def present? : Bool
    !empty?
  end

  def prefixed_by?(object)
    bytesize > object.bytesize && starts_with?(object)
  end

  def postfixed_by?(object)
    bytesize > object.bytesize && ends_with?(object)
  end

  def surrounded_by?(l, r)
    prefixed_by?(l) && postfixed_by?(r)
  end

  def starts_with?(range : Range(Char, Char))
    return unless first_char = self[0]?

    first_char.in?(range)
  end
end

struct BigInt < Int
  def add!(other)
    LibGMP.add(mpz, self, other)
    self
  end

  def mul!(other)
    LibGMP.mul(mpz, self, other)
    self
  end
end

class Pf::MapBox(K, V)
  def initialize(@map = Pf::Map(K, V).new)
  end

  private def_copy_with

  def size
    @map.size
  end

  def includes?(k : K) : Bool
    @map.includes?(k)
  end

  def each(&)
    @map.each { |k, v| yield k, v }
  end

  def [](k : K) : V
    @map[k]
  end

  def []?(k : K) : V?
    @map[k]?
  end

  def fetch(k : K, & : V ->) : Nil
    yield @map.fetch(k) { return }
  end

  def assoc(k : K, v : V)
    copy_with(map: @map.assoc(k, v))
  end

  def dissoc(k : K)
    copy_with(map: @map.dissoc(k))
  end
end

class Pf::SetBox(T)
  def initialize(@set = Pf::Set(T).new)
  end

  private def_copy_with

  delegate :size, :empty?, to: @set

  def includes?(v : T) : Bool
    @set.includes?(v)
  end

  def each(& : T ->) : Nil
    @set.each { |v| yield v }
  end

  def add(v : T)
    copy_with(set: @set.add(v))
  end

  def delete(v : T)
    copy_with(set: @set.delete(v))
  end
end

class Pf::BidiMapBox(K, V)
  def initialize(@map = Pf::BidiMap(K, V).new)
  end

  private def_copy_with

  def key_for(v : V) : K
    @map.key_for(v)
  end

  def value_for(k : K) : V
    @map.value_for(k)
  end

  def key_for?(v : V) : K?
    @map.key_for?(v)
  end

  def value_for?(k : K) : V?
    @map.value_for?(k)
  end

  def assoc(k : K, v : V)
    copy_with(map: @map.assoc(k, v))
  end

  def dissoc_by_key(k : K)
    copy_with(map: @map.dissoc_by_key(k))
  end

  def dissoc_by_value(v : V)
    copy_with(map: @map.dissoc_by_value(v))
  end
end

macro pipe(object, call)
  ({{(call.receiver ? "#{call.receiver}." : "").id}}{{call.name}}({{object}}, {{(call.args + (call.named_args || [] of ::NoReturn)).splat}}) {{call.block}})
end

macro pipe(object, call, *calls)
  pipe(pipe({{object}}, {{call}}), {{calls.splat}})
end

abstract struct Enum
  def self.with(symbols : Tuple() | Enumerable(Symbol)) : self
    {% begin %}
      {% unless @type.annotation(Flags) %}
        {% raise "expected a Flags enum" %}
      {% end %}

      state = {{@type}}::None

      symbols.each do |symbol|
        case symbol
        {% for member in @type.constants %}
          {% name = member.underscore.symbolize %}
          {% unless name == :none || name == :all %}
          when {{name}}
            state |= {{@type}}::{{member}}
          {% end %}
        {% end %}
        else
          raise ArgumentError.new("no member corresponds to #{symbol}")
        end
      end

      state
    {% end %}
  end

  def -(other : self) : self
    {% unless @type.annotation(Flags) %}
      {% raise "expected a Flags enum" %}
    {% end %}

    self & ~other
  end

  def symbolize : Symbol
    {% begin %}
      case self
      {% for member in @type.constants %}
      in .{{member.underscore}}?
        {{member.underscore.symbolize}}
      {% end %}
      end
    {% end %}
  end

  def ===(object : Symbol)
    object == symbolize
  end
end

struct Symbol
  def ===(object : Enum)
    object === self
  end
end

class Channel
  def <<(object)
    send(object)
  end
end

module Indexable(T)
  def ends_with?(objects : Indexable(T)) : Bool
    return false unless size >= objects.size

    (size - objects.size...size).each_with_index do |i, j|
      return false unless self[i] == objects[j]
    end

    true
  end

  def single?(&)
    result = nil

    each do |object|
      next unless candidate = yield object

      if result.nil?
        result = candidate
        next
      end

      return
    end

    result
  end

  def single?(cls : U.class) : U? forall U
    single?(&.as?(U)).as?(U)
  end

  def single? : T?
    return unless size == 1

    unsafe_fetch(0)
  end

  def single : T
    unless size == 1
      raise IndexError.new
    end

    unsafe_fetch(0)
  end

  def compare(other : Indexable, &)
    min_size = Math.min(size, other.size)

    0.upto(min_size - 1) do |i|
      n = yield unsafe_fetch(i), other.unsafe_fetch(i)
      next if n == 0 # equal
      return n
    end

    size <=> other.size
  end

  def compare(other : Indexable)
    compare(other) { |a, b| a <=> b }
  end

  def starts_with?(other : Indexable(U), & : T, U -> Bool) : Bool forall U
    return false unless size >= other.size

    other.each_with_index do |b, index|
      a = unsafe_fetch(index)

      return false unless yield a, b
    end

    true
  end

  def starts_with?(other : Indexable)
    starts_with?(other) { |a, b| a == b }
  end

  def median
    if empty?
      raise EmptyError.new
    end

    workspace = sort

    # size > 0

    if size.odd?
      return workspace[size//2]
    end

    # size > 0 && size.even?
    #   => size >= 2

    i = size//2
    m0 = workspace[i - 1]
    m1 = workspace[i]
    (m0 + m1)/2
  end
end

struct Slice(T)
  def self.of(*objects)
    pointer = Pointer(T).malloc(objects.size)
    objects.each_with_index do |object, index|
      pointer[index] = object
    end

    Slice(T).new(pointer, objects.size, read_only: true)
  end

  def self.join(left : Indexable(Slice(T)), mid : Indexable(U), right : Indexable(Slice(V))) forall T, U, V
    size = left.sum(&.size) + mid.size + right.sum(&.size)

    buffer = Pointer(T | U | V).malloc(size)
    cursor = buffer

    left.each do |slice|
      cursor.copy_from(slice.to_unsafe, slice.size)
      cursor += slice.size
    end

    mid.each do |object|
      cursor[0] = object
      cursor += 1
    end

    right.each do |slice|
      cursor.copy_from(slice.to_unsafe, slice.size)
      cursor += slice.size
    end

    Slice.new(buffer, size)
  end

  def prior : Slice(T)
    trim(size - 1)
  end

  def rest : Slice(T)
    self + 1
  end

  def starts_with?(other : Slice(T)) : Bool
    size >= other.size && trim(other.size) == other
  end

  def prefixed_by?(other : Slice(T)) : Bool
    size > other.size && trim(other.size) == other
  end

  def prepend(object : T) : Slice(T)
    mem = Pointer(T).malloc(size + 1)
    mem[0] = object
    (mem + 1).copy_from(to_unsafe, size)

    Slice.new(mem, size + 1, read_only: @read_only)
  end

  def prepend(object, & : T -> U) : Slice(U) forall U
    mem = Pointer(U).malloc(size + 1) do |index|
      if index.zero?
        object
      else
        yield unsafe_fetch(index - 1)
      end
    end

    Slice.new(mem, size + 1, read_only: @read_only)
  end

  def append(object : T) : Slice(T)
    mem = Pointer(T).malloc(size + 1)
    mem.copy_from(to_unsafe, size)
    mem[size] = object

    Slice.new(mem, size + 1, read_only: @read_only)
  end

  def append(object, & : T -> U) : Slice(U) forall U
    mem = Pointer(U).malloc(size + 1) do |index|
      if index < size
        yield unsafe_fetch(index)
      else
        object
      end
    end

    Slice.new(mem, size + 1, read_only: @read_only)
  end

  def prepend_many(objects : Indexable(T), & : T -> U) : Slice(U) forall U
    mem = Pointer(U).malloc(objects.size + size)

    index = 0

    objects.each do |object|
      mem[index] = yield object
      index += 1
    end

    each do |object|
      mem[index] = yield object
      index += 1
    end

    Slice.new(mem, objects.size + size, read_only: @read_only)
  end

  def append_many(objects : Indexable(T), & : T -> U) : Slice(U) forall U
    mem = Pointer(U).malloc(size + objects.size)

    index = 0

    each do |object|
      mem[index] = yield object
      index += 1
    end

    objects.each do |object|
      mem[index] = yield object
      index += 1
    end

    Slice.new(mem, size + objects.size, read_only: @read_only)
  end

  private module NullState
  end

  def chunk_by(accessor : T -> U, & : U, Slice(T) ->) forall U
    start = self
    size = 0

    state0 = NullState

    each do |el|
      state1 = accessor.call(el)
      if state0 == state1
        size += 1
        next
      end

      if state0.is_a?(NullState.class)
        state0 = state1
        size += 1
        next
      end

      yield state0, start.trim(size)

      state0 = state1
      start += size
      size = 0
    end

    unless state0.is_a?(NullState.class)
      yield state0, start.trim(size)
    end
  end

  def split(object : T, &)
    return if empty?

    start = 0
    count = 0

    each do |current|
      if current == object
        yield self[start, count]
        start += count + 1
        count = 0
      else
        count += 1
      end
    end

    if count > 0
      yield self[start, count]
    end
  end

  def upto(object : T) : {Slice(T), Slice(T)}
    unless index = index(object)
      return self, Slice(T).empty
    end

    {self[...index], self[index + 1..]}
  end

  def trim(newsize : Int) : Slice(T)
    unless 0 <= newsize <= size
      raise IndexError.new
    end

    Slice(T).new(@pointer, newsize, read_only: @read_only)
  end

  # Alias for `to_unsafe`.
  def ptr
    to_unsafe
  end

  # Leaves elements from the start of this slice up to but not
  # including *other*.
  #
  # NOTE: The caller must guarantee that `self` and other come from the same allocation.
  def upto(other : Slice(T)) : Slice(T)
    assert to_unsafe <= other.to_unsafe

    trim(other.to_unsafe - to_unsafe)
  end

  def -(n : Int) : Slice(T)
    trim(size - n)
  end

  def rchop(*objects, &)
    return if size < objects.size

    newsize = size

    objects.reverse_each do |char|
      return unless unsafe_fetch(newsize - 1) === char

      newsize -= 1
    end

    yield trim(newsize)
  end

  def in_chunks_of(*, size_lte chunk_size : Int32, & : Slice(T) ->) : Nil
    if size <= chunk_size
      yield self
    end

    chunk_count, remainder_count = size.divmod(chunk_size)
    chunk_count.times do |chunk_index|
      yield Slice(T).new(to_unsafe + chunk_index*chunk_size, chunk_size, read_only: @read_only)
    end

    yield Slice(T).new(to_unsafe + size - remainder_count, remainder_count, read_only: @read_only)
  end
end

class AssertionError < Exception
end

macro expect(x)
  raise AssertionError.new unless {{x}}
end

macro assert(x)
  raise AssertionError.new({{x.id.stringify}}) unless {{x}}
end

macro assert(x, msg)
  raise AssertionError.new({{msg}}) unless {{x}}
end

struct Time::Span
  def humanize(io)
    nanos = total_nanoseconds

    if self < 1.nanosecond
      io << total_nanoseconds.round(2) << "ns"
      return
    end

    if self < 1.millisecond
      io << total_microseconds.round(2) << "µs"
      return
    end

    if self < 1.second
      io << total_milliseconds.round(2) << "ms"
      return
    end

    if self < 1.minute
      io << total_seconds.round(2) << "s"
      return
    end

    io << total_minutes.round(2) << "m"
  end

  def humanize
    String.build { |io| humanize(io) }
  end
end

struct Bag(T)
  include Enumerable(T)

  def initialize
    @storage = {} of T => UInt32
  end

  protected def initialize(@storage)
  end

  # Returns the number of unique objects in this bag.
  def nunique : UInt32
    @storage.size.to_u32
  end

  def ntotal
    @storage.sum(0u32) { |_, tally| tally }
  end

  # Returns the number of objects of which there is more than one occurrence in this bag.
  def nrepeats : UInt32
    @storage.count { |_, tally| tally > 1 }.to_u32
  end

  def tally?(object : T) : UInt32?
    @storage[object]?
  end

  def tally(object : T) : UInt32
    @storage[object]
  end

  def add(object : T) : self
    @storage[object] = (@storage[object]? || 0u32) + 1

    self
  end

  def <<(object : T) : self
    add(object)
  end

  def concat(other : Bag(T)) : self
    other.each_with_tally do |key, tally|
      @storage[key] = (@storage[key]? || 0u32) + tally
    end

    self
  end

  def add?(object : T) : Bool
    @storage[object] = tally = (@storage[object]? || 0u32) + 1

    tally == 1
  end

  def subset_of?(other : Bag) : Bool
    each_with_tally do |x, n|
      return false unless m = other.tally?(x)
      return false if n > m
    end

    true
  end

  def delete?(object : T) : Bool
    return false unless refcount = @storage[object]?

    if refcount == 1
      @storage.delete(object)

      true
    else
      @storage[object] = refcount - 1

      false
    end
  end

  def delete_all(object)
    @storage.delete(object)
    self
  end

  def each(& : T ->)
    each_with_tally do |object, tally|
      tally.times { yield object }
    end
  end

  def each_with_tally(& : T, Int32 ->)
    @storage.each do |object, tally|
      yield object, tally.to_i
    end
  end

  def empty? : Bool
    @storage.empty?
  end

  def &(other : Bag(T)) : Bag(T)
    intersection = {} of T => UInt32

    # An element appears in the intersection of two bags the minimum of
    # the number of times it appears in either.

    if @storage.size < other.@storage.size
      @storage.each do |object, tally0|
        next unless tally1 = other.tally?(object)

        intersection[object] = Math.min(tally0, tally1)
      end
    else
      other.@storage.each do |object, tally1|
        next unless tally0 = tally?(object)

        intersection[object] = Math.min(tally0, tally1)
      end
    end

    Bag.new(intersection)
  end

  def -(other : Bag(T)) : Bag(T)
    difference = {} of T => UInt32

    # An element appears in the intersection of two bags the minimum of
    # the number of times it appears in either.

    @storage.each do |object, tally0|
      tally1 = other.@storage[object]? || 0u32
      next if tally0 <= tally1

      difference[object] = tally0 - tally1
    end

    Bag.new(difference)
  end

  def dup
    Bag.new(@storage.dup)
  end

  def set : Set(T)
    set = Set(T).new(@storage.size)
    @storage.each do |object, _|
      set << object
    end
    set
  end

  def array : Array(T)
    array = Array(T).new(@storage.size)
    @storage.each do |object, _|
      array << object
    end
    array
  end

  def inspect(io)
    io << "Bag{"
    @storage.join(io, ", ") do |(object, tally)|
      if tally > 1
        io << tally << "×"
      end
      object.inspect(io)
    end
    io << "}"
  end
end

module Enumerable(T)
  def flat_map_with_index(*, offset : Int = 0, &)
    index = offset
    flat_map do |item|
      result = yield item, index
      index += 1
      result
    end
  end

  def inverted_index : Hash(T, Array(Int32))
    hash = {} of T => Array(Int32)
    each_with_index do |key, index|
      bucket = hash.put_if_absent(key) { [] of Int32 }
      bucket << index
    end
    hash
  end

  def to_bag(& : T -> U) : Bag(U) forall U
    bag = Bag(U).new
    each do |object|
      bag << yield object
    end
    bag
  end

  def to_bag
    to_bag(&.itself)
  end

  def min(& : T, T -> Int32)
    min = uninitialized T
    found = false

    each_with_index do |elem, i|
      if i == 0 || yield(elem, min) < 0
        min = elem
      end
      found = true
    end

    raise Enumerable::EmptyError.new unless found

    min
  end
end

struct Range(B, E)
  def to_readonly_slice(&)
    {% unless B == E %}
      {% raise "cannot call #to_readonly_slice on ranges whose B != E" %}
    {% end %}

    {% unless B < ::Int && E < ::Int %}
      {% raise "expected Range(_ < Int, _ < Int)" %}
    {% end %}

    Slice((typeof (yield @begin))).new(size) do |index|
      yield @begin + index
    end
  end

  def segments(indices : Enumerable(T), &) forall T
    {% unless T < ::Int %}
      {% raise "unsupported type of indices: #{T}" %}
    {% end %}

    prev = T.new(0)

    indices.each_with_index do |i, j|
      unless prev <= i < size
        raise IndexError.new
      end

      if prev < i
        yield prev...i, nil
      end

      yield i...i + 1, j

      prev = i + 1
    end

    # Remaining range after the last index
    if prev < size
      yield (prev...size), nil
    end
  end

  def intersects?(other : Range(B, E)) : Bool
    assert @begin <= @end
    assert other.begin <= other.end

    left = Math.max(@begin, other.begin)
    right = Math.min(@end, other.end)
    left < right
  end

  def subrange_of?(other : Range(B, E)) : Bool
    {% unless B < ::Int && E < ::Int %}
      {% raise "expected Range(_ < Int, _ < Int)" %}
    {% end %}

    if empty?
      return @begin.in?(other) || @begin == other.end
    end

    @begin.in?(other) && (exclusive? ? (@end - 1).in?(other) : @end.in?(other))
  end

  def proper_subrange_of?(other : Range) : Bool
    subrange_of?(other) && size > other.size
  end

  def overlaps?(other : Range) : Bool
    other.begin.in?(self) || other.end.in?(self) || self.begin.in?(other) || self.end.in?(other)
  end

  def in_subranges_of(size step : Int, & : Range(B, E) ->) : Nil
    if step.zero?
      raise ArgumentError.new("cannot take subranges of size 0")
    end

    p, q = size.divmod(step)

    b = @begin

    p.times do
      e = b + step
      yield b...e
      b = e
    end

    if q > 0
      yield b...b + q
    end
  end

  def ===(other : StringView) : Bool
    return false unless other.size == 1

    other.first_char.in?(self)
  end

  def split(n : Int, & : Range(B, E) ->)
    {% unless B < ::Int && E < ::Int %}
      {% raise "expected Range(_ < Int, _ < Int)" %}
    {% end %}

    unless n.positive?
      raise ArgumentError.new
    end

    unless exclusive?
      raise ArgumentError.new("expected an exclusive rangej")
    end

    if size < n
      yield self
      return
    end

    step, rem = size.divmod(n)
    from = @begin
    n.times do |i|
      to = from + step - 1
      to += 1 if i < rem
      yield (from...to + 1), i
      from = to + 1
    end
  end

  # Slides a window of size *n* and yields the corresponding ranges while
  # the window fits.
  #
  # This range must be an exclusive integer range. *n* must be zero or positive.
  #
  # ```text
  #    0   1   2   3   4   5   6   7   8   9    0...10
  # ^    ^   ^   ^   ^   ^   ^   ^   ^   ^   ^  N=0
  #    -   -   -   -   -   -   -   -   -   -    N=1
  #    -----   -----   -----   -----   -----    N=2
  #    ---------   ---------   ----------       N=3
  #    -------------   --------------           N=4
  #
  #    ... and so on
  # ```
  #
  # The block must return a boolean indicating whether the subrange was accepted
  # or not. If accepted, this method advances by *n* (if zero, by `1`). If it was
  # rejected, this method advances by `1`.
  def slide_subrange_of(n : Int, & : Range(B, E) -> Bool)
    {% unless B < ::Int && E < ::Int %}
      {% raise "expected Range(_ < Int, _ < Int)" %}
    {% end %}

    assert exclusive?
    assert n.zero? || n.positive?

    return if n > size

    if n.zero?
      (@begin..@end).each do |i|
        _ = yield i...i
      end
      return
    end

    i = @begin
    while i + n <= @end
      accepted = yield i...i + n
      if accepted
        i += n
      else
        i += 1
      end
    end
  end
end

struct Int
  def subscript
    to_s.tr("0123456789", "₀₁₂₃₄₅₆₇₈₉")
  end

  def entering?(range : Range(Int32, Int32)) : Bool
    !range.includes?(self - 1) && range.includes?(self)
  end

  def leaving?(range : Range(Int32, Int32), size = 1) : Bool
    range.includes?(self) && !range.includes?(self + size)
  end

  def self.min
    {% begin %}
      {{@type}}::MIN
    {% end %}
  end

  def self.max
    {% begin %}
      {{@type}}::MAX
    {% end %}
  end
end

{% for width in %w(8 16 32 64 128) %}
  struct UInt{{width.id}}
    def self.bit_width
      {{width.id}}
    end
  end

  struct Int{{width.id}}
    def self.bit_width
      {{width.id}}
    end
  end
{% end %}

module Indexable(T)
  def each_with_last(& : T, Bool ->) : Nil
    return if empty?
    (0...size - 1).each do |index|
      yield unsafe_fetch(index), false
    end
    yield unsafe_fetch(size - 1), true
  end

  def to_readonly_slice(& : T, Int32 -> U) : Slice(U) forall U
    Slice(U).new(size, read_only: true) do |index|
      yield unsafe_fetch(index), index
    end
  end

  def to_slice(& : T, Int32 -> U) : Slice(U) forall U
    Slice(U).new(size) do |index|
      yield unsafe_fetch(index), index
    end
  end
end

module Enumerable(T)
  def to_compact_readonly_slice(& : T, Int32 -> U?) : Slice(U) forall U
    buffer = Pf::Kit.stack_array(U, 32)

    each_with_index do |object0, index|
      object1 = (yield object0, index)
      next if object1.nil?

      buffer << object1
    end

    buffer.to_unsafe_readonly_slice!
  end

  def to_readonly_slice(& : T, Int32 -> U) : Slice(U) forall U
    buffer = Pf::Kit.stack_array(U, 32)
    each_with_index do |item, index|
      buffer << (yield item, index)
    end

    buffer.to_unsafe_readonly_slice!
  end
end

# No idea whether this works.
#
# Source: https://lemire.me/blog/2017/09/18/visiting-all-values-in-an-array-exactly-once-in-random-order/
module Disorder
  extend self

  MAX_COUNT = 100_000

  private def coprime(min, target, rng)
    count = 0
    selected = 0u32

    (min...target).each do |val|
      if coprime?(val, target)
        count += 1
        if count == 1 || rng.rand(count) < 1
          selected = val
        end
      end

      if count == MAX_COUNT
        return val
      end
    end

    selected
  end

  private def coprime?(a, b)
    a.gcd(b) == 1
  end

  def state(n : UInt32, rng)
    {rng.rand(n), coprime(n // 2, n, rng)}
  end

  def next(n, state, prime) : UInt32
    state &+= prime
    if state >= n
      state &-= n
    end
    state
  end
end

struct Float
  def approx?(other : Float32, *, eps = 0.001f32)
    (self - other).abs <= eps
  end
end

module Iterator(T)
  def next! : T
    object = self.next
    if object.is_a?(Iterator::Stop)
      raise IndexError.new
    end

    object
  end
end

module IStack(T)
  abstract def empty? : Bool
  abstract def push(value : T)
  abstract def pop : T
end

class Array(T)
  include IStack(T)
end

# A bidirectional mapping between objects of type `L` and `R`.
class Bimap(L, R)
  include Enumerable({L, R})

  # Constructs a bidirectional map with an optional *initial capacity*.
  def initialize(*, initial_capacity cap0 = nil)
    {% if L.resolve == R.resolve %}
      {% raise "cannot use bimap with the same L and R type" %}
    {% end %}

    @l = Hash(L, R).new(initial_capacity: cap0)
    @r = Hash(R, L).new(initial_capacity: cap0)
  end

  def each(& : {L, R} ->) : Nil
    @l.each { |key, value| yield({key, value}) }
  end

  def includes?(object : L)
    @l.has_key?(object)
  end

  def includes?(object : R)
    @r.has_key?(object)
  end

  # Returns the object of type `R` associated with the object of type `L`.
  # Returns `nil` if no such association exists.
  def []?(object : L) : R?
    @l[object]?
  end

  # Returns the object of type `L` associated with the object of type `R`.
  # Returns `nil` if no such association exists.
  def []?(object : R) : L?
    @r[object]?
  end

  def [](object)
    value = self[object]?
    if value.nil?
      raise KeyError.new
    end

    value
  end

  # Creates an association between an object of type `L`, *key*, and an object
  # of type `R`, *value*, removing any previous association for both. Returns *value*.
  def []=(key : L, value value1 : R) : R
    if value0 = @l[key]?
      @r.delete(value0)
    end

    @l[key] = value1
    @r[value1] = key

    value1
  end

  # Creates an association between an object of type `R`, *key*, and an object
  # of type `L`, *value*, removing any previous association for both. Returns *value*.
  def []=(key : R, value value1 : L) : L
    if value0 = @r[key]?
      @l.delete(value0)
    end

    @r[key] = value1
    @l[value1] = key

    value1
  end

  def put_if_absent(key : L, & : -> R) : R
    if value = self[key]?
      return value
    end

    self[key] = yield
  end

  def put?(key : L, value : R) : Bool
    if includes?(key)
      return false
    end

    self[key] = value
    true
  end

  # Removes the association between the object of type `L` and an object of
  # type `R`. Returns the latter if found & removed. Returns `nil` otherwise.
  def delete(object : L) : R?
    if value = @l.delete(object)
      @r.delete(value)
    end

    value
  end

  # Removes the association between the object of type `R` and an object of
  # type `L`. Returns the latter if found & removed. Returns `nil` otherwise.
  def delete(object : R) : L?
    if value = @r.delete(object)
      @l.delete(value)
    end

    value
  end

  def delete!(object)
    value = delete(object)
    if value.nil?
      raise KeyError.new
    end

    value
  end

  # Clears this bimap.
  def clear : Nil
    @l.clear
    @r.clear
  end
end

struct BitReader
  def initialize(@bytes : Bytes, @i = 0, @j = 0u8)
  end

  def progress?
    return if @i >= @bytes.size # At end

    # Since we're reading bits from left to right j is offset from MSB
    # (j = 0 means MSB). Make a mask to take j MSB bits from the byte.
    mask = 0xFFu8 << (7 - @j)

    {@bytes[...@i], @bytes[@i] & mask, @j}
  end

  def consume? : UInt8?
    return if @i >= @bytes.size # At end

    bit = @bytes[@i].bit(7 - @j)

    @j += 1

    if @j >= 8
      @i += 1
      @j = 0u8
    end

    bit
  end
end

struct BitWriter
  def initialize(@prefix = Bytes.empty)
    @state = 0u8
    @cursor = 0u8
  end

  def <<(bit : UInt8)
    @state |= bit << (7 - @cursor)
    @cursor += 1

    if @cursor == 8
      @prefix += Bytes.with(@state)
      @state = 0u8
      @cursor = 0u8
    end
  end

  def progress
    {@prefix, @state, @cursor}
  end

  def final
    if @cursor.zero?
      @prefix
    else
      @prefix + Bytes.with(@state)
    end
  end

  def inspect(io)
    io << @prefix.hexstring << '|' << @cursor
  end
end

class MutBitWriter
  getter cursor

  def initialize(@prefix = [] of UInt8)
    @state = 0u8
    @cursor = 0u8
  end

  def initialize(prefix : Bytes)
    initialize(prefix.to_a)
  end

  def append(bits : Int, bitsize : Int32)
    (0...bitsize).reverse_each do |bit_index|
      self << bits.bit(bit_index).to_u8
    end
  end

  def append(bits : Enum, bitsize)
    append(bits.value, bitsize)
  end

  def bytesize : Int32
    @prefix.size + (@cursor.zero? ? 0 : 1)
  end

  def bitsize
    @prefix.size*8 + @cursor
  end

  def <<(bit : UInt8)
    unless bit.in?(0u8, 1u8)
      raise ArgumentError.new("invalid bit digit")
    end

    @state |= bit << (7 - @cursor)
    @cursor += 1

    if @cursor == 8
      @prefix << @state
      @state = 0u8
      @cursor = 0u8
    end

    self
  end

  def progress
    # NOTE: for whatever reason we get garbage if we do not copy here. Probably
    # has to do with realloc/malloc/etc. in the array.
    {@prefix.to_readonly_slice(&.itself), @state, @cursor}
  end

  def detach
    @prefix = @prefix.dup
  end

  def each_byte(&)
    @prefix.each { |byte| yield byte }
    return if @cursor.zero?
    yield @state
  end

  def final
    if @cursor.zero?
      @prefix.to_readonly_slice(&.itself)
    else
      final = Bytes.new(@prefix.size + 1)
      final.copy_from(@prefix.to_readonly_slice)
      final[-1] = @state
      final
    end
  end

  def inspect(io)
    io << @prefix.to_readonly_slice.hexstring << '|' << @cursor
  end
end

struct StaticArray(T, N)
  def to_unsafe_bytes : Bytes
    to_slice.to_unsafe_bytes
  end

  def to_voidptr : Void*
    to_unsafe.as(Void*)
  end

  def to_slice(size : Int)
    to_slice.trim(size)
  end
end

struct Time
  def self.measured(& : -> T) : {Time::Span, T} forall T
    b = Time.instant
    result = yield
    e = Time.instant
    {e - b, result}
  end

  def self.measure(sink : Time::Span ->, & : -> T) : T forall T
    b = Time.instant
    result = yield
    e = Time.instant
    sink.call(e - b)
    result
  end
end

abstract class Digest
end

# Reference: https://github.com/maiha/crc16.cr
#
# CRC16/XMODEM
module Digest::CRC16
  TABLE = UInt16.static_array(
    0x0000, 0x1021, 0x2042, 0x3063, 0x4084, 0x50A5, 0x60C6, 0x70E7,
    0x8108, 0x9129, 0xA14A, 0xB16B, 0xC18C, 0xD1AD, 0xE1CE, 0xF1EF,
    0x1231, 0x0210, 0x3273, 0x2252, 0x52B5, 0x4294, 0x72F7, 0x62D6,
    0x9339, 0x8318, 0xB37B, 0xA35A, 0xD3BD, 0xC39C, 0xF3FF, 0xE3DE,
    0x2462, 0x3443, 0x0420, 0x1401, 0x64E6, 0x74C7, 0x44A4, 0x5485,
    0xA56A, 0xB54B, 0x8528, 0x9509, 0xE5EE, 0xF5CF, 0xC5AC, 0xD58D,
    0x3653, 0x2672, 0x1611, 0x0630, 0x76D7, 0x66F6, 0x5695, 0x46B4,
    0xB75B, 0xA77A, 0x9719, 0x8738, 0xF7DF, 0xE7FE, 0xD79D, 0xC7BC,
    0x48C4, 0x58E5, 0x6886, 0x78A7, 0x0840, 0x1861, 0x2802, 0x3823,
    0xC9CC, 0xD9ED, 0xE98E, 0xF9AF, 0x8948, 0x9969, 0xA90A, 0xB92B,
    0x5AF5, 0x4AD4, 0x7AB7, 0x6A96, 0x1A71, 0x0A50, 0x3A33, 0x2A12,
    0xDBFD, 0xCBDC, 0xFBBF, 0xEB9E, 0x9B79, 0x8B58, 0xBB3B, 0xAB1A,
    0x6CA6, 0x7C87, 0x4CE4, 0x5CC5, 0x2C22, 0x3C03, 0x0C60, 0x1C41,
    0xEDAE, 0xFD8F, 0xCDEC, 0xDDCD, 0xAD2A, 0xBD0B, 0x8D68, 0x9D49,
    0x7E97, 0x6EB6, 0x5ED5, 0x4EF4, 0x3E13, 0x2E32, 0x1E51, 0x0E70,
    0xFF9F, 0xEFBE, 0xDFDD, 0xCFFC, 0xBF1B, 0xAF3A, 0x9F59, 0x8F78,
    0x9188, 0x81A9, 0xB1CA, 0xA1EB, 0xD10C, 0xC12D, 0xF14E, 0xE16F,
    0x1080, 0x00A1, 0x30C2, 0x20E3, 0x5004, 0x4025, 0x7046, 0x6067,
    0x83B9, 0x9398, 0xA3FB, 0xB3DA, 0xC33D, 0xD31C, 0xE37F, 0xF35E,
    0x02B1, 0x1290, 0x22F3, 0x32D2, 0x4235, 0x5214, 0x6277, 0x7256,
    0xB5EA, 0xA5CB, 0x95A8, 0x8589, 0xF56E, 0xE54F, 0xD52C, 0xC50D,
    0x34E2, 0x24C3, 0x14A0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405,
    0xA7DB, 0xB7FA, 0x8799, 0x97B8, 0xE75F, 0xF77E, 0xC71D, 0xD73C,
    0x26D3, 0x36F2, 0x0691, 0x16B0, 0x6657, 0x7676, 0x4615, 0x5634,
    0xD94C, 0xC96D, 0xF90E, 0xE92F, 0x99C8, 0x89E9, 0xB98A, 0xA9AB,
    0x5844, 0x4865, 0x7806, 0x6827, 0x18C0, 0x08E1, 0x3882, 0x28A3,
    0xCB7D, 0xDB5C, 0xEB3F, 0xFB1E, 0x8BF9, 0x9BD8, 0xABBB, 0xBB9A,
    0x4A75, 0x5A54, 0x6A37, 0x7A16, 0x0AF1, 0x1AD0, 0x2AB3, 0x3A92,
    0xFD2E, 0xED0F, 0xDD6C, 0xCD4D, 0xBDAA, 0xAD8B, 0x9DE8, 0x8DC9,
    0x7C26, 0x6C07, 0x5C64, 0x4C45, 0x3CA2, 0x2C83, 0x1CE0, 0x0CC1,
    0xEF1F, 0xFF3E, 0xCF5D, 0xDF7C, 0xAF9B, 0xBFBA, 0x8FD9, 0x9FF8,
    0x6E17, 0x7E36, 0x4E55, 0x5E74, 0x2E93, 0x3EB2, 0x0ED1, 0x1EF0
  )

  def self.checksum(data) : UInt16
    slice = data.to_slice

    checksum = 0u16
    slice.each do |byte|
      checksum = (checksum << 8) ^ TABLE.unsafe_fetch(((checksum >> 8) ^ byte) & 0xFF)
    end

    checksum
  end
end

# TODO: lots of very hot places rely on this. split into buckets & in general
# see SOTA parallel hashes !!! Sync Map is buggy and causes occasional deadlocks.
class SyncHash(K, V)
  def initialize(initial_capacity : Int32? = nil)
    @hash = Hash(K, V).new(initial_capacity: initial_capacity)
    @lock = Sync::RWLock.new
  end

  def size : Int32
    @lock.read { @hash.size }
  end

  def each(& : K, V ->)
    @lock.read do
      @hash.each { |key, value| yield key, value }
    end
  end

  def put_if_absent(key : K, & : -> V) : V
    # Fast path: definitely exists
    @lock.read do
      if value = @hash[key]?
        return value
      end
    end

    # Slow path: probably does not exist
    computed = yield

    @lock.write do
      if value = @hash[key]?
        return value
      end

      @hash[key] = computed
    end
  end


  def []=(key : K, value : V)
    @lock.write { @hash[key] = value }
  end

  def []?(key : K) : V?
    find(key) { |value| return value }
  end

  def delete(key : K)
    @lock.write { @hash.delete(key) }
  end

  def find(key : K, &)
    @lock.read do
      return unless value = @hash[key]?
      yield value
    end
  end

  def update(key : K, &)
    @lock.read do
      return unless value = @hash[key]?
      @hash[key] = yield value
    end
  end

  def select!(& : K, V -> Bool) : Nil
    @lock.write do
      @hash.select! do |key, value|
        yield key, value
      end
    end
  end

  def clear
    @lock.write { @hash.clear }
  end
end

module InspectToS
  def to_s(io)
    inspect(io)
  end
end

module Math
  DEG_TO_RAD = Math::PI/180

  def deg2rad(degrees)
    degrees * DEG_TO_RAD
  end
end

class BlockingSignal
  def initialize
    @epoch = 0u64
    @mutex = Sync::Mutex.new
    @cv = Sync::ConditionVariable.new(@mutex)
  end

  def wait(epoch : UInt64) : UInt64
    @mutex.synchronize do
      loop do
        if @epoch > epoch
          return @epoch
        end

        @cv.wait
      end
    end
  end

  def wait_until(epoch : UInt64, timeout : Nil) : UInt64
    wait(epoch)
  end

  def wait_until(epoch : UInt64, timeout : Time::Span) : UInt64
    if timeout.zero? || timeout.negative?
      call
      return wait(epoch)
    end

    # Fast path
    @mutex.synchronize do
      if @epoch > epoch
        return @epoch
      end
    end

    # Slower path
    Fiber.yield
    @mutex.synchronize do
      if @epoch > epoch
        return @epoch
      end
    end

    # Slowest path.
    #
    # This "contraption" manages to properly cancel the timeout if wait() is
    # woken up; and waits for the cleaning up of the worker fiber on timeout().
    # That I call an achievement, although it is probably quite costly vs.
    # a simple wait()!

    chan = Channel(UInt64).new

    spawn do
      chan.send(wait(epoch))
    end

    select
    when timeout(timeout)
      call
      chan.receive
    when next_epoch = chan.receive
      next_epoch
    end
  end

  def each(&) : Nil
    epoch = 0u64
    loop do
      epoch = wait(epoch)
      yield
    end
  end

  def call
    @mutex.synchronize do
      @epoch += 1
      @cv.broadcast
    end
  end
end

class BlockingSlot(T)
  defrecord Empty
  defrecord Nonempty(T), object : T

  @slot : Empty | Nonempty(T)

  def initialize
    @slot = Empty.new
    @mutex = Sync::Mutex.new
    @cv = Sync::ConditionVariable.new(@mutex)
  end

  def set(object : T) : Nil
    @mutex.synchronize do
      @slot = Nonempty.new(object)
      @cv.signal
    end
  end

  def get : T
    @mutex.synchronize do
      loop do
        if slot = @slot.as?(Nonempty)
          object = slot.object
          @slot = Empty.new
          return object
        end

        @cv.wait
      end
    end
  end
end

class BlockingQueue(T)
  def initialize
    @queue = Deque(T).new
    @mutex = Sync::Mutex.new
    @cv = Sync::ConditionVariable.new(@mutex)
  end

  def present? : Bool
    @mutex.synchronize { @queue.present? }
  end

  def interject(*objects : T) : Nil
    @mutex.synchronize do
      objects.reverse_each do |object|
        @queue.unshift(object)
      end
      @cv.signal
    end
  end

  def enqueue(*objects : T) : Nil
    @mutex.synchronize do
      objects.each do |object|
        @queue << object
      end
      @cv.signal
    end
  end

  def <<(object : T) : self
    enqueue(object)

    self
  end

  def shift : T
    @mutex.synchronize do
      loop do
        unless @queue.empty? # remember we can send nils as well!!
          return @queue.shift
        end

        @cv.wait
      end
    end
  end

  def shift? : T?
    @mutex.synchronize do
      @queue.shift?
    end
  end
end

# Copied from https://github.com/ysbaddaden/sync

module ::Sync
  annotation Safe
  end

  # :nodoc:
  struct Dll(T)
    module Node
      macro included
        property next : ::Pointer(self) = ::Pointer(self).null
        property prev : ::Pointer(self) = ::Pointer(self).null

        macro init(*args)
          \%node = ::{{@type}}.new(\{{args.splat}})
          ::Sync::Dll(::{{@type}}).init(pointerof(\%node))
          pointerof(\%node)
        end

        def alone? : Bool
          @next == @prev
        end
      end
    end

    # Nodes must be explicitly initialized to point to themselves, so they act
    # as a circular list with a single element. Since we include Node into
    # structs, we can't have the initializer do it automatically (the struct is
    # returned by copy, and the pointers become invalid).
    def self.init(node : Pointer(T)) : Nil
      node.value.next = node
      node.value.prev = node
    end

    # Points to the `Node` at the tail of the list.
    @list : Pointer(T) = Pointer(T).null

    def empty? : Bool
      @list.null?
    end

    # Returns the last node in the list. Returns a NULL pointer when empty.
    def last? : Pointer(T)
      @list
    end

    # Returns the first node in the list. Returns a NULL pointer when empty.
    def first? : Pointer(T)
      if list = @list
        list.value.next
      else
        Pointer(T).null
      end
    end

    # Returns the node that comes immediately after *node* in the list. Returns
    # a NULL pointer if *node* is the last node in the list.
    def next?(node : Pointer(T)) : Pointer(T)
      if node != @list
        node.value.next
      else
        Pointer(T).null
      end
    end

    # Returns the node that comes immediately before *node* in the list. Returns
    # a NULL pointer id *node* is the first node in the list.
    def prev?(node : Pointer(T)) : Pointer(T)
      if node != @list.value.next
        node.value.prev
      else
        Pointer(T).null
      end
    end

    # Yields each node in the list. The block owns the node; it can be deleted
    # from the list, for example, then inserted into another list.
    def each(& : Pointer(T) ->) : Nil
      node = first?

      while node
        next_ = next?(node)
        yield node
        node = next_
      end
    end

    # Removes and yields each node in the list. The block owns the node; it can
    # be inserted into another list, for example.
    def consume_each(& : Pointer(T) ->) : Nil
      while list = @list
        node = list.value.next
        delete(node)
        yield node
      end
    end

    # Removes *node* from the list.
    def delete(node : Pointer(T)) : Nil
      if @list == node
        if @list.value.prev == @list
          @list = Pointer(T).null
        else
          @list = @list.value.prev
        end
      end
      node.value.next.value.prev = node.value.prev
      node.value.prev.value.next = node.value.next
      node.value.next = node
      node.value.prev = node
    end

    # Removes and returns the last *node* from the list.
    def pop? : Pointer(T)
      if node = @list
        delete(node)
        node
      else
        Pointer(T).null
      end
    end

    # Removes and returns the first *node* from the list.
    def shift? : Pointer(T)
      if (list = @list) && (node = list.value.next)
        delete(node)
        node
      else
        Pointer(T).null
      end
    end

    # Inserts *node* into the list, at the beginning.
    def unshift(node : Pointer(T)) : Nil
      unless node.null?
        # raise "BUG: #{node} isn't in pristine state" unless node.value.alone?

        if @list.null?
          @list = node.value.prev
        else
          self.class.splice_after(@list, node)
        end
      end
    end

    # Inserts *node* into the list, at the end.
    def push(node : Pointer(T)) : Nil
      unless node.null?
        # raise "BUG: #{node} isn't in pristine state" unless node.value.alone?

        unshift(node.value.next)
        @list = node
      end
    end

    # Makes *succ* and its successors come after *node*.
    def self.splice_after(node : Pointer(T), succ : Pointer(T)) : Nil
      tmp1 = node.value.next
      tmp2 = succ.value.prev

      node.value.next = succ
      succ.value.prev = node

      tmp2.value.next = tmp1
      tmp1.value.prev = tmp2
    end
  end

  struct Waiter
    include Dll::Node
  end

  class Error
    # Raised by `Future` when the future failed without an explicit exception.
    class Failed < Error
    end
  end

  # An object that will eventually hold a value.
  #
  # You can for example delegate the computation of a value to another fiber,
  # that can resolve is asynchronously, without blocking the current fiber, that
  # can regularly poll for the value, or explicitly wait until the value is
  # resolved.
  #
  # For example:
  #
  # ```
  # result = Future(Int32).new
  #
  # spawn do
  #   result.set(compute_some_value)
  # rescue exception
  #   result.fail(exception)
  # end
  #
  # loop do
  #   do_something
  #
  #   if value = result.get?
  #     p value
  #     break
  #   end
  # end
  # ```
  @[Sync::Safe]
  class Future(T)
    # :nodoc:
    enum State
      UNSET
      RESOLVED
      FAILED
    end

    @reason : Exception | String | Nil

    def initialize
      {% if (T.union? && T.union_types.any? { |t| t == Nil }) || T == Nil %}
        {% raise "Can't create Sync::Future for a nilable type" %}
      {% end %}
      @value = uninitialized T
      @mu = MU.new
      @state = State::UNSET
      @waiters = Dll(Waiter).new
    end

    # Sets the value, then wakes up pending fibers.
    #
    # Raises a `RuntimeError` if the future has already been resolved or has
    # already failed.
    def set(value : T) : T
      resolve(State::RESOLVED) { @value = value }
      value
    end

    # Sets the future as failed, then wakes up pending fibers.
    #
    # Raises a `RuntimeError` if the future has already been resolved or has
    # already failed.
    def fail(reason : Exception | String | Nil = nil) : Nil
      resolve(State::FAILED) { @reason = reason }
    end

    private def resolve(new_state, &)
      @mu.lock

      unless @state.unset?
        @mu.unlock
        raise RuntimeError.new("Can't resolve a future twice")
      end

      # we need an explicit fence for the compiler and weak cpu architectures
      # (e.g. ARM) to not reorder the memory stores, so any thread can safely
      # access the value, or the reason, depending on the observed state
      yield
      Atomic.fence(:acquire_release)
      @state = new_state

      @mu.unlock

      # @waiters is owned by the current fiber (neither #resolve nor #get will
      # try to access it anymore), we can safely iterate the list
      @waiters.consume_each(&.value.wake)
    end

    # Returns the value if resolved, otherwise returns `nil` immediately.
    # Raises an exception if the future has failed.
    def get? : T?
      case @state
      when State::RESOLVED
        @value
      when State::FAILED
        raise_exception!
      end
    end

    # Returns the value.
    # Blocks the current fiber until the future is resolved.
    # Raises an exception if the future has failed.
    def get : T
      loop do
        case @state
        when State::RESOLVED
          return @value
        when State::FAILED
          raise_exception!
        when State::UNSET
          waiter = Waiter.init(:reader)
          @mu.lock
          if @state.unset?
            @waiters.push(waiter)
            @mu.unlock
            waiter.value.wait
          else
            @mu.unlock
          end
        end
      end
    end

    private def raise_exception! : NoReturn
      case reason = @reason
      in Exception
        raise reason
      in String
        raise Error::Failed.new(reason)
      in Nil
        raise Error::Failed.new
      end
    end

    # :nodoc:
    def dup
      {% raise "Can't dup {{@type}}" %}
    end

    def inspect(io)
      io << "#<Sync::Future:0x"
      object_id.to_s(io, 16)
      io << " ...>"
    end
  end
end

class ::WaitGroup
  def inspect(io)
    io << "#<WaitGroup:0x"
    object_id.to_s(io, 16)
    io << " ...>"
  end
end

# Reference: https://github.com/crystal-lang/crystal/issues/13481#issuecomment-2603298285
macro stack_alloc(call)
 {% if call.is_a?(Assign) %}
   {% target = call.target %}
   {% call = call.value %}
   {{ target }} = uninitialized ReferenceStorage({{ call.receiver }})
   {{ call.receiver }}.unsafe_construct(pointerof({{ target }}), {% unless call.args.empty? %} {{ call.args.splat }}, {% end %}{% unless call.named_args.is_a?(Nop) %}{{ call.named_args.splat }}{% end %})
 {% else %}
   stack_alloc %storage = {{ call }}
 {% end %}
end

macro nested_scopes_rec(objects, types, &block)
  {% if type = types[0] %}
    {{type}}.scope do |%object|
      nested_scopes_rec([{{objects.splat(",")}} %object], {{types[1..]}} of ::NoReturn) {{block}}
    end
  {% else %}
    pass({{objects.splat}}) {{block}}
  {% end %}
end

macro nested_scopes(*args, &block)
  nested_scopes_rec([] of ::NoReturn, [{{args.splat}}] of ::NoReturn) {{block}}
end

class List(T)
  include Enumerable(T)

  # @type_id : Int32

  # Returns the size of this list.
  getter size : Int32

  # Returns a list containing all elements except the last one.
  getter? prior : List(T)?

  # Returns the last element in this list.
  getter last : T

  def initialize(@size, @prior, @last)
  end

  def self.append(arena : Arena(List(T), _), pred : List(T)?, object : T)
    arena.construct(pred ? pred.size + 1 : 1, pred, object)
  end

  # Yields each element in this list back-to-front.
  def reverse_each(& : T ->) : Nil
    current = self
    while current
      yield current.last
      current = current.prior?
    end
  end

  # Yields each element in this list front-to-back.
  def each(& : T ->) : Nil
    objects = Pf::Kit.stack_array(T, 16)

    reverse_each do |object|
      objects << object
    end

    objects.reverse_each do |object|
      yield object
    end
  end

  def pretty_print(pp)
    pp.list("List[", self, "]")
  end

  def inspect(io)
    io << "List["
    join(io, ", ", &.inspect(io))
    io << "]"
  end

  def to_s(io)
    io << "List["
    join(io, ", ")
    io << "]"
  end

  def_equals_and_hash @prior, @last
end

class Arena(T, N)
  MINCAP = 16

  # @type_id : Int32
  @auxcap : Int32

  @memsize : Int32
  @auxsize : Int32

  @mem : ReferenceStorage(T)*
  @aux : ReferenceStorage(T)*

  # :nodoc:
  def initialize(@mem)
    @aux = typeof(@aux).null
    @memsize = 0
    @auxsize = 0
    @auxcap = 0
  end

  # Yields an arena whose lifetime is equal to the lifetime of the block.
  #
  # WARNING: No checks are done with respect to the lifetime: this is not Rust.
  # It is entirely your responsibility to make sure memory stays tidy.
  def self.scope(&)
    mem = uninitialized ReferenceStorage(T)[N]
    arena = stack_alloc Arena(T, N).new(mem.to_unsafe)

    yield arena
  end

  def size
    @auxsize + @memsize
  end

  def construct(*args, **kwargs) : T
    if @memsize + 1 > N
      if @auxsize + 1 > @auxcap
        @auxcap = Math.max(MINCAP, @auxcap + @auxcap//2)
        @aux = typeof(@aux).malloc(@auxcap)
        @auxsize = 0
      end

      dstptr = @aux + @auxsize
      @auxsize += 1
    else
      dstptr = @mem + @memsize
      @memsize += 1
    end

    T.unsafe_construct(dstptr, *args, **kwargs)
  end
end

struct ListMap(K, V)
  include Enumerable({K, V})

  getter entries, filter

  # :nodoc:
  def initialize(@entries : List({K, V})?, @filter : UInt64)
  end

  def self.new : ListMap(K, V)
    new(entries: nil, filter: 0u64)
  end

  # :nodoc:
  def self.hash64(object : Term)
    Term.hashcode(object)
  end

  # :nodoc:
  def self.hash64(object)
    object.hash
  end

  # :nodoc:
  def self.fbit64(object)
    hash64(object) % 64
  end

  # :nodoc:
  def self.fset64?(filter : UInt64, object)
    filter & (1u64 << fbit64(object)) > 0
  end

  # :nodoc:
  def self.fmix64(filter : UInt64, key)
    filter | (1u64 << fbit64(key))
  end

  def self.assoc(arena : Arena, map : ListMap(K, V), key : K, value : V)
    new(List.append(arena, map.entries, {key, value}), fmix64(map.filter, key))
  end

  def probably_includes?(key : K) : Bool
    ListMap.fset64?(@filter, key)
  end

  def has_key?(key : K) : Bool
    !!self[key]?
  end

  def each(& : {K, V} ->)
    return unless entries = @entries

    seen = Pf::Kit.stack_array(K, 16)

    entries.reverse_each do |(key, value)|
      next if seen.any?(key)
      yield({key, value})
      seen << key
    end
  end

  def fetch(key needle : K, & : -> W) : V | W forall W
    return yield unless entries = @entries
    return yield unless probably_includes?(needle)

    entries.reverse_each do |key, value|
      next unless key == needle
      return value
    end

    yield
  end

  def []?(key : K) : V?
    fetch(key) { nil }
  end

  def [](key : K) : V
    fetch(key) { raise KeyError.new }
  end

  def pretty_print(pp) : Nil
    pp.list("ListMap{", self, "}") do |key, value|
      pp.group do
        key.pretty_print(pp)
        pp.text " =>"
        pp.nest do
          pp.breakable
          value.pretty_print(pp)
        end
      end
    end
  end
end

module Benchmark
  def memory(cont : Int64 ->, &)
    result = nil
    mem = memory { result = {yield} }
    cont.call(mem)
    result.not_nil![0]
  end
end

struct HybridMap(K, V)
  include Enumerable({K, V})

  # Size of the stack-allocated buffer.
  N = 8

  # :nodoc:
  def initialize(@keys : Pf::Kit::HybridArray(K, N), @values : Pf::Kit::HybridArray(V, N))
    assert @keys.empty? && @values.empty?
  end

  def self.scope(& : HybridMap(K, V) ->)
    keys = Pf::Kit.stack_array(K, N)
    values = Pf::Kit.stack_array(V, N)

    yield new(keys, values)
  end

  def empty? : Bool
    @keys.empty?
  end

  def each(& : {K, V} ->)
    @keys.zip(@values) do |key,value|
      yield({key, value})
    end
  end

  def fetch(key : K, &)
    @keys.each_with_index do |candidate, index|
      next unless candidate == key
      return @values.unsafe_fetch(index)
    end

    yield
  end

  def []?(key : K) : V?
    fetch(key) { }
  end

  def [](key : K) : V
    fetch(key) { raise KeyError.new }
  end

  def []=(key : K, value : V) : V
    @keys.each_with_index do |candidate, index|
      next unless candidate == key
      # Found
      @values.unsafe_put(index, value)
      return value
    end

    # Not found
    @keys << key
    @values << value
    value
  end

  def clear : Nil
    @keys.clear
    @values.clear
  end

  def pretty_print(pp) : Nil
    pp.list("HybridMap{", self, "}") do |key, value|
      pp.group do
        key.pretty_print(pp)
        pp.text " =>"
        pp.nest do
          pp.breakable
          value.pretty_print(pp)
        end
      end
    end
  end
end

SYNC_RAND      = Random::PCG32.new
SYNC_RAND_LOCK = Sync::Mutex.new

def sync_rand(arg)
  SYNC_RAND_LOCK.synchronize do
    SYNC_RAND.rand(arg)
  end
end

module ::Compress::Gzip
  def self.compress(data) : Bytes
    io = IO::Memory.new
    Compress::Gzip::Writer.open(io, level: level, &.write(data.to_slice))
    io.to_slice
  end

  def self.decompress(data, *, level = BEST_SPEED) : Bytes
    io = IO::Memory.new(data.to_slice)
    Compress::Gzip::Reader.open(io, &.getb_to_end)
  end
end

struct ::Path
  def extension?(*extensions : String)
    return false if @name.bytesize < 3

    bytes = @name.to_slice
    separators = self.separators.map &.ord

    # Ignore trailing separators
    offset = bytes.size - 1
    while bytes.unsafe_fetch(offset).in? separators
      return false if offset == 0
      offset -= 1
    end

    # Get the first occurrence of a separator or a '.' past the trailing separators
    dot_index = bytes.rindex(offset: offset) { |byte| byte === '.' || byte.in? separators }

    # Return "" if '.' is the first character (ex. ".dotfile"),
    # or if the '.' character follows after a separator (ex. "pathto/.dotfile")
    # or if the character at the returned index is a separator (ex. "no/extension")
    # or if the filename ends with a '.'
    return false unless dot_index
    return false if dot_index == 0
    return false if dot_index == offset
    return false if bytes.unsafe_fetch(dot_index - 1).in?(separators)
    return false if bytes.unsafe_fetch(dot_index).in?(separators)

    extensions.any? do |extension|
      extension.to_slice == bytes[dot_index, offset - dot_index + 1]
    end
  end

  def normal? : Bool
    return false if windows? # Because who the f would use Windows!!!1
    return false if @name.empty?

    behind = '\0'

    @name.each_char do |ahead|
      return false if behind == '.' && ahead == '.'
      return false if behind == '.' && ahead.in?(separators)
      return false if behind.in?(separators) && ahead == '.'
      return false if behind.in?(separators) && ahead.in?(separators)

      behind = ahead
    end

    !(behind == '.' || behind.in?(separators))
  end
end

class ::File
  def self.tempfile(random : ::Random, *, tempdir : String | Path = Dir.tempdir)
    fileno, path, blocking = Crystal::System::File.mktemp(prefix: nil, suffix: nil, dir: tempdir.to_s, random: random)
    new(path, fileno, blocking: blocking)
  end
end

module Enumerable(T)
  def adjoin_by(&)
    clusters = [] of Slice(T)
    cluster = Pf::Kit.stack_array(T, 8)
    trait = Tuple.new

    each_with_index do |object, index|
      trait1 = {(yield object, index)}
      if trait == trait1
        cluster << object
        next
      end

      if trait.empty?
        cluster << object
        trait = trait1
        next
      end

      clusters << cluster.to_readonly_slice(&.itself)
      cluster.clear
      cluster << object
      trait = trait1
    end

    unless trait.empty?
      clusters << cluster.to_readonly_slice(&.itself)
      cluster.clear
    end

    clusters
  end
end

struct Range(B, E)
  def adjoin_by(&)
    {% unless B == E && B < ::Int %}
      {% raise "expected a range with the same begin and end integer type" %}
    {% end %}

    unless exclusive?
      raise ArgumentError.new("expected an exclusive range")
    end

    clusters = [] of Range(B, E)
    cluster = B.zero...B.zero
    trait = Tuple.new

    each do |object|
      trait1 = {yield object}
      if trait == trait1
        cluster = cluster.begin...cluster.end + 1
        next
      end

      if trait.empty?
        cluster = cluster.begin...cluster.end + 1
        trait = trait1
        next
      end

      clusters << cluster
      cluster = cluster.end...cluster.end + 1
      trait = trait1
    end

    unless trait.empty?
      clusters << cluster
    end

    clusters
  end
end

class Promise(T)
  struct Accepted(T)
    getter object : T

    # :nodoc:
    def initialize(@object)
    end

    def unwrap(cls : U.class = T) forall U
      @object.as(U)
    end

    def unwrap?
      @object
    end
  end

  struct Rejected
    getter detail : String

    def initialize(@detail)
    end

    def unwrap(cls)
      raise RejectedError.new(@detail)
    end

    def unwrap
      raise RejectedError.new(@detail)
    end

    def unwrap?
    end
  end

  class RejectedError < Exception
  end

  # :nodoc:
  def initialize(@poll : -> Accepted(T) | Rejected | Nil, @wait : -> Accepted(T) | Rejected)
  end

  def self.accepted(object)
    Accepted(T).new(object).as(Accepted(T) | Rejected)
  end

  def self.rejected(detail : String)
    Rejected.new(detail)
  end

  def self.resolved(object)
    poll = -> { Promise(T).accepted(object).as(Accepted(T) | Rejected | Nil)}
    wait = -> { Promise(T).accepted(object) }

    new(poll, wait)
  end

  def self.new(future : Sync::Future(T)) forall T
    poll = -> do
      if object = future.get?
        Accepted(T).new(object).as(Accepted(T) | Rejected)
      end
    end

    wait = -> do
      object = future.get

      Accepted(T).new(object).as(Accepted(T) | Rejected)
    end

    new(poll, wait)
  end

  def wait : Accepted(T) | Rejected
    @wait.call
  end

  def poll? : Accepted(T) | Rejected | Nil
    @poll.call
  end

  def map(&fn : T -> Accepted(U) | Rejected) forall U
    poll = -> do
      state = @poll.call

      case state
      in Nil
      in Accepted(T) then fn.call(state.object)
      in Rejected    then state
      end
    end

    wait = -> do
      state = @wait.call

      case state
      in Accepted(T) then fn.call(state.object)
      in Rejected    then state
      end
    end

    Promise(U).new(poll, wait)
  end

  def bind(&fn : T -> Promise(U)) : Promise(U) forall U
    inner = nil
    lock = Sync::Mutex.new

    poll = -> do
      result = lock.synchronize do
        if tmp = inner
          next tmp
        end

        state = @poll.call

        case state
        in Nil
        in Accepted(T)
          tmp = fn.call(state.object)
          inner = tmp
          tmp
        in Rejected
          state
        end
      end

      case result
      in Nil
      in Promise(U) then result.poll?
      in Rejected then result
      end
    end

    wait = -> do
      state = @wait.call

      case state
      in Accepted(T)
      in Rejected
        return state
      end

      promise = lock.synchronize do
        (inner ||= fn.call(state.object)).not_nil!
      end

      promise.wait
    end

    Promise(U).new(poll, wait)
  end
end

# A map of *K*s to a source of promises of *V*. Promises can be `add`ed,
# then polled by *K* using `[]?`. Promises can be removed by *K*. Importantly,
# *K*s can be `invalidate`d; this makes the map ask the promise source for
# a new promise.
class PromiseMap(K, V)
  def initialize
    @lock = Sync::Mutex.new
    @sources = {} of K => (-> Promise(V))
    @promises = {} of K => Promise(V)
  end

  # Returns `true` if a promise with the given *key* exists.
  def includes?(key : K) : Bool
    @lock.synchronize { @promises.has_key?(key) }
  end

  def size : Int32
    @lock.synchronize { @promises.size }
  end

  def each_key(& : K ->) : Nil
    @lock.synchronize do
      @promises.each_key do |key|
        yield key
      end
    end
  end

  # Polls the promise for *key*. Returns `nil` if no such promise exists,
  # or if the promise rejects (in that case it will keep rejecting until
  # you invalidate it).
  def []?(key : K) : V?
    return unless promise = @lock.synchronize { @promises[key]? }
    return unless result = promise.poll?

    result.unwrap?
  end

  def touch(key : K) : Nil
    return unless promise = @lock.synchronize { @promises[key]? }

    _ = promise.poll?
  end

  # Registers a source for *key*.
  def add(key : K, &fn : -> Promise(V)) : Nil
    promise = fn.call

    @lock.synchronize do
      @sources[key] = fn
      @promises[key] = promise
    end
  end

  # Removes the promise for *key*.
  def delete(key : K) : Nil
    @lock.synchronize do
      @sources.delete(key)
      @promises.delete(key)
    end
  end

  # Invalidates just the promise for *key*. Returns `true` if invalidated. Returns
  # `false` if *key* does not exist.
  def invalidate?(key : K) : Bool
    unless source = @lock.synchronize { @sources[key]? }
      return false
    end

    promise = source.call

    @lock.synchronize do
      # If someone called add() while we were calling source, this means
      # they've called source and the promise is up-to-date already (and so,
      # invalidated already).
      #
      # If someone called delete(), likewise, the promise does not exist. We
      # don't have to do anything.
      unless @sources[key]? == source
        return false
      end

      @promises[key] = promise
    end

    true
  end

  # Invalidates all of keys. Returns `true` if any key was invalidated.
  def invalidate?(*keys : K) : Bool
    invalidated = false

    keys.each do |key|
      if invalidate?(key)
        invalidated = true
      end
    end

    invalidated
  end

  # Invalidates all promises. Returns `true` if at least one promise was
  # invalidated.
  def invalidate? : Bool
    @lock.synchronize do
      if @sources.empty?
        return false
      end

      @promises.clear
      @sources.each do |key, source|
        @promises[key] = source.call
      end

      true
    end
  end
end

lib LibC
  fun fdopendir(fd : LibC::Int) : DIR*  # ?!?!?!?!?
end

class ::PrettyPrint
  # Forces a break.
  def break : Nil
    flush
    @output << @newline
    @indent.times { @output << ' ' }
    @output_width = @indent
    @buffer_width = 0
  end
end

struct ::Pf::StringSeln
  # :nodoc:
  #
  # WARNING: *bytes* MUST be valid UTF-8 bytes.
  # WARNING: *bytes* MUST start and end on UTF-8 character boundaries.
  # WARNING: *bytesize* must be in `1..4`.
  def self.unsafe_chr(bytes : UInt8*, bytesize : UInt32) : Char
    case bytesize
    when 1u32
      codepoint = bytes[0]
    when 2u32
      byte0 = bytes[0].to_u32
      byte1 = bytes[1].to_u32
      codepoint = ((byte0 & 0x1F) << 6) | (byte1 & 0x3F)
    when 3u32
      byte0 = bytes[0].to_u32
      byte1 = bytes[1].to_u32
      byte2 = bytes[2].to_u32
      codepoint = ((byte0 & 0x0F) << 12) | ((byte1 & 0x3F) << 6) | (byte2 & 0x3F)
    when 4u32
      byte0 = bytes[0].to_u32
      byte1 = bytes[1].to_u32
      byte2 = bytes[2].to_u32
      byte3 = bytes[3].to_u32
      codepoint = ((byte0 & 0x07) << 18) | ((byte1 & 0x3F) << 12) | ((byte2 & 0x3F) << 6) | (byte3 & 0x3F)
    else
      Intrinsics.unreachable
    end

    codepoint.unsafe_chr
  end

  # :nodoc:
  #
  # WARNING: *bytes* MUST be valid UTF-8 bytes.
  # WARNING: *bytes* MUST start and end on UTF-8 character boundaries.
  def self.unsafe_chrsize_and_flags(bytes : UInt8*, bytesize : UInt32) : {UInt32, SelnFlags}
    if bytesize.zero?
      return 0u32, SelnFlags::None
    end

    head = bytes[0]
    if head >= 0x80
      return (~head).leading_zeros_count.to_u32, SelnFlags::None
    end

    {1u32, SelnFlags::AsciiChar}
  end

  # :nodoc:
  #
  # WARNING: *bytes* MUST be valid UTF-8 bytes.
  # WARNING: *bytes* MUST start and end on UTF-8 character boundaries.
  def self.unsafe_rchrsize_and_flags(bytes : UInt8*, bytesize : UInt32) : {UInt32, SelnFlags}
    if bytesize.zero?
      return 0u32, SelnFlags::None
    end

    # We're unsafe, and *bytes* is valid UTF-8, so we consider OOB impossible here;
    # i.e., if valid UTF-8 says "continue", we continue.

    unless (bytes[bytesize &- 1] & 0xC0) == 0x80
      return 1u32, SelnFlags::AsciiChar
    end

    unless (bytes[bytesize &- 2] & 0xC0) == 0x80
      return 2u32, SelnFlags::None
    end

    unless (bytes[bytesize &- 3] & 0xC0) == 0x80
      return 3u32, SelnFlags::None
    end

    {4u32, SelnFlags::None}
  end

  private def self.continuation_byte?(byte : UInt8) : Bool
    {byte.bit(7), byte.bit(6)} == {1, 0}
  end

  private def self.continuation_count(bytes : Bytes) : Int32
    if bytes.size == 8
      blks = bytes.unsafe_slice_of(UInt64)
      blk = blks.unsafe_fetch(0)
      mask = (blk & 0x8080808080808080u64) & ~(blk << 1)
      return mask.popcount.to_i
    end

    count = 0

    bytes.each do |byte|
      next unless continuation_byte?(byte)

      # See the NOTE below: continuation count can never exceed bytesize, and
      # bytesize is in Int32.
      count &+= 1
    end

    count
  end

  # :nodoc:
  #
  # WARNING: *bytes* MUST be valid UTF-8 bytes.
  # WARNING: *bytes* MUST start and end on UTF-8 character boundaries.
  def self.unsafe_measure(bytes : Bytes) : Int32
    # NOTE: Max continuation count is *bytes* bytesize which is in Int32 bounds.
    continuation_count = 0
    bytes.in_chunks_of(size_lte: 8) do |blk|
      continuation_count &+= continuation_count(blk)
    end

    bytes.size &- continuation_count
  end
end

struct ::Pf::StringSeln
  # Returns the starting byte of this selection (inclusive).
  getter byte_start : UInt32
  # Returns the ending byte of this selection (exclusive).
  getter byte_end : UInt32

  @[Flags]
  enum SelnFlags : UInt32
    AsciiChar
  end

  # :nodoc:
  #
  # WARNING: *trunk* MUST be valid UTF-8.
  # WARNING: `byte_start <= byte_end <= trunk.bytesize` must be true.
  def initialize(@trunk : String, @byte_start : UInt32, @byte_end : UInt32, @flags : SelnFlags = SelnFlags::None)
    # We call this a lot, sometimes per character, so checking this every time
    # is quite expensive.
    # {% if flag?(:safe) %}
      assert @byte_start <= @byte_end <= @trunk.bytesize
    # {% end %}
  end

  # Constructs a string selection from the given *string*.
  #
  # This forces an encoding check on *string*. This method requires *string* to
  # be a valid UTF-8 string.
  #
  # If you want to `scrub`, use `new_scrub` instead to skip the check.
  def self.new(string : String) : StringSeln
    new?(string) || raise ArgumentError.new("string must be valid UTF-8")
  end

  def self.new?(string : String) : StringSeln?
    return unless string.valid_encoding?

    new(string, byte_start: 0u32, byte_end: string.bytesize.to_u32)
  end

  # Constructs a string selection from the given *string*, replacing invalid
  # bytes (according to UTF-8) with *replacement*.
  #
  # See also: `String#scrub`.
  #
  # Prefer this method over `new(string.scrub)` to avoid the overhead of validating
  # the encoding.
  def self.new_scrub(string : String, replacement : Char = Char::REPLACEMENT) : StringSeln
    string = string.scrub(replacement)

    new(string, byte_start: 0u32, byte_end: string.bytesize.to_u32)
  end

  # Returns `true` if *seln*, *selns* are all of common descent, pointing
  # to the same trunk.
  def self.siblings?(seln ref : StringSeln, *selns : StringSeln) : Bool
    selns.all? { |seln| ref.@trunk.same?(seln.@trunk) }
  end

  def self.contiguous?(a : StringSeln, b : StringSeln) : Bool
    siblings?(a, b) && a.byte_end <= b.byte_start
  end

  # Joins two contiguous selections *a*, *b*. *a* must end exactly where *b* begins.
  def self.chain(a : StringSeln, b : StringSeln) : StringSeln
    assert siblings?(a, b)
    assert a.byte_end == b.byte_start

    new(a.@trunk, a.byte_start, b.byte_end)
  end

  def self.span(a : StringSeln, b : StringSeln) : StringSeln
    assert siblings?(a, b)

    new(a.@trunk, a.byte_start, b.byte_end)
  end

  def self.between(a : StringSeln, b : StringSeln) : StringSeln
    assert contiguous?(a, b)

    new(a.@trunk, a.byte_end, b.byte_start)
  end

  def self.extend(a : StringSeln, b : StringSeln) : StringSeln
    assert siblings?(a, b)

    new(a.@trunk, a.byte_start, b.byte_start)
  end

  private def startptr : UInt8*
    @trunk.to_unsafe + @byte_start
  end

  def <=>(other : StringSeln) : Int32
    # Fast path.
    if @trunk.same?(other.@trunk) && {byte_start, byte_end} == {other.byte_start, other.byte_end}
      return 0
    end

    (to_slice <=> other.to_slice).sign
  end

  # Returns the number of selected bytes.
  def bytesize : UInt32
    @byte_end &- @byte_start
  end

  def byte_bounds : Range(UInt32, UInt32)
    @byte_start...@byte_end
  end

  # Expands this selection to enclose the entire trunk string.
  def expand : StringSeln
    StringSeln.new(@trunk, 0u32, @trunk.bytesize)
  end

  # Returns the index of the first selected character.
  def char_start : UInt32
    offset = 0u64
    index = 0u32

    loop do
      chrsize, _ = StringSeln.unsafe_chrsize_and_flags(@trunk.to_unsafe + offset, @trunk.bytesize.to_u32)
      if offset == @byte_start
        return index
      end

      offset += chrsize
      index += 1
    end
  end

  # Returns the index of the last selected character.
  def char_end : UInt32
    offset = 0u64
    index = 0u32

    loop do
      chrsize, _ = StringSeln.unsafe_chrsize_and_flags(@trunk.to_unsafe + offset, @trunk.bytesize.to_u32)
      if offset == @byte_end
        return index
      end

      offset += chrsize
      index += 1
    end
  end

  # Returns the number of selected characters.
  #
  # NOTE: Unlike `String`, this method does *not* cache the size -- since
  # `StringSeln` is located entirely on the stack (minus the trunk string),
  # there's nowhere for the size to go.
  #
  # However, I tried to optimize this method well. In fact, due to the invariants
  # of `StringSeln`, it runs about 10x faster than uncached `String#size` on
  # my machine (the example I used was a 20MiB string; the results were: Crystal ~11ms,
  # StringSeln ~1.25ms).
  #
  # The design is that you aren't expected to call this method very often. If you
  # do end up doing that, we try really hard to remain fast, but ultimately,
  # this is still worst-case O(N).
  def size : Int32
    StringSeln.unsafe_measure(to_slice)
  end

  # Returns `true` if this selection contains zero characters.
  def empty? : Bool
    @byte_start == @byte_end
  end

  # Returns `true` if this selection contains one or more characters.
  def nonempty? : Bool
    !empty?
  end

  def covers_fully? : Bool
    {@byte_start, @byte_end} == {0u32, @trunk.bytesize.to_u32}
  end

  def includes?(object : Char) : Bool
    each_char do |chr|
      return true if chr == object
    end

    false
  end

  def prefixed_by?(l : Char | String) : Bool
    bytesize > l.bytesize && starts_with?(l)
  end

  def postfixed_by?(r : Char | String) : Bool
    bytesize > r.bytesize && ends_with?(r)
  end

  def surrounded_by?(l : Char | String, r : Char | String) : Bool
    bytesize > l.bytesize + r.bytesize && starts_with?(l) && ends_with?(r)
  end

  def starts_with?(prefix : Char) : Bool
    bytesize >= prefix.bytesize && first_char == prefix
  end

  def starts_with?(prefix : String) : Bool
    to_slice.starts_with?(prefix.to_slice)
  end

  def ends_with?(suffix : Char) : Bool
    bytesize >= suffix.bytesize && suffix == last_char
  end

  def ends_with?(suffix : String) : Bool
    to_slice.ends_with?(suffix.to_slice)
  end

  # Returns `true` if this selection is empty or contains exclusively
  # whitespace (see `Char#whitespace?`).
  def blank? : Bool
    each_char do |chr|
      next if chr.whitespace?
      return false
    end

    true
  end

  # Splits this selection into two: the first selection contains the first
  # character, and the second one contains the rest of characters. If this
  # selection is empty, returns two empty selections.
  def first_and_rest : {StringSeln, StringSeln}
    chrsize, flags = StringSeln.unsafe_chrsize_and_flags(startptr, bytesize)
    first = StringSeln.new(@trunk, @byte_start, @byte_start + chrsize, flags)
    rest = StringSeln.new(@trunk, @byte_start + chrsize, @byte_end)
    {first, rest}
  end

  # Splits this selection into two: the first selection contains characters before
  # the last character, and the second selection contains the last character.
  def prior_and_last : {StringSeln, StringSeln}
    chrsize, flags = StringSeln.unsafe_rchrsize_and_flags(startptr, bytesize)
    prior = StringSeln.new(@trunk, @byte_start, @byte_end - chrsize, flags)
    last = StringSeln.new(@trunk, @byte_end - chrsize, @byte_end)
    {prior, last}
  end

  # Selects the *first* character if there are one or more characters. If there are
  # zero characters, the returned selection is empty.
  def first : StringSeln
    first, _ = first_and_rest
    first
  end

  # Selects characters after the *first* character in this selection. If there are
  # zero characters, the returned selection is empty.
  def rest : StringSeln
    _, rest = first_and_rest
    rest
  end

  # Selects characters before the *last* character in this selection. If there are
  # zero characters, the returned selection is empty.
  def prior : StringSeln
    prior, _ = prior_and_last
    prior
  end

  # Selects the *last* character if there is one or more characters. If there are
  # zero characters, the returned selection is empty.
  def last : StringSeln
    _, last = prior_and_last
    last
  end

  # Returns an empty selection pointing before the beginning of this one. Imagine
  # this as placing an "I-beam" before the first character in this selection (if any).
  def before_begin : StringSeln
    StringSeln.new(@trunk, @byte_start, @byte_start)
  end

  # Selects all characters in the trunk string before the start of this selection.
  def all_before_begin : StringSeln
    StringSeln.new(@trunk, 0u32, @byte_start)
  end

  # Returns an empty selection pointing after the end of this one. Imagine this
  # this as placing an "I-beam" after the last character in this selection (if any).
  def after_end : StringSeln
    StringSeln.new(@trunk, @byte_end, @byte_end)
  end

  # Selects all characters in the trunk string after the end of this selection.
  def all_after_end : StringSeln
    StringSeln.new(@trunk, @byte_end, @trunk.bytesize.to_u32)
  end

  # :nodoc:
  def chr : Char
    if @flags.ascii_char? # Fast path
      return startptr[0].unsafe_chr
    end

    assert 1u32 <= bytesize <= 4u32

    StringSeln.unsafe_chr(startptr, bytesize)
  end

  # Returns the first character in this selection. Raises `IndexError` if
  # this selection is empty.
  def first_char : Char
    first_char? || raise IndexError.new
  end

  # Returns the last character in this selection. Raises `IndexError` if
  # this selection is empty.
  def last_char : Char
    last_char? || raise IndexError.new
  end

  # Returns the first character in this selection. Returns `nil` if
  # this selection is empty.
  def first_char? : Char?
    empty? ? nil : first.chr
  end

  # Returns the last character in this selection. Returns `nil` if
  # this selection is empty.
  def last_char? : Char?
    empty? ? nil : last.chr
  end

  def each_char_seln(& : StringSeln ->) : Nil
    remainder = self

    until remainder.empty?
      first, rest = remainder.first_and_rest
      yield first

      remainder = rest
    end
  end

  def reverse_each_char_seln(& : StringSeln ->) : Nil
    remainder = self

    until remainder.empty?
      prior, last = remainder.prior_and_last
      yield last

      remainder = prior
    end
  end

  struct EE
    include Enumerable(Char)

    def initialize(@seln : StringSeln)
    end

    def each(& : Char ->)
      @seln.each_char { |chr| yield chr }
    end
  end

  def ee : Enumerable(Char)
    EE.new(self)
  end

  # Yields selected characters.
  def each_char(& : Char ->) : Nil
    each_char_seln { |seln| yield seln.chr }
  end

  # Yields selected characters along with their *absolute* byte index.
  def each_char_with_abs_byte_index(& : Char, Int32 ->) : Nil
    each_char_seln do |seln|
      yield seln.chr, seln.byte_start.to_i # ?!
    end
  end

  # Yields selected characters in reverse.
  def reverse_each_char(& : Char ->) : Nil
    reverse_each_char_seln { |seln| yield seln.chr }
  end

  def each_char_with_index(& : Char, Int32 ->) : Nil
    index = 0

    each_char_seln do |seln|
      yield seln.chr, index

      index += 1
    end
  end

  def each_before_and_after(& : StringSeln, StringSeln ->) : Nil
    each_char_seln do |seln|
      before = StringSeln.between(before_begin, seln.before_begin)
      after = StringSeln.between(seln.before_begin, after_end)
      yield before, after
    end

    yield self, after_end
  end

  def reverse_each_before_and_after(& : StringSeln, StringSeln ->) : Nil
    yield self, after_end

    reverse_each_char_seln do |seln|
      before = StringSeln.between(before_begin, seln.before_begin)
      after = StringSeln.between(seln.before_begin, after_end)
      yield before, after
    end
  end

  def each_partition(& : StringSeln, StringSeln, StringSeln ->) : Nil
    each_char_seln do |seln|
      l = StringSeln.between(before_begin, seln.before_begin)
      m = seln
      r = StringSeln.between(seln.after_end, after_end)
      yield l, m, r
    end

    yield self, after_end, after_end
  end

  def reverse_each_partition(& : StringSeln, StringSeln, StringSeln ->) : Nil
    yield self, after_end, after_end

    reverse_each_char_seln do |seln|
      l = StringSeln.between(before_begin, seln.before_begin)
      m = seln
      r = StringSeln.between(seln.after_end, after_end)
      yield l, m, r
    end
  end

  def each_split(& : StringSeln, StringSeln, StringSeln ->) : Nil
    each_partition do |l, m, r|
      return if m.empty? # last
      yield l, m, r
    end
  end

  def reverse_each_split(& : StringSeln, StringSeln, StringSeln ->) : Nil
    reverse_each_partition do |l, m, r|
      next if m.empty? # last
      yield l, m, r
    end
  end

  # Yields each word in this selection.
  #
  # This method never consumes any characters. All trailing and leading whitespaces
  # are kept (if any) -- attached either to the left- or the right-hand side word.
  def each_word(& : StringSeln ->) : Nil
    l, sep0, r = partition(' ')

    loop do
      yield l unless l.empty?

      break if sep0.empty?

      if r.empty?
        yield sep0
        break
      end

      succ, sep1, r1 = r.partition(' ')

      l = sep0 &+ succ
      sep0 = sep1
      r = r1
    end
  end

  # Yields each word in this selection along with its index.
  #
  # See also: `each_word`.
  def each_word_with_index(& : StringSeln, Int32 ->) : Nil
    index = 0

    each_word do |word|
      yield word, index

      index += 1
    end
  end

  def each_line(& : StringSeln ->)
    remainder = self

    until remainder.empty?
      l, m, remainder = remainder.partition('\n')
      yield l &+ m
    end
  end

  def each_line_with_index(& : StringSeln, Int32 ->)
    index = 0

    each_line do |line|
      yield line, index

      index += 1
    end
  end

  def partition(& : Char -> Bool) : {StringSeln, StringSeln, StringSeln}
    each_split do |l, m, r|
      chr = m.chr
      if yield chr
        return l, m, r
      end
    end

    {self, after_end, after_end}
  end

  def partition(pattern : Char) : {StringSeln, StringSeln, StringSeln}
    partition { |chr| chr === pattern }
  end

  def partition(index needle : Int) : {StringSeln, StringSeln, StringSeln}
    index = 0

    each_split do |l, m, r|
      if index == needle
        return l, m, r
      end

      index += m.bytesize
    end

    {self, after_end, after_end}
  end

  def rpartition(& : Char -> Bool) : {StringSeln, StringSeln, StringSeln}
    reverse_each_split do |l, m, r|
      chr = m.chr
      if yield chr
        return l, m, r
      end
    end

    {before_begin, before_begin, self}
  end

  def rpartition(pattern : Char) : {StringSeln, StringSeln, StringSeln}
    rpartition { |chr| chr === pattern }
  end

  def split(sep : Char, **kwargs) : Array(StringSeln)
    segments = [] of StringSeln

    split(sep, **kwargs) do |segment|
      segments << segment
    end

    segments
  end

  def split(sep : Char, **kwargs, &) : Nil
    split_and_rest(sep, **kwargs) do |segment, _|
      yield segment
    end
  end

  def split_and_rest(sep : Char, *, allow_empty : Bool = true, & : StringSeln, StringSeln ->) : Nil
    remainder = self

    loop do
      l, m, remainder = remainder.partition(sep)
      if l.nonempty? || allow_empty
        yield l, remainder
      end

      break if m.empty?
    end
  end

  def &+(other : StringSeln) : StringSeln
    StringSeln.chain(self, other)
  end

  # Returns a selection that excludes the first selected character.
  def lchop : StringSeln
    _, rest = first_and_rest
    rest
  end

  # Returns a selection that excludes the last selected character.
  def rchop : StringSeln
    prior, _ = prior_and_last
    prior
  end

  # Skips one leading character *chr* from this selection, if present.
  def lchop(chr : Char) : StringSeln
    lchop?(chr) || self
  end

  # Skips one trailing character *chr* from this selection, if present.
  def rchop(chr : Char) : StringSeln
    rchop?(chr) || self
  end

  # Skips one leading character *chr* from this selection, if present.
  # Returns `nil` if *chr* is absent.
  def lchop?(pattern : Char) : StringSeln?
    return if empty?

    first, rest = first_and_rest
    first.chr == pattern ? rest : nil
  end

  def lchop?(pattern : String) : StringSeln?
    return unless starts_with?(pattern)

    StringSeln.new(@trunk, @byte_start + pattern.bytesize, @byte_end)
  end

  def rchop?(chr : Char) : StringSeln?
    return if empty?

    prior, last = prior_and_last
    last.chr == chr ? prior : nil
  end

  def rchop?(pattern : String) : StringSeln?
    return unless ends_with?(pattern)

    StringSeln.new(@trunk, @byte_start, @byte_end - pattern.bytesize)
  end

  def chop?(l, r) : StringSeln?
    remainder = self
    return unless remainder = remainder.lchop?(l)
    return unless remainder = remainder.rchop?(r)

    remainder
  end

  def chomp : StringSeln
    if ends_with?('\r')
      return prior
    end

    if ends_with?('\n')
      prefix = prior
      if prefix.ends_with?('\r') # \r\n
        return prefix.prior
      end

      return prefix
    end

    self
  end

  def strip(charset : String = "\n") : StringSeln
    lstrip(charset).rstrip(charset)
  end

  def lstrip(charset : String = "\n") : StringSeln
    _, r = skip_thru(charset)
    r
  end

  def rstrip(charset : String = "\n") : StringSeln
    l, _ = rskip_thru(charset)
    l
  end

  def skip_thru_seq(*, limit : UInt32 = UInt32::MAX, & : Char -> T?) : {StringSeln, Array(T), StringSeln} forall T
    seq = [] of T
    remainder = self

    until remainder.empty? || limit.zero?
      first, rest = remainder.first_and_rest

      object = yield first.chr
      break if object.nil?

      seq << object
      remainder = rest
      limit -= 1
    end

    {StringSeln.between(before_begin, remainder), seq, remainder}
  end

  def skip_thru(charset : String) : {StringSeln, StringSeln}
    l, m, r = partition { |chr| !chr.in_set?(charset) }
    {l, m &+ r}
  end

  def rskip_thru(charset : String) : {StringSeln, StringSeln}
    l, m, r = rpartition { |chr| !chr.in_set?(charset) }
    {l &+ m, r}
  end

  def skip_to(charset : String) : {StringSeln, StringSeln}
    l, m, r = partition(&.in_set?(charset))
    {l, m &+ r}
  end

  def rskip_to(charset : String) : {StringSeln, StringSeln}
    l, m, r = rpartition(&.in_set?(charset))
    {l &+ m, r}
  end

  # Skips a number of characters from the left.
  def lskip(nchars : Int) : StringSeln
    remainder = self

    nchars.times do
      break if remainder.empty?

      remainder = remainder.rest
    end

    remainder
  end

  def ltake?(pattern) : StringSeln?
    return unless starts_with?(pattern)

    StringSeln.new(@trunk, @byte_start, @byte_start + pattern.bytesize)
  end

  def find(pattern) : StringSeln
    each_before_and_after do |before, after|
      if segment = after.ltake?(pattern)
        return segment
      end
    end

    raise ArgumentError.new("not found")
  end

  def extend(*, exclusive = true, & : Char -> Bool) : StringSeln
    l, m, _ = all_after_end.partition { |chr| !(yield chr) }
    unless exclusive
      l &+= m
    end

    StringSeln.between(before_begin, l.after_end)
  end

  def reverse_extend(*, exclusive = true, & : Char -> Bool) : StringSeln
    _, m, r = all_before_begin.rpartition { |chr| !(yield chr) }
    unless exclusive
      r = m &+ r
    end

    StringSeln.between(r.before_begin, after_end)
  end

  def span(other : StringSeln) : StringSeln
    StringSeln.span(self, other)
  end

  def upto(other : StringSeln) : StringSeln
    StringSeln.extend(self, other)
  end

  # A hand-optimized alternative to `ee.count`. This is about a hundred times faster
  # than `ee.count` on my machine for basic tasks such as counting newlines, due to
  # vectorization-friendliness in the single-byte fast path.
  def count(pattern : Char) : Int32
    buffer = uninitialized UInt8[4]
    buffer_size = 0

    pattern.each_byte do |byte|
      buffer[buffer_size] = byte
      buffer_size += 1
    end

    ptr = startptr
    ptrsize = bytesize
    count = 0u32

    # Fast path for single-byte chars. This can be auto-vectorized with --mcpu=native.
    if buffer_size == 1
      needle = buffer.unsafe_fetch(0)

      ptrsize.times do |index|
        if ptr[index] == needle
          # Worst case is count = bytesize. Bytesize is proven to be UInt32.
          # So an overflow is impossible.
          count &+= 1
        end
      end

      return count.to_i # ?!
    end

    until ptrsize.zero?
      match = (0...buffer_size).all? do |index|
        ptr[index] == buffer.unsafe_fetch(index)
      end

      if match
        # Worst case is count = bytesize. Bytesize is proven to be UInt32.
        # So an overflow is impossible.
        count &+= 1
      end

      ptr += 1
      ptrsize &-= 1
    end

    count.to_i # ?!
  end

  # Returns the underlying bytes. The returned slice is read-only.
  # It is guaranteed to contain valid UTF-8 encoded bytes. It is
  # guaranteed to start and end at a UTF-8 character boundary.
  def to_slice : Bytes
    Bytes.new(startptr, bytesize, read_only: true)
  end

  def clone : StringSeln
    self
  end

  def highlight(io, marker : String = "⏏")
    b = char_start

    if bytesize.zero?
      io << @trunk.insert(b, marker)
      return
    end

    e = char_end

    io << @trunk.insert(e, marker).insert(b, marker)
  end

  def inspect(io)
    io << "…\""

    each_char do |chr|
      case chr
      when '"'  then io << "\\\""
      when '\\' then io << "\\\\"
      when '\a' then io << "\\a"
      when '\b' then io << "\\b"
      when '\e' then io << "\\e"
      when '\f' then io << "\\f"
      when '\n' then io << "\\n"
      when '\r' then io << "\\r"
      when '\t' then io << "\\t"
      when '\v' then io << "\\v"
      when '\0' then io << "\\0"
      else
        if chr.printable?
          io << chr
        else
          chr.unicode_escape(io)
        end
      end
    end

    io << "\"…"
  end

  def to_s(io)
    io.write(to_slice)
  end

  def to_s : String
    if covers_fully?
      return @trunk
    end

    @trunk.byte_slice(@byte_start, bytesize)
  end

  def ==(other : StringSeln) : Bool
    # Fast path.
    if @trunk.same?(other.@trunk) && {byte_start, byte_end} == {other.byte_start, other.byte_end}
      return true
    end

    to_slice == other.to_slice
  end

  def ==(other : String) : Bool
    to_slice == other.to_slice
  end

  def ==(other : Char) : Bool
    bytesize == other.bytesize && starts_with?(other)
  end

  def_hash to_slice
end

alias StringView = Pf::StringSeln

class String
  def view : StringView
    StringView.new(self)
  end

  def view? : StringView?
    StringView.new?(self)
  end

  # Yields string views corresponding to each line in this string. Byte slices
  # will include *trailing* newlines (i.e. this method does not "take away" any
  # characters from the string).
  #
  # If this string is empty, does not yield anything.
  def each_line_view(& : StringView ->) : Nil
    view.each_line { |v| yield v }
  end

  def ===(other : StringView) : Bool
    # Fast path
    unless bytesize == other.bytesize
      return false
    end

    to_slice == other.to_slice
  end
end

class ::Pf::Kit::HybridArray(T, N)
  def to_unsafe_readonly_buffer_or_spill_slice! : Slice(T)
    if @spillsize.zero?
      return Slice(T).new(@buffer, @bufsize, read_only: true)
    end

    to_unsafe_readonly_slice!
  end
end
