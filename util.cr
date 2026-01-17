macro on_demand(typedecl)
  getter({{typedecl}}) { {{typedecl.type}}.new }

  protected def has_{{typedecl.var.id}}? : Bool
    return false unless %collection = @{{typedecl.var.id}}
    return true unless {{ typedecl.type.resolve.has_method?(:empty?) }}
    !%collection.empty?
  end
end

macro defrecord(name, *properties, includes = [] of ::NoReturn)
  struct {{name.id}}
    {% for dep in includes %}
      include {{dep}}
    {% end %}

    {% for property in properties %}
      {% if property.is_a?(Assign) %}
        getter {{property.target.id}}
      {% elsif property.is_a?(TypeDeclaration) %}
        getter {{property}}
      {% else %}
        getter :{{property.id}}
      {% end %}
    {% end %}

    def initialize({{ properties.map { |field| "@#{field.id}".id }.splat }})
      {{yield}}
    end
  end
end

annotation DefcaseField
end

macro defcase(cls, *typedecls, inherit = false, equality = :value, caches_hash = false, copying = true, mutation = false, &)
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
    names = typedecls.map do |typedecl|
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

    def initialize({{typedecls.map { |typedecl| "@#{typedecl}".id }.splat}})
    end

    {% if copying %}
      def copy_with({{
                      typedecls.map do |property|
                        if property.is_a?(Assign)
                          "#{property.target.id} _#{property.target.id} = @#{property.target.id}".id
                        elsif property.is_a?(TypeDeclaration)
                          "#{property.var.id} _#{property.var.id} = @#{property.var.id}".id
                        else
                          "#{property.id} _#{property.id} = @#{property.id}".id
                        end
                      end.splat
                    }})
        self.class.new({{
                         typedecls.map do |property|
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
      @hash : UInt64?

      def hash(hasher)
        h64 = @hash ||= previous_def(Crystal::Hasher.new).result
        h64.hash(hasher)
      end
    {% end %}

    {{yield}}
  end
end

macro subclass(*args, &block)
  defcase({{args.splat}}, inherit: true) {{ block }}
end

class ::UnreachableException < Exception
end

macro unreachable(detail = "unreachable")
  raise ::UnreachableException.new({{detail}})
end

macro unimplemented
  {% verbatim do %}
    raise ::NotImplementedError.new("subclass must implement #{{{@type.name.stringify}}}#{ {{@type.class? ? "." : "#"}} }#{{{@def.name.stringify}}}")
  {% end %}
end

def pass(*args, &)
  yield *args
end

struct MutView(T)
  include Indexable::Mutable(T)

  def initialize(@operand : Array(T), @begin = 0, @end = operand.size)
  end

  def slice(index : Int)
    {MutView.new(@operand, @begin, @begin + index), MutView.new(@operand, @begin + index, @end)}
  end

  def size
    @end - @begin
  end

  def rest
    MutView.new(@operand, @begin + 1, @end)
  end

  def unsafe_fetch(index : Int)
    @operand.unsafe_fetch(@begin + index)
  end

  def unsafe_put(index : Int, value : T)
    @operand.unsafe_put(@begin + index, value)
  end

  def inspect(io)
    io << "MutView{"
    join(io, ", ") do |value|
      io << value
    end
    io << "}"
  end
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

  def fuse(other, & : MutView(T) ->)
    yield MutView(T).new(concat(other), size - other.size, size)

    self
  end

  def concat(other : Indexable(U), & : U -> T) forall U
    resize_if_cant_insert(other.size)

    other.each do |el|
      @buffer[@size] = yield el
      @size += 1
    end
  end

  def view
    MutView.new(self)
  end
end

# Defines a `change` method which functions like `#copy_with` for records.
macro def_change
  {% verbatim do %}
    def change(**kwargs) : self
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

# Defines a `change` method which functions like `#copy_with` for records. The
# difference from `def_change` is that for all instance variables an equality check
# is performed (`Reference#same?` for reference-typed and `Value#==` for value-
# typed objects). If all instances variables remain unchanged, `self` is returned
# instead of making a useless copy (as `def_change` would have done).
macro def_change_eq
  {% verbatim do %}
    def change(**kwargs) : self
      {% begin %}
        %unchanged = true

        {% for var in @type.instance_vars %}
          {{var.id}} = kwargs.fetch({{var.symbolize}}, @{{var.id}}).as({{var.type}})
          {% if var.type.has_method?(:same?) %}
            %unchanged &&= {{var.id}}.same?(@{{var.id}})
          {% else %}
            %unchanged &&= {{var.id}} == @{{var.id}}
          {% end %}
        {% end %}

        return self if %unchanged

        {{@type}}.new({{ @type.instance_vars.map { |var| "#{var.id}: #{var.id}".id }.splat }})
      {% end %}
    end
  {% end %}
end

struct Set(T)
  def take? : T?
    return unless object = first?
    delete(object)
    object
  end

  def shift : T
    object = first
    delete(object)
    object
  end

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
  def view : MutView(T)
    to_a.view
  end

  def to_compact_set(& : T -> U?) : Set(U) forall U
    set = Set(U).new
    each do |object0|
      next unless object1 = yield object0
      set << object1
    end
    set
  end

  # FIXME: Maybe more idiomatically: first_of? / last_of?

  def leftmost?(& : T -> U?) : U? forall U
    each do |object0|
      next unless object1 = yield object0
      return object1
    end
  end

  def leftmost?(cls : T.class) : T?
    each do |object|
      return object if object.is_a?(T)
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

class DeepSet(T)
  include Enumerable(T)

  # Returns the amount of elements in this deep set.
  getter size

  def initialize
    @sets = [] of Set(T)
    @size = 0
  end

  def includes?(el : T)
    @sets.any? &.includes?(el)
  end

  def each(& : T ->)
    @sets.each do |set|
      set.each { |el| yield el }
    end
  end

  def concat(set : Set(T))
    @size += set.size
    @sets << set
    self
  end

  def inspect(io)
    io << "DeepSet{"
    join(io, ", ")
    io << "}"
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
        {% for key, cls in T %}
          {{key.id}}: (yield {{key.symbolize}}, self[{{key.symbolize}}]).as({{cls.instance}}),
        {% end %}
  }.as({
      {% for key, cls in T %}
        {{key.id}}: {{cls.instance}},
      {% end %}
  })
    {% end %}
  end
end

def minfirst(*, a, b, & : Symbol ->)
  if a.size < b.size
    yield :a
    yield :b
  else
    yield :b
    yield :a
  end
end

module Enumerable(T)
  def select(*types : *U) forall U
    {% begin %}
      {% for cls in U %}
        {{ cls.id.downcase.gsub(/[^\w]/, "_") }} = [] of {{cls.instance}}
      {% end %}

      each do |object|
        case object
        {% for cls in U %}
        when {{cls.instance}}
          {{cls.id.downcase.gsub(/[^\w]/, "_")}} << object
        {% end %}
        end
      end

      { {% for cls in U %}
          {{ cls.id.downcase.gsub(/[^\w]/, "_") }},
        {% end %} }
    {% end %}
  end

  def partition(*types : *U) forall U
    {% begin %}
      {% for cls, i in U %}
        %tmp{i} = [] of {{cls.instance}}
      {% end %}

      %rest = [] of T

      each do |object|
        case object
        {% for cls, i in U %}
        when {{cls.instance}}
          %tmp{i} << object
        {% end %}
        else
          %rest << object
        end
      end

      { {% for cls, i in U %} %tmp{i}, {% end %} %rest }
    {% end %}
  end
end

# Todo: remove these vvv I don't use them anymore

# :nodoc:
#
# Sometimes Crystal goes weird and `embed` stops working because
# function calls get wrapped in `()`s, for whatever reason. So we
# have to use this workaround to get the call to work.
def port_surround(x, &)
  with x yield
end

macro port_embed(outer, branches)
  %source = {{outer}}
  {% for accept, pipeline in branches %}
    {% if pipeline.is_a?(Call) && pipeline.name == :redirect %}
      {% dest = pipeline.args[0] %}
      {% pipeline = pipeline.args[1] %}
      %outbound = port_surround(%source.select({{accept}})) { {{pipeline}} }
      if %outbound.responds_to?(:into)
        %outbound.into({{dest}})
      end
    {% else %}
      %outbound = port_surround(%source.select({{accept}})) { {{pipeline}} }
      if %outbound.responds_to?(:into)
        %outbound.into(%source)
      end
    {% end %}
  {% end %}
end

# ^^^

abstract struct Int
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

  def each_digit(base = 10, &) : Nil
    if base < 2
      raise ArgumentError.new("Invalid base #{base}")
    end

    if self < 0
      raise ArgumentError.new("Can't request digits of negative number")
    end

    if self == 0
      yield 0
      return
    end

    n = self

    until n == 0
      yield n % base

      n //= base
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

  def self.byte_size
    bit_size//8
  end

  def byte_size
    self.class.byte_size
  end

  def nonzero?
    !zero?
  end

  # Iteration order: LSB to MSB.
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

  def lsb_set_index
    trailing_zeros_count
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

class Object
  # Doesn't yield to the block.
  def orelse(& : ->) : self
    self
  end

  def default(object : T) : self forall T
    self
  end
end

struct Nil
  # Yields to the block.
  def orelse(& : -> T) : T forall T
    yield
  end

  def default(object : T) : T forall T
    object
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

class IO::ByteStream < IO
  def initialize(&@fn : Bytes ->)
  end

  def read(slice : Bytes)
    0
  end

  def write(slice : Bytes) : Nil
    @fn.call(slice)
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


module TextWrap
  extend self

  private def scan(text : String, maxw : Int, maxh : Int, &) : Nil
    w = 0
    h = 1
    wsidx = wsx = nil

    text.view.each_line do |line|
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

  def wrap(io : IO, text : String, maxw : Int, maxh : Int, *, ellipsis : String = "…") : Nil
    cursor = 0
    truncated = false

    scan(text, maxw, maxh) do |action, index|
      prefix = text.view(byte_start: cursor, byte_end: index)
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

    remaining = text.view(byte_start: cursor, byte_end: text.bytesize)
    io << remaining
  end

  def wrap(text : String, maxw : Int, maxh : Int, **kwargs)
    String.build((text.bytesize * 1.33).to_i) do |io|
      wrap(io, text, maxw, maxh, **kwargs)
    end
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

  def to_readonly_slice(& : {K, V} -> T) : Slice(T) forall T
    ptr = Pointer(T).malloc(size)
    each_with_index do |(key, value), index|
      ptr[index] = yield({key, value})
    end
    Slice.new(ptr, size, read_only: true)
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

  # Reference: https://github.com/rakudo/rakudo/blob/6b47541e27a4a0bcc9bbc07cdbf944b7174cc01c/src/Raku/ast/regex.rakumod#L715
  def hspace? : Bool
    ord.in?(0x09, 0x20, 0xa0, 0x1680, 0x180e, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008, 0x2009, 0x200a, 0x202f, 0x205f, 0x3000)
  end

  # Reference: https://github.com/rakudo/rakudo/blob/6b47541e27a4a0bcc9bbc07cdbf944b7174cc01c/src/Raku/ast/regex.rakumod#L793
  def vspace? : Bool
    ord.in?(0x0a, 0x0b, 0x0c, 0x0d, 0x85, 0x2028, 0x2029)
  end
end

# FIXME: I use `ascii_only?` but really I meant `single_byte_optimizable?`. Rename
# and fix conditions!!!
#
# TODO: StringView should be implemented properly and moved to Permafrost. We can
# copy some of Char::Reader's methods for .first, .rest, .prior, .last, and maybe &+
# (aka join consecutive); everything else can be built on top of them.
struct StringView
  # WARNING: This will return the original string into which the view
  # is pointing! You probably want `to_s`.
  getter string : String
  getter byte_start : Int32

  @byte_tail : UInt32

  def byte_end : Int32
    (@byte_tail >> 1).to_i
  end

  # Returns `true` if this string view only contains ASCII characters.
  def ascii_only? : Bool
    @byte_tail & 0b1 == 1
  end

  def initialize(@string, @byte_start, byte_end : Int32, ascii_only : Bool)
    unless 0 <= @byte_start <= byte_end <= @string.bytesize
      raise ArgumentError.new("invalid byte range #{@byte_start}...#{byte_end}")
    end

    # Byte end is nonnegative, we know that now!

    @byte_tail = (byte_end.to_u32 << 1) | (ascii_only ? 1u32 : 0u32)

    {% if flag?(:view_check_valid) %}
      unless Unicode.valid?(@string.to_slice[@byte_start...byte_end])
        raise ArgumentError.new("invalid encoding")
      end
    {% end %}
  end

  def <=>(other : StringView) : Int32
    if string.same?(other.string) && {byte_start, byte_end} == {other.byte_start, other.byte_end}
      return 0
    end

    (to_slice <=> other.to_slice).sign
  end

  def self.join(views : Enumerable(StringView)) : StringView
    views.reduce? { |memo, view| memo + view } || "".view
  end

  def self.join(*views : StringView)
    join(*views)
  end

  def self.between(a : StringView, b : StringView) : StringView
    unless a.string.same?(b.string)
      raise ArgumentError.new("cannot take a view between views whose underlying strings compare different by reference")
    end

    # Sort by endpoints
    if a.byte_end > b.byte_end
      a, b = b, a
    end

    assert a.byte_end <= b.byte_start

    StringView.new(a.string, a.byte_end, b.byte_start, ascii_only: a.string.single_byte_optimizable?)
  end

  def self.difference(a : StringView, b : StringView) : StringView
    unless a.string.same?(b.string)
      raise ArgumentError.new("cannot take a difference of views whose underlying strings compare different by reference")
    end

    StringView.new(a.string, a.byte_start, Math.min(a.byte_end, b.byte_start), ascii_only: a.string.single_byte_optimizable?)
  end

  def self.intersection(a : StringView, b : StringView) : StringView
    unless a.string.same?(b.string)
      raise ArgumentError.new("cannot intersect string views whose underlying strings compare different by reference")
    end

    xb = Math.max(a.byte_start, b.byte_start)
    xe = Math.min(a.byte_end, b.byte_end)

    StringView.new(a.string, xb, xe, ascii_only: a.ascii_only?)
  end

  def self.intersection(views : Enumerable(StringView)) : StringView
    result = views.reduce? do |memo, view|
      StringView.intersection(memo, view)
    end
    result || "".view
  end

  def self.intersection(*views : StringView) : StringView
    intersection(views)
  end

  def self.cat(*args) : StringView
    content = String.build(args.sum(&.bytesize)) do |io|
      args.each do |arg|
        io << arg
      end
    end

    content.view
  end

  def before_begin : StringView
    StringView.new(@string, @byte_start, @byte_start, ascii_only: true)
  end

  def after_end : StringView
    StringView.new(@string, byte_end, byte_end, ascii_only: true)
  end

  def char_start : Int32
    if @byte_start == @string.bytesize
      return @string.size
    end

    @string.byte_index_to_char_index(@byte_start).not_nil!
  end

  def char_end : Int32
    if byte_end == @string.bytesize
      return @string.size
    end

    @string.byte_index_to_char_index(byte_end).not_nil!
  end

  def includes?(needle : Char) : Bool
    each_char do |char|
      return true if char == needle
    end
    false
  end

  def rincludes?(needle : Char) : Bool
    reverse_each_char do |char|
      return true if char == needle
    end
    false
  end

  def empty? : Bool
    @byte_start == byte_end
  end

  def nonempty? : Bool
    !empty?
  end

  def bytesize : Int32
    byte_end - @byte_start
  end

  def size : Int32
    if ascii_only?
      bytesize
    elsif covers_fully?
      # String always knows better than we do how to do this efficiently!
      @string.size
    else
      size = 0
      each_char do
        size += 1
      end
      size
    end
  end

  def byte_bounds : Range(Int32, Int32)
    @byte_start...byte_end
  end

  def blank? : Bool
    each_char do |char|
      return false unless char.whitespace?
    end

    true
  end

  def prefixed_by?(object) : Bool
    bytesize > object.bytesize && starts_with?(object)
  end

  def postfixed_by?(object) : Bool
    bytesize > object.bytesize && ends_with?(object)
  end

  def surrounded_by?(l, r) : Bool
    prefixed_by?(l) && postfixed_by?(r)
  end

  def starts_with?(range : Range(Char, Char)) : Bool
    return false unless fst = first_char?

    fst.in?(range)
  end

  def starts_with?(*chars : Char) : Bool
    chars.any? { |char| starts_with?(char) }
  end

  def starts_with?(ch : Char) : Bool
    return false if bytesize < ch.bytesize

    offset = @byte_start

    ch.each_byte do |byte|
      unless @string.byte_at(offset) == byte
        return false
      end

      offset += 1
    end

    true
  end

  def starts_with?(prefix : String | StringView) : Bool
    return false if bytesize < prefix.bytesize

    to_unsafe.memcmp(prefix.to_unsafe, prefix.bytesize) == 0
  end

  def ends_with?(ch : Char)
    last_char? == ch
  end

  def ends_with?(postfix : String) : Bool
    return false if postfix.bytesize > bytesize

    (to_unsafe + bytesize - postfix.bytesize).memcmp(postfix.to_unsafe, postfix.bytesize) == 0
  end

  def precedes?(other : StringView)
    @string.same?(other.string) && byte_end == other.byte_start
  end

  def covers_fully? : Bool
    {0, @string.bytesize} == {@byte_start, byte_end}
  end

  def blank? : Bool
    each_char do |char|
      return false unless char.whitespace?
    end

    true
  end

  def +(other : StringView) : StringView
    if empty?
      return other
    end

    ascii_only = ascii_only? && other.ascii_only?

    if precedes?(other)
      return StringView.new(@string, @byte_start, other.byte_end, ascii_only)
    end

    sum = String.build do |io|
      io << self
      io << other
    end

    StringView.new(sum, 0, sum.bytesize, ascii_only)
  end

  def &+(other : StringView)
    unless precedes?(other)
      raise ArgumentError.new("lhs string view does not precede rhs")
    end

    self + other
  end

  struct EE
    include Enumerable(Char)

    def initialize(@v : StringView)
    end

    def each(& : Char ->)
      @v.each_char { |ch| yield ch }
    end
  end

  def ee : Enumerable(Char)
    EE.new(self)
  end

  def -(other : StringView) : StringView
    unless @string.same?(other.@string)
      raise ArgumentError.new("cannot use `-` on string views pointing to different strings")
    end

    StringView.new(@string, other.@byte_start, @byte_start, ascii_only?)
  end

  def []?(byte_range : Range)
    return unless response = Indexable.range_to_index_and_count(byte_range, bytesize)

    byte_offset, byte_count = response

    StringView.new(@string, @byte_start + byte_offset, @byte_start + byte_offset + byte_count, ascii_only?)
  end

  def []?(index : Int32) : Char?
    if index == 0
      return first_char?
    end

    if index == -1
      return last_char?
    end

    return unless offset = @string.byte_index_to_char_index(@byte_start)

    @string[offset + index]?
  end

  def [](object)
    self[object]? || raise IndexError.new
  end

  # NOTE: this is probably not what you want. You probably want `lskip`
  # or `rskip`.
  def skip(nbytes : Int32) : StringView
    StringView.new(@string, @byte_start + nbytes, byte_end, ascii_only?)
  end

  def skip(nest : String? = nil, unnest : String? = nil, & : Char -> Bool) : StringView
    if nest && nest == unnest
      raise ArgumentError.new("nest and unnest arguments must not be the same")
    end

    remainder = self
    nesting = 1

    until nesting.zero? || remainder.empty?
      if nest && remainder.starts_with?(nest)
        nesting += 1
        remainder = remainder.skip(nest.bytesize)
      elsif unnest && remainder.starts_with?(unnest)
        nesting -= 1
        break if nesting.zero?
        remainder = remainder.skip(unnest.bytesize)
      else
        char = remainder.first_char
        break unless yield char
        remainder = remainder.skip(char.bytesize)
      end
    end

    remainder
  end

  def prev_char? : Char?
    if ascii_only?
      return unless @byte_start > 0
      return @string.to_slice[@byte_start - 1].chr
    end

    reader = Char::Reader.new(@string, pos: @byte_start)
    return unless reader.has_previous?

    reader.previous_char
  end

  @[AlwaysInline]
  private def first_char_ascii? : Char?
    empty? ? nil : to_unsafe[0].unsafe_chr
  end

  private def first_char_unicode? : Char?
    if covers_fully?
      return @string[0]?
    end

    each_char { |char| return char }
  end

  def first_char? : Char?
    ascii_only? ? first_char_ascii? : first_char_unicode?
  end

  def first_char : Char
    first_char? || raise IndexError.new
  end

  def last_char? : Char?
    if ascii_only?
      return unless codepoint = to_slice.last?
      return codepoint.unsafe_chr
    end

    if covers_fully?
      return @string[@string.size - 1]?
    end

    reverse_each_char { |char| return char }
  end

  def last_char : Char
    last_char? || raise IndexError.new
  end

  def first_char_and_rest : {Char, StringView}
    fst = first_char

    {fst, StringView.new(@string, byte_start + fst.bytesize, byte_end, ascii_only: ascii_only?)}
  end

  def first? : StringView?
    return if empty?

    if ascii_only?
      return StringView.new(@string, @byte_start, @byte_start + 1, ascii_only: true)
    end

    each_char do |char|
      return StringView.new(@string, @byte_start, @byte_start + char.bytesize, ascii_only: false)
    end

    unreachable
  end

  def rest? : StringView?
    return if empty?

    if ascii_only?
      return StringView.new(@string, @byte_start + 1, byte_end, ascii_only: true)
    end

    each_char do |char|
      return StringView.new(@string, @byte_start + char.bytesize, byte_end, ascii_only: false)
    end

    unreachable
  end

  def first : StringView
    first? || raise IndexError.new
  end

  def first_or_empty : StringView
    first? || before_begin
  end

  def rest : StringView
    rest? || raise IndexError.new
  end

  def rest_or_empty : StringView
    rest? || after_end
  end

  def last? : StringView?
    return if empty?

    if ascii_only?
      return StringView.new(@string, byte_end - 1, byte_end, ascii_only: true)
    end

    reverse_each_char do |char|
      return StringView.new(@string, byte_end - char.bytesize, byte_end, ascii_only: false)
    end

    unreachable
  end

  def last_or_empty : StringView
    last? || after_end
  end

  def prior? : StringView?
    return if empty?

    if ascii_only?
      return StringView.new(@string, @byte_start, byte_end - 1, ascii_only: true)
    end

    reverse_each_char do |char|
      return StringView.new(@string, @byte_start, byte_end - char.bytesize, ascii_only: false)
    end

    unreachable
  end

  def prior_or_empty : StringView
    prior? || before_begin
  end

  def last : StringView
    last? || raise IndexError.new
  end

  def prior : StringView
    prior? || raise IndexError.new
  end

  def prior_string : StringView
    StringView.new(@string, 0, @byte_start, @string.single_byte_optimizable?)
  end

  def posterior_string : StringView
    StringView.new(@string, byte_end, @string.bytesize, @string.single_byte_optimizable?)
  end

  def count(&)
    count = 0
    each_char do |chr|
      next unless yield chr
      count += 1
    end
    count
  end

  def count(pattern)
    count { |chr| pattern === chr }
  end

  def lcount(prefix : Char) : Int32
    count = 0

    each_char do |char|
      break unless prefix == char

      count += 1
    end

    count
  end

  def lskip(nchars : Int32, charset = nil) : StringView
    reader = Char::Reader.new(@string, pos: @byte_start)
    reader.each do |char|
      break if reader.pos >= byte_end

      if nchars.zero? || (charset && !char.in?(charset))
        return StringView.new(@string, reader.pos, byte_end, ascii_only?)
      end

      nchars -= 1
    end

    after_end
  end

  def rskip(nchars : Int32, charset = nil) : StringView
    return self if empty?

    reader = Char::Reader.new(@string, pos: byte_end)
    single_bytes = true

    while reader.pos > @byte_start
      if nchars.zero?
        return StringView.new(@string, byte_start, reader.pos, single_bytes)
      end

      reader.previous_char
      char = reader.current_char
      if charset && !char.in?(charset)
        return StringView.new(@string, byte_start, reader.pos + reader.current_char_width, single_bytes)
      end

      nchars -= 1
      single_bytes &&= reader.current_char_width == 1
    end

    before_begin
  end

  def rchop
    rskip(1)
  end

  # Removes one leading character *ch* from this string view, if present.
  def lchop(ch : Char) : StringView
    starts_with?(ch) ? skip(ch.bytesize) : self
  end

  def lchop?(ch : Char) : StringView?
    starts_with?(ch) ? skip(ch.bytesize) : nil
  end

  def subview(char_start : Int32, char_end : Int32) : StringView
    if ascii_only?
      return byte_subview(char_start, char_end)
    end

    start = self.char_start
    b = @string.char_index_to_byte_index(start + char_start) || raise IndexError.new
    e = @string.char_index_to_byte_index(start + char_end) || raise IndexError.new

    StringView.new(@string, b, e, ascii_only: ascii_only?)
  end

  def subview(range : Range) : StringView
    b = range.begin || 0
    e = range.end || size

    subview(b, e)
  end

  def byte_subview(byte_start_rel : Int32, byte_end_rel : Int32)
    StringView.new(@string, byte_start + byte_start_rel, byte_start + byte_end_rel, ascii_only: ascii_only?)
  end

  def byte_subview(byte_start_rel, size byte_size : Int32)
    byte_subview(byte_start_rel, byte_start_rel + byte_size)
  end

  def byte_subview(byte_start_rel, size char : Char)
    byte_subview(byte_start_rel, size: char.bytesize)
  end

  def upto(other : StringView)
    unless @string.same?(other.@string)
      raise ArgumentError.new("views point to different strings")
    end

    StringView.new(@string, byte_start, other.byte_start, ascii_only: ascii_only?)
  end

  def chomp : StringView
    if ends_with?('\r')
      rskip(1)
    elsif ends_with?('\n')
      prefix = rskip(1)
      if prefix.ends_with?('\r') # \r\n
        return prefix.rskip(1)
      end

      prefix
    else
      self
    end
  end

  def lstrip(charset = "\n") : StringView
    reader = Char::Reader.new(@string, pos: @byte_start)
    reader.each do |char|
      break if reader.pos >= byte_end

      unless charset.includes?(char)
        return StringView.new(@string, reader.pos, byte_end, ascii_only?)
      end
    end

    after_end
  end

  def rstrip(charset = "\n") : StringView
    reader = Char::Reader.new(@string, pos: byte_end)

    until reader.pos == @byte_start
      reader.previous_char

      unless charset.includes?(reader.current_char)
        reader.next_char

        return StringView.new(@string, @byte_start, reader.pos, ascii_only?)
      end
    end

    before_begin
  end

  def strip(charset = "\n")
    lstrip(charset).rstrip(charset)
  end

  # Splits before *index* (i.e. *index*-th character is included in the right-
  # hand side).
  def split(index : Int32) : {StringView, StringView}
    l, mid, r = partition(index)
    {l, mid + r}
  end

  def partition(index : Int32) : {StringView, StringView, StringView}
    reader = Char::Reader.new(@string, pos: @byte_start)

    lhs_ascii_only = true

    until reader.pos == byte_end
      char = reader.current_char
      char_ascii = char.ascii?

      if index.zero?
        return {StringView.new(@string, @byte_start, reader.pos, lhs_ascii_only),
                StringView.new(@string, reader.pos, reader.pos + char.bytesize, char_ascii),
                StringView.new(@string, reader.pos + char.bytesize, byte_end, ascii_only?)}
      end

      index -= 1
      lhs_ascii_only &&= char_ascii
      reader.next_char
    end

    {self, after_end, after_end}
  end

  def partition(& : Char -> Bool) : {StringView, StringView, StringView}
    reader = Char::Reader.new(@string, pos: @byte_start)

    lhs_ascii_only = true

    until reader.pos == byte_end
      char = reader.current_char
      char_ascii = char.ascii?

      if yield char
        l = StringView.new(@string, @byte_start, reader.pos, lhs_ascii_only)
        mid = StringView.new(@string, reader.pos, reader.pos + char.bytesize, char_ascii)
        r = StringView.new(@string, reader.pos + char.bytesize, byte_end, ascii_only?)
        return l, mid, r
      end

      lhs_ascii_only &&= char_ascii

      reader.next_char
    end

    {self, after_end, after_end}
  end

  def partition(separator : Char)
    partition { |ch| ch == separator }
  end

  def rpartition(*, limit : Int = Int32::MAX, & : Char -> Bool)
    reader = Char::Reader.new(@string, pos: byte_end)

    single_bytes = true

    loop do
      # Not found
      if limit.zero? || reader.pos == byte_start
        return before_begin, before_begin, self
      end

      reader.previous_char
      if yield reader.current_char
        l = StringView.new(@string, @byte_start, reader.pos, ascii_only?)
        m = StringView.new(@string, reader.pos, reader.pos + reader.current_char_width, reader.current_char_width == 1)
        r = StringView.new(@string, reader.pos + reader.current_char_width, byte_end, single_bytes)
        return l, m, r
      end

      single_bytes &&= reader.current_char_width == 1
      limit -= 1
    end
  end

  def rpartition(separator : Char, **kwargs)
    rpartition { |ch| ch == separator }
  end

  def split(separator : Char)
    segments = [] of StringView
    split(separator) do |segment|
      segments << segment
    end
    segments
  end

  def split(separator : Char, &)
    split_and_rest(separator) { |segment, _, _| yield segment }
  end

  def split_and_rest(separator : Char, &)
    lhs = self

    until lhs.empty?
      lhs, sep, rest = lhs.partition(separator)
      yield lhs, sep, rest
      lhs = rest
    end
  end

  def each_byte(& : UInt8 ->) : Nil
    to_slice.each { |byte| yield byte }
  end

  def each_char_view(& : StringView ->) : Nil
    reader = Char::Reader.new(@string, pos: @byte_start)

    until reader.pos == byte_end
      yield StringView.new(@string, byte_start: reader.pos, byte_end: reader.pos + reader.current_char_width, ascii_only: reader.current_char.single_byte?)
      reader.next_char
    end
  end

  # Yields each character in this string view, going from left to right.
  def each_char(& : Char ->) : Nil
    reader = Char::Reader.new(@string, pos: @byte_start)

    until reader.pos == byte_end
      yield reader.current_char
      reader.next_char
    end
  end

  # Yields each character in this string view, going from right to left.
  def reverse_each_char(& : Char ->) : Nil
    reverse_each_char_with_abs_byte_index do |chr, _|
      yield chr
    end
  end

  def reverse_each_char_with_abs_byte_index(& : Char, Int32 ->) : Nil
    reader = Char::Reader.new(@string, pos: byte_end)

    until reader.pos <= @byte_start
      reader.previous_char # byte end must be skipped, we're exclusive!
      yield reader.current_char, reader.pos
    end
  end

  # Yields each character in this string view along with its view-local index.
  def each_char_with_index(& : Char, Int32 ->) : Nil
    index = 0

    each_char do |char|
      yield char, index

      index += 1
    end
  end

  # Yields each character in this string view along with its byte index
  # within this view's parent string.
  def each_char_with_abs_byte_index(*, eoi = false, & : Char, Int32 ->) : Nil
    byte_index = @byte_start

    each_char do |char|
      yield char, byte_index

      byte_index += char.bytesize
    end

    if eoi
      yield '\0', byte_index
    end
  end

  def each_char_with_rel_byte_index(& : Char, Int32 ->) : Nil
    byte_index = 0

    each_char do |char|
      yield char, byte_index

      byte_index += char.bytesize
    end
  end

  # Yields each word in this view.
  #
  # This method never consumes any characters. All trailing and leading whitespaces
  # are kept (if any) -- attached either to the left- or the right-hand side word.
  def each_word(& : StringView ->) : Nil
    l, sep0, r = partition(' ')

    loop do
      yield l unless l.empty?

      break if sep0.empty?

      if r.empty?
        yield sep0
        break
      end

      succ, sep1, r1 = r.partition(' ')

      l = sep0 + succ
      sep0 = sep1
      r = r1
    end
  end

  # Yields each word in this view along with its index.
  #
  # See also: `each_word`.
  def each_word_with_index(& : StringView, Int32 ->) : Nil
    index = 0

    each_word do |word|
      yield word, index

      index += 1
    end
  end

  class LineIterator
    include Iterator(StringView)

    def initialize(@view : StringView)
      @state = :normal
    end

    def next : StringView | Stop
      case @state
      when :exhausted
        Iterator.stop
      when :endl
        @state = :exhausted
        @view
      when :normal
        l, sep, @view = @view.partition('\n')

        if sep.empty? && @view.empty?
          @state = :exhausted
        elsif @view.empty?
          @state = :endl
        end

        l + sep
      else
        unreachable
      end
    end
  end

  class ReverseLineIterator
    include Iterator(StringView)

    @endl : StringView?

    def initialize(@view : StringView)
    end

    def next : StringView | Stop
      endl0 = @endl

      if endl0 && endl0.empty? && @view.empty?
        return Iterator.stop
      end

      @view, @endl, r = @view.rpartition('\n')

      endl0 ? r + endl0 : r
    end
  end

  def lines : Iterator(StringView)
    LineIterator.new(self)
  end

  def rlines : Iterator(StringView)
    ReverseLineIterator.new(self)
  end

  def each_line(& : StringView ->)
    return if empty?

    start = @byte_start
    line_ascii_only = true

    each_char_with_abs_byte_index do |char, byte_index|
      line_ascii_only &&= char.ascii?
      next unless char == '\n'

      yield StringView.new(@string, start, byte_index + 1, line_ascii_only)
      start = byte_index + 1 # start after newline
      line_ascii_only = true
    end

    # Handle nonempty tail
    if start < byte_end
      yield StringView.new(@string, start, byte_end, line_ascii_only)
      line_ascii_only = true
    end
  end

  def each_line_with_index(& : StringView, Int32 ->)
    index = 0
    each_line do |line|
      yield line, index
      index += 1
    end
  end

  def span(other : StringView) : StringView
    unless string.same?(other.string)
      raise ArgumentError.new("expected string views of the same string")
    end

    StringView.new(string, byte_start, other.byte_end, string.single_byte_optimizable?)
  end

  def reader : Reader
    Reader.new(self)
  end

  def reader(& : Pointer(Reader) ->)
    reader = self.reader
    r = pointerof(reader)
    yield r
  end

  private module NullState
  end

  def chunk_by(& : Char, Int32 -> T) : Array({T, StringView}) forall T
    state0 = NullState
    start0 = @byte_start
    chunks = [] of {T, StringView}

    ascii = true

    each_char_with_abs_byte_index do |char, start1|
      state1 = yield char, start1 - @byte_start
      if state0 == state1
        ascii &&= char.ascii?
        next
      end

      if state0.is_a?(T)
        chunks << {state0, StringView.new(@string, start0, start1, ascii_only: ascii)}
      end

      state0 = state1
      start0 = start1
      ascii = char.ascii?
    end

    if state0.is_a?(T)
      chunks << {state0, StringView.new(@string, start0, byte_end, ascii_only: ascii)}
    end

    chunks
  end

  def each_split(& : StringView, StringView, StringView ->)
    each_char_with_abs_byte_index do |chr, byte_index|
      l = StringView.new(@string, byte_start, byte_index, ascii_only: ascii_only?)
      m = StringView.new(@string, byte_index, byte_index + 1, ascii_only: chr.ascii?)
      r = StringView.new(@string, byte_index + 1, byte_end, ascii_only: ascii_only?)
      yield l, m, r
    end
  end

  def each_inflection(& : StringView, StringView ->)
    each_char_with_abs_byte_index do |chr, byte_index|
      l = StringView.new(@string, byte_start, byte_index, ascii_only: ascii_only?)
      r = StringView.new(@string, byte_index, byte_end, ascii_only: ascii_only?)
      yield l, r
    end

    l = StringView.new(@string, byte_start, byte_end, ascii_only: ascii_only?)
    r = StringView.new(@string, byte_end, byte_end, ascii_only: ascii_only?)
    yield l, r
  end

  def reverse_each_inflection(& : StringView, StringView ->)
    l = StringView.new(@string, byte_start, byte_end, ascii_only: ascii_only?)
    r = StringView.new(@string, byte_end, byte_end, ascii_only: ascii_only?)
    yield l, r

    reverse_each_char_with_abs_byte_index do |chr, byte_index|
      l = StringView.new(@string, byte_start, byte_index, ascii_only: ascii_only?)
      r = StringView.new(@string, byte_index, byte_end, ascii_only: ascii_only?)
      yield l, r
    end
  end

  def extend(*, exclusive = true, & : Char -> Bool) : StringView
    reader = Char::Reader.new(@string, pos: byte_end)

    single_byte = true

    loop do
      chr = reader.current_char
      break if chr == '\0'

      unless ok = yield chr
        break if exclusive
      end

      single_byte &&= chr.single_byte?
      reader.next_char

      break unless ok
    end

    StringView.new(@string, byte_start, reader.pos, single_byte)
  end

  def reverse_extend(& : Char -> Bool) : StringView
    reader = Char::Reader.new(@string, pos: byte_start)

    single_byte = true

    loop do
      break unless reader.has_previous?
      chr = reader.previous_char
      unless yield chr
        reader.next_char
        break
      end
      single_byte &&= chr.single_byte?
    end

    StringView.new(@string, reader.pos, byte_end, single_byte)
  end

  def to_s(io)
    if covers_fully?
      io << @string
      return
    end

    (@byte_start...byte_end).each do |byte_index|
      io.write_byte(@string.byte_at(byte_index))
    end
  end

  def to_s : String
    if covers_fully?
      return @string
    end

    super
  end

  def show(io : IO, view : StringView) : Nil
    unless @string.same?(view.@string)
      raise ArgumentError.new
    end

    each_char_with_abs_byte_index do |chr, byte_index|
      if view.empty?
        if byte_index == view.byte_start
          io << '⏏'
        end
        io << chr
        next
      end

      io << '⏏' if byte_index.entering?(view.byte_bounds)
      io << chr
      io << '⏏' if byte_index.leaving?(view.byte_bounds, size: chr.bytesize)
    end

    if view.empty? && view.byte_start == byte_end
      io << '⏏'
    end
  end

  def show(view : StringView) : String
    String.build { |io| show(io, view) }
  end

  def to_slice : Bytes
    Bytes.new(to_unsafe, byte_end - @byte_start, read_only: true)
  end

  def to_unsafe : UInt8*
    @string.to_unsafe + @byte_start
  end

  def inspect(io)
    io << "…\""
    each_char do |char|
      if char.printable?
        io << char
      else
        char.unicode_escape(io)
      end
    end
    io << "\"…"
  end

  def ==(other : String)
    to_slice == other.to_slice
  end

  def ==(other : Char)
    bytesize == other.bytesize && starts_with?(other)
  end

  def clone
    self
  end

  def_equals_and_hash to_slice
end

struct StringView
  struct Reader
    def initialize(source : StringView)
      @r = Char::Reader.new(source.string, source.byte_start)
      @end = source.byte_end
    end

    def has_next?
      @r.pos < @end
    end

    def pos
      @r.pos
    end

    def max_pos : Int32
      @end
    end

    def current_char
      if @r.pos >= @end
        '\0'
      else
        @r.current_char
      end
    end

    def next_char
      if @r.pos >= @end
        raise IndexError.new
      else
        @r.next_char
      end
    end

    def string
      @r.string
    end
  end
end

class String
  alias Ellipsis = String | CharsOmitted

  record CharsOmitted, l = "[…", r = "…]"

  private def brief_render(ellipsis : String, limit : Int)
    ellipsis
  end

  private def brief_render(ellipsis : CharsOmitted, limit : Int)
    "#{ellipsis.l}#{size - limit} char(s)#{ellipsis.r}"
  end

  def brief(*, limit : Int = 60, ellipsis : Ellipsis = "…") : String
    return self if size <= limit

    rendered = brief_render(ellipsis, limit)

    if limit <= rendered.size
      return self[0, limit]
    end

    rem = limit - rendered.size
    lsize = rem // 2
    rsize = rem - lsize

    "#{self[0, lsize]}#{rendered}#{self[-rsize, rsize]}"
  end

  def fill(char : Char) : String
    String.build(bytesize) do |io|
      size.times do
        io << char
      end
    end
  end

  def starts_with?(range : Range(Char, Char))
    return unless first_char = self[0]?

    first_char.in?(range)
  end

  def ===(other : StringView) : Bool
    # Fast path
    unless bytesize == other.bytesize
      return false
    end

    to_slice == other.to_slice
  end
end

struct Char::Reader
  def reverse_each(& : Char ->)
    while has_previous?
      yield previous_char
    end
  end
end

class String
  def view : StringView
    StringView.new(self, 0, bytesize, single_byte_optimizable?)
  end

  def view(byte_start : Int32, *, byte_end : Int32) : StringView
    unless 0 <= byte_start && byte_end <= bytesize
      raise IndexError.new
    end

    StringView.new(self, byte_start, byte_end, single_byte_optimizable?)
  end

  def view(byte_start : Int32, *, byte_size : Int32) : StringView
    view(byte_start, byte_end: byte_start + byte_size)
  end

  def li(*, bullet = "*", indent = 0, ws = ' ', strip_first = false) : String
    String.build(indent * ws.bytesize + bullet.bytesize + ' '.bytesize + bytesize) do |io|
      indent.times { io << ws }
      unless bullet.empty?
        io << bullet << ' '
      end

      first = true
      each_line_view do |line|
        if first && strip_first
          line = line.lstrip(" ")
        else
          indent.times { io << ws }
        end
        io << line.rstrip
        io.puts
        first = false
      end
    end
  end

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

  def rcut(search : Char) : Tuple(Bytes, Bytes?)
    reader = Char::Reader.new(at_end: self)

    while true
      if reader.current_char == search
        lhs = unsafe_byte_slice(0, reader.pos)
        rhs = unsafe_byte_slice(reader.pos + reader.current_char_width, bytesize - (reader.pos + reader.current_char_width))
        return lhs, rhs
      end

      break unless reader.has_previous?

      reader.previous_char
    end

    {to_slice, nil}
  end

  def digest(algorithm, *, base = 16) : String
    case base
    when 16
      algorithm.hexdigest(self)
    when 64
      algorithm.base64digest(self)
    else
      raise ArgumentError.new("base not supported: #{base}")
    end
  end

  # FIXME: improve, reuse word string
  def each_word(& : String ->) : Nil
    l, sep0, r = partition(' ')

    loop do
      yield l unless l.empty?

      break if sep0.empty?

      if r.empty?
        yield sep0
        break
      end

      succ, sep1, r1 = r.partition(' ')

      l = sep0 + succ
      sep0 = sep1
      r = r1
    end
  end

  def each_word_with_index(& : String, Int32 ->) : Nil
    index = 0

    each_word do |word|
      yield word, index
      index += 1
    end
  end

  # Yields string views corresponding to each line in this string. Byte slices
  # will include *trailing* newlines (i.e. this method does not "take away" any
  # characters from the string).
  #
  # If this string is empty, does not yield anything.
  def each_line_view(& : StringView ->) : Nil
    view.each_line { |v| yield v }
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

def cat(d, *ds)
  d = Term.of(d)
  ds = ds.compact_map do |xs|
    next unless xs
    Term.of(xs)
  end
  n = d.items.size
  d.transaction do |commit|
    ds.each do |xs|
      xs.items.each do |item|
        commit.with(n, item)
        n += 1
      end
    end
    ds.each do |xs|
      xs.pairspart.each_entry { |k, v| commit.with(k, v) }
    end
  end.upcast
end

struct Pf::Multiset(T)
  def initialize(@tally = Pf::Map(T, UInt32).new)
  end

  private def_change

  # size equality
  def parity?(other : Multiset(T)) : Bool
    size == other.size
  end

  # reference equality
  def same?(other : Multiset(T)) : Bool
    @tally.same?(other.@tally)
  end

  def empty? : Bool
    size.zero?
  end

  # Number of unique elements
  def size
    @tally.size
  end

  def add(object : T) : Multiset(T)
    change(tally: @tally.update(object, 1, &.succ))
  end

  def delete(object : T) : Multiset(T)
    return self unless count = @tally[object]?

    if count == 1
      change(tally: @tally.dissoc(object))
    else
      change(tally: @tally.assoc(object, count - 1))
    end
  end
end

# abstract class BiList(T)
#   def self.[] : BiList(T)
#     Zero(T).new
#   end

#   def self.[](*objects : T) : BiList(T)
#     objects.reduce(Zero(T).new) { |lst, object| lst.append(object) }
#   end

#   def first
#     first? || raise ArgumentError.new
#   end

#   def last
#     last? || raise ArgumentError.new
#   end

#   def each(& : T ->)
#     lst = self
#     while object = lst.first?
#       yield object
#       lst = lst.rest
#     end
#   end

#   def empty? : Bool
#     false
#   end

#   def one? : Bool
#     false
#   end
# end

# class BiList::Zero(T) < BiList(T)
#   def empty? : Bool
#     true
#   end

#   def append(object : T)
#     One(T).new(object)
#   end

#   def prepend(object : T)
#     One(T).new(object)
#   end

#   def first? : T?
#   end

#   def last? : T?
#   end

#   def prior : BiList(T)
#     self
#   end

#   def rest : BiList(T)
#     self
#   end

#   # From right to left
#   def reverse_each(*, _root = true, &fn : T, Bool ->)
#   end

#   def_equals_and_hash
# end

# class BiList::One(T) < BiList(T)
#   def initialize(@v : T)
#   end

#   def one? : Bool
#     true
#   end

#   def append(object : T)
#     Many(T).new(@v, Zero(T).new, object)
#   end

#   def prepend(object : T)
#     Many(T).new(object, Zero(T).new, @v)
#   end

#   def first? : T?
#     @v
#   end

#   def last? : T?
#     @v
#   end

#   def prior : BiList(T)
#     Zero(T).new
#   end

#   def rest : BiList(T)
#     Zero(T).new
#   end

#   # From right to left
#   def reverse_each(*, _root = true, &fn : T, Bool ->)
#     fn.call(@v, _root)
#   end

#   def_equals_and_hash @v
# end

# class BiList::Many(T) < BiList(T)
#   def initialize(@l : T, @mid : BiList(T), @r : T)
#   end

#   def append(object : T)
#     Many(T).new(@l, @mid.append(@r), object)
#   end

#   def prepend(object : T)
#     Many(T).new(object, @mid.prepend(@l), @r)
#   end

#   def first? : T?
#     @l
#   end

#   def last? : T?
#     @r
#   end

#   def prior : BiList(T)
#     @mid.prepend(@l)
#   end

#   def rest : BiList(T)
#     @mid.append(@r)
#   end

#   # From right to left
#   # TODO: convert to iterative
#   def reverse_each(*, _root = false, &fn : T, Bool ->)
#     fn.call(@r, false)
#     @mid.reverse_each(_root: false, &fn)
#     fn.call(@l, _root)
#   end

#   def_equals_and_hash @l, @mid, @r
# end

struct TinyArray(T)
  include Indexable(T)

  def initialize(object : T)
    @mem = Pointer(T).malloc(1)
    @mem[0] = object
  end

  protected def initialize(@mem : T*)
  end

  def self.[](object : T)
    new(object)
  end

  def self.[](object : T, *objects : T)
    objects.reduce(self[object]) { |ary, object| ary.append(object) }
  end

  @[AlwaysInline]
  private def mem : T*
    Pointer(T).new(@mem.address & ~0b111)
  end

  @[AlwaysInline]
  def unsafe_fetch(index : Int)
    mem[index]
  end

  @[AlwaysInline]
  def size
    (@mem.address & 0b111) &+ 1
  end

  @[AlwaysInline]
  def conceal : Void*
    @mem.as(Void*)
  end

  def self.reveal(pointer : Void*) : TinyArray(T)
    new(pointer.as(T*))
  end

  def append(object : T)
    size0 = size
    if size0 == 8
      raise IndexError.new
    end

    size1 = size0 &+ 1

    # Copy and append
    mem1 = Pointer(T).malloc(size1)
    mem1.copy_from(mem, size0)
    mem1[size0] = object

    # Repackage pointer with size embedded
    mem1 = Pointer(T).new(mem1.address | (size1 &- 1))

    TinyArray.new(mem1)
  end

  def prepend(object : T)
    size0 = size
    if size0 == 8
      raise IndexError.new
    end

    size1 = size0 &+ 1

    # Copy and append
    mem1 = Pointer(T).malloc(size1)
    (mem1 + 1).copy_from(mem, size0)
    mem1[0] = object

    # Repackage pointer with size embedded
    mem1 = Pointer(T).new(mem1.address | (size1 &- 1))

    TinyArray.new(mem1)
  end

  def rest : TinyArray(T)
    size0 = size
    if size0 == 1
      raise IndexError.new
    end

    size1 = size0 &- 1

    # Copy and append
    mem1 = Pointer(T).malloc(size1)
    mem1.copy_from(mem + 1, size1)

    # Repackage pointer with size embedded
    mem1 = Pointer(T).new(mem1.address | (size1 &- 1))

    TinyArray.new(mem1)
  end

  def prior : TinyArray(T)
    size0 = size
    if size0 == 1
      raise IndexError.new
    end

    size1 = size0 &- 1

    # Copy and append
    mem1 = Pointer(T).malloc(size1)
    mem1.copy_from(mem, size1)

    # Repackage pointer with size embedded
    mem1 = Pointer(T).new(mem1.address | (size1 &- 1))

    TinyArray.new(mem1)
  end

  def where(index, object : T)
    size0 = size

    # Copy and append
    mem1 = Pointer(T).malloc(size0)
    mem1.copy_from(mem, size0)
    mem1[index] = object

    # Repackage pointer with size embedded
    mem1 = Pointer(T).new(mem1.address | (size0 &- 1))

    TinyArray.new(mem1)
  end

  def ==(other : TinyArray(T)) : Bool
    equals?(other) { |a, b| a == b }
  end
end

CAPACITY = 8

abstract class BiList(T)
  def self.[] : BiList(T)
    Zero(T).new
  end

  def self.[](*objects : T) : BiList(T)
    objects.reduce(Zero(T).new) { |lst, object| lst.append(object) }
  end

  def first
    first? || raise ArgumentError.new
  end

  def last
    last? || raise ArgumentError.new
  end

  def one? : Bool
    false
  end

  def self.new : BiList(T)
    Zero(T).new
  end

  def empty? : Bool
    false
  end

  def each(&fn : T ->) : Nil
    each(fn)
  end

  def reverse_each(&fn : T, Bool ->) : Nil
    reverse_each(true, fn)
  end

  def reduce(state : U, &fn : U, T -> U) : U forall U
    each { |object| state = fn.call(state, object) }

    state
  end

  def to_a : Array(T)
    objects = [] of T
    each { |object| objects << object }
    objects
  end

  def inspect(io)
    io << "BiList["
    index = 0
    each do |object|
      io << ", " if index > 0
      object.inspect(io)
      index += 1
    end
    io << "]"
  end
end

class BiList::Zero(T) < BiList(T)
  def initialize
  end

  def first? : T?
  end

  def rest : BiList(T)
    self
  end

  def last? : T?
  end

  def prior : BiList(T)
    self
  end

  def prepend(object : T) : BiList(T)
    One(T).new(object)
  end

  def append(object : T) : BiList(T)
    One(T).new(object)
  end

  def empty? : Bool
    true
  end

  def mapfirst(& : T -> T) : BiList(T)
    self
  end

  def maplast(& : T -> T) : BiList(T)
    self
  end

  def each(fn : T ->) : Nil
  end

  # From right to left
  def reverse_each(_root, fn : T, Bool ->)
  end

  def_equals_and_hash
end

class BiList::One(T) < BiList(T)
  def initialize(@object : T)
  end

  def one?
    true
  end

  def first? : T?
    @object
  end

  def rest : BiList(T)
    Zero(T).new
  end

  def last? : T?
    @object
  end

  def prior : BiList(T)
    Zero(T).new
  end

  def prepend(object : T) : BiList(T)
    Many(T).new(TinyArray(T)[object], BiList(Void*).new, TinyArray(T)[@object])
  end

  def append(object : T) : BiList(T)
    Many(T).new(TinyArray(T)[@object], BiList(Void*).new, TinyArray(T)[object])
  end

  def mapfirst(& : T -> T) : BiList(T)
    One(T).new(yield @object)
  end

  def maplast(& : T -> T) : BiList(T)
    One(T).new(yield @object)
  end

  def each(fn : T ->) : Nil
    fn.call(@object)
  end

  # From right to left
  def reverse_each(_root, fn : T, Bool ->)
    fn.call(@object, _root)
  end

  def_equals_and_hash @object
end

class BiList::Many(T) < BiList(T)
  def initialize(@l : TinyArray(T), @mid : BiList(Void*), @r : TinyArray(T))
  end

  def first? : T?
    @l.first
  end

  def rest : BiList(T)
    unless @l.size == 1
      return Many(T).new(@l.rest, @mid, @r)
    end

    if @mid.empty?
      if @r.size == 1
        return One(T).new(@r.first)
      else
        return Many(T).new(TinyArray(T)[@r.first], @mid, @r.rest)
      end
    end

    head = TinyArray(T).reveal(@mid.first?.not_nil!)
    unless head.size == CAPACITY
      return Many(T).new(head, @mid.rest, @r)
    end

    Many(T).new(TinyArray(T)[head.first], @mid.mapfirst { |it| TinyArray(T).reveal(it).rest.conceal }, @r)
  end

  def last? : T?
    @r.last
  end

  def prior : BiList(T)
    unless @r.size == 1
      return Many(T).new(@l, @mid, @r.prior)
    end

    if @mid.empty?
      if @l.size == 1
        return One(T).new(@l.last)
      else
        return Many(T).new(@l.prior, @mid, TinyArray(T)[@l.last])
      end
    end

    tail = TinyArray(T).reveal(@mid.last?.not_nil!)
    unless tail.size == CAPACITY
      return Many(T).new(@l, @mid.prior, tail)
    end

    Many(T).new(@l, @mid.maplast { |it| TinyArray(T).reveal(it).prior.conceal }, TinyArray(T)[tail.last])
  end

  def prepend(object : T) : BiList(T)
    if @l.size == CAPACITY
      Many(T).new(TinyArray(T)[object], @mid.prepend(@l.conceal), @r)
    else
      Many(T).new(@l.prepend(object), @mid, @r)
    end
  end

  def append(object : T) : BiList(T)
    if @r.size == CAPACITY
      Many(T).new(@l, @mid.append(@r.conceal), TinyArray(T)[object])
    else
      Many(T).new(@l, @mid, @r.append(object))
    end
  end

  def mapfirst(& : T -> T) : BiList(T)
    Many(T).new(@l.where(0, yield @l.first), @mid, @r)
  end

  def maplast(& : T -> T) : BiList(T)
    Many(T).new(@l, @mid, @r.where(@r.size - 1, yield @r.last))
  end

  def each(fn : T ->) : Nil
    @l.each(&fn)
    @mid.each do |objects|
      TinyArray(T).reveal(objects).each(&fn)
    end
    @r.each(&fn)
  end

  def reverse_each(_root, fn : T, Bool ->)
    @r.reverse_each { |object| fn.call(object, false) }
    @mid.reverse_each do |objects|
      TinyArray(T).reveal(objects).reverse_each { |object| fn.call(object, false) }
    end
    @l.reverse_each { |object| fn.call(object, _root) }
  end

  # FIXME: optimize
  def ==(other : BiList(T))
    return false unless other.is_a?(Many(T))
    return false unless @l == other.@l
    return false unless @r == other.@r

    seen = Set(T).new
    equals = true

    @mid.each do |objects|
      TinyArray(T).reveal(objects).each do |object|
        seen << object
      end
    end

    other.@mid.each do |objects|
      TinyArray(T).reveal(objects).each do |object|
        equals &&= object.in?(seen)
      end
    end

    equals
  end

  def hash(hasher)
    each do |object|
      hasher = object.hash(hasher)
    end
    hasher
  end
end

class Pf::MapBox(K, V)
  def initialize(@map = Pf::Map(K, V).new)
  end

  private def_change

  def size
    @map.size
  end

  def includes?(k : K) : Bool
    @map.includes?(k)
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
    change(map: @map.assoc(k, v))
  end

  def dissoc(k : K)
    change(map: @map.dissoc(k))
  end
end

class Pf::SetBox(T)
  def initialize(@set = Pf::Set(T).new)
  end

  private def_change

  delegate :size, :empty?, to: @set

  def includes?(v : T) : Bool
    @set.includes?(v)
  end

  def each(& : T ->) : Nil
    @set.each { |v| yield v }
  end

  def add(v : T)
    change(set: @set.add(v))
  end

  def delete(v : T)
    change(set: @set.delete(v))
  end
end

class Pf::BidiMapBox(K, V)
  def initialize(@map = Pf::BidiMap(K, V).new)
  end

  private def_change

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
    change(map: @map.assoc(k, v))
  end

  def dissoc_by_key(k : K)
    change(map: @map.dissoc_by_key(k))
  end

  def dissoc_by_value(v : V)
    change(map: @map.dissoc_by_value(v))
  end
end

macro pipe(object, call)
  ({{(call.receiver ? "#{call.receiver}." : "").id}}{{call.name}}({{object}}, {{(call.args + (call.named_args || [] of ::NoReturn)).splat}}) {{call.block}})
end

macro pipe(object, call, *calls)
  pipe(pipe({{object}}, {{call}}), {{calls.splat}})
end

macro try?(head, *tail)
  pass do
    %result = {{head}}
    next if %result.nil?

    {% for node in tail %}
      {% if node.is_a?(Path) || node.is_a?(TypeNode) %}
        %result = %result.as?({{node.resolve}})
      {% elsif node.is_a?(Call) %}
        %result = %result.{{node}}
      {% else %}
        {% node.raise "unsupported node in tail" %}
      {% end %}
      next if %result.nil?
    {% end %}

    %result

  end
end

abstract struct Enum
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

def parse(dict : Term::Dict, spec : T) forall T
  {% begin %}
    { {% for key, spec in T %}
        {% if spec.keys.map(&.id).includes?(:default.id) %}
          {{key}}: dict[{{key.symbolize}}]?
            .try(&.to?({{spec[:type].instance}}))
            .default(spec[{{key.symbolize}}][:default]),
        {% else %}
          {{key}}: dict[{{key.symbolize}}].to({{spec[:type].instance}}),
        {% end %}
      {% end %} }
  {% end %}
end

class SleepyQueue(T)
  enum State : UInt8
    Asleep
    Empty
    Nonempty
  end

  @state : State

  getter size

  protected def initialize(@queue, @state, @size)
  end

  def initialize
    @queue = BiList(T).new
    @state = :asleep
    @size = 0u32
  end

  delegate :asleep?, :empty?, :nonempty?, to: @state

  def enqueue(object : T) : SleepyQueue(T)
    SleepyQueue.new(@queue.append(object), :nonempty, @size + 1)
  end

  def interject(object : T) : SleepyQueue(T)
    SleepyQueue.new(@queue.prepend(object), :nonempty, @size + 1)
  end

  def dequeue : {SleepyQueue(T), T?}
    case @queue
    when .empty? then {SleepyQueue.new(@queue, :asleep, 0), nil}
    when .one?   then {SleepyQueue.new(@queue.rest, :empty, 0), @queue.first}
    else
      {SleepyQueue.new(@queue.rest, :nonempty, @size - 1), @queue.first}
    end
  end
end

class AtomicSleepyQueue(T)
  def initialize
    @queue = Atomic(SleepyQueue(T)).new(SleepyQueue(T).new)
  end

  {% for method in %w(enqueue interject) %}
    def {{method.id}}(object : T, & : ->) : Nil
      queue0 = @queue.get(:relaxed)
      while true
        queue1 = queue0.{{method.id}}(object)
        queue0, ok = @queue.compare_and_set(queue0, queue1, :relaxed, :relaxed)
        break if ok
      end

      return unless queue0.asleep?

      yield
    end
  {% end %}

  def dequeue? : T?
    queue0 = @queue.get(:relaxed)
    while true
      queue1, object = queue0.dequeue
      queue0, ok = @queue.compare_and_set(queue0, queue1, :relaxed, :relaxed)
      break if ok
    end
    object
  end

  def wait(limit = 1) : Nil
    while true
      queue = @queue.get(:relaxed)
      break if queue.size < limit

      Intrinsics.pause
    end
  end
end

class Channel
  Void = begin
    chan = Channel(Nil).new
    chan.close
    chan
  end

  macro mux(type, ctx, count)
    %master = Channel({{type}}).new
    %relays = { {{ (0...count).map { nil }.splat }} }.map { Channel({{type}}).new }

    {{ctx}}.spawn do
      while object = %master.receive
        %relays.each &.send(object)
      end
    end

    { %master, *%relays }
  end

  def <<(object)
    send(object)
  end
end

module Indexable(T)
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
end

struct Slice(T)
  def self.with(*objects)
    Slice(T).new(objects.size, read_only: true) { |index| objects[index].as(T) }
  end

  def starts_with?(other : Slice(T)) : Bool
    size >= other.size && self[0...other.size] == other
  end

  def prefixed_by?(other : Slice(T)) : Bool
    size > other.size && self[0...other.size] == other
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

  def prepend_many(objects : Slice(T), & : T -> U) : Slice(U) forall U
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

  def append_many(objects : Slice(T), & : T -> U) : Slice(U) forall U
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

  def read_only : Slice(T)
    Slice(T).new(to_unsafe, size, read_only: true)
  end

  private module NullState
  end

  def chunk_by(accessor : T -> U, & : U, Slice(T) ->) forall U
    start = self
    size = 0
    chunks = [] of {T, Slice(T)}

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

      yield state0, start[0, size]

      state0 = state1
      start += size
      size = 0
    end

    chunks
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

module Append
end

module Deepset
  extend self

  def clean(dict : Term::Dict, step)
    Term.of(step)
  end

  def clean(dict : Term::Dict, step, *steps)
    k = Term.of(step)
    if v0 = dict[k]?
      v0 = v0.as_d?
    end
    v0 ||= Term[]
    v1 = clean(v0, *steps)
    if v1.nil? || (v1.type.dict? && v1.empty?)
      dict.without(k)
    else
      dict.with(k, v1)
    end
  end

  def clean(dict : Term::Dict, step : Append.class, *steps)
    clean(dict, dict.items.size, *steps)
  end

  def clean(dict : Term::Dict, step : Term::Dict::ItemsView, *steps)
    unless k = step[0]?
      return clean(dict, *steps)
    end

    clean(dict, k, step + 1, *steps)
  end

  def cleanless(dict : Term::Dict, step)
    Term.of(step)
  end

  def cleanless(dict : Term::Dict, step, *steps)
    k = Term.of(step)
    if v0 = dict[k]?
      v0 = v0.as_d?
    end
    v0 ||= Term[]
    v1 = cleanless(v0, *steps)
    dict.with(k, v1)
  end

  def cleanless(dict : Term::Dict, step : Append.class, *steps)
    cleanless(dict, dict.items.size, *steps)
  end

  def cleanless(dict : Term::Dict, step : Term::Dict::ItemsView, *steps)
    unless k = step[0]?
      return cleanless(dict, *steps)
    end

    cleanless(dict, k, step + 1, *steps)
  end
end

def deepset(dict : Term::Dict, *steps, cleanup : Bool = true)
  cleanup ? Deepset.clean(dict, *steps) : Deepset.cleanless(dict, *steps)
end

def keypaths(haystack : Term, needle : Term::Match::Pattern, *, cue : Term::Sym? = nil, env env0 = Term[], keypath = Term[], &fn : Term::Dict, Term::Dict ->)
  if row = needle.match?(env0, haystack)
    env, _ = row
    fn.call(keypath, env)
    return
  end

  return unless haystack.type.dict?
  return if cue && !haystack.probably_includes?(cue)

  dict = haystack.unsafe_as_d
  dict.each_entry do |k, v|
    keypaths(v, needle, keypath: keypath.append(k), env: env0, &fn)
  end
end

struct Time::Span
  def humanize(io)
    nanos = total_nanoseconds

    k1 = 1000u64

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
    @storage = {} of T => UInt16
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

  def tally?(object : T) : UInt16?
    @storage[object]?
  end

  def tally(object : T) : UInt16
    @storage[object]
  end

  def add(object : T) : self
    @storage[object] = (@storage[object]? || 0u16) + 1

    self
  end

  def <<(object : T) : self
    add(object)
  end

  def add?(object : T) : Bool
    @storage[object] = tally = (@storage[object]? || 0u16) + 1

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
    intersection = {} of T => UInt16

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
    difference = {} of T => UInt16

    # An element appears in the intersection of two bags the minimum of
    # the number of times it appears in either.

    @storage.each do |object, tally0|
      tally1 = other.@storage[object]? || 0u16
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

  def quickselect(k : Int) : T
    raise ArgumentError.new("k must be positive") if k < 0
    data = self.is_a?(Array) ? self.dup : self.to_a
    quickselect_internal(data, 0, data.size - 1, k)
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

  def segments(indices : Enumerable(Int32), &)
    prev = 0

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
  def slide_subrange_of(n : Int, & : Range(B, E) ->)
    {% unless B < ::Int && E < ::Int %}
      {% raise "expected Range(_ < Int, _ < Int)" %}
    {% end %}

    assert exclusive?
    assert n.zero? || n.positive?

    return if n > size

    if n.zero?
      (@begin..@end).each do |i|
        yield i...i
      end
      return
    end

    i = @begin
    while i + n <= @end
      yield i...i + n
      i += n
    end
  end
end

struct Int
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

# TODO: capacity and eviction.
class Pf::Cache(K, V)
  def initialize
    @storage = Atomic(Pf::MapBox(K, V)).new(Pf::MapBox(K, V).new)
  end

  def fetch(key : K, *, fresh = false, & : -> V) : V
    if fresh
      return yield
    end

    storage0 = @storage.get(:acquire)
    proposal = nil
    while true
      if value = storage0[key]?
        return value
      end
      proposal ||= yield
      storage1 = storage0.assoc(key, proposal)
      storage0, ok = @storage.compare_and_set(storage0, storage1, :release, :acquire)
      break if ok
    end
    proposal
  end
end

{% for width in %w(8 16 32 64 128) %}
  struct UInt{{width.id}}
    def self.width
      {{width.id}}
    end
  end

  struct Int{{width.id}}
    def self.width
      {{width.id}}
    end
  end
{% end %}

struct BigRational
  def to_u128
    if integer?
      to_big_i.to_u128
    else
      raise ArgumentError.new # ?!
    end
  end
end

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
end

module Enumerable(T)
  def to_readonly_slice(& : T, Int32 -> U) : Slice(U) forall U
    buffer = stack_alloc Pf::Kit::HybridArray(U, 32).new
    each_with_index do |item, index|
      buffer << (yield item, index)
    end

    buffer.to_readonly_slice(&.itself)
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

def oklch(l : Float64, c : Float64, h : Float64)
  Oklch.to_rgb(l, c, h)
end

struct Float32
  def approx?(other : Float32, *, eps = 0.001f32)
    (self - other).abs <= eps
  end
end

module ICache(K, V)
  abstract def []?(key : K) : V?
  abstract def []=(key : K, value : V) : V

  def fetch?(key : K, &) : {Bool, V}
    if value = self[key]?
      return true, value
    end

    value = yield

    self[key] = value

    {false, value}
  end

  def fetch(key : K, &) : V
    _, value = fetch?(key) { yield }

    value
  end

  def put_if_absent(key, &)
    fetch(key) { yield }
  end
end

# FIFO fixed-capacity cache.
#
# TODO: this will obviously "leak" memory, in the sense that it keeps
# pointers to K/V, and thus very large caches will keep in memory something
# that may have been collected by the GC already. We need a WeakRef impl
# of this, but as far as I understand, Hash based stuff is very clumsy with
# WeakRef; so we'll probably have to consider a hand-written HAMT based solution.
# But then finding the node to delete would be clumsy. We can do it as HAMT to
# map key to index + Binary Tree but this requires balancing in any case if we
# want some kind of order -- which is tough...
#
# TODO: lots of very hot places rely on this. split into buckets & in general
# see SOTA parallel hashes !!! Sync Map is buggy and causes occasional deadlocks.
class SyncCache(K, V)
  include ICache(K, V)

  def initialize(@capacity : Int32, *, preallocate : Bool, byref : Bool = false)
    if preallocate
      @data = Hash(K, V).new(initial_capacity: @capacity)
    else
      @data = {} of K => V
    end
    @data.compare_by_identity if byref
    @lock = Sync::RWLock.new
  end

  def size
    @lock.read { @data.size }
  end

  def []?(key : K) : V?
    @lock.read { @data[key]? }
  end

  def []=(key : K, value : V) : V
    @lock.write do
      if @data.size > @capacity
        @data.delete(@data.first_key)
      end
      @data[key] = value
    end
  end
end

struct Uncached(K, V)
  include ICache(K, V)

  def []?(key : K) : V?
  end

  def []=(key : K, value : V) : V
    value
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
end

require "bit_array"

class BitList
  include Indexable::Mutable(Bool)

  GROWTH_FACTOR = 1.5

  def initialize(capacity0 = 32)
    @bits = BitArray.new(capacity0)
    @size = 0
  end

  protected def initialize(*, @size)
    @bits = BitArray.new(@size)
  end

  def self.zeros(n)
    new(size: n)
  end

  def size : Int32
    @size
  end

  def unsafe_fetch(index : Int) : Bool
    @bits.unsafe_fetch(index)
  end

  def unsafe_put(index : Int, value : Bool) : Nil
    @bits.unsafe_put(index, value)
  end

  def push(value : Bool) : Nil
    # Resize
    if @size + 1 > @bits.size
      bits1 = BitArray.new((@bits.size * GROWTH_FACTOR).to_i)
      @bits.each_with_index do |bit, index|
        bits1.unsafe_put(index, bit)
      end
      @bits = bits1
    end

    unsafe_put(@size, value)

    @size += 1
  end

  def <<(value : Bool) : self
    push(value)

    self
  end

  def clear : Nil
    @size = 0
  end

  def resize(@size)
  end

  def each_bucket(&)
    @bits.each_bucket { |bucket| yield bucket }
  end

  def nbuckets
    @bits.nbuckets
  end
end

struct BitArray
  def each_bucket(&)
    @bits.to_slice(malloc_size).each do |bucket|
      yield bucket
    end
  end

  def nbuckets
    malloc_size
  end
end

struct Time
  def self.measured(& : -> T) : {Time::Span, T} forall T
    b = Time.monotonic
    result = yield
    e = Time.monotonic
    {e - b, result}
  end

  def self.measure(sink : Time::Span ->, & : -> T) : T forall T
    b = Time.monotonic
    result = yield
    e = Time.monotonic
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

  def clear
    @lock.write { @hash.clear }
  end
end

module InspectToS
  def to_s(io)
    inspect(io)
  end
end

def watch(path : Path, &fn : IO ->)
  File.open(path, "r") do |io|
    fn.call(io)
  end

  MT.spawn do
    modt0 = nil

    loop do
      info = File.info(path)
      modt1 = info.modification_time
      next if modt0 == modt1

      Log.info { "file at #{path} changed" }

      modt0 = modt1

      File.open(path, "r") do |io|
        fn.call(io)
      end
    ensure
      sleep 300.milliseconds
    end
  end
end

module Math
  DEG_TO_RAD = Math::PI/180

  def deg2rad(degrees)
    degrees * DEG_TO_RAD
  end
end

class Log::AsyncInMemoryBackend < Log::Backend
  getter entries = Array(Log::Entry).new

  def initialize(@severity : Log::Severity)
    super(:async)
  end

  def write(entry : Log::Entry) : Nil
    @entries << entry
  end
end

class BlockingQueue(T)
  def initialize
    @queue = Deque(T).new
    @mutex = Sync::Mutex.new
    @cv = Sync::ConditionVariable.new(@mutex)
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

  def lock(& : Deque(T) -> U) : U forall U
    @mutex.synchronize do
      yield @queue
    end
  end

  def clear : Nil
    lock(&.clear)
  end
end

module Parseout
  extend self

  # :nodoc:
  macro try(branch)
      {{branch}}
    end

  # :nodoc:
  macro try(branch, *branches)
      {{@type}}.either({{branch}}) { {{@type}}.try({{branches.splat}}) }
    end

  # :nodoc:
  def either(π, &)
    π
  end

  # :nodoc:
  def either(π : Rej, &)
    yield
  end

  # :nodoc:
  def map(π, &)
    assert !π.is_a?(Nok)

    yield π
  end

  # :nodoc:
  def map(π : Nok, &)
    π
  end

  # :nodoc:
  def map(a, b, &)
    map(a) do |x|
      map(b) do |y|
        yield x, y
      end
    end
  end

  alias Nok = Err | Rej

  # Represents a failed parse. Issues were reported to the issue sink.
  record Err

  # Represents a rejection: not necessarily a failure, but rather, a failure
  # to recognize that didn't generate any issues.
  record Rej

  def cached(cache : ICache, term : Term, issues : Issue::Sink, &)
    if cached = cache[term]?
      return cached
    end

    version0 = issues.version
    π = yield
    version1 = issues.version

    if version0 == version1 && !π.is_a?(Nok)
      cache[term] = π
    end

    π
  end
end

class ::Sync::Future
  def inspect(io)
    io << "Sync::Future(...)"
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
    arena = stack_alloc self.new(mem.to_unsafe)
    yield arena
  end

  def size
    @auxsize + @memsize
  end

  def construct(*args, **kwargs) : T
    if @memsize + 1 > N
      if @auxsize + 1 > @auxcap
        @auxcap = Math.max(MINCAP, (@auxcap * 1.5).to_i)
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
end
