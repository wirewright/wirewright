macro on_demand(typedecl)
  getter({{typedecl}}) { {{typedecl.type}}.new }

  protected def has_{{typedecl.var.id}}? : Bool
    return false unless %collection = @{{typedecl.var.id}}
    return true unless {{ typedecl.type.resolve.has_method?(:empty?) }}
    !%collection.empty?
  end
end

# consistency
macro defrecord(cls, *typedecls, inherit = false, &)
  {% header = "".id %}
  {% if inherit && @type.module? %}
    {% header = "include #{@type.id}".id %}
  {% elsif inherit && @type.struct? && @type.abstract? %}
    {% cls = "#{cls.id} < #{@type.id}".id %}
  {% end %}

  struct {{cls}}
    {{header}}

    {% for typedecl in typedecls %}
      getter {{typedecl}}
    {% end %}

    def initialize({{typedecls.map { |typedecl| "@#{typedecl}".id }.splat}})
    end

    def_change

    {{yield}}

    def_equals_and_hash {{typedecls.map { |typedecl| "@#{typedecl.var}".id }.splat}}
  end
end

macro defcase(cls, *typedecls, inherit = false, &)
  {% header = "".id %}
  {% if inherit && @type.module? %}
    {% header = "include #{@type.id}".id %}
  {% elsif inherit && @type.class? %}
    {% cls = "#{cls.id} < #{@type.id}".id %}
  {% end %}

  class {{cls}}
    {{header}}

    {% for typedecl in typedecls %}
      getter {{typedecl}}
    {% end %}

    def initialize({{typedecls.map { |typedecl| "@#{typedecl}".id }.splat}})
    end

    def_change

    {{yield}}

    def_equals_and_hash {{typedecls.map { |typedecl| "@#{typedecl.var}".id }.splat}}
  end
end

macro subclass(*args, &block)
  defcase({{args.splat}}, inherit: true) {{ block }}
end

macro substruct(*args, &block)
  defrecord({{args.splat}}, inherit: true) {{ block }}
end

class ::UnreachableException < Exception
end

macro unreachable
  raise ::UnreachableException.new
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

  def to_readonly_slice : Slice(T)
    Slice.new(to_unsafe, size, read_only: true)
  end

  def fuse(other, & : MutView(T) ->)
    yield MutView(T).new(concat(other), size - other.size, size)

    self
  end

  def concat(other : Array(U), & : U -> T) forall U
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
      {% for cls in U %}
        {{ cls.id.downcase.gsub(/[^\w]/, "_") }} = [] of {{cls.instance}}
      {% end %}

      %others = [] of T

      each do |object|
        case object
        {% for cls in U %}
        when {{cls.instance}}
          {{cls.id.downcase.gsub(/[^\w]/, "_")}} << object
        {% end %}
        else
          %others << object
        end
      end

      { {% for cls in U %}
          {{ cls.id.downcase.gsub(/[^\w]/, "_") }},
        {% end %} %others }
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

# Stack is a dynamic stack implementation. It grows by a factor of 1.5 when
# it's full and does not reallocate when popping.
class Stack(T)
  include Indexable::Mutable(T)

  getter size

  # Initializes a new stack with the given *capacity*.
  def initialize(@capacity = 0)
    @stack = Pointer(T).null
    unless @capacity.zero?
      @stack = @stack.realloc(@capacity)
    end
    @size = 0
  end

  def unsafe_fetch(index : Int)
    @stack[index]
  end

  def unsafe_put(index : Int, value : T)
    @stack[index] = value
  end

  # Pushes *element* onto the stack. If the stack is full, increases the capacity
  # by a factor of 1.5.
  def push(element : T) : self
    if @size == @capacity
      @capacity = Math.max(2, @capacity * 1.5).ceil.to_i
      @stack = @stack.realloc(@capacity)
    end
    @stack[@size] = element
    @size += 1
    self
  end

  def <<(element : T)
    push(element)
  end

  # Pops an element from the top of the stack. Returns `nil` if the stack is empty.
  def pop? : T?
    return if @size == 0
    @size -= 1
    @stack[@size]
  end

  def pop
    pop? || raise IndexError.new
  end

  def pretty_print(pp)
    pp.list("Stack[", self, "]")
  end
end

class IO::Empty < IO
  def read(slice : Bytes)
    0
  end

  def write(slice : Bytes) : Nil
  end
end

# Performs word wrapping followed by character wrapping on *text*.
#
# - *maxwidth* is the maximum number of columns that the text should occupy (inclusive).
#
# Appends wrapped text to *io*.
def wrap(io : IO, text : String, maxwidth = 60) : Nil
  return unless maxwidth > 0

  width = 0

  text.each_line do |line|
    line.split(' ') do |word|
      while width + word.size >= maxwidth
        # Insert sort breaks.
        if width > 0
          io.puts
        end

        trunk = word[0, maxwidth]? || word
        io << trunk

        width = trunk.size
        word = word[width..]
      end

      next if word.empty?

      if width > 0
        io << ' '
        width += 1
      end

      io << word
      width += word.size
    end

    # Insert hard breaks.
    io.puts
    width = 0
  end
end

# Same as `wrap`, but builds a `String` instead of appending to an IO.
def wrap(text : String, maxwidth = 60) : String
  String.build { |io| wrap(io, text, maxwidth) }
end

# Represents an LCH color, used throughout the code.
record Color, l : Int32, c : Int32, h : Int32, a : Int32 = 0xff do
  Transparent = Color.new(0, 0, 0, 0)
  Black       = Color.new(0, 0, 0, 255)
  White       = Color.new(100, 0, 0, 255)
  DarkGray    = Color.new(20, 0, 0, 255)
  Gray        = Color.new(50, 0, 0, 255)
  LightGray   = Color.new(80, 0, 0, 255)
  DarkBlue    = Color.new(20, 40, 270, 255)
  LightBlue   = Color.new(80, 40, 270, 255)

  TW = {
    "neutral-50":  Color.rgb(0xfa, 0xfa, 0xfa),
    "neutral-100": Color.rgb(0xf5, 0xf5, 0xf5),
    "neutral-200": Color.rgb(0xe5, 0xe5, 0xe5),
    "neutral-300": Color.rgb(0xd4, 0xd4, 0xd4),
    "neutral-400": Color.rgb(0xa3, 0xa3, 0xa3),
    "neutral-500": Color.rgb(0x73, 0x73, 0x73),
    "neutral-600": Color.rgb(0x52, 0x52, 0x52),
    "neutral-700": Color.rgb(0x40, 0x40, 0x40),
    "neutral-800": Color.rgb(0x26, 0x26, 0x26),
    "neutral-900": Color.rgb(0x17, 0x17, 0x17),
    "neutral-950": Color.rgb(0x0a, 0x0a, 0x0a),
    "blue-50":     Color.rgb(0xef, 0xf6, 0xff),
    "blue-100":    Color.rgb(0xdb, 0xea, 0xfe),
    "blue-200":    Color.rgb(0xbf, 0xdb, 0xfe),
    "blue-300":    Color.rgb(0x93, 0xc5, 0xfd),
    "blue-400":    Color.rgb(0x60, 0xa5, 0xfa),
    "blue-500":    Color.rgb(0x3b, 0x82, 0xf6),
    "blue-600":    Color.rgb(0x25, 0x63, 0xeb),
    "blue-700":    Color.rgb(0x1d, 0x4e, 0xd8),
    "blue-800":    Color.rgb(0x1e, 0x40, 0xaf),
    "blue-900":    Color.rgb(0x1e, 0x3a, 0x8a),
    "blue-950":    Color.rgb(0x17, 0x25, 0x54),
    "yellow-50":   Color.rgb(0xfe, 0xfc, 0xe8),
    "yellow-100":  Color.rgb(0xfe, 0xf9, 0xc3),
    "yellow-200":  Color.rgb(0xfe, 0xf0, 0x8a),
    "yellow-300":  Color.rgb(0xfd, 0xe0, 0x47),
    "yellow-400":  Color.rgb(0xfa, 0xcc, 0x15),
    "yellow-500":  Color.rgb(0xea, 0xb3, 0x08),
    "yellow-600":  Color.rgb(0xca, 0x8a, 0x04),
    "yellow-700":  Color.rgb(0xa1, 0x62, 0x07),
    "yellow-800":  Color.rgb(0x85, 0x4d, 0x0e),
    "yellow-900":  Color.rgb(0x71, 0x3f, 0x12),
    "yellow-950":  Color.rgb(0x42, 0x20, 0x06),
    "orange-50":   Color.rgb(0xff, 0xf7, 0xed),
    "orange-100":  Color.rgb(0xff, 0xed, 0xd5),
    "orange-200":  Color.rgb(0xfe, 0xd7, 0xaa),
    "orange-300":  Color.rgb(0xfd, 0xba, 0x74),
    "orange-400":  Color.rgb(0xfb, 0x92, 0x3c),
    "orange-500":  Color.rgb(0xf9, 0x73, 0x16),
    "orange-600":  Color.rgb(0xea, 0x58, 0x0c),
    "orange-700":  Color.rgb(0xc2, 0x41, 0x0c),
    "orange-800":  Color.rgb(0x9a, 0x34, 0x12),
    "orange-900":  Color.rgb(0x7c, 0x2d, 0x12),
    "orange-950":  Color.rgb(0x43, 0x14, 0x07),
    "amber-50":    Color.rgb(0xff, 0xfb, 0xeb),
    "amber-100":   Color.rgb(0xfe, 0xf3, 0xc7),
    "amber-200":   Color.rgb(0xfd, 0xe6, 0x8a),
    "amber-300":   Color.rgb(0xfc, 0xd3, 0x4d),
    "amber-400":   Color.rgb(0xfb, 0xbf, 0x24),
    "amber-500":   Color.rgb(0xf5, 0x9e, 0x0b),
    "amber-600":   Color.rgb(0xd9, 0x77, 0x06),
    "amber-700":   Color.rgb(0xb4, 0x53, 0x09),
    "amber-800":   Color.rgb(0x92, 0x40, 0x0e),
    "amber-900":   Color.rgb(0x78, 0x35, 0x0f),
    "amber-950":   Color.rgb(0x45, 0x1a, 0x03),
    "green-50":    Color.rgb(0xf0, 0xfd, 0xf4),
    "green-100":   Color.rgb(0xdc, 0xfc, 0xe7),
    "green-200":   Color.rgb(0xbb, 0xf7, 0xd0),
    "green-300":   Color.rgb(0x86, 0xef, 0xac),
    "green-400":   Color.rgb(0x4a, 0xde, 0x80),
    "green-500":   Color.rgb(0x22, 0xc5, 0x5e),
    "green-600":   Color.rgb(0x16, 0xa3, 0x4a),
    "green-700":   Color.rgb(0x15, 0x80, 0x3d),
    "green-800":   Color.rgb(0x16, 0x65, 0x34),
    "green-900":   Color.rgb(0x14, 0x53, 0x2d),
    "green-950":   Color.rgb(0x05, 0x2e, 0x16),
  }

  def self.[](tailwind : Symbol)
    TW[tailwind]
  end

  def self.rgb(r : UInt8, g : UInt8, b : UInt8, a : UInt8 = 255)
    Color.new(*LCH.rgb2lch(r, g, b).map(&.to_i), a.to_i)
  end

  # Changes this color's brightness according to *percentage*.
  def brighten(percentage : Float64) : Color
    Color.new((l*percentage).clamp(0..360).to_i, c, h, a)
  end

  # Returns the corresponding SFML color.
  def sf : SF::Color
    SF::Color.new(*LCH.lch2rgb(l, c, h), a)
  end
end

class ::Hash
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
end

class List(T)
  def initialize(@head : List(T)?, @object : T)
  end

  def self.[](*objects : T)
    objects.reduce(nil) { |head, object| new(head, object) }
  end

  def self.build(objects : Enumerable(T))
    objects.reduce(nil) { |head, object| new(head, object) }
  end

  def self.add(a : Nil, b : T)
    List(T)[b]
  end

  def self.add(a : List(T), b : T)
    a.add(b)
  end

  def self.concat(a : Nil, b : Nil)
  end

  def self.concat(a : List(T), b : Nil) forall T
    a
  end

  def self.concat(a : Nil, b : List(T)) forall T
    b
  end

  def self.concat(a : List(T), b : List(T)) forall T, U
    a + b
  end

  def includes?(object : T) : Bool
    @object == object || @head.try(&.includes?(object)) || false
  end

  def prepend(other : List(T)) : List(T)
    List.new(@head.try(&.prepend(other)) || other, @object)
  end

  def +(other : List(T))
    other.prepend(self)
  end

  def add(object : T)
    List(T).new(self, object)
  end

  def size
    n = 0
    reverse_each { n += 1 }
    n
  end

  def last : T
    @object
  end

  def pop? : {List(T)?, T}
    {@head, @object}
  end

  def each(& : T ->)
    stack = Stack(T).new

    node = self
    while node
      stack << node.@object
      node = node.@head
    end

    stack.reverse_each do |object|
      yield object
    end
  end

  def reverse_each(& : T ->) : Nil
    node = self
    while node
      yield node.@object
      node = node.@head
    end
  end

  # Returns an enumerable wrapper over this source.
  def ee : Enumerable(T)
    EE(T).new(self)
  end

  # :nodoc:
  struct EE(T)
    include Enumerable(T)

    def initialize(@list : List(T))
    end

    def each(& : T ->) : Nil
      @list.each { |datum| yield datum }
    end
  end

  def reverse : List(T)
    list = nil
    reverse_each do |element|
      list = List.new(list, element)
    end
    list || self
  end

  def pretty_print(pp)
    index = 0
    pp.group(2, "list(", ")") do
      each do |object|
        if index > 0
          pp.text(",")
          pp.breakable
        end
        object.pretty_print(pp)
        index += 1
      end
    end
  end

  def inspect(io)
    io << "list("
    ee.join(io, ", ") do |el|
      el.inspect(io)
    end
    io << ")"
  end

  def to_s(io)
    inspect(io)
  end

  def_equals_and_hash @head, @object
end

struct Queue(T)
  def initialize(@front : List(T)? = nil, @rear : List(T)? = nil)
  end

  def self.[](*objects : T) : Queue(T)
    objects.reduce(Queue(T).new) { |q, object| q.add(object) }
  end

  def empty?
    @front.nil? && @rear.nil?
  end

  def add(object : T) : Queue(T)
    if rear = @rear
      Queue(T).new(@front, rear.add(object))
    else
      Queue(T).new(@front, List(T).new(nil, object))
    end
  end

  def shift? : {Queue(T), T}?
    if front = @front
      front, element = front.pop?
      return Queue.new(front, @rear), element
    end

    return unless rear = @rear

    Queue.new(rear.reverse).shift?
  end
end

class String
  def present? : Bool
    !empty?
  end

  def prefixed_by?(char : Char)
    size > 1 && starts_with?(char)
  end

  def postfixed_by?(char : Char)
    size > 1 && ends_with?(char)
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
      xs.pairs.each_entry { |k, v| commit.with(k, v) }
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
  def self.new(tuple : Tuple)
    join(*tuple)
  end

  private def self.join(v : Symbol)
    {% for member in @type.constants %}
      if v == {{member.underscore.symbolize}}
        return {{@type}}::{{member}}
      end
    {% end %}

    raise ArgumentError.new
  end

  private def self.join(v1, v2, *vs)
    join(v1) | join(v2, *vs)
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

class Memo(K, V)
  def initialize(@table = Pf::Map(K, V).new)
  end

  private def_change

  def []?(k : K)
    @table[k]?
  end

  def size
    @table.size
  end

  def assoc(k : K, v : V)
    change(table: @table.assoc(k, v))
  end

  def dissoc(k : K)
    change(table: @table.dissoc(k))
  end
end

macro memoized(method)
  {% itypes = method.args.map(&.restriction) %}
  {% otype = method.return_type %}

  @@%memo = Atomic(Memo({ {{itypes.splat}} }, {{otype}})).new(Memo({ {{itypes.splat}} }, {{otype}}).new)

  def {{ method.receiver ? "#{method.receiver}.".id : "".id }}__{{method.name}}({{method.args.splat}}) : {{method.return_type}}
    {{method.body}}
  end

  def {{ method.receiver ? "#{method.receiver}.".id : "".id }}{{method.name}}(*args) : {{method.return_type}}
    memo0 = @@%memo.get(:acquire)
    if value = memo0[args]?
      return value
    end
    if memo0.size > 128
      memo1 = Memo({ {{itypes.splat}} }, {{otype}}).new
    else
      memo1 = memo0
    end
    value = __{{method.name}}(*args)
    memo1 = memo1.assoc(args, value)
    _, _= @@%memo.compare_and_set(memo0, memo1, :relaxed, :relaxed)
    value
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

struct Slice(T)
  def starts_with?(other : Slice(T)) : Bool
    size >= other.size && self[0...other.size] == other
  end

  def prefixed_by?(other : Slice(T)) : Bool
    size > other.size && self[0...other.size] == other
  end

  def split(object : T)
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
end

class AssertionError < Exception
end

macro expect(x)
  raise AssertionError.new unless {{x}}
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

    if nanos < k1**1
      io << (nanos/k1**0).round(2) << "ns"
      return
    end

    if nanos < k1**2
      io << (nanos/k1**1).round(2) << "µs"
      return
    end

    if nanos < k1**3
      io << (nanos/k1**2).round(2) << "ms"
      return
    end

    if nanos < k1**4
      io << (nanos/k1**3).round(2) << "s"
      return
    end
  end

  def humanize
    String.build { |io| humanize(io) }
  end
end

struct Bag(T)
  def initialize
    @storage = {} of T => UInt16
  end

  protected def initialize(@storage)
  end

  # Returns the number of unique objects in this bag.
  def size
    @storage.size
  end

  # Returns the number of objects of which there is more than one occurrence in this bag.
  def nrepeats
    @storage.count { |_, tally| tally > 1 }
  end

  def tally?(object : T) : UInt16?
    @storage[object]?
  end

  def add(object : T) : Nil
    @storage[object] = (@storage[object]? || 0u16) + 1
  end

  def <<(object : T)
    add(object)
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
end

struct Range(B, E)
  def subrange_of?(other : Range) : Bool
    @begin.in?(other) && @end.in?(other)
  end

  def proper_subrange_of?(other : Range) : Bool
    subrange_of?(other) && size > other.size
  end
end

struct Int
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
