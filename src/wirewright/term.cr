module Ww
  class MathDomainError < Exception
  end

  # Lists the possible types of terms.
  enum TermType : UInt8
    # WARNING! This enum is assumed to contain at most 8 values (0-7)
    # by the rest of the code.

    Any     = 0
    Number  = 1
    String  = 2
    Symbol  = 3
    Boolean = 4
    Dict    = 5

    def self.parse(cls : Term::Num.class)
      TermType::Number
    end

    def self.parse(cls : Term::Str.class)
      TermType::String
    end

    def self.parse(cls : Term::Sym.class)
      TermType::Symbol
    end

    def self.parse(cls : Term::Boolean.class)
      TermType::Boolean
    end

    def self.parse(cls : Term::Dict.class)
      TermType::Dict
    end

    def self.parse(cls : Term.class)
      TermType::Any
    end

    def subtype?(other : TermType)
      other.any? || self == other
    end

    def blank : Term::Sym
      case self
      in .any?     then SYM_BLANK_ANY
      in .number?  then SYM_BLANK_NUMBER
      in .symbol?  then SYM_BLANK_SYMBOL
      in .string?  then SYM_BLANK_STRING
      in .boolean? then SYM_BLANK_BOOLEAN
      in .dict?    then SYM_BLANK_DICT
      end
    end
  end

  # TODO: remove in favor of Term::Any
  # TODO: refactor autocast to use AoT introspection + annotations instead of method_missing.
  #  Autocast is a huge wart on the face of the project the way it is implemented right now.
  #  You can get infinite loops, at runtime, out of nowhere, just because you've called the
  #  wrong method, and if you're lucky to get a compile error, it points to nowhere in particular.
  module ITerm
    def downcast
      self
    end

    def upcast : Term
      Term.of(self)
    end

    # Automatically upcasts `self` to `Term` and tries to run *call* on it.
    macro method_missing(call)
      {% unless Term.has_method?(call.name) %}
        {% raise "#{call}: no such method in Term, cannot automatically upcast" %}
      {% end %}

      # Oh Crystal gods, just add Def#callable_by?(Call) or something...
      {% candidate = Term.methods.find { |method| method.name == call.name && (method.args.size == call.args.size || method.splat_index) } %}
      {% unless candidate %}
        {% raise "#{call}: no such method in term, cannot automatically upcast" %}
      {% end %}

      {% if call.named_args && !candidate.double_splat %}
        {% raise "#{call}: giving named arguments to a double-splatless method is unsupported during automatic upcast" %}
      {% end %}

      upcast.{{call}}
    end
  end

  # All terms (including dictionary terms `Term::Dict`) are persistent, thread-safe,
  # and immutable.
  struct Term
    # This module implements the basics of type conversion using the `to?`, `to` methods.
    #
    # The `to` method is used to convert terms to Crystal objects of various types.
    #
    # Includers are expected to define `to?(type : T.class) : T?` for each T they
    # can be converted to.
    #
    # ```
    # x = Term["hello"]
    #
    # x            # => "hello" : Term::Str
    # x.to(String) # => "hello" : String
    # ```
    module TypeConversion
      # Converts this term instance to an object of the given *type*, if possible.
      # Returns `nil` if not.
      def to?(type : T.class) : T? forall T
      end

      macro included
        # Returns `self`.
        def to?(type : {{@type}}.class)
          self
        end
      end

      # Same as `to?`, but raises `TypeCastError` instead of returning `nil`.
      def to(type : T.class) : T forall T
        result = to?(type)
        if result.nil?
          raise TypeCastError.new
        end
        result
      end
    end

    # Implements equality methods for comparing term instances with `Term`s,
    # hashing of term instances, and cloning (noop).
    module Equality
      # NOTE: we eqcast with an upcast to allow Term to compare @mem before doing value
      # equality. See also: `Term#==`.

      def ==(other : Term) : Bool
        Term.of(self) == other
      end

      def ===(other : Term) : Bool
        Term.of(self) === other
      end

      # Support for hashing terms on the Crystal side.
      #
      # Delegates actual hashing to `Term.hashcode` to obtain a globally stable hash.
      #
      # WARNING: the stable hash is then hashed using the default Crystal hasher, which
      # is seeded randomly on startup. This means that this method will produce different
      # hashes across runs despite the same `Term.hashcode`. Use `Term.hashcode` directly
      # to avoid this.
      def hash(hasher)
        Term.hashcode(self).hash(hasher)
      end

      # Returns `self`, as all terms are immutable.
      def clone
        self
      end
    end

    # TODO: by being smarter with tagging we can cram many more term shapes in here.
    #
    # For instance:
    #  - `0` marks a pointer, leaving us with two bits `00` (4 pointer tags). Currently
    #    we only need three: dict, rational number, and string.
    #  - `1` marks an immediate (i.e. value type). Since we're a runtime, we can
    #    cram a huge lot of shapes in there structurally, at least those we can
    #    cheaply recognize; saving us some extra pointer hops we'd need otherwise.
    #    We can reserve 7 bits for a type and the rest of 7 bytes we'd have available
    #    as payload. Short strings (e.g. unicode codepoints), numbers of up to 7 bytes,
    #    booleans (only type -- true or false), and so on all go here.
    #  - Moreover, it's stupid to think of individual elements when it comes to optimization.
    #    One could imagine a TermArray that will use `1`-tagging and perhaps some kind of *mode*
    #    tagging as well, so that it can claim the 7 bytes for itself. Thus, we'd have stuff
    #    like compact strings stored immediately across multiple term-sized cells by TermArray.

    enum Tag : UInt8
      Dict    = 0u8 # << MUST be here
      NumRat  = 1u8
      NumInt  = 2u8
      Str     = 3u8
      Sym     = 4u8
      Boolean = 5u8
      # Left for future use: 6u8 7u8
    end

    # :nodoc:
    def initialize(@mem : Void*)
      if @mem.null?
        raise ArgumentError.new
      end
    end

    # Returns the underlying tagged pointer.
    def unsafe_repr : Void*
      @mem
    end

    # Constructs a generic `Term` instance from the given number *term*.
    def self.of(term : Num) : Term
      Term.new(Pointer(Void).new(term.@k.@mem.address))
    end

    # Downcasts this term to a number term without performing any checks.
    def unsafe_as_n : Num
      Num.new(Num::Kernel.new(@mem))
    end

    # Constructs a generic `Term` instance from the given string *term*.
    def self.of(term : Str) : Term
      Term.new(Pointer(Void).new(term.as(Void*).address | Tag::Str.value))
    end

    # Downcasts this term to a string term without performing any checks.
    def unsafe_as_s : Str
      Pointer(Void).new(@mem.address >> 3 << 3).as(Str)
    end

    # Constructs a generic `Term` from the given symbol *term*.
    def self.of(term : Sym) : Term
      data = term.@spec
      Term.new(Pointer(Void).new(((data << 3) | Tag::Sym.value).to_u64))
    end

    # Downcasts this term to a symbol term without performing any checks.
    def unsafe_as_sym : Sym
      data = @mem.address >> 3
      Sym.new(data.to_u32)
    end

    # Constructs a generic `Term` from the given boolean *term*.
    def self.of(term : Boolean) : Term
      if term.true?
        Term.new(Pointer(Void).new((1 << 3 | Tag::Boolean.value).to_u64))
      else
        Term.new(Pointer(Void).new(Tag::Boolean.value.to_u64))
      end
    end

    # Downcasts this term to a boolean term without performing any checks.
    def unsafe_as_b : Boolean
      Boolean.new((@mem.address >> 3) == 1)
    end

    # Constructs a generic `Term` from the given dictionary *term*.
    def self.of(term : Dict) : Term
      Term.new(term.as(Void*))
    end

    # Downcasts this term to a dictionary term without performing any checks.
    def unsafe_as_d : Dict
      @mem.as(Dict)
    end

    # Returns the pointer tag of this term.
    def tag : Tag
      Tag.new((@mem.address & 0b111).to_u8)
    end

    # Returns the `TermType` corresponding to this term. Guarantees to never
    # return `TermType::Any`.
    def type : TermType
      case tag
      in .num_int?, .num_rat? then TermType::Number
      in .str?                then TermType::String
      in .sym?                then TermType::Symbol
      in .boolean?            then TermType::Boolean
      in .dict?               then TermType::Dict
      end
    end

    # Returns `self`.
    def upcast : Term
      self
    end

    # Returns one of concrete structs corresponding to `Term`.
    def downcast : ITerm
      case tag
      in .num_int?, .num_rat? then unsafe_as_n
      in .str?                then unsafe_as_s
      in .boolean?            then unsafe_as_b
      in .sym?                then unsafe_as_sym
      in .dict?               then unsafe_as_d
      end
    end

    # Converts this term to an object of the given *type*, if possible.
    # Returns `nil` if not.
    def to?(type)
      return unless instance = downcast.as?(TypeConversion) # supports

      instance.to?(type)
    end

    # Converts this term to an object of the given *type*, if possible.
    # Raises `TypeCastError` if not.
    def to(type)
      to?(type) || raise TypeCastError.new
    end

    # Attempts to downcast this term to a number term. Returns `nil` if impossible.
    def as_n? : Num?
      case tag
      when .num_int?, .num_rat?
        unsafe_as_n
      end
    end

    # Attempts to downcast this term to a string term. Returns `nil` if impossible.
    def as_s? : Str?
      tag.str? ? unsafe_as_s : nil
    end

    # Attempts to downcast this term to a boolean term. Returns `nil` if impossible.
    def as_b? : Boolean?
      tag.boolean? ? unsafe_as_b : nil
    end

    # Attempts to downcast this term to a symbol term. Returns `nil` if impossible.
    def as_sym? : Sym?
      tag.sym? ? unsafe_as_sym : nil
    end

    # Attempts to downcast this term to a dictionary term. Returns `nil` if impossible.
    def as_d? : Dict?
      tag.dict? ? unsafe_as_d : nil
    end

    # Attempts to downcast this term to an itemsonly dictionary term. Returns
    # `nil` if impossible.
    def as_itemsonly_d? : Dict?
      return unless dict = as_d?
      return unless dict.itemsonly?

      dict
    end

    # Attempts to downcast this term to a pairsonly dictionary term. Returns
    # `nil` if impossible.
    def as_pairsonly_d? : Dict?
      return unless dict = as_d?
      return unless dict.pairsonly?

      dict
    end

    # Attempts to downcast this term to a nonempty dictionary term. Returns
    # `nil` if impossible.
    def as_nonempty_d? : Dict?
      return unless dict = as_d?
      return if dict.empty?

      dict
    end

    # Attempts to downcast this term to a dictionary term. Returns the dictionary's
    # itemspart if succeeded. Returns `nil` otherwise.
    def as_itemspart_d? : Dict?
      return unless dict = as_d?

      dict.itemspart
    end

    # Attempts to downcast this term to a dictionary term. Returns the dictionary's
    # pairspart if succeeded. Returns `nil` otherwise.
    def as_pairspart_d? : Dict?
      return unless dict = as_d?

      dict.pairspart
    end

    {% for method in %w[as_n as_s as_b as_sym as_d] %}
      # Same as `{{method.id}}?`, but raises `TypeCastError` with *detail*
      # instead of returning `nil`.
      def {{method.id}}(detail : String? = nil)
        {{method.id}}? || raise TypeCastError.new(detail)
      end

      # Map-like function to transform terms that downcast using `{{method.id}}?`
      # successfully. Other terms are returned unchanged.
      def {{method.id}}(&) : Term
        return self unless input = {{method.id}}?

        output = yield input
        output.upcast
      end
    {% end %}

    # Returns a dictionary where *key* is bound to `self`, followed by
    # key-value pairs from *rest* (if any).
    def pack(key, **rest) : Dict
      Term[**rest].with(key, self)
    end

    # Computes and returns the hexdigest of this term using the given *algorithm*.
    def hexdigest(*, algorithm : Digest = Digest::SHA256.new) : String
      digest = IO::Digest.new(IO::Empty.new, algorithm, mode: IO::Digest::DigestMode::Write)
      inspect(digest)
      digest.final.hexstring
    end

    # Writes a string representation of this term to *io*.
    def to_s(io)
      inspect(io)
    end

    def inspect(io)
      downcast.inspect(io)
    end

    # Returns `true` if this and *other* terms are equal by reference.
    def same?(other : Term) : Bool
      @mem == other.@mem
    end

    # :ditto:
    def same?(other : ITerm) : Bool
      same?(other.upcast)
    end

    # Returns `true` if this and *other* terms are equal.
    def ==(other : Term) : Bool
      @mem == other.@mem || downcast == other.downcast
    end

    # :ditto:
    def ==(other : ITerm) : Bool
      self == other.upcast # Gives a chance to compare @mem first
    end

    def hash(hasher)
      downcast.hash(hasher)
    end

    def clone : Term
      self
    end

    # Automatically downcasts `self` to one of `ITerm` includers and tries to
    # run *call* on it.
    macro method_missing(call)
      {% found_some = false %}
      {% return_types = [] of ::NoReturn %}

      pass do
        %instance = downcast
        {% for candidate in ITerm.includers %}
          {% for method in candidate.methods %}
            {% if method.name == call.name %}
              {% found_some = true %}
              {% if method.return_type %}
                {% return_types << method.return_type.resolve %}
              {% end %}
              if %instance.is_a?({{candidate}})
                break %instance.{{call}}
              end
            {% end %}
          {% end %}
        {% end %}

        {% return_types = return_types.uniq %}

        {% if found_some && !return_types.empty? && call.name.ends_with?("?") %} # Question-method
          {% if return_types == [::Bool] %}
            break false # Question method supposed to return bool, return false
          {% elsif return_types[0].nilable? %}
            break # Question method supposed to return nil, return nil
          {% end %}
        {% end %}

        raise TypeCastError.new("method {{call}} not found on #{%instance.class}")
      end

      {% unless found_some %}
        {% raise "no such method in any ITerm includer: #{call}, cannot autocast" %}
      {% end %}
    end
  end

  # Smart constructors

  struct Term
    # Downcasts `Term` for compatibility with other `[]` constructors.
    def self.[](object : Term) : ITerm
      object.downcast
    end

    def self.[](object : ITerm) : ITerm
      object
    end

    # Constructs a number term from the given number *object*.
    def self.[](object : Number) : Num
      Num.new(object)
    end

    # Constructs a string term from the given string view *object*.
    def self.[](object : StringView) : Str
      Str.new(object)
    end

    # Constructs a string term from the given string *object*.
    def self.[](object : String) : Str
      Term[object.view]
    end

    # Constructs a string term from the given character *object*.
    def self.[](object : Char) : Str
      Term[object.to_s]
    end

    # Constructs a symbol term from the given symbol *object*.
    def self.[](object : Symbol) : Sym
      Sym.new(object.to_s)
    end

    # Constructs a boolean term from the given boolean *object*.
    def self.[](object : Bool) : Boolean
      Boolean.new(object)
    end

    # Constructs a term from the given enum *object*.
    #
    # - `Issue::Severity` is encoded with a symbol.
    # - All other enums are encoded using their numeric value.
    def self.[](object : Enum) : Sym | Num
      case object
      when Issue::Severity
        case object
        when .note?   then Term[:note]
        when .minor?  then Term[:minor]
        when .major?  then Term[:major]
        when .severe? then Term[:severe]
        when .fatal?  then Term[:fatal]
        else
          raise ArgumentError.new("no term representation for severity #{object}")
        end
      else
        Num.new(object.value)
      end
    end

    {% for spec in { {:UUID, "UUID"}, {:H256, "256-bit term hash"}, {:Path, "path"} } %}
      {% type, name = spec %}

      # Constructs a string term from the given {{name.id}} *object*.
      def self.[](object : {{type.id}}) : Str
        Term[object.to_s]
      end
    {% end %}

    # Constructs an indexed dictionary from the given enumerable *object*.
    # Elements of *object* receive successive keys 0, 1, 2, etc.
    #
    # See also: `#with`.
    def self.[](object : Enumerable) : Dict
      Dict.build do |commit|
        object.each_with_index do |el, i|
          next if el.nil?
          commit.with(i, el)
        end
      end
    end

    {% for cls in %w(Hash NamedTuple) %}
      # Constructs a dictionary from the given hash or named tuple *object*.
      #
      # See also: `#with`.
      def self.[](object : {{cls.id}}) : Dict
        Dict.build do |commit|
          object.each do |k, v|
            next if v.nil?
            commit.with(k, v)
          end
        end
      end
    {% end %}

    # Constructs a dictionary from the given `JSON::Any` *object*.
    #
    # Raises `ArgumentError` on `null`.
    def self.[](object : JSON::Any) : ITerm
      if (value = object.as_f? || object.as_s? || object.as_a? || object.as_h? || object.as_bool?).nil?
        raise ArgumentError.new
      end

      Term[value]
    end

    # Passes `nil` through so you can safely construct off nilable types
    # and get a nilable term as the result.
    def self.[](object : Nil) : Nil
    end

    # :nodoc:
    def self.[](object)
      raise TypeCastError.new("cannot cast object of type #{object.class} to Term")
    end

    # Constructs a dictionary (see `dict`).
    def self.[](*items, **entries) : Dict
      dict(*items, **entries)
    end

    # Constructs an empty dictionary.
    def self.dict : Dict
      Dict::EMPTY
    end

    # Constructs a dictionary that contains both *items* (each receiving successive
    # keys 0, 1, 2, etc.), and *entries*.
    def self.dict(*items, **entries) : Dict
      Dict.build do |commit|
        items.each_with_index do |el, i|
          next if el.nil?
          commit.with(i, el)
        end
        entries.each do |k, v|
          next if v.nil?
          commit.with(k, v)
        end
      end
    end

    # Shorthand for `of(dict(*args, **kwargs))`.
    def self.of_dict(*args, **kwargs) : Term
      of(dict(*args, **kwargs))
    end

    # Constructs a dict set containing terms from the given enumerable *ee*.
    def self.set(ee : Enumerable(_))
      Dict.build do |commit|
        ee.each { |arg| commit.with(arg, true) }
      end
    end

    # Constructs a dict set containing the terms provided in *args*.
    def self.set(*args) : Dict
      set(args)
    end

    # Constructs a dict; each tuple in *args* provides an object for the key followed
    # by one for the value. The resulting dict is also extended with **kwargs**, if they
    # are provided.
    def self.entries(*args : {_, _}, **kwargs) : Dict
      Dict.build do |commit|
        args.each { |key, value| commit.with(key, value) }
        kwargs.each { |key, value| commit.with(key, value) }
      end
    end

    # Shorthand for `of(entries(*args, **kwargs))`.
    def self.of_entries(*args, **kwargs) : Term
      of(entries(*args, **kwargs))
    end

    # Same as `.[]` but upcasts to generic `Term` for you.
    def self.of(*args, **kwargs)
      Term[*args, **kwargs].try(&.upcast)
    end
  end

  # Hashing

  struct Term
    # Term hasher object, similar in purpose to `Crystal::Hasher`.
    #
    # Currently implements 64-bit Fowler–Noll–Vo hash function.
    #
    # Wirewright is assumed to run on x86-64 only. This means system-endian is
    # little-endian. Under this assumption we say that the hash is *globally
    # stable*, meaning it stays the same across runs and machines for
    # equal (or colliding!) values.
    #
    # Global stability is explicitly implemented despite susceptibility to
    # HashDoS etc. This is because Wirewright's Terms are for use in a purely
    # functional setting; randomly seeded hash functions lead to different dict
    # entry order per run/machine => different return result for `(hashcode term_)`
    # in particular, which we would consider as an implementation error.
    #
    # Note that due to hash collisions and the way symbols are implemented right now,
    # dictionaries have a special ordered variant of their `each_entry`, namely
    # `each_entry_ord`, that one must use if one wants to do globally stable, ordered
    # pretty printing or entry iteration. This is because the moment we have a collision,
    # the hash function is of no use ordering entries. And with symbols -- the way we
    # implement them right now for efficiency -- their hash code roughly depends on
    # the time they were created at / the order in which they were created, which is
    # obviously globally indeterminate.
    #
    # WARNING: `Hasher` is a mutable struct. Pass it around with care.
    struct Hasher
      # Reference: https://softwareengineering.stackexchange.com/a/145633

      # :nodoc:
      FNV_OFFSET_BASIS = 14695981039346656037u64
      # :nodoc:
      FNV_PRIME = 1099511628211u64

      def initialize
        @state = FNV_OFFSET_BASIS
      end

      # Breaks up *object* into constituent bytes.
      def blast(object : UInt8, & : UInt8 ->) : Nil
        yield object
      end

      # :ditto:
      def blast(object : UInt32, & : UInt8 ->) : Nil
        bytes = object.unsafe_as(StaticArray(UInt8, 4))
        bytes.each { |byte| yield byte }
      end

      # :ditto:
      def blast(object : Int32, & : UInt8 ->) : Nil
        blast(object.to_u32) { |byte| yield byte }
      end

      # :ditto:
      def blast(object : UInt64, & : UInt8 ->) : Nil
        bytes = object.unsafe_as(StaticArray(UInt8, 8))
        bytes.each { |byte| yield byte }
      end

      # :ditto:
      def blast(object : Float64, & : UInt8 ->) : Nil
        blast(object.unsafe_as(UInt64)) { |byte| yield byte }
      end

      # :ditto:
      def blast(object : Enum, & : UInt8 ->) : Nil
        blast(object.value) { |byte| yield byte }
      end

      # Returns the hashcode built so far.
      def result : UInt64
        @state
      end

      # Incorporates *object* into the hash. See `blast` for a list of
      # supported types.
      def append(object) : Nil
        blast(object) do |byte|
          @state ^= byte
          @state &*= FNV_PRIME
        end
      end

      # Alias of `append`.
      def <<(object) : Nil
        append(object)
      end
    end

    # Returns the hashcode of *object*. See `hashcode(hasher, object)` overloads
    # to learn about supported types of *object*s.
    def self.hashcode(object) : UInt64
      hasher = hashcode(Hasher.new, object)
      hasher.result
    end

    # Fast path for itemspart keys (indices).
    def self.hashcode(hasher : Hasher, object : Int32) : Hasher
      hasher << object
      hasher
    end

    # Appends the hash of a symbol term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term::Sym) : Hasher
      hasher << TermType::Symbol
      hasher << object.@spec
      hasher
    end

    # Appends the hash of a string term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term::Str) : Hasher
      hasher << TermType::String
      object.each_byte do |byte|
        hasher << byte
      end
      hasher
    end

    # Appends the hash of a number term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term::Num) : Hasher
      hasher << TermType::Number
      hasher << object.to_f64 # ?!
      hasher
    end

    # Appends the hash of a boolean term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term::Boolean) : Hasher
      hasher << TermType::Boolean

      if object.true?
        hasher << 1u8
      else
        hasher << 0u8
      end

      hasher
    end

    # :nodoc:
    HASHCODE_DICT_TYPE = begin
      hasher = Hasher.new
      hasher << TermType::Dict
      hasher.result
    end

    # Appends the hash of a dict term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term::Dict) : Hasher
      hashcode = object.hashcode do
        memo = HASHCODE_DICT_TYPE

        object.each_entry do |key, value|
          pair_hasher = Hasher.new
          pair_hasher = hashcode(pair_hasher, key)
          pair_hasher = hashcode(pair_hasher, value)
          memo &+= pair_hasher.result
        end

        memo
      end

      hasher << hashcode
      hasher
    end

    # Appends the hash of a term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term) : Hasher
      hashcode(hasher, object.downcast)
    end
  end

  # Digest

  struct Term
    # Represents a 256-bit hash of a term using four 64-bit blocks.
    record H256, blk0 : UInt64, blk1 : UInt64, blk2 : UInt64, blk3 : UInt64 do
      ALGORITHM = Digest::Blake3

      # :nodoc:
      def self.new(term : Term)
        digest = ALGORITHM.new
        io = IO::ByteStream.new { |slice| digest.update(slice) }

        ML.compact(io, term)

        scratch = uninitialized UInt8[32]
        blks = scratch.to_slice.unsafe_slice_of(UInt64)

        digest.final(scratch.to_slice)

        H256.new(blks[0], blks[1], blks[2], blks[3])
      end

      # Ordered combination of *hashes*.
      def self.combine(hashes : Enumerable(H256)) : H256
        digest = ALGORITHM.new

        scratch = uninitialized UInt8[32]
        blks = scratch.to_slice.unsafe_slice_of(UInt64)

        hashes.each do |hash|
          blks[0] = hash.blk0
          blks[1] = hash.blk1
          blks[2] = hash.blk2
          blks[3] = hash.blk3
          digest.update(scratch.to_slice)
        end

        digest.final(scratch.to_slice)

        H256.new(blks[0], blks[1], blks[2], blks[3])
      end

      # Returns an indexable of blocks `blk0-3`.
      def blks : Indexable(UInt64)
        {blk0, blk1, blk2, blk3}
      end

      # Returns an indexable of blocks `blk0-1` as `UInt128`s.
      def blks128 : Indexable(UInt128)
        scratch = uninitialized UInt128[2]

        blks = scratch.to_slice.unsafe_slice_of(UInt64)
        blks[0] = blk0
        blks[1] = blk1
        blks[2] = blk2
        blks[3] = blk3

        {scratch[0], scratch[1]}
      end

      def <=>(other : H256)
        blks <=> other.blks
      end

      def inspect(io)
        io << "H256("
        to_s(io)
        io << ")"
      end

      def to_s(io)
        Alpha48.encode(io, blk0)
        io << "-"
        Alpha48.encode(io, blk1)
        io << "-"
        Alpha48.encode(io, blk2)
        io << "-"
        Alpha48.encode(io, blk3)
      end
    end

    # Returns the 256-bit hash of *term* calculated using `H256::ALGORITHM`.
    def self.hashcode256(term : Term | ITerm) : H256
      H256.new(Term.of(term))
    end
  end

  # Pattern matching

  struct Term
    def self.matches(pattern, matchee, *, engine : Engine.class = M1, env = Term[]) : Array(Term::Dict) forall Engine
      engine.matches(Term.of(pattern), Term.of(matchee), env: env)
    end

    # :nodoc:
    MATCHERS = SyncCache(UInt32, Case::Matcher).new(capacity: 512, preallocate: true)

    # Advanced: Direct form of `Term.case` allowing explicit control over the matcher
    # instance and the initial environment.
    #
    # See also: `Case.defcase`, `Case.scan`.
    macro case(matchee, *, matcher, env = Term[], &block)
      {{@type}}::Case.scan({{@type}}::MATCHERS, {{matcher}}, Term.of({{matchee}}), {{env}}) {{block}}
    end

    # Advanced: Lets you pick an engine explicitly (e.g. `M0`, `M1`), constructing
    # an appropriate matcher
    macro case(matchee, *, engine, **kwargs, &block)
      {{@type}}.case({{matchee}}, matcher: {{@type}}::Case::MM({{engine}}), {{kwargs.double_splat}}) {{block}}
    end

    # Structural pattern matching DSL over `Term`s.
    #
    # `case` expands to efficient pattern matching code that uses one of the matchers
    # provided by `Case` (see `Case::Matcher`).
    #
    # - *matchee* is the term to match.
    # - *env* can be provided as a keyword argument, to be used as the initial
    #   match env for all patterns.
    #
    # ```
    # def calc(expr : Term) : Term
    #   Term.of_case(expr) do
    #     matchpiT %{(+ a←(%number i32) b←(%number i32))} do
    #       typeof(a) # => Int32
    #       typeof(b) # => Int32
    #       a + b
    #     end
    #     matchpiT %{(- a←(%number i32) b←(%number i32))} { a - b }
    #     matchpiT %{(* a←(%number i32) b←(%number i32))} { a * b }
    #     matchpiT %{(/ a←(%number i32) b←(%number i32))} { a // b }
    #     otherwise { expr }
    #   end
    # end
    #
    # calc(ML.term(%{(+ 1 2)})) # => 3
    # calc(ML.term(%{(- 1 2)})) # => -1
    # calc(ML.term(%{(* 2 2)})) # => 4
    # calc(ML.term(%{(/ 4 2)})) # => 2
    #
    # calc(ML.term(%{abc})) # => abc
    # ```
    #
    # See `Case` for details on syntax.
    macro case(matchee, **kwargs, &block)
      {{@type}}.case({{matchee}}, matcher: {{@type}}::Case::MM1, {{kwargs.double_splat}}) {{block}}
    end

    # A shorthand for wrapping `Term.case` in `Term.of`.
    macro of_case(*args, **kwargs, &block)
      {{@type}}.of({{@type}}.case({{args.splat}}, {{kwargs.double_splat}}) {{block}})
    end

    # Shorthand for a single-`matchpi` call to `Term.case`:
    #
    # ```
    # Term.case(term, **kwargs) do
    #   matchpi *patterns do
    #     # Block
    #   end
    #
    #   otherwise { }
    # end
    # ```
    macro matchpi?(term, *patterns, **kwargs, &block)
      {{@type}}.case({{term}}, {{kwargs.double_splat}}) do
        matchpi({{patterns.splat}}) {{block}}
        otherwise { }
      end
    end

    # Shorthand for a single-`matchpi` call to `Term.case`:
    #
    # ```
    # Term.case(term, **kwargs) do
    #   matchpi *patterns do
    #     # Block
    #   end
    # end
    # ```
    macro matchpi(term, *patterns, **kwargs, &block)
      {{@type}}.case({{term}}, {{kwargs.double_splat}}) do
        matchpi({{patterns.splat}}) {{block}}
      end
    end

    # Shorthand for a single-`givenpi` call to `Term.case`:
    #
    # ```
    # Term.case(term, **kwargs) do
    #   givenpi *patterns do
    #     # Block
    #   end
    #
    #   otherwise { }
    # end
    # ```
    macro givenpi?(term, *patterns, **kwargs, &block)
      {{@type}}.case({{term}}, {{kwargs.double_splat}}) do
        givenpi({{patterns.splat}}) {{block}}
        otherwise { }
      end
    end

    # Shorthand for a single-`givenpi` call to `Term.case`:
    #
    # ```
    # Term.case(term, **kwargs) do
    #   givenpi *patterns do
    #     # Block
    #   end
    # end
    # ```
    macro givenpi(term, *patterns, **kwargs, &block)
      {{@type}}.case({{term}}, {{kwargs.double_splat}}) do
        givenpi({{patterns.splat}}) {{block}}
      end
    end
  end

  # Utilities

  struct Term
    def self.merge(a : Term, b : Term, *cs : Term) : Term
      cs.reduce(merge(a, b)) { |memo, x| merge(memo, x) }
    end

    # :ditto:
    def self.merge(a : ITerm, b : ITerm, *cs : ITerm) : ITerm
      cs.reduce(merge(a, b)) { |memo, x| merge(memo, x) }
    end

    def self.merge(a : Term, b : Term) : Term
      Term.of(merge(a.downcast, b.downcast))
    end

    # Deep merge.
    #
    # Merges this dictionary with a *newer* one. If two keys are equal and both
    # values are dictionaries, merging descends recursively. Otherwise, *newer*'s
    # value is preferred.
    def self.merge(a : ITerm, b : ITerm) : ITerm
      case {a, b}
      when {Dict, Dict}
        # TODO: move to a separate method once ITerm is removed in favor of Term::Any.
        # Crystal dispatch is being stupid on this and is upcasting the more specific
        # Dict restriction to ITerm, which leads to invalid behavior.
        #
        # The doc belongs to the Dict,Dict method as well! Everything else is sugar,
        # more or less.
        return b if a.empty?
        return a if b.empty?

        # Don't waste on singleton dicts.
        if a.size == 1
          k, v0 = a.ee.first
          unless v1 = b[k]?
            return b.with(k, v0)
          end
          unless (v0d = v0.as_d?) && (v1d = v1.as_d?)
            return b
          end
          return b.with(k, merge(v0d, v1d))
        end

        if b.size == 1
          k, v1 = b.ee.first
          unless (v0 = a[k]?) && (v0d = v0.as_d?) && (v1d = v1.as_d?)
            return a.with(k, v1)
          end
          return a.with(k, merge(v0d, v1d))
        end

        # Use commits otherwise.
        if a.size < b.size
          b.transaction do |commit|
            a.each_entry do |k, v0|
              unless v1 = b[k]?
                commit.with(k, v0)
                next
              end

              next unless v0d = v0.as_d?
              next unless v1d = v1.as_d?

              commit.with(k, merge(v0d, v1d))
            end
          end
        else
          a.transaction do |commit|
            b.each_entry do |k, v1|
              if (v0 = a[k]?) && (v0d = v0.as_d?) && (v1d = v1.as_d?)
                commit.with(k, merge(v0d, v1d))
                next
              end

              commit.with(k, v1)
            end
          end
        end
      else
        b
      end
    end

    # Returns a copy of the dict *a* with all of *keys* removed. Missing keys are
    # skipped. Raises `TypeCastError` if *a* is not a dict. Upcasts the result
    # back to `Term`.
    def self.exclude(a : Term, keys : Enumerable(Term)) : Term
      Term.of(exclude(a.as_d, keys))
    end

    # Returns a copy of the dict *a* with all of *keys* removed. Missing keys
    # are skipped.
    def self.exclude(a : Dict, keys : Enumerable(Term)) : Dict
      a.transaction do |commit|
        keys.each { |key| commit.without(key) }
      end
    end
  end

  # Morph API
  #
  # TODO: Morph API should gradually replace the zoo of inconsistent keypath-
  # following methods such as `follow`, `where`, and `morph` that we have on Dict
  # at the moment.
  #
  #  Morph is really a language for describing how to reach a leaf and how to rewrite it.
  #
  #  <expr>
  #    <op>
  #    Op(<leaf>)
  #    Seq(<op>+)
  #      E.g. Seq(Op(Item(0), AfterLast, One(100)), Op(Item(1), AfterLast, One(200)))
  #
  #  <op>
  #    Op(<step>+, <leaf>)
  #
  #  <step>
  #    Item(index : Int32)
  #    Key(term)
  #    Last()
  #    AfterLast()
  #
  #  <leaf>
  #    Zero()
  #    One(term)
  #    Many(term list)
  #    Map(fn)
  #

  struct Term
    module Last
    end

    module AfterLast
    end

    alias Anchor = Last.class | AfterLast.class

    module Absent
    end

    alias Action = Absent.class

    def self.morph(term root : Term::Dict, keypath : Indexable, & : Term -> Term | Action) : Term::Dict
      stack = [] of {Term::Dict, Term}
      tip = Term.of(root)

      keypath.each do |key|
        unless tip.type.dict?
          # Abort, keypath points into something we can't deal with.
          return root
        end

        node = tip.unsafe_as_d

        case key
        in Term
        in Last.class
          key = Term.of(node.itemsize - 1)
        in AfterLast.class
          key = Term.of(node.itemsize)
        end

        unless value = node[key]?
          # Abort, value does not exist.
          return root
        end

        stack << {node, key}
        tip = value
      end

      if stack.empty?
        # Keypath is empty. Abort, nothing to do. This is the Dict overload, so
        # we cannot run the block on root itself.
        return root
      end

      tip = yield tip

      while entry = stack.pop?
        parent, key = entry

        case tip
        in Term
          tip = Term.of(parent.with(key, tip))
        in Absent.class
          tip = Term.of(parent.without(key))
        end
      end

      tip.as(Term).unsafe_as_d
    end

    def self.morph(term : Term, keypath : Indexable, &) : Term
      if keypath.empty?
        result = yield term
        if result.is_a?(Action)
          raise ArgumentError.new("cannot execute action on the toplevel term")
        end
        return result
      end

      term.as_d do |dict|
        morph(dict, keypath) { |value| yield value }
      end
    end

    def self.morph(term, *args, &)
      morph(term, args.map { |arg| arg.is_a?(Anchor) ? arg : Term.of(arg) }) do |leaf|
        result = yield leaf
        unless result.is_a?(Action)
          result = Term.of(result)
        end
        result
      end
    end

    # TODO: block-less morph on dict (creates intermediate dicts if missing)

    # TODO: block morph on term
    # TODO: block-less morph on term (creates intermediate dicts if missing)

    # TODO: instead of Enumerable(Term) work on an enumerable which can include sentinels.
    #     Sentinel: first and last item of dict
    #
    # TODO: Allow block variants and setter variants to return sentinel for
    # removal and recursive removal (two different sentinels). The dict morph
    # API does'nt do this cleanly: it only supports nils, and removes recursively
    # on them.

    # Executes a sequence of `morph` *steps* on *term*. Each step is a tuple
    # of arguments to `morph`.
    def self.morphseq(term, *steps : Tuple)
      steps.reduce(term) do |memo, step|
        Term.morph(memo, *step)
      end
    end

    # Executes a sequence of `morph` *steps* on *term*, using the same block
    # for all `morph`s.
    def self.morphseq(term, *steps : Tuple, &)
      steps.reduce(term) do |memo, step|
        Term.morph(memo, *step) { |leaf| yield leaf }
      end
    end
  end

  # Misc

  struct Term
    # WARNING: keypaths are contained within a mutable `Array` for memory
    # efficiency; you do not own the stack, for you the stack is read-only! Do not
    # mutate the key path stack, instead, make a copy of it (`dup`) and mutate
    # your copy instead. Or if you know what you're doing, make sure to return
    # the stack to valid condition after you've modified it.

    def self.each_node(root : Term, & : Term -> Bool) : Nil
      ns = Pf::Kit::HybridArray(Int32, 32).new
      nodes = Pf::Kit::HybridArray(Term, 32){root}

      while node = nodes.pop?
        descend = yield node

        # Descend
        if descend && (dict = node.as_d?) && dict.size > 0
          key, value = dict.nth(0)
          ns << 0
          nodes << node
          nodes << value
          next
        end

        # Ascend
        loop do
          return unless parent = nodes.pop?

          n = ns.pop
          next unless successor = parent.nth?(n + 1)

          key, value = successor
          ns << n + 1
          nodes << parent
          nodes << value
          break
        end
      end
    end

    # Traversal proceeds left-to-right, parent before children. *root* is
    # yielded first.
    def self.each_keypath_and_node(root : Term, & : Array(Term), Term -> Bool) : Nil
      ns = Pf::Kit::HybridArray(Int32, 32).new
      nodes = Pf::Kit::HybridArray(Term, 32){root}
      keypath = [] of Term

      while node = nodes.pop?
        descend = yield keypath, node

        # Descend
        if descend && (dict = node.as_d?) && dict.size > 0
          key, value = dict.nth(0)
          ns << 0
          nodes << node
          nodes << value
          keypath << key
          next
        end

        # Ascend
        loop do
          return unless parent = nodes.pop?

          n = ns.pop
          keypath.pop
          next unless successor = parent.nth?(n + 1)

          key, value = successor
          ns << n + 1
          nodes << parent
          nodes << value
          keypath << key
          break
        end
      end
    end

    # Yields each keypath and item node starting at *root*.
    #
    # The block should return `true` if it wishes to continue descent; `false`
    # if it wishes to skip descent.
    #
    # Traversal proceeds left-to-right, parent before children. *root* is
    # yielded first.
    def self.each_keypath_and_itemnode(root : Term, & : Array(Term), Term -> Bool) : Nil
      nodes = [root]
      keypath = [] of Term

      while node = nodes.pop?
        descend = yield keypath, node

        # Descend
        if descend && (dict = node.as_d?) && (dict.itemsize > 0)
          keypath << Term.of(0)
          nodes << node << dict[0]
          next
        end

        # Ascend
        loop do
          return unless parent = nodes.pop?

          index = keypath.pop
          next unless successor = parent[index + 1]?

          nodes << parent << successor
          keypath << Term.of(index + 1)
          break
        end
      end
    end

    def self.each_keypath_and_leaf(root : Term, & : Array(Term), Term -> Bool) : Nil
      each_keypath_and_node(root) do |keypath, node|
        if (dict = node.as_d?) && !dict.empty?
          next true # descend
        end

        # Either non-dict or empty dict -- leaves.
        return unless yield keypath, node

        false # no descend
      end
    end

    def self.each_keypath_and_item_leaf(root : Term, & : Array(Term), Term -> Bool) : Nil
      each_keypath_and_itemnode(root) do |keypath, node|
        if (dict = node.as_d?) && !dict.empty?
          next true # descend
        end

        # Either non-dict or empty dict -- leaves.
        return unless yield keypath, node

        false # no descend
      end
    end

    # Traversal proceeds left-to-right, children before parents. *root* is
    # yielded last.
    def self.each_keypath_bottom_up(root : Term, & : Array(Term) ->) : Nil
      nodes = [root]
      keypath = [] of Term

      while node = nodes.pop?
        # Descend
        if (dict = node.as_d?) && (dict.itemsize > 0)
          keypath << Term.of(0)
          nodes << node << dict[0]
          next
        end

        yield keypath

        # Ascend
        loop do
          return unless parent = nodes.pop?

          index = keypath.pop
          unless successor = parent[index + 1]?
            yield keypath
            next
          end

          nodes << parent << successor
          keypath << Term.of(index + 1)
          break
        end
      end
    end

    def self.ancestors(root : Term, keypath : Indexable(Term)) : Array(Term)
      ancestors = Array(Term).new(keypath.size - 1)
      parent = root

      keypath.each do |key|
        ancestors << parent
        parent = parent[key]
      end

      ancestors
    end
  end

  # TODO: convert these to iterative and use blocks!!!

  struct Term
    module Patch
      alias Any = Skip | ReplaceSkip | ReplaceDescend

      record Skip
      record ReplaceSkip, rep : Term
      record ReplaceDescend, rep : Term
    end

    # Thoroughly visits all nodes in the subtree of *term*, yielding each node
    # to the block for replacement. If the block returns `nil`, the node is
    # left unchanged; traversal will continue to its subtree, if any.
    #
    # NOTE: If replacement occurs within a key, and it collides, with an existing
    # key, the replacement's value is preferred over the existing key's.
    def self.patch(term : Term, &fn : Term -> Patch::Any) : Term
      case response = fn.call(term)
      in Patch::Skip
      in Patch::ReplaceDescend
        term = response.rep
      in Patch::ReplaceSkip
        return response.rep
      end

      unless dict = term.as_d?
        return term
      end

      dict1 = dict
      dict.each_entry do |key0, value0|
        key1 = patch(key0, &fn)
        value1 = patch(value0, &fn)

        # Key changed
        unless key0.same?(key1)
          dict1 = dict1.without(key0).with(key1, value1)
          next
        end

        # Value changed
        unless value0.same?(value1)
          dict1 = dict1.with(key1, value1)
          next
        end
      end

      Term.of(dict1)
    end

    # Thoroughly traverses all nodes in the subtree of *term*, and yields them
    # to the block.
    #
    # *Thoroughly* here means that both keys and values are visited recursively
    # (vs. e.g. `each_keypath_and_leaf` which visits values only).
    #
    # Dictionaries are emitted before their entries. For each entry, first, its
    # key is visited recursively; then, its value is visited recursively.
    def self.each_leaf_thorough(term : Term, & : Term ->) : Nil
      state_initial = -1 # const

      stack = [{state: state_initial, term: term}]

      while rec = stack.pop?
        case rec[:state]
        when state_initial
          yield rec[:term]
          next unless rec[:term].type.dict?

          stack << {state: 0, term: rec[:term]}
        else
          # If state is zero or positive = N, this means nth(N) on the term
          # and an assertion that term is a dict.
          dict = rec[:term].unsafe_as_d
          next unless entry = dict.nth?(rec[:state])

          key, value = entry
          stack << {state: rec[:state] + 1, term: rec[:term]}
          stack << {state: state_initial, term: value}
          stack << {state: state_initial, term: key}
        end
      end
    end

    # Yields keypath and *itemspart stem* into the given *root*.
    #
    # A *stem* is an itemsonly dict whose last item is a node from the root's subtree;
    # prefixed by its ancestor nodes all the way up to, but not including the root itself.
    #
    # Stems are used to turn tree traversal into declarative pattern matching.
    #
    # An *itemspart stem* is a *stem* restricted to the itemsparts of *root*'s subtree.
    def self.each_itemspart_stem(root : Term::Dict, & : Term::Dict, Term::Dict -> Bool)
      queue = Deque{ {Term[], Term[]} }

      while entry = queue.shift?
        keypath, stem = entry

        if keypath.empty?
          root.each_item_with_index do |item, index|
            queue << {Term[{index}], stem.append(item)}
          end
        elsif yield keypath, stem.append(:node)
          next unless tip = stem.items.last.as_d?

          l = Term[]
          r = tip.itemspart

          while item = r.items.first?
            if yield keypath, stem.append({l, :children, r})
              # Block says to descend into rest.
              break
            end

            l = l.append(item)
            r = r.items.move(1).collect # FIXME: gosh this is inefficient!!
          end

          r.each_item_with_index do |item, index|
            queue << {keypath.append(l.size + index), stem.append(item)}
          end
        end
      end
    end
  end
end

require "./term/num"
require "./term/str"
require "./term/sym"
require "./term/boolean"
require "./term/dict"
require "./term/case"
