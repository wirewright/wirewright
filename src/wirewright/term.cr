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

  module ITerm
    def to_json(builder : JSON::Builder)
      upcast.to_json(builder)
    end

    # Attempts to convert this term to the given Crystal *type*. If unsuccessful,
    # returns `nil`.
    def to?(type : T.class) : T? forall T
    end

    macro included
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

    def downcast
      self
    end

    def upcast : Term
      Term.of(self)
    end

    def &(newer : ITerm) : ITerm
      newer
    end

    def &-(other : ITerm) : ITerm?
      self == other ? nil : self
    end

    def ==(other : Term) : Bool
      self == other.downcast
    end

    def ===(other : Term) : Bool
      self === other.downcast
    end

    # Writes a string representation of this term to *io*.
    def to_s(io)
      inspect(io)
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

  # Terms are thread-safe values, the primary operand in Wirewright. Sometimes
  # they are passed around as comprehensible *messages*, sometimes they are
  # serialized, packed and sent over the network, sometimes indexed, and sometimes
  # manipulated as data. Terms are also the code and data for WwML, a language
  # with S-expression-like syntax used to program nodes.
  #
  # All terms (including dictionary terms `Term::Dict`) are persistent, thread-safe,
  # and immutable.
  struct Term
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
    end

    def unsafe_repr : Void*
      @mem
    end

    # Constructs a generic `Term` instance from the given number *term*.
    def self.of(term : Num) : Term
      Term.new(Pointer(Void).new(term.@k.@mem.address))
    end

    # :nodoc:
    def unsafe_as_n : Num
      Num.new(Num::Kernel.new(@mem))
    end

    # Constructs a generic `Term` instance from the given string *term*.
    def self.of(term : Str) : Term
      Term.new(Pointer(Void).new(term.@value.as(Void*).address | Tag::Str.value))
    end

    # :nodoc:
    def unsafe_as_s : Str
      Str.new(Pointer(Void).new(@mem.address >> 3 << 3).as(String))
    end

    # Constructs a generic `Term` from the given symbol *term*.
    def self.of(term : Sym) : Term
      data = term.@spec
      Term.new(Pointer(Void).new(((data << 3) | Tag::Sym.value).to_u64))
    end

    # :nodoc:
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

    # :nodoc:
    def unsafe_as_b : Boolean
      Boolean.new((@mem.address >> 3) == 1)
    end

    # Constructs a generic `Term` from the given dictionary *term*.
    def self.of(term : Dict) : Term
      Term.new(term.as(Void*))
    end

    # :nodoc:
    def unsafe_as_d : Dict
      @mem.as(Dict)
    end

    # :nodoc:
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

    def to(type : T.class) : T forall T
      downcast.to(type)
    end

    def as_n? : Num?
      case tag
      when .num_int?, .num_rat?
        unsafe_as_n
      end
    end

    def as_s? : Str?
      tag.str? ? unsafe_as_s : nil
    end

    def as_b? : Boolean?
      tag.boolean? ? unsafe_as_b : nil
    end

    def as_sym? : Sym?
      tag.sym? ? unsafe_as_sym : nil
    end

    def as_d? : Dict?
      tag.dict? ? unsafe_as_d : nil
    end

    def as_itemsonly_d? : Dict?
      return unless tag.dict?
      dict = unsafe_as_d
      return unless dict.itemsonly?
      dict
    end

    # Attempts to cast this term into a number term `Term::Num`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_n : Num
      case tag
      when .num_int?, .num_rat?
        unsafe_as_n
      else
        raise TypeCastError.new
      end
    end

    # Attempts to cast this term into a string term `Term::Str`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_s : Str
      tag.str? ? unsafe_as_s : raise TypeCastError.new
    end

    # Attempts to cast this term into a boolean term `Term::Boolean`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_b : Boolean
      tag.boolean? ? unsafe_as_b : raise TypeCastError.new
    end

    # Attempts to cast this term into a symbol term `Term::Sym`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_sym : Sym
      tag.sym? ? unsafe_as_sym : raise TypeCastError.new
    end

    # Attempts to cast this term into a dictionary term `Term::Dict`.
    #
    # Raises `TypeCastError` if the cast cannot be performed.
    def as_d : Dict
      tag.dict? ? unsafe_as_d : raise TypeCastError.new
    end

    # Returns a dictionary where *key* is bound to `self`, followed by
    # key-value pairs from *rest* (if any).
    def pack(key, **rest) : Dict
      Term[**rest].with(key, self)
    end

    def &(newer : Term) : Term
      (downcast & newer.downcast).upcast
    end

    def &-(other : Term) : Term?
      (downcast &- other.downcast).try(&.upcast)
    end

    def each_prefix(&fn : BiList(Term), Term ->) : Nil
      type.dict? ? unsafe_as_d.each_prefix(&fn) : fn.call(BiList(Term)[], self)
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
        {% raise "no such method in any ITerm includer: #{call.name}, cannot autocast" %}
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

    # Constructs a string term from the given string *object*.
    def self.[](object : String) : Str
      Str.new(object)
    end

    # Constructs a string term from the given character *object*.
    def self.[](object : Char) : Str
      Str.new(object.to_s)
    end

    # Constructs a symbol term from the given symbol *object*.
    def self.[](object : Symbol) : Sym
      Sym.new(object.to_s)
    end

    # Constructs a boolean term from the given boolean *object*.
    def self.[](object : Bool) : Boolean
      Boolean.new(object)
    end

    # Constructs a number term from the given enum *object*.
    def self.[](object : Enum) : Num
      Num.new(object.value)
    end

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

    def self.set(ee : Enumerable(Term))
      Dict.build do |commit|
        ee.each { |arg| commit.with(arg, true) }
      end
    end

    def self.set(*args) : Dict
      set(args)
    end

    # :nodoc:
    def self.of(object : Nil) : Nil
    end

    # Same as `.[]` but upcasts to generic `Term` for you.
    def self.of(*args, **kwargs) : Term
      Term[*args, **kwargs].upcast
    end
  end

  # Hashing

  struct Term
    # Term hasher object, similar in purpose to `Crystal::Hasher`.
    #
    # Currently implements 64-bit Fowler–Noll–Vo hash function.
    #
    # Wirewright is assumed to run on x86 only. This means system-endian is
    # little-endian. Under this assumption we say that the hash is *globally
    # stable*, meaning it stays the same across runs and machines for
    # equal values.
    #
    # Global stability is explicitly implemented despite susceptibility to
    # HashDoS etc. This is because Wirewright's Terms are for use in a purely
    # functional setting; randomly seeded hash functions lead to different dict
    # entry order per run/machine => different return result of e.g. `(entries ...)`
    # per run/machine, which we would consider as an implementation error.
    #
    # WARNING: `Hasher` is a mutable struct. Pass it around carefully.
    struct Hasher
      # Reference: https://softwareengineering.stackexchange.com/a/145633

      # :nodoc:
      FNV_OFFSET_BASIS = 14695981039346656037u64
      # :nodoc:
      FNV_PRIME        = 1099511628211u64

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
    end

    # Returns the hashcode of *object*. See `hashcode(hasher, object)` variants
    # to learn about supported types of *object*s.
    def self.hashcode(object) : UInt64
      hasher = hashcode(Hasher.new, object)
      hasher.result
    end

    # Fast path for itemspart keys (indices).
    def self.hashcode(hasher : Hasher, object : Int32) : Hasher
      hasher.append(object)
      hasher
    end

    # Appends the hash of a symbol term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term::Sym) : Hasher
      hasher.append(TermType::Symbol)
      hasher.append(object.@spec)
      hasher
    end

    # Appends the hash of a string term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term::Str) : Hasher
      hasher.append(TermType::String)
      object.each_byte do |byte|
        hasher.append(byte)
      end
      hasher
    end

    # Appends the hash of a number term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term::Num) : Hasher
      hasher.append(TermType::Number)
      hasher.append(object.to_f64)
      hasher
    end

    # Appends the hash of a boolean term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term::Boolean) : Hasher
      if object.true?
        hasher.append(TermType::Boolean)
        hasher.append(1u8)
      else
        hasher.append(TermType::Boolean)
        hasher.append(0u8)
      end

      hasher
    end

    # :nodoc:
    HASHCODE_DICT_TYPE = begin
      hasher = Hasher.new
      hasher.append(TermType::Dict)
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

      hasher.append(hashcode)
      hasher
    end

    # Appends the hash of a term *object* to *hasher*.
    def self.hashcode(hasher : Hasher, object : Term) : Hasher
      hashcode(hasher, object.downcast)
    end
  end

  # Pattern matching entrypoints

  struct Term
    def self.matches(pattern, matchee, *, engine : Engine.class = M1, env = Term[]) : Array(Term::Dict) forall Engine
      engine.matches(Term.of(pattern), Term.of(matchee), env: env)
    end

    def self.case(matchee, *, engine : Engine.class = M1, **kwargs, &) forall Engine
      context = CaseContext(Engine).new(Term.of(matchee), **kwargs)

      with context yield context

      raise ArgumentError.new("no match found for #{matchee}")
    end

    macro of_case(*args, **kwargs, &block)
      ::Ww::Term.of(::Ww::Term.case({{args.splat}}, {{kwargs.double_splat}}) {{block}})
    end

    # Shorthand for a single-`matchpi` call to `Term.case`:
    #
    # ```
    # Term.case(term) do
    #   matchpi pattern do
    #     # Block
    #   end
    #
    #   otherwise { }
    # end
    # ```
    macro matchpi?(term, pattern, &)
      Term.case({{term}}) do
        matchpi {{pattern}} do
          {{yield}}
        end

        otherwise { }
      end
    end
  end

  # Misc

  struct Term
    private def self.each_keypath_and_leaf?(node : Term::Dict, prefix : Stack(Term), fn : Stack(Term), Term -> Bool) : Bool
      # Empty dict literal `{}` is a (leaf).
      if node.empty?
        return fn.call(prefix, node.upcast)
      end

      node.each_entry do |key, value|
        prefix.push(key)

        break unless each_keypath_and_leaf?(value.downcast, prefix, fn)

        prefix.pop
      end

      true
    end

    private def self.each_keypath_and_leaf?(node, prefix : Stack(Term), fn : Stack(Term), Term -> Bool) : Bool
      fn.call(prefix, node.upcast)
    end

    # Calls *fn* with non-dictionary values along with key paths to them (which
    # keys to follow to get to the value), arbitrarily long.
    #
    # WARNING: keypaths are contained within a mutable `Stack` for memory
    # efficiency; you do not own the stack, for you the stack is read-only! Do not
    # mutate the key path stack, instead, make a copy of it (`dup`) and mutate
    # your copy instead. Or if you know what you're doing, make sure to return
    # the stack to valid condition after you've modified it.
    def self.each_keypath_and_leaf(node : Term, &fn : Stack(Term), Term -> Bool) : Nil
      each_keypath_and_leaf?(node: node.downcast, prefix: Stack(Term).new, fn: fn)
    end
  end

  struct Term
    private def self.each_keypath_and_item?(node : Term::Dict, prefix : Stack(Term), fn : Stack(Term), Term -> Bool) : Bool
      node.items.each_with_index do |item, index|
        prefix.push(Term.of(index))

        break unless each_keypath_and_item?(item.downcast, prefix, fn)

        prefix.pop
      end

      true
    end

    private def self.each_keypath_and_item?(node, prefix : Stack(Term), fn : Stack(Term), Term -> Bool) : Bool
      fn.call(prefix, node.upcast)
    end

    def self.each_keypath_and_item(node : Term, &fn : Stack(Term), Term -> Bool) : Nil
      each_keypath_and_item?(node: node.downcast, prefix: Stack(Term).new, fn: fn)
    end
  end

  # Encoding / decoding front-end

  # Raised by `Term.decode` if it cannot decode a term.
  class TermDecodeError < Exception
  end

  struct Term
    # Wait until all overloads are defined...
    macro finished
      {% verbatim do %}
        # Turns a Crystal object of type `T` into a `Term`.
        #
        # Implementors define overloads for their specific type. This method handles
        # union `T`s and raises at comptime otherwise.
        def self.encode(src : T) : Term forall T
          {% if T.union? %}
            {% for member in T.union_types %}
              if src.is_a?({{member}})
                return encode(src)
              end
            {% end %}

            unreachable
          {% else %}
            {% @caller.raise "Term.encode: cannot find an overload that can encode this type: #{T}" %}
          {% end %}
        end

        # Turns *term* into a Crystal object of type `T`. Returns `nil` if this
        # cannot be done.
        #
        # Implementors define overloads for their specific type. This method handles
        # union `T`s and raises at comptime otherwise (or returns `nil` if appropriate).
        def self.decode?(dst : T.class, term : Term) : T? forall T
          {% if T.union? %}
            # Ideally Crystal should be able to do this on its own but it doesn't
            # seem that it can...
            {% for member in T.union_types %}
              if object = decode?({{member}}, term)
                return object.as(T)
              end
            {% end %}
          {% else %}
            {% @caller.raise "Term.decode: cannot find an overload that can decode this type: #{T}" %}
          {% end %}
        end

        # Turns *term* into a Crystal object of type `T`. Raises `TermDecodeError`
        # if this cannot be done.
        def self.decode(dst : T.class, term : Term) : T forall T
          decode?(dst, term) || raise TermDecodeError.new("failed to decode from term to #{dst}")
        end
      {% end %}
    end
  end
end

require "./term/num"
require "./term/str"
require "./term/sym"
require "./term/boolean"
require "./term/dict"
require "./term/match"
