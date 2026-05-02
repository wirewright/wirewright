struct Nil
  {% for method in %w[as_n as_s as_sym as_b as_d as_blob] %}
    # Returns `nil`. This is a shorthand for casting nilable `Ww::Term`s or
    # `Ww::Term::Any`s, e.g. `dict[:key]?.as_n?`.
    def {{method.id}}? : Nil
    end
  {% end %}
end

module Ww
  # Lists the possible types of terms.
  enum TermType : UInt8
    Any     = 0
    Number  = 1
    String  = 2
    Symbol  = 3
    Boolean = 4
    Dict    = 5
    Blob    = 6

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

    def self.parse(cls : Term::Blob.class)
      TermType::Blob
    end

    def self.parse(cls : Term.class)
      TermType::Any
    end

    def self.parse?(sym : Term::Sym)
      case sym
      when SYM_BLANK_ANY     then Any
      when SYM_BLANK_NUMBER  then Number
      when SYM_BLANK_STRING  then String
      when SYM_BLANK_SYMBOL  then Symbol
      when SYM_BLANK_BOOLEAN then Boolean
      when SYM_BLANK_DICT    then Dict
      when SYM_BLANK_BLOB    then Blob
      end
    end

    def self.parse(sym : Term::Sym)
      parse?(sym) || raise ArgumentError.new
    end

    def subtype?(other : TermType)
      other.any? || self == other
    end

    def atom? : Bool
      !(any? || dict?)
    end

    def blank : Term::Sym
      case self
      in .any?     then SYM_BLANK_ANY
      in .number?  then SYM_BLANK_NUMBER
      in .string?  then SYM_BLANK_STRING
      in .symbol?  then SYM_BLANK_SYMBOL
      in .boolean? then SYM_BLANK_BOOLEAN
      in .dict?    then SYM_BLANK_DICT
      in .blob?    then SYM_BLANK_BLOB
      end
    end
  end

  # All terms (including dictionary terms `Term::Dict`) are persistent, thread-safe,
  # and immutable.
  struct Term
    macro finished
      # Tagged union of term instance types.
      #
      # Internally, we store terms as *tagged pointers*. The three last bits of
      # a pointer wrapped by the `Term` struct designate the stored type, and
      # we can cast to it unsafely (*downcast*) and back (*upcast*, see `Term.of`),
      # avoiding tagged unions.
      #
      # However, sometimes (e.g., for method overloading), we may want to "unpack"
      # the tagged pointer to obtain a normal Crystal tagged union -- of type `Any`
      # (see `Term.[]`).
      alias Any = Sym | Num | Str | Boolean | Blob | Dict

      {% unless Tag.constants.size <= 8 %}
        {% raise "enum #{Tag} must contain at most 8 values" %}
      {% end %}

      {% unless TermType.constants.size <= 8 %}
        {% raise "enum #{TermType} must contain at most 8 values" %}
      {% end %}
    end

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

        # Returns `Term.of(self)`.
        def to?(type : ::Ww::Term.class)
          ::Ww::Term.of(self)
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

    # Connects a term instance (e.g., `Term::Sym`) to its term type (`TermType::Symbol`)
    # and an unsafe cast method (`Term#unsafe_as_sym`).
    #
    # ```
    # @[Term::Assoc(TermType::Symbol, :unsafe_as_sym)]
    # struct Term::Sym
    #   # ...
    # end
    # ```
    annotation Assoc
    end

    # `Term` methods marked with this annotation are targets of *automatic upcast*
    # of `Any` to `Term`; meaning, for example, that `Term#foo` annotated with
    # `Upcast` will define `Term::Num#foo`, `Term::Str#foo` and so on, which calls
    # `Term#foo` through `Term.of` (see also:`AutoUpcast`)
    annotation Upcast
    end

    # Methods of term instances (`Any`) marked with this annotation are targets of
    # *automatic downcast* of `Term` to `Any`; meaning, for example, that
    # `Term::Num#foo` annotated with `Dncast` defines `Term#foo`, which casts
    # the term to `Term::Num` and calls `Term::Num#foo`.
    #
    # NOTE: Currently, having the same method name on multiple term instances,
    # all with `Dncast`, is unsupported.
    annotation Dncast
    end

    # This module implements automatic upcasting from term instances (`Any`)
    # to `Term`s to call `Term` methods annotated with `Upcast`.
    module AutoUpcast
      macro finished
        {% for method in Term.methods %}\
          {% if method.annotation(Upcast) %}\
            # :nodoc:
            {% if method.accepts_block? %}\
              def {{method.name}}(*args, **kwargs, &)
                ::Ww::Term.of(self).{{method.name}}(*args, **kwargs) { |*blkargs| yield *blkargs }
              end
            {% else %}\
              def {{method.name}}(*args, **kwargs)
                ::Ww::Term.of(self).{{method.name}}(*args, **kwargs)
              end
            {% end %}\
          {% end %}\
        {% end %}\
      end
    end

    # Represents the pointer tag of a term.
    enum Tag : UInt64
      Dict    = 0u64 # < must be zero!
      Sym     = 1u64
      NumRat  = 2u64
      NumFlt  = 3u64
      NumInt  = 4u64
      Str     = 5u64
      Boolean = 6u64
      Blob    = 7u64
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

    # Returns the pointer tag of this term.
    def tag : Tag
      Tag.new(@mem.address & 0b111u64)
    end

    # Returns `true` if this term's underlying data is stored inline rather
    # than behind a pointer.
    def inline? : Bool
      case tag
      when .sym?, .num_int?, .boolean?
        true
      else
        false
      end
    end

    # Returns the un-tagged pointer.
    def unsafe_ptr : Void*
      Pointer(Void).new(@mem.address & ~0b111u64)
    end

    # Returns the `TermType` corresponding to this term. Guarantees to never
    # return `TermType::Any`.
    @[Upcast]
    def type : TermType
      case tag
      in .sym?
        TermType::Symbol
      in .dict?
        TermType::Dict
      in .num_int?, .num_rat?, .num_flt?
        TermType::Number
      in .str?
        TermType::String
      in .boolean?
        TermType::Boolean
      in .blob?
        TermType::Blob
      end
    end

    def subtype?(candidate : TermType) : Bool
      type.subtype?(candidate)
    end

    # Constructs a `Term` wrapping the given number term *instance*.
    def self.of(instance : Num) : Term
      case a = instance.@k
      in Int64
        bits = Num::Int61.bits(a) # 61 bit, 3 MSB clear
        address = (bits << 3) | Tag::NumInt.value
      in Float64
        bits = Num::Float61.bits(a)
        address = (bits << 3) | Tag::NumFlt.value
      in Pointer(BigRational)
        address = a.address | Tag::NumRat.value
      end

      Term.new(Pointer(Void).new(address))
    end

    # Downcasts this term to a number term without performing any checks.
    def unsafe_as_n : Num
      case tag
      when .num_int?
        bits = @mem.address >> 3
        value = Num::Int61.value(bits)

        Num.unsafe_new(value)
      when .num_flt?
        bits = @mem.address >> 3
        value = Num::Float61.value(bits)

        Num.unsafe_new(value)
      when .num_rat?
        Num.unsafe_new(unsafe_ptr.as(BigRational*))
      else
        raise TypeCastError.new
      end
    end

    # Constructs a `Term` wrapping the given string term *instance*.
    def self.of(instance : Str) : Term
      Term.new(Pointer(Void).new(instance.as(Void*).address | Tag::Str.value))
    end

    # Downcasts this term to a string term without performing any checks.
    def unsafe_as_s : Str
      unsafe_ptr.as(Str)
    end

    # Constructs a `Term` wrapping the given symbol term *instance*.
    def self.of(instance : Sym) : Term
      Term.new(Pointer(Void).new((instance.@bits << 3) | Tag::Sym.value))
    end

    # Downcasts this term to a symbol term without performing any checks.
    def unsafe_as_sym : Sym
      Sym.new(@mem.address >> 3)
    end

    # Constructs a `Term` wrapping the given boolean term *instance*.
    def self.of(instance : Boolean) : Term
      if instance.true?
        Term.new(Pointer(Void).new((1u64 << 3) | Tag::Boolean.value))
      else
        Term.new(Pointer(Void).new(Tag::Boolean.value))
      end
    end

    # Downcasts this term to a boolean term without performing any checks.
    def unsafe_as_b : Boolean
      Boolean.new((@mem.address >> 3) == 1)
    end

    # Constructs a `Term` wrapping the given dictionary term *instance*.
    def self.of(instance : Dict) : Term
      Term.new(instance.as(Void*))
    end

    # Downcasts this term to a dictionary term without performing any checks.
    def unsafe_as_d : Dict
      @mem.as(Dict)
    end

    # Constructs a `Term` wrapping the given blob term *instance*.
    def self.of(instance : Blob) : Term
      Term.new(Pointer(Void).new(instance.as(Void*).address | Tag::Blob.value))
    end

    # Downcasts this term to a blob term without performing any checks.
    def unsafe_as_blob : Blob
      unsafe_ptr.as(Blob)
    end

    # Downcasts `Term` to one of the term instance types.
    def self.[](term : Term) : Any
      case term.tag
      in .sym?
        term.unsafe_as_sym
      in .dict?
        term.unsafe_as_d
      in .num_int?, .num_rat?, .num_flt?
        term.unsafe_as_n
      in .str?
        term.unsafe_as_s
      in .boolean?
        term.unsafe_as_b
      in .blob?
        term.unsafe_as_blob
      end
    end

    # Attempts to downcast this term to a number term. Returns `nil` if impossible.
    @[Upcast]
    def as_n? : Num?
      case tag
      when .num_int?, .num_rat?, .num_flt?
        unsafe_as_n
      end
    end

    # Attempts to downcast this term to a string term. Returns `nil` if impossible.
    @[Upcast]
    def as_s? : Str?
      tag.str? ? unsafe_as_s : nil
    end

    # Attempts to downcast this term to a boolean term. Returns `nil` if impossible.
    @[Upcast]
    def as_b? : Boolean?
      tag.boolean? ? unsafe_as_b : nil
    end

    # Attempts to downcast this term to a symbol term. Returns `nil` if impossible.
    @[Upcast]
    def as_sym? : Sym?
      tag.sym? ? unsafe_as_sym : nil
    end

    # Attempts to downcast this term to a dictionary term. Returns `nil` if impossible.
    @[Upcast]
    def as_d? : Dict?
      tag.dict? ? unsafe_as_d : nil
    end

    # Attempts to downcast this term to an itemsonly dictionary term. Returns
    # `nil` if impossible.
    @[Upcast]
    def as_itemsonly_d? : Dict?
      return unless dict = as_d?
      return unless dict.itemsonly?

      dict
    end

    # Attempts to downcast this term to a pairsonly dictionary term. Returns
    # `nil` if impossible.
    @[Upcast]
    def as_pairsonly_d? : Dict?
      return unless dict = as_d?
      return unless dict.pairsonly?

      dict
    end

    # Attempts to downcast this term to a nonempty dictionary term. Returns
    # `nil` if impossible.
    @[Upcast]
    def as_nonempty_d? : Dict?
      return unless dict = as_d?
      return if dict.empty?

      dict
    end

    # Attempts to downcast this term to a dictionary term. Returns the dictionary's
    # itemspart if succeeded. Returns `nil` otherwise.
    @[Upcast]
    def as_itemspart_d? : Dict?
      return unless dict = as_d?

      dict.itemspart
    end

    # Attempts to downcast this term to a dictionary term. Returns the dictionary's
    # pairspart if succeeded. Returns `nil` otherwise.
    @[Upcast]
    def as_pairspart_d? : Dict?
      return unless dict = as_d?

      dict.pairspart
    end

    # Attempts to downcast this term to a blob term. Returns `nil` if this is
    # not possible.
    @[Upcast]
    def as_blob? : Blob?
      tag.blob? ? unsafe_as_blob : nil
    end

    {% for method in %w[as_n as_s as_b as_sym as_d as_blob] %}
      # Same as `{{method.id}}?`, but raises `TypeCastError` with *detail*
      # instead of returning `nil`.
      @[Upcast]
      def {{method.id}}(detail : String? = nil)
        {{method.id}}? || raise TypeCastError.new(detail)
      end

      # Map-like function to transform terms that downcast using `{{method.id}}?`
      # successfully. Other terms are returned unchanged.
      @[Upcast]
      def {{method.id}}(&) : Term
        return self unless input = {{method.id}}?

        Term.of(yield input)
      end
    {% end %}

    # Converts this term to an object of the given *type*, if possible.
    # Returns `nil` if not.
    def to?(type)
      return unless instance = Term[self].as?(TypeConversion) # supports

      instance.to?(type)
    end

    # Converts this term to an object of the given *type*, if possible.
    # Raises `TypeCastError` if not.
    def to(type)
      result = to?(type)
      if result.nil?
        raise TypeCastError.new
      end

      result
    end

    # Compares this and *other* terms using `Term.compare`. This method mainly
    # exists for interop with the Crystal stdlib.
    def <=>(other : Term) : Int32
      Term.compare(self, other)
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
      Term[self].inspect(io)
    end

    # Returns `true` if this and *other* terms are equal by reference.
    def same?(other : Term) : Bool
      @mem == other.@mem
    end

    # :ditto:
    def same?(other : Any) : Bool
      same?(Term.of(other))
    end

    # Returns `true` if this and *other* terms are equal.
    def ==(other : Term) : Bool
      return true if @mem == other.@mem

      # Fast path.
      if tag == other.tag
        if inline?
          return false # @mem check fail is enough
        end
      end

      # Another fast path.
      unless type == other.type
        return false
      end

      # Slow path.
      Term[self] == Term[other]
    end

    # :ditto:
    def ==(other : Any) : Bool
      self == Term.of(other) # Gives a chance to compare @mem first
    end

    def hash(hasher)
      Term[self].hash(hasher)
    end

    def clone : Term
      self
    end

    # Reference: https://github.com/crystal-lang/crystal/issues/5735#issuecomment-367564550
    macro finished
      {% for member in Any.union_types %}\
        {% unless ann = member.annotation(Assoc) %}\
          {% raise "#{member} must be annotated with Term::Assoc(term type, unsafe downcast method name)" %}\
        {% end %}\
        {% query, dncast = ann %}\
        {% for method in member.methods %}\
          {% if method.annotation(Dncast) %}\
            {% splatidx = method.splat_index %}\
            # :nodoc:
            def {{ method.name }}(
              {% for arg, i in method.args %}\
                {% if i == splatidx %}*{% end %}{{ arg }},
              {% end %}\
              {% if double_splat = method.double_splat %}\
                **{{ double_splat }},
              {% end %}\
              {% if (arg = method.block_arg) && arg.name %}\
                &{{arg}}
              {% elsif method.accepts_block? %}\
                &
              {% end %}\
            ) {% if method.return_type %}: {{method.return_type}}{% end %}\
              {% unless method.free_vars.empty? %} forall {{method.free_vars.splat}}{% end %}
              unless type == {{query.id}}
                {% if (rty = method.return_type) && method.name.ends_with?("?") %}\
                  \{% begin %}
                    \{% rty = parse_type({{rty.stringify}}).resolve %}
                    \{% if rty == ::Bool %}\
                       return false
                    \{% elsif rty.nilable? %}\
                       return
                    \{% end %}\
                  \{% end %}
                {% end %}

                raise TypeCastError.new
              end

              {{dncast.id}}.{{method.name}}(
                {% for arg, i in method.args %}\
                  {% if !splatidx || i < splatidx %}\
                    {{arg.internal_name}},
                  {% elsif i == splatidx %}\
                    {% unless arg.name.empty? %}\
                      *{{arg.name}},
                    {% end %}\
                  {% else %}\
                    {{arg.name}}: {{arg.internal_name}},
                  {% end %}\
                {% end %}\
                {% if double_splat = method.double_splat %}\
                  **{{ double_splat }},
                {% end %}\
                {% if (arg = method.block_arg) && !arg.name.empty? %}&{{arg.name}}{% end %}\
              ) {% if (arg = method.block_arg) && arg.name.empty? %}{ |*%blkargs| yield *%blkargs }{% end %}
            end
          {% end %}\
        {% end %}\
      {% end %}\
    end
  end

  # Smart constructors

  struct Term
    def self.[](object : Any) : Any
      object
    end

    # Constructs a number term representing the given number *object*.
    def self.[](object : Int | BigDecimal | BigRational) : Num
      Num.exact(object)
    end

    # :ditto:
    def self.[](object : Float) : Num
      Num.approx(object)
    end

    # Constructs a string term representing the given string *object*.
    def self.[](object : String) : Str
      Str.new(object)
    end

    # Constructs a string term representing the given string view *object*.
    def self.[](object : StringView) : Str
      Term[object.to_s]
    end

    # Constructs a string term representing the given character *object*.
    def self.[](object : Char) : Str
      Term[object.to_s]
    end

    # Constructs a symbol term representing the given symbol *object*.
    def self.[](object : Symbol) : Sym
      Sym.new(object.to_s)
    end

    # Constructs a boolean term representing the given boolean *object*.
    def self.[](object : Bool) : Boolean
      Boolean.new(object)
    end

    # Constructs a term representing the given enum *object*.
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
        Num.exact(object.value)
      end
    end

    {% for spec in { {:UUID, "UUID"}, {:H256, "256-bit term hash"},
                    {:Path, "path"}, {:NormalPath, "normal path"} } %}
      {% type, name = spec %}

      # Constructs a string term representing the given {{name.id}} *object*.
      def self.[](object : {{type.id}}) : Str
        Term[object.to_s]
      end
    {% end %}

    # Constructs an indexed dictionary representing the given enumerable *object*.
    # Elements of *object* receive successive keys 0, 1, 2, etc.
    #
    # See also: `#with`.
    def self.[](object : Enumerable) : Dict
      Dict.build do |commit|
        object.each_with_index do |el, i|
          commit.with(i, el)
        end
      end
    end

    {% for cls in %w(Hash NamedTuple) %}
      # Constructs a dictionary representing the given hash or named tuple *object*.
      #
      # See also: `#with`.
      def self.[](object : {{cls.id}}) : Dict
        Dict.build do |commit|
          object.each do |k, v|
            commit.with(k, v)
          end
        end
      end
    {% end %}

    # Constructs a dictionary representing the given `JSON::Any` *object*.
    #
    # Raises `ArgumentError` on `null`.
    def self.[](object : JSON::Any) : Any
      if (value = object.as_f? || object.as_s? || object.as_a? || object.as_h? || object.as_bool?).nil?
        raise ArgumentError.new
      end

      Term[value]
    end

    # Constructs a blob term from the given *bytes*.
    #
    # WARNING: the bytes are copied. Use `Blob.build` to avoid this (in other words,
    # start from blobs instead of converting into blobs).
    def self.[](object : Bytes) : Any
      Blob.new(object)
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

    # Shorthand for `of(set(*args))`.
    def self.of_set(*args) : Term
      of(set(*args))
    end

    # Constructs a dict; each tuple in *args* provides an object for the key followed
    # by one for the value. The resulting dict is also extended with **kwargs**, if they
    # are provided.
    def self.entries(args : Enumerable({_, _}), **kwargs) : Dict
      Dict.build do |commit|
        args.each { |key, value| commit.with(key, value) }
        kwargs.each { |key, value| commit.with(key, value) }
      end
    end

    # Shorthand for `of(entries(*args, **kwargs))`.
    def self.of_entries(*args, **kwargs) : Term
      of(entries(*args, **kwargs))
    end

    # Passes `nil` through so you can safely construct off nilable types
    # and get a nilable term as the result.
    def self.of(object : Nil) : Nil
    end

    # Passes *object* through.
    def self.of(object : Term) : Term
      object
    end

    # Same as `.[]` but upcasts to generic `Term` for you.
    def self.of(*args, **kwargs)
      of(Term[*args, **kwargs])
    end
  end

  # Hashing, comparison

  struct Term
    # Reference: https://mostlymangling.blogspot.com/2019/01/better-stronger-mixer-and-test-procedure.html
    # See also: https://jonkagstrom.com/bit-mixer-construction/
    private def self.mix(x : UInt64) : UInt64
      x ^= x.rotate_right(25) ^ x.rotate_right(50)
      x &*= 0xA24BAED4963EE407u64
      x ^= x.rotate_right(24) ^ x.rotate_right(49)
      x &*= 0x9FB21C651E98DF25u64
      x ^ (x >> 28)
    end

    # :nodoc:
    def self.hashcode(a : UInt64, b : UInt64) : UInt64
      mix(a ^ b.rotate_left(5))
    end

    # :nodoc:
    #
    # Symbols use plain bit mixing.
    def self.hashcode(term : Term::Sym)
      hashcode(TermType::Symbol.value.to_u64, mix(term.@bits))
    end

    # :nodoc:
    def self.hashcode(term : Term::Str) : UInt64
      hashcode(TermType::String.value.to_u64, term.hashcode)
    end

    # :nodoc:
    #
    # Numbers use plain bit mixing.
    def self.hashcode(term : Term::Num) : UInt64
      hashcode(TermType::Number.value.to_u64, mix(term.hashrepr))
    end

    # :nodoc:
    #
    # Booleans hash into a TRUE or FALSE constant, which are simply random numbers.
    def self.hashcode(term : Term::Boolean)
      if term.true?
        hashcode(TermType::Boolean.value.to_u64, 0x473419c1b81a5431u64)
      else
        hashcode(TermType::Boolean.value.to_u64, 0x143ea81786b6282du64)
      end
    end

    # :nodoc:
    def self.hashcode(term : Term::Dict) : UInt64
      hashcode(TermType::Dict.value.to_u64, term.hashcode)
    end

    # :nodoc:
    def self.hashcode(term : Term::Blob) : UInt64
      hashcode(TermType::Blob.value.to_u64, term.hashcode)
    end

    # :nodoc:
    def self.hashcode(term : Term)
      hashcode(Term[term])
    end

    {% if flag?(:docs) %}
      # Returns a 64-bit digest of *term*.
      #
      # NOTE: This is a non-cryptographic hash. It is not meant to be used in
      # adversarial scenarios. I also cannot attest to its quality as I'm not
      # a cryptographer.
      def self.hashcode(term : Term::Any | Term) : UInt64
      end
    {% end %}

    # Compares two term instances. Comparison is performed on the `TermType` first,
    # and then on the actual value using `<=>` if the types are equal. See `TermType`
    # to learn about term type-order. See `Num#<=>(other : Num)`, `Boolean#<=>(other : Boolean)`,
    # and others to learn more about same-type comparison.
    def self.compare(a : Any, b : Any) : Int32
      cmp = a.type <=> b.type

      if cmp.zero?
        case a
        in Num     then return a <=> b.as(Num)
        in Str     then return a <=> b.as(Str)
        in Sym     then return a <=> b.as(Sym)
        in Boolean then return a <=> b.as(Boolean)
        in Dict    then return a <=> b.as(Dict)
        in Blob    then return a <=> b.as(Blob)
        end
      end

      cmp
    end

    # Compares two terms. This overload simply downcasts and calls the other
    # overloads of `compare`.
    #
    # - Returns `-1` if *a* is less than *b*.
    # - Returns `0` if *a* is equal to *b*.
    # - Returns `+1` if *a* is greater than *b*.
    def self.compare(a : Term, b : Term) : Int32
      compare(Term[a], Term[b])
    end

    # Compares two entries (key-value tuples).
    def self.compare(a : {Term, Term}, b : {Term, Term}) : Int32
      a.compare(b) { |l, r| Term.compare(l, r) }
    end
  end

  # Digest

  struct Term
    # Represents a 256-bit hash of a term using four 64-bit blocks.
    struct H256
      ALGORITHM = Digest::SHA256

      getter blk0 : UInt64
      getter blk1 : UInt64
      getter blk2 : UInt64
      getter blk3 : UInt64

      def initialize(@blk0, @blk1, @blk2, @blk3)
      end

      def self.new(& : IO ->) : H256
        digest = ALGORITHM.new
        io = IO::ByteStream.new { |slice| digest.update(slice) }

        yield io

        scratch = uninitialized UInt8[32]
        blks = scratch.to_slice.unsafe_slice_of(UInt64)

        digest.final(scratch.to_slice)

        H256.new(blks[0], blks[1], blks[2], blks[3])
      end

      def self.new(term : Term) : H256
        new { |io| ML.compact(io, term) }
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
        Alpha48.encode(io, blk1)
        Alpha48.encode(io, blk2)
        Alpha48.encode(io, blk3)
      end
    end

    # Returns the 256-bit hash of *term* calculated using `H256::ALGORITHM`.
    def self.hashcode256(term : Term | Any) : H256
      H256.new(Term.of(term))
    end
  end

  # Pattern matching

  struct Term
    # Advanced: Direct form of `Term.case` allowing explicit control over the matcher
    # instance and the initial environment.
    #
    # See also: `Case.defcase`, `Case.scan`.
    macro case(matchee, *, matcher, env = Term[], **kwargs, &block)
      {% unless kwargs.empty? %}\
        {% raise "unrecognized keyword arguments passed to Term.case" %}
      {% end %}\
      {{@type}}::Case.scan({{matcher}}, Term.of({{matchee}}), {{env}}) {{block}}
    end

    # Advanced: Lets you pick an engine explicitly (e.g. `M0`, `M1`), constructing
    # an appropriate matcher.
    #
    # NOTE: You can use `engine: :m0` or `engine: :m1` in case Crystal fails to
    # resolve *engine* at the call-site. `Case::MM` compile-time raises on M0/M1
    # which actually helps you here.
    macro case(matchee, *, engine, **kwargs, &block)
      {% cls = engine.resolve? %}
      {% if cls == M0 || engine.id.downcase == :m0 %}
        {{@type}}.case({{matchee}}, matcher: {{@type}}::Case::MM0, {{kwargs.double_splat}}) {{block}}
      {% elsif cls == M1 || engine.id.downcase == :m1 %}
        {{@type}}.case({{matchee}}, matcher: {{@type}}::Case::MM1, {{kwargs.double_splat}}) {{block}}
      {% else %}
        {{@type}}.case({{matchee}}, matcher: {{@type}}::Case::MM({{engine}}), {{kwargs.double_splat}}) {{block}}
      {% end %}
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
      {{@type}}.case({{matchee}}, engine: M1, {{kwargs.double_splat}}) {{block}}
    end

    # A shorthand for wrapping `Term.case` in `Term.of`.
    macro of_case(*args, **kwargs, &block)
      {{@type}}.of({{@type}}.case({{args.splat}}, {{kwargs.double_splat}}) {{block}})
    end

    {% for name in %w[matchp matchpi givenpi matchpiT givenpiT] %}
      # Shorthand for a single-`{{name.id}}` call to `Term.case`. Raises
      # `ArgumentError` on mismatch.
      #
      # ```
      # Term.case(term, **kwargs) do
      #   {{name.id}} *patterns do
      #     # Block
      #   end
      # end
      # ```
      macro {{name.id}}(term, *patterns, **kwargs, &block)
        \{{@type}}.case(\{{term}}, \{{kwargs.double_splat}}) do
          {{name.id}}(\{{patterns.splat}}) \{{block}}
        end
      end

      # Shorthand for a single-`{{name.id}}` call to `Term.case`. Returns `nil`
      # on mismatch.
      #
      # ```
      # Term.case(term, **kwargs) do
      #   {{name.id}} *patterns do
      #     # Block
      #   end
      #
      #   otherwise { }
      # end
      # ```
      macro {{name.id}}?(term, *patterns, **kwargs, &block)
        \{{@type}}.case(\{{term}}, \{{kwargs.double_splat}}) do
          {{name.id}}(\{{patterns.splat}}) \{{block}}
          otherwise { }
        end
      end
    {% end %}
  end

  # Utilities

  struct Term
    # Returns `true` if *term* is a well-formed edge.
    #
    # This is just a "hand-optimized" version of the pattern `(%'edge _)`.
    def self.edge?(term : Dict, *, type : TermType) : Bool
      return false unless term.itemsonly?
      return false unless term.size == 2
      return false unless term.probably_includes?(SYM_EDGE)

      term[0] == SYM_EDGE && term[1].type.subtype?(type)
    end

    # :ditto:
    def self.edge?(term : Any, *, type : TermType) : Bool
      false
    end

    # :ditto:
    def self.edge?(term : Term, *, type : TermType = TermType::Any) : Bool
      return false unless term.type.dict?

      edge?(term.unsafe_as_d, type: type)
    end

    # Returns the maximum depth of *term*.
    #
    # If *term* is a dict, its depth is `1` plus the maximum depth of its
    # children. If *term* is a non-dict, its depth is always `0`.
    def self.depth(term : Dict) : Magnitude
      term.summary.maxdepth
    end

    # :ditto:
    def self.depth(term : Any) : Magnitude
      Magnitude.new(0)
    end

    # :ditto:
    def self.depth(term : Term) : Magnitude
      depth(Term[term])
    end

    # Recursively merges two dictionaries *a* and *b*.
    #
    # If two keys are equal and both values are dictionaries, those dictionaries are
    # recursively merged. Otherwise, prefers *b*'s values.
    #
    # See also: `union` for shallow merge.
    def self.merge(a : Dict, b : Dict) : Dict
      return b if a.empty?
      return a if b.empty?

      # Fast path for singleton dicts.
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
    end

    # Returns *b* for terms with different types.
    def self.merge(a : Any, b : Any) : Any
      b
    end

    # Shorthand for merging two `Term`s to obtain a `Term`.
    def self.merge(a : Term, b : Term) : Term
      Term.of(merge(Term[a], Term[b]))
    end

    # Shorthand for merging more than two `Any`s to obtain an `Any`.
    def self.merge(a : Any, b : Any, *cs : Any) : Any
      cs.reduce(merge(a, b)) { |memo, x| merge(memo, x) }
    end

    # Shorthand for merging more than two `Term`s to obtain a `Term`.
    def self.merge(a : Term, b : Term, *cs : Term) : Term
      cs.reduce(merge(a, b)) { |memo, x| merge(memo, x) }
    end

    # Shallow merge of entries from *a* and *b*.
    #
    # TODO: This should be implemented by `Dict`/`Pf::Map`/etc., i.e., internally,
    # with tree-specific optimizations, structurally.
    def self.union(a : Dict, b : Dict) : Dict
      return b if a.empty? || a.same?(b)
      return a if b.empty?

      assert a.nonempty? && b.nonempty?

      if a.size < b.size
        # Extend b with missing.
        b.transaction do |commit|
          a.each_entry do |k, v|
            next if k.in?(b)

            commit.with(k, v)
          end
        end
      else
        # Overwrite with values from b.
        a.transaction do |commit|
          b.each_entry do |k, v|
            # FIXME: this shouldn't be necessary, Dict#with[!]() should do this
            next if a[k]? == v

            commit.with(k, v)
          end
        end
      end
    end

    # Returns *b* for terms with different types.
    def self.union(a : Any, b : Any) : Any
      b
    end

    # Shallow merge of two terms *a* and *b*.
    #
    # For *a* and *b* of different types, *b* is preferred.
    # For *a* and *b* that are both a Dict, their entries are merged.
    def self.union(a : Term, b : Term) : Term
      Term.of(union(Term[a], Term[b]))
    end

    # Intersects a dictionary with a *mask*: if *mask* contains a key, then
    # the intersection of *a* with the *mask* contains the key.
    def self.intersection(a : Dict, mask : Dict) : Dict
      Dict.build do |commit|
        a.each_entry do |key, value|
          next unless key.in?(mask)

          commit.with(key, value)
        end
      end
    end

    # :nodoc:
    def self.extension?(b : Dict, *, of a : Dict) : Bool
      return false if a.size > b.size

      a.each_entry do |key, value0|
        unless value1 = b[key]?
          return false # disagrees
        end

        unless extension?(value1, of: value0)
          return false # disagrees
        end
      end

      true # agrees
    end

    # :nodoc:
    def self.extension?(b : Any, *, of a : Any) : Bool
      a == b ? true : false
    end

    # All keys in *a* must exist in *b*, and their values in *b* must be
    # an extension of the corresponding values in *a* (recursively), for
    # *b* to extend *a*. For non-dictionary terms, *b* extends *a* only if
    # *b* is equal to *a*.
    def self.extension?(b : Term, *, of a : Term) : Bool
      extension?(Term[b], of: Term[a])
    end

    # Returns a copy of the dict *a* with keys in *keys*. *keys* are converted
    # to `Term` using `Term.of`.
    def self.select(a : Dict, keys : Enumerable) : Dict
      Dict.build do |commit|
        keys.each do |key|
          commit.with(key, a[key]?)
        end
      end
    end

    # Returns a copy of the dict *a* with all of *keys* removed. Missing keys
    # are skipped. *keys* are converted to `Term` using `Term.of`.
    def self.exclude(a : Dict, keys : Enumerable) : Dict
      a.transaction do |commit|
        keys.each { |key| commit.without(key) }
      end
    end

    # Returns a copy of the dict *a* with all of *keys* removed. Missing keys are
    # skipped. Raises `TypeCastError` if *a* is not a dict. Upcasts the result
    # back to `Term`. *keys* are converted to `Term` using `Term.of`
    def self.exclude(a : Term, keys : Enumerable) : Term
      Term.of(exclude(a.as_d, keys))
    end

    # Returns a dict that contains children terms of *term* found at a set *depth*,
    # or its leaves if *depth* is `nil`.
    #
    # Traversal is performed in DFS-order. The resulting dict is ordered accordingly.
    #
    # - *depth* equal to `0` means *term* itself is returned.
    # - *depth* equal to `1` means the items of *term* are returned.
    #
    # Ignores the pairspart of traversed dicts. For non-dictionary *term*, returns
    # *term* if *depth* is nonzero.
    def self.flatten(term : Term, *, depth : Int32?) : Term
      return term unless dict = term.as_d?

      Term.of(flatten(dict, depth: depth))
    end

    # Same as `flatten(term : Term, *, depth : Int32)`, but accepts a known dict
    # and responds with a dict, too.
    def self.flatten(term : Dict, *, depth : Int32?) : Dict
      return term if depth == 0

      Dict.build do |commit|
        flatten(commit, term, depth)
      end
    end

    private def self.flatten(commit, term : Dict, depth : Int32?) : Nil
      if depth == 0
        commit << term
        return
      end

      term.items.each do |item|
        unless dict = item.as_d?
          commit << item
          next
        end

        flatten(commit, dict, depth ? depth - 1 : nil)
      end
    end

    private def self.assign?(root : Term, keypath : Indexable, rvalue : Term?, index : Int32)
      if index == keypath.size
        return rvalue
      end

      unless dict = root.as_d?
        return root
      end

      key = Term.of(keypath[index])
      value0 = dict[key]?
      value1 = assign?(value0 || Term.of, keypath, rvalue, index + 1)

      if value1.nil? || (rvalue.nil? && value1.type.dict? && value1.unsafe_as_d.empty?)
        return Term.of(dict.without(key))
      end

      Term.of(dict.with(key, value1))
    end

    # *value* is converted to a term using `Term.of`.
    #
    # If *value* is `nil`, this triggers removal. The leaf entry is removed,
    # and parent dicts, if they become empty after recursive removal, are also
    # removed.
    #
    # Each key in *keypath* is converted to a term using `Term.of`.
    #
    # New dictionaries are created as needed as this function follows through *keypath*.
    # If this function hits a non-dict as it follows the keypath, it aborts and returns
    # *root* unchanged. For example, if we try to do `assign?(root, {:x, :y, :z}, to: 10}`
    # but the value of `y` is a number, this function aborts and returns *root* unchanged.
    def self.assign?(root : Term, keypath : Indexable, *, to value) : Term?
      assign?(root, keypath, Term.of(value), index: 0)
    end

    # Same as `assign?`, but raises instead of returning `nil` when *root*
    # itself is removed (i.e., *value* is `nil` and *keypath* is empty).
    #
    # Since this function's return restriction is `Term`, it allows *keypath*
    # to be empty (and will thus return *value* as-is)..
    def self.assign(root : Term, keypath : Indexable, *, to value) : Term
      assign?(root, keypath, to: value) || raise ArgumentError.new("Term.assign() does not support removal of root")
    end

    # If you want the return type of `assign` to be restricted to `Dict`,
    # you must pass a *root* dict to trigger this overload.
    #
    # This overload does not support empty *keypath*, since that would mean *value*
    # must be used, which isn't necessarily a dict.
    def self.assign(root : Dict, keypath : Indexable, *, to value) : Dict
      assert keypath.present?

      assign(Term.of(root), keypath, to: value).as_d
    end

    # A utility function to perform one or more assignments on root.
    #
    # Each assign in *assignments* is a tuple of the form: `{*keypath, value}`. Here,
    # *keypath* represents one or more keys, and *value* is the target value, which could
    # be any object including `nil`; the latter signifying removal. See also: `assign`.
    #
    # Like `assign`, this function keeps the type of *root* as the return type. If you
    # give it a `root : Term`, it will return a `Term`; if you give it `root : Term::Dict`,
    # it will return the modified `Term::Dict`.
    #
    # ```
    # input = Term[x: {a: 100, b: 200}, c: 300]
    #
    # # input : Term::Dict
    # Term.morph(input, {:x, :a, "Foo"}, {:c, "Bar"})
    # # => {x: {a: "Foo", b: 200}, c: "Bar"} : Term::Dict
    #
    # input = Term.of(input)
    #
    # # input : Term
    # Term.morph(input, {:x, :a, "Foo"}, {:c, "Bar"})
    # # => {x: {a: "Foo", b: 200}, c: "Bar"} : Term
    #
    # state = Term[]
    #
    # Term.morph(state,
    #   # Create keys
    #   {:name, "Samuel Doe"},
    #   # Create nested keys
    #   {:born, :day, 10},
    #   {:born, :month, "June"},
    #   {:born, :year, 2000},
    #   # Assignments are executed inorder, so this one will overwrite our previous
    #   # assignment name: "Samuel Doe".
    #   {:name, "Jane Doe"},
    #   # Remove keys
    #   {:born, :month, nil},
    #   # Attempts to descend into non-dicts will abort (do nothing)
    #   {:born, :day, :xyz, "Foobar"},
    # )
    # # => {born: {day: 10 year: 2000} name: "Jane Doe"}
    # ```
    def self.morph(root, *assignments)
      assignments.reduce(root) do |memo, assignment|
        Term.assign(memo, assignment[...-1], to: assignment[-1])
      end
    end
  end

  # Replacement

  struct Term
    # Represents a *replacement*.
    #
    # A replacement is, for clients, like an `Array(Term)` or `Slice(Term)`. That
    # is, `Rep` lets us say, "a term is replaced by zero or more of these
    # *offspring* terms". Idiomatically, the elements or members of `Rep` are
    # called *offspring*. You can call them any way you want, but you are
    # recommended to use the word *offspring*  and derived.
    #
    # Internally, we provide non-allocating One variant and Many, which includes
    # allocating and non-allocating variants to adapt to the situation at hand
    # and avoid extra work.
    #
    # Several subsystems of Wirewright want to talk about replacements. `Alloy`,
    # `M1::Backmap`, `D7`, `Rewriter` are among such subsystems. Thus, we centralize
    # replacement logic in `Rep` and a suite of related functions defined on `Term`
    # for brevity (such as constructors for `Rep`, `Term.rep`; `Term.subst`, some
    # overloads of `Term.flatten` and so on.)
    struct Rep
      include Indexable(Term)

      # :nodoc:
      alias Any = One | Many

      # :nodoc:
      alias Many = ManySlice | ManyDict

      # :nodoc:
      defrecord One, offspring : Term

      # :nodoc:
      defrecord ManySlice, offspring : Slice(Term) do
        assert offspring.size != 1
      end

      # :nodoc:
      defrecord ManyDict, offspring : Term::Dict do
        assert offspring.itemsize != 1
      end

      # :nodoc:
      def initialize(@rep : Any)
      end

      def size : Int
        case rep = @rep
        in One       then 1
        in ManySlice then rep.offspring.size
        in ManyDict  then rep.offspring.itemsize
        end
      end

      def unsafe_fetch(index : Int)
        case rep = @rep
        in One       then rep.offspring
        in ManySlice then rep.offspring.unsafe_fetch(index)
        in ManyDict  then rep.offspring[index]
        end
      end

      # :nodoc:
      def collapse
        case rep = @rep
        in One  then rep.offspring
        in Many then Term.of(rep.offspring)
        end
      end
    end

    # Constructs an empty replacement.
    def self.rep : Rep
      Rep.new(Rep::ManySlice.new(Slice(Term).empty))
    end

    # Constructs a singleton replacement.
    def self.rep(offspring : Term) : Rep
      Rep.new(Rep::One.new(offspring))
    end

    # Constructs a replacement by two or more *offspring*.
    def self.rep(*offspring : Term) : Rep
      rep(offspring)
    end

    # Constructs a replacement by zero or more *offspring*.
    def self.rep(offspring : Indexable(Term)) : Rep
      if term = offspring.single?
        return Rep.new(Rep::One.new(term))
      end

      if offspring.is_a?(Dict::ItemsView) && offspring.covers_fully?
        return Rep.new(Rep::ManyDict.new(offspring.collect))
      end

      Rep.new(Rep::ManySlice.new(offspring.to_readonly_slice(&.itself)))
    end

    # Constructs a replacement by zero or more *offspring*.
    #
    # NOTE: if *offspring* is read-only, we skip copying it.
    def self.rep(offspring : Slice(Term)) : Rep
      if term = offspring.single?
        return Rep.new(Rep::One.new(term))
      end

      if offspring.read_only?
        return Rep.new(Rep::ManySlice.new(offspring))
      end

      rep(offspring.to_readonly_slice(&.itself))
    end

    # We collapse *rep* to a term in the following way:
    #
    # - Replacement with one collapses to itself.
    # - Replacement with zero or many collapses to a list term.
    #
    # This behavior is useful when the caller does not accept a `Rep`.
    #
    # Importantly, the above means that the empty dict `()` is highly ambiguous.
    # It may result from either of the two. Normal dictionaries are ambiguous
    # as well, because they may come from a replace-with-one, or they may represent
    # a replace-with-many. In a sense, by calling `collapse`, you "burn" information
    # about what kind of *rep* you had.
    #
    # This is dirty, but it works in practice -- most of the times, you just don't
    # care. If clients want full information, they should accept `Rep` directly.
    # The caller can also encode `Rep` itself, say, by always using a list. Zero then
    # becomes `()`, replace-with-one `(x)`, replace-with-many `(x y z)`. This is as
    # easy as just doing `Term.of(rep)` as opposed to `Term.collapse(rep)`.
    def self.collapse(rep : Rep) : Term
      rep.collapse
    end

    # Returns `true` if applying *rep0* would give a term different than *term0*.
    def self.changes?(term0 : Term, *, after rep : Rep) : Bool
      if term1 = rep.single?
        return term0 != term1
      end

      true
    end

    private def self.subst(root : Term, keypath : Indexable, fn, index)
      if index == keypath.size
        return fn.call(root)
      end

      unless dict = root.as_d?
        return rep(root)
      end

      key = keypath[index]
      unless value = dict[key]?
        return rep(root)
      end

      rep = subst(value, keypath, fn, index + 1)

      rep(Term.of(subst(dict, key, value, rep)))
    end

    private def self.subst(dict : Dict, key : Term, value : Term, rep : Rep) : Dict
      if offspring = rep.single?
        if offspring == value # No change
          return dict
        end
        return dict.with(key, rep)
      end

      if index = dict.index32?(key)
        return dict.replace(index, rep)
      end

      # We're in a pair, as in:
      #
      #   x: (^* (1 2 3))
      #
      # There are only two possible states for a pair if it is treated like
      # a container:
      #
      #   zero terms -- the pair does not exist
      #   one term   -- the pair exists
      #
      # A replacement with more than one term does not fit in a pair -- the extra
      # terms have nowhere to go. We handle this by wrapping such cases in `()`,
      # but, unfortunately, just like at the top-level, this generates a nasty,
      # unpredictable interface; not something with clean zero/one/many boundaries
      # (as e.g. a list). Anything else would be inconvenient.

      if rep.empty?
        return dict.without(key)
      end

      assert rep.size > 1

      dict.with(key, rep)
    end

    # Replaces the value at *keypath* using *fn*. Noop if could not
    # follow *keypath*. Returns *rep* if *keypath* is empty. Noop if
    # *root* is not a dict.
    def self.subst(root : Term, keypath : Indexable, &fn : Term -> Rep) : Rep
      subst(root, keypath, fn, index: 0)
    end

    # Replaces the value at *keypath* with *rep*. Noop if could not
    # follow *keypath*. Returns *rep* if *keypath* is empty. Noop if
    # *root* is not a dict.
    def self.subst(root : Term, keypath : Indexable, rep : Rep) : Rep
      subst(root, keypath) { rep }
    end

    # Replaces entry values in *root*'s *part* using the block.
    #
    # Yields each key and value in *root*'s *part* to the block. The replacement
    # returned by the block is used to replace the value.
    #
    # See also `Dict#each_entry` overloads for info on *part*.
    #
    # In itemspart, zero or many replacements are handled as expected. Replacement
    # with zero signifies removal of an item; with one, its replacement; with many,
    # its substitution by many offspring.
    #
    # In pairspart, replacement with zero signifies the removal of the pair; with one,
    # its replacement; with many, its replacement with a list of offspring.
    def self.flatten(root dict : Dict, *, part = Dict.itemspart, & : Term, Term -> Rep) : Dict
      changes = Pf::Kit.stack_array({Term, Term, Rep}, 8)

      dict.each_entry(in: part) do |key, value|
        rep = yield key, value
        next unless changes?(value, after: rep)

        changes << {key, value, rep}
      end

      # Fast, no-alloc path for cases when no changes were made to the dict.
      if changes.empty?
        return dict
      end

      multi = Pf::Kit.stack_array({Term, Term, Rep}, 4)

      dict = dict.transaction do |commit|
        changes.each do |key, value, rep|
          unless term = rep.single?
            multi << {key, value, rep}
            next
          end

          commit.with(key, term)
        end
      end

      if multi.empty?
        return dict
      end

      multi.sort_by! { |index, _, _| index }
      multi.reverse_each do |key, value, rep|
        dict = subst(dict, key, value, rep)
      end

      dict
    end

    # Replaces entry values in *root* using the block.
    #
    # Passthrough if *root* is not a dictionary.
    #
    # See `flatten(Dict, **kwargs, &)` for more info.
    def self.flatten(root term : Term, **kwargs, &) : Term
      unless dict = term.as_d?
        return term
      end

      dict = flatten(dict, **kwargs) do |item, index|
        yield item, index
      end

      Term.of(dict)
    end

    # Transforms offspring in *rep* using the block and flattens the reps
    # it returns into a single rep.
    def self.flatten(rep : Rep, & : Term -> Rep) : Rep
      sink = Pf::Kit.stack_array(Term)
      changed = false

      rep.each do |offspring|
        expansion = yield offspring
        expansion.each { |member| sink << member }
        changed ||= changes?(offspring, after: expansion)
      end

      return rep unless changed

      rep(sink)
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
      ns = Pf::Kit.stack_array(Int32, 32)
      nodes = Pf::Kit.stack_array(Term, 32)
      nodes << root

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
      ns = Pf::Kit.stack_array(Int32, 32)
      nodes = Pf::Kit.stack_array(Term, 32)
      nodes << root

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
  end
end

require "./term/num"
require "./term/str"
require "./term/sym"
require "./term/boolean"
require "./term/dict"
require "./term/blob"
require "./term/case"
