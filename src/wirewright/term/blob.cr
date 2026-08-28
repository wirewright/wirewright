module Ww
  # Blobs represent opaque binary data.
  #
  # Blob terms are similar to string terms. The difference is that strings are
  # used for plaintext data. Their design and optimizations bias strongly toward
  # UTF-8. Moreover, strings are required to be valid UTF-8.
  #
  # Blobs, on the other hand, are simply vectors of bytes (with an optional
  # media type), with no presuppositions about their content.
  #
  # We hash blobs on construction using a cryptographic hash function (see `DIGEST_ALGORITHM`).
  # Having such a hash is very useful in practice, so the small added overhead of
  # computing a hash is justified. For example, equality and hashcode become O(1)
  # for arbitrary blobs.
  #
  # We use `UInt64`s instead of `Int32` for size and capacity, because unlike
  # most other types, ~4 GiB is something one can readily imagine with blobs.
  #
  # Reference: https://github.com/crystal-lang/crystal/blob/master/src/string.cr
  @[Term::Assoc(TermType::Blob, :unsafe_as_blob)]
  class Term::Blob
    include Equality
    include AutoUpcast
    include TypeConversion

    # Constructs an empty blob.
    class_getter empty : Blob = Blob.new(Bytes.empty)

    # The algorithm used to compute blob digest.
    DIGEST_ALGORITHM = Digest::SHA256

    # Returns the hash digest of this blob.
    #
    # The hash is computed by `DIGEST_ALGORITHM`.
    getter digest : Bytes

    # Returns the *classification* of this blob -- its media type.
    #
    # See `Classif` to learn more.
    getter? classif : Classif?

    @size : UInt64
    @mem : UInt8*

    # :nodoc:
    def initialize(@size, @mem, digester : ::Digest, @classif)
      @digest = digester.final
    end

    # :nodoc:
    def initialize(@size, @mem, @digest, @classif)
    end

    # Constructs a blob with the given byte *slice*.
    #
    # WARNING: If *slice* is read-only, the underlying pointer is reused; otherwise,
    # *slice* is copied.
    def self.new(slice : Bytes, classif : Classif? = nil) : Blob
      if slice.read_only?
        digester = DIGEST_ALGORITHM.new
        digester.update(slice)
        return new(slice.size.to_u64, slice.to_unsafe, digester, classif)
      end

      capacity = slice.size.to_u64

      builder = stack_alloc Builder.new(capacity)
      builder.write(slice)
      builder.to_blob(classif)
    end

    # Yields a builder object to incrementally construct a blob.
    #
    # WARNING: The builder must not outlive the block, because it is allocated on
    # the stack. If it does, that's UB.
    def self.build(*, capacity : UInt64 = MIN_CAPACITY, classif : Classif? = nil, & : Builder ->) : Blob
      builder = stack_alloc Builder.new(capacity)
      yield builder
      builder.to_blob(classif)
    end

    # Constructs a blob from the given *string*.
    def self.new(string : String, classif : Classif? = nil) : Blob
      digester = DIGEST_ALGORITHM.new
      digester.update(string)
      Term::Blob.new(string.bytesize.to_u64, string.to_unsafe, digester, classif)
    end

    def self.refine(blob : Blob, classif : Classif?) : Blob
      new(blob.@size, blob.@mem, blob.@digest, classif)
    end

    def self.classify(blob : Blob) : Blob
      refine(blob, classif: classif(blob))
    end

    def self.unclassify(blob : Blob) : Blob
      refine(blob, classif: nil)
    end

    # Returns the classification of *blob* (`Blob#classif?`), or determines it
    # using `PantoMIME`.
    def self.classif(blob : Blob) : Classif
      blob.classif? || Classif.of(blob.to_slice, blob.digest)
    end

    def self.simplify(blob : Blob) : Str | Blob
      classif = blob.classif?
      if (classif.nil? || (classif.plain? && classif.utf8?)) && blob.utf8?
        return Term[blob.to_string]
      end

      blob
    end

    def self.unsimplify(blob : Blob) : Blob
      blob
    end

    CLASSIF_PLAINTEXT = Term::Blob::Classif.of(MIME::MediaType.parse("text/plain;charset=UTF-8"))

    def self.unsimplify(string : Str) : Blob
      unsimplify(string.to(String))
    end

    def self.unsimplify(string : String) : Blob
      Term::Blob.new(string, CLASSIF_PLAINTEXT)
    end

    # The minimum capacity for blobs, used in methods like `build`. Blobs are
    # down-sized if possible once their real size is known; before that point,
    # however, allocations are at least of `MIN_CAPACITY` (bytes).
    MIN_CAPACITY = 32u64

    # The builder object lets you build a blob incrementally.
    #
    # Reference: https://github.com/crystal-lang/crystal/blob/master/src/string/builder.cr
    class Builder < IO
      @digester : ::Digest
      @capacity : UInt64

      def initialize(capacity : UInt64)
        @digester = DIGEST_ALGORITHM.new
        @capacity = Math.max(capacity, MIN_CAPACITY)
        @mem = Pointer(UInt8).malloc(@capacity)
        @size = 0u64
      end

      def bytesize : UInt64
        @size
      end

      def read(slice : Bytes) : NoReturn
        raise EOFError.new
      end

      def write(slice : Bytes) : Nil
        return if slice.empty?

        reserve(@size + slice.size)

        slice.copy_to(@mem + @size, slice.size)

        @size += slice.size
        @digester.update(slice)
      end

      private def reserve(newsize : UInt64) : Nil
        return if newsize <= @capacity

        @capacity = Math.max(newsize, @capacity + @capacity//2)
        @mem = @mem.realloc(@capacity)
      end

      # Tries to reclaim some memory if capacity is bigger than what was requested.
      private def shrink_to_fit
        return unless @size < @capacity

        @capacity = @size
        @mem = @mem.realloc(@size)
      end

      # :nodoc:
      def to_blob(classif : Classif?) : Blob
        shrink_to_fit

        Blob.new(@size, @mem, @digester, classif)
      end
    end

    # :nodoc:
    def to_unsafe : UInt8*
      @mem
    end

    # Returns the byte content of this blob.
    def to_slice : Bytes
      Slice.new(to_unsafe, @size, read_only: true)
    end

    # Unconditionally converts this blob to a Crystal string.
    def to_string : String
      String.new(bytes)
    end

    # :ditto:
    @[Dncast]
    def bytes : Bytes
      to_slice
    end

    # Returns the bytesize of this Blob as a `UInt64`.
    def ubytesize64 : UInt64
      @size
    end

    # Returns `true` if this blob contains no bytes.
    def empty? : Bool
      ubytesize64.zero?
    end

    # Blobs are compared lexicographically like Crystal slices. See `Slice#<=>`.
    def <=>(other : Blob) : Int32
      bytes <=> other.bytes
    end

    # Returns `true` if the content of this blob is valid UTF-8.
    def utf8? : Bool
      ::Unicode.valid?(to_slice)
    end

    def to_io : IO
      IO::Memory.new(to_slice)
    end

    def inspect(io)
      ML.compact(io, self)
    end

    def to_s(io)
      inspect(io)
    end

    # :nodoc:
    #
    # Non-cryptographic hash for use primarily by `Term.hashcode`.
    def hashrepr : UInt64
      # NOTE: different classifs for the same blob will collide, but we consider it a rare
      # enough thing not to worry. Equality will discriminate everything properly.
      digest.unsafe_slice_of(UInt64)[0]
    end

    def_equals @digest, @classif
  end

  class Term::Blob::Classif
    getter media_type : Term::Str
    getter type : Term::Str
    getter? subtype : Term::Str?
    getter media_params : Term::Dict

    def initialize(@media_type, @type, @subtype, @media_params)
    end

    # Classifies the given *slice* using `PantoMIME`.
    def self.of(slice : Bytes) : Classif
      of(PantoMIME.detect(slice))
    end

    @@cache = SyncLRU(Bytes, Classif).new(64)

    # Classifies the given *slice* using `PantoMIME`.
    #
    # *digest* is assumed to be the digest of *slice*. It is used for
    # caching detection.
    def self.of(slice : Bytes, digest : Bytes) : Classif
      @@cache.put_if_absent(digest) do
        of(PantoMIME.detect(slice))
      end
    end

    # :nodoc:
    CASE_INSENSITIVE_VALUE_KEYS = Set{"charset"}

    # Constructs a classification object based on a known *mime* type.
    def self.of(mime : MIME::MediaType) : Classif
      # [RFC 2045](https://datatracker.ietf.org/doc/rfc2045/):
      # > All media type values, subtype values, and parameter names as defined
      # > are case-insensitive.  However, parameter values are case-sensitive
      # > unless otherwise specified for the specific parameter.
      #
      # Where things are case-insensitive, we normalize them to lowercase.

      media_full_type = Term[mime.media_type.downcase]
      media_type = Term[mime.type.downcase]
      media_subtype = Term[mime.sub_type.try(&.downcase)]

      media_params = Term::Dict.build do |commit|
        mime.each_parameter do |key, value|
          key = key.downcase
          if key.in?(CASE_INSENSITIVE_VALUE_KEYS)
            value = value.downcase
          end

          commit.with(Term::Sym.new(key), Term.of(value))
        end
      end

      new(media_full_type, media_type, media_subtype, media_params)
    end

    # Nil passthrough shorthand.
    def self.of(object : Nil) : Nil
    end

    # :nodoc:
    MEDIA_CHARSET_UTF8 = "UTF-8"
    # :nodoc:
    MEDIA_CHARSET_ASCII = "US-ASCII"

    # Returns `true` if the content of this blob is encoded using UTF-8
    # (or US-ASCII, which is a subset of UTF-8).
    #
    # This can be used to e.g. decide whether to convert this blob to a `Term::Str`,
    # which is much more convenient for previewing and working with UTF-8-encoded data.
    def utf8? : Bool
      return false unless charset = media_params[:charset]?
      return false unless charset = charset.as_s?

      charset = charset.to(String)

      charset.compare(MEDIA_CHARSET_UTF8, case_insensitive: true) == 0 ||
        charset.compare(MEDIA_CHARSET_ASCII, case_insensitive: true) == 0
    end

    def plain? : Bool
      media_type == Term["text/plain"]
    end

    def to_s(io)
      io << media_type.to(String)
      return if media_params.empty?

      io << ';'

      media_params.each_entry do |key, value|
        assert key.type.symbol?
        assert value.type.string?

        io << key.to(String) << '=' << value.to(String)
      end
    end

    def_equals_and_hash @media_type, @media_params
  end
end
