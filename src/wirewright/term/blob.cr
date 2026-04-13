module Ww
  # Blobs represent opaque binary data.
  #
  # Blobs are very much like strings except strings are used for plaintext data,
  # and their design and optimizations bias strongly toward UTF-8. Blobs, on
  # the other hand, are simply vectors of bytes, with no presuppositions about
  # their content.
  #
  # We hash blobs on construction using a cryptographic hash function (see `DIGEST_ALGORITHM`),
  # because having such a hash is very useful in practice; so the small added
  # overhead of computing a hash is justified. For example, equality and hashcode
  # become O(1) for arbitrary blobs.
  #
  # We use `UInt64`s instead of `Int32` for size and capacity because, unlike
  # most other types, ~4 GiB is something one can easily imagine with blobs.
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

    @size : UInt64
    @digest : UInt8[32]
    @classif : Atomic(Classif?)
    @mem : UInt8*

    # :nodoc:
    def initialize(@size, @mem, digester, classif = nil)
      @digest = uninitialized UInt8[32]
      digester.final(@digest.to_slice)

      @classif = Atomic(Classif?).new(classif)
    end

    # Constructs a blob with the given byte *slice*.
    #
    # WARNING: If *slice* is read-only, the underlying pointer is reused; otherwise,
    # *slice* is copied.
    def self.new(slice : Bytes, *, classif : Classif? = nil) : Blob
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
      def to_unclassified_blob : Blob
        shrink_to_fit

        Blob.new(@size, @mem, @digester)
      end

      # :nodoc:
      def to_blob(classif : Classif? = nil) : Blob
        instance = to_unclassified_blob
        if classif
          instance.classify!(classif)
        end
        instance
      end

      # :nodoc:
      def to_classif_blob : Blob
        instance = to_unclassified_blob
        instance.classify!(classif: nil)
        instance
      end
    end

    # Yields a builder object to incrementally construct a blob.
    #
    # WARNING: The builder must not outlive the block, because it is allocated on
    # the stack. If it does, that's UB.
    def self.build(*, capacity : UInt64 = MIN_CAPACITY, classify : Bool = false, & : Builder ->) : Blob
      builder = stack_alloc Builder.new(capacity)
      yield builder

      if classify
        builder.to_classif_blob
      else
        builder.to_blob
      end
    end

    # Constructs a blob from the given *string*.
    def self.new(string : String) : Blob
      build(capacity: string.bytesize.to_u64, classify: true) do |io|
        io.write(string.to_slice)
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

    # Returns the classification of this blob.
    #
    # NOTE: The classification is computed on-demand unless it was explicitly provided
    # by the constructors of this object. We uses libmagic to classify the bytes. libmagic
    # is pretty expensive. Its expected runtime is in the high hundreds of microseconds to
    # milliseconds even for very small blobs (e.g., on my machine, 300 bytes is detected as
    # plaintext in about 1 millisecond). The classification is cached thereafter. Constructors
    # which do expensive stuff anyway (e.g. `PathServer`, when reading a file) usually
    # precompute `Classif` as well, so that clients never have to go through this expense.
    # Worst-case analysis, however, must account for missing `Classif`.
    @[Dncast]
    def classif
      if classif = @classif.get(:acquire)
        return classif
      end

      classify!(classif: nil)
    end

    # :nodoc:
    #
    # WARNING: Only call this if the blob wasn't published yet!
    def classify!(classif : Classif?) : Classif
      @classif.set(classif || Classif.of(bytes), :release)
    end

    # Returns the hash digest of this blob.
    #
    # The hash is computed by `DIGEST_ALGORITHM`.
    def digest : Bytes
      Slice.new(@digest.to_unsafe, @digest.size, read_only: true)
    end

    # Blobs are compared lexicographically like Crystal slices. See `Slice#<=>`.
    def <=>(other : Blob) : Int32
      bytes <=> other.bytes
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
    def hashcode : UInt64
      digest.unsafe_slice_of(UInt64)[0]
    end

    def_equals digest
  end

  class Term::Blob::Classif
    getter media_type : Term::Str
    getter media_params : Term::Dict

    def initialize(@media_type, @media_params)
    end

    # Constructs a classification object for *slice*.
    def self.of(slice : Bytes) : Classif?
      of(Magic.mime(slice))
    end

    # Constructs a classification object based on a known *mime* type. We normally
    # do this for HTTP responses which can tell us their MIME.
    def self.of(mime : MIME::MediaType) : Classif?
      media_type = Term[mime.media_type]
      media_params = Term::Dict.build do |commit|
        mime.each_parameter do |key, value|
          commit.with(Term::Sym.new(key), Term.of(value))
        end
      end

      new(media_type, media_params)
    end

    # :nodoc:
    MEDIA_TYPE_PLAIN = Term["text/plain"]
    # :nodoc:
    MEDIA_CHARSET_UTF8 = Term["UTF-8"]
    # :nodoc:
    MEDIA_CHARSET_ASCII = Term["US-ASCII"]

    # Returns `true` if the blob this classification describes looks like UTF-8
    # plaintext. This can be used to e.g. convert the blob to a `Term::Str` which
    # is much more convenient for previewing and working with UTF-8.
    def utf8? : Bool
      return false unless media_type == MEDIA_TYPE_PLAIN
      return false unless charset = media_params[:charset]?
      return false unless charset = charset.as_s?
      return false unless charset.upcase.in?(MEDIA_CHARSET_UTF8, MEDIA_CHARSET_ASCII)

      true
    end
  end
end
