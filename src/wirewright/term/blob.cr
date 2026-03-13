module Ww
  # Blobs represent opaque binary data.
  #
  # Blobs are very much like strings except strings are used for plaintext data,
  # and their design and optimizations bias strongly toward Unicode. Blobs, on
  # the other hand, are simply vectors of bytes, with no presuppositions about
  # their content.
  #
  # We perform cryptocraphically secure hashing (using `DIGEST_ALGORITHM`) on all
  # blobs because having such a hash is very useful in practice, so the small added
  # cost of computing digests on construction is justified. For example, equality
  # and hashcode become O(1) for arbitrary blobs.
  #
  # Like Crystal's own `String` type, blobs store their byte content inline in memory:
  # the payload follows directly after a small header.
  #
  # Unlike String, we try to use `UInt64`s instead of `Int32` for size and capacity
  # because, compared with other types, ~4 GiB is something one can easily imagine.
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

    @size = uninitialized UInt64
    @digest = uninitialized UInt8[32]
    @classif = uninitialized Atomic(Classif?)
    @first = uninitialized UInt8

    protected def initialize_header(@size, digester)
      @digest = uninitialized UInt8[32]
      digester.final(@digest.to_slice)

      @classif = Atomic(Classif?).new(nil)
    end

    # Constructs a blob with the given byte *slice*.
    #
    # WARNING: *bytes* are copied. Build blobs with `build` to avoid extra copies
    # (i.e., start with blobs instead of converting into blobs).
    def self.new(slice : Bytes, *, classif : Classif? = nil) : Blob
      capacity = slice.size.to_u64

      builder = stack_alloc Builder.new(capacity)
      builder.write(slice)
      builder.to_blob(classif)
    end

    # :nodoc:
    HEADER_SIZE = offsetof(self, @first)

    # The minimum capacity for blobs, used in methods like `build`. Blobs are
    # down-sized if possible once their real size is known; before that point,
    # however, allocations are at least of `MIN_CAPACITY` (bytes).
    MIN_CAPACITY = 32u64

    # The builder object lets you build a blob incrementally.
    class Builder < IO
      @digester : ::Digest
      @capacity : UInt64

      def initialize(capacity : UInt64)
        @digester = DIGEST_ALGORITHM.new

        @capacity = Math.min(capacity, MIN_CAPACITY)

        # TODO: Can we use malloc_atomic here somehow? Maybe we should just switch to
        # a separate buffer approach after all? Since Classif is a reference pointing
        # to other things, we can't use malloc_atomic right now. I'm not sure we want
        # to strain the GC here because Blobs are the only type that can be very large.
        @mem = GC.malloc(HEADER_SIZE + @capacity).as(UInt8*)

        @size = 0u64
      end

      def read(slice : Bytes) : NoReturn
        raise EOFError.new
      end

      def write(slice : Bytes) : Nil
        return if slice.empty?

        if @size + slice.size > @capacity
          @capacity *= 2
          @mem = GC.realloc(@mem, HEADER_SIZE + @capacity).as(UInt8*)
        end

        slice.copy_to(@mem.as(Blob).to_unsafe + @size, slice.size)

        @size += slice.size
        @digester.update(slice)
      end

      # :nodoc:
      def to_uninitialized_blob : Blob
        mem = @mem

        # Try to reclaim some memory if capacity is bigger than what was requested
        if @size < @capacity
          mem = GC.realloc(@mem, HEADER_SIZE + @size).as(UInt8*)
        end

        Blob.set_crystal_type_id(mem)

        mem.as(Blob)
      end

      # :nodoc:
      def to_blob(classif : Classif? = nil) : Blob
        instance = to_uninitialized_blob
        instance.initialize_header(@size, @digester)
        instance.classify!(classif)
        instance
      end

      # :nodoc:
      def to_classif_blob : Blob
        instance = to_uninitialized_blob
        instance.initialize_header(@size, @digester)
        instance.classify!(classif: nil)
        instance
      end
    end

    # Yields a builder object to incrementally construct a blob.
    #
    # WARNING: The builder must not outlive the block, as it is allocated on
    # the stack.
    def self.build(*, capacity : UInt64 = MIN_CAPACITY, classify : Bool = false, & : Builder ->) : Blob
      builder = stack_alloc Builder.new(capacity)
      yield builder

      if classify
        builder.to_classif_blob
      else
        builder.to_blob
      end
    end

    # :nodoc:
    def to_unsafe : UInt8*
      pointerof(@first)
    end

    # Returns the byte content of this blob.
    @[Dncast]
    def bytes : Bytes
      Slice.new(to_unsafe, @size, read_only: true)
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

    protected def classify!(classif : Classif?) : Classif
      @classif.set(classif || Classif.of(bytes), :release)
    end

    # Returns the hash digest of this blob.
    #
    # The hash is computed by `DIGEST_ALGORITHM`.
    def digest : Bytes
      @digest.to_slice
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
    # :nodoc:
    class_getter magic_handle : LibMagic::Handle do
      handle = LibMagic.magic_open(LibMagic::OpenFlags.mime)
      if handle.nil?
        raise "libmagic: could not initialize"
      end

      at_exit { LibMagic.magic_close(handle) }

      LibMagic.magic_load(handle, nil)
      if error = LibMagic.magic_error(handle)
        raise String.new(error)
      end

      handle
    end

    # As I have absolutely no clue about what's going on inside libmagic wrt
    # thread-safety, let's assume it's thread-unsafe and synchronize all
    # access to it from our side.
    @@magic_lock = Sync::Mutex.new

    getter media_type : Term::Str
    getter media_params : Term::Dict

    def initialize(@media_type, @media_params)
    end

    # Constructs a classification object for *slice*.
    def self.of(slice : Bytes) : Classif?
      mime_string = @@magic_lock.synchronize do
        LibMagic.magic_buffer(magic_handle, slice, slice.size) || raise "libmagic: could not classify buffer"
      end

      mime = MIME::MediaType.parse(String.new(mime_string))

      media_type = Term[mime.media_type]
      media_params = Term::Dict.build do |commit|
        mime.each_parameter do |key, value|
          commit.with(Term::Sym.new(key), Term.of(value))
        end
      end

      new(media_type, media_params)
    end
  end
end
