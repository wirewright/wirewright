module Ww::ML
  # Instantiate `Renderer` with `TrackedTsrc` if you want to source-map the rendered
  # term. Otherwise, prefer `UntrackedTsrc`.
  struct TrackedTsrc
    getter term : Term

    # :nodoc:
    getter srcmap : Hash(Tpath, StringView)

    # :nodoc:
    def initialize(@term, @srcmap)
    end

    def self.of(text : StringView?, object : TrackedTsrc, *, src : StringView? = nil)
      object
    end

    def self.of(text : StringView?, object : Tuple, *, src : StringView? = nil)
      build(text) do |commit|
        object.each { |item| commit << TrackedTsrc.of(text, item) }
      end
    end

    # *src* gives the preferred location, if any. Otherwise, `ctx.location` is used.
    def self.of(text : StringView?, object, *, src : StringView? = nil)
      if location = src || text
        new(Term.of(object), srcmap: SrcMapHash{Tpath[] => location})
      else
        new(Term.of(object), srcmap: SrcMapHash.new)
      end
    end

    def self.build(text : StringView?, &)
      if text
        srcmap = SrcMapHash{Tpath[] => text}
      else
        srcmap = SrcMapHash.new
      end

      term = Term::Dict.build do |commit|
        yield Commit.new(commit, srcmap)
      end

      new(Term.of(term), srcmap)
    end

    def self.build(&)
      srcmap = SrcMapHash.new

      term = Term::Dict.build do |commit|
        yield Commit.new(commit, srcmap)
      end

      new(Term.of(term), srcmap)
    end

    struct Commit
      def initialize(@commit : Term::Dict::Commit, @srcmap : Hash(Tpath, StringView))
      end

      private def subsume(prefix, srcmap)
        srcmap.each do |path, text|
          @srcmap[path.prepend(prefix)] = text
        end
      end

      def includes?(object)
        @commit.includes?(object)
      end

      def includes?(object : TrackedTsrc)
        @commit.includes?(object.term)
      end

      def []?(key)
        @commit[key]?
      end

      def []?(key : TrackedTsrc)
        @commit[key.term]?
      end

      def with(key, value)
        if key.is_a?(TrackedTsrc)
          subsume(Tpath.key(key.term), key.srcmap)

          key = key.term
        end

        if value.is_a?(TrackedTsrc)
          subsume(Tpath.value(key), value.srcmap)

          value = value.term
        end

        @commit.with(key, value)

        self
      end

      def <<(object : TrackedTsrc)
        subsume(Tpath.value(@commit.itemsize), object.srcmap)

        @commit << object.term

        self
      end

      def concat(objects, &)
        objects.each { |object| self << (yield object) }

        self
      end

      def concat(objects)
        concat(objects, &.itself)
      end
    end

    def type : TermType
      term.type
    end

    def [](object) : TrackedTsrc
      key = Term.of(object)

      srcmap = SrcMapHash.new

      @srcmap.each do |path, text|
        next unless key == path.first?

        srcmap[path[1..]] = text
      end

      TrackedTsrc.new(@term[key], srcmap)
    end

    def append(tsrc : TrackedTsrc) : TrackedTsrc
      dict1 = @term.append(tsrc.term)

      prefix = Tpath.value(@term.itemsize)
      srcmap1 = @srcmap.dup
      tsrc.srcmap.each do |path, text|
        srcmap1[path.prepend(prefix)] = text
      end

      TrackedTsrc.new(Term.of(dict1), srcmap1)
    end

    def add?(key : TrackedTsrc, value : TrackedTsrc) : TrackedTsrc?
      return if key.term.in?(@term)

      dict1 = @term.with(key.term, value.term)
      srcmap1 = @srcmap.dup

      prefix = Tpath.key(key.term)
      key.srcmap.each do |path, text|
        srcmap1[path.prepend(prefix)] = text
      end

      prefix = Tpath.value(key.term)
      value.srcmap.each do |path, text|
        srcmap1[path.prepend(prefix)] = text
      end

      TrackedTsrc.new(Term.of(dict1), srcmap1)
    end
  end

  # Instantiate `Renderer` with `UntrackedTsrc` if you don't want source-mapping.
  # This way, the machinery won't have to track locations and merge source maps
  # recursively, which leads to an about 2x improvement in performance (at the cost
  # of, well... not having source maps).
  struct UntrackedTsrc
    getter term : Term

    # :nodoc:
    def initialize(@term)
    end

    def self.of(text : StringView?, object : UntrackedTsrc, *, src : StringView? = nil)
      object
    end

    def self.of(text : StringView?, object : Tuple, *, src : StringView? = nil)
      build(text) do |commit|
        object.each { |item| commit << UntrackedTsrc.of(text, item) }
      end
    end

    def self.of(text : StringView?, object, *, src : StringView? = nil)
      new(Term.of(object))
    end

    def self.build(text : StringView?, &)
      build { |commit| yield commit }
    end

    def self.build(&)
      term = Term::Dict.build { |commit| yield Commit.new(commit) }

      new(Term.of(term))
    end

    struct Commit
      def initialize(@commit : Term::Dict::Commit)
      end

      def includes?(object)
        @commit.includes?(object)
      end

      def includes?(object : UntrackedTsrc)
        @commit.includes?(object.term)
      end

      def []?(key)
        @commit[key]?
      end

      def []?(key : UntrackedTsrc)
        @commit[key.term]?
      end

      def with(key, value)
        if key.is_a?(UntrackedTsrc)
          key = key.term
        end

        if value.is_a?(UntrackedTsrc)
          value = value.term
        end

        @commit.with(key, value)

        self
      end

      def <<(object : UntrackedTsrc)
        @commit << object.term

        self
      end

      def concat(objects, &)
        objects.each { |object| self << (yield object) }

        self
      end

      def concat(objects)
        concat(objects, &.itself)
      end
    end

    def type : TermType
      term.type
    end

    def [](object) : self
      UntrackedTsrc.new(@term[object])
    end

    def append(tsrc : self) : self
      UntrackedTsrc.new(Term.of(@term.append(tsrc.term)))
    end

    def add?(key : self, value : self) : self?
      return if key.term.in?(@term)

      dict1 = @term.with(key.term, value.term)

      UntrackedTsrc.new(Term.of(dict1))
    end
  end
end
