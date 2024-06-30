module Ww
  # A data structure for efficiently storing and retrieving terms by query.
  #
  # The basic idea is, `Sparse::Map` efficiently stores queries and allows to
  # retrieve a query by providing a term that it accepts; and `Term::Map`
  # efficiently stores terms and allows to retrieve a term by providing a query
  # that it is accepted by.
  class Term::Map(T)
    private on_demand l : Hash(Term, Set(T))
    private on_demand r : Hash(T, Set(Term))
    private on_demand trie : Term::Trie

    # Returns `true` if this map contains no bindings.
    def empty? : Bool
      !(has_l? && has_r? && has_trie?)
    end

    # Returns a named tuple contaning the number of unique terms and objects
    # stored in this map.
    def count : {terms: Int32, objects: Int32}
      {terms: has_l? ? l.size : 0, objects: has_r? ? r.size : 0}
    end

    # Returns `true` if this map has one or more binding with *object* as the term.
    def includes?(object : Term) : Bool
      has_l? && l.has_key?(object)
    end

    # Returns `true` if this map has one or more binding with *object* as the object.
    def includes?(object : T) : Bool
      has_r? && r.has_key?(object)
    end

    # :nodoc:
    def includes?(object) : Bool
      false
    end

    # Yields unique terms from bindings whose object is *object*.
    def each_term_bound_to(object : T, & : Term ->) : Nil
      return unless has_r?
      return unless terms = r[object]?

      terms.each { |term| yield term }
    end

    # Yields each unique term followed by the set of objects bound to it.
    def each_term_and_object_set(& : Term, Set(T) ->) : Nil
      return unless has_l?

      l.each { |term, object_set| yield term, object_set }
    end

    # Yields each unique binding from this map.
    def each_term_and_object(& : Term, T ->) : Nil
      each_term_and_object_set do |term, object_set|
        object_set.each { |object| yield term, object }
      end
    end

    # Inserts a bidirectional binding between *term* and *object*.
    #
    # See `Term::Map` to learn more.
    #
    # ```
    # map = Term::Map(String).new
    #
    # # Many-to-many binding of terms to objects:
    # map.bind(Term[100], "hello")
    # map.bind(Term[200], "hello")
    # map.bind(Term[100], "world")
    # ```
    def bind(term : Term | ITerm, object : T) : self
      term = term.upcast

      unless objects = l[term]?
        l[term] = objects = Set(T).new
        trie.add(term)
      end

      unless terms = r[object]?
        r[object] = terms = Set(Term).new
      end

      terms << term
      objects << object

      self
    end

    # Removes all bindings with the given *term*. Noop if *term* is in
    # none of the bindings.
    #
    # ```
    # map = Term::Map(String).new
    #
    # map.bind(Term[200], "foo")
    # map.bind(Term[200], "bar")
    # map.bind(Term[300], "bar")
    #
    # map.detect(Sparse::Query.parse("200").translate) # => Set{"foo", "bar"}
    # map.detect(Sparse::Query.parse("300").translate) # => Set{"bar"}
    #
    # map.unbind(Term[200])
    #
    # map.detect(Sparse::Query.parse("200").translate) # => Set{}
    # map.detect(Sparse::Query.parse("300").translate) # => Set{"bar"}
    # ```
    def unbind(term : Term | ITerm) : self
      term = term.upcast

      return self unless has_l?
      return self unless objects = l.delete(term)

      trie.delete(term)

      objects.each do |object|
        dependencies = r[object]
        if dependencies.size == 1 # Would remove the last one
          r.delete(object)
        else
          dependencies.delete(term)
        end
      end

      self
    end

    # Removes all bindings with the given *object*. Noop if *object* is in
    # none of the bindings.
    #
    # ```
    # map = Term::Map(String).new
    #
    # map.bind(Term[200], "foo")
    # map.bind(Term[200], "bar")
    # map.bind(Term[300], "bar")
    #
    # map.detect(Sparse::Query.parse("200").translate) # => Set{"foo", "bar"}
    # map.detect(Sparse::Query.parse("300").translate) # => Set{"bar"}
    #
    # map.unbind("foo")
    #
    # map.detect(Sparse::Query.parse("200").translate) # => Set{"bar"}
    # map.detect(Sparse::Query.parse("300").translate) # => Set{"bar"}
    # ```
    def unbind(object : T) : self
      return self unless has_r?
      return self unless terms = r.delete(object)

      terms.each do |term|
        dependencies = l[term]
        if dependencies.size == 1 # Would remove the last one
          l.delete(term)
          trie.delete(term)
        else
          dependencies.delete(object)
        end
      end

      self
    end

    # Yields bindings matched by *unit*.
    def eval(unit : Sparse::Unit::Query, & : Term, T ->) : Nil
      return unless has_trie?

      matches = trie.eval(unit)
      matches.each do |term|
        objects = l[term]
        objects.each { |object| yield term, object }
      end
    end

    # Returns an array of bindings matched by *unit*.
    def eval(unit : Sparse::Unit::Query) : Array({Term, T})
      bindings = [] of {Term, T}
      eval(unit) { |term, object| bindings << {term, object} }
      bindings
    end

    # Returns a set of objects matched by *unit*. Useful if you only need to check
    # for the presence of certain objects without having to discriminate between
    # different terms bound to the same object.
    #
    # ```
    # map = Term::Map(String).new
    #
    # map.bind(Term[x: 100, y: 200, z: 300], "foo")
    # map.bind(Term[x: 100, y: 200], "foo")
    # map.bind(Term[x: 100], "bar")
    #
    # map.detect(Sparse::Query.parse("{ x }").translate)       # => Set{"foo", "bar"}
    # map.detect(Sparse::Query.parse("{ x, y, z }").translate) # => Set{"foo"}
    # ```
    def detect(unit : Sparse::Unit::Query) : Set(T)
      objects = Set(T).new
      eval(unit) { |term, object| objects << object }
      objects
    end

    # Shorthand for `detect(unit).each(&)`.
    def detect(unit : Sparse::Unit::Query, & : T ->) : Nil
      detect(unit).each { |object| yield object }
    end
  end
end
