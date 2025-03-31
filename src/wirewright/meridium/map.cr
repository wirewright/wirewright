module Ww::Meridium
  # Includers can be used as map backends for `Tspace`, `Tbase`, and so on.
  module IMap(K, V)
    # Returns the latest value of *key*, or `nil` if absent.
    #
    # "Latest" means "at the time of fetch". At the same time as the result of
    # the fetch (e.g. `nil`) is being returned to the caller, *key* might have
    # been added.
    abstract def latest?(referrer : Label, key : K) : V?

    # Returns an approximate number of entries in this map. Includers are allowed
    # to return `0` unconditionally if they cannot determine the size (e.g. in
    # a distributed, decentralized setting).
    abstract def size : Int32

    # Atomically registers a reference of *referrer* to *key*. If *key* is absent,
    # creates it and sets its value to *default*. Returns the value read at
    # the time of assignment. This is an INCREF. Each call will increment the reference
    # count of *key* by *referrer*.
    abstract def inc(referrer : Label, key : K, default : V) : V

    # Atomically removes *referrer*'s reference to *key*, removing the underlying
    # key-value pair if necessary. This is a DECREF; i.e., if you called `inc`
    # N times you'd have to call `dec` N times.
    abstract def dec(referrer : Label, key : K) : Nil

    # Conceptually the same as calling `dec` with each key from *keys* and *referrer*.
    # In fact, some includers will do just that, if they cannot delegate to an
    # underlying map.
    #
    # This method exists to allow map client implementations that talk to a centralized
    # map server to send one big `decall` request instead of thousands of small `dec`
    # requests. This is beneficial for compression since the majority of `dec` calls
    # are very similar to each other; and is in general a good practice.
    abstract def decall(referrer : Label, keys : Array(K)) : Nil

    # Constructs a submap (see `SubMap`).
    def submap(k : Sk.class, v : Sv.class) forall Sk, Sv
      SubMap(K, V, Sk, Sv).new(self)
    end
  end

  # A view of an `IMap(Pk, Pv)` with a narrowed key-value type `(K, V)`,
  # assuming `K` and `V` are safely castable to `Pk` and `Pv`, respectively.
  #
  # `SubMap` delegates all operations to the underlying map, performing
  # type conversions as needed.
  struct SubMap(Pk, Pv, K, V)
    include IMap(K, V)

    def initialize(@map : IMap(Pk, Pv))
    end

    def latest?(referrer : Label, key : K) : V?
      @map.latest?(referrer, key.as(Pk)).as?(V)
    end

    def size : Int32
      @map.size
    end

    def inc(referrer : Label, key : K, default : V) : V
      @map.inc(referrer, key.as(Pk), default.as(Pv)).as(V)
    end

    def dec(referrer : Label, key : K) : Nil
      @map.dec(referrer, key.as(Pk))
    end

    def decall(referrer : Label, keys : Array(K)) : Nil
      @map.decall(referrer, keys.map &.as(Pk))
    end
  end

  # A synchronous in-memory implementation of `IMap(K, V)`. Uses a lock for
  # thread-safety.
  class SyncInMemoryMap(K, V)
    include IMap(K, V)

    record Cell(T), refs : Bag(Label), value : T

    @data = {} of K => Cell(V)
    @lock = Mutex.new

    def latest?(referrer : Label, key : K) : V?
      return unless cell = @lock.synchronize { @data[key]? }

      cell.value
    end

    def size : Int32
      @data.size
    end

    def inc(referrer : Label, key : K, default : V) : V
      @lock.synchronize do
        if cell = @data[key]?
          cell.refs.add(referrer)
          cell.value
        else
          @data[key] = Cell.new(Bag{referrer}, default)

          default
        end
      end
    end

    def dec(referrer : Label, key : K) : Nil
      @lock.synchronize do
        return unless cell = @data[key]?
        return unless cell.refs.delete?(referrer)
        return unless cell.refs.empty?

        @data.delete(key)
      end
    end

    def decall(referrer : Label, keys : Array(K)) : Nil
      keys.each { |key| dec(referrer, key) }
    end

    def pretty_print(pp)
      @lock.synchronize do
        pp.list("{", @data, "}") do |key, cell|
          pp.group do
            key.pretty_print(pp)
            pp.text ": "
            pp.nest do
              pp.breakable
              cell.value.pretty_print(pp)
            end
          end
        end
      end
    end
  end

  # Raised by `TermMap` if it cannot decode a term into a `V` type. This means
  # `V`'s `decode?` was not able to decode the term; or if `V` is a union, none
  # of its members were able to `decode?` the term.
  class TermDecodeError < Exception
  end

  # An `IMap(K, V)` backed by an `IMap(Term, Term)`, encoding keys and values
  # as `Term`s.
  #
  # Used primarily as an intermediate step for (de)serialization (since `Term`s are
  # the lingua franca of Wirewright).
  #
  # `TermMap` ensures that stored values can be decoded back into `V`, raising
  # `TermDecodeError` if decoding fails.
  class TermMap(K, V)
    include IMap(K, V)

    def initialize(@map : IMap(Term, Term))
    end

    private def decode(term : Term) : V
      {% for type in V.union_types %}
        if object = {{type}}.decode?(term)
          return object.as(V)
        end
      {% end %}

      raise TermDecodeError.new("#{term}")
    end

    def latest?(referrer : Label, key : K) : V?
      return unless value = @map.latest?(referrer, key.encode(Term))

      decode(value)
    end

    def size : Int32
      @map.size
    end

    def inc(referrer : Label, key : K, default : V) : V
      value = @map.inc(referrer, key.encode(Term), default.encode(Term))

      decode(value)
    end

    def dec(referrer : Label, key : K) : Nil
      @map.dec(referrer, key.encode(Term))
    end

    def decall(referrer : Label, keys : Array(K)) : Nil
      @map.decall(referrer, keys.map &.encode(Term))
    end
  end

  # An `IMap(Term, Term)` backed by an `IMap(String, String)`, using CompactML for storage.
  #
  # CompactML is a human-readable, space-efficient subset of WwML, designed to
  # represent `Term`s in a compact string format.
  #
  # `CompactMLMap` serializes terms into their CompactML string representation for
  # efficient storage and retrieval, and deserializes them back to `Term`s on access.
  class CompactMLMap
    include IMap(Term, Term)

    def initialize(@map : IMap(String, String))
    end

    def latest?(referrer : Label, key : Term) : Term?
      return unless value = @map.latest?(referrer, ML.compact(key))

      ML.term(value)
    end

    def size : Int32
      @map.size
    end

    def inc(referrer : Label, key : Term, default : Term) : Term
      value = @map.inc(referrer, ML.compact(key), ML.compact(default))

      ML.term(value)
    end

    def dec(referrer : Label, key : K) : Nil
      @map.dec(referrer, ML.compact(key))
    end

    def decall(referrer : Label, keys : Array(K)) : Nil
      @map.decall(referrer, keys.map { |key| ML.compact(key) })
    end
  end

  # An `IMap(K, V)` backed by an `IMap(String, V)`, storing keys as their digests.
  #
  # `KeyDigestMap` hashes keys using the specified digest algorithm
  # (default: SHA-256) and uses the resulting string as the storage key.
  class KeyDigestMap(K, V)
    include IMap(K, V)

    def initialize(@map : IMap(String, V), @algorithm : Digest::ClassMethods = Digest::SHA256)
    end

    private def digest(key : K) : String
      key.digest(@algorithm, base: 64)
    end

    def latest?(referrer : Label, key : K) : V?
      @map.latest?(referrer, digest(key))
    end

    def size : Int32
      @map.size
    end

    def inc(referrer : Label, key : K, default : V) : V
      @map.inc(referrer, digest(key), default)
    end

    def dec(referrer : Label, key : K) : Nil
      @map.dec(referrer, digest(key))
    end

    def decall(referrer : Label, keys : Array(K)) : Nil
      @map.decall(referrer, keys.map { |key| digest(key) })
    end
  end
end
