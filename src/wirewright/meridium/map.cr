module Ww::Meridium
  module ISet(T)
    abstract def includes?(identity : T) : Bool
    abstract def add(referrer : Label, identity : T) : Nil
    abstract def delete(referrer : Label, identity : T) : Nil

    def subset(cls : St.class) forall St
      SubSet(T, St).new(self)
    end
  end

  struct SubSet(T, St)
    include ISet(St)

    def initialize(@set : ISet(T))
    end

    def includes?(identity : St) : Bool
      @set.includes?(identity.as(T))
    end

    def add(referrer : Label, identity : St) : Nil
      @set.add(referrer, identity.as(T))
    end

    def delete(referrer : Label, identity : St) : Nil
      @set.delete(referrer, identity.as(T))
    end
  end

  class SyncInMemorySet(T)
    include ISet(T)

    @data = {} of T => Set(Label)
    @lock = Mutex.new

    def includes?(identity : T) : Bool
      @lock.synchronize { @data.has_key?(identity) }
    end

    def add(referrer : Label, identity : T) : Nil
      @lock.synchronize do
        referrers = @data.put_if_absent(identity) { Set(Label).new }
        referrers << referrer
      end
    end

    def delete(referrer : Label, identity : T) : Nil
      @lock.synchronize do
        return unless referrers = @data[identity]?
        return unless referrers.delete(referrer)
        return unless referrers.empty?

        @data.delete(identity)
      end
    end

    def clear : Nil
      @lock.synchronize { @data.clear }
    end
  end

  class DigestSet
    include ISet(String)

    def initialize(@set : ISet(String), @algorithm : Digest::ClassMethods = Digest::SHA256)
    end

    private def digest(identity : String) : String
      identity.digest(@algorithm, base: 64)
    end

    def includes?(identity : String) : Bool
      @set.includes?(digest(identity))
    end

    def add(referrer : Label, identity : String) : Nil
      @set.add(referrer, digest(identity))
    end

    def delete(referrer : Label, identity : String) : Nil
      @set.delete(referrer, digest(identity))
    end
  end

  class TermSet(T)
    include ISet(T)

    def initialize(@set : ISet(Term))
    end

    def includes?(identity : T) : Bool
      @set.includes?(Term.encode(identity))
    end

    def add(referrer : Label, identity : T) : Nil
      @set.add(referrer, Term.encode(identity))
    end

    def delete(referrer : Label, identity : T) : Nil
      @set.delete(referrer, Term.encode(identity))
    end
  end

  class CompactMLSet
    include ISet(Term)

    def initialize(@set : ISet(String))
    end

    def includes?(identity : Term) : Bool
      @set.includes?(ML.compact(identity))
    end

    def add(referrer : Label, identity : Term) : Nil
      @set.add(referrer, ML.compact(identity))
    end

    def delete(referrer : Label, identity : Term) : Nil
      @set.delete(referrer, ML.compact(identity))
    end
  end

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
    # the time of assignment.
    abstract def ref(referrer : Label, key : K, default : V) : V

    # Atomically removes *referrer*'s reference to *key*, removing the underlying
    # key-value pair if necessary.
    abstract def unref(referrer : Label, key : K) : Nil

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

    def ref(referrer : Label, key : K, default : V) : V
      @map.ref(referrer, key.as(Pk), default.as(Pv)).as(V)
    end

    def unref(referrer : Label, key : K) : Nil
      @map.unref(referrer, key.as(Pk))
    end
  end

  # A synchronous in-memory implementation of `IMap(K, V)`. Uses a lock for
  # thread-safety.
  class SyncInMemoryMap(K, V)
    include IMap(K, V)

    record Cell(T), refs : Set(Label), value : T

    @data = {} of K => Cell(V)
    @lock = Mutex.new

    def latest?(referrer : Label, key : K) : V?
      return unless cell = @lock.synchronize { @data[key]? }

      cell.value
    end

    def size : Int32
      @data.size
    end

    def ref(referrer : Label, key : K, default : V) : V
      @lock.synchronize do
        if cell = @data[key]?
          cell.refs.add(referrer)
          cell.value
        else
          @data[key] = Cell.new(Set{referrer}, default)

          default
        end
      end
    end

    def unref(referrer : Label, key : K) : Nil
      @lock.synchronize do
        return unless cell = @data[key]?
        return unless cell.refs.delete(referrer)
        return unless cell.refs.empty?

        @data.delete(key)
      end
    end

    def clear : Nil
      @lock.synchronize { @data.clear }
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

  # An `IMap(K, V)` backed by an `IMap(Term, Term)`, encoding keys and values
  # as `Term`s.
  #
  # Used primarily as an intermediate step for (de)serialization (since `Term`s are
  # the lingua franca of Wirewright; and have several further (de)serialization options).
  # See also: `Term.encode`, `Term.decode`.
  class TermMap(K, V)
    include IMap(K, V)

    def initialize(@map : IMap(Term, Term))
    end

    def latest?(referrer : Label, key : K) : V?
      return unless value = @map.latest?(referrer, Term.encode(key))

      Term.decode(V, value)
    end

    def size : Int32
      @map.size
    end

    def ref(referrer : Label, key : K, default : V) : V
      value = @map.ref(referrer, Term.encode(key), Term.encode(default))

      Term.decode(V, value)
    end

    def unref(referrer : Label, key : K) : Nil
      @map.unref(referrer, Term.encode(key))
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

    def ref(referrer : Label, key : Term, default : Term) : Term
      value = @map.ref(referrer, ML.compact(key), ML.compact(default))

      ML.term(value)
    end

    def unref(referrer : Label, key : K) : Nil
      @map.unref(referrer, ML.compact(key))
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

    def ref(referrer : Label, key : K, default : V) : V
      @map.ref(referrer, digest(key), default)
    end

    def unref(referrer : Label, key : K) : Nil
      @map.unref(referrer, digest(key))
    end
  end
end
