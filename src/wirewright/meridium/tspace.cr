module Ww::Meridium
  # Implementations are sets or set-like objects that store `Atom`s produced
  # by e.g. a `Conn`.
  module IAtomSet
    include IAtomAppend
    include IAtomsPresent

    alias Submit = Atom ->

    # Yields two procs: the first one is to *add*, and the second one is to
    # *remove* atoms. Whether each atom is submitted separately or all are
    # submitted in batch is implementation-defined; this method is simply
    # a hint for that. Similarly, the order of additions/removals is
    # implementation-defined. Callers aren't expected to care about that
    # at this point.
    abstract def transaction(& : Submit, Submit ->) : Nil
  end

  # Implementations are "chats" where connections can send messages -- "write" --
  # to each other, knowing only each other's connection id -- *conid* for short.
  module IActivationChat
    alias Subscribe = ->
    alias Unsubscribe = ->

    # Returns a pair of procs: the first is to subscribe *conid* to activations
    # from the chat, and the second is to unsubscribe it (noop if subscribed).
    #
    # This method itself does not create any state; there is no need to "disconnect".
    abstract def connect(conid : WWID, &recv : Activation ->) : {Subscribe, Unsubscribe}

    # Sends *act* to *conid*.
    #
    # Note that we do not guarantee delivery, for one because *conid* might not
    # exist at this point.
    abstract def send(conid : WWID, act : Activation) : Nil
  end

  # A *termspace* is an entity that acts both as an `IAtomSet` and an `IActivationChat`.
  # In other words, a *termspace* provides both communication and set-like storage
  # capabilities for connections. Implementations can range from simple in-memory ones
  # to ones that use the client-server model; hub and spokes model; and beyond to
  # distributed (e.g. through consensus) or even emergent models.
  module Tspace
    include IAtomSet
    include IActivationChat
  end

  # A simple in-memory `IActivationChat` protected by a lock (and therefore thread-safe).
  class SyncActivationChat
    include IActivationChat

    @subscribers = {} of WWID => Set(Activation ->)
    @lock = Mutex.new

    def connect(conid : WWID, &recv : Activation ->) : {Subscribe, Unsubscribe}
      sub = Subscribe.new do
        @lock.synchronize do
          recvs = @subscribers.put_if_absent(conid) { Set(Activation ->).new }
          recvs << recv
        end

        nil
      end

      unsub = Unsubscribe.new do
        @lock.synchronize do
          next unless recvs = @subscribers[conid]?
          next unless recvs.delete(recv)
          next unless recvs.empty?

          @subscribers.delete(conid)
        end

        nil
      end

      {sub, unsub}
    end

    def send(conid : WWID, act : Activation) : Nil
      recvs = @lock.synchronize do
        # Copy receiver procs (if any) so that we can call them outside of the lock,
        # and so that they're "frozen in time".
        @subscribers[conid]?.try(&.dup)
      end

      return unless recvs

      recvs.each &.call(act)
    end
  end

  # Implements `transaction` and multi-atom `present?` serially, that is,
  # without any kind of batching: the atoms are added, removed, or looked
  # up one after another.
  module SerialAtomSet
    include IAtomSet

    # Removes the given *atom*.
    abstract def delete(atom : Atom) : Nil

    def transaction(& : Submit, Submit ->) : Nil
      yield ->(atom : Atom) { self << atom; nil }, ->delete(Atom)
    end

    def present?(objects : Enumerable(T), & : T -> Atom | Enumerable(Atom)) : BitList forall T
      answer = BitList.new

      objects.each do |object|
        ee = yield object

        unless ee.is_a?(Enumerable(Atom))
          ee = {ee}
        end

        ee.each do |atom|
          answer << present?(atom)
        end
      end

      answer
    end
  end

  # A simple in-memory atom multiset protected by a lock (and therefore
  # thread-safe).
  struct SyncAtomMultiset
    include IAtomSet
    include SerialAtomSet

    @hash = {} of Atom => UInt32
    @lock = Mutex.new

    def present?(atom : Atom) : Bool
      @lock.synchronize { @hash.has_key?(atom) }
    end

    def <<(atom : Atom)
      @lock.synchronize do
        @hash[atom] = (@hash[atom]? || 0u32) + 1
      end

      self
    end

    def delete(atom : Atom) : Nil
      @lock.synchronize do
        return unless tally = @hash[atom]?

        if tally == 1
          @hash.delete(atom)
        else
          @hash[atom] = tally - 1
        end
      end
    end
  end

  # A simple in-memory atom multiset that consists of `N` `SyncAtomMultiset`
  # buckets; this means that by using this implementation, you gain a bit
  # more parallelism by reducing contention.
  struct SyncBucketedAtomMultiset(N)
    include IAtomSet
    include SerialAtomSet

    @buckets = Slice(SyncAtomMultiset).new(N) { SyncAtomMultiset.new }

    def present?(atom : Atom) : Bool
      bucket = @buckets[atom.@blk0 % N]
      bucket.present?(atom)
    end

    def <<(atom : Atom) : self
      bucket = @buckets[atom.@blk0 % N]
      bucket << atom

      self
    end

    def delete(atom : Atom) : Nil
      bucket = @buckets[atom.@blk0 % N]
      bucket.delete(atom)
    end
  end
end
