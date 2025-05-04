module Ww::Meridium
  alias AtomSubmit = Atom ->

  module Tspace
    abstract def present?(conn : Conn, atoms : AtomSource) : BitList
    abstract def send(conn : Conn, act : Activation) : Nil
    abstract def transaction(conn : Conn, & : AtomSubmit, AtomSubmit ->) : Nil
    abstract def subscribe(conn : Conn) : Nil
    abstract def unsubscribe(conn : Conn) : Nil

    # :nodoc:
    struct Presences
      include IAtomsPresent

      def initialize(@tspace : Tspace, @conn : Conn)
      end

      def present?(atoms : AtomSource) : BitList
        @tspace.present?(@conn, atoms)
      end
    end

    def presences(conn : Conn) : IAtomsPresent
      Presences.new(self, conn)
    end
  end

  module Tspace::Meetable
    abstract def conid : WWID
    abstract def meet(tspace : Tspace) : Nil
  end

  # alias ChatSubscribe = ->
  # alias ChatUnsubscribe = ->

  # # Implementations are "chats" where connections can send messages -- "write" --
  # # to each other, knowing only each other's connection id -- *conid* for short.
  # module IActivationChatClient
  #   abstract def subscribe(&recv : Activation ->) : Nil

  #   abstract def unsubscribe : Nil

  #   # Sends *act* to *receiver*.
  #   #
  #   # Note that we cannot guarantee delivery, for one because *receiver* might not
  #   # exist at this point.
  #   abstract def send(receiver : WWID, act : Activation) : Nil
  # end

  # # A simple in-memory activation chat protected by a lock (and therefore thread-safe).
  # module SyncActivationChat
  # end

  # # Server side of `SyncActivationChat`.
  # class SyncActivationChat::Server
  #   @subscribers = {} of WWID => Set(Activation ->)
  #   @lock = Mutex.new

  #   def subscribe(conid : WWID, &recv : Activation ->)
  #     @lock.synchronize do
  #       recvs = @subscribers.put_if_absent(conid) { Set(Activation ->).new }
  #       recvs << recv
  #     end
  #   end

  #   def unsubscribe(conid : WWID) : Nil
  #     @lock.synchronize do
  #       next unless recvs = @subscribers[conid]?
  #       next unless recvs.delete(recv)
  #       next unless recvs.empty?

  #       @subscribers.delete(conid)
  #     end
  #   end

  #   def send(conid : WWID, act : Activation) : Nil
  #     recvs = @lock.synchronize do
  #       # Copy receiver procs (if any) so that we can call them outside of the lock,
  #       # and so that they're "frozen in time".
  #       @subscribers[conid]?.try(&.dup)
  #     end

  #     return unless recvs

  #     recvs.each &.call(act)
  #   end
  # end

  # # Client side for `SyncActivationChat`.
  # struct SyncActivationChat::Client
  #   include IActivationChatClient

  #   def initialize(@server : Server, @conid : WWID)
  #   end

  #   def subscribe(&recv : Activation ->) : Nil
  #     @server.subscribe(@conid, &recv)
  #   end

  #   def unsubscribe : Nil
  #     @server.unsubscribe(@conid)
  #   end

  #   def send(receiver : WWID, act : Activation) : Nil
  #     @server.send(receiver, act)
  #   end
  # end

  module IAtomsTransact
    # Yields two procs: the first one is to *add*, and the second one is to
    # *remove* atoms. Whether each atom is submitted separately or all are
    # submitted in batch is implementation-defined; this method is simply
    # a hint for that. Similarly, the order of additions/removals is
    # implementation-defined. Callers aren't expected to care about that
    # at this point.
    abstract def transaction(& : AtomSubmit, AtomSubmit ->) : Nil
  end

  # Sets or set-like objects that store & allow to query `Atom`s produced by
  # e.g. a `Conn`.
  # struct AtomSet
  #   getter presence : IAtomsPresent
  #   getter content : IAtomsTransact

  #   def initialize(@presence, @content)
  #   end
  # end

  # # Implements `transaction` and multi-atom `present?` serially, that is,
  # # without any kind of batching: the atoms are added, removed, or looked
  # # up one after another.
  # module SerialAtomSet
  #   include IAtomAppend
  #   include IAtomsPresent
  #   include IAtomsTransact

  #   # Returns `true` if *atom* exists. Returns `false` otherwise.
  #   abstract def present?(atom : Atom) : Bool

  #   # Removes the given *atom*.
  #   abstract def delete(atom : Atom) : Nil

  #   def transaction(& : AtomSubmit, AtomSubmit ->) : Nil
  #     add = ->(atom : Atom) { self << atom; nil }
  #     del = ->delete(Atom)

  #     yield add, del
  #   end

  #   def present?(atoms : AtomSource) : BitList
  #     answer = BitList.new
  #     atoms.each do |atom|
  #       answer << present?(atom)
  #     end
  #     answer
  #   end
  # end

  # # A simple in-memory atom multiset protected by a lock (and therefore
  # # thread-safe).
  # struct SyncAtomMultiset
  #   include SerialAtomSet

  #   @hash = {} of Atom => UInt32
  #   @lock = Mutex.new

  #   def present?(atom : Atom) : Bool
  #     @lock.synchronize { @hash.has_key?(atom) }
  #   end

  #   def <<(atom : Atom) : self
  #     @lock.synchronize do
  #       @hash[atom] = (@hash[atom]? || 0u32) + 1
  #     end

  #     self
  #   end

  #   def delete(atom : Atom) : Nil
  #     @lock.synchronize do
  #       return unless tally = @hash[atom]?

  #       if tally == 1
  #         @hash.delete(atom)
  #       else
  #         @hash[atom] = tally - 1
  #       end
  #     end
  #   end
  # end

  # # A simple in-memory atom multiset that consists of `N` `SyncAtomMultiset`
  # # buckets; this means that by using this implementation, you gain a bit
  # # more parallelism by reducing contention.
  # struct SyncBucketedAtomMultiset(N)
  #   include SerialAtomSet

  #   @buckets = Slice(SyncAtomMultiset).new(N) { SyncAtomMultiset.new }

  #   def present?(atom : Atom) : Bool
  #     bucket = @buckets[atom.@blk0 % N]
  #     bucket.present?(atom)
  #   end

  #   def <<(atom : Atom) : self
  #     bucket = @buckets[atom.@blk0 % N]
  #     bucket << atom

  #     self
  #   end

  #   def delete(atom : Atom) : Nil
  #     bucket = @buckets[atom.@blk0 % N]
  #     bucket.delete(atom)
  #   end
  # end

  # FIXME: do not include IAtomsPresent here; implement as a wrapper method or smth!!!!

  # Equips each `IAtomsPresent` presence query with a number of sanity checks;
  # namely *negative* and *consistency* checks. After the successor `presence?`
  # returns, verifies the resulting bit list -- makes sure the negative checks
  # and consistency checks pass before returning to the caller. If any check fails,
  # returns an all-zero bit list to make sure the caller halts quickly.
  class CheckedAtomsPresence
    include IAtomsPresent

    def initialize(@successor : IAtomsPresent, @random : Random = Random::Secure)
    end

    private def negatom(hasher : Atom::Hasher) : Atom
      Meridium.h(hasher, Meridium.h(hasher, :negative), Atom.rand(@random))
    end

    def present?(atoms : AtomSource) : BitList
      hasher = Atom::Hasher.new

      array = [] of {Atom, Int32}
      atoms.each_with_index do |atom, index|
        array << {atom, index}
      end

      size0 = array.size

      # - Sample 3%-5% of selection for consistency check.
      # - Emit 3%-5% of selection for negative check.
      check_percent = @random.rand(0.03..0.05)
      check_count = (check_percent * array.size).ceil.to_i

      checks_consistency = array.sample(check_count, @random)
      array.concat(checks_consistency)
      check_count.times do
        array << {negatom(hasher), array.size}
      end

      answer = @successor.present?(array) { |atom, _| atom }

      unless answer.size == array.size
        Log.debug { "reject present? response: invalid size (#{array.size})" }
        return BitList.zeros(size0)
      end

      if (answer.size - check_count...answer.size).any? { |i| answer[i] }
        Log.debug { "reject present? response: negative check failed" }
        return BitList.zeros(size0)
      end

      consistent = (0...check_count).all? do |offset|
        i = answer.size - check_count*2 + offset
        _, j = checks_consistency[offset]
        answer[i] == answer[j]
      end

      unless consistent
        Log.debug { "reject present? response: positive check failed" }
        return BitList.zeros(size0)
      end

      answer.resize(size0)
      answer
    end
  end

  # A *termspace* is an entity that acts both as an `IAtomSet` and an `IActivationChat`.
  # In other words, a *termspace* provides both communication and set-like storage
  # capabilities for connections. Implementations can range from simple in-memory ones
  # to ones that use the client-server model; hub and spokes model; and beyond to
  # distributed (e.g. through consensus) or even emergent models.
  # module Tspace
  #   include IAtomsPresent
  #   include IAtomsTransact
  # end
end
