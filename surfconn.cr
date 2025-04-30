require "./src/wirewright"

Log.setup_from_env(default_level: :debug)

include Meridium

alias AtomFn = Atom ->

module IAtomSet
  include IAtomsPresent

  abstract def transaction(& : AtomFn, AtomFn ->) : Nil
end

module IChat(M)
  alias Subscribe = ->
  alias Unsubscribe = ->

  abstract def connect(conid : WWID, &recv : M ->) : {Subscribe, Unsubscribe}
  abstract def send(conid : WWID, message : M) : Nil
end

class SyncInMemoryChat(M)
  include IChat(M)

  @subscribers = {} of WWID => Set(M ->)
  @lock = Mutex.new

  def connect(conid : WWID, &recv : M ->) : {Subscribe, Unsubscribe}
    sub = Subscribe.new do
      @lock.synchronize do
        recvs = @subscribers.put_if_absent(conid) { Set(M ->).new }
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

  def send(conid : WWID, message : M) : Nil
    recvs = @lock.synchronize do
      # Copy receiver procs (if any) so that we can call them outside of the lock,
      # and so that they're "frozen in time".
      @subscribers[conid]?.try(&.dup)
    end

    return unless recvs

    recvs.each &.call(message)
  end
end

module SerialAtomSet
  include IAtomAppend

  abstract def delete(atom : Atom) : Nil

  def transaction(& : AtomFn, AtomFn ->) : Nil
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

struct SyncAtomMultiset
  include IAtomAppend
  include IAtomSet
  include SerialAtomSet

  def initialize
    @hash = {} of Atom => UInt32
    @lock = Mutex.new
  end

  def size
    @lock.synchronize { @hash.size }
  end

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

struct SyncBucketedAtomMultiset(N)
  include IAtomSet
  include SerialAtomSet

  def initialize
    @buckets = Slice(SyncAtomMultiset).new(N) { SyncAtomMultiset.new }
  end

  def size
    @buckets.sum(&.size)
  end

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

{% skip_file %}
MT.spawn do
  set = SyncBucketedAtomMultiset(1024).new
  chat = SyncInMemoryChat(Activation).new
  n = Atomic(Int32).new(0)
  conn = Conn.new(set, chat) do |c|
    # pp c.view.dict_multisets
    if n.add(1) % 1000 == 0
      Log.notice { "#{n}" }
    end
  end
  conn.summon

  conn[0] = Sensor.new(Term.of(:+, :a_number, :b_number), relook: nil)
  # try to trigger races bugs etc
  spawn do
    (0...100_000).each do |n|
      conn[1] = Appearance.new(Term.of(:+, n, n))
    end
  end
  spawn do
    (100_000...200_000).each do |n|
      conn[2] = Appearance.new(Term.of(:+, n, n))
    end
  end
  # conn.each do |slot, surface|
  #   puts "Conn has #{slot} #{surface}"
  # end

  # conn.clear
end
sleep
