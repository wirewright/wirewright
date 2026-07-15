module Ww
  defrecord StopListening, target : ServiceBroadcast::QueueId, wg : WaitGroup

  # Implements `listen`, `broadcast`, and some auxiliary functions for services to
  # broadcast `Notification`s conveniently and for listeners to receive them.
  module ServiceBroadcast(Notification)
    macro included
      @@listener_queue_lock = ::Sync::Mutex.new
      @@listener_queues = Set(BlockingQueue({{Notification}} | StopListening)).new.compare_by_identity
    end

    # Broadcasts *notification* to all listeners.
    def broadcast(notification : Notification | StopListening) : Nil
      Log.trace { "broadcast(#{notification.class})" }

      @@listener_queue_lock.synchronize do
        @@listener_queues.each do |queue|
          queue << notification
        end
      end
    end

    @@qid = Atomic(UInt64).new(0u64)

    defrecord QueueId, id : UInt64

    def self.qid : QueueId
      QueueId.new(@@qid.add(1, :relaxed))
    end

    # Taps the block into the stream of notifications broadcast by the service.
    # The calling fiber blocks while waiting for notifications.
    #
    # *wg* can be used by the client to guarantee that a listener is set up before
    # the client proceeds, to prevent message loss.
    def listen(wg : WaitGroup | BlockingQueue(QueueId) = WaitGroup.new(1), & : Notification ->)
      queue = BlockingQueue(Notification | StopListening).new
      qid = ServiceBroadcast.qid

      @@listener_queue_lock.synchronize do
        @@listener_queues << queue
      end

      case wg
      in WaitGroup
        wg.done
      in BlockingQueue(QueueId)
        wg << qid
      end

      Log.trace { "listen()ing on queue 0x#{queue.object_id.to_s(base: 16)} (qid #{qid})" }

      begin
        loop do
          notification = queue.shift
          if notification.is_a?(StopListening)
            if qid == notification.target
              notification.wg.done
              break
            end

            next
          end

          yield notification
        end
      ensure
        @@listener_queue_lock.synchronize do
          @@listener_queues.delete(queue)
        end

        Log.trace { "stop listen()ing on queue 0x#{queue.object_id.to_s(base: 16)} (qid #{qid})" }
      end
    end

    # Blocks the calling fiber until a notification is emitted.
    def wait(*args) : Nil
      listen(*args) { break }
    end
  end
end

require "./service/*"
