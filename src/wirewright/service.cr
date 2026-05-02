module Ww
  # Implements `listen`, `broadcast`, and some auxiliary functions for services to
  # broadcast `Notification`s conveniently and for listeners to receive them.
  module ServiceBroadcast(Notification)
    macro included
      @@listener_queue_lock = ::Sync::Mutex.new
      @@listener_queues = Set(BlockingQueue({{Notification}})).new.compare_by_identity
    end

    # Broadcasts *notification* to all listeners.
    def broadcast(notification : Notification) : Nil
      Log.trace { "broadcast(#{notification})" }

      @@listener_queue_lock.synchronize do
        @@listener_queues.each do |queue|
          queue << notification
        end
      end
    end

    # Taps the block into the stream of notifications broadcast by the service.
    # The calling fiber blocks while waiting for notifications.
    #
    # *wg* can be used by the client to guarantee that a listener is set up before
    # the client proceeds, to prevent message loss.
    def listen(wg : WaitGroup = WaitGroup.new(1), & : Notification ->)
      queue = BlockingQueue(Notification).new

      @@listener_queue_lock.synchronize do
        @@listener_queues << queue
      end

      wg.done

      Log.trace { "listen()ing on queue 0x#{queue.object_id.to_s(base: 16)}" }

      begin
        loop do
          notification = queue.shift
          yield notification
        end
      ensure
        @@listener_queue_lock.synchronize do
          @@listener_queues.delete(queue)
        end

        Log.trace { "stop listen()ing on queue 0x#{queue.object_id.to_s(base: 16)}" }
      end
    end

    # Blocks the calling fiber until a notification is emitted.
    def wait(*args) : Nil
      listen(*args) { break }
    end
  end
end

require "./service/*"
