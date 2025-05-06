# Axis is a fully centralized, client-server implementation of a termspace.
module Ww::Meridium::Axis
  alias AxT = Proto::Token

  # Specifies buffer size on buffered IOs.
  BUFFER_SIZE = 8192

  # Specifies write timeout on `Socket` IOs.
  WRITE_TIMEOUT = 16.seconds

  # NOTE: this timeout isn't used directly on IO reads, since we're supposed
  # to listen indefinitely on both ends. It is used however in cases when
  # an answer *must* be received to make any progress.
  READ_TIMEOUT = 16.seconds

  # Raised when there is an IO/network error, e.g. the IO is closed mid-send,
  # timeout, etc.
  class NetworkError < Exception
  end

  # Raised when the Axis protocol (`Proto`) is violated. Remotely similar to
  # syntax errors in programming languages.
  class ProtocolError < Exception
    getter? quiet : Bool

    def initialize(*args, @quiet : Bool = false, **kwargs)
    end
  end

  # Lets Axis configure *io* as it desires.
  def self.configure(io : IO)
    if io.is_a?(IO::Buffered)
      io.buffer_size = BUFFER_SIZE
    end

    if io.is_a?(Socket)
      io.write_timeout = WRITE_TIMEOUT
    end

    if io.is_a?(TCPSocket)
      io.tcp_nodelay = true
    end
  end
end

module Ww::Meridium
  # `Tspace` implementation that uses `Meridium::Axis`. It is designed to be as
  # plug-and-play as possible, abstracting away connection establishment,
  # communication, automatic reconnect, and connection failure.
  class Tspace::Axis
    include Tspace::IBookMeeting

    Log = ::Log.for(self)

    enum Status : UInt8
      # Connection with the termspace present.
      Online

      # Connection with the termspace absent.
      Offline

      # Connection with the termspace was suspended due to misbehavior on our
      # or the termspace's end. A manual re-`connect` is required.
      Suspended
    end

    def initialize(&@connect : -> IO)
      @running = false
      @status = Status::Offline
      @bookings = Channel(Tspace::Meetable).new
      @subscribers = Set(IConn).new
      @lock = Mutex.new
    end

    # Returns the termspace status.
    def status : Status
      @lock.synchronize { @status }
    end

    def book(meetable : Tspace::Meetable) : Nil
      chan = @lock.synchronize do
        unless @status.online?
          Log.trace { "ignore book(): call because status=#{@status}" }
          return
        end

        @bookings
      end

      begin
        chan.send(meetable)
      rescue e : Channel::ClosedError
        Log.debug(exception: e) { "ignore book(): bookings chan was closed in-flight" }
      end
    end

    # Subscribes *conn* to notifications about `status`.
    def subscribe(conn : IConn) : Nil
      online = @lock.synchronize do
        @subscribers << conn
        @running && @status.online?
      end

      if online
        conn.online
      end
    end

    # Unsubscribes *conn* from notifications about `status`.
    def unsubscribe(conn : IConn) : Nil
      @lock.synchronize { @subscribers.delete(conn) }

      conn.offline
    end

    # Starts the connect loop in the calling fiber. The connect loop runs until
    # `disconnect` is called, or the connection is suspended due to misbehavior
    # (exception on our end or protocol violation on the termspace's end).
    #
    # The connect loop can be restarted after `disconnect`. The connect loop won't
    # be started if one is running already.
    def connect : Nil
      @lock.synchronize do
        return if @running

        @running = true

        if @bookings.closed?
          @bookings = Channel(Tspace::Meetable).new
        end
      end

      reconnects = 0

      loop do
        Log.debug { "connecting to server" }

        begin
          io = @connect.call
        rescue e : IO::Error | Socket::ConnectError
          reconnects += 1
          nap = (200 * 2**reconnects).milliseconds
          if nap > 30.seconds
            nap = 30.seconds
          end

          Log.debug(exception: e) { "connection failed, nap=#{nap.total_seconds.round(2)}s" }

          start0 = Time.monotonic

          # Catch any bookings in the interim and ignore them. We'll notify them
          # later on when we're able to connect. Assuming whomever is booking is
          # subscribed to our notifications, that is!
          loop do
            select
            when meetable = @bookings.receive?
              return unless meetable

              start1 = Time.monotonic
              nap -= (start1 - start0)
              start0 = start1

              Log.debug { "woken up by #{meetable}, ignore; nap=#{nap.total_seconds.round(2)}s" }
            when timeout(nap)
              break
            end
          end

          next
        end

        Meridium::Axis.configure(io)

        reconnects = 0

        @lock.synchronize do
          @status = Status::Online
        end

        # Notify all of our subscribers that we're online now. This almost certainly will
        # trigger a number of sends to @bookings. We must be careful since the mainloop
        # isn't running yet; nor are we able to handle the @bookings. That's why we spawn
        # here; the fiber will block on @bookings.send.
        spawn do
          subscribers = @lock.synchronize { @subscribers.dup }
          subscribers.each(&.online)
        end

        state = Meridium::Axis::Client.run(io, @bookings)

        Log.debug { "run terminated with state=#{state}" }

        @lock.synchronize do
          case state
          in .online?
            raise "BUG: online state after run ended"
          in .offline?
            # state=offline is used to notify us about an abnormal disconnect.
            @status = Status::Offline
          in .closed?
            # state=closed is used to notify us about a nominal disconnect. It is triggered
            # by `disconnect` (closure of the bookings channel).
            @status = Status::Offline
            return
          in .suspended?
            # state=suspended is used to notify us that the connection could be online,
            # but the server violated the protocol; so we terminate the connection
            # preventively and wait until the user manually reconnects.
            @status = Status::Suspended
            return
          end
        ensure
          @subscribers.each(&.offline)
        end
      end
    ensure
      Log.debug { "awaiting manual connect in state=#{status}" }

      @lock.synchronize { @running = false }
    end

    # Ends the connect loop and disconnects from the termspace.
    def disconnect : Nil
      @bookings.close
    end
  end
end

require "./axis/proto"
require "./axis/client"
require "./axis/session"
require "./axis/server"
