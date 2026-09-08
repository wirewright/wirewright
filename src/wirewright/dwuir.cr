module Ww::DwUIR
  extend self

  Log = ::Log.for(self)
end

require "./dwuir/font"

require "./dwuir/tf"
require "./dwuir/magn"
require "./dwuir/pencil"
require "./dwuir/point"
require "./dwuir/segment"
require "./dwuir/rect"
require "./dwuir/quad"
require "./dwuir/paint"
require "./dwuir/draw_command"
require "./dwuir/layer"
require "./dwuir/pixel"

require "./dwuir/wrap_token"
require "./dwuir/text_element"
require "./dwuir/text_command"
require "./dwuir/text_drawable"
require "./dwuir/picture"
require "./dwuir/walk"
require "./dwuir/render"
require "./dwuir/hit"

require "./dwuir/pixel_rect"
require "./dwuir/compositor"
require "./dwuir/viewer"
require "./dwuir/replier"
require "./dwuir/snap"

require "./dwuir/pvg"
require "./dwuir/pvg_image_server"

require "./dwuir/window"
require "./dwuir/textual"

module Ww::DwUIR
  # Groups configuration for the `show` shorthand function.
  #
  # - *width* is the width of the resulting image, in pixels.
  # - *height* is the height of the resulting image, in pixels.
  # - *backdrop* is the clear color of the resulting image.
  # - *content* is the DwUIR to make a snapshot of.
  record ShowConf,
    width : Int32,
    height : Int32,
    backdrop : Pigment::RGBA,
    content : Term

  # A shorthand function to perform a single, one-off draw of *content*.
  # Returns the resulting pixel rectangle `PixelRect`.
  def show(ctx : Viewer::Context, conf : ShowConf)
    screen = PixelRect.new(0, 0, conf.width, conf.height)
    viewer = Viewer.new(screen, ctx)
    viewer.show(conf.content, bg: conf.backdrop)

    screen
  end

  # Facilitates interaction with a DwUIR `server`.
  alias Request = FrameRequest | SnapRequest | TextReplyRequest | GraphicsReplyRequest

  # Corresponds to `Viewer#show`.
  defcase FrameRequest,
    content : Term,
    pixels : PixelRect,
    backdrop : Pigment::RGBA,
    response : Sync::Future(FrameResponse)

  defcase FrameResponse, damage : Array(Rect)

  # Corresponds to `DwUIR::Textual.reply`.
  defcase TextReplyRequest,
    subject : Term,
    response : Sync::Future(Term)

  # Corresponds to `DwUIR.reply`.
  defcase GraphicsReplyRequest,
    subject : Term,
    response : Sync::Future(Term)

  # Corresponds to `snap`.
  defcase SnapRequest,
    conf : ShowConf,
    format : SnapFormat::Sink,
    response : Sync::Future(Bytes) | Sync::Future(String)

  # Starts a thread that will handle DwUIR requests. All rendering and measurement
  # should take place on this thread through `Request`s, since DwUIR is deeply single-
  # threaded. Yields a channel through which you should make requests. The thread
  # is stopped and the channel is closed when this function returns.
  #
  # NOTE: You transfer ownership of *ctx* to this function until it returns.
  def serve(ctx : Viewer::Context, & : Channel(Request) -> _)
    dw = Channel(Request).new
    worker = WaitGroup.new(1)

    begin
      Fiber::ExecutionContext::Isolated.new("DwUIR Server", spawn_context: MT) do
        while request = dw.receive?
          begin
            case request
            in FrameRequest
              viewer = Viewer.new(request.pixels, ctx)
              damage = viewer.show(request.content, bg: request.backdrop)
              request.response.set(FrameResponse.new(damage))
            in SnapRequest
              case response = request.response
              in Sync::Future(Bytes)
                io = IO::Memory.new
                snap(io, ctx, request.conf, request.format)
                response.set(io.to_readonly_slice)
              in Sync::Future(String)
                src = IO::Memory.new
                snap(src, ctx, request.conf, request.format)
                response.set(Base64.urlsafe_encode(src))
              end
            in TextReplyRequest
              result = Textual.reply(request.subject)
              request.response.set(result)
            in GraphicsReplyRequest
              result = reply(ctx.platform, request.subject)
              request.response.set(result)
            end
          rescue e : Exception
            request.response.fail(e)
          end
        end
      ensure
        worker.done
      end

      yield dw
    ensure
      dw.close
      worker.wait
    end
  end
end
