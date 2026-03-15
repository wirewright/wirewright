module Ww::DwUIR
  # Platforms are "machines" that convert `DrawKey`s to raster images thereof,
  # returned as `Layer`s.
  #
  # Platforms also host / point to a suite of related objects, such as the pencil
  # server `pencils` or the image server `images`.
  module Platform
    # Returns a proc that handles pencil requests.
    abstract def pencils : PencilServer

    # Returns the image server associated with this platform.
    abstract def images : ImageServer

    # Converts *key* to a `Layer` using this platform.
    abstract def layer_for(key : DrawKey) : Layer
  end

  # An image server is the part of a platform that keeps/caches images
  # in the platform-specific representation.
  module ImageServer
    # Loads the image at *src* into memory. Returns the resulting `Image`.
    #
    # Raises `ImageServerError` if something went wrong.
    abstract def load(src : Term) : Image

    # Unloads the image at *src* from memory if it was loaded.
    abstract def unload(src : Term) : Nil
  end

  # Raised when the image server cannot load an image.
  class ImageServerError < Exception
  end

  # Represents an image response returned by an `ImageServer`. Specific
  # image servers may enhance this; this interface defines the most
  # basic requirements that image responses must meet.
  module Image
    # Returns the size of this image.
    abstract def size : Point
  end
end
