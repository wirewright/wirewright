module Ww::Soma::DwUIR
  # Platforms are "machines" that convert `DrawKey`s to raster images thereof,
  # in the form of `Layer`s. They are also responsible for handling pencil
  # requests since those are two are very intertwined.
  module Platform
    # Returns a proc that handles pencil requests.
    abstract def pencils : (PencilRequest -> IPencil)

    # Converts *key* to a `Layer` using this platform.
    #
    # The resource manager at *resources* is currently only used to load images.
    abstract def layer_for(resources : ResourceLoader, key : DrawKey) : Layer
  end
end
