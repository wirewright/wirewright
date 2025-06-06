module Ww::Soma::DwUIR
  # Keeps track of resources for DwUIR.
  class ResourceLoader
    @images = {} of Path => Bytes

    # Loads the resource at *path* into memory. Returns the resulting byte slice.
    #
    # NOTE: This method expects *path* to be an absolute path.
    def ref?(spec : Term) : Bytes?
      Term.case(spec) do
        matchpi %{(file path_string)} do |path|
          path = Path[path.to(String)]

          unless path.absolute?
            Log.debug { "path is not an absolute path: #{path}" }
            return
          end

          path = path.normalize

          @images.put_if_absent(path) do
            File.open(path, "rb", &.getb_to_end)
          end
        rescue e : File::Error
          Log.debug(exception: e) { "failed to read resource from disk" }
        end

        otherwise do
          Log.debug { "invalid or unsupported resource spec" }
        end
      end
    end

    # Unloads the image at *path* from memory, if one was loaded.
    def unref(path : Path) : Nil
      @data.delete(path)
    end
  end
end
