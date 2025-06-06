module Ww::Soma::DwUIR
  # Measures *leaf* if it requested measurement. Returns the resulting (measured
  # or unchanged) version of *leaf*.
  #
  # The main mode of talking to this function is placing `intrinsic-w: ?` or
  # `intrinsic-h: ?` queries on *leaf*. See also: `soma.dwuir.measured`.
  def measured(platform : Platform, leaf : Term) : Term
    Term.of_case(leaf) do
      # |@ soma.dwuir.measured.text
      #
      # |@block
      # Answers `text`'s query about its intrinsic-h in the presence of final-w.
      # If wrapping is enabled will set intrinsic-h to the wrapped height of
      # the text.
      # |@endblock
      #
      # TODO: cache
      matchpi %{(text ¦ _ final-w_number intrinsic-h: ?)} do
        wrap_extent = Point.new(final_w.to(Float32), Float32::INFINITY)

        continue unless spec = text_spec?(leaf, platform.pencils, wrap_extent)

        bounds = Rect.empty
        spec.each_text_drawable(platform.pencils) do |dw|
          bounds |= dw.bounds
        end

        leaf.morph({:"intrinsic-h", bounds.h})
      end

      # |@ soma.dwuir.measured.text
      #
      # |@block
      # Answers `text`'s query about either or both of its intrinsic-w, intrinsic-h.
      # |@endblock
      #
      # TODO: cache
      matchpi(
        %{(text ¦ _ intrinsic-w: ? intrinsic-h: ?)},
        %{(text ¦ _ intrinsic-w: ?)},
        %{(text ¦ _ intrinsic-h: ?)},
      ) do
        wrap_extent = Point.inf

        continue unless spec = text_spec?(leaf, platform.pencils, wrap_extent)

        size = Rect.empty
        spec.each_text_drawable(platform.pencils) do |dw|
          size |= dw.bounds
        end

        answer = leaf

        Term.case(leaf) do
          matchpi %[{¦ content-w: ?}] do
            answer = answer.morph({:"content-w", size.w})
            continue
          end

          matchpi %[{¦ content-h: ?}] do
            answer = answer.morph({:"content-h", size.h})
            continue
          end

          otherwise { answer }
        end
      end

      # |@ soma.dwuir.measured.rect
      #
      # |@block
      # Answers the rectangle's query about its intrinsic width and height if
      # its fill is an image. The image's dimensions will be retrieved and used
      # to answer the query.
      # |@endblock
      matchpi %{(rect ¦ _ fill_: [image src_] intrinsic-w: ? intrinsic-h: ?)} do
        begin
          image = platform.images.load(src)
        rescue e : ImageServerException
          Log.warn(exception: e) { "could not measure rect" }

          continue
        end

        leaf.morph(
          {:"intrinsic-w", image.size.x},
          {:"intrinsic-h", image.size.y},
        )
      end

      otherwise { leaf }
    end
  end
end
