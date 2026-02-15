module Ww::DwUIR
  # Responds to measurement queries embedded within *subject*.
  #
  # *subject* may include `dw-request` as part of its structure, effectively
  # querying its own layout properties; this method interprets that request,
  # computes the necessary values (such as width and height), and returns
  # a transformed version of *subject* with the query resolved and replaced
  # by the answer.
  #
  # See `soma.dwuir.replier` in the doctool to learn more.
  def reply(platform : Platform, subject : Term) : Term
    # Fast paths for the vast majority of subjects.
    return subject unless subject.type.dict?
    return subject unless subject.includes?(:"dw-request")

    Term.case(subject) do
      # |@ soma.dwuir.replier.text
      #
      # |@block
      # Answers `text`'s query about its height when width is known.
      # |@endblock
      #
      # TODO: cache
      matchpi %{(text ⍊ dw-request: (measure w_number kout_symbol ⍊ status_symbol))} do
        space = Point.new(w.to(Float32), Float32::INFINITY)

        continue unless spec = text_spec?(subject, space)
        continue unless font = spec.font?

        size = Rect[0, 0, 0, spec.leading.resolve(spec.size)]
        pencil = platform.pencils.call(PencilRequest.new(font, spec.size, spec.leading, spec.tracking))
        spec.each_text_drawable(pencil) do |dw|
          size |= dw.bounds
        end

        Term.morph(subject, {kout, size.h}, {status, :ok}, {:"dw-request", nil})
      end

      # |@ soma.dwuir.replier.text
      #
      # |@block
      # Answers `text`'s query about its width and height.
      # |@endblock
      #
      # TODO: cache
      matchpi %{(text ⍊ dw-request: (measure wout_symbol hout_symbol ⍊ status_symbol))} do
        space = Point.inf

        continue unless spec = text_spec?(subject, space)
        continue unless font = spec.font?

        size = Rect[0, 0, 0, spec.leading.resolve(spec.size)]
        pencil = platform.pencils.call(PencilRequest.new(font, spec.size, spec.leading, spec.tracking))
        spec.each_text_drawable(pencil) do |dw|
          size |= dw.bounds
        end

        Term.morph(subject, {wout, size.w}, {hout, size.h}, {status, :ok}, {:"dw-request", nil})
      end

      # |@ soma.dwuir.replier.svg
      #
      # |@block
      # Answers `svg`'s query about its width and height.
      # |@endblock
      #
      # TODO: cache
      matchpi %{(svg ⍊ src_ dw-request: (measure wout_symbol hout_symbol ⍊ status_symbol))} do
        begin
          image = platform.images.load(src)
        rescue e : ImageServerError
          Log.debug(exception: e) { "failed to measure svg at #{src}" }

          next Term.morph(subject, {:"dw-request", nil}, {status, {:err, e.message}})
        end

        size = image.size

        Term.morph(subject, {:"dw-request", nil}, {wout, size.x}, {hout, size.y}, {status, :ok})
      end

      # |@ soma.dwuir.replier.rect
      #
      # |@block
      # Answers `rect`'s query about its width and height if its fill is an image.
      # The image's dimensions will be retrieved and used to answer the query.
      # |@endblock
      matchpi %{(rect ⍊ fill_: [image src_] dw-request: (measure wout_symbol hout_symbol ⍊ status_symbol))} do
        begin
          image = platform.images.load(src)
        rescue e : ImageServerError
          Log.debug(exception: e) { "failed to measure rect image at #{src}" }

          next Term.morph(subject, {:"dw-request", nil}, {status, {:err, e.message}})
        end

        size = image.size

        Term.morph(subject, {:"dw-request", nil}, {status, :ok}, {wout, size.x}, {hout, size.y})
      end

      otherwise { subject }
    end
  end
end
