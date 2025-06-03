module Ww::Soma::DwUIR
  # Represents the context of each node.
  defcase Context,
    view : Rect,
    view_tf : Tf,
    layer : Int32,
    tf : Tf,
    opacity : Float32,
    pivot : Point,
    bounds : Rect

  # Returns `true` if *bounds* are going to be seen by the user after all
  # transformations based on *context*. Returns `false` otherwise.
  def visible?(context : Context, bounds : Rect = context.bounds) : Bool
    tfview = context.view_tf.map(context.view)
    tfbounds = context.tf.map(bounds)

    !(tfview & tfbounds).empty?
  end

  # Determines whether `walk` should recurse into children nodes.
  enum WalkFlow
    Recurse
    Next
  end

  # Calls *fn* with each node of *dwuir* and its corresponding `Context`.
  #
  # *fn* must in turn respond whether to recurse into the node or not.
  def walk(dwuir : Term, &fn : Context, Term -> WalkFlow) : Nil
    context = Context.new(
      view: Rect.inf,
      view_tf: Tf.new,
      layer: 0,
      tf: Tf.new,
      opacity: 1.0f32,
      pivot: Point.new(0, 0),
      bounds: Rect.empty,
    )

    walk(context, dwuir, fn)
  end

  # :nodoc:
  def walk(context, node, fn)
    Term.case(node) do
      # NOTE: the order of matchpis here is important for some nodes but
      # not others; try not to shuffle them too much.

      # |@ soma.dwuir.z-index
      #
      # |@block
      # Use `z-index` to specify the z-index of any node.
      #
      # z-index determines the draw order. Nodes with higher z-indices are drawn
      # on top of those with a lower one.
      #
      # The default, implicit z-index is `0`.
      #
      # NOTE: it is the client's responsibility to annotate their nodes with
      # the desired z-index. DwUIR gives absolutely no guarantees about the draw
      # order of nodes that have the same z-index; it is undefined. To reiterate,
      # there is no dependence between item order in the DwUIR markup and the z-index
      # etc. Clients must express their intent clearly; DwUIR will *not* try to guess.
      #
      # See also: `soma.dwuir.composite` to learn about scoping z-indices.
      # |@endblock
      #
      # |@key z-index -- The z-index to use.
      matchpi %[{¦ z-index: z←(%number i32)}] do
        context = context.copy_with(layer: z.to(Int32))

        continue
      end

      # |@ soma.dwuir.dl
      #
      # |@block
      # Short for *delta left*. Use `dl` to specify the offset of any node
      # from its parent's left side.
      #
      # The default, implicit `dl` is `0`.
      # |@endblock
      #
      # |@key dl soma.dwuir.measure -- Relative values are treated as a fraction
      # of the width of the nearest enclosing parent with a definite `final-w`.
      matchpi %[{¦ dl_}] do
        magn = Magn.abst(dl, Magn.abs(0))
        offset = Point.new(magn.resolve(context.bounds.w), 0.0f32)
        context = context.copy_with(bounds: context.bounds.translate(offset))

        continue
      end

      # |@ soma.dwuir.dt
      #
      # |@block
      # Short for *delta top*. Use `dt` to specify the offset of any node
      # from its parent's top side.
      #
      # The default, implicit `dl` is `0`.
      # |@endblock
      #
      # |@key dt soma.dwuir.measure -- Relative values are treated as a fraction
      # of the height of the nearest enclosing parent with a definite `final-h`.
      matchpi %[{¦ dt_}] do
        magn = Magn.abst(dt, Magn.abs(0))
        offset = Point.new(0.0f32, magn.resolve(context.bounds.h))
        context = context.copy_with(bounds: context.bounds.translate(offset))

        continue
      end

      # |@ soma.dwuir.size
      #
      # |@block
      # Sizes must be set using the `final-w` and `final-h` attributes for most
      # nodes, including but not limited to: `text`, `rect`, `viewport`.
      #
      # DwUIR size **does not** define a node's clipping box. If the node's content
      # overflows, there will be no clipping. This was a conscious design choice. Things
      # such as `angle` make everything much more complicated for DwUIR, and require
      # the client's intervention. Clients are expected to manage overflow explicitly,
      # with the viewport node: `viewport`.
      #
      # In other words, `final-w` and `final-h` act as *strict hints*; but DwUIR is
      # allowed to deviate from them due to drawing imprecisions or content overflow.
      # Clients must use the `viewport` node to make sure clipping happens the way
      # they want it to.
      #
      # It goes without saying that it is the responsibility of the client to calculate
      # and set final-w/h appropriately, and to values that make sense. DwUIR will
      # make no attempts to calculate, guess, or fix them.
      # |@endblock

      # |@ soma.dwuir.size.final-w
      #
      # |@block
      # Specifies the width of any node in pixels.
      #
      # The default, implicit `final-w` is `0`, which will render the node invisible.
      # |@endblock
      #
      # |@key final-w -- Sets the width of a node in pixels.
      matchpi %[{¦ final-w: w←(%number _ >= 0)}] do
        context = context.copy_with(bounds: context.bounds.resize(w: w.to(Float32)))

        continue
      end

      # |@ soma.dwuir.size.final-h
      #
      # |@block
      # Specifies the height of any node in pixels.
      #
      # The default, implicit `final-h` is `0`, which will render the node invisible.
      # |@endblock
      #
      # |@key final-h -- Sets the height of a node in pixels.
      matchpi %[{¦ final-h: h←(%number _ >= 0)}] do
        context = context.copy_with(bounds: context.bounds.resize(h: h.to(Float32)))

        continue
      end

      # |@ soma.dwuir.origin
      #
      # |@block
      # Use `origin` properties to move the origin of a node, effectively changing what
      # `dl` and `dt` mean for that node. The default origin is `0; 0`. It is sometimes
      # useful to move the origin to the center of a node; this can be achieved with
      # origin `0.5; 0.5`.
      # |@endblock
      #
      # |@key origin -- Enables the handling of origin properties.
      #
      # |@key origin-l -- Defines the left offset of the origin point, in normalized
      # coordinates within the bounding box of the node, including the node's own
      # or nearest enclosing `final-w` and `final-h`.
      #
      # |@key origin-t -- Defines the top offset of the origin point, in normalized
      # coordinates within the bounding box of the node, including the node's own
      # or nearest enclosing `final-w` and `final-h`.
      matchpi(<<-WWML
        {¦ origin: true
           origin-l⋮ 0
           origin-t⋮ 0}
      WWML
      ) do
        origin = Point.new(origin_l.to(Float32), origin_t.to(Float32))
        point = context.bounds.map(origin)
        offset = context.bounds.tl - point
        context = context.copy_with(bounds: context.bounds.translate(offset))

        continue
      end

      # |@ soma.dwuir.pivot
      #
      # |@block
      # Use `pivot` properties to move the rotation pivot of a node. The default pivot
      # is `0.5; 0.5` (the node's center). For instance, something like a clock hand
      # would have the pivot `0.5; 1.0` (bottom center).
      # |@endblock
      #
      # |@key pivot -- Enables the handling of pivot properties.
      #
      # |@key pivot-l -- Defines the left offset of the pivot point, in normalized
      # coordinates within the bounding box of the node, including the node's own
      # or nearest enclosing `final-w` and `final-h`.
      #
      # |@key pivot-t -- Defines the top offset of the pivot point, in normalized
      # coordinates within the bounding box of the node, including the node's own
      # or nearest enclosing `final-w  and `final-h`.
      matchpi(<<-WWML
        {¦ pivot: true
           pivot-l⋮ 0.5
           pivot-t⋮ 0.5}
      WWML
      ) do
        pivot = Point.new(pivot_l.to(Float32), pivot_t.to(Float32))
        context = context.copy_with(pivot: context.bounds.map(pivot))

        continue
      end

      # |@ soma.dwuir.angle
      #
      # |@block
      # Use `angle` to set the rotation angle of a node in degrees.
      # Use `pivot` to set the rotation pivot.
      # |@endblock
      #
      # |@key angle -- rotation angle in degrees. `0` or `360` etc. angle
      # will be ignored.
      matchpi %[{¦ angle: (%pipe (mod 360) (%all (%not 0) angle_number))}] do
        action = Tf[
          Tf.translate(context.pivot),
          Tf.rotate(angle.to(Float32)),
          Tf.translate(-context.pivot),
        ]

        context = context.copy_with(tf: context.tf.append(action))

        continue
      end

      # |@ soma.dwuir.viewport
      #
      # |@block
      # Use the `viewport` node to control overflow by clipping.
      # |@endblock
      #
      # |@key pan-l -- used as pivot left for zoom; and also as additional left
      # offset for nodes in the viewport.
      #
      # |@key pan-t -- used as pivot top for zoom; and also as additional top
      # offset for nodes in the viewport.
      #
      # |@key zoom -- zoom factor (increase to zoom in, decrease to zoom out).
      matchpi(<<-WWML
        (viewport _ ¦ _ pan-l⋮ 0
                        pan-t⋮ 0
                        zoom: (%optional 1 zoom←(%number _ > 0)))
      WWML
      ) do
        return unless visible?(context)

        pan = Point.new(pan_l.to(Float32), pan_t.to(Float32))

        action = Tf[
          Tf.translate(context.bounds.tl + pan),
          Tf.scale(zoom.to(Float32)),
          Tf.translate(-(context.bounds.tl + pan)),
        ]

        context = context.copy_with(
          view: context.bounds,
          view_tf: context.tf,
          bounds: context.bounds.translate(pan),
          tf: context.tf.append(action),
        )

        continue
      end

      otherwise do
        case fn.call(context, node)
        in .next?
        in .recurse?
          next unless node.type.dict?

          node.each_item_unordered do |child|
            walk(context, child, fn)
          end
        end
      end
    end
  end
end
