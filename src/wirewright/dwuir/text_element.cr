module Ww::DwUIR
  # A "text element" is currently either a `Cursor` (e.g. an I-beam) or a so-
  # called `Run`; which is essentially a `WrapToken` that may be selected (through
  # being part of a selection range, if any).
  module TextElement
    extend self

    alias Any = Cursor | Run

    private alias Wt = WrapToken

    # An I-beam.
    record Cursor

    # A run of characters (the characters themselves are represented by *token*).
    #
    # *selected* tells whether those characters are in the selection range.
    record Run, token : Wt::Any, selected : Bool

    # Calls *sink* with each text element in *string*.
    def each(pencil : IPencil, spec : WrapSpec, string : String, selection : TextSelectionRange, &sink : Any ->)
      Wt.each_with_range(pencil, spec, string) do |token, range|
        if selection.nil?
          # Selection is absent.
          sink.call(Run.new(token, false))
        elsif selection.empty?
          # Selection is empty. Emit a logical cursor I-beam at before the appropriate
          # position in the token.
          if range.includes?(selection.begin)
            l, m, r = Wt.partition(token, selection.begin - range.begin)

            sink.call(Run.new(l, false))
            sink.call(Cursor.new)
            sink.call(Run.new(m, false))
            sink.call(Run.new(r, false))
          else
            sink.call(Run.new(token, false))
          end
        elsif selection.subrange_of?(range)
          # Selection is fully within the token. Split the token and mark the
          # parts correspondingly.
          l, ml, rest = Wt.partition(token, selection.begin - range.begin)

          # Selection ends where token ends.
          if rest.is_a?(Wt::Empty)
            sink.call(Run.new(l, false)) unless l.is_a?(Wt::Empty)
            sink.call(Run.new(ml, true)) unless ml.is_a?(Wt::Empty)
            next
          end

          mr, rl, rr = Wt.partition(rest, selection.size - 1)

          sink.call(Run.new(l, false)) unless l.is_a?(Wt::Empty)
          sink.call(Run.new(ml, true)) unless ml.is_a?(Wt::Empty)
          sink.call(Run.new(mr, true)) unless mr.is_a?(Wt::Empty)
          sink.call(Run.new(rl, false)) unless rl.is_a?(Wt::Empty)
          sink.call(Run.new(rr, false)) unless rr.is_a?(Wt::Empty)
        elsif range.includes?(selection.begin)
          # Selection begin is located within the token. Split it and mark the
          # parts accordingly. `m` is the token at selection begin, it is included.
          l, m, r = Wt.partition(token, selection.begin - range.begin)

          sink.call(Run.new(l, false)) unless l.is_a?(Wt::Empty)
          sink.call(Run.new(m, true)) unless m.is_a?(Wt::Empty)
          sink.call(Run.new(r, true)) unless r.is_a?(Wt::Empty)
        elsif range.includes?(selection.end)
          # Selection end is located within the token. Split it and mark the parts
          # accordingly. `m` is the token at selection end; since the selection range
          # is exclusive, it is excluded.
          l, m, r = Wt.partition(token, selection.end - range.begin)

          sink.call(Run.new(l, true)) unless l.is_a?(Wt::Empty)
          sink.call(Run.new(m, false)) unless m.is_a?(Wt::Empty)
          sink.call(Run.new(r, false)) unless r.is_a?(Wt::Empty)
        elsif range.subrange_of?(selection)
          # Token is fully within the selection.
          sink.call(Run.new(token, true))
        else
          # Token is outside of the selection.
          sink.call(Run.new(token, false))
        end
      end

      # Handle cursor at the very end of the string.
      if selection && selection.empty? && selection.begin == string.size
        sink.call(Cursor.new)
      end
    end
  end
end
