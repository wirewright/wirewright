module Ww::M1
  # :nodoc:
  def head?(prod : Π::Pattern(Term::Dict)) : Term?
    op = prod.pattern

    Term.case(op, engine: M0) do
      # Proceed into the successor of %let.
      #
      # (⏏op←+⏏ a_ b_), head is `+`.
      matchpi %{[%'%let _ successor_]}, cue: :"%let" do
        head?(Π.pattern(successor.as_d))
      end

      # (⏏+⏏ a_ b_)
      matchpi %{[%'%literal term_]}, cue: :"%literal" do
        term
      end

      otherwise { }
    end
  end

  # :nodoc:
  def head?(prod : Π::Toplevel(Term::Dict)) : Term?
    op = prod.pattern

    Term.case(op, engine: M0) do
      # (+ a_ b_), (`p + a_ b_), ((%group xs_ + a_) b_) and so on, head is `+`.
      matchpi %{[%seq _*]}, cue: :"%seq" do
        items = Pf::Kit.stack_array(Term::Dict, 8)
        Kit.flatseq(op, items)
        return unless item = items.first?

        Term.case(item, engine: M0) do
          # (`front (%group xs_ ⏏+⏏ a_) b_)
          matchpi %{[%'%singular successor_]}, cue: :"%singular" do
            head?(Π.pattern(successor.as_d))
          end

          otherwise { }
        end
      end

      # Proceed into the itemspart of %partition.
      #
      # [+ a_ b_], (+ a_ b_ ¦ rest_) and so on, head is `+`.
      matchpi %{[%'%partition itemspart_ _]}, cue: :"%partition" do
        head?(Π.toplevel(itemspart.as_d))
      end

      # (+ 1 2), head is `+`.
      matchpi %{[%'%literal [head_ _*]]}, cue: :"%literal" do
        head
      end

      # Proceed into the successor of %let.
      #
      # x←(+ a_ b_), x←[`front + a_] and so on, head is `+`.
      matchpi %{[%'%let _ successor_]}, cue: :"%let" do
        head?(Π.toplevel(successor.as_d))
      end

      otherwise { }
    end
  end

  # Returns the *head* of a normal pattern *pattern*.
  #
  # The head of a pattern is the first literal in an expected dictionary matchee.
  # For example, in `(+ a_ b_)` that would be `+`; and in ``(`a `b x←qux x_ y_)`` that
  # would be `qux`, and so on: groups, slots, and `min > 1` `%past`s and `%many`s are
  # handled properly by this function, as are literal dicts.
  #
  # On the other hand, for `qux` or `(xs_* qux)` the head is indeterminate: the first
  # isn't even a dict, and for the second, there's no first literal -- we'd have to match
  # *xs* first. For such cases, this function returns `nil`.
  def head?(pattern : Normp) : Term?
    pattern.unwrap { |op| head?(Π.toplevel(op)) }
  end
end
