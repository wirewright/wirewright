module Ww::M1
  # :nodoc:
  #
  # A tiny language for doing arithmetic on ranges whose sides can be unknown.
  #
  # Unknown bounds are represented by `∞`. Here, it does not mean infinity
  # in the strictest sense. Our ranges are closed, and for closed ranges,
  # `[3; ∞]` makes no sense. However, if you interpret `∞` as "unknown" -- say,
  # `?`, then it starts making sense: `[3; ?]`.
  #
  # The reason we use infinity is that clients using this language like
  # infinities. That is, for instance, when compiling to an operator,
  # we translate `∞` to floating-point infinity. If we used a different
  # symbol (e.g. `?`), that would require too much absolutely useless work
  # translating between the two at the boundaries. There is simply no point --
  # the tiny amount of initial confusion with `∞` goes away pretty quickly.
  module RangeCalc
    extend self

    # :nodoc:
    EXPR_SUM = Term.of(:∩, {:sum, :member, :min}, {:sum, :member, :max})
    # :nodoc:
    EXPR_ENVELOPE = Term.of(:∩, {:max, :member, :min}, {:max, :member, :max})

    private def extract(members : Enumerable(Term::Dict), fn, kmin, kmax)
      Term.case(fn, engine: M0) do
        matchpi %{min}, cue: :min do
          members.map { |member| member[kmin]? || Term.of(0) }
        end

        matchpi %{max}, cue: :min do
          members.map { |member| member[kmax]? || Term.of(:"∞") }
        end
      end
    end

    private def aggregate?(objects, fn)
      Term.case(fn, engine: M0) do
        matchpi %{sum}, cue: :sum { objects.sum { |object| Ww.magn(object) } }
        matchpi %{min}, cue: :min { objects.min_by? { |object| Ww.magn(object) } }
        matchpi %{max}, cue: :max { objects.max_by? { |object| Ww.magn(object) } }
      end
    end

    private def reply(aggregate, fn)
      if aggregate == Magnitude::INFINITY
        aggregate = Term.of(:"∞")
      end

      Term.case(fn, engine: M0) do
        matchpi %{min}, cue: :min { Term.of(aggregate || 0, :"..=", :"∞") }
        matchpi %{max}, cue: :max { Term.of(0, :"..=", aggregate || :"∞") }
      end
    end

    # Evaluates a range calc expression *expr*.
    #
    # *op* is the normal pattern node relative to which queries in *expr* should
    # be evaluated.
    #
    # *kmin* sets the key to use to extract *op* member minima.
    # *kmax* sets the key to use to extract *op* member maxima.
    def eval(expr : Term, kmin : Term, kmax : Term, op : Term::Dict) : Term
      Term.case(expr, engine: M0) do
        # `(∩ (sum member min) (sum member max))` is a very common idiom. We refer
        # to it simply as `sum`.
        matchpi %{sum}, cue: :sum do
          eval(EXPR_SUM, kmin, kmax, op)
        end

        # `(∩ (max member min) (max member max))` is a very common idiom. We refer
        # to it simply as `envelope`. Notice how we take max min (meaning strictest
        # min) but then also max max (meaning loosest max). This is, roughly, because
        # the less our members know, the less we know. Something like `∞` represents
        # total absence of knowledge. If one of the members doesn't know anything at
        # all about the matchee's max (e.g.), then we don't know anything either,
        # regardless of how knowledgeable sibling members are.
        matchpi %{envelope}, cue: :envelope do
          eval(EXPR_ENVELOPE, kmin, kmax, op)
        end

        # Constructs a unit range. E.g. 100 -> (100 ..= 100) aka [100;100]
        matchpiT %{±x} do
          Term.of(x, :"..=", x)
        end

        # Constructs a range.
        matchpi %{(_ ..= _)}, cue: :"..=" do
          expr
        end

        # Queries each member in range using the given *query* function (see `query`),
        # then aggregates the result(s) with an aggregation function *agg* (see `aggregate`).
        # Finally, forms an appropriate reply for *query* (see `reply`).
        matchpi %{(agg_ (members from_ ..< to_) query_)}, cue: {:members, :"..<"} do
          candidates = Kit.members(op).truncate(from.to(Int32)...to.to(Int32))

          pipe(candidates,
            extract(query, kmin, kmax),
            aggregate?(agg),
            reply(query),
          )
        end

        # Queries *all non-sealed members*, otherwise the same as above.
        matchpi %{(agg_ member query_)}, cue: :member do
          candidates = Kit.members(op).reject(&.[:sealed]?)

          pipe(candidates,
            extract(query, kmin, kmax),
            aggregate?(agg),
            reply(query),
          )
        end

        # Selects minimum of a range. E.g. (min (10 ..= 20)) -> (10 ..= ∞).
        matchpi %{(min a_)}, cue: :min do
          v = eval(a, kmin, kmax, op)

          Term.matchpiT(v, %{(min_ ..= _)}, engine: M0) do
            Term.of(min, :"..=", :"∞")
          end
        end

        # Selects maximum of a range. E.g. (max (10 ..= 20)) -> (0 ..= 20).
        matchpi %{(max a_)}, cue: :max do
          v = eval(a, kmin, kmax, op)

          Term.matchpiT(v, %{(_ ..= max_)}, engine: M0) do
            Term.of(0, :"..=", max)
          end
        end

        # Adds two ranges. E.g. `(+ (5 ..= 10) (3 ..= 8))` -> `(8 ..= 18)`.
        matchpi %{(+ a_ b_)}, cue: :+ do
          v = eval(a, kmin, kmax, op)
          w = eval(b, kmin, kmax, op)

          Term.givenpi({v, w}, %{(min0_ ..= max0_) (min1_ ..= max1_)}, engine: M0) do
            min = Term.of(:"∞").in?(min0, min1) ? Term.of(:"∞") : min0 + min1
            max = Term.of(:"∞").in?(max0, max1) ? Term.of(:"∞") : max0 + max1

            Term.of(min, :"..=", max)
          end
        end

        # Multiplies two ranges. E.g. `(* (2 ..= 10) (3 ..= 8))` -> `(6 ..= 80)`.
        matchpi %{(* a_ b_)}, cue: :* do
          v = eval(a, kmin, kmax, op)
          w = eval(b, kmin, kmax, op)

          Term.givenpi({v, w}, %{(min0_ ..= max0_) (min1_ ..= max1_)}, engine: M0) do
            min = Term.of(:"∞").in?(min0, min1) ? Term.of(:"∞") : min0 * min1
            max = Term.of(:"∞").in?(max0, max1) ? Term.of(:"∞") : max0 * max1

            Term.of(min, :"..=", max)
          end
        end

        # Unions two ranges. E.g. `(∪ (2 ..= 10) (3 ..= 15))` -> `(2 ..= 15)`
        # (loosest min, loosest max).
        matchpi %{(∪ a_ b_)}, cue: :∪ do
          v = eval(a, kmin, kmax, op)
          w = eval(b, kmin, kmax, op)

          Term.givenpi({v, w}, %{(min0_ ..= max0_) (min1_ ..= max1_)}, engine: M0) do
            min = {min0, min1}.min_by { |bound| Ww.magn(bound) }
            max = {max0, max1}.max_by { |bound| Ww.magn(bound) }

            Term.of(min, :"..=", max)
          end
        end

        # Intersects two ranges. E.g. `(∩ (2 ..= 10) (3 ..= 15))` -> `(3 ..= 10)`
        # (strictest min, strictest max).
        #
        # This is also used to combine the min of one range with the max of another
        # when using `min` and `max`: `(∩ (min (2 ..= 10)) (max (3 ..= 15)))` gives
        # `(∩ (2 ..= ∞) (0 ..= 15))` which in turn gives `(2 ..= 15)`.
        matchpi %{(∩ a_ b_)}, cue: :∩ do
          v = eval(a, kmin, kmax, op)
          w = eval(b, kmin, kmax, op)

          Term.givenpi({v, w}, %{(min0_ ..= max0_) (min1_ ..= max1_)}, engine: M0) do
            min = {min0, min1}.max_by { |bound| Ww.magn(bound) }
            max = {max0, max1}.min_by { |bound| Ww.magn(bound) }

            Term.of(min, :"..=", max)
          end
        end
      end
    end
  end
end
