module Ww::M1
  # :nodoc:
  def simplify(prod : Π::Normal(Term::Dict)) : Term::Dict
    op = prod.op

    Term.case(op, engine: M0) do
      matchpi %{[%'%split [%'%pass] mid_ [%'%pass]]}, cue: {:"%split", :"%pass"} do
        Term[:"%item", mid,
          guarded: true,
          bounds: {1, :"..=", :"∞"},
          depth: {:+, :envelope, 1},
        ]
      end

      matchpi %{[%'%split° [%'%pass] mid_ [%'%pass]]}, cue: {:"%split°", :"%pass"} do
        Term[:"%item°", mid,
          guarded: true,
          bounds: {1, :"..=", :"∞"},
          depth: {:+, :envelope, 1},
        ]
      end

      matchpi %{[%'%split [%'%pass] a_ [%'%item b_]]}, cue: {:"%split", :"%pass", :"%item"} do
        Term[:"%adjacent", a, b,
          guarded: true,
          bounds: {2, :"..=", :"∞"},
          depth: {:+, :envelope, 1},
        ]
      end

      matchpi %{[%'%split [%'%pass] a_ bs←(%'%adjacent _ _* ⍊ bounds: (±lo ..= ?))]}, cue: {:"%split", :"%pass", :"%adjacent"} do
        # Insert *a* before `%adjacent`'s members: (%adjacent ⏏ _ _*).
        result = bs.replace(Term[1]...Term[1], &.append(a))
        # Increment its lower bound.
        Term.morph(result, {:bounds, 0, lo + 1})
      end

      otherwise { op }
    end
  end

  # :nodoc:
  def simplify(prod : Π::Guarded(Term::Dict)) : Term::Dict
    op = prod.op

    Term.case(op, engine: M0) do
      # If we see min-bounds and max-bounds on seq, and the contents of that
      # seq are passes or unnamed plurals, then the bounds check already does
      # all the work; we don't need to have `%seq` at all. For example, consider
      # `(_ _ _)` or `(_ _* _)` or `(_*)` or `(_ _+)`. These kinds of patterns
      # are bounds checks in disguise.
      matchpi %{(%'%seq _* ⍊ min-bounds max-bounds)}, cue: :"%seq" do
        eligible = true

        members = op.items.move(1)
        members.each do |member|
          Term.case(member, engine: M0) do
            # Eligible
            matchpi(
              %{[%'%singular [%'%pass]]},
              %{(%'%plural ⍊ type: %'_)},
              %{(%'%plural/min ⍊ type: %'_)},
              %{(%'%plural/max ⍊ type: %'_)},
            ) { }

            # Not eligible
            otherwise { eligible = false }
          end

          break unless eligible
        end

        continue unless eligible

        Term[{:"%itemsonly"}]
      end

      # A pattern like (xs_*) has a more optimal implementation: we don't
      # need to invoke Spatial machinery for this!
      matchpi(
        %{(%'%seq (%'%plural capture←[%'%capture _] ⍊ type: %'_) ⍊ min-bounds max-bounds)},
        %{(%'%seq (%'%plural/min capture←[%'%capture _] ⍊ type: %'_) ⍊ min-bounds max-bounds)},
        %{(%'%seq (%'%plural/max capture←[%'%capture _] ⍊ type: %'_) ⍊ min-bounds max-bounds)},
        cue: {:"%seq", :"%capture"},
        cues: {:"%plural", :"%plural/min", :"%plural/max"},
      ) do
        Term[:"%itemsonly", capture]
      end

      # Rewrite (¦ _) = (%partition () _) and (%partition () _dict) into a pairsonly
      # check (which is cheaper!)
      matchpi(
        %{[%'%partition [%'%literal ()] [%'%pass]]},
        %{[%'%partition [%'%literal ()] [%'%dict]]},
        cue: {:"%partition", :"%literal"},
        cues: {:"%pass", :"%dict"},
      ) do
        Term[{:"%pairsonly"}]
      end

      # Rewrite (¦ xs←...) -> xs←(¦ ...).
      matchpi(
        %{[%'%partition [%'%literal ()] [%'%let capture←[%'%capture _] successor_]]},
        cue: {:"%partition", :"%literal", :"%let"}
      ) do
        Term[:"%let", capture, op.with(2, successor)]
      end

      # (_* `x) and similar are a common occurrence, rewrite to (%all (_*) (%back (%ref x)))
      # which has much less overhead.
      matchpi %{[%'%seq _ _ _*]}, cue: {:"%seq", :"%slot"} do
        members = op.items.move(1)

        Term.case(members[-2], engine: M0) do
          # Could be two adjacent slots, not eligible.
          matchpi %{[%'%slot _]}, cue: :"%slot" { }

          otherwise do
            Term.case(members[-1], engine: M0) do
              matchpi %{[%'%slot ref←[%'%ref _]]} do
                # Eligible
                return Term[:"%all", {:"%back", ref}, op.without_item(op.itemsize - 1)]
              end

              otherwise { }
            end
          end
        end

        continue # Not eligible
      end

      # (`x _*) and similar become (%all (_*) (%front (%ref x))) similarly.
      matchpi %{[%'%seq a_ b_ _*]}, cue: {:"%seq", :"%slot"} do
        Term.case(a, engine: M0) do
          matchpi %{[%'%slot ref←[%'%ref _]]}, cue: :"%slot" do
            Term.case(b, engine: M0) do
              matchpi %{[%'%slot _]}, cue: :"%slot" { }

              otherwise do
                # Eligible
                return Term[:"%all", {:"%front", ref}, op.without_item(1)]
              end
            end
          end

          otherwise { }
        end

        continue # Not eligible
      end

      # Rewrite (_ _ ... _ _*) -> %prefix, which is more efficient than %seq.
      matchpi %{[%'%seq _ _ _*]}, cue: {:"%seq", :"%plural"} do
        prefix = op.items.move(1).grow(-1)
        continue unless Kit.singulars?(prefix)

        Term.case(op.items.last, engine: M0) do
          matchpi %{(%'%plural ⍊ min: 0 max: ∞ type: %'_)} do
            # Eligible
            rw = Term::Dict.build do |commit|
              commit << :"%prefix"
              commit.concat(prefix) { |(_, successor)| successor }
            end

            return rw
          end

          otherwise { }
        end

        # Not eligible
        continue
      end

      # Rewrite (_* _ _ ... _) -> %postfix, which is more efficient than %seq.
      matchpi %{[%'%seq (%'%plural ⍊ min: 0 max: ∞ type: %'_) _ _*]}, cue: {:"%seq", :"%plural"} do
        postfix = op.items.move(2)
        continue unless Kit.singulars?(postfix)

        Term::Dict.build do |commit|
          commit << :"%postfix"
          commit.concat(postfix) { |(_, successor)| successor }
        end
      end

      # Rewrite (_ _ ... _) -> %singular-seq, which is more efficient than %seq.
      matchpi %{[%'%seq _*]}, cue: :"%seq" do
        members = op.items.move(1)
        continue unless Kit.singulars?(members)

        Term::Dict.build do |commit|
          commit << :"%singular-seq"
          commit.concat(members) { |(_, successor)| successor }
        end
      end

      # Rewrite pointless nested guards in partition itemspart.
      matchpi(
        %{(%'%dict-guard [%'%partition (%'%dict-guard successor_ ¦ opts_) _] ¦ opts_)},
        %{(%'%bounds [%'%partition (%'%bounds successor_ ¦ opts_) _] ¦ opts_)},
        %{(%'%depth [%'%partition (%'%depth successor_ ¦ opts_) _] ¦ opts_)},
        %{(%'%sketch [%'%partition (%'%sketch successor_ ¦ opts_) _] ¦ opts_)},
        cue: {:"%partition"},
        cues: {:"%dict-guard", :"%bounds", :"%depth", :"%sketch"},
      ) do
        Term.morph(op, {1, 1, successor})
      end

      # Rewrite pointless nested guards in partition pairspart.
      matchpi(
        %{(%'%dict-guard [%'%partition _ (%'%dict-guard successor_ ¦ opts_)] ¦ opts_)},
        %{(%'%bounds [%'%partition _ (%'%bounds successor_ ¦ opts_)] ¦ opts_)},
        %{(%'%depth [%'%partition _ (%'%depth successor_ ¦ opts_)] ¦ opts_)},
        %{(%'%sketch [%'%partition _ (%'%sketch successor_ ¦ opts_)] ¦ opts_)},
        cue: {:"%partition"},
        cues: {:"%dict-guard", :"%bounds", :"%depth", :"%sketch"},
      ) do
        Term.morph(op, {1, 2, successor})
      end

      otherwise { op }
    end
  end

  # Performs a single bottom-up pass of simplification rewrites on the given
  # normal *pattern*.
  #
  # Some simplification rewrites are only possible on normal patterns, as
  # `guard`ing presents several obstructions to easy rewriting. Hence, the normal
  # pattern simplifiction pass.
  def simplifyp(pattern : Normp) : Normp
    pattern.map do |op|
      Kit.ascend(op) do |member|
        simplify(Π.normal(member))
      end
    end
  end

  # Performs a single bottom-up pass of simplification rewrites on the given
  # guarded normal *pattern*.
  #
  # The majority of useful rewrites happen at this stage, including elimination
  # of duplicate guards.
  def simplifyp(pattern : Guardedp) : Guardedp
    pattern.map do |op|
      Kit.ascend(op) do |member|
        simplify(Π.guarded(member))
      end
    end
  end
end
