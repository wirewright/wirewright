module Ww::Soma::Microfold::Pass
  # Hosts functions and data structures to perform the decomposition pass.
  module Decompose
    extend self

    # :nodoc:
    defcase Context, theme : Theme, pairs : Term::Dict, sink : Term::Dict::Commit

    # If sink's size increased, this means decomposition succeeded (in the loosest sense;
    # we're committing to the decision because it *already* mutated the sink).
    private def committed?(sink, &) : Bool
      size0 = sink.size
      yield
      size1 = sink.size
      size1 > size0
    end

    # Decomposes utilities in `style: "..."`.
    def styles(root : Term, theme : Theme, issues : Issue::Sink) : Term
      Pass.mapwalk_preset_and_style(root, issues) do |_, pairs, style, issues|
        Term::Dict.build do |commit|
          ctx = Context.new(theme, pairs, commit)

          style.items.each do |utility|
            utility(ctx, utility, issues)
          end
        end
      end
    end

    # Decomposes known properties from pairs. Removes properties that were
    # successfully decomposed from the node.
    def properties(root : Term, theme : Theme, issues : Issue::Sink) : Term
      Pass.mapwalk(root) do |node0, keypath|
        Term.of_case(node0) do
          matchpi %[(_ _* ¦ pairs_ µ-preset µ-style: style0←(_*))] do
            node1 = node0

            style1 = style0.transaction do |commit|
              ctx = Context.new(theme, pairs.unsafe_as_d, commit)

              node0.each_pair do |key, value|
                next unless spec = theme.property_spec?(key)

                node1 = node1.morph({key, nil})

                spots = {
                  Issue::Spot::Keypath.new(keypath),
                  Issue::Spot::TermDetail.new("property", key),
                  Issue::Spot::TermDetail.new("property value", value),
                }

                issues.adjoin(*spots) do |issues|
                  prop(ctx, spec, value, issues)
                end
              end
            end

            node1.morph({:"µ-style", style1})
          end

          otherwise { node0 }
        end
      end
    end

    # Decomposes a utility, e.g. `(utility "bg-neutral-500")`.
    def utility(ctx : Context, utility : Term, issues : Issue::Sink)
      Term.case(utility) do
        matchpi %[(utility id_string)] do
          view = id.to(StringView)

          issues.adjoin(Issue::Spot::Detail.new("utility", view)) do |issues|
            utility(ctx, view, issues)
          end
        end

        matchpi %[(items child_)] do
          ctx.sink << Term::Dict.build do |commit|
            commit << :items
            utility(ctx.copy_with(sink: commit), child, issues)
          end
        end

        # Pass through unchanged.
        otherwise do
          ctx.sink << utility
        end
      end
    end

    private def utility(ctx, utility : StringView, issues)
      utility_spec_and_rest(ctx.theme, utility, issues) do |spec, rest|
        committed?(ctx.sink) do
          utility(ctx, spec, rest, issues)
        end
      end
    end

    private def utility(ctx, spec, rest : StringView, issues)
      issues.adjoin(Issue::Spot::TermDetail.new("utility spec", spec)) do |issues|
        Term.case(spec) do
          # |@ soma.microfold.theme.utility-spec.short-for
          #
          # |@block
          # Defines this utility as a shorthand for one or more other utilities.
          # |@endblock
          matchpi %[(short-for utilities_string+)] do
            utilities.items.each do |utility|
              unless successor = ctx.theme.utility_spec?(utility)
                issues.severe("reference to utility `#{utility}`, which does not exist")
                next
              end

              utility(ctx, successor, rest, issues)
            end
          end

          # |@ soma.microfold.theme.utility-spec.leaf
          #
          # |@block
          # Defines an argument-less utility.
          # |@endblock
          #
          # |@key soma.microfold.theme.template template -- The template to use to obtain
          # the mixin.
          #
          # |@key unset -- A dict set of keys to unset, if any (optional).
          matchpi %[(box_ template_dict ⍊ unset⋮ {})] do
            return unless rest.empty?
            return unless instance = instantiate?(ctx.theme, Term[], template.unsafe_as_d, issues)

            ctx.sink << Term.of(:mixin, rank: Rank::Style, box: box, plus: instance, minus: unset.as_nonempty_d?)
          end

          # |@ soma.microfold.theme.utility-spec.leaf
          #
          # |@block
          # Defines a utility that takes an argument.
          # |@endblock
          #
          # |@key soma.microfold.theme.argspec argspec -- Argument specification used
          # to parse and validate the mixin.
          #
          # |@key soma.microfold.theme.template template -- The template to use to obtain
          # the mixin.
          #
          # |@key unset -- A dict set of keys to unset, if any (optional).
          matchpi %[(box_ argspec_ template_dict ⍊ unset⋮ {})] do
            return if rest.empty?
            return unless arg = argparse?(ctx.pairs, argspec, rest, issues)
            return unless arg = argcheck?(ctx.theme, ctx.pairs, argspec, arg, issues)
            return unless instance = instantiate?(ctx.theme, Term[arg: arg], template.unsafe_as_d, issues)

            ctx.sink << Term.of(:mixin, rank: Rank::Style, box: box, plus: instance, minus: unset.as_nonempty_d?)
          end

          # |@ soma.microfold.theme.utility-spec.try
          #
          # |@block
          # Picks the first successful branch.
          #
          # NOTE: You are recommended to define a separate utility for each leaf branch
          # in addition to the group shorthand `try`; otherwise, it will be impossible
          # to target any branch past the first one with the `utility-[*]` form (aka
          # unchecked argument). Unchecked arguments are not parsed or validated;
          # thus, the first branch will always succeed for them.
          # |@endblock
          matchpi %[(try branches_+)] do
            branches.items.each do |branch|
              issues.suppress do
                return if committed?(ctx.sink) { utility(ctx, branch, rest, issues) }
              end
            end
          end

          otherwise do
            issues.severe("unrecognized utility spec #{spec}")
          end
        end
      end
    end

    # Decomposes a Microfold-administered property, as in `wrap-ellipsis: "<..>"`.
    def prop(ctx : Context, spec : Term, arg : Term, issues : Issue::Sink)
      issues.adjoin(Issue::Spot::TermDetail.new("property spec", spec)) do |issues|
        Term.case(spec) do
          # |@ soma.microfold.theme.property-spec.leaf
          #
          # |@block
          # See `soma.microfold.theme.utility-spec.leaf`.
          # |@endblock
          matchpi %[(box_ argspec_ template_dict ⍊ unset⋮ {})] do
            return unless arg = argcheck?(ctx.theme, ctx.pairs, argspec, arg, issues)
            return unless instance = instantiate?(ctx.theme, Term[arg: arg], template.unsafe_as_d, issues)

            ctx.sink << Term.of(:mixin, rank: Rank::Style, box: box, plus: instance, minus: unset.as_nonempty_d?)
          end

          # |@ soma.microfold.theme.property-spec.try
          #
          # |@block
          # See `soma.microfold.theme.utility-spec.try`.
          # |@endblock
          matchpi %[(try branches_+)] do
            branches.items.each do |branch|
              issues.suppress do
                return if committed?(ctx.sink) { prop(ctx, branch, arg, issues) }
              end
            end
          end

          otherwise do
            issues.severe("unrecognized property spec")
          end
        end
      end
    end

    private def instantiate?(theme, env : Term::Dict, template : Term::Dict, issues) : Term::Dict?
      template.transaction do |commit|
        template.each_entry do |key, value0|
          Term.case(value0) do
            # |@ soma.microfold.theme.template.$
            #
            # |@block
            # Dicts of this form are replaced with the result of evaluating a Microfold
            # theme expression.
            # |@endblock
            #
            # |@key soma.microfold.theme.expr expr -- The expression to evaluate.
            matchpi %[($ expr_)] do
              return unless value1 = eval?(theme, env, expr, issues)

              commit.with(key, value1)
            end

            # |@ soma.microfold.theme.template
            #
            # |@block
            # All terms are passed through in Microfold theme templates, except
            # for `($ _)`.
            # |@endblock

            matchpi %[_dict] do
              return unless instance = instantiate?(theme, env, value0.unsafe_as_d, issues)

              commit.with(key, instance)
            end

            otherwise { }
          end
        end
      end
    end

    private def eval?(theme, env : Term::Dict, expr : Term, issues) : Term?
      issues.adjoin(Issue::Spot::TermDetail.new("expression", expr)) do |issues|
        Term.of_case(expr) do
          # |@ soma.microfold.theme.expr.global
          #
          # |@block
          # Evaluates to the current root em size (simply put, root font size).
          #
          # `rem` is not exactly a theme-configured property but rather, a user-
          # configured one; thus, we provide it as a "pseudo-global" of sorts,
          # recomputed along with the theme on change.
          # |@endblock
          matchpi %[(global rem)] do
            theme.rem
          end

          # |@ soma.microfold.theme.expr.global
          #
          # |@block
          # Evaluates to the value of a global with the given name, stored in
          # the `globals` section of the theme document.
          # |@endblock
          matchpi %[(global name_)] do
            return unless value = theme.global?(name)

            eval?(theme, env, value, issues)
          end

          # |@ soma.microfold.theme.expr.local
          #
          # |@block
          # Evaluates to the value of a local with the given name. Locals
          # are produced by argument specifications. See `soma.microfold.theme.argspec`
          # to learn more.
          # |@endblock
          matchpi %[(local name_)] do
            unless value = env[name]?
              issues.severe("local `#{ML.compact(name)}` not found")
              return
            end

            value
          end

          # |@ soma.microfold.theme.expr.*
          #
          # |@block
          # Evaluates to a number between 0 and 1, interpreted from a percentage.
          # |@endblock
          #
          # @key soma.microfold.theme.expr a -- Percentage value expression.
          matchpi %[(* a_ percents)] do
            return unless n = eval?(theme, env, a, issues)

            unless n.type.number?
              issues.severe("`* … percents` expects a number, but got: #{ML.compact(n)}")
              return
            end

            n = n.unsafe_as_n

            if Term[0] <= n <= Term[1]
              n
            else
              n = Math.min(Math.max(n, Term[0]), Term[100])
              n / Term[100]
            end
          end

          # |@ soma.microfold.theme.expr.*
          #
          # |@block
          # Evaluates to a number in degrees, wrapped to the [0, 360) range.
          # |@endblock
          #
          # @key soma.microfold.theme.expr a -- Degree value expression.
          matchpi %[(* a_ degrees)] do
            return unless n = eval?(theme, env, a, issues)

            unless n.type.number?
              issues.severe("`* … degrees` expects a number, but got: #{ML.compact(n)}")
              return
            end

            n = n.unsafe_as_n
            n % Term[360]
          end

          # |@ soma.microfold.theme.expr.*
          #
          # |@block
          # Evaluates to the product of two numeric expressions.
          # |@endblock
          #
          # @key soma.microfold.theme.expr a -- First numeric expression.
          # @key soma.microfold.theme.expr b -- Second numeric expression.
          matchpi %[(* a_ b_)] do
            return unless n = eval?(theme, env, a, issues)
            return unless m = eval?(theme, env, b, issues)

            unless n.type.number? && m.type.number?
              issues.severe("`*` expects two numbers, but got: #{ML.compact(n)}, #{ML.compact(m)}")
              return
            end

            n.unsafe_as_n * m.unsafe_as_n
          end

          matchpi %[(%any° _number _string _boolean)] do
            expr
          end

          otherwise do
            issues.severe("invalid expression")
          end
        end
      end
    end

    # :nodoc:
    alias Arg = ArgUnchecked | ArgChecked

    # :nodoc:
    record ArgUnchecked, term : Term
    # :nodoc:
    record ArgChecked, term : Term

    private def argparse?(pairs : Term::Dict, argspec : Term, argview : StringView, issues) : Arg?
      if argview.empty?
        raise ArgumentError.new
      end

      if argview.surrounded_by?('[', ']')
        interior = argview[1...-1]

        case id = Parse.symbol(interior, issues)
        in Term::Sym
          unless value = pairs[id]?
            issues.minor("key `#{id}` not found in node pairspart")
            return
          end
          return ArgUnchecked.new(value)
        in Parse::Err
          return
        end
      end

      Term.case(argspec) do
        matchpi %[real], %[integer] do
          case result = Parse.number(argview, issues)
          in Term::Num
            ArgChecked.new(Term.of(result))
          in Parse::Err
          end
        end

        otherwise do
          ArgChecked.new(Term.of(argview))
        end
      end
    end

    private def argcheck?(theme, pairs, argspec : Term, arg : ArgUnchecked, issues)
      arg.term
    end

    private def argcheck?(theme, pairs, argspec : Term, arg : ArgChecked, issues)
      argcheck?(theme, pairs, argspec, arg.term, issues)
    end

    private def argcheck?(theme, pairs, argspec : Term, arg : Term, issues)
      Term.case({argspec, arg}) do
        # |@ soma.microfold.theme.argspec.real
        #
        # |@block
        # Matches a real number.
        # |@endblock
        givenpi %[real _number] { arg }

        # |@ soma.microfold.theme.argspec.string
        #
        # |@block
        # Matches any string.
        # |@endblock
        givenpi %[string _string] { arg }

        # |@ soma.microfold.theme.argspec.integer
        #
        # |@block
        # Matches a whole number.
        # |@endblock
        givenpi %[integer (%number (whole _))] { arg }

        # |@ soma.microfold.theme.argspec.one-of
        #
        # |@block
        # Matches a string key from the table *table*, a section in the theme document.
        # |@endblock
        givenpi %[(one-of table_) _string] do
          unless options = theme.table?(table)
            issues.severe("table `#{ML.compact(table)}` does not exist")
            return
          end

          unless value = options[arg]?
            issues.minor("argument not found in the table `#{ML.compact(table)}`")
            return
          end

          value
        end

        otherwise do
          issues.major("invalid argument")
        end
      end
    end

    private def utility_spec_and_rest(theme, utility : StringView, issues, &fn : Term, StringView -> Bool) : Nil
      utility_spec_and_rest(theme, utility, utility.after_end, utility.after_end, issues, fn)
    end

    private def utility_spec_and_rest(theme, l : StringView, sep : StringView, r : StringView, issues, fn)
      if l.empty?
        issues.major("utility `#{r}` does not exist")
        return
      end

      if spec = theme.utility_spec?(Term.of(l))
        return if fn.call(spec, r)
      end

      prefix, sep1, suffix = l.rpartition('-')
      utility_spec_and_rest(theme, prefix, sep1, suffix + sep + r, issues, fn)
    end
  end

  # Mixin rank assigned during decomposition; from least to most preferred.
  enum Rank
    Cascade
    Item
    Style
  end

  # Performs the decomposition pass on *root*.
  #
  # During the decomposition pass, styles and properties are converted to box-
  # targeting mixins that add or subtract key-value pairs in the node's pairspart.
  #
  # Reports any issues that arise during the pass to *issues*.
  def decompose(root : Term, theme : Theme, issues : Issue::Sink) : Term
    pipe(root,
      Decompose.styles(theme, issues),
      Decompose.properties(theme, issues),
    )
  end
end
