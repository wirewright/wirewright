module Ww::Alloy
  # The eval function attempts to evaluate an Alloy expression just before
  # it is passed to `PRIMITIVES`. If the function succeeds, Alloy does not
  # evaluate the returned term any futher. If the function fails, Alloy proceeds
  # to `PRIMITIVES`.

  # The first argument is the expression to be evaluated. The second argument
  # is a continuation which can be used for evaluating the original expression,
  # for evaluating sub-expressions, or both. The third arg is an issue sink where
  # issues must be sent.
  alias Eval = Term, EvalDefault, EvalSubexpr, Issue::Sink -> Term

  alias EvalDefault = Issue::Sink -> Term
  alias EvalSubexpr = Term, Issue::Sink -> Term

  # The refine function runs on a node after it is recursively expanded by Alloy.
  alias Refine = Term, Issue::Sink -> Term::Rep

  # :nodoc:
  record RenderContext, vars : Term::Dict, eval : Eval, refine : Refine

  # Returns an empty rep if no such var exists.
  private def get_var(ctx : RenderContext, name : Term, issues : Issue::Sink, & : Term, Issue::Sink -> T) : T forall T
    issues.adjoin("variable", Term.of(name)) do |issues|
      unless value = ctx.vars[name]?
        issues.major("variable `#{name}` does not exist")
        next Term.rep
      end

      issues.adjoin("variable value", value) do |issues|
        yield value, issues
      end
    end
  end

  # Returns an empty rep if no such var exists.
  private def get_var_dict(ctx : RenderContext, name : Term, issues : Issue::Sink, & : Term, Issue::Sink -> T) : T forall T
    get_var(ctx, name, issues) do |value, issues|
      unless dict = value.as_d?
        issues.major("value must be a dict")
        next Term.rep
      end

      yield value, issues
    end
  end

  # Evaluates an Alloy value expression *expr*. Reports any issues found during
  # evaluation to *issues*. Returns the resulting value.
  private def eval(ctx : RenderContext, expr : Term, issues : Issue::Sink, *, index : Int32? = nil) : Term
    issues.adjoin("while evaluating", expr) do |issues|
      Term.of_case(expr) do
        # |@ alloy.expr.literal
        #
        # |@pattern
        # 'term_ ;; (literal term_)
        #
        # |@block
        # Returns *term* without further evaluation.
        matchpi %{'term_} { term }

        # |@ alloy.expr.literal
        #
        # |@pattern
        # (^ _)
        #
        # |@block
        # Returns itself without further evaluation.
        matchpi %{(^ _)} { expr }

        matchpi %{(pipe state_ seq_*)} do
          memo = eval(ctx, state, issues, index: 1)

          seq.items.each_with_index(offset: 2) do |step, index|
            Term.case(step) do
              matchpi %{(head_ args_* ¦ pairs_)} do
                call = pairs.transaction do |commit|
                  commit << head << Term.of(:literal, memo)
                  commit.concat(args.items)
                end

                memo = eval(ctx, Term.of(call), issues, index: index)
              end

              otherwise do
                issues.minor("unexpected step in `pipe` sequence")
              end
            end
          end

          memo
        end

        matchpi %{(args_* ¦ kwargs_)} do
          default = ->(issues : Issue::Sink) do
            # On the way in.
            value = expr.transaction do |commit|
              # Eval arguments.
              args.items.each_with_index do |arg, index|
                commit.with(index, eval(ctx, arg, issues, index: index))
              end

              # Eval keyword arguments.
              kwargs.each_entry do |key, value|
                commit.with(key, eval(ctx, value, issues))
              end
            end

            value = Term.of(value)

            case r = PRIMITIVES.call(value)
            in Rewrite::None then value
            in Rewrite::One  then r.term
            in Rewrite::Many then Term.of(r.list)
            end
          end

          eval_subexpr = ->(subexpr : Term, issues : Issue::Sink) do
            eval(ctx, subexpr, issues) # index: ???
          end

          ctx.eval.call(expr, default, eval_subexpr, issues)
        end

        matchpi %{_symbol} do
          if value = ctx.vars[expr]?
            return value
          end

          # NOTE: Writing e.g. (^ ^x) is one of the valid ways of escaping. ML will
          # read stuff like ^^^x  as (^ (^ ^x)), which evaluates here to (^ ^x),
          # removing one level of escaping -- exactly what we want.
          #
          # NOTE: we avoid emitting an issue here if index=0, because most likely it's
          # going to be the head of a primitive call. This is a compromise; *of course*
          # we'd want something better. But since the primitives and this `eval` machinery
          # is so hacky regardless, we're fine -- for now.
          if expr.as_sym.prefixed_by?('^') || index == 0
            return expr
          end

          # We can't say it's a major error because it might not be one; nor can we
          # be completely silent because most of the time this branch is hit
          # we're truly looking at a typo or something along those lines...
          issues.minor("symbol `#{expr}` is not an Alloy variable")

          expr
        end

        otherwise { expr }
      end
    end
  end

  private def render_many(issues : Issue::Sink, &) : Term::Rep
    children = Pf::Kit.stack_array(Term)

    submit = ->(ctx : RenderContext, item : Term, index : Int32) do
      expansion = issues.adjoin(key: index, detail: "item") do |issues|
        render0(ctx, item, issues)
      end

      expansion.each { |offspring| children << offspring }

      nil # OR ELSE THE COMPILER CRASHES ...
    end

    yield submit

    Term.rep(children)
  end

  # Yields a dictionary and a proc. The block is expected to iterate through
  # the dict in whatever way it prefers; then, call the proc with each context
  # to use to evaluate *body*.
  private def render_each(ctx : RenderContext, iteratee : Term, body : Term::Dict, issues : Issue::Sink, &) : Term::Rep
    issues.adjoin("`^each` template expression") do |issues|
      iteratee_value = eval(ctx, iteratee, issues)

      issues.adjoin("iteratee", iteratee_value) do |issues|
        unless iteratee_dict = iteratee_value.as_d?
          issues.major("iteratee must be a dict")
          return Term.rep
        end

        children = Pf::Kit.stack_array(Term)

        submit = ->(item_ctx : RenderContext) do
          issues.adjoin(Spot::VarDelta.new(ctx.vars, item_ctx.vars)) do |issues|
            expansion = render_many(issues) do |submit_to_body|
              body.items.each_with_index(offset: 2) do |item, index|
                submit_to_body.call(item_ctx, item, index)
              end
            end

            expansion.each do |offspring|
              children << offspring
            end
          end
        end

        yield iteratee_dict, submit

        Term.rep(children)
      end
    end
  end

  private def render0(ctx : RenderContext, template : Term, issues : Issue::Sink) : Term::Rep
    Term.case(template) do
      # |@ alloy.template.^var
      #
      # |@pattern
      # _symbol
      #
      # |@block
      # **Variable**
      # Use the `^` prefix to replace the symbol with a variable's value.
      #
      # **String variable**
      # Use the `^\` prefix to replace the symbol with a string of WwML
      # for the variable's value.
      #
      # **Splice**
      # Use the `^*` prefix to replace the symbol with the items and pairs
      # from a dictionary variable value.
      matchpi %{_symbol} do
        id = template.unsafe_as_sym
        continue unless id.prefixed_by?('^') # Fast path

        case id
        when .prefixed_by?('^', '\\')
          # ^\qux
          kind = :ml
          name = id.ldrop(nchars: 2)
        when .prefixed_by?('^', '*')
          # ^*qux
          kind = :splice
          name = id.ldrop(nchars: 2)
        else
          # ^qux
          kind = :var
          name = id.ldrop(nchars: 1)
        end

        get_var(ctx, Term.of(name), issues) do |value, issues|
          case kind
          when :ml
            Term.rep_of(ML.compact(value))
          when :splice
            unless dict = value.as_d?
              issues.major("value must be a dict")
              return Term.rep
            end

            Term.rep(dict.items)
          when :var
            Term.rep(value)
          else
            unreachable
          end
        end
      end

      matchpi %{_symbol}, %{_number}, %{_string}, %{_boolean} do
        ctx.refine.call(template, issues)
      end

      # |@ alloy.template.^
      #
      # |@pattern
      # (^ expr_)
      #
      # |@key expr alloy.expr
      # Value expression to evaluate.
      #
      # |@block
      # Replaces itself with the result of evaluating an Alloy expression.
      matchpi %{(^ expr_)} do
        issues.adjoin(key: 1, detail: "value expression") do |issues|
          Term.rep(eval(ctx, expr, issues))
        end
      end

      # |@ alloy.template.^case
      #
      # |@pattern
      # (^case subject_ branches_*)
      #
      # |@key subject alloy.template.^case.subject
      # Specifies the subject of case.
      #
      # |@key branches alloy.template.^case.branch
      # Zero or more branches.
      #
      # |@block
      # Use `^case` for M1 pattern matching on the result of a case *subject*.
      # Pattern match envs are exposed as Alloy variables in the corresponding branch.
      #
      # Use wildcard match `(when _ ...)` for catch-all/`else` behavior.
      matchpi %{(^case subject_ branches_*)} do
        issues.adjoin("`^case` template expression") do |issues|
          matchee = nil

          Term.case(subject) do
            # |@ alloy.template.^case.subject.vars
            #
            # |@block
            # Use `vars` to match on the variables the template is being instantiated with.
            #
            # This can be useful for e.g. fallback rendering: if something is missing or
            # otherwise invalid in the variables dict (you get the full power of M1 to
            # determine that), render a placeholder instead.
            matchpi %{vars} do
              matchee = Term.of(ctx.vars)
            end

            # |@ alloy.template.^case.subject.value
            #
            # |@pattern
            # (value expr_)
            #
            # |@key expr alloy.expr
            # Value expression to match on.
            #
            # |@block
            # Use `(value _)` to match on the value of an expression.
            matchpi %{(value expr_)} do
              matchee = eval(ctx, expr, issues)
            end

            otherwise do
              issues.major("invalid `^case` subject, did you mean `(value _)`?")
            end
          end

          if matchee
            branches.items.each_with_index(offset: 2) do |branch, index|
              issues.adjoin(key: index, detail: "`^case` branch") do |issues|
                Term.case(branch) do
                  # |@ alloy.template.^case.branch
                  #
                  # |@pattern
                  # (when pattern_ body_*)
                  #
                  # |@block
                  # A `when` branch consists of a pattern followed by zero or more templates,
                  # which constitute the body of the branch -- to be spliced in place
                  # of the `^case` on match.
                  matchpi %{(when pattern_ body_*)} do
                    matches = M1.matches(pattern, matchee)
                    next unless matches.present?

                    issues.adjoin("`^case` branch with pattern", pattern) do |issues|
                      expansion = render_many(issues) do |submit|
                        matches.each do |env|
                          subctx = ctx.copy_with(vars: Term.union(ctx.vars, env))

                          body.items.each_with_index(offset: 2) do |item, index|
                            submit.call(subctx, item, index)
                          end
                        end
                      end

                      return expansion
                    end
                  end

                  otherwise do
                    issues.major("invalid `^case` branch, expected `(when pattern_ body_*)`")
                    # ignore
                  end
                end
              end
            end
          end

          issues.note("none of the branches matched")

          Term.rep
        end
      end

      # |@ alloy.template.^match
      #
      # |@pattern
      # (^match expr_ branches_*)
      #
      # |@key expr alloy.expr
      # Value expression to match on.
      #
      # |@key branches alloy.template.^case.branch
      # Zero or more branches.
      #
      # |@block
      # `^match` is a shorthand for `(^case (value expr_) branches_*)`.
      matchpi %{(^match expr_ branches_*)} do
        expansion = Term::Dict.build do |commit|
          commit << :"^case" << {:value, expr}
          commit.concat(branches.items)
        end

        expansion = Term.of(expansion)

        issues.adjoin(Spot::Expansion.new("^match", expansion)) do |issues|
          render0(ctx, expansion, issues)
        end
      end

      # |@ alloy.template.^when
      #
      # |@pattern
      # (^when (expr_ pattern_) body_*)
      #
      # |@key expr alloy.expr
      # Value expression to match on.
      #
      # |@key pattern m1.pattern
      # An M1 pattern to match the value of *expr* against. Alloy vars are
      # available in the pattern. Captures made in the pattern are exposed
      # to the body.
      #
      # |@block
      # `^when` is a shorthand for a single-branch `^match`, as in:
      # `(^match expr_ (when pattern_ body_*))`.
      #
      # On mismatch, `^when` replaces itself with nothing (disappears).
      matchpi %{(^when (expr_ pattern_) body_*)} do
        branch = Term::Dict.build do |commit|
          commit << :when << pattern
          commit.concat(body.items)
        end

        expansion = Term.of(:"^match", expr, branch)

        issues.adjoin(Spot::Expansion.new("^when", expansion)) do |issues|
          render0(ctx, expansion, issues)
        end
      end

      # |@ alloy.template.^case'
      #
      # |@pattern
      # (^case' (expr_ pattern_) body_*)
      #
      # |@key expr alloy.expr
      # Value expression to match on.
      #
      # |@key pattern m1.pattern
      # An M1 pattern to match the value of *expr* against. Alloy vars are
      # available in the pattern. Captures made in the pattern are exposed
      # to the body.
      #
      # |@block
      # `^case'` is a shorthand for a single-branch `^case`, as in:
      # `(^case expr_ (when pattern_ body_*))`.
      #
      # On mismatch, `^case'` replaces itself with nothing (disappears).
      matchpi %{(^case' (expr_ pattern_) body_*)} do
        branch = Term::Dict.build do |commit|
          commit << :when << pattern
          commit.concat(body.items)
        end

        expansion = Term.of(:"^case", expr, branch)

        issues.adjoin(Spot::Expansion.new("^case'", expansion)) do |issues|
          render0(ctx, Term.of(expansion), issues)
        end
      end

      # |@ alloy.template.^each
      #
      # |@pattern
      # (^each (iteratee_ as pattern_) body_)
      #
      # |@key iteratee alloy.expr
      # A value expression whose result should be a dict that `^each` will
      # iterate over.
      #
      # |@key pattern m1.pattern
      # An M1 pattern that filters on the items to emit. Alloy vars are
      # available in the pattern. Captures made in the pattern are exposed
      # to the body.
      #
      # |@block
      # Use `as` or the more explicit `as item` to iterate over items of
      # the iteratee dict.
      matchpi %{(^each (iteratee_ as pattern_) body_*)} do
        render_each(ctx, iteratee, body.unsafe_as_d, issues) do |dict, submit|
          dict.items.each do |item|
            matches = M1.matches(pattern, item)
            matches.each do |env|
              submit.call(ctx.copy_with(vars: Term.union(ctx.vars, env)))
            end
          end
        end
      end

      # |@ alloy.template.^each
      #
      # |@pattern
      # (^each (iteratee_ entry as pattern_) body_*)
      #
      # |@key pattern m1.pattern
      # An M1 pattern for filtering on `(key value)`. Alloy vars are available
      # in the pattern. Captures made in the pattern are exposed to the body.
      #
      # |@block
      # Use `entry as` to iterate over entries of the iteratee dict. The order of
      # iteration is guaranteed to be stable across different runs and machines.
      matchpi %{(^each (iteratee_ entry as pattern_) body_*)} do
        render_each(ctx, iteratee, body.unsafe_as_d, issues) do |dict, submit|
          dict.each_entry(in: Term::Dict.entries_ord) do |key, value|
            matches = M1.matches(pattern, Term.of(key, value))
            matches.each do |env|
              submit.call(ctx.copy_with(vars: Term.union(ctx.vars, env)))
            end
          end
        end
      end

      # |@ alloy.template.^each
      #
      # |@pattern
      # (^each (iteratee_ item as pattern_) body_*)
      #
      # |@key pattern m1.pattern
      # An M1 pattern for filtering on `(item index)`. Alloy vars are available
      # in the pattern. Captures made in the pattern are exposed to the body.
      #
      # |@block
      # Use `item as` to iterate over items of the iteratee dict *with index*.
      matchpi %{(^each (iteratee_ item as pattern_) body_*)} do
        render_each(ctx, iteratee, body.unsafe_as_d, issues) do |dict, submit|
          dict.items.each_with_index do |item, index|
            matches = M1.matches(pattern, Term.of(item, index))
            matches.each do |env|
              submit.call(ctx.copy_with(vars: Term.union(ctx.vars, env)))
            end
          end
        end
      end

      # |@ alloy.template.^paste
      #
      # |@pattern
      # (^paste var_ index←(%number i32))
      #
      # |@block
      # You can provide a negative or positive index. Negative indices count
      # from the end of the value dict's items: `-1` means the last item, `-2`
      # second last, and so on (a useful mnemonic could be: `-1` is the first last
      # item, `-2` the second last item, etc.)
      matchpi %{(^paste var_ index←(%number i32))} do
        issues.adjoin("`^paste` template expression") do |issues|
          get_var_dict(ctx, var, issues) do |dict, issues|
            unless item = dict.items[index.to(Int32)]?
              issues.major("dict does not have an item with index #{index}")
              return Term.rep
            end

            Term.rep(item)
          end
        end
      end

      # |@ alloy.template.^paste
      #
      # |@pattern
      # (^paste var_ b←(%number i32) op←(%any ..< ..=) e←(%number i32))
      #
      # |@block
      # You can provide an exclusive `..<` or inclusive `..=` range. The range's
      # begin and end indices may also be negative. Out-of-bounds indices are clamped
      # into the value dict's bounds.
      matchpi %{(^paste var_ b←(%number i32) op←(%any ..< ..=) e←(%number i32))} do
        issues.adjoin("`^paste` template expression") do |issues|
          get_var_dict(ctx, var, issues) do |dict, issues|
            bi = b.to(Int32)
            ei = e.to(Int32)

            bneg = bi < 0
            eneg = ei < 0

            case op
            when Term.of(:"..<")
            when Term.of(:"..=")
              # Convert to exclusive. Note that e.g. -1 + 1 => 0, so we have
              # to remember whether ei < 0 beforehand (as we do above).
              ei += 1
            else
              unreachable
            end

            bi = dict.itemsize + bi if bneg
            ei = dict.itemsize + ei if eneg

            if bi > ei
              # E.g. 1 ..< 0
              # It's not super severe and () is more or less expected, but let
              # them know anyway.
              issues.minor("begin and end indices are out of order")
              next Term.rep
            end

            bi = bi.clamp(0..dict.itemsize)
            ei = ei.clamp(0..dict.itemsize)

            start, count = bi, ei - bi

            selection = dict.items
              .move(start)
              .begin
              .grow(count)
              .collect

            Term.rep_of(selection)
          end
        end
      end

      # |@ alloy.template.^br
      #
      # |@pattern
      # (^br cond_ then_ else_)
      #
      # |@key cond alloy.expr
      # |@key then alloy.template
      # |@key else alloy.template
      #
      # |@block
      # Replaces itself with *else* if the condition expression evaluates to `false`.
      # Replaces itself with *then* otherwise.
      matchpi %{(^br cond_ truthy_ falsey_)} do
        issues.adjoin("`^br` template expression") do |issues|
          if eval(ctx, cond, issues) == Term[false]
            render0(ctx, falsey, issues)
          else
            render0(ctx, truthy, issues)
          end
        end
      end

      # |@ alloy.template.^if
      #
      # |@pattern
      # (^if cond_ body_*)
      #
      # |@key cond alloy.expr
      #
      # |@block
      # Replaces itself with the body (spliced) if the condition expression
      # is not `false`.
      matchpi %{(^if cond_ body_*)} do
        issues.adjoin("`^if` template expression") do |issues|
          if eval(ctx, cond, issues) == Term[false]
            return Term.rep
          end

          render_many(issues) do |submit|
            body.items.each_with_index(offset: 2) do |item, index|
              submit.call(ctx, item, index)
            end
          end
        end
      end

      # |@ alloy.template.^unless
      #
      # |@pattern
      # (^unless cond_ body_*)
      #
      # |@key cond alloy.expr
      #
      # |@block
      # Replaces itself with the body (spliced) if the condition expression
      # is `false`.
      matchpi %{(^unless cond_ body_*)} do
        issues.adjoin("`^unless` template expression") do |issues|
          unless eval(ctx, cond, issues) == Term[false]
            return Term.rep
          end

          render_many(issues) do |submit|
            body.items.each_with_index(offset: 2) do |item, index|
              submit.call(ctx, item, index)
            end
          end
        end
      end

      # |@ alloy.template.^let
      #
      # |@pattern
      # [^let body_*]
      #
      # |@block
      # Replaces itself with the body (spliced), enhanced with zero or more
      # Alloy variables, declared in the pairspart. Each pair's value is an Alloy
      # value expression `alloy.expr`.
      matchpi %{[^let body_*]} do
        vars1 = ctx.vars.transaction do |commit|
          template.each_entry(in: Term::Dict.pairspart) do |key, expr|
            value = issues.adjoin("`^let` definition for", key) do |issues|
              eval(ctx, expr, issues)
            end

            commit.with(key, value)
          end
        end

        subctx = ctx.copy_with(vars: vars1)

        render_many(issues) do |submit|
          body.items.each_with_index(offset: 1) do |item, index|
            submit.call(subctx, item, index)
          end
        end
      end

      # |@ alloy.template.^extend
      #
      # |@pattern
      # (^extend child_ extras_)
      #
      # |@block
      # Replaces itself with the child dict(s, spliced) shallowly merged
      # with *extras*.
      matchpi %{(^extend child_ extras_)} do
        issues.adjoin("`^extend` template expression") do |issues|
          expansion = issues.adjoin(key: 1, detail: "`^extend` child") do |issues|
            render0(ctx, child, issues)
          end

          extras_value = eval(ctx, extras, issues)

          unless extras_dict = extras_value.as_pairsonly_d?
            issues.adjoin("extras", extras_value, &.major("expected a pairsonly dict"))
            return Term.rep
          end

          if offspring = expansion.single?
            unless base_dict = offspring.as_d?
              issues.adjoin("^extend child", offspring, &.major("expected a dict child"))
              return Term.rep
            end

            return Term.rep_of(Term.union(base_dict, extras_dict))
          end

          unions = expansion.to_readonly_slice do |offspring, index|
            unless base_dict = offspring.as_d?
              issues.adjoin("spliced ^extend child", offspring, &.major("expected a dict child"))
              return Term.rep
            end

            Term.of(Term.union(base_dict, extras_dict))
          end

          Term.rep(unions)
        end
      end

      # |@ alloy.template.^verbatim
      #
      # |@pattern
      # (^verbatim body_*)
      #
      # |@block
      # Replaces itself with the body (spliced) without recursive expansion.
      #
      # `^verbatim` is useful in multi-pass Alloy.
      #
      # ```wwml
      # ;; Original:
      # (^verbatim
      #   (^verbatim
      #     x))
      #
      # ;; After Alloy pass 1:
      # (^verbatim
      #   x)
      #
      # ;; After Alloy pass 2:
      # x
      # ```
      matchpi %{(^verbatim body_*)} do
        Term.rep(body.items)
      end

      # |@ alloy.template.^membrane
      #
      # |@pattern
      # [^membrane body_*]
      #
      # |@block
      # Replaces itself with `(^let body_*)` without recursive expansion. Unions
      # evaluated pairspart with expansion `^let`'s pairspart.
      #
      # `^membrane` is useful for establishing communication between multiple passes
      # of Alloy.
      #
      # ```wwml
      # ;; Original
      # (^membrane :x :y
      #   ("Hello" ^x ^y))
      #
      # ;; After Alloy pass 1 with vars={x: 100, y: 200}:
      # (^let x: 100 y: 200
      #    ("Hello" ^x ^y))
      #
      # ;; After Alloy pass 2:
      # ("Hello" 100 200)
      # ```
      #
      # Notably, with `^membrane`, *body* can be enriched with variables from the first
      # pass as well as variables from the second pass:
      #
      # ```wwml
      # ;; Original
      # (^membrane :x :y
      #   ("Hello" ^x ^y ^z))
      #
      # ;; After Alloy pass 1 with vars={x: 100, y: 200}:
      # (^let x: 100 y: 200
      #    ("Hello" ^x ^y ^z))
      #
      # ;; After Alloy pass 2 with vars={z: 300}:
      # ("Hello" 100 200 300)
      # ```
      matchpi %{[^membrane body_*]} do
        issues.adjoin("`^membrane` template expression") do |issues|
          expansion = Term::Dict.build do |commit|
            commit << :"^let"
            commit.concat(body.items)

            template.each_entry(in: Term::Dict.pairspart) do |key, value|
              issues.adjoin("binding", key) do |issues|
                result = eval(ctx, value, issues)

                commit.with(key, {:literal, result})
              end
            end
          end

          Term.rep_of(expansion)
        end
      end

      # |@ alloy.template.^*
      #
      # |@pattern
      # (^* expr_)
      #
      # |@key expr alloy.expr
      # Alloy value expression to obtain the dict to splice.
      #
      # |@block
      # Splices the result of an Alloy value expression *expr*, expected to be a dict.
      matchpi %{(^* expr_)} do
        value = eval(ctx, expr, issues)

        unless dict = value.as_d?
          issues.adjoin("spliced value", value, &.major("value must be a dict"))
          return Term.rep
        end

        Term.rep(dict.items)
      end

      # |@ alloy.template.^splice
      #
      # |@pattern
      # (^splice body_*)
      #
      # |@key body alloy.template
      # Nodes to render and insert.
      #
      # |@block
      # Inserts the renders of multiple nodes at the point where it is used.
      #
      # This template expression is particularly useful when you want to "return"
      # multiple nodes from e.g. an Alloy view component.
      matchpi %{(^splice body_*)} do
        render_many(issues) do |submit|
          body.items.each_with_index(offset: 1) do |node, index|
            submit.call(ctx, node, index)
          end
        end
      end

      # |@ alloy.template.^\.
      #
      # |@pattern
      # (^. keys_+)
      #
      # |@block
      # Follows a keypath into the variables dict, replacing itself with the value
      # thus reached. For example, with vars `{screen: {width: 500, height: 400}}`,
      # you can reach width using `(^. screen width)` and height `(^. screen height)`.
      matchpi %{(^. keys_+)} do
        unless value = ctx.vars.follow?(keys.items)
          issues.adjoin("keypath", keys, &.major("no value at keypath"))
          return Term.rep
        end

        Term.rep(value)
      end

      # |@ alloy.template.^render
      #
      # |@pattern
      # (^render (pattern_ bindings←(%any° vars _dict)) subject_ body_*)
      #
      # |@key pattern m1.pattern
      # Pattern to match the expansion of *subject* against.
      #
      # |@key bindings alloy.expr
      # Binds interior variable names to value expressions evaluated in the current
      # scope (i.e. as in `alloy.template.^let`). Can be set to `vars` to import
      # all variables.
      #
      # |@key subject alloy.template
      # The template to expand.
      #
      # |@block
      # Renders a subordinate Alloy template *subject*, and captures the resulting
      # template expansion into a variable with the given *name*, for *body* to
      # work with. Variables are explicitly imported from the exterior scope
      # using *bindings*. *bindings* can be set to `vars` to import all variables
      # from exterior scope.
      #
      # NOTE: For consistent return results, the expansion of *subject* is always
      # a list. If *subject* expands to one term, that's a list of one term; if to
      # zero terms, that's an empty list; if to many terms, a list of those terms.
      matchpi %{(^render (pattern_ bindings←(%any° vars _dict)) subject_ body_*)} do
        issues.adjoin("`^render` template expression") do |issues|
          # Determine interior vars.
          if bindings == Term.of(:vars)
            interior = ctx.vars
          else
            # bindings : dict
            interior = bindings.transaction do |commit|
              bindings.each_entry do |key, expr|
                value = issues.adjoin("binding for", key) do |issues|
                  eval(ctx, expr, issues)
                end

                commit.with(key, value)
              end
            end
          end

          # Obtain expansion of subject with interior vars.
          expansion = issues.adjoin(key: 2, detail: "`^render` subject") do |issues|
            render0(ctx.copy_with(vars: interior), subject, issues)
          end

          # Normalize expansion.
          matchee = Term.of(expansion)

          render_many(issues) do |submit|
            # Filter on expansion.
            matches = M1.matches(pattern, matchee)
            matches.each do |env|
              subctx = ctx.copy_with(vars: Term.union(ctx.vars, env))
              body.items.each_with_index(offset: 3) do |item, index|
                submit.call(subctx, item, index)
              end
            end
          end
        end
      end

      # |@ alloy.template.^var
      #
      # |@pattern
      # _dict
      #
      # |@block
      # Recursive expansion proceeds into *entries*, meaning both items and pairs.
      #
      # ```wwml
      # (^let caption: "Hello World"
      #       style: "text-neutral-500"
      #   (p ^caption style: ^style))
      #
      # ;; ... is the same as writing:
      #
      # (p "Hello World" style: "text-neutral-500")
      # ```
      matchpi %{_dict} do
        expansion = Term.flatten(template, part: Term::Dict.entries) do |key, value|
          issues.adjoin(key: key, detail: "in key") do |issues|
            render0(ctx, value, issues)
          end
        end

        ctx.refine.call(expansion, issues)
      end
    end
  end

  # Default value for the eval function (noop).
  DEFAULT_EVAL = Eval.new { |expr, default, _, issues| default.call(issues) }

  # Default value for the refine function (noop).
  DEFAULT_REFINE = Refine.new { |term, _| Term.rep(term) }

  # Renders an Alloy *template*. Returns its expansion.
  #
  # Reports issues to *issues*.
  def render0(
    vars : Term::Dict,
    template : Term,
    issues : Issue::Sink, *,
    eval : Eval = DEFAULT_EVAL,
    refine : Refine = DEFAULT_REFINE,
  ) : Term::Rep
    issues.adjoin(Spot::Template.new(template)) do |issues|
      render0(RenderContext.new(vars, eval, refine), template, issues)
    end
  end

  # Renders an Alloy *template*, using *vars* as the initial variables dict.
  #
  # Returns the expanded *template*, and an array of issue backtraces containing
  # issues that were found during expansion (if any).
  #
  # Suppresses issues below *severity*.
  def render0(vars : Term::Dict, template : Term, *, severity : Issue::Severity, **kwargs) : {Term::Rep, Array(Issue::Backtrace)}
    Issue.setup(severity: severity) do |issues|
      render0(vars, template, issues, **kwargs)
    end
  end

  # Renders an Alloy *template*, using *vars* as the initial variables dict.
  #
  # Returns the collapsed expansion of *template* (see `Term.collapse`), and an array
  # of issue backtraces containing issues that were found during expansion (if any).
  #
  # Suppresses issues below *severity*.
  def render_with_issues(
    vars : Term::Dict,
    template : Term, *,
    severity : Issue::Severity = :minor,
    **kwargs,
  ) : {Term, Array(Issue::Backtrace)}
    expansion, issues = render0(vars, template, **kwargs, severity: severity)

    {Term.collapse(expansion), issues}
  end

  # Shorthand for `render_with_issues` that suppresses all issues.
  def render(vars : Term::Dict, template : Term, **kwargs) : Term
    renderout, _ = render_with_issues(vars, template, **kwargs, severity: :quiet)
    renderout
  end

  # Reverses the order of arguments to support `pipe`.
  def render(template : Term, vars = Term[], **kwargs) : Term
    render(vars, template, **kwargs)
  end
end
