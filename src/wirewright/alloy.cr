# Alloy is a general-purpose term templating engine for Wirewright.
#
# ```
# vars = Term[x: 100, y: 200]
# template = ML.term(%{(sum "of" ^x ^y is ^(+ x y))})
# renderout = Alloy.render(vars, template)
# renderout # => term (sum "of" 100 200 is 300)
# ```
module Ww::Alloy
  extend self

  # Represents the result of template expansion. You usually do not have to
  # deal with this unless you want to support splicing of top-level Alloy
  # templates or views into your term-of-choice.
  alias Expansion = Assign | Splice | Err

  # Represents the expansion of the current term into one offspring term
  # (possibly the same term).
  record Assign, term : Term

  # Represents the expansion of the current term into zero or more offspring
  # terms (possibly containing the original term).
  record Splice, offspring : Term::Dict

  # Represents the absence of an expansion due to an error (e.g. undefined variable).
  # Alloy nodes may choose to handle this differently in principle; but in practice,
  # for consistency, we *omit* them from the template. This has the nice property that
  # if multiple Alloy passes are stacked, errors in one will not be "injected" into
  # the next pass.
  #
  # If an `Err` occurs at the top-level, we replace it with the empty dict `()`. If you
  # for some reason want to distinguish between empty dicts and errs, then use one of
  # the `X` methods which return Expansions: `renderX`.
  record Err

  # :nodoc:
  record Context, vars : Term::Dict

  # Contains Alloy-related `Issue::Spot`s.
  module Spot
    # Represents the difference between *env* and *vars*, *env* being a guaranteed
    # superset of *vars*.
    record VarDelta, vars : Term::Dict, env : Term::Dict do
      include Issue::Spot
    end

    # Marks the beginning of an Alloy view. Keypaths and other spots
    # below `ViewSpot` refer to the view passed to `Alloy.render`.
    record View do
      include Issue::Spot
    end

    # Marks the beginning of an Alloy template. Keypaths and other spots
    # below `Template` refer to the template passed to `Alloy.render`.
    record Template do
      include Issue::Spot
    end

    # Marks the beginning of an Alloy component. Keypaths and other spots
    # below `Component` refer to *template*; with *pattern* provided as additional
    # information to search the ruleset, determine source location, or both.
    record Component, pattern : Term, template : Term do
      include Issue::Spot
    end
  end

  # Base dictionary rewriting.
  private def flat_map(keypath : Stack(Term), dict0 : Term::Dict, & : Term -> Expansion) : Expansion
    dict1 = dict0.pairspart.transaction do |commit|
      # Recurse on items.
      dict0.items.each_with_index do |item, index|
        keypath.push(Term.of(index)) do
          case expansion = yield item
          in Err # omit
          in Assign then commit << expansion.term
          in Splice then commit.concat(expansion.offspring.items)
          end
        end
      end

      # Recurse on pairs.
      dict0.each_pair do |key, value|
        keypath.push(key) do
          case expansion = yield value
          in Err
            commit.without(key) # omit
          in Assign
            commit.with(key, expansion.term)
          in Splice
            # We're in a pair, as in:
            #
            #   x: (^* (1 2 3))
            #
            # There are only two possible states for a pair if it is treated like
            # a container:
            #
            #   zero terms -- as in an empty splice or an error
            #   one term   -- as in Assign
            #
            # A splice with more than one term does not fit in a pair -- the extra terms
            # have nowhere to go. We handle this by wrapping such cases in `()`,
            # but, unfortunately, just like at the top-level, this generates a nasty,
            # unpredictable interface; not something as clean as zero/one/many.
            # Anything else would be worse, though; we do normalize e.g. in `^render`,
            # but here, there'd be no easy way to extract vs `^render` (besides, all
            # Alloy templates written so far would be broken!) I am therefore in favor
            # of this behavior as it is an OK compromise between purity and practice.
            case expansion.offspring.size
            when 0 then commit.without(key)
            when 1 then commit.with(key, expansion.offspring[0])
            else
              commit.with(key, expansion.offspring)
            end
          end
        end
      end
    end

    Assign.new(Term.of(dict1))
  end

  private def get_var(ctx : Context, name : Term, issues : Issue::Sink, & : Term, Issue::Sink -> T) : T | Err forall T
    issues.adjoin("variable", Term.of(name)) do |issues|
      unless value = ctx.vars[name]?
        issues.major("variable `#{name}` does not exist")
        next Err.new
      end

      issues.adjoin("variable value", value) do |issues|
        yield value, issues
      end
    end
  end

  private def get_var_dict(ctx : Context, name : Term, issues : Issue::Sink, & : Term, Issue::Sink -> T) : T | Err forall T
    get_var(ctx, name, issues) do |value, issues|
      unless dict = value.as_d?
        issues.major("value must be a dict")
        next Err.new
      end

      yield value, issues
    end
  end

  # Evaluates an Alloy value expression *expr*. Reports any issues found during
  # evaluation to *issues*. Returns the resulting value.
  private def eval(ctx : Context, expr : Term, issues : Issue::Sink) : Term
    issues.adjoin("value expression", expr) do |issues|
      Term.case(expr) do
        # |@ alloy.expr.literal
        #
        # |@block
        # Returns *term* without further evaluation.
        # |@endblock
        matchpi %{(literal term_)} do
          term
        end

        matchpi %{(_ args_* ¦ kwargs_)}, %{(args_* ¦ kwargs_)} do
          # On the way in.
          expr = expr.transaction do |commit|
            # Eval arguments.
            args.items.each_with_index(offset: 1) do |arg, index|
              commit.with(index, eval(ctx, arg, issues))
            end

            # Eval keyword arguments.
            kwargs.each_entry do |key, value|
              commit.with(key, eval(ctx, value, issues))
            end
          end

          expr = Term.of(expr)

          # On the way out, try to rewrite with the primitives ruleset.
          case r = PRIMITIVES.call(expr)
          in Rewrite::None then expr
          in Rewrite::One  then r.term
          in Rewrite::Many then Term.of(r.list)
          end
        end

        matchpi %{_symbol} do
          unless value = ctx.vars[expr]?
            # NOTE: Writing e.g. (^ ^x) is one of the valid ways of escaping. ML will
            # read stuff like ^^^x  as (^ (^ ^x)), which evaluates here to (^ ^x),
            # removing one level of escaping -- exactly what we want.

            unless expr.alloy?
              # We can't say it's an error because it might not be one; nor can we
              # be completely silent because most of the time this branch is hit
              # we're truly looking at a typo or something along those lines...
              issues.minor("symbol `#{expr}` is not an Alloy variable")
            end

            return expr
          end

          value
        end

        otherwise { expr }
      end
    end
  end

  private def render_many(keypath : Stack(Term), issues : Issue::Sink, &) : Assign | Splice
    children = [] of Term

    submit = ->(ctx : Context, item : Term, index : Int32) do
      expansion = keypath.push(Term.of(index)) do
        renderX(ctx, keypath, item, issues)
      end

      case expansion
      in Err # omit
      in Assign then children << expansion.term
      in Splice then children.concat(expansion.offspring.items)
      end
    end

    yield submit

    if children.size == 1
      return Assign.new(children.first)
    end

    Splice.new(Term[children])
  end

  # Yields a dictionary and a proc. The block is expected to iterate through
  # the dict in whatever way it prefers; then, call the proc with each context
  # to use to evaluate *body*.
  private def render_each(ctx : Context, keypath : Stack(Term), iteratee : Term, body : Term::Dict, issues : Issue::Sink, &) : Expansion
    issues.adjoin("`^each` items template expression") do |issues|
      iteratee_value = eval(ctx, iteratee, issues)

      issues.adjoin("iteratee", iteratee_value) do |issues|
        unless iteratee_dict = iteratee_value.as_d?
          issues.major("iteratee must be a dict")
          return Err.new
        end

        children = [] of Term
        submit = ->(item_ctx : Context) do
          issues.adjoin(Spot::VarDelta.new(ctx.vars, item_ctx.vars)) do |issues|
            expansion = render_many(keypath, issues) do |submit_to_body|
              body.items.each_with_index(offset: 2) do |item, index|
                submit_to_body.call(item_ctx, item, index)
              end
            end

            case expansion
            in Assign then children << expansion.term
            in Splice then children.concat(expansion.offspring.items)
            end
          end
        end

        yield iteratee_dict, submit

        if children.size == 1
          return Assign.new(children.first)
        end

        Splice.new(Term[children])
      end
    end
  end

  private def render0(ctx : Context, keypath : Stack(Term), template : Term, issues : Issue::Sink) : Expansion
    Term.case(template) do
      # |@ alloy.template.^case
      #
      # |@block
      # Use `^case` for M1 pattern matching on the result of a case *subject*.
      # Pattern match envs are exposed as Alloy variables in the corresponding branch.
      #
      # Use wildcard match `(when _ ...)` for catch-all/`else` behavior.
      # |@endblock
      #
      # |@key subject alloy.template.^case.subject -- Specifies the subject of case.
      #
      # |@key branches alloy.template.^case.branch -- Zero or more branches.
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
            # |@endblock
            matchpi %{vars} do
              matchee = Term.of(ctx.vars)
            end

            # |@ alloy.template.^case.subject.value
            #
            # |@block
            # Use `(value _)` to match on the value of an expression.
            # |@endblock
            #
            # |@key expr alloy.expr -- Value expression to match on.
            matchpi %{(value expr_)} do
              matchee = eval(ctx, expr, issues)
            end

            otherwise do
              issues.major("invalid `^case` subject, did you mean `(value _)`?")
            end
          end

          if matchee
            branches.items.each_with_index(offset: 2) do |branch, index|
              keypath.push(Term.of(index)) do
                Term.case(branch) do
                  # |@ alloy.template.^case.branch
                  #
                  # |@block
                  # Each `^case` branch consists of a pattern followed by zero or more
                  # templates, which constitute the body of the branch -- to be spliced
                  # n place of the `^case` if successful.
                  # |@endblock
                  matchpi %{(when pattern_ body_*)} do
                    matches = M1.matches(pattern, matchee, env: ctx.vars)
                    next unless matches.present?

                    issues.adjoin("`^case` branch with pattern", pattern) do |issues|
                      expansion = render_many(keypath, issues) do |submit|
                        matches.each do |env|
                          subctx = ctx.copy_with(vars: env)

                          body.items.each_with_index(offset: 2) do |item, index|
                            submit.call(subctx, item, index)
                          end
                        end
                      end

                      return expansion
                    end
                  end

                  otherwise do
                    issues.major("invalid `^case` branch, expected `(when pattern_ body_)`")
                    # ignore
                  end
                end
              end
            end
          end

          issues.note("none of the branches matched")

          Splice.new(Term[])
        end
      end

      # |@ alloy.template.^match
      #
      # |@block
      # `^match` is a shorthand for `(^case (value expr_) branches_*)`.
      # |@endblock
      #
      # |@key expr alloy.expr -- Value expression to match on.
      #
      # |@key branches alloy.template.^case.branch -- Zero or more branches.
      matchpi %{(^match expr_ branches_*)} do
        issues.adjoin("`^match` shorthand for `(^case (value _) _*)`") do |issues|
          expansion = Term::Dict.build do |commit|
            commit << :"^case" << {:value, expr}
            commit.concat(branches.items)
          end

          renderX(ctx, keypath, Term.of(expansion), issues)
        end
      end

      # |@ alloy.template.^match'
      #
      # |@block
      # `^match'` is a shorthand for a single-branch `^match`, as in:
      # `(^match expr_ (when pattern_ body_*))`.
      #
      # On mismatch, `^match'` replaces itself with nothing (disappears).
      # |@endblock
      #
      # |@key expr alloy.expr -- Value expression to match on.
      #
      # |@key pattern m1 -- An M1 pattern to match the value of *expr* against.
      # Alloy vars are available in the pattern. Captures made in the pattern
      # are exposed to the body.
      matchpi %{(^match' (expr_ pattern_) body_*)} do
        issues.adjoin("`^match'` shorthand for `(^match _ (when _ _*))`") do |issues|
          branch = Term::Dict.build do |commit|
            commit << :when << pattern
            commit.concat(body.items)
          end

          expansion = Term.of(:"^match", expr, branch)

          renderX(ctx, keypath, Term.of(expansion), issues)
        end
      end

      # |@ alloy.template.^case'
      #
      # |@block
      # `^case'` is a shorthand for a single-branch `^case`, as in:
      # `(^case expr_ (when pattern_ body_*))`.
      #
      # On mismatch, `^case'` replaces itself with nothing (disappears).
      # |@endblock
      #
      # |@key expr alloy.expr -- Value expression to match on.
      #
      # |@key pattern m1 -- An M1 pattern to match the value of *expr* against.
      # Alloy vars are available in the pattern. Captures made in the pattern
      # are exposed to the body.
      matchpi %{(^case' (expr_ pattern_) body_*)} do
        issues.adjoin("`^case'` shorthand for `(^case _ (when _ _*))`") do |issues|
          branch = Term::Dict.build do |commit|
            commit << :when << pattern
            commit.concat(body.items)
          end

          expansion = Term.of(:"^case", expr, branch)

          renderX(ctx, keypath, Term.of(expansion), issues)
        end
      end

      # |@ alloy.template.^each
      #
      # |@block
      # Use `as` or the more explicit `as item` to iterate over items of
      # the iteratee dict.
      # |@endblock
      #
      # |@key iteratee alloy.expr -- A value expression whose result should be a dict
      # that `^each` will iterate over.
      #
      # |@key pattern m1 -- An M1 pattern that filters on the items to emit. Alloy
      # vars are available in the pattern. Captures made in the pattern are exposed
      # to the body.
      matchpi %{(^each (iteratee_ as pattern_) body_*)} do
        render_each(ctx, keypath, iteratee, body.unsafe_as_d, issues) do |dict, submit|
          dict.items.each do |item|
            matches = M1.matches(pattern, item, env: ctx.vars)
            matches.each { |env| submit.call(ctx.copy_with(vars: env)) }
          end
        end
      end

      # |@ alloy.template.^each
      #
      # |@block
      # Use `entry as` to iterate over entries of the iteratee dict. The order of
      # iteration is guaranteed to be stable across different runs and machines.
      # |@endblock
      #
      # |@key pattern m1 -- An M1 pattern for filtering on `(key value)`. Alloy vars
      # are available in the pattern. Captures made in the pattern are exposed
      # to the body.
      matchpi %{(^each (iteratee_ entry as pattern_) body_*)} do
        render_each(ctx, keypath, iteratee, body.unsafe_as_d, issues) do |dict, submit|
          dict.each_entry_ord do |key, value|
            matches = M1.matches(pattern, Term.of(key, value), env: ctx.vars)
            matches.each { |env| submit.call(ctx.copy_with(vars: env)) }
          end
        end
      end

      # |@ alloy.template.^each
      #
      # |@block
      # Use `item as` to iterate over items of the iteratee dict *with index*.
      # |@endblock
      #
      # |@key pattern m1 -- An M1 pattern for filtering on `(item index)`. Alloy vars
      # are available in the pattern. Captures made in the pattern are exposed to
      # the body.
      matchpi %{(^each (iteratee_ item as pattern_) body_*)} do
        render_each(ctx, keypath, iteratee, body.unsafe_as_d, issues) do |dict, submit|
          dict.items.each_with_index do |item, index|
            matches = M1.matches(pattern, Term.of(item, index), env: ctx.vars)
            matches.each { |env| submit.call(ctx.copy_with(vars: env)) }
          end
        end
      end

      # |@ alloy.template.^paste
      #
      # |@block
      # You can provide a negative or positive index. Negative indices count
      # from the end of the value dict's items: `-1` means the last item, `-2`
      # second last, and so on (a useful mnemonic could be: `-1` is the first last
      # item, `-2` the second last item, etc.)
      # |@endblock
      matchpi %{(^paste var_ index←(%number i32))} do
        issues.adjoin("`^paste` template expression") do |issues|
          get_var_dict(ctx, var, issues) do |dict, issues|
            unless item = dict.items[index.to(Int32)]?
              issues.major("dict does not have an item with index #{index}")
              return Err.new
            end

            Assign.new(item)
          end
        end
      end

      # |@ alloy.template.^paste
      #
      # |@block
      # You can provide an exclusive `..<` or inclusive `..=` range. The range's
      # begin and end indices may also be negative. Out-of-bounds indices are clamped
      # into the value dict's bounds.
      # |@endblock
      matchpi %{(^paste var_ b←(%number i32) op←(%any ..< ..=) e←(%number i32))} do
        issues.adjoin("`^paste` template expression") do
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
              next Splice.new(Term[])
            end

            bi = bi.clamp(0..dict.itemsize)
            ei = ei.clamp(0..dict.itemsize)

            start, count = bi, ei - bi

            selection = dict.items
              .move(start)
              .begin
              .grow(count)
              .collect

            Splice.new(Term[selection])
          end
        end
      end

      # |@ alloy.template.^if
      #
      # |@block
      # Replaces itself with the body (spliced) if the condition expression
      # is not `false`.
      # |@endblock
      #
      # |@key cond alloy.expr
      matchpi %{(^if cond_ body_*)} do
        issues.adjoin("`^if` template expression") do |issues|
          if eval(ctx, cond, issues) == Term[false]
            return Splice.new(Term[])
          end

          render_many(keypath, issues) do |submit|
            body.items.each_with_index(offset: 2) do |item, index|
              submit.call(ctx, item, index)
            end
          end
        end
      end

      # |@ alloy.template.^unless
      #
      # |@block
      # Replaces itself with the body (spliced) if the condition expression
      # is `false`.
      # |@endblock
      #
      # |@key cond alloy.expr
      matchpi %{(^unless cond_ body_*)} do
        issues.adjoin("`^unless` template expression") do |issues|
          unless eval(ctx, cond, issues) == Term[false]
            return Splice.new(Term[])
          end

          render_many(keypath, issues) do |submit|
            body.items.each_with_index(offset: 2) do |item, index|
              submit.call(ctx, item, index)
            end
          end
        end
      end

      # |@ alloy.template.^let
      #
      # |@block
      # Replaces itself with the body (spliced), enhanced with zero or more
      # Alloy variables, declared in the pairspart. Each pair's value is
      # an Alloy value expression `alloy.expr`.
      # |@endblock
      matchpi %{(^let body_* ¦ defns_)} do
        vars1 = ctx.vars.transaction do |commit|
          defns.each_entry do |key, expr|
            value = issues.adjoin("`^let` definition for", key) do |issues|
              eval(ctx, expr, issues)
            end

            commit.with(key, value)
          end
        end

        subctx = ctx.copy_with(vars: vars1)

        render_many(keypath, issues) do |submit|
          body.items.each_with_index(offset: 1) do |item, index|
            submit.call(subctx, item, index)
          end
        end
      end

      # |@ alloy.template.^
      #
      # |@block
      # Replaces itself with the result of evaluating an Alloy expression.
      # |@endblock
      #
      # |@key expr alloy.expr -- Value expression to evaluate.
      matchpi %{(^ expr_)} do
        value = eval(ctx, expr, issues)

        Assign.new(value)
      end

      # |@ alloy.template.^extend
      #
      # |@block
      # Replaces itself with the child dict(s, spliced) shallowly merged
      # with *extras*.
      # |@endblock
      matchpi %{(^extend child_ extras_)} do
        issues.adjoin("`^extend` template expression") do |issues|
          expansion = keypath.push(Term.of(1)) do
            renderX(ctx, keypath, child, issues)
          end

          if expansion.is_a?(Err)
            return expansion
          end

          extras_value = eval(ctx, extras, issues)

          unless extras_dict = extras_value.as_pairsonly_d?
            issues.adjoin("extras", extras_value, &.major("expected a pairsonly dict"))
            return Err.new
          end

          case expansion
          in Assign
            unless base_dict = expansion.term.as_d?
              issues.adjoin("^extend child", expansion.term, &.major("expected a dict child"))
              return Err.new
            end

            expansion.copy_with(term: Term.of(base_dict | extras_dict))
          in Splice
            dict = expansion.offspring.transaction do |commit|
              expansion.offspring.each_item_with_index do |item, index|
                unless base_dict = item.as_d?
                  issues.adjoin("spliced ^extend child", item, &.major("expected a dict child"))
                  return Err.new
                end

                commit.with(index, Term.of(base_dict | extras_dict))
              end
            end

            expansion.copy_with(offspring: dict)
          end
        end
      end

      # |@ alloy.template.^verbatim
      #
      # |@block
      # Replaces itself with the body (spliced) without recursive expansion.
      #
      # Using `^verbatim` may be useful if you use multi-pass Alloy.
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
      # |@endblock
      matchpi %{(^verbatim body_*)} do
        if body.size == 1
          Assign.new(body[0])
        else
          Splice.new(body.unsafe_as_d)
        end
      end

      # |@ alloy.template.^*
      #
      # |@block
      # Splices the result of an Alloy value expression *expr*, expected to be a dict.
      # |@endblock
      #
      # |@key expr alloy.expr -- Alloy value expression to obtain the dict to splice.
      matchpi %{(^* expr_)} do
        value = eval(ctx, expr, issues)

        unless value.type.dict?
          issues.adjoin("spliced value", value, &.major("expected a dict value"))
          return Err.new
        end

        Splice.new(value.itemspart)
      end

      # |@ alloy.template.^splice
      #
      # |@block
      # Inserts the renders of multiple nodes at the point where it is used.
      #
      # This template expression is particularly useful when you want to "return"
      # multiple nodes from e.g. an Alloy view component.
      # |@endblock
      #
      # |@key nodes alloy.template -- Nodes to render and insert.
      matchpi %{(^splice nodes_*)} do
        render_many(keypath, issues) do |submit|
          nodes.items.each_with_index(offset: 1) do |node, index|
            submit.call(ctx, node, index)
          end
        end
      end

      # |@ alloy.template.^\.
      #
      # |@block
      # Follows a keypath into the variables dict, replacing itself with the value
      # thus reached. For example, with vars `{screen: {width: 500, height: 400}}`,
      # you can reach width using `(^. viewport width)` and height `(^. viewport height)`.
      # |@endblock
      matchpi %{(^. keys_+)} do
        unless value = ctx.vars.follow?(keys.items)
          issues.adjoin("keypath", keys, &.major("no value at keypath"))
          return Err.new
        end

        Assign.new(value)
      end

      # |@ alloy.template.^render
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
      # zero or many terms, then the expansion is a list of those.
      # |@endblock
      #
      # |@key pattern m1 -- Pattern to match the expansion of *subject* against.
      #
      # |@key bindings alloy.expr -- Binds interior variable names to value expressions
      # evaluated in the current scope (i.e. as in `alloy.template.^let`). Can be set to
      # `vars` to import all variables.
      #
      # |@key subject alloy.template -- The template to expand.
      matchpi %{(^render (pattern_ vars←(%any° vars _dict)) subject_ body_*)} do
        issues.adjoin("`^render` template expression") do |issues|
          # Determine interior vars.
          if vars == Term.of(:vars)
            interior = ctx.vars
          else
            # vars : dict
            interior = vars.transaction do |commit|
              vars.each_entry do |key, expr|
                value = issues.adjoin("binding for", key) do |issues|
                  eval(ctx, expr, issues)
                end

                commit.with(key, value)
              end
            end
          end

          # Obtain expansion of subject with interior vars.
          expansion = keypath.push(Term.of(2)) do
            renderX(ctx.copy_with(vars: interior), keypath, subject, issues)
          end

          # Normalize expansion.
          case expansion
          in Err    then return Err.new
          in Assign then matchee = Term.of({expansion.term})
          in Splice then matchee = Term.of(expansion.offspring)
          end

          render_many(keypath, issues) do |submit|
            # Filter on expansion.
            matches = M1.matches(pattern, matchee, env: ctx.vars)
            matches.each do |env|
              subctx = ctx.copy_with(vars: env)
              body.items.each_with_index(offset: 3) do |item, index|
                submit.call(subctx, item, index)
              end
            end
          end
        end
      end

      # |@ alloy.template.^var
      #
      # @block
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
      # @endblock
      matchpi %{_symbol} do
        id = template.unsafe_as_sym
        continue unless id.alloy? # Fast path

        id = id.to(String).view
        continue unless id = id.lchop?('^')
        continue unless id.size > 0

        case
        when suffix = id.lchop?('\\')
          continue unless suffix.size > 0

          kind = :ml
          name = Term::Sym.new(suffix.to_s)
        when suffix = id.lchop?('*')
          continue unless suffix.size > 0

          kind = :splice
          name = Term::Sym.new(suffix.to_s)
        else
          kind = :var
          name = Term::Sym.new(id.to_s)
        end

        get_var(ctx, Term.of(name), issues) do |value, issues|
          case kind
          when :ml
            Assign.new(Term.of(ML.compact(value)))
          when :splice
            unless value.type.dict?
              issues.major("value must be a dict")
              return Err.new
            end

            Splice.new(value.unsafe_as_d)
          when :var
            Assign.new(value)
          else
            unreachable
          end
        end
      end

      # |@ alloy.template.^var
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
      # |@endblock
      matchpi %{_dict} do
        flat_map(keypath, template.unsafe_as_d) do |value|
          renderX(ctx, keypath, value, issues)
        end
      end

      otherwise { Assign.new(template) }
    end
  end

  # Renders an Alloy *template*. Returns its expansion.
  #
  # - *ctx* is the context for expansion.
  # - *keypath* specifies a keypath prefix to use.
  #
  # Reports any issues found during expansion to *issues*.
  #
  # NOTE: this is public API, but it offers more control than is usually necessary.
  # Consider non-X overloads (e.g. `render`) before use.
  def renderX(ctx : Context, keypath : Stack(Term), template : Term, issues : Issue::Sink) : Expansion
    issues.adjoin(Issue::Spot::KeypathRef.new(keypath)) do |issues|
      render0(ctx, keypath, template, issues)
    end
  end

  # Renders an Alloy *template*, using *vars* as an initial variables dict.
  #
  # Returns the expanded *template*, and an array of issue backtraces containing
  # issues that were found during expansion (if any).
  #
  # *severity* specifies severity cutoff.
  #
  # NOTE: this is public API, but it offers more control than is usually necessary.
  # Consider non-X overloads (e.g. `render`) before use.
  def renderX(vars : Term::Dict, template : Term, *, severity : Issue::Severity) : {Expansion, Array(Issue::Backtrace)}
    Issue.setup(severity: severity) do |issues|
      issues.adjoin(Spot::Template.new) do |issues|
        render0(Context.new(vars), Stack(Term).new, template, issues)
      end
    end
  end

  private def renderX0(ruleset : Ruleset, cache : ICache(Term, Expansion), keypath : Stack(Term), view : Term, issues : Issue::Sink) : Expansion
    responses = ruleset.responses(view)
    responses.each do |response|
      pr, rule = response

      case pr
      in Pr::One  then env = pr.env
      in Pr::Many then env = pr.envs[0]
      end

      next unless rule.is_a?(Rule::Template)

      expansion = issues.adjoin(Spot::Component.new(rule.pattern, rule.body)) do |issues|
        render0(Context.new(env), keypath, rule.body, issues)
      end

      case expansion
      in Err
        # This should usually not be hit; errors are handled by Alloy nodes
        # such as `^each`; but if we e.g. have an undefined variable at
        # the very top level, then it's our responsibility to contain that.
        return Err.new
      in Assign
        if view == expansion.term # Base case
          return Assign.new(view)
        end

        return renderX(ruleset, cache, keypath, expansion.term, issues)
      in Splice
        children = Term::Dict.build do |commit|
          expansion.offspring.items.each do |item|
            if view == item # Base case
              commit << view
              next
            end

            case item_expansion = renderX(ruleset, cache, keypath, item, issues)
            in Err # omit
            in Assign then commit << item_expansion.term
            in Splice then commit.concat(item_expansion.offspring.items)
            end
          end
        end

        return Splice.new(children)
      end

      # Try next rule
    end

    unless view.type.dict?
      return Assign.new(view)
    end

    flat_map(keypath, view.unsafe_as_d) do |value|
      renderX(ruleset, cache, keypath, value, issues)
    end
  end

  # Renders an Alloy *view*, using *ruleset* as a component database. Only
  # template rules are supported; other rules are ignored. Returns the
  # resulting expansion. Uses *cache* to cache expansions that do not contain
  # any issues.
  #
  # *keypath* specifies a keypath prefix to use.
  #
  # Reports any issues found during expansion to *issues*.
  #
  # NOTE: this is public API, but it offers more control than is usually necessary.
  # Consider non-X overloads (e.g. `render`) before use.
  #
  # TODO: limit recursion depth.
  def renderX(
    ruleset : Ruleset,
    cache : ICache(Term, Expansion),
    keypath : Stack(Term),
    view : Term,
    issues : Issue::Sink,
  ) : Expansion
    if memo = cache[view]?
      return memo
    end

    version0 = issues.version
    expansion = renderX0(ruleset, cache, keypath, view, issues)
    version1 = issues.version

    if version0 == version1
      cache[view] = expansion
    end

    expansion
  end

  # Renders an Alloy *view*, using *ruleset* as a component database. Only
  # template rules are supported; other rules are ignored. Returns the resulting
  # expansion, and an array of issue backtraces containing issues that were
  # found during expansion (if any).
  #
  # *severity* specifies severity cutoff.
  #
  # NOTE: this is public API, but it offers more control than is usually necessary.
  # Consider non-X overloads (e.g. `render`) before use.
  def renderX(
    ruleset : Ruleset,
    cache : ICache(Term, Expansion),
    view : Term, *,
    severity : Issue::Severity,
  ) : {Expansion, Array(Issue::Backtrace)}
    Issue.setup(severity: severity) do |issues|
      issues.adjoin(Spot::View.new) do |issues|
        renderX(ruleset, cache, Stack(Term).new, view, issues)
      end
    end
  end

  # Renders an Alloy *template*, using *vars* as an initial variables dict.
  #
  # Returns the expanded *template*, and an array of issue backtraces containing
  # issues that were found during expansion (if any).
  #
  # *severity* specifies severity cutoff.
  def render_with_issues(
    vars : Term::Dict,
    template : Term, *,
    severity : Issue::Severity = :minor,
  ) : {Term, Array(Issue::Backtrace)}
    expansion, issues = renderX(vars, template, severity: severity)

    case expansion
    in Err    then {Term.of, issues} # Toplevel err resolves to ()
    in Assign then {expansion.term, issues}
    in Splice then {Term.of(expansion.offspring), issues}
    end
  end

  # Renders an Alloy *view*, using *ruleset* as a component database. Only
  # template rules are supported; other rules are ignored. Returns the expanded
  # *view*, and an array of issue backtraces containing issues that were
  # found during expansion (if any).
  #
  # *severity* specifies severity cutoff.
  def render_with_issues(
    ruleset : Ruleset,
    view : Term, *,
    severity : Issue::Severity = :minor,
    cache : ICache(Term, Expansion) = Uncached(Term, Expansion).new,
  ) : {Term, Array(Issue::Backtrace)}
    expansion, issues = renderX(ruleset, cache, view, severity: severity)

    case expansion
    in Err    then {Term.of, issues} # Toplevel err resolves to ()
    in Assign then {expansion.term, issues}
    in Splice then {Term.of(expansion.offspring), issues}
    end
  end

  # Renders an Alloy *template* while suppressing all issues.
  #
  # Shorthand for a similar overload of `render_with_issues`, with severity
  # set to `quiet`.
  def render(vars : Term::Dict, template : Term, **kwargs) : Term
    renderout, _ = render_with_issues(vars, template, **kwargs, severity: :quiet)
    renderout
  end

  # Reverses the order of arguments to support `pipe`.
  def render(template : Term, vars : Term::Dict, **kwargs) : Term
    render(vars, template, **kwargs)
  end

  # Renders an Alloy *view* while suppressing all issues.
  #
  # Shorthand for a similar overload of `render_with_issues`, with severity
  # set to `quiet`.
  def render(ruleset : Ruleset, view : Term, **kwargs) : Term
    renderout, _ = render_with_issues(ruleset, view, **kwargs, severity: :quiet)
    renderout
  end

  # Reverses the order of arguments to support `pipe`.
  def render(view : Term, ruleset : Ruleset, **kwargs) : Term
    render(ruleset, view, **kwargs)
  end
end
