module Ww::Alloy
  # Like `Ruleset` but for Alloy `Component`s.
  #
  # NOTE: Headed components (`M1.head?`) are considered more specific than
  # unheaded ones, unconditionally. Even if an unheaded component ends up
  # being more specific in terms of M1, it is still preferred less than
  # a headed one.
  #
  # TODO: Use `M1::PatternSet` once it's a little less clumsy.
  class ComponentSet
    # :nodoc:
    def initialize(
      @headed : Hash(Term, Slice(Component)),
      @unheaded : Slice(Component),
    )
    end

    def self.map(objects : Enumerable, &) : ComponentSet
      headed = {} of Term => Slice(Component)
      unheaded = Pf::Kit.stack_array(Component)

      objects.each_with_index do |object, index|
        component = yield object, index

        unless head = component.head
          unheaded << component
          next
        end

        headed.update(head, Slice(Component).empty, &.append(component))
      end

      # Sort by specificity descending so that more specific components are
      # checked first.

      headed.each do |head, bucket|
        bucket.sort! do |a, b|
          b.pattern.specificity <=> a.pattern.specificity
        end
      end

      unheaded.sort! do |a, b|
        b.pattern.specificity <=> a.pattern.specificity
      end

      new(headed, unheaded.to_unsafe_readonly_slice!)
    end

    # Yields each component in the set in the order of lookup.
    def each(& : Component ->) : Nil
      @headed.each_value do |bucket|
        bucket.each do |component|
          yield component
        end
      end

      @unheaded.each { |component| yield component }
    end

    # Yields each component matching *term*, along with the corresponding indexable
    # of match envs.
    def each_with_matches(term : Term, & : Component, Indexable(Term::Dict) ->) : Nil
      return unless @unheaded.present? || term.type.dict?

      pass do
        next unless dict = term.as_d?
        next unless head = dict.items.first?
        next unless bucket = @headed[head]?

        bucket.each do |component|
          matches = M1.matches(Term[], component.pattern.op, term)
          next unless matches.present?

          yield component, matches
        end
      end

      @unheaded.each do |component|
        matches = M1.matches(Term[], component.pattern.op, term)
        next unless matches.present?

        yield component, matches
      end
    end
  end

  # A template is grouped with its set of components (if any) into an Alloy *unit*.
  struct Unit
    getter template : Template
    getter components : ComponentSet

    # :nodoc:
    def initialize(@template, @components)
    end

    # Returns the approximate bytesize of this unit.
    #
    # NOTE: The real bytesize may be wildly different, this is simply a "memory-
    # inspired" caching heuristic. It uses `weigh`.
    def approx_bytesize : UInt64
      {% begin %}
        k = {
        {% for member in Template.union_types %}
          instance_sizeof({{member}}),
        {% end %}
        }.max.to_u64

        base = Alloy.weigh(@template)
        @components.each do |component|
          base += Alloy.weigh(component.template)
        end

        k * base
      {% end %}
    end
  end

  # Represents an Alloy component, which is basically a rule whose right-hand
  # side (*template*) is recursively evaluated before substitution occurs.
  #
  # Headed components can be analyzed at compile-time, possibly providing references
  # during `register`, and thus, improving caching. Refs for unheaded components,
  # on the other hand, are "unknowable" ahead-of-time.
  defcase Component,
    head : Term?,
    pattern : Pattern,
    template : Template

  # Represents an Alloy *sheet*, which is an ML document containing an Alloy
  # template along with some components. For example:
  #
  # ```wwml
  # ;; Components
  # (X a_) => ^a
  # (Y b_) => ^b
  #
  # ;; Alloy template
  # (^let foo: 100
  #       bar: 200
  #   (X ^foo)
  #   (Y ^bar))
  # ```
  defrecord Sheet, document : Term

  # Constructs an Alloy `Sheet`.
  def sheet(document : Term) : Sheet
    Sheet.new(document)
  end

  # *filter* is used to distinguish between literal nodes (e.g. `(foo 1 2 3)`)
  # and components.
  private def recognize(template : Term, filter : Term -> Bool) : Template
    Term.case(template, block_type: {:proc, filter : (Term -> Bool)}) do
      # |@ alloy.var
      #
      # |@pattern
      # _symbol
      #
      # |@block
      # `^x` is replaced with the value of the variable `x`.
      #
      # `^\x` is replaced with the pretty-printed value of `x` (see also: `nitrene.ml/display`).
      #
      # `^*x` expects `x` to refer to a dictionary. It is replaced by the items of
      # that dictionary, spliced.
      matchpi %{_symbol} do
        id = template.unsafe_as_sym
        continue unless id.prefixed_by?('^') # Fast path

        case id
        when .prefixed_by?('^', '\\')
          name = id.ldrop(nchars: 2)

          DisplayVar.new(name)
        when .prefixed_by?('^', '*')
          # ^*qux -> (^* qux)
          name = id.ldrop(nchars: 2)

          SpliceVar.new(name)
        else
          # ^qux -> (^ qux)
          name = id.ldrop(nchars: 1)

          Var.new(name)
        end
      end

      # |@ alloy.literal
      #
      # |@pattern
      # (%atom)
      #
      # |@block
      # Literals are kept unchanged.
      #
      # ```wwml
      # 100 ;; => 100
      # ```
      matchpi %{_symbol}, %{_number}, %{_string}, %{_boolean}, %{_blob} do
        Literal.new(template)
      end

      # |@ alloy.expr
      #
      # |@pattern
      # (^ expr_)
      #
      # |@key expr nitrene
      #
      # |@block
      # Replaces itself with the value of a Nitrene expression *expr*.
      #
      # ```wwml
      # ;; ^(...) is a shorthand for (^ (...)).
      # (foo ^(+ 1 2) ^(* 3 4))
      # ;; => (foo 3 12)
      # ```
      matchpi %{(^ expr_)} do
        NiExpr.new(expr)
      end

      # |@ alloy.splice
      #
      # |@pattern
      # (^* expr_)
      #
      # |@key expr nitrene
      #
      # |@block
      # Replaces itself with the items of a dictionary returned by the Nitrene
      # expression *expr*.
      #
      # ```wwml
      # ;; ^*(...) is a shorthand for (^* (...)).
      # (foo ^*(1 2 3))
      # ;; => (foo 1 2 3)
      # ```
      matchpi %{(^* expr_)} do
        NiSplice.new(NiExpr.new(expr))
      end

      # |@ alloy.literal
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
        NiSplice.new(NiExpr.new(Term.of(:literal, body)))
      end

      # |@ alloy.splice
      #
      # |@pattern
      # (^splice body_*)
      #
      # |@key body alloy
      # Template expressions to splice.
      #
      # |@block
      # Replaces itself with zero or more results of template expressions
      # in *body*.
      #
      # `^splice` is particularly useful when you want to "return" multiple nodes
      # from an Alloy component:
      #
      # ```wwml
      # (X) => (^splice 1 2 3)
      #
      # (frobnicate (X))
      # ;; => (frobnicate 1 2 3)
      # ```
      matchpi %{(^splice body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        Splice.new(children)
      end

      # |@ alloy.module
      #
      # |@pattern
      # (^module bindings_dict body_*)
      #
      # |@key bindings
      # A dictionary of bindings. Keys are *inner* variables and values are
      # *outer* variables.
      #
      # |@key body alloy
      #
      # |@block
      # Replaces itself with *body*, spliced, with *body* expanded in an isolated
      # environment where only explicitly *bound* variables are allowed.
      #
      # The reason `^module` exists is *caching*. Sometimes Alloy can't figure
      # out which variables a template expression refers to. In fact, this can
      # happen even in very simple cases, such as `^(+ a b)`.
      #
      # This is because Nitrene is opaque to Alloy, so all it sees is a "blank",
      # a black box, with no way of knowing which variables are referenced in it.
      # So it gives up; and this giving up poisons everything up the tree -- until
      # reaching a `^module` or the toplevel.
      #
      # By wrapping "black boxes" like these in a module you condition their environment
      # in such a way that they can't refer to any other variable (global or local,
      # in a component or outside it), at runtime. Simultaneously, modules serve as
      # a compile-time hint to Alloy, which tells it that this particular part can
      # be cached if *bindings* didn't change. In an ideal world, Alloy would do
      # that automatically, but we don't live in such a world. Alloy tries to do
      # this for many expressions, however; so the uses of `^module` should
      # be strategic.
      #
      # ```wwml
      # (^let a: 100
      #       b: 200
      #       c: 300
      #   ;; :b is a shorthand for b: b
      #   (^module {foo: a, :b}
      #     (qux ^(+ foo b) ^c)))
      #
      # ;; => (qux 300)
      # ;;
      # ;; Notice how `c` disappeared as there's no value for it inside the module.
      # ```
      matchpi %{(^module bindings_dict body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        bindings_slice = bindings.ee.to_compact_readonly_slice do |(inner, outer)|
          next unless inner = inner.as_sym?
          next unless outer = outer.as_sym?

          {inner, outer}
        end

        Module.new(bindings_slice, Splice.new(children))
      end

      # |@ alloy.case
      #
      # |@pattern
      # (^case (value expr_) branches_*)
      # (^match expr_ branches_*)
      #
      # |@key expr nitrene
      # Expression to match the result of.
      #
      # |@key branches alloy.branch
      #
      # |@block
      # Matches the value of *expr*. Replaces itself with the first branch that
      # matched successfully. Removes itself if none of the branches match.
      #
      # ```wwml
      # (^match (+ 2 2)
      #   (when 4 "2 + 2 = 4")
      #   (when _ "Wrong universe"))
      # ```
      matchpi(
        %{(^case (value expr_) branches_*)},
        %{(^match expr_ branches_*)},
      ) do
        branch_templates = branches.items.to_compact_readonly_slice do |branch|
          # |@ alloy.branch
          #
          # |@pattern
          # (when pattern_ body_*)
          #
          # |@key pattern m1.operator
          #
          # |@key body alloy
          #
          # |@block
          # Represents a case branch. If the branch matches, the case template
          # expression is replaced with *body*, spliced.
          #
          # If *pattern* is an M1 source pattern, and it two or more times, `when`
          # effectively acts as an `^each`:
          #
          # ```wwml
          # (numbers
          #   (^match (1 a 2 b 3 c)
          #     (when ⟨±n⟩° ^n)))
          #
          # ;; => (numbers 1 2 3)
          # ```
          Term.matchpi?(branch, %{(when pattern_ body_*)}) do
            children = body.items.to_readonly_slice { |item| recognize(item, filter) }

            CaseWhen.new(Pattern.new(pattern), Splice.new(children))
          end
        end

        Case.new(NiExpr.new(expr), branch_templates)
      end

      # |@ alloy.case
      #
      # |@pattern
      # (^case vars branches_*)
      #
      # |@key branches alloy.branch
      #
      # |@block
      # Matches the variables dict itself. Replaces itself with the first branch
      # that matched successfully.
      #
      # Removes itself if no branches match.
      #
      # ```wwml
      # (^case vars
      #   (when {¦ a} "A is defined")
      #   (when {¦ b} "B is defined")
      #   (when _ "A and B are not defined"))
      # ```
      matchpi %{(^case vars branches_*)} do
        branch_templates = branches.items.to_compact_readonly_slice do |branch|
          Term.matchpi?(branch, %{(when pattern_ body_*)}) do
            children = body.items.to_readonly_slice { |item| recognize(item, filter) }

            CaseWhen.new(Pattern.new(pattern), Splice.new(children))
          end
        end

        VarsCase.new(branch_templates)
      end

      # |@ alloy.case
      #
      # |@pattern
      # (^when (expr_ pattern_) body_*)
      #
      # |@key expr nitrene
      # Expression to match the result of.
      #
      # |@key pattern m1.operator
      # Pattern to match.
      #
      # |@key body alloy
      #
      # |@block
      # A shorthand for a `^match` with a single branch. Removes itself if *pattern*
      # does not match.
      #
      # ```wwml
      # (^let op: (square 4)
      #   (^when (op (square ±n))
      #     ;; Captures are available.
      #     ^(* n n)))
      #
      # ;; => 16
      # ```
      #
      # For more info, see `alloy.branch`.
      #
      # `^when` with a source pattern can be useful for constraints:
      #
      # ```wwml
      # (^let posts:
      #        ({title: "A", author: "John Doe"}
      #         {title: "B", author: "Samantha Doe"}
      #         {title: "C", author: "John Doe"})
      #       search: "John Doe"
      #   (title-list
      #     (^when ((posts author) (⟨{¦ title_ author_}⟩° author_))
      #       ^title))
      #
      # ;; => (title-list "A" "C")
      # ```
      matchpi %{(^when (expr_ pattern_) body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }
        branch = CaseWhen.new(Pattern.new(pattern), Splice.new(children))

        Case.new(NiExpr.new(expr), Slice[branch])
      end

      # |@ alloy.br
      #
      # |@pattern
      # (^br cond_ truthy_ falsey_)
      #
      # |@key cond nitrene
      # |@key truthy alloy
      # |@key falsey alloy
      #
      # |@block
      # Replaces itself with the *truthy* template expression if the condition
      # is truthy. Replaces itself with the *falsey* template expression otherwise.
      #
      # ```wwml
      # (foo
      #   (^br true
      #     a
      #     b)
      #   (^br false
      #     c
      #     d))
      #
      # ;; => (foo a d)
      # ```
      matchpi %{(^br cond_ truthy_ falsey_)} do
        Cond.new(NiExpr.new(cond), recognize(truthy, filter), recognize(falsey, filter))
      end

      # |@ alloy.if
      #
      # |@pattern
      # (^if cond_ body_*)
      #
      # |@key cond nitrene
      # |@key body alloy
      #
      # |@block
      # Replaces itself with the body, spliced, if the condition expression
      # is truthy. Removes itself otherwise.
      #
      # ```wwml
      # (foo
      #   (^if true
      #     a)
      #   (^if false
      #     b))
      #
      # ;; => (foo a)
      # ;; `b` was removed because `false` is not truthey.
      # ```
      matchpi %{(^if cond_ body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        Cond.new(NiExpr.new(cond), Splice.new(children), Splice.new(Slice(Template).empty))
      end

      # |@ alloy.unless
      #
      # |@pattern
      # (^unless cond_ body_*)
      #
      # |@key cond nitrene
      # |@key body alloy
      #
      # |@block
      # Replaces itself with the body, spliced, if the condition expression
      # is falsey. Removes itself otherwise.
      #
      # ```wwml
      # (foo
      #   (^unless true
      #     a)
      #   (^unless false
      #     b))
      #
      # ;; => (foo b)
      # ;; `a` was removed because `true` is not falsey.
      # ```
      matchpi %{(^unless cond_ body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        Cond.new(NiExpr.new(cond), Splice.new(Slice(Template).empty), Splice.new(children))
      end

      # |@ alloy.each
      #
      # |@pattern
      # (^each (iterable_ as pattern_) body_*)
      # (^each (iterable_ item as pattern_) body_*)
      # (^each (iterable_ pair as pattern_) body_*)
      # (^each (iterable_ entry as pattern_) body_*)
      #
      # |@key iterable nitrene
      #
      # |@key pattern m1.operator
      #
      # |@key body alloy
      #
      # |@block
      # Replaces itself with *body*, spliced, for each match of *pattern*
      # on *iterable* (including M1 source pattern matches).
      #
      # - `as` iterates the itemspart and ignores the pairspart.
      # - `item as` iterates the itemspart, passing `(item_ index_)` to the pattern.
      # - `pair as` iterates the pairspart in lexicographical order, passing
      #   `(key_ value_)` to the pattern.
      # - `entry as` iterates the entries (both itemspart and pairspart) in
      #   lexicographical order, items first, passing `(key_ value_)` to
      #   the pattern.
      #
      # ```wwml
      # (^let people: ("John Doe" "Samantha Doe" junk "Sarah Doe")
      #   (group
      #     (^each (people as name_string)
      #       (p ^"Hello, I'm ⸢name⸣!"))))
      #
      # ;; => (group
      # ;;      (p "Hello, I'm John Doe!")
      # ;;      (p "Hello, I'm Samantha Doe!")
      # ;;      (p "Hello, I'm Sarah Doe!"))
      # ```

      matchpi %{(^each (iterable_ as pattern_) body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        EachItem.new(NiExpr.new(iterable), Pattern.new(pattern), Splice.new(children))
      end

      matchpi %{(^each (iterable_ item as pattern_) body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        EachItemEntry.new(NiExpr.new(iterable), Pattern.new(pattern), Splice.new(children))
      end

      matchpi %{(^each (iterable_ pair as pattern_) body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        EachPairEntry.new(NiExpr.new(iterable), Pattern.new(pattern), Splice.new(children))
      end

      matchpi %{(^each (iterable_ entry as pattern_) body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        EachEntry.new(NiExpr.new(iterable), Pattern.new(pattern), Splice.new(children))
      end

      # |@ alloy.let
      #
      # |@pattern
      # (^let body_* ¦ pairs_)
      #
      # |@key pairs nitrene
      # Key is the variable name, and value is the Nitrene expression to evaluate,
      # e.g. `x: (+ a b)`.
      #
      # |@key body
      #
      # |@block
      # Defines zero or more local variables for *body*. Replaces itself with
      # the body, spliced.
      #
      # ```wwml
      # (^let s: (+ 1 2)
      #   ^s)
      #
      # ;; => 3
      # ```
      #
      # NOTE: Variables defined in *pairs* are not visible to each other. In cases
      # of shadowing, the old value is still accessible:
      #
      # ```wwml
      # (^let x: 100
      #   (^let x: (* x 2)
      #     ^x))
      #
      # ;; => 200
      # ```
      matchpi %{(^let body_* ¦ pairs_)} do
        bindings = pairs.ee.to_compact_readonly_slice do |(key, value)|
          next unless key = key.as_sym?

          {key, NiExpr.new(value)}
        end

        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        Let.new(bindings, Splice.new(children))
      end

      # |@ alloy.extend
      #
      # |@pattern
      # (^extend child_ extension_)
      #
      # |@key child alloy
      # Should produce zero or more dictionaries to extend with *extension*.
      # Non-dictionary terms are passed through.
      #
      # |@key extension nitrene
      # Should evaluate to a dictionary, which is unioned with dictionary
      # offspring of *child* (see also: `nitrene.union`).
      #
      # |@block
      # Replaces itself with expansion(s) of *child*. Each dictionary expansion
      # is extended with *extension* using `nitrene.union`.
      #
      # NOTE: *child* is not a rule (component) application site. We will wait
      # until *extension* extends its offspring before trying to find
      # a matching rule.
      #
      # ```wwml
      # (^let a: 1
      #       b: 3
      #       pairs: {precision: 3}
      #   (^extend (/ ^a ^b) pairs))
      #
      # ;; => (/ 1 3 precision: 3)
      # ```
      matchpi %{(^extend child_ extension_)} do
        Extend.new(recognize(child, filter), NiExpr.new(extension))
      end

      # |@ alloy.render
      #
      # |@pattern
      # (^render pattern_ subordinate_ body_*)
      #
      # |@key pattern m1.operator
      # Pattern to match the expansion of *subordinate* against.
      #
      # |@key subordinate alloy
      # The template to expand, and the expansion of which to observe.
      #
      # |@block
      # Renders a subordinate Alloy template, then matches it using *pattern*.
      # The *pattern*'s captures are available to *body*, which `^render` is
      # replaced with, spliced.
      #
      # NOTE: For consistency, the expansion of *subordinate* is always a list.
      # If *subordinate* expands to one term, that's a list of one term; if to
      # zero terms, that's an empty list; if to many terms, that's a list of
      # those terms.
      #
      # ```wwml
      # (^render (greeting_ _*) ;; Match first greeting
      #   (^let people: ("John Doe" "Samantha Doe" "Sarah Doe")
      #     (^each (people as name_string)
      #       (p ^"Hello, I'm ⸢name⸣!")))
      #   (group style: "bg-red"
      #     ^greeting))
      #
      # ;; => (group (p "Hello, I'm John Doe!") style: "bg-red")
      # ```
      matchpi %{(^render pattern_ subordinate_ body_*)} do
        children = body.items.to_readonly_slice { |item| recognize(item, filter) }

        Render.new(Pattern.new(pattern), recognize(subordinate, filter), Splice.new(children))
      end

      matchpi %{_dict} do
        parts = {} of Term => Template
        literal = true

        template.each_entry do |key, value|
          part = recognize(value, filter)
          parts[key] = part
          literal &&= part.is_a?(Literal)
        end

        if literal && !filter.call(template)
          return Literal.new(template)
        end

        Site.new(template.as_d, parts)
      end
    end
  end

  # Parses *template* to find Alloy template expressions; returns the resulting `Unit`.
  # Components are parsed from a prepared *rules* enumerable.
  def recognize(template : Term, rules : Enumerable({Term, Term}) = Slice({Term, Term}).empty) : Unit
    heads = Pf::Kit.stack_array(Term?)
    patterns = Pf::Kit.stack_array(Pattern)

    rules.each do |pattern, _|
      normp = M1.normal(pattern)
      heads << M1.head?(normp)
      patterns << Pattern.new(pattern, normp)
    end

    filter = ->(term : Term) do
      patterns.any? { |pattern| M1.probe?(Term[], pattern.op, term) }
    end

    # Recognize components.
    components = ComponentSet.map(rules) do |(_, body), index|
      Component.new(heads[index], patterns[index], recognize(body, filter))
    end

    Unit.new(recognize(template, filter), components)
  end

  # Parses *sheet* to find Alloy template expressions; returns the resulting `Unit`.
  def recognize(sheet : Sheet) : Unit
    unless document = sheet.document.as_d?
      return recognize(sheet.document)
    end

    rules = Pf::Kit.stack_array({Term, Term})

    template = document.pairspart.transaction do |commit|
      document.items.each do |item|
        Term.case(item) do
          matchpi %{[rule pattern_ body_]} do
            rules << {pattern, body}
          end

          # Usually this branch is hit just once. I.e., most often we have simply:
          #
          #   key0: value0
          #   key1: value1
          #   ...
          #   keyN: valueN
          #
          #   rule0
          #   rule1
          #   ...
          #   ruleM
          #
          #   toplevel
          otherwise do
            commit << item
          end
        end
      end
    end

    recognize(Term.of(template), rules)
  end
end
