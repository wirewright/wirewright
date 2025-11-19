module Input
  extend self

  CUE = Term[:|]

  record Context, rewriter : Rewriter

  def context(base : Term) : Context
    Context.new(inputR(base))
  end

  def inputR(ruleset : Ruleset)
    onceR = callR(PRIMITIVES)

    # First rewrite entries, then rewrite self.
    set, exhevalR = recR
    set.call chainR(entriesR(exhevalR), onceR)

    evalR = dfsR(
      switchR(
        { %[($ rewritee_)], exhevalR },
        { %[($once rewritee_)], onceR },
      )
    )

    refR = dfsR(
      switchR(
        { %[($my rewritee_)], envR(Term.of(:"$my")) },
        { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
        { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
      )
    )

    backmapR = chainR(refR, evalR)

    exhR(absR(
      rulesetR(ruleset, noR, backmapR, noR),
    ))
  end

  def inputR(rulebase : Term)
    selector = ML.term(%[(%any° [rule pattern_ template_] [backmap pattern_ backspec_])])
    inputR(Ruleset.select(selector, rulebase))
  end

  # TODO: you can see that the idea of *pattern grammars* emerges here, and I
  # think it will be important for simplifying inputR especially, since its ruleset
  # is full of duplicated constructs related to the pattern grammar below. If only
  # we could refer to things instead of writing them over and over again... The
  # interface is unclear, but something along the lines of
  #
  #    (%grammar <match env pattern>
  #       compose: [compose _string (%choice kernel compose)]
  #       kernel: [| _*])
  #       ;; ...
  #
  # ... could work. The question is, this looks very sloppy. Whereas a pattern
  # grammar is very easy to imagine, how to use one and info from one is unclear,
  # as in, how do we use pattern grammars in practice in e.g. the input code?

  private def compose?(term : Term) : Bool
    Term.case(term) do
      matchpi %{[compose _string arg_]} { kernel?(arg) || compose?(arg) }
      otherwise { false }
    end
  end

  def kernel?(term : Term) : Bool
    Term.case(term) do
      matchpi %{[| _*]} { true }
      otherwise { false }
    end
  end

  private def interior?(term : Term) : Bool
    kernel?(term) || compose?(term)
  end

  # Returns `true` if *term* matches the `input` production from the following
  # pattern grammar.
  #
  # ```text
  # input
  #   [_string _string <interior> _string]
  #   [_string <interior> _string _string]
  #
  # interior
  #   kernel
  #   compose
  #
  # kernel
  #   [| _*]
  #
  # compose
  #   [compose _string <interior>]
  # ```
  #
  # NOTE: an *input field* is different from a *cursor* in that an input field
  # contains a cursor. A cursor is its structural "kernel", containing the motion
  # queue and all sorts of other data, such as the control edge.
  def input?(term : Term) : Bool
    Term.case(term) do
      matchpi %{[_string _string arg_ _string]} { interior?(arg) }
      matchpi %{[_string arg_ _string _string]} { interior?(arg) }
      matchpi %{[_string arg_ _string]} { interior?(arg) }
      otherwise { interior?(term) }
    end
  end

  def enqueue(kernel : Term, event : Term) : Term
    unless Input.kernel?(kernel)
      raise ArgumentError.new
    end

    Term.of(kernel.append(event))
  end

  def send(ctx : Context, rewritee rewritee0 : Term, event : Term, &fn : String -> Bool)
    # Find cursors and direct *event* at them.
    Term.each_keypath_and_node(rewritee0) do |keypath, node|
      Term.case(node) do
        matchpi %{[| _*]} do
          rewritee0 = Term.morph(rewritee0, keypath) do |cursor|
            Term.of(cursor.append(event))
          end

          false # no descend
        end

        otherwise do
          true # descend
        end
      end
    end

    # Check for pending submissions. If some, let the block respond.
    loop do
      rewritee1 = rewritee0 = rewrite(rewritee0, ctx.rewriter)

      Term.each_keypath_and_node(rewritee1) do |keypath, node|
        Term.case(node) do
          # Found a pending submission.
          matchpi %{[(pending |) [submit (reply text_string) _] _*]} do
            accepted = fn.call(text.to(String))

            keypath.push(Term.of(0), Term.of(0)) do
              rewritee1 = Term.morph(rewritee1, keypath) do
                if accepted
                  Term.of(:accepted)
                else
                  Term.of(:rejected)
                end
              end
            end

            false # no descend
          end

          otherwise do
            true # descend
          end
        end
      end

      break if rewritee0 == rewritee1

      rewritee0 = rewritee1
    end

    rewritee0
  end
end
