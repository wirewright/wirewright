require "./baz5"

CURSORP  = M1.operator(ML.term(%([_string (%any° | [| _string]) _string (_*) @_])))
CURSORPE = M1.operator(ML.term(%([_string (%any° | [| _string]) _string (_*) @EDGE_])))

def subsume1(cursor, motion)
  Term.of(cursor.morph({3, cursor[3].size, motion}))
end

def subsume(root, motion, edge)
  if M1::Operator.probe?(Term["EDGE": edge], CURSORPE, root)
    return subsume1(root, motion)
  end

  unless dict0 = root.as_d?
    return root
  end

  Term.of(dict0.replace { |_, v| subsume(v, motion, edge) })
end

enum Judgement : UInt8
  # Bad, impassable places such as `log` entries have the worst judgement.
  BadImpassable

  # Bad, passable places such as `frag` value have the worst judgement.
  BadPassable

  # Everything else has good judgement.
  Good
end

def smart_subsume(root, motion, edge)
  impassable = [] of Term::Dict
  passable = [] of Term::Dict
  good = [] of Term::Dict

  smart_subsume(root, motion, edge, Term[], judgement: :good) do |keypath, judgement|
    case judgement
    in .bad_impassable?
      next unless good.empty? && passable.empty?

      impassable << keypath
    in .bad_passable?
      next unless good.empty?

      passable << keypath
    in .good?
      good << keypath
    end
  end

  {good, passable, impassable}.each do |category|
    next if category.empty?

    category.each do |keypath|
      root = root.as_d.follow(keypath.items) { |cursor| subsume1(cursor, motion) }
    end

    break
  end

  Term.of(root)
end

# TODO: this must have more patterns (and be "smarter"!!)
def smart_subsume(root, motion, edge, keypath, judgement : Judgement, &sink : Term::Dict, Judgement ->)
  if M1::Operator.probe?(Term["EDGE": edge], CURSORPE, root)
    sink.call(keypath, judgement)
    return
  end

  return unless dict0 = root.as_d?

  Term.case(dict0) do
    matchpi %{(log @_ in (entries_*))} do
      smart_subsume(entries, motion, edge, keypath.append(3), judgement: :bad_impassable, &sink)
    end

    matchpi %{(cell value_dict @_)} do
      smart_subsume(value, motion, edge, keypath.append(1), judgement: :bad_impassable, &sink)
    end

    matchpi %{(frag value_dict @_)} do
      smart_subsume(value, motion, edge, keypath.append(1), judgement: :bad_passable, &sink)
    end

    otherwise do
      dict0.each_entry do |k, v|
        next if Rhodium.shadow?(k)

        smart_subsume(v, motion, edge, keypath.append(k), judgement, &sink)
      end
    end
  end
end

def editR : Rewriter
  selector = ML.term(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_))])

  editor_base = ML.terms(File.read(RESOURCES / "editor.soma.wwml"))
  editor_ruleset = Ruleset.select(selector, editor_base)

  suggestions_base = ML.terms(File.read(RESOURCES / "editor-suggestions.soma.wwml"))
  suggestion_ruleset = Ruleset.select(selector, suggestions_base)

  # (ruleset editor) ;; Let Ww find editor rules
  # ;; Rely on Ww's native code primitives
  # (ruleset/native primitives
  #   (+ - * / ~ string ml substring/runes substring/words))
  #
  # (rewriter (refR)
  #   (switchR
  #      ($my rewritee_) (envR $my)
  #      ($up rewritee_) (choiceR (envR $up) (envR $my))
  #      ($down rewritee_) (choiceR (envR $down) (envR $my))))
  #
  # (rewriter (dollarR)
  #   (dfsR
  #     (switchR
  #       ($ rewritee_) (exhR (dfsR (callR primitives)))
  #       ($once rewritee_) (callR primitives))))
  #
  # (rewriter (backmapR)
  #   (chainR (refR) (dollarR)))
  #
  # (rewriter (editR)
  #   (exhR
  #     (relR cursor ascent: 3
  #       (absR (rulesetR editor rule: (dfsR (envR))
  #                              backmap: (backmapR)
  #                              missing: noR)))))
  #
  # (master editR)

  refR = dfsR(
    switchR(
      { %[($my rewritee_)], envR(Term.of(:"$my")) },
      { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
      { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
    )
  )

  evalR = dfsR(
    switchR(
      { %[($ rewritee_)], exhR(dfsR(callR(PRIMITIVES))) },
      { %[($once rewritee_)], callR(PRIMITIVES) },
    )
  )

  backmapR = chainR(refR, evalR)

  multiphaseR = chainR(
    absR(rulesetR(suggestion_ruleset, dfsR(envR), backmapR, noR, envopt: Term.of(:env))),
    absR(rulesetR(editor_ruleset, dfsR(envR), backmapR, noR, envopt: Term.of(:env))),
  )

  exhR(relR(CURSORPE, multiphaseR, ascent: 3, env: RelrEnv::Option.new(Term.of(:env))))
end

EDITR = editR

def edit(root root0 : Term, motion : Term, edge = Term.of(:edge, :user), *, smart = false) : Term
  if smart
    root1 = smart_subsume(root0, motion, edge)
  else
    root1 = subsume(root0, motion, edge)
  end

  if root0.same?(root1)
    return root1
  end

  rewrite(root1, EDITR, env: Term["EDGE": edge])
end

# staging0 = orig = Term.of(:qux, { { {"", :|, "", Term[], {:edge, :user} } } }, 2)
# puts "Loaded"
# puts ML.display(pipe(orig, edit(Term.of(:key, :"C-left"))))

# obs = Observer.new do |kp, explanation, rewrite|
#   staging1 = rewrite1(staging0, kp.keypath, rewrite)
#   puts "#{explanation}"
#   puts ML.display(staging0)
#   puts ML.display(staging1.term?)
#   staging0 = staging1.term?
# end

# pp dfsR(selR(%[rewritee_number], callR { |term| Rewrite.one(term + 1) })).call(RewriterContext.new(Backpath::Appender.new, obs), Rewrite.one(orig))
# re = exhR exhR exhR exhR exhR exhR exhR(exhR(exhR(itemsR(selR(%[rewritee_number], callR { |term| Rewrite.one(Term.of(:ready, term + 1)) })))))
# re = exhR(absR(selR(%[rewritee_number], callR { |term| Rewrite.one(Term.of(term + 1)) })))

# re = dfsR(rulesetR(rs, dfsR(envR), dfsR(backmapr), noR)) # relR(%[200], oneR(Term.of(123)), ascent: 1)
# re = EDITR

# pp re.call(RewriterContext.new(Random::PCG32.new(rand(UInt64)), Backpath::Appender.new, Term[], obs), Rewrite.one(orig))
