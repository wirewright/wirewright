require "./baz5"

CURSORP  = ML.parse1(%([_string (%any° | (| _string)) _string (_*) @_]))
CURSORPE = M1.operator(ML.parse1(%([_string (%any° | (| _string)) _string (_*) @edge_])))

# Constructs an editor rewriter.
def editR : Rewriter
  primitives = ProcRuleset.build do
    rulepi1 %[(+ a_number b_number)] { a + b }
    rulepi1 %[(- a_number b_number)] { a - b }
    rulepi1 %[(* a_number b_number)] { a * b }
    rulepi1 %[(/ a_number (%all b_number (%not 0)))] { a / b }
    rulepi1 %[(~ a_string b_string)] { a.stitch(b) }
    rulepi1 %[(string term_)] { ML.display(term, endl: false) }
    rulepi1 %[(ml ml_string)] do
      begin
        {:"ml/ok", ML.parse1(ml.to(String))}
      rescue ML::SyntaxError
        # TODO: line col message
        {:"ml/err"}
      end
    end

    # TODO: support mixed substring?
    rulepi1 %[(substring s_string (rune b←(%number i32)) (rune e←(%number i32)))] do
      Term::Str::Substring.runes(s.unsafe_as_s, b.to(Int32), e.to(Int32))
    end

    rulepi1 %[(substring s_string (word b←(%number i32)) (word e←(%number i32)))] do
      Term::Str::Substring.words(s.unsafe_as_s, b.to(Int32), e.to(Int32))
    end

    # TODO: take substring by lines.
  end

  selector = ML.parse1(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_))])

  editor_base = ML.parse(File.read("#{__DIR__}/editor.soma.wwml"))
  editor_ruleset  = Ruleset.select(selector, editor_base)

  updownmyr = dfsR(
    switchR(
      { %[($my rewritee_)], envR(Term.of(:"$my")) },
      { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
      { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
    )
  )

  dollarr = dfsR(
    switchR(
      { %[($ rewritee_)], exhR(dfsR(callR(primitives))) },
      { %[($once rewritee_)], callR(primitives) },
    )
  )

  backmapr = chainR(updownmyr, dollarr)

  exhR(relR(CURSORP, absR(rulesetR(editor_ruleset, dfsR(envR), backmapr, noR)), ascent: 2))
end

EDITR = editR

def subsume1(cursor, motion)
  Term.of(cursor.morph({3, cursor[3].size, motion}))
end

def subsume(root, motion, edge)
  if M1::Operator.probe?(Term[edge: edge], CURSORPE, root)
    return subsume1(root, motion)
  end

  unless dict0 = root.as_d?
    return root
  end

  Term.of(dict0.replace { |_, v| subsume(v, motion, edge) })
end

def edit(root : Term, motion : Term, edge = Term.of(:edge, :user)) : Term
  pipe(root, subsume(motion, edge), rewrite(EDITR))
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

# pp dfsR(selR(%[rewritee_number], callR { |term| Rewrite.one(term + 1) })).call(RewriterContext.new(Keypath::Appender.new, obs), Rewrite.one(orig))
# re = exhR exhR exhR exhR exhR exhR exhR(exhR(exhR(itemsR(selR(%[rewritee_number], callR { |term| Rewrite.one(Term.of(:ready, term + 1)) })))))
# re = exhR(absR(selR(%[rewritee_number], callR { |term| Rewrite.one(Term.of(term + 1)) })))


# re = dfsR(rulesetR(rs, dfsR(envR), dfsR(backmapr), noR)) # relR(%[200], oneR(Term.of(123)), ascent: 1)
# re = EDITR

# pp re.call(RewriterContext.new(Random::PCG32.new(rand(UInt64)), Keypath::Appender.new, Term[], obs), Rewrite.one(orig))
