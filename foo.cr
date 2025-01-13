require "./wirewright"

include Ww

struct Applier(PostRewriter)
  def initialize(@post : PostRewriter)
  end

  def call(up0, up1, down, my, matchee0, body)
    app = Term::M1::DefaultApplier.new
    up1, matchee1 = app.call(up0, up1, down, my, matchee0, body)

    case offspring = @post.call(matchee1)
    in Offspring::None
      {up1, matchee1}
    in Offspring::One
      {up1, offspring.term}
    in Offspring::Many
      # TODO: we'd probably want to convert the backspec entry into a () entry
      # somehow if it's not one already
      {up1, Term.of(offspring.list)}
    end
  end
end

module Rule
  extend self

  alias Any = Template | BackmapOne | BackmapMany | Neg

  record Neg
  record Template, body : Term
  record BackmapOne, backspec : Term
  record BackmapMany, toplevel : Term, backspec : Term

  def backmap(rs, pr : Pr::Pos, backspec : Term, matchee : Term) : Term
    Term::M1.backmap(pr.envs, backspec, matchee, applier: rs.applier)
  end

  def offspring(rs, rule : Template, pr : Pr::One, matchee : Term) : Offspring::Any
    if dict = rule.body.as_d?
      Offspring::One.new(Term.of(dict.subst(pr.env)))
    elsif rule.body.type.symbol?
      Offspring::One.new(Term.of(pr.env[rule.body]? || rule.body))
    else
      Offspring::One.new(rule.body)
    end
  end
  
  def offspring(rs, rule : BackmapOne, pr : Pr::One, matchee : Term) : Offspring::Any
    if pr.env.includes?(:"(keypaths)")
      offspring = backmap(rs, pr, rule.backspec, matchee)
    else
      offspring = backmap(rs, pr.pattern.response(matchee, keypaths: true).as(Pr::One), rule.backspec, matchee)
    end

    Offspring::One.new(offspring)
  end

  def offspring(rs, rule : BackmapMany, pr : Pr::One, matchee : Term) : Offspring::Any
    if pr.env.includes?(:"(keypaths)")
      offspring = backmap(rs, pr, rule.backspec, matchee)
    else
      offspring = backmap(rs, pr.pattern.response(matchee, keypaths: true).as(Pr::One), rule.backspec, matchee)
    end

    if rule.backspec.includes?({rule.toplevel}) && (list = offspring.as_d?)
      Offspring::Many.new(list)
    else
      Offspring::One.new(offspring)
    end
  end

  def offspring(rs, rule : Template, pr : Pr::Many, matchee : Term) : Offspring::Any
    list = Term::Dict.build do |commit|
      pr.ones do |one|
        case offspring = offspring(rs, rule, one, matchee)
        in Offspring::One
          commit << offspring.term 
        in Offspring::Many
          commit.concat(offspring.list.items)
        in Offspring::None
        end
      end
    end

    Offspring::Many.new(list)
  end

  def offspring(rs, rule : BackmapOne, pr : Pr::Many, matchee : Term) : Offspring::Any
    if pr.envs.all?(&.includes?(:"(keypaths)"))
      offspring = backmap(rs, pr, rule.backspec, matchee)
    else
      offspring = backmap(rs, pr.pattern.response(matchee, keypaths: true).as(Pr::Many), rule.backspec, matchee)
    end

    Offspring::One.new(offspring)
  end

  def offspring(rs, rule : BackmapMany, pr : Pr::Many, matchee : Term) : Offspring::Any
    unless pr.envs.all?(&.includes?(:"(keypaths)"))
      pr = pr.pattern.response(matchee, keypaths: true).as(Pr::Many)
    end

    list = Term::Dict.build do |commit|
      pr.ones do |one|
        case offspring = offspring(rs, rule, one, matchee)
        in Offspring::One
          commit << offspring.term 
        in Offspring::Many
          commit.concat(offspring.list.items)
        in Offspring::None
        end
      end
    end

    Offspring::Many.new(list)
  end

  def offspring(rs, rule : Neg, pr, matchee)
    Offspring::Neg.new
  end
end

struct Ruleset(ApplierType)
  getter applier : ApplierType

  # :nodoc:
  def initialize(@pset : Pset, @rules : Slice(Rule::Any), @applier : ApplierType)
  end

  def self.select(selector, base, applier)
    rules = [] of Rule::Any

    pset = Pset.select(selector, base) do |normp, env|
      if template = env[:template]?
        rule = Rule::Template.new(template)
      elsif backspec = env[:backspec]?
        rule = Term.case(normp) do
          matchpi %[((%literal %let) (%capture toplevel_) _)] { Rule::BackmapMany.new(toplevel, backspec) }
          otherwise { Rule::BackmapOne.new(backspec) }
        end
      elsif neg = env[:negative]?
        rule = Rule::Neg.new
      else
        next
      end

      rules << rule

      true
    end

    new(pset, rules.to_readonly_slice.dup, applier)
  end

  def call(matchee : Term) : Offspring::Any
    case res = @pset.response(matchee)
    in Pr::Pos
      Rule.offspring(self, @rules[res.pattern.index], res, matchee)
    in Pr::Neg
      Offspring::None.new
    end
  end
end

module Offspring
  alias Any = Neg | Pos
  alias Pos = None | Some
  alias Some = One | Many

  record None

  # Similar to `None`, but indicates to the caller that the matchee
  # and the children must be avoided (never rewritten).
  record Neg

  # Shorthand for `None`.
  def self.none : None
    None.new
  end

  record One, term : Term

  # Shorthand for `One`.
  def self.one(offspring) : One
    One.new(Term.of(offspring))
  end

  record Many, list : Term::Dict

  # Shorthand for `Many`.
  def self.many(offspring : Term::Dict) : Many
    Many.new(offspring)
  end

  # :ditto:
  def self.many(*offspring : Term) : Many
    Many.new(Term[{*offspring}])
  end
end

# Imagine you're a writer who is currently fixing errors in their novel.
# "Fixing an error" involves finding it and rewriting it (and maybe its context)
# to a fixed version, whatever that is.
#
# - The `dfsr` (depth-first search rewrite) way would be to scan through the entire
#   novel, chapter by chapter, paragraph by paragraph, sentence by sentence, word by
#   word, letter by letter. When you encounter a typo, regardless of the level you're
#   currently at, you'll fix it and move on without descending into a fix. For example,
#   if you encountered a chapter-level error, let's say a typo in the chapter title,
#   you'll fix the typo and *move on to the next chapter*. You can then choose to do
#   a second pass, third pass, etc. over the entire book.
# - The `bfsr` (breadth-first search rewrite) way would be to scan through the entire
#   novel, first examining chapter-level errors; if none are found, paragraph-level,
#   and so on. Similarly to dfsr, you won't descend into a fix.
# - The `ubfsr` (upward breadth-first search rewrite) way would be to scan through
#   the entire novel, first examining letter-level errors; if none are found, then word-
#   level, then similarly sentence-level, paragraph-level, chapter-level. By definition
#   you won't descend into a fix.
# - The `absr` (absolute rewrite) way would be to scan through the entire novel, first
#   examining chapter-level errors, then paragraph-level, and so on. Upon finding an error,
#   you will fix it, and immediately go back to the beginning of the novel, and begin
#   scanning from chapter-level down again ("you've backjumped to the root"). It is obvious
#   to see how inefficient this approach -- when used raw -- can be.
# - `relr` (relative rewrite) is a higher-order, parametric rewriter. It accepts a "focus"
#   pattern and another rewriter R as arguments. `relr` expects R's associated ruleset to match
#   *exclusively* terms that contain the focus. So it asks R's associated ruleset for the maximum
#   height of the patterns in it. `relr` assumes the focus is contained somewhere within this
#   height. Thus the `relr` way would be to determine common "focal points" for the kinds of errors
#   you expect; see at which maximum depth they are expected to occur. Then you scan the entire
#   novel, find all "foci", and climb up to maximum depth. Then you call R at that depth.
module Rewriter
  private def self.absr(successor, term : Term) : Offspring::Pos
    case offspring = successor.call(term)
    in Offspring::Some
      return offspring
    in Offspring::Neg
      return Offspring::None.new
    in Offspring::None
    end
  
    unless dict = term.as_d?
      return Offspring::None.new
    end
  
    dict.each_entry_randomized do |key, value|
      case offspring = absr(successor, value)
      in Offspring::One
        if value == offspring.term
          next
        else
          return Offspring::One.new(Term.of(dict.with(key, offspring.term)))
        end
      in Offspring::Many
        if Term[{value}] == offspring.list
          next
        end
        if (index = key.as_n?) && index.in?(Term[0]...Term[dict.items.size])
          return Offspring::One.new(Term.of(dict.replace(Term[index]...Term[index + 1], &.concat(offspring.list.items))))
        else
          return Offspring::One.new(Term.of(dict.with(key, offspring.list)))
        end
      in Offspring::None
      end
    end
  
    Offspring::None.new
  end

  # Returns an *absolute rewriter*: a rewriter that performs an *absolute rewrite*
  # (absr) of some root term. See `Rewriter` for an analogy explaining the different
  # kinds of rewriters.
  #
  # *successor* must respond to `call(Term) : Offspring::Any`.
  def self.absr(successor)
    ->(term : Term) { absr(successor, term) }
  end

  # :nodoc:
  module Relr
    alias Any = Ready | Ascend | None

    record Ready, offspring : Offspring::Pos
    record Ascend, ascent : UInt32
    record None
  end

  private alias O = Term::M1::Operator

  # TODO: use pattern's sketch to guide descent
  private def self.relr0(focus, successor, term, ascent)
    if O.probe?(Term[], focus, term)
      return Relr::Ascend.new(ascent)
    end

    unless dict0 = term.as_d?
      return Relr::None.new
    end

    splices = nil

    dict1 = dict0
    dict0.each_entry do |key, value|
      case res = relr0(focus, successor, value, ascent)
      in Relr::None
        next
      in Relr::Ascend
        unless res.ascent.zero?
          return res.copy_with(ascent: res.ascent - 1)
        end
        offspring = successor.call(value)
      in Relr::Ready
        offspring = res.offspring
      end

      case offspring
      in Offspring::None
      in Offspring::One
        dict1 = dict1.with(key, offspring.term)
      in Offspring::Many
        if index = dict0.index?(key)
          splices ||= [] of {Term::Num, Term::Dict}
          splices << {index, offspring.list}
        else
          dict1 = dict1.with(key, offspring.list)
        end
      end
    end

    if splices
      splices.unstable_sort_by! { |index, _| -index }
      splices.each do |index, list|
        dict1 = dict1.replace(index...index + 1, &.concat(list.items))
      end
    end

    Relr::Ready.new(dict0 == dict1 ? Offspring::None.new : Offspring::One.new(Term.of(dict1)))
  end

  private def self.relr(focus, successor, root : Term, ascent : UInt32 = 0u32) : Offspring::Pos
    case response = relr0(focus, successor, root, ascent)
    in Relr::None   then Offspring::None.new
    in Relr::Ready  then response.offspring
    in Relr::Ascend then successor.call(root)
    end
  end

  # Returns a *relative rewriter*: a rewriter that performs a *relative rewrite*
  # (relr) of a root term by finding all instances of the focus pattern *focus*,
  # then climbing *ascent* levels up to reach a so-called "ceiling", then using
  # *successor* to rewrite the ceiling.
  def self.relr(focus : Term, successor, *, ascent = 0u32)
    ofocus = Term::M1.operator(focus)

    ->(term : Term) { relr(ofocus, successor, term, ascent) }
  end
end

# Makes successive rewrites of *term* until no rewrites are found using
# the given *rewriter*.
#
# *rewriter* must respond to `call(Term) : Offspring::Any`.
#
# Yields latest version of *term* (including the initial term) along with control
# to you on every rewrite. This lets you stop rewriting at the expense of your
# choice, from none (by accepting that some rewrites are infinite) to cheap
# (capping the number of iterations) to expensive (maintaining a "seen" set) etc.;
# or alternatively, by user's request.
def rewrite(term term0 : Term, rewriter, & : Term ->) : Term
  while true
    yield term0

    case offspring = rewriter.call(term0)
    in Offspring::One
      term0 = offspring.term
    in Offspring::Many
      term0 = Term.of(offspring.list)
    in Offspring::None
      break
    end
  end

  term0
end

class TimedOut < Exception
  getter initial : Term
  getter latest : Term

  def initialize(@initial, @latest)
  end
end

# `rewrite` with a capped number of rewrites. Raises `TimedOut` with the latest term
# when the number of rewrites exceeds *cap*.
def rewrite(term : Term, rewriter, *, cap : UInt32 = 2u32**16) : Term
  rewrite(term, rewriter) do |rewrite|
    if cap.zero?
      raise TimedOut.new(term, rewrite)
    end
    cap &-= 1
  end
end

# base = ML.parse(<<-WWML
# ;;(rule qux 0)
# ;;(rule (into x_ (%item° (set x_ values_*))) values)
# ;;(rule (+ a_ 0) a)
# ;;(rule (+ 0 a_) a)
# ;;(rule (+ a_ b_) (sum a b))
# ;;(rule (sum a_ b_) (sum/over a b))

# (-rule (rule _ _))
# (-rule (backmap _ _))
# (-rule (-rule _))


# ;; TODO: allow -rule to accept a "pattern pattern" for matching
# ;; rules that can be allowed in.

# ;;(backmap (swap a_ b_) {a: ↓b, b: ↓a})
# ;;(backmap X←(multiswap a_ b_) {(X): (↓b ↓a)})
# ;;(backmap (delay n_number) {n: (- ↓n 1)})
# ;;(backmap X←(delays (%item° n_number)) {(X): ((delay ↓n))})

# (backmap [_string | _string (M←_ _*) @_] {(M): ()})

# ;; INTERPRET

# ;; When the user types something, this means they want what they've typed to
# ;; be appended to the left-hand side.
# (backmap [_string | _string (M←(type text_string) _*) @_] {M: (input →text)})

# ;; When the user types whitespace, we try to submit the left-hand side.
# (backmap [(%string nonempty) | "" (M←(type " ") _*) @_] {M: lsubmit})

# ;; When the user hits enter with nonempty left-hand side, we submit it;
# ;; similarly for nonempty right-hand side. If both are nonempty, we try
# ;; submitting both as if cutting in the middle.
# (backmap [(%string nonempty) | "" (M←(key enter) _*) @_] {M: lsubmit})
# (backmap ["" | (%string nonempty) (M←(key enter) _*) @_] {M: rsubmit})
# (backmap [(%string nonempty) | (%string nonempty) (M←(key enter) _*) @_] {M: lrsubmit})

# ;; MORPH

# ;; Text input
# (backmap [lhs_string | _string (M←(input text_string) _*) @_] {(M): (), lhs: (~ →lhs →text)})

# ;; Submission
# ;;
# ;; Submission involves parsing left/right-hand sides (or both). If that's
# ;; successful the term is expelled to the left or to the right of the cursor,
# ;; and the corresponding side is cleared.
# (backmap [lhs_string | _string (M←lsubmit _*) @_] {(M): ((ml →lhs) lsubmit/finalize)})
# (backmap [_string | rhs_string (M←rsubmit _*) @_] {(M): ((ml →rhs) rsubmit/finalize)})

# (backmap (_* ⭳pred [lhs_string | _string ((%group M (ml/ok term_) lsubmit/finalize) _*) @_] _*)
#   {(M): (), lhs: "", pred: →term})

# (backmap [_string | _string ((%group M (ml/err) lsubmit/finalize) _*) @_]
#   {(M): ()})

# (backmap (_* [_string | rhs_string ((%group M (ml/ok term_) rsubmit/finalize) _*) @_] ⭳succ _*)
#   {(M): (), rhs: "", succ: →term})

# (backmap [_string | _string ((%group M (ml/err) rsubmit/finalize) _*) @_]
#   {(M): ()})

# ;; Misc: cons
# ;;(backmap [_string | _string ((head_ _*) (cons head_ ok_) _*)])

# ;; (backmap [lhs_string | _string (M←(ml/valid) _*) @_] {M: (ml lhs)})

# ;; (backmap C←["" | "" (M←(dup) _*) @_] {(M): (), (C): (↑C ↑C)})

# TODO: this should become a rewriter test
# ;;(backmap (block (%all (%leaf° (cell n←(%number _ < 10000) @edge_))
# ;;                      (%leaf° (step @edge_ m_number))))
# ;;  {n: (+ ↓n ↓m)})
# ;;
# ;;(block
# ;;  ((cell 0 @x)
# ;;   (cell 0 @y)
# ;;   (cell 0 @z)
# ;;   (step @x 1)
# ;;   (step @y 10)
# ;;   (step @z 100)))
# WWML
# )

def rec(term : Term) : Term
  Term.case(term) do
    matchpi %[(~ a_string b_string)] do
      Term.of(a.stitch(b))
    end

    matchpi %[(ml ml_string)] do
      begin
        Term.of(:"ml/ok", ML.parse1(ml.to(String)))
      rescue ML::SyntaxError
        Term.of({:"ml/err"})
      end
    end

    matchpi %[(substring s_string b←(%number i32) e←(%number i32))] do
      Term.of(s.to(String)[b.to(Int32)..e.to(Int32)]? || "")
    end

    matchpi %[(string arg_)] do
      Term.of(ML.display(arg, endl: false))
    end

    matchpi %[_dict] do
      dict0 = dict1 = term.unsafe_as_d
      dict0.each_entry do |k, v|
        dict1 = dict1.with(k, rec(v))
      end
      if dict0 == dict1
        Term.of(dict0)
      else
        rec(Term.of(dict1))
      end
    end

    otherwise { term }
  end
end

CURSORP  = ML.parse1(%([_string | _string (_*) @_]))
CURSORPE = Term::M1.operator(ML.parse1(%([_string | _string (_*) @edge_])))

def subsume1(cursor, motion)
  Term.of(cursor.morph({3, cursor[3].size, motion}))
end

def subsume(root, motion, edge)
  if Term::M1::Operator.probe?(Term[edge: edge], CURSORPE, root)
    return subsume1(root, motion)
  end

  unless dict0 = root.as_d?
    return root
  end

  Term.of(dict0.replace { |_, v| subsume(v, motion, edge) })
end

def editr(ruleset)
  Rewriter.relr(CURSORP, Rewriter.absr(ruleset), ascent: 2u32)
end

prog = ML.parse(<<-WWML
("hello" | "" ((type " ")) @user)
("" | "world" ((key enter)) @user)
("hello" | "world" ((key enter)) @user)
("hello" | "\\\"hello" ((key enter)) @user)
;;("" | "" ((type "(") (type "button") (key enter) (type "0") (type " ") (type "@xs") (type ")")) @user)
;;(+ 1 ("" | "" ((key left) (key left) (key left)) @user) 2)
;;(+ 1 ("" | "" ((key left)) @user) 2)
;;(+ 1 ("" | "" ((key left)) @user) 2)
;;(+ 1 ("" | "" ((key left)) @user) 2)
;;(+ 1 ("" | "" ((dup) (key left)) @user) 2)

;; TODO: These should somehow be formatted into rewriter tests:
;;(delay 1000)
;;(into foo ((set foo 1 2 3) (set foo qux qyx qix) (set bar 4 5 6)))
;;qux
;;(+ 0 100)
;;(+ 100 0)
;;(+ a b)
;;(delays (100 200 300))
;;(+ c d)
;;(+ 100 200)
;;(swap 100 200)
;;(+ (multiswap 1 2))
WWML
)

# cc = ->(matchee : Term) do
#   case offspring = rs.call(matchee)
#   in Offspring::Some
#     offspring
#   in Offspring::Neg
#     offspring
#   in Offspring::None
#     Term.case(matchee) do
#       matchpi %{(~ a_string b_string)} do
#         Offspring::One.new(Term.of(a.stitch(b)))
#       end

#       otherwise { Offspring::None.new }
#     end
#   end
# end

# rewrite(Rewriter.relr(ML.parse1(%([_string | _string (_*) @_])), Rewriter.absr(rs), ascent: 1u32), prog) do |im|
#   puts ML.display(im)
#   sleep 1.second
# end


# rewriter = Rewriter.absr(cc)

# pp rewrite(rewriter, base)

# Benchmark.ips do |x|
#   x.report("eval") do
# seen = Set(Term).new
#  pp(absr(rs, prog) do |im|
#   # unless seen.add?(im)
#   #   break im
#   # end
#   puts ML.display(im)
#   sleep 1.second
# end) 
#   end
# end

# Benchmark.ips do |x|
#   x.report("parse") do
#     Pset.parse(selector, rules)
#   end

  # matchee = Term.of(:+, 1, 2)
  # # x.report("response") do
  # pp ps.response(matchee)
#   end
# end

# def rewrite(circuit, event, subject0)
#   circuit.each_entry do |_, node|
#     Term.case({node, event}) do
#       givenpi %[(dfsr @pin_ @pout_) boot] do
#         unless dict = subject0.as_d?
#           subject0 = rewrite(circuit, Term.of(:pulse, pin, subject0), subject0)
#           next
#         end

#         dict.pairs.transaction do |commit|
#           dict.items.each do |item|
#             rewrite(circuit, Term.of(:pulse, pin, item), item)
#           end
#         end

#         subject1 = subject0
#         subject0.each_entry do |k, v|
#           subject1 = subject1.with(k, rewrite(circuit, Term.of(:pulse, pin, v), v))
#         end
#         subject0 = subject1
#       end

#       otherwise {}
#     end
#   end

#   subject0
# end

# circuit = ML.parse(<<-WWML
# (dfsr @in @out)
# (pipe @in @out)
# WWML
# )

# pp rewrite(circuit, Term.of(:boot), Term.of(:+, 1, 2))

# 1.
# pp pipe(Term.of(:+, {:_}, {:_}, {:_}), Term::M1.normal, Term::M1.opcount)
# pp pipe(Term.of(:+, {:_}, {:_}, {:_}), Term::M1.normal, Term::M1.depth)
# pp pipe(Term.of(:+, {:_}, {:_}, {:_}), Term::M1.normal, Term::M1.breadth)

# # this one is equal in opcount but not in depth to 1.
# pp pipe(Term.of(:+, :x_, :y_, :z_), Term::M1.normal, Term::M1.opcount)
# pp pipe(Term.of(:+, :x_, :y_, :z_), Term::M1.normal, Term::M1.depth)
# pp pipe(Term.of(:+, :x_, :y_, :z_), Term::M1.normal, Term::M1.breadth)

# # 2. this one is equal in opcount & depth to 1, but not in breadth.
# pp pipe(Term.of(:+, :x_, {:y_, :_}), Term::M1.normal, Term::M1.opcount)
# pp pipe(Term.of(:+, :x_, {:y_, :_}), Term::M1.normal, Term::M1.depth)
# pp pipe(Term.of(:+, :x_, {:y_, :_}), Term::M1.normal, Term::M1.breadth)

# # this one is equal in opcount & depth & breadth to 2
# pp pipe(Term.of(:+, :x_, {:x_, :_}), Term::M1.normal, Term::M1.opcount)
# pp pipe(Term.of(:+, :x_, {:x_, :_}), Term::M1.normal, Term::M1.depth)
# pp pipe(Term.of(:+, :x_, {:x_, :_}), Term::M1.normal, Term::M1.breadth)
# pp pipe(Term.of(:+, :x_, {:x_, :_}), Term::M1.normal, Term::M1.captures, Term::M1.capcount)

# pp pipe(Term.of(:+, :_, :_), Term::M1.normal, Term::M1.specificity).ord
# pp pipe(Term.of(:+, :_number, :_), Term::M1.normal, Term::M1.specificity).ord
# pp pipe(Term.of(:+, :"xs_*", :_), Term::M1.normal, Term::M1.specificity).ord


# todo: fix `walk` walking where it shouldn't (into literals, captures, etc.)
#      
# todo: bounds is wrong right now, we'll have to fix it too.

# rs = Ruleset.parse(
#   ML.parse1(<<-WWML
#   (ruleset
#     (rule qux 0)
#     (rule (+ a_ 0) a)
#     (rule (+ 0 a_) a)
#     (rule (+ a_ b_) (sum a b))
#     (backmap (swap a_ b_) {a: ↓b, b: ↓a}))
#   WWML
#   )
# )
# ps = Pattern::Set.new

# ps.add(Term.of(:qux))
# ps.add(Term.of(:+, :a_, 0))
# ps.add(Term.of(:+, 0, :a_))
# ps.add(Term.of(:+, :a_, :b_))
# ps.add(Term.of(:items, {:"%item°", :x_} ))

# pp ps.response(Term.of(:+, 0, 1))
# pp rs.match?(Term.of(:qux))

# (ruleset @xs ...)
# (ruleset @ys ...)
# ...
#
# Rulesets can also be defined in native code. Maybe we can
# import them using (ruleset @foo)? 
#
# A rule in a ruleset contains pattern (left-hand side) and body (right-hand side).
# Body is uninterpreted by rule (i.e. no distinction between backmap/etc.)
#
# A ruleset may have an "extender": a rule that can be used to extend that ruleset.
# It may emit as many match envs as it wants, but only those that contain `pattern`
# and `body` captures are considered.
#
# One then builds circuits made from "rewriters" such as absr, relr, dfsr, bfsr; rulesets
# (treated as nodes that take a term and emit zero or more match envs); and match env interpreters
# which determine how the match should be interpreted. Some rewriters are higher-order, i.e., they
# take a rewriter as an argument. What we can do is reuse edges for this, parsing them into something
# that native code can understand. Rewriters determine where and in what order rewrites happen,
# but not what kind of rewrite happens. Here is an example circuit:
#
#   (dfsr @in @out) ;; Declare input and output terms
#   (ruleset @choices-1 @in @envs)
#   (ruleset @choices-2 @in @envs)
#   (gsub @spot @envs @out) ;; (gsub) or (backmap) match dicts
# 
# Here is a simple cicuit that does a dfsr without changing anything:
#
#   (dfsr @in @out)
#   (pipe @in @out)
#
# We can introduce a ruleset:
#
# (stage constants
#   (dfsr @in @out)
#   (ruleset @constants @in @matches)
#   ())
#
# relr over a cursor:
#
# (relr [_string | _string (motions_+) @pin_])
