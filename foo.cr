require "./wirewright"

include Ww

module Rule
  extend self

  alias Any = Template | BackmapOne | BackmapMany | Barrier

  record Barrier
  record Template, body : Term
  record BackmapOne, backspec : Term
  record BackmapMany, toplevel : Term, backspec : Term

  def offspring(rule : Template, pr : Pr::One, matchee : Term) : Offspring::Any
    if dict = rule.body.as_d?
      Offspring::One.new(Term.of(dict.subst(pr.env)))
    elsif rule.body.type.symbol?
      Offspring::One.new(Term.of(pr.env[rule.body]? || rule.body))
    else
      Offspring::One.new(rule.body)
    end
  end
  
  def offspring(rule : BackmapOne, pr : Pr::One, matchee : Term) : Offspring::Any
    if pr.env.includes?(:"(keypaths)")
      offspring = Term::M1.backmap({pr.env}, rule.backspec, matchee)
    else
      offspring = Term::M1.backmap?(pr.pattern.operator, rule.backspec, matchee)
      offspring ||= raise "BUG: different match in keypath vs. non-keypath mode"
    end

    Offspring::One.new(offspring)
  end

  def offspring(rule : BackmapMany, pr : Pr::One, matchee : Term) : Offspring::Any
    if pr.env.includes?(:"(keypaths)")
      offspring = Term::M1.backmap({pr.env}, rule.backspec, matchee)
    else
      offspring = Term::M1.backmap?(pr.pattern.operator, rule.backspec, matchee)
      offspring ||= raise "BUG: different match in keypath vs. non-keypath mode"
    end

    if rule.backspec.includes?({rule.toplevel}) && (list = offspring.as_d?)
      Offspring::Many.new(list)
    else
      Offspring::One.new(offspring)
    end
  end

  def offspring(rule : Template, pr : Pr::Many, matchee : Term) : Offspring::Any
    list = Term::Dict.build do |commit|
      pr.ones do |one|
        case offspring = offspring(rule, one, matchee)
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

  def offspring(rule : BackmapOne, pr : Pr::Many, matchee : Term) : Offspring::Any
    if pr.envs.all?(&.includes?(:"(keypaths)"))
      offspring = Term::M1.backmap(pr.envs, rule.backspec, matchee)
    else
      offspring = Term::M1.backmap?(pr.pattern.operator, rule.backspec, matchee)
      offspring ||= raise "BUG: different match in keypath vs. non-keypath mode"
    end

    Offspring::One.new(offspring)
  end

  def offspring(rule : BackmapMany, pr : Pr::Many, matchee : Term) : Offspring::Any
    unless pr.envs.all?(&.includes?(:"(keypaths)"))
      pr = pr.pattern.response(matchee, keypaths: true).as?(Pr::Many)
      pr ||= raise "BUG: different match in keypath vs. non-keypath mode"
    end

    list = Term::Dict.build do |commit|
      pr.ones do |one|
        case offspring = offspring(rule, one, matchee)
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

  def offspring(rule : Barrier, pr, matchee)
    Offspring::Barrier.new
  end
end

struct Ruleset
  # :nodoc:
  def initialize(@pset : Pset, @rules : Slice(Rule::Any))
  end

  def self.select(selector, base)
    rules = [] of Rule::Any

    pset = Pset.select(selector, base) do |normp, env|
      if template = env[:template]?
        rule = Rule::Template.new(template)
      elsif backspec = env[:backspec]?
        rule = Term.case(normp) do
          matchpi %[((%literal %let) (%capture toplevel_) _)] { Rule::BackmapMany.new(toplevel, backspec) }
          otherwise { Rule::BackmapOne.new(backspec) }
        end
      elsif barrier = env[:barrier]?
        rule = Rule::Barrier.new
      else
        next
      end

      rules << rule

      true
    end

    new(pset, rules.to_readonly_slice.dup)
  end

  def call(matchee : Term) : Offspring::Any
    case res = @pset.response(matchee)
    in Pr::Pos
      Rule.offspring(@rules[res.pattern.index], res, matchee)
    in Pr::Neg
      Offspring::None.new
    end
  end
end

module Offspring
  alias Any = Barrier | NonBarrier
  alias NonBarrier = None | Some
  alias Some = One | Many

  record None

  # Similar to `None`, but indicates to the caller that the matchee
  # and the children must be avoided (never rewritten).
  record Barrier

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

# One step of absr. Depth-first search of a rewriteable term, followed by
# a rewrite, followed by an immediate backjump to the root.
private def absr1(callable, term : Term) : Offspring::NonBarrier
  case offspring = callable.call(term)
  in Offspring::Some
    return offspring
  in Offspring::Barrier
    return Offspring::None.new
  in Offspring::None
  end

  unless dict = term.as_d?
    return Offspring::None.new
  end

  dict.each_entry_randomized do |key, value|
    case offspring = absr1(callable, value)
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

# Makes successive absolute rewrites of *term* until no rewrites are found.
#
# *callable* must respond to `call(Term) : Offspring::Any`.
#
# Yields latest version of *term* (including the initial term) along with control
# to you on every rewrite. This lets you stop rewriting at the expense of your
# choice, from none (by accepting that some rewrites are infinite) to cheap
# (capping the number of iterations) to expensive (maintaining a "seen" set) etc.;
# or alternatively, by user's request.
def absr(callable, term term0 : Term, & : Term ->) : Term
  while true
    yield term0

    case offspring = absr1(callable, term0)
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

# `absr` with a capped number of rewrites. Raises `TimedOut` with the latest term
# when the number of rewrites exceeds *cap*.
def absr(callable, term, *, cap : UInt32 = 2u32**16) : Term
  absr(callable, term) do |rewrite|
    if cap.zero?
      raise TimedOut.new(term, rewrite)
    end
    cap &-= 1
  end
end

base = ML.parse(<<-WWML
;;(rule qux 0)
;;(rule (into x_ (%item° (set x_ values_*))) values)
;;(rule (+ a_ 0) a)
;;(rule (+ 0 a_) a)
;;(rule (+ a_ b_) (sum a b))
;;(rule (sum a_ b_) (sum/over a b))

;; Disable evaluation of rule, backmap, and barrier itself.
(barrier (rule _ _))
(barrier (backmap _ _))
(barrier (barrier _))


;; TODO: allow barriers to accept a "pattern pattern" for matching
;; rules that can be allowed in.

;;(backmap (swap a_ b_) {a: ↓b, b: ↓a})
;;(backmap X←(multiswap a_ b_) {(X): (↓b ↓a)})
;;(backmap (delay n_number) {n: (- ↓n 1)})
;;(backmap X←(delays (%item° n_number)) {(X): ((delay ↓n))})

(backmap (block (%all (%leaf° (cell n←(%number _ < 10000) @edge_))
                      (%leaf° (step @edge_ m_number))))
  {n: (+ ↓n ↓m)})

(block
  ((cell 0 @x)
   (cell 0 @y)
   (cell 0 @z)
   (step @x 1)
   (step @y 10)
   (step @z 100)))
WWML
)

# `template` maps to a standard rule
# `backspec` maps to a backmap rule
selector = ML.parse1(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_) (barrier pattern←barrier←_))])

rs = Ruleset.select(selector, base)

# pp rs
prog = ML.parse(<<-WWML

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


require "benchmark"

cc = ->(matchee : Term) do
  case offspring = rs.call(matchee)
  in Offspring::Some
    offspring
  in Offspring::Barrier
    offspring
  in Offspring::None
    Term.case(matchee) do
      matchpi %[(+ a_number b_number)] do
        Offspring::One.new(Term.of(a + b))
      end

      matchpi %[(- a_number b_number)] do
        Offspring::One.new(Term.of(a - b))
      end

      otherwise { Offspring::None.new }
    end
  end
end

require "benchmark"

pp absr(cc, base)

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


require "benchmark"

# Benchmark.ips do |x|
#   x.report("parse") do
#     Pset.parse(selector, rules)
#   end

  # matchee = Term.of(:+, 1, 2)
  # # x.report("response") do
  # pp ps.response(matchee)
#   end
# end

# 3. Implement relr.
# 4. Editor TUI
#    subsumption (deliver motion to cursor mailboxes)
#    interpret + morph (as a single step)
#     E.g. interpret:
#      ("" | "" (motion←(key escape) _* ⭳tail) ¦ _ -state_) <-> {(motion): (), tail: (submit), state: special}
#      [_string | _string (motion←(key enter) _* ⭳tail)] <-> {(motion): (), tail: (submit)}
#     -state_ is a shorthand for state: (%pair/absent state) which should be a variant of (%pair/absent) that mounts
#     the keypath to the absent pair in keypath mode.

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
