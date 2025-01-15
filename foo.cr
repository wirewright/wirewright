require "./wirewright"

include Ww

struct Applier(Successor)
  def initialize(@successor : Successor)
  end

  def call(up0, up1, down, my, matchee0, body)
    app = Term::M1::DefaultApplier.new
    up1, matchee1 = app.call(up0, up1, down, my, matchee0, body)

    case offspring = @successor.call(matchee1)
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
    Term::M1.backmap(pr.envs, backspec, matchee, applier: Applier.new(rs.fanout[:backmap]))
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

struct Ruleset(SuccBackmap, SuccRule)
  getter fanout : {backmap: SuccBackmap, rule: SuccRule}

  # :nodoc:
  def initialize(@pset : PatternSet, @rules : Slice(Rule::Any), @fanout)
  end

  def self.select(selector, base, fanout)
    rules = [] of Rule::Any

    pset = PatternSet.select(selector, base) do |normp, env|
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

    new(pset, rules.to_readonly_slice.dup, fanout)
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
  
  # TODO: this isn't really None, come up with a better name? We return None when
  # the node 1) cannot/should not be rewritten into anything 2) when it rewrites into itself.
  # Calling it None may lead to confusion. Calling it Zero will lead to confusion with One/Many,
  # None != no offspring?
  record None

  # Similar to `None`, but indicates to the caller that the matchee
  # and the children must be avoided altogether (never rewritten).
  record Neg

  record One, term : Term
  record Many, list : Term::Dict
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
  # :nodoc:
  module Absr
    def self.absr(successor, term : Term) : Offspring::Pos
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
  end

  # Returns an *absolute rewriter*: a rewriter that performs an *absolute rewrite*
  # (absr) of some root term. See `Rewriter` for an analogy explaining the different
  # kinds of rewriters.
  #
  # *successor* must respond to `call(Term) : Offspring::Any`.
  def self.absr(successor)
    # TODO: struct
    ->(term : Term) { Absr.absr(successor, term) }
  end

  # :nodoc:
  module Relr
    private alias O = Term::M1::Operator

    alias Any = Ready | Ascend | None

    record Ready, offspring : Offspring::Pos
    record Ascend, ascent : UInt32
    record None

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
        # TODO: we can come up with some kind of queue-like approach?
        # Take last, copy up to, concat last, take last, copy up to, ...
        splices.sort_by! { |index, _| -index }
        splices.each do |index, list|
          dict1 = dict1.replace(index...index + 1, &.concat(list.items))
        end
      end
  
      Relr::Ready.new(dict0 == dict1 ? Offspring::None.new : Offspring::One.new(Term.of(dict1)))
    end
  
    def self.relr(focus, successor, root : Term, ascent : UInt32 = 0u32) : Offspring::Pos
      case response = relr0(focus, successor, root, ascent)
      in Relr::None   then Offspring::None.new
      in Relr::Ready  then response.offspring
      in Relr::Ascend then successor.call(root)
      end
    end
  end

  # Returns a *relative rewriter*: a rewriter that performs a *relative rewrite*
  # (relr) of a root term by finding all instances of the focus pattern *focus*,
  # then climbing *ascent* levels up to reach a so-called "ceiling", then using
  # *successor* to rewrite the ceiling.
  def self.relr(focus : Term, successor, *, ascent = 0u32)
    ofocus = Term::M1.operator(focus)

    # TODO: struct
    ->(term : Term) { Relr.relr(ofocus, successor, term, ascent) }
  end
end

class TimedOut < Exception
  getter initial : Term
  getter latest : Term

  def initialize(@initial, @latest)
  end
end

def repeatr(term term0, successor, *, limit : UInt32? = nil, raises : Bool = true) : Offspring::Any
  term1 = term0

  (0..limit).each do
    case offspring = successor.call(term1)
    in Offspring::One
      term1 = offspring.term
    in Offspring::Many
      term1 = Term.of(offspring.list)
    in Offspring::None
      return term0 == term1 ? Offspring::None.new : Offspring::One.new(term1)
    end
  end

  if raises
    raise TimedOut.new(term0, term1)
  end

  term0 == term1 ? Offspring::None.new : Offspring::One.new(term1)
end

def repeatr(successor, **kwargs)
  ->(term : Term) { repeatr(term, successor, **kwargs) }
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
def rewrite(term0 : Term, rewriter, *, limit : UInt32? = nil, raises : Bool = true, & : Term ->) : Term
  term1 = term0

  (0..limit).each do
    yield term1

    case offspring = rewriter.call(term1)
    in Offspring::One
      term1 = offspring.term
    in Offspring::Many
      term1 = Term.of(offspring.list)
    in Offspring::Neg, Offspring::None
      return term1
    end
  end

  if raises
    raise TimedOut.new(term0, term1)
  end

  term1
end

# Block-less `rewrite`.
def rewrite(*args, **kwargs) : Term
  rewrite(*args, **kwargs) {}
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

struct ProcRuleset
  alias ProcRule = Term::Dict -> Offspring::Any
  alias ProcBackmap = Term::Dict -> Term::Dict

  struct Builder
    def initialize(@ruleary : Array({Term, ProcRule | ProcBackmap}))
    end

    def rule(pattern : Term, &fn : ProcRule) : Nil
      @ruleary << {pattern, fn}
    end

    def backmap(pattern : Term, &fn : ProcBackmap) : Nil
      @ruleary << {pattern, fn}
    end
    
    def rulep(ml : String, &fn : ProcRule) : Nil
      rule(ML.parse1(ml), &fn)
    end

    def backmapp(ml : String, &fn : ProcBackmap) : Nil
      backmap(ML.parse1(ml), &fn)
    end

    # :nodoc:
    macro pi(methodp, ml, &block)
      {% icaps = ml.scan(::Ww::Term::CaseContext::RE_CAPTURES).map { |match| (match[1] || match[2]).id }.uniq %}
      {% location = "#{block.filename.id}:#{block.line_number}:#{block.column_number}" %}

      {{methodp}}({{ml}}) do |%env|
        {% for icap in icaps %}
          {% unless block.args.any? { |arg| arg.id == icap.id } %}
            {{icap.id}} = (%env[{{icap.id.symbolize}}]? || raise "case: #{ {{location}} }: missing capture '{{icap.id}}'")
          {% end %}
        {% end %}

        pass(
          {% for capture in block.args %}
            (%env[{{capture.id.symbolize}}]? || raise "rulepi: #{ {{location}} }: missing capture '{{capture.id}}'"),
          {% end %}
        ) {{block}}
      end
    end

    macro rulepi(ml, &block)
      pi(rulep, {{ml}}) {{block}}
    end

    macro backmappi(ml, &block)
      pi(backmapp, {{ml}}) {{block}}
    end

    macro rulepi1(ml, &block)
      rulepi({{ml}}) do {% unless block.args.empty? %} |{{block.args.splat}}| {% end %}
        %result = pass do
          {{block.body}}
        end

        Offspring::One.new(Term.of(%result))
      end
    end
  end

  # :nodoc:
  def initialize(
    @pset : PatternSet,
    @rules : Hash(UInt32, ProcRule)?,
    @backmaps : Hash(UInt32, ProcBackmap)?,
  )
  end

  def self.build(&)
    ruleary = [] of {Term, ProcRule | ProcBackmap}

    builder = Builder.new(ruleary)
    with builder yield builder

    decls = Term::Dict.build do |commit|
      ruleary.each_with_index do |(pattern, proc), index|
        case proc
        in ProcRule    then commit << {:rule, index, pattern}
        in ProcBackmap then commit << {:backmap, index, pattern}
        end
      end
    end

    # TODO: for signed types, add signs e.g. +i8, -i8 to only match the correspondingly signed range
    # (and zero in both cases)

    # todo: use +i32
    selector = ML.parse1(%[(type←(%any rule backmap) index←(%number i32) pattern_)])

    rules = backmaps = nil

    index = 0u32
    pset = PatternSet.select(selector, Term.of(decls)) do |_, env|
      _, proc = ruleary[env[:index].to(Int32)]

      case env[:type]
      when Term.of(:rule)
        rules ||= {} of UInt32 => ProcRule
        rules[index] = proc.as(ProcRule)
      when Term.of(:backmap)
        backmaps ||= {} of UInt32 => ProcBackmap
        backmaps[index] = proc.as(ProcBackmap)
      else
        unreachable
      end

      index += 1

      true
    end

    new(pset, rules, backmaps)
  end

  private def rule?(index) : ProcRule?
    @rules.try { |rules| rules[index]? }
  end

  private def backmap?(index) : ProcBackmap?
    @backmaps.try { |backmaps| backmaps[index]? }
  end

  def call(matchee matchee0 : Term) : Offspring::Any
    case pr = @pset.response(matchee0)
    in Pr::One
      if rule = rule?(pr.pattern.index)
        offspring = rule.call(pr.env)
      end

      if backmap = backmap?(pr.pattern.index)
        unless pr.env.includes?(:"(keypaths)")
          pr = pr.pattern.response(matchee0, keypaths: true).as(Pr::One)
        end
        backspec = backmap.call(pr.env)
        matchee1 = Term::M1.backmap(pr.envs, Term.of(backspec), matchee0)
        offspring = Offspring::One.new(matchee1)
      end

      case offspring
      in Nil
        unreachable
      in Offspring::One
        different = matchee0 != offspring.term
      in Offspring::Many
        different = Term[{matchee0}] != offspring.list
      in Offspring::None, Offspring::Neg
        different = true
      end

      different ? offspring : Offspring::None.new
    in Pr::Many
      raise ""
    in Pr::Neg
      Offspring::None.new
    end
  end
end

def dfsr(successor, term) : Offspring::Pos
  case offspring = successor.call(term)
  in Offspring::Some
    return offspring
  in Offspring::Neg
    return Offspring::None.new
  in Offspring::None
  end

  unless dict0 = term.as_d?
    return Offspring::None.new
  end

  dictr(dict0, dfsr(successor))
end

def dfsr(successor)
  # TODO: struct
  ->(term : Term) { dfsr(successor, term) }
end

def dictr(dict dict0 : Term::Dict, successor)
  splices = nil

  dict1 = dict0
  dict0.each_entry do |key, value|
    case offspring = successor.call(value)
    in Offspring::One
      dict1 = dict1.with(key, offspring.term)
    in Offspring::Many
      if index = dict0.index?(key)
        splices ||= [] of {Term::Num, Term::Dict}
        splices << {index, offspring.list}
      else
        dict1 = dict1.with(key, offspring.list)
      end
    in Offspring::None
    end
  end
  
  if splices
    # TODO: we can come up with some kind of queue-like approach?
    # Take last, copy up to, concat last, take last, copy up to, ...
    splices.sort_by! { |index, _| -index }
    splices.each do |index, list|
      dict1 = dict1.replace(index...index + 1, &.concat(list.items))
    end
  end

  dict0 == dict1 ? Offspring::None.new : Offspring::One.new(Term.of(dict1))
end

private alias O = Term::M1::Operator

def spotr(selector, successor, term)
  if env = O.match?(Term[], selector, term)
    if subject = env[:subject]?
      return successor.call(subject)
    end
  end

  unless dict = term.as_d?
    return Offspring::None.new
  end

  dictr(dict, spotr(selector, successor))
end

def spotr(selector : O::Any, successor)
  # TODO: struct
  ->(term : Term) { spotr(selector, successor, term) }
end

def spotr(selector : Term, successor)
  spotr(Term::M1.operator(selector), successor)
end

def spotrp(selector : String, successor)
  spotr(ML.parse1(selector), successor)
end

def selfr
  ->(term : Term) { Offspring::None.new }
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

SELECTOR = ML.parse1(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_) (-rule pattern←negative_))])
BASE = ML.parse(File.read("#{__DIR__}/editor.soma.wwml"))
NATRS = ProcRuleset.build do
  rulepi1 %[(+ a_number b_number)] { a + b }
  rulepi1 %[(- a_number b_number)] { a - b }
  rulepi1 %[(* a_number b_number)] { a * b }
  rulepi1 %[(/ a_number (%all b_number (%not 0)))] { a / b }
  rulepi1 %[(~ a_string b_string)] { a.stitch(b) }

  # Converts term to a string.
  rulepi1 %[(string term_)] { ML.display(term, endl: false) }

  # Converts (parses) a string into a term.
  rulepi1 %[(ml ml_string)] do
    begin
      {:"ml/ok", ML.parse1(ml.to(String))}
    rescue ML::SyntaxError
      # TODO: line col message
      {:"ml/err"}
    end
  end

  # TODO: support mixed substring?

  # Take substring by runes (characters).
  rulepi1 %[(substring s_string (rune b←(%number i32)) (rune e←(%number i32)))] do
    Term::Str::Substring.runes(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  # Take substring by words (includes spaces).
  rulepi1 %[(substring s_string (word b←(%number i32)) (word e←(%number i32)))] do
    Term::Str::Substring.words(s.unsafe_as_s, b.to(Int32), e.to(Int32))
  end

  # TODO: take substring by lines.
end

RULESET = Ruleset.select(SELECTOR, BASE, fanout: {rule: selfr, backmap: spotrp(%[($ subject_)], repeatr(dfsr(NATRS)))})
# RULESET = Ruleset.select(SELECTOR, BASE, applier: Applier.new(->(term : Term) { Offspring::One.new(rec(term)) }))
REWRITER = editr(RULESET)

def apply(root : Term, motion : Term) : Term
  pipe(root,
    subsume(motion, Term.of(:edge, :user)),
    rewrite(REWRITER, limit: 64u32))
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
#     PatternSet.parse(selector, rules)
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
