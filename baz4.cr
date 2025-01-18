require "./wirewright"

module Rewrite
  alias Any = Some | None
  alias Some = One | Many

  record None do
    def each(&)
    end

    def reduce(& : Term -> Rewrite::Any)
      self
    end

    def diff(orig : Term)
      self
    end
  end

  record One, term : Term do
    def each(&)
      yield term
    end

    def reduce(& : Term -> Rewrite::Any)
      yield term
    end

    def diff(orig : Term)
      term == orig ? Rewrite.none : self
    end
  end

  record Many, list : Term::Dict do
    def each(&)
      yield Term.of(list)
    end

    def reduce(& : Term -> Rewrite::Any)
      changed = false

      newlist = Term::Dict.build do |commit|
        list.items.each do |item|
          case offspring = yield item
          in Rewrite::None
            commit << item
          in Rewrite::One
            commit << offspring.term
            changed = true
          in Rewrite::Many
            commit.concat(offspring.list.items)
            changed = true
          end
        end
      end

      changed ? Many.new(newlist) : None.new
    end

    def diff(orig : Term)
      list == Term[{orig}] ? Rewrite.none : self
    end
  end

  def self.none
    None.new
  end

  def self.one(term)
    One.new(Term.of(term))
  end

  def self.many(list)
    Many.new(Term[list])
  end
end

# A special Crystal-friendly ruleset. Rules are Crystal procs (later, native code).
struct ProcRuleset
  alias ProcRule = Term::Dict -> Rewrite::Any
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

        Rewrite::One.new(Term.of(%result))
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

    selector = ML.parse1(%[(type←(%any rule backmap) index←(%number +i32) pattern_)])

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

  def call(matchee matchee0 : Term) : Rewrite::Any
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
        offspring = Rewrite::One.new(matchee1)
      end

      case offspring
      in Nil
        unreachable
      in Rewrite::One
        different = matchee0 != offspring.term
      in Rewrite::Many
        different = Term[{matchee0}] != offspring.list
      in Rewrite::None
        different = false
      end

      different ? offspring : Rewrite::None.new
    in Pr::Many
      raise "pr::many not implemented"
    in Pr::Neg
      Rewrite::None.new
    end
  end
end

module Rule
  extend self

  alias Any = Template | BackmapOne | BackmapMany

  record Template, body : Term
  record BackmapOne, backspec : Term
  record BackmapMany, toplevel : Term, backspec : Term
end

struct Ruleset
  # :nodoc:
  def initialize(@pset : PatternSet, @rules : Slice(Rule::Any))
  end

  def self.select(selector, base)
    rules = [] of Rule::Any

    pset = PatternSet.select(selector, base) do |normp, env|
      if template = env[:template]?
        rule = Rule::Template.new(template)
      elsif backspec = env[:backspec]?
        rule = Term.case(normp) do
          matchpi %[((%literal %let) (%capture toplevel_) _)] { Rule::BackmapMany.new(toplevel, backspec) }
          otherwise { Rule::BackmapOne.new(backspec) }
        end
      else
        next
      end

      rules << rule

      true
    end

    new(pset, rules.to_readonly_slice.dup)
  end

  def call(matchee : Term) : {Pr::Pos, Rule::Any}?
    case res = @pset.response(matchee)
    in Pr::Pos
      {res, @rules[res.pattern.index]}
    in Pr::Neg
    end
  end
end

SELECTOR = ML.parse1(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_))])
BASE = ML.parse(File.read("#{__DIR__}/editor.soma.wwml"))
RULESET = Ruleset.select(SELECTOR, BASE)

module Changes
  alias Any = Preview | Accept
  alias Preview = Term, Rewrite::Some ->
  alias Accept = ->
end

# TODO: merge itemsr and pairsr into a single entriesr with an optional
# part arg. 
# TODO: add a flag to use each_entry_randomized

# FIXME: crazy crazy shitcode
# :nodoc:
#
# Less efficient `itemsr` implementation for `Changes::Preview` procs.
def itemsr(changes : Changes::Preview, term : Term, successor, *, limit) : Rewrite::Any?
  return Rewrite.none unless dict0 = term.as_d?

  dict1 = dict0
  changed = false

  (0...dict0.items.size).reverse_each do |index|
    break if limit && limit.zero?

    item0 = dict0[index]

    # Generate a preview proc that will fill the dict with the successor's
    # (potential) rewrite.
    subchanges = Changes::Preview.new do |original, rewrite|
      case rewrite
      in Rewrite::One
        filled = dict1.with(Term[index], rewrite.term)
      in Rewrite::Many
        filled = dict1.replace(Term[index], &.concat(rewrite.list.items))
      end

      changes.call(Term.of(dict1.with(index, original)), Rewrite.one(filled))
    end
    
    # Ask the successor for rewrite, and incorporate it into the dict.
    case rewrite = successor.call(subchanges, Rewrite.one(item0))
    in Rewrite::One
      dict1 = dict1.with(Term[index], rewrite.term)
    in Rewrite::Many
      dict1 = dict1.replace(Term[index], &.concat(rewrite.list.items))
    in Rewrite::None
      next
    end

    changed = true
    limit -= 1 if limit
  end

  changed ? Rewrite.one(dict1) : Rewrite.none
end

# FIXME: crazy crazy shitcode
# :nodoc:
#
# More efficient `itemsr` implementation for `Changes::Accept` procs.
def itemsr(changes : Changes::Accept, term : Term, successor, *, limit) : Rewrite::Any?
  return Rewrite.none unless dict0 = term.as_d?

  splices = nil
  changed = false

  dict1 = dict0.transaction do |commit|
    dict0.each_item_with_index do |item, index|
      break if limit && limit.zero?

      # Ask the successor for rewrite, and incorporate it into the dict.
      case rewrite = successor.call(changes, Rewrite.one(item))
      in Rewrite::One
        commit.with(index, rewrite.term)
      in Rewrite::Many
        # Rewrite::Many's are rare.
        splices ||= [] of {Int32, Term::Dict}
        splices << {index, rewrite.list}
      in Rewrite::None
        next
      end

      changed = true
      limit -= 1 if limit
    end
  end

  unless splices
    return changed ? Rewrite.one(dict1) : Rewrite.none
  end

  splices.unstable_sort_by! { |index, _| -index } 
  splices.each do |index, list|
    dict1 = dict1.replace(Term[index], &.concat(list.items))
  end

  Rewrite.one(dict1)
end

# Makes up to *limit* rewrites of items from the itemspart of a dictionary
# using *successor*.
def itemsr(successor, *, limit = nil)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce do |term|
      itemsr(changes, term, successor, limit: limit)
    end
  end
end

# FIXME: crazy crazy shitcode
# :nodoc:
#
# Less efficient `pairsr` implementation for `Changes::Preview` procs.
def pairsr(changes : Changes::Preview, term : Term, successor, *, limit) : Rewrite::Any?
  return Rewrite.none unless dict0 = term.as_d?

  dict1 = dict0
  changed = false

  dict0.pairs.each_entry do |key, value|
    break if limit && limit.zero?

    # Generate a preview proc that will fill the dict with the successor's
    # (potential) rewrite.
    subchanges = Changes::Preview.new do |original, rewrite|
      case rewrite
      in Rewrite::One
        filled = dict1.with(key, rewrite.term)
      in Rewrite::Many
        filled = dict1.with(key, rewrite.list)
      end

      changes.call(Term.of(dict1.with(key, original)), Rewrite.one(filled))
    end
    
    # Ask the successor for rewrite, and incorporate it into the dict.
    case rewrite = successor.call(subchanges, Rewrite.one(value))
    in Rewrite::One
      dict1 = dict1.with(key, rewrite.term)
    in Rewrite::Many
      dict1 = dict1.with(key, rewrite.list)
    in Rewrite::None
      next
    end

    changed = true
    limit -= 1 if limit
  end

  changed ? Rewrite.one(dict1) : Rewrite.none
end

# FIXME: crazy crazy shitcode
# :nodoc:
#
# More efficient `pairsr` implementation for `Changes::Accept` procs.
def pairsr(changes : Changes::Accept, term : Term, successor, *, limit) : Rewrite::Any?
  return Rewrite.none unless dict0 = term.as_d?

  changed = false

  dict1 = dict0.transaction do |commit|
    dict0.pairs.each_entry do |key, value|
      break if limit && limit.zero?

      # Ask the successor for rewrite, and incorporate it into the dict.
      case rewrite = successor.call(changes, Rewrite.one(value))
      in Rewrite::One
        commit.with(key, rewrite.term)
      in Rewrite::Many
        commit.with(key, rewrite.list)
      in Rewrite::None
        next
      end

      changed = true
      limit -= 1 if limit
    end
  end

  changed ? Rewrite.one(dict1) : Rewrite.none
end

# Makes up to *limit* rewrites of pair values from the pairspart of a dictionary
# using *successor*.
def pairsr(successor, *, limit = nil)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce do |term|
      pairsr(changes, term, successor, limit: limit)
    end
  end
end

# Rewrites one item of a dictionary.
#
# Shorthand for `itemsr(successor, limit: 1)`
def itemr(successor)
  itemsr(successor, limit: 1)
end

# Rewrites one pair value of a dictionary.
#
# Shorthand for `pairsr(successor, limit: 1)`
def pairr(successor)
  pairsr(successor, limit: 1)
end

# Rewrites one entry (an item or a pair value) of a dictionary.
def entryr(successor)
  choicer(itemr(successor), pairr(successor))
end

# A rewriter that does nothing.
def nor
  ->(changes : Changes::Any, operand : Rewrite::Any) { Rewrite.none }
end

# :nodoc:
def callr(changes : Changes::Any, term : Term, callable)
  rewrite = callable.call(term).diff(term)

  if rewrite.is_a?(Rewrite::Some)
    case changes
    in Changes::Preview
      changes.call(term, rewrite)
    in Changes::Accept
      changes.call
    end
  end

  rewrite
end

# Rewrites a term using *callable*.
#
# *callable* must respond to `call(term : Term) : Rewrite::Any`
def callr(callable)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce { |term| callr(changes, term, callable) }
  end
end

# :nodoc:
def chainr(changes : Changes::Any, term : Term, a, b)
  lhs = a.call(changes, Rewrite.one(term))
  rhs = b.call(changes, lhs.as?(Rewrite::Some) || Rewrite.one(term))

  {lhs, rhs}.rightmost?(Rewrite::Some) || Rewrite.none
end

# Rewrites a term first using *a*, then the result of that using *b*, etc.
def chainr(a, b)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce { |term| chainr(changes, term, a, b) }
  end
end

# :ditto:
def chainr(a, b, *cs)
  chainr(chainr(a, b), *cs)
end

# :nodoc:
def choicer(changes : Changes::Any, term : Term, a, b)
  lhs = a.call(changes, Rewrite.one(term))
  lhs.as?(Rewrite::Some) || b.call(changes, Rewrite.one(term))
end

# Picks the first successful rewriter out of *a*, *b*, etc.
def choicer(a, b)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce { |term| choicer(changes, term, a, b) }
  end
end

# :ditto:
def choicer(a, b, *cs)
  choicer(choicer(a, b), *cs)
end

# Rewrites the items and pair values of a dictionary term using *successor*.
def entriesr(successor)
  chainr(itemsr(successor), pairsr(successor))
end

# Rewrites a term using successor; if that produces no change recurses on
# the items and pair values of a dictionary term. Items are visited in reverse.
def dfsr(successor)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    choicer(successor, entriesr(dfsr(successor))).call(changes, operand)
  end
end

# :nodoc:
def selr(changes : Changes::Any, term : Term, selector, successor)
  if env = Term::M1::Operator.match?(Term[], selector, term)
    if rewritee = env[:rewritee]?
      return successor.call(changes, Rewrite.one(rewritee))
    end
  end

  Rewrite.none
end

# :nodoc:
def selr(selector : Term::M1::Operator::Any, successor)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce { |term| selr(changes, term, selector, successor) }
  end
end

# Selective rewriter.
#
# *selector* pattern is used to match a term, and if a match is found, the capture
# `rewritee` is passed to the *successor* rewriter.
def selr(selector : Term, successor)
  selr(Term::M1.operator(selector), successor)
end

# :ditto:
def selr(selector : String, successor)
  selr(ML.parse1(selector), successor)
end

# :nodoc:
def exhr(changes : Changes::Any, term : Term, successor)
  memo = Rewrite.one(term)
  changed = false

  while true
    case rewrite = successor.call(changes, memo)
    in Rewrite::Some
      memo = rewrite
      changed = true
    in Rewrite::None
      return changed ? memo : Rewrite.none
    end
  end
end

# Exhaustive rewriter. Performs exhaustive rewriting of a term using *successor*.
#
# *Exhaustive rewriting* is rewriting that stops only when there are no more rewrites
# to do. The resulting term is therefore an exhaustively rewritten term.
def exhr(successor)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce { |term| exhr(changes, term, successor) }
  end
end

# Absolute rewriter. Performs absolute rewriting of a term using *successor*.
#
# *Absolute rewriting* is similar to depth-first search rewriting `dfsr`;
# the notable difference is that `absr` rewrites just one entry at any depth
# (the first one it can reach); whereas `dfsr` rewrites all entries at any
# depth, minus those that are themselves results of previous rewriting.
#
# Absolute rewriting is useful when we want to prioritize context in rules.
# That is, rules with most context must be tried first. With `absr`, any
# rewrite can subsequently be assessed in context. 
#
# `absr` is an intrinsically inefficient way to rewrite, especially for very
# deep terms (since for any smallest change `absr` will backjump to the root).
# It is often used in combination with `relr`, which allows to set the "ceiling"
# for `absr` to jump to based on some floor pattern.
def absr(successor)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    choicer(successor, entryr(absr(successor))).call(changes, operand)
  end
end

module Relr
  record Ready, rewrite : Rewrite::Any
  record Ascend, ascent : Int32
  record None
end

# FIXME: crazy crazy shitcode
def relr0(changes : Changes::Preview, floor, term, ascent, successor)
  if Term::M1::Operator.probe?(Term[], floor, term)
    return Relr::Ascend.new(ascent)
  end

  unless dict0 = term.as_d?
    return Relr::None.new
  end

  dict1 = dict0
  changed = false

  dict0.each_entry do |key, value|
    subchanges = Changes::Preview.new do |original, rewrite|
      case rewrite
      in Rewrite::One
        filled = dict1.with(key, rewrite.term)
      in Rewrite::Many
        if dict1.index?(key)
          filled = dict1.replace(key.as_n, &.concat(rewrite.list.items))
        else
          filled = dict1.with(key, rewrite.list) 
        end
      end

      changes.call(Term.of(dict1.with(key, original)), Rewrite.one(filled))
    end

    case res = relr0(subchanges, floor, value, ascent, successor)
    in Relr::None
      next
    in Relr::Ascend
      unless res.ascent.zero?
        return res.copy_with(ascent: res.ascent - 1)
      end
      rewrite = successor.call(subchanges, Rewrite.one(value))
    in Relr::Ready
      rewrite = res.rewrite
    end
  
    case rewrite
    in Rewrite::None
    in Rewrite::One
      dict1 = dict1.with(key, rewrite.term)
      changed = true
    in Rewrite::Many
      if index = dict0.index?(key)
        dict1 = dict1.replace(key.as_n, &.concat(rewrite.list.items))
      else
        dict1 = dict1.with(key, rewrite.list)
      end
      changed = true
    end
  end

  Relr::Ready.new(changed ? Rewrite.one(dict1) : Rewrite.none)
end

# FIXME: crazy crazy shitcode
def relr0(changes : Changes::Accept, floor, term, ascent, successor)
  if Term::M1::Operator.probe?(Term[], floor, term)
    return Relr::Ascend.new(ascent)
  end

  unless dict0 = term.as_d?
    return Relr::None.new
  end

  splices = nil
  
  dict1 = dict0
  changed = false

  dict0.each_entry do |key, value|
    case res = relr0(changes, floor, value, ascent, successor)
    in Relr::None
      next
    in Relr::Ascend
      unless res.ascent.zero?
        return res.copy_with(ascent: res.ascent - 1)
      end
      rewrite = successor.call(changes, Rewrite.one(value))
    in Relr::Ready
      rewrite = res.rewrite
    end
  
    case rewrite
    in Rewrite::None
    in Rewrite::One
      dict1 = dict1.with(key, rewrite.term)
      changed = true
    in Rewrite::Many
      if index = dict0.index?(key)
        splices ||= [] of {Term::Num, Term::Dict}
        splices << {index, rewrite.list}
      else
        dict1 = dict1.with(key, rewrite.list)
      end
      changed = true
    end
  end
  
  if splices
    splices.unstable_sort_by! { |index, _| -index }
    splices.each do |index, list|
      dict1 = dict1.replace(index...index + 1, &.concat(list.items))
    end
  end
  
  Relr::Ready.new(changed ? Rewrite.one(dict1) : Rewrite.none)
end

def relr(floor : Term::M1::Operator::Any, successor, *, ascent : Int32 = 0)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce do |term|
      case response = relr0(changes, floor, term, ascent, successor)
      in Relr::None   then Rewrite.none
      in Relr::Ready  then response.rewrite
      in Relr::Ascend then successor.call(changes, Rewrite.one(term))
      end
    end
  end
end

def relr(floor : Term, successor, **kwargs)
  relr(Term::M1.operator(floor), successor, **kwargs)
end

def relr(floor : String, successor, **kwargs)
  relr(ML.parse1(floor), successor, **kwargs)
end

# :nodoc:
def pbr(changes, term, pset, a, b)
  case pset.response(term)
  in Pr::Pos then a.call(changes, Rewrite.one(term))
  in Pr::Neg then b.call(changes, Rewrite.one(term))
  end
end

# Rewrites using *a* any term to which pattern set *pset* responds positively.
# Rewrites using *b* any other term.
def pbr(pset : PatternSet, a, b)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce { |term| pbr(changes, term, pset, a, b) }
  end
end

# TODO: support Changes::Preview
struct RewriteApplier(T)
  def initialize(@changes : Changes::Accept, @rewriter : T)
  end

  def call(up0, up1, down, my, matchee0, body)
    app = Term::M1::DefaultApplier.new
    up1, matchee1 = app.call(up0, up1, down, my, matchee0, body)

    case rewrite = @rewriter.call(@changes, Rewrite.one(matchee1))
    in Rewrite::None then {up1, matchee1}
    in Rewrite::One  then {up1, rewrite.term}
    in Rewrite::Many
      # TODO: we'd probably want to convert the backspec entry into a () entry
      # somehow if it's not one already
      {up1, Term.of(rewrite.list)}
    end
  end
end

def ruler(changes, term, ruleset, ruler, backmapr, elser)
  unless row = ruleset.call(term)
    return elser.call(changes, Rewrite.one(term))
  end

  pr, rule = row

  case rule
  in Rule::Template
    raise "template not implemented"
  in Rule::BackmapOne
    # Keeping track of keypaths is expensive, so we do pattern matching without
    # keypaths; when we're sure we need keypaths we re-match with keypaths: true.
    unless pr.envs.all? &.includes?(:"(keypaths)")
      pr = pr.pattern.response(term, keypaths: true).as(Pr::Pos)
    end

    case changes
    in Changes::Accept
      subchanges = changes
    in Changes::Preview
      # TODO: support Changes::Preview
      subchanges = ->{ changes.call(term, Rewrite.one(term)) }
    end

    result = Term::M1.backmap(pr.envs, rule.backspec, term, applier: RewriteApplier.new(subchanges, backmapr))
    result == term ? Rewrite.none : Rewrite.one(result)
  in Rule::BackmapMany
    unless pr.envs.all? &.includes?(:"(keypaths)")
      pr = pr.pattern.response(term, keypaths: true).as(Pr::Pos)
    end

    case changes
    in Changes::Accept
      subchanges = changes
    in Changes::Preview
      # TODO: support Changes::Preview
      subchanges = ->{ changes.call(term, Rewrite.one(term)) }
    end

    case pr
    in Pr::One
      result = Term::M1.backmap(pr.envs, rule.backspec, term, applier: RewriteApplier.new(subchanges, backmapr))

      if rule.backspec.includes?({rule.toplevel}) && (list = result.as_d?)
        list == Term[{term}] ? Rewrite.none : Rewrite.many(list)
      else
        result == term ? Rewrite.none : Rewrite.one(result)
      end
    in Pr::Many
      list = Term::Dict.build do |commit|
        pr.ones do |one|
          commit << Term::M1.backmap(one.envs, rule.backspec, term, applier: RewriteApplier.new(subchanges, backmapr))
        end
      end

      list == Term[{term}] ? Rewrite.none : Rewrite::Many.new(list)
    end
  end
end

def ruler(ruleset, ruler, backmapr, elser)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce { |term| ruler(changes, term, ruleset, ruler, backmapr, elser) }
  end
end

term = Term.of(1, {3, {:"$", {:ml, "100"}}, 4}, { {:"$", {:+, {:+, {:"$", 1}, 2}, 3}} }, { {:"$", {:string, 100}} }, x: {:+, 4, 5}, y: 200)

successor = ->(term : Term) do
  pp term
  Rewrite.none
end

preview = Changes::Preview.new do |before, after|
  puts "REWRITE ----"
  puts ML.display(before)
  puts "----------->"
  after.each do |term|
    puts ML.display(term)
  end
end
accept = Changes::Accept.new { }

# pp itemsr(successor).call(preview, Rewrite.one(term))
# pp itemsr(successor).call(accept, Rewrite.one(term))
# pp pairsr(successor).call(preview, Rewrite.one(term))
# pp pairsr(successor).call(accept, Rewrite.one(term))

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

CURSORP = ML.parse1(%([_string (%any° | (| _string)) _string (_*) @_]))
CURSORPE = Term::M1.operator(ML.parse1(%([_string (%any° | (| _string)) _string (_*) @edge_])))

# TODO: Currently reads&writes very obscurely. We'd want to be able to define these
# more, how shall I say... "elegantly". These look like grammars or some kind
# of reverse grammars to me. So maybe looking into the grammar description languages
# out there, we can take some inspiration. In my mind they form a kind of bottom-up
# graph or more precisely, tree. But trees are unreadable as S-expressions, what you
# see below is basically a tree, and it'd look almost the same in Sexps (and similarly unreadable).
def editr
  dollarr = exhr(dfsr(callr(NATRS)))
  backmapr = dfsr(
    choicer(
      selr(%[($ rewritee_)], dollarr),
      selr(%[($once rewritee_)], callr(NATRS)),
    ),
  )
  exhr(relr(CURSORP, absr(ruler(RULESET, nor, backmapr, nor)), ascent: 2))
end

EDITR = editr

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

def edit(root : Term, motion : Term, edge = Term.of(:edge, :user)) : Term
  root = subsume(root, motion, edge)
  rewrite = EDITR.call(-> {}, Rewrite.one(root))

  case rewrite
  in Rewrite::None then root
  in Rewrite::One  then rewrite.term
  in Rewrite::Many then Term.of(rewrite.list)
  end
end

# pp(edit(ML.parse1(<<-WWML
# ((button "+ Increment" @actions) (button "- Decrement" @actions) ("" | "" {} @user))
# WWML
# ), Term.of(:key, :"C-delete")))

# pp relr(%[($ _)], exhr(absr(selr(%[($ rewritee_)], callr(successor)))), ascent: 3).call(preview, Rewrite.one(term))
# pp relr(%[($ _)], callr(successor), ascent: 1).call(preview, Rewrite.one(term))

# pp exhr(absr(selr(%[($ rewritee_)], exhr(dfsr(callr(successor)))))).call(preview, Rewrite.one(term))
# pp itemsr(callr(successor)).call(preview, Rewrite.one(term))

# [x] itemsr
# [x] pairsr
# [x] chainr
# [x] choicer
# [x] entriesr
# [x] dfsr
# [x] callr
# [x] selr
# [x] exhr
# [x] nor
# [x] itemr -> itemsr(limit: 1)
# [x] pairr -> pairsr(limit: 1)
# [x] entryr -> choicer(itemr, pairr)
# [x] absr
# [x] relr
