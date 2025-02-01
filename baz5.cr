require "./wirewright"
require "./baz5_common"

alias Rewriter = RewriterContext, Rewrite::Any -> Rewrite::Any
alias Observer = Keypath::Appender, String, Rewrite::Some ->

record RewriterContext, rng : Random, keypath : Keypath::Appender?, envs = Term[], observer : Observer = (Observer.new {}), exhr = {} of {UInt64, Term} => Rewrite::Any do
  # If available, returns memoized exhaustive rewrite of *term* for an exhR rewriter
  # with the given *id*.
  #
  # Otherwise, yields and adds the resulting *rewrite* as the memoized exhaustive
  # rewrite of *term* for *id*. The addition is available within all versions of
  # this context (mutates the context).
  def exhr(id : UInt64, term : Term, & : -> Rewrite::Any) : Rewrite::Any
    @exhr.put_if_absent({id, term}) { yield }
  end

  def keypath(& : Keypath::Appender -> Keypath::Appender)
    return self unless kp0 = @keypath

    copy_with(keypath: yield kp0)
  end

  # Passthrough that notifies the observer (if any) of a *leaf rewrite*: rewrite that
  # does not have a successor.
  #
  # "The observer" here means some kind of function that reacts to rewrites at keypaths.
  # See `observer`.
  def observable(leaf : Rewrite::Any, &explanation : -> String) : Rewrite::Any
    if leaf.is_a?(Rewrite::Some)
      @keypath.try { |keypath| @observer.call(keypath, yield, leaf) }
    end

    leaf
  end
end

def noR : Rewriter
  Rewriter.new { Rewrite.none }
end

# A rewriter that unconditionally replaces anything with one *term*.
def oneR(term term1 : Term) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce do |term0|
      # NOTE: since this is a Rewrite introduction point we must fortify it with
      # a diff(). Otherwise we risk rewriters to short-circuit if they cannot
      # determine when to stop due to a sneaky One(T) -> Many([T]) or something
      # like that.
      ctx.observable(Rewrite.one(term1).diff(term0)) { "unconditional replace with one" }
    end
  end
end

# A rewriter that unconditionally replaces anything with many terms specified
# in *list*.
def manyR(list : Term::Dict) : Rewriter
  Rewriter.new do |ctx, staging|
    # NOTE: just as above, we're defensive here.
    staging.reduce do |term0|
      ctx.observable(Rewrite.many(list).diff(term0)) { "unconditional replace with many" }
    end
  end
end

def envR(ctx : RewriterContext, prefix : Enumerable(Term), term term0 : Term)
  ctx.envs.items.reverse_each do |env|
    next unless pterm = env.follow?(prefix)
    next unless pdict = pterm.as_d?
    next unless term1 = pdict[term0]?

    return ctx.observable(Rewrite.one(term1).diff(term0)) { "replace with value from the environment" }
  end

  Rewrite.none
end

def envR(prefix : Enumerable(Term)) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| envR(ctx, prefix, term) }
  end
end

def envR(*prefix : Term) : Rewriter
  envR(prefix)
end

EMPTY_PREFIX = [] of Term

def envR : Rewriter
  envR(EMPTY_PREFIX)
end

# TODO: we should be able to implement this more efficiently in the future!
def splice(dict : Term::Dict, splices : Array({Term::Num, Term::Dict}))
  splices.each do |start, splice|
    dict = dict.replace(start, &.concat(splice.items))
  end

  dict
end

# :nodoc:
def itemsR(ctx0 : RewriterContext, term : Term, successor : Rewriter) : Rewrite::Any
  unless dict0 = term.as_d?
    return Rewrite.none
  end

  splices = nil

  dict1 = dict0.transaction do |commit|
    # NOTE: we **must** call successor in proper order due to observers which are only
    # capable of doing one keypath-insert at a time.
    (0...dict0.itemsize).reverse_each do |index|
      item = dict0[index]

      ctx1 = ctx0.keypath &.update_value(index)

      case rewrite = successor.call(ctx1, Rewrite.one(item))
      in Rewrite::None
      in Rewrite::One
        commit.with(index, rewrite.term)
      in Rewrite::Many
        splices ||= [] of {Term::Num, Term::Dict}
        splices << {Term[index], rewrite.list}
      end
    end
  end

  unless splices
    # NOTE: Dict transactions only modify the underlying dict upon the first with()/
    # without()/etc. call. Since we only call with() on a Rewrite.one of the successor,
    # and since we trust the successor in that its Rewrite.one signals change unconditionally,
    # we thus consider a `same?` check sufficient to determine whether the dictionary
    # was modified.
    return dict0.same?(dict1) ? Rewrite.none : Rewrite.one(dict1)
  end

  dict1 = splice(dict1, splices)

  Rewrite.one(dict1)
end

# Rewrites the itemspart of a dictionary using *successor*.
def itemsR(successor : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| itemsR(ctx, term, successor) }
  end
end

# :nodoc:
def pairsR(ctx0 : RewriterContext, term : Term, successor : Rewriter) : Rewrite::Any
  unless dict0 = term.as_d?
    return Rewrite.none
  end

  dict1 = dict0.transaction do |commit|
    dict0.pairspart.each_entry do |key, value0|
      ctx1 = ctx0.keypath &.update_value(key)

      case rewrite = successor.call(ctx1, Rewrite.one(value0))
      in Rewrite::None
      in Rewrite::One  then commit.with(key, rewrite.term)
      in Rewrite::Many then commit.with(key, rewrite.list)
      end
    end
  end

  # See the NOTE in itemsR explaining why `same?` is sufficient here.
  dict0.same?(dict1) ? Rewrite.none : Rewrite.one(dict1)
end

# Rewrites the pairspart of a dictionary using *successor*.
def pairsR(successor : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| pairsR(ctx, term, successor) }
  end
end

# :nodoc:
def callR(ctx : RewriterContext, term : Term, callable)
  # We do not trust *callable*, and perform an explicit diff to make sure the
  # Rewrite *callable* returns reflects whether or not there actually was
  # a rewrite.
  rewrite = callable.call(term).diff(term)

  ctx.observable(rewrite) { "rewrite" }
end

# Rewrites a term using *callable*.
#
# *callable* must respond to `call(term : Term) : Rewrite::Any`
def callR(callable) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| callR(ctx, term, callable) }
  end
end

# See `callR(callable)`.
def callR(&callable : Term -> Rewrite::Any) : Rewriter
  callR(callable)
end

# :nodoc:
def chainR(ctx : RewriterContext, term : Term, a : Rewriter, b : Rewriter) : Rewrite::Any
  lhs = a.call(ctx, Rewrite.one(term))
  rhs = b.call(ctx, lhs.as?(Rewrite::Some) || Rewrite.one(term))

  {lhs, rhs}.rightmost?(Rewrite::Some) || Rewrite.none
end

# Rewrites a term first using *a*, then the result of that using *b*, and so on.
def chainR(a : Rewriter, b : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| chainR(ctx, term, a, b) }
  end
end

# :ditto:
def chainR(a : Rewriter, b : Rewriter, *cs) : Rewriter
  chainR(chainR(a, b), *cs)
end

# :nodoc:
def choiceR(ctx : RewriterContext, term : Term, a : Rewriter, b : Rewriter) : Rewrite::Any
  lhs = a.call(ctx, Rewrite.one(term))
  lhs.as?(Rewrite::Some) || b.call(ctx, Rewrite.one(term))
end

# Picks the leftmost successful rewriter among *a*, *b*, etc.
def choiceR(a : Rewriter, b : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| choiceR(ctx, term, a, b) }
  end
end

# :ditto:
def choiceR(a : Rewriter, b : Rewriter, *cs) : Rewriter
  choiceR(choiceR(a, b), *cs)
end

# Rewrites entries of a dictionary (items and pairs) using *successor*.
def entriesR(successor : Rewriter) : Rewriter
  chainR(itemsR(successor), pairsR(successor))
end

# :nodoc:
def entryR1(ctx0 : RewriterContext, dict : Term::Dict, key : Term, value : Term, successor : Rewriter)
  ctx1 = ctx0.keypath &.update_value(key)

  case rewrite = successor.call(ctx1, Rewrite.one(value))
  in Rewrite::None
    Rewrite.none
  in Rewrite::One
    Rewrite.one(dict.with(key, rewrite.term))
  in Rewrite::Many
    if index = dict.index?(key)
      Rewrite.one(dict.replace(index, &.concat(rewrite.list.items)))
    else
      Rewrite.one(dict.with(key, rewrite.list))
    end
  end
end

# :nodoc:
def entryR(ctx : RewriterContext, term : Term, successor : Rewriter) : Rewrite::Any
  unless dict = term.as_d?
    return Rewrite.none
  end

  case dict.size
  when 0 # Empty dict, nothing to randomize
    Rewrite.none
  when 1 # One entry, nothing to randomize
    key, value = dict.nth(0)

    entryR1(ctx, dict, key, value, successor)
  when 2 # Two elements, randomize A,B s. B,A
    indices = { {0, 1}, {1, 0} }[(0..1).sample(ctx.rng)]

    indices.each do |index|
      key, value = dict.nth(index)

      case rewrite = entryR1(ctx, dict, key, value, successor)
      in Rewrite::None
      in Rewrite::Some then return rewrite
      end
    end

    # No rewritable entries
    Rewrite.none
  else # More than two elements, visit in disorder
    n = dict.size.to_u32
    state, prime = Disorder.state(n, ctx.rng)

    n.times do
      state = index = Disorder.next(n, state, prime)
      key, value = dict.nth(index.to_i)

      case rewrite = entryR1(ctx, dict, key, value, successor)
      in Rewrite::None
      in Rewrite::Some then return rewrite
      end
    end

    # No rewritable entries
    Rewrite.none
  end
end

# Rewrites one random entry of a dictionary using *successor*.
def entryR(successor : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| entryR(ctx, term, successor) }
  end
end

{% for row in { {:item, :pair}, {:pair, :item} } %}
  {% lpart, rpart = row %}

  # Rewrites one random {{lpart.id}} of a dictionary using *successor*.
  def {{lpart.id}}R(successor : Rewriter) : Rewriter
    slave = entryR(successor)

    Rewriter.new do |ctx, staging|
      staging.reduce do |term|
        unless dict = term.as_d?
          next Rewrite.none
        end

        case rewrite = slave.call(ctx, Rewrite.one(dict.{{lpart.id}}spart))
        in Rewrite::None
          Rewrite.none
        in Rewrite::One
          # NOTE: entryR always returns a dictionary, but we're a little too loose
          # on the types so we have to error-cast. Whatever.
          Rewrite.one(rewrite.term.as_d | dict.{{rpart.id}}spart)
        in Rewrite::Many
          raise "BUG: entryR returned Rewrite::Many"
        end
      end
    end
  end
{% end %}

# Allows you to set up a rewriter that can reference itself. Returns a pair
# of procs: the first one is for setting the rewriter that will be used for
# recursion, and the second one represents the recursive rewriter itself.
#
# Here is an example dfsR definition:
#
# ```
# # successor = <successor of dfsR>
#
# set, rec = recR
# set.call choiceR(successor, entriesR(rec))
#
# # You can use `rec` or the rewriter returned by `set` now. They're not identical
# # (`rec` introduces one level of indirection); but their result will be the same.
# ```
def recR : {(Rewriter -> Rewriter), Rewriter}
  slot = nil

  set = ->(rewriter : Rewriter) { slot = rewriter }
  rec = Rewriter.new do |ctx, staging|
    slot.try(&.call(ctx, staging)).default(Rewrite.none)
  end

  {set, rec}
end

# :nodoc:
def selR(ctx : RewriterContext, term : Term, selector, successor : Rewriter) : Rewrite::Any
  if env = M1::Operator.match?(Term[], selector, term)
    if rewritee = env[:rewritee]?
      return successor.call(ctx, Rewrite.one(rewritee))
    end
  end

  Rewrite.none
end

# :nodoc:
def selR(selector : M1::Operator::Any, successor : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| selR(ctx, term, selector, successor) }
  end
end

# Selective rewriter.
#
# *selector* pattern is used to match a term, and if a match is found, the capture
# `rewritee` is passed to the *successor* rewriter.
def selR(selector : Term, successor : Rewriter) : Rewriter
  selR(M1.operator(selector), successor)
end

# See the main overload (`Term`) for more info.
def selR(selector : String, successor : Rewriter) : Rewriter
  selR(ML.parse1(selector), successor)
end

def switchR(branches : Enumerable({Term, Rewriter})) : Rewriter
  choice = nil

  branches.each do |selector, successor|
    rewriter = selR(selector, successor)
    choice = choice ? choiceR(choice, rewriter) : rewriter
  end

  choice || noR
end

def switchR(*branches : {Term, Rewriter})
  switchR(branches)
end

def switchR(*branches : {String, Rewriter})
  switchR(branches.map { |selector, rewriter| {ML.parse1(selector), rewriter} })
end


# Rewrites a term using *successor*; if that produces no change and the term is
# a dictionary term, recurses on its items and pair values (`entriesR`).
def dfsR(successor : Rewriter) : Rewriter
  set, rec = recR
  set.call choiceR(successor, entriesR(rec))
end

# :nodoc:
def exhR(ctx : RewriterContext, term : Term, successor : Rewriter) : Rewrite::Any
  state = Rewrite.one(term)
  changed = false

  while true
    case rewrite = successor.call(ctx, state)
    in Rewrite::Some
      state = rewrite
      changed = true
    in Rewrite::None
      return changed ? state : Rewrite.none
    end
  end
end

# :nodoc:
def exhR(ctx : RewriterContext, id : UInt64, term : Term, successor : Rewriter) : Rewrite::Any
  ctx.exhr(id, term) { exhR(ctx, term, successor) }
end

# Absolute rewriter. Performs absolute rewriting of a term using *successor*.
#
# *Absolute rewriting* is similar to depth-first search rewriting `dfsR`.
#
# One difference is that `absR` rewrites just one entry and backjumps to the root;
# whereas `dfsR` rewrites all entries at any depth, minus those it already visited.
#
# Another difference is that `absR` performs *randomized* or *disordered rewriting*.
# `absR` tries to rewrite the entries it comes upon in random order, and the first
# successful rewrite causes it to backjump to the root. Therefore, "observing this
# from the outside", we see that `absR` finds a random entry at a random depth
# to rewrite.
#
# Absolute rewriting is useful when we want to prioritize context in rules.
# That is, rules with most context must be tried first. With `absR`, any
# rewrite will subsequently and inevitably be assessed in context.
#
# `absR` is an intrinsically inefficient way to rewrite, especially for very
# deep terms (since for any smallest change `absR` will backjump to the root).
# But it is most thorough and most context-aware. If an opportunity for rule
# application exists, it is guaranteed to be taken, regardless of rewrite depth
# and so on.
#
# Obviously, the above starts to shake whenever there are multiple cross-dependent
# opportunities for rewriting. `absR` can choose at random to take one of those
# opportunities, thus disrupting the second one and so on.
#
# Where possible, we recommend using `absR` in combination with `relR`, which
# allows to set a "ceiling"  for `absR` to backjump to based on some pattern
# for the "bottom" and a numeric *ascent* -- the number of depth levels to climb.
def absR(successor) : Rewriter
  set, rec = recR
  set.call choiceR(successor, entryR(rec))
end

module ExhrId
  @@fresh = Atomic(UInt64).new(0u64)

  # Returns a fresh, application-unique exhR id.
  def self.fresh : UInt64
    @@fresh.add(1, :relaxed)
  end
end

# Exhaustive rewriter. Performs exhaustive rewriting of a term using *successor*.
#
# *Exhaustive rewriting* is rewriting that stops only when there are no more rewrites
# to do. The resulting term is an *exhaustively rewritten term*. If *memoize* is set
# to `true` (it is by default), this exhaustively rewritten term is stored so that
# further rewrites with the same exhR instance are a noop.
def exhR(successor : Rewriter) : Rewriter
  id = ExhrId.fresh

  Rewriter.new do |ctx, staging|
    staging.reduce { |term| exhR(ctx, id, term, successor) }
  end
end

# :nodoc:
module Relr
  record Ready, rewrite : Rewrite::Any
  record Ascend, ascent : Int32
  record None

  # NOTE: just like in `itemsR`, here we must iterate in a keypath-friendly
  # way so that observers that rely solely on {keypath, rewrite} pairs can
  # reconstruct what we're doing here without messing up indices etc.
  private def self.each_entry_keypath_friendly(dict : Term::Dict, &)
    (0...dict.itemsize).reverse_each do |index|
      yield Term.of(index), dict[index]
    end

    dict.pairspart.each_entry do |key, value|
      yield key, value
    end
  end

  def self.relr(ctx0, bottom, term, ascent, successor)
    if M1::Operator.probe?(Term[], bottom, term)
      return Ascend.new(ascent)
    end

    unless dict0 = term.as_d?
      return None.new
    end

    splices = nil

    dict1 = dict0.transaction do |commit|
      each_entry_keypath_friendly(dict0) do |key, value|
        ctx1 = ctx0.keypath &.update_value(key)

        case response = relr(ctx1, bottom, value, ascent, successor)
        in None
          next
        in Ascend
          unless response.ascent.zero?
            return response.copy_with(ascent: response.ascent - 1)
          end
          rewrite = successor.call(ctx1, Rewrite.one(value))
        in Ready
          rewrite = response.rewrite
        end

        case rewrite
        in Rewrite::None
        in Rewrite::One
          commit.with(key, rewrite.term)
        in Rewrite::Many
          if index = dict0.index?(key)
            splices ||= [] of {Term::Num, Term::Dict}
            splices << {index, rewrite.list}
          else
            commit.with(key, rewrite.list)
          end
        end
      end
    end

    unless splices
      # NOTE: See `itemsR` to learn why this `same?` check is sufficient here.
      return Ready.new(dict0.same?(dict1) ? Rewrite.none : Rewrite.one(dict1))
    end

    dict1 = splice(dict0, splices)

    Ready.new(Rewrite.one(dict1))
  end
end

# Relative rewriter. Performs relative rewriting of a term using *successor*,
# with a *bottom* pattern that determines a stopping point and an *ascent* that
# controls backjumping.
#
# *Relative rewriting* is rewriting that is *anchored* to a specific subterm
# (the *bottom* match) rather than operating globally. When `relR` finds the
# *bottom* pattern, it begins ascending. Once it has climbed *ascent* levels,
# it rewrites the term thus reached using *successor*.
#
# - If `relR` encounters the bottom pattern, it begins its ascent.
# - Once it ascends *ascent* levels, it applies `successor` at that point.
# - If *ascent* is `0`, `relR` rewrites the bottom directly.
# - If multiple *bottom* matches exist, `relR` processes them independently,
#   meaning rewriting order can influence results.
#
# `relR` is useful for localizing rewrites to a structured region of the term
# *based on the content of that term*, allowing precise control over where rewriting
# occurs without hard-coded keypaths etc.
#
# `relR` is often used in combination with `absR` to improve efficiency by
# reducing redundant backjumping - `relR` sets the "ceiling" for a successor
# `absR` to backjump to.
#
# NOTE: partially written by ChatGPT because I'm terrible at explaining things.
def relR(bottom : M1::Operator::Any, successor : Rewriter, *, ascent : Int32 = 0) : Rewriter
  if ascent.negative?
    raise ArgumentError.new("ascent must be positive or 0")
  end

  Rewriter.new do |ctx, staging|
    staging.reduce do |term|
      case response = Relr.relr(ctx, bottom, term, ascent, successor)
      in Relr::None   then Rewrite.none
      in Relr::Ready  then response.rewrite
      in Relr::Ascend then successor.call(ctx, Rewrite.one(term))
      end
    end
  end
end

# Check out the main `relR` overload (one for `M1::Operator::Any`) to learn more.
def relR(bottom : Term, successor : Rewriter, **kwargs) : Rewriter
  relR(M1.operator(bottom), successor, **kwargs)
end

# Check out the main `relR` overload (one for `M1::Operator::Any`) to learn more.
def relR(bottom : String, successor : Rewriter, **kwargs) : Rewriter
  relR(ML.parse1(bottom), successor, **kwargs)
end

# :nodoc:
def pbranchR(ctx : RewriterContext, term : Term, pset : PatternSet, a : Rewriter, b : Rewriter) : Rewrite::Any
  case pset.response(term)
  in Pr::Pos then a.call(ctx, Rewrite.one(term))
  in Pr::Neg then b.call(ctx, Rewrite.one(term))
  end
end

# Rewrites using *a* any term to which pattern set *pset* responds positively.
# Rewrites using *b* any other term.
def pbranchR(pset : PatternSet, a : Rewriter, b : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| pbranchR(ctx, term, pset, a, b) }
  end
end

def effectR(successor : Rewriter, callable) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce do |term|
      callable.call(term)
      successor.call(ctx, Rewrite.one(term))
    end
  end
end

def effectR(successor : Rewriter, &effect : Term ->) : Rewriter
  effectR(successor, effect)
end

struct RewriteApplier
  def initialize(@ctx : RewriterContext, @rewriter : Rewriter)
  end

  def apply(up, down, my, body)
    subctx = @ctx.copy_with(keypath: nil, envs: @ctx.envs.append(Term["$up": up, "$down": down, "$my": my]))
    rewrite = @rewriter.call(subctx, Rewrite.one(body))
    rewrite.as?(Rewrite::Some) || Rewrite.one(body)
  end

  # FIXME: Can we let rewriters handle $tr?
  def call(up0, up1, down, my, matchee0, body)
    Term.case(body) do
      matchpi %[($tr pred_ succ_)] do
        {up1.with(pred, matchee0), apply(up0, down, my, succ)}
      end

      otherwise do
        {up1, apply(up0, down, my, body)}
      end
    end
  end
end

# :nodoc:
#
# Rewrites what ruleset identified as rule templates, most often something like
# `(rule (square a_) (* a a))`.
def ruleR(ctx0, term, rule : Rule::Template, pr : Pr::One, templr, backmapr)
  rewrite = Rewrite.one(rule.body).diff(term)

  ctx1 = ctx0.copy_with(envs: ctx0.envs.append(pr.env))

  # NOTE: maybe the observer shows envs for debugging; we don't know it here.
  # So show the rewrite to the latest context.
  ctx1.observable(rewrite) { "replace with rule template body" }

  # Even if templR fails to do something with the body, we still succeeded in
  # replacing it (maybe, if the diff() call doesn't end up with .none).
  templr.call(ctx1, rewrite).as?(Rewrite::Some) || rewrite
end

def ruleR(ctx0, term, rule : Rule::Template, pr : Pr::Many, templr, backmapr)
  raise "not implemented"
end

def ruleR(ctx, term, rule : Rule::BackmapOne, pr : Pr::Pos, templr, backmapr)
  # Keeping track of keypaths is expensive, so we do pattern matching without
  # keypaths; when we're sure we need keypaths we re-match with keypaths: true.
  #
  # TODO: we should probably use .probe? for this in the pattern set.
  unless pr.envs.all? &.includes?(:"(keypaths)")
    pr = pr.pattern.response(term, keypaths: true).as(Pr::Pos)
  end

  rewrite = M1.backmapr(pr.envs, rule.backspec, term, applier: RewriteApplier.new(ctx, backmapr))

  # The backmap rewriter may lie sometimes that there was a change, make sure
  # we're on track with Rewrites.
  ctx.observable(rewrite.diff(term)) { "replace with backmapped (one)" }
end

def ruleR(ctx, term, rule : Rule::BackmapMany, pr : Pr::Pos, templr, backmapr)
  # TODO: we should probably use .probe? for this in the pattern set.
  unless pr.envs.all? &.includes?(:"(keypaths)")
    pr = pr.pattern.response(term, keypaths: true).as(Pr::Pos)
  end

  case pr
  in Pr::One
    rewrite = M1.backmapr(pr.envs, rule.backspec, term, applier: RewriteApplier.new(ctx, backmapr))

    plural = rule.backspec.includes?({rule.toplevel})

    case rewrite
    in Rewrite::One
      if plural && (list = rewrite.term.as_itemsonly_d?)
        rewrite = Rewrite.many(list)
      end
    in Rewrite::Many
      unless plural
        rewrite = Rewrite.one(rewrite.list)
      end
    end
  in Pr::Many
    list = Term::Dict.build do |commit|
      pr.ones do |one|
        commit << M1.backmapr(one.envs, rule.backspec, term, applier: RewriteApplier.new(ctx, backmapr))
      end
    end

    rewrite = Rewrite.many(list)
  end

  ctx.observable(rewrite.diff(term)) { "replace with backmapped (many)" }
end

def rulesetR(ctx, term, ruleset, templr, backmapr, elser)
  needle = ruleset.responses(term).compact_map do |pr, rule|
    rewrite = ruleR(ctx, term, rule, pr, templr, backmapr)
    rewrite.diff(term).as?(Rewrite::Some)
  end

  needle.first? || elser.call(ctx, Rewrite.one(term))
end

def rulesetR(ruleset, ruler, backmapr, elser) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| rulesetR(ctx, term, ruleset, ruler, backmapr, elser) }
  end
end

def preview1(term, cursor : Term::Dict::ItemsView, leaf : Rewrite::Some)
  unless word = cursor.first?
    return leaf
  end

  Term.case(word) do
    matchpi %[(pair key_)] do
      cursor = cursor.move(1) # Read (pair ...)

      raise KeypathError.new unless word = cursor.first?
      raise ArgumentError.new unless word == Term.of(:value)
      raise ArgumentError.new unless dict = term.as_d?

      cursor = cursor.move(1) # Read value

      value0 = dict[key]

      case rewrite = preview1(value0, cursor, leaf)
      in Rewrite::One
        Rewrite.one(dict.with(key, rewrite.term))
      in Rewrite::Many
        if index = dict.index?(key)
          Rewrite.one(dict.replace(index, &.concat(rewrite.list.items)))
        else
          Rewrite.one(dict.with(key, rewrite.list))
        end
      end
    end
  end
end

def preview1(term, keypath : Term::Dict, leaf)
  preview1(term, keypath.items, leaf)
end

def rewrite(term : Term, rewriter : Rewriter) : Term
  rng = Random::PCG32.new(rand(UInt64))
  ctx = RewriterContext.new(rng, keypath: nil)

  rewrite = rewriter.call(ctx, Rewrite.one(term))
  rewrite.term? || term
end

def rewrite(term : Term, rewriter : Rewriter, &observer : Observer) : Term
  rng = Random::PCG32.new(rand(UInt64))
  ctx = RewriterContext.new(rng, keypath: Keypath::Appender.new, observer: observer)

  rewrite = rewriter.call(ctx, Rewrite.one(term))
  rewrite.term? || term
end

# [x] itemsR
# [x] pairsR
# [x] chainR
# [x] choiceR
# [x] entriesR -> choiceR(itemsR, pairsR)
# [x] recR
# [x] dfsR
# [x] callR
# [x] selR
# [x] exhR
# [x] noR
# [x] oneR
# [x] manyR
# [x] itemR (fair random item)
# [x] pairR (fair random pair)
# [x] entryR (fair random entry)
# [x] absR
# [x] relR
# [x] pbranchR
# [x] effectR
# [x] rulesetR
# [ ] rewriteR "rewrite rewriter", detects modifications of successor1, passes to successor2, result is successor2
# [ ] pathR
#    - rewrites a path from root to some offspring,
#    - using passable and impassable psets,
#    - successor rewrites path,
#    - allows to specify max path depth pattern (overall) and path view pattern (how much of path to give to successor)
