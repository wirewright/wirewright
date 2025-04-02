require "./src/wirewright"
require "./baz5_common"

alias Rewriter = RewriterContext, Rewrite::Any -> Rewrite::Any
alias Observer = Backpath::Appender, String, Rewrite::Some ->
alias Tick = ->

record RewriterContext, rng : Random, backpath : Backpath::Appender?, envs = Term[], observer : Observer | Tick = (Tick.new {}), exhr = {} of {UInt64, Term} => Rewrite::Any, options = Term[] do
  # If available, returns memoized exhaustive rewrite of *term* for an exhR rewriter
  # with the given *id*.
  #
  # Otherwise, yields and adds the resulting *rewrite* as the memoized exhaustive
  # rewrite of *term* for *id*. The addition is available within all versions of
  # this context (mutates the context).
  def exhr(id : UInt64, term : Term, & : -> Rewrite::Any) : Rewrite::Any
    @exhr.put_if_absent({id, term}) { yield }
  end

  def backpath(& : Backpath::Appender -> Backpath::Appender)
    return self unless kp0 = @backpath

    copy_with(backpath: yield kp0)
  end

  # Passthrough that notifies the observer (if any) of a *leaf rewrite*: rewrite that
  # does not have a successor.
  #
  # "The observer" here means some kind of function that reacts to rewrites at backpaths.
  # See `observer`.
  def observable(leaf : Rewrite::Any, &explanation : -> String) : Rewrite::Any
    if leaf.is_a?(Rewrite::Some)
      case observer = @observer
      in Observer
        @backpath.try { |backpath| observer.call(backpath, yield, leaf) }
      in Tick
        observer.call
      end
    end

    leaf
  end
end

# A rewriter that does not rewrite. The "zero" of rewriters.
#
# NOTE: noRs should be handled by the caller. noR itself does not know whether
# what you give it is a rewrite or not vs. the original term (whatever it is).
# The caller must keep track of its rewriting progress; if a rewriter like noR
# refuses to rewrite, the caller must keep its rewriting progress unchanged. If
# a rewriter agrees to rewrite, the caller must update its progress.
def noR : Rewriter
  Rewriter.new { Rewrite.none }
end

# A rewriter that unconditionally replaces anything with one *term*.
def oneR(term term1 : Term) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce do |term0|
      # Since this is a Rewrite introduction point we must fortify it with
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
    # Just as above, we're defensive here.
    staging.reduce do |term0|
      ctx.observable(Rewrite.many(list).diff(term0)) { "unconditional replace with many" }
    end
  end
end

# :nodoc:
def envR(ctx, prefix, term term0)
  ctx.envs.items.reverse_each do |env|
    next unless pterm = env.follow?(prefix)
    next unless pdict = pterm.as_d?
    next unless term1 = pdict[term0]?

    return ctx.observable(Rewrite.one(term1).diff(term0)) { "replace with value from the environment" }
  end

  Rewrite.none
end

# Rewrites a term by replacing it with a corresponding value from the environment.
#
# Follows keypath *prefix* into the environment, replaces the rewritten term with
# its corresponding value from the dictionary thus reached. Noop if *prefix* cannot
# be followed or there is no corresponding value there.
#
# The environment acts as a lookup table that certain rewriters (like `envR`) consult
# to determine how a term should be transformed; in case of `envR`, what it should be
# replaced with.
#
# The environment is mainly managed by `rulesetR`.
def envR(prefix : Enumerable(Term)) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| envR(ctx, prefix, term) }
  end
end

# See `envR(prefix : Enumerable(Term))`.
def envR(*prefix : Term) : Rewriter
  envR(prefix)
end

# :nodoc:
EMPTY_PREFIX = [] of Term

# Same as `envR(prefix : Enumerable(Term))`, but with an empty prefix.
def envR : Rewriter
  envR(EMPTY_PREFIX)
end

# TODO: we should be able to implement this more efficiently in the future!
private def splice(dict : Term::Dict, splices : Array({Term::Num, Term::Dict}))
  splices.each do |start, splice|
    dict = dict.replace(start, &.concat(splice.items))
  end

  dict
end

# :nodoc:
def itemsR(ctx0, term, successor, start : Int32) : Rewrite::Any
  unless dict0 = term.as_d?
    return Rewrite.none
  end

  splices = nil

  dict1 = dict0.transaction do |commit|
    # We **must** call successor in proper order due to observers which are only
    # capable of doing one backpath-insert at a time.
    (start...dict0.itemsize).reverse_each do |index|
      item = dict0[index]

      ctx1 = ctx0.backpath &.update_value(index)

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
    # Dict transactions only modify the underlying dict upon the first with()/
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
#
# *start* specifies which item should be considered the first. If the
# dictionary contains less items than that, it won't be rewritten.
def itemsR(successor : Rewriter, *, start : Int32 = 0) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| itemsR(ctx, term, successor, start) }
  end
end

# :nodoc:
def pairsR(ctx0, term, successor) : Rewrite::Any
  unless dict0 = term.as_d?
    return Rewrite.none
  end

  dict1 = dict0.transaction do |commit|
    dict0.pairspart.each_entry do |key, value0|
      ctx1 = ctx0.backpath &.update_value(key)

      case rewrite = successor.call(ctx1, Rewrite.one(value0))
      in Rewrite::None
      in Rewrite::One  then commit.with(key, rewrite.term)
      in Rewrite::Many then commit.with(key, rewrite.list)
      end
    end
  end

  # See `itemsR` to learn why `same?` is sufficient here.
  dict0.same?(dict1) ? Rewrite.none : Rewrite.one(dict1)
end

# Rewrites the pairspart of a dictionary using *successor*.
def pairsR(successor : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| pairsR(ctx, term, successor) }
  end
end

# :nodoc:
def callR(ctx, term, callable)
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
def chainR(ctx : RewriterContext, term : Term, a : Rewriter, b : Rewriter)
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
def chainR(a : Rewriter, b : Rewriter, *cs : Rewriter) : Rewriter
  chainR(chainR(a, b), *cs)
end

# :nodoc:
def allR(ctx : RewriterContext, term : Term, a : Rewriter, b : Rewriter)
  unless lhs = a.call(ctx, Rewrite.one(term)).as?(Rewrite::Some)
    return Rewrite.none
  end

  b.call(ctx, lhs)
end

# Similar to `chainR`, but rewrites to the last successful rewrite
# (i.e. skipping all rewriters past the one that failed, if any).
def allR(a : Rewriter, b : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| allR(ctx, term, a, b) }
  end
end

# :ditto:
def allR(a : Rewriter, b : Rewriter, *cs : Rewriter) : Rewriter
  allR(allR(a, b), *cs)
end

# :nodoc:
def choiceR(ctx : RewriterContext, term : Term, a : Rewriter, b : Rewriter)
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

# :ditto:
def choiceR(a : Rewriter) : Rewriter
  a
end

# Rewrites entries of a dictionary (items and pairs) using *successor*.
def entriesR(successor : Rewriter) : Rewriter
  chainR(itemsR(successor), pairsR(successor))
end

# :nodoc:
def entryR1(ctx0, dict, key, value, successor)
  ctx1 = ctx0.backpath &.update_value(key)

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
def entryR(ctx, term, successor)
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
          # entryR always returns a dictionary, but we're a little too loose
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
def selR(ctx, term, selector, successor)
  if env = M1::Operator.match?(Term[], selector, term)
    if rewritee = env[:rewritee]?
      return successor.call(ctx, Rewrite.one(rewritee)).as?(Rewrite::Some) || Rewrite.one(rewritee)
    end
  end

  Rewrite.none
end

# :nodoc:
def selR(selector, successor)
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

SELR_SELECTOR_CACHE = SyncCache(String, Term).new(1024, preallocate: true, byref: true)

# See the main overload (`Term`) for more info.
def selR(selector : String, successor : Rewriter) : Rewriter
  selR(SELR_SELECTOR_CACHE.fetch(selector) { ML.term(selector) }, successor)
end

# Generates a `choiceR` with more than two branches for you to reduce typing.
def switchR(branches : Enumerable({String, Rewriter}) | Enumerable({Term, Rewriter}) | Enumerable({M1::Operator::Any, Rewriter})) : Rewriter
  choice = nil

  branches.each do |selector, successor|
    rewriter = selR(selector, successor)
    choice = choice ? choiceR(choice, rewriter) : rewriter
  end

  choice || noR
end

# :ditto:
def switchR(*branches : {String, Rewriter} | {Term, Rewriter} | {M1::Operator::Any, Rewriter})
  switchR(branches)
end

# Rewrites a term using *successor*; if that produces no change and the term is
# a dictionary term, recurses on its items and pair values (`entriesR`).
def dfsR(successor : Rewriter) : Rewriter
  set, rec = recR
  set.call choiceR(successor, entriesR(rec))
end

# Rewrites a term using *successor*; if that produces no change and the term is
# a dictionary term, recurses on its items (`itemsR`).
def itemdfsR(successor : Rewriter) : Rewriter
  set, rec = recR
  set.call choiceR(successor, itemsR(rec))
end

# :nodoc:
def exhR(ctx, term, successor)
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
def exhR(ctx, id, term, successor)
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

# :nodoc:
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

  # Just like in `itemsR`, here we must iterate in a keypath-friendly
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

  def self.relr(ctx0, bottom, term, ascent, env, successor)
    if M1::Operator.probe?(env, bottom, term)
      return Ascend.new(ascent)
    end

    unless dict0 = term.as_d?
      return None.new
    end

    splices = nil

    dict1 = dict0.transaction do |commit|
      each_entry_keypath_friendly(dict0) do |key, value|
        ctx1 = ctx0.backpath &.update_value(key)

        case response = relr(ctx1, bottom, value, ascent, env, successor)
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
      # See `itemsR` to learn why this `same?` check is sufficient here.
      return Ready.new(dict0.same?(dict1) ? Rewrite.none : Rewrite.one(dict1))
    end

    dict1 = splice(dict0, splices)

    Ready.new(Rewrite.one(dict1))
  end
end

module RelrEnv
  extend self

  alias Any = Literal | Option

  record Literal, dict : Term::Dict
  record Option, key : Term

  def resolve(ctx, env : Literal) : Term::Dict
    env.dict
  end

  def resolve(ctx, env : Option) : Term::Dict
    return Term[] unless candidate = ctx.options[env.key]?
    return Term[] unless dict = candidate.as_d?

    dict
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
# - Once it ascends *ascent* levels, it applies *successor* at that point.
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
def relR(bottom : M1::Operator::Any, successor : Rewriter, *, ascent : Int32 = 0, env : RelrEnv::Any = RelrEnv::Literal.new(Term[])) : Rewriter
  if ascent.negative?
    raise ArgumentError.new("ascent must be positive or 0")
  end

  Rewriter.new do |ctx, staging|
    env_dict = RelrEnv.resolve(ctx, env)

    staging.reduce do |term|
      case response = Relr.relr(ctx, bottom, term, ascent, env_dict, successor)
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
  relR(ML.term(bottom), successor, **kwargs)
end

# :nodoc:
def pbranchR(ctx, term, pset, a, b) : Rewrite::Any
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

# Lists the call edges supported by `effectR`. Combining them will result
# in multiple calls to the callable at the corresponding points in time.
@[Flags]
enum EffectEdge : UInt8
  # The callable is called with the rewrite before passing it to the successor.
  In
  # The callable is called with the rewrite that the successor made.
  Out
end

# Passthrough to *successor* that calls *callable* on *edge*.
#
# *callable* must respond to `#call(Rewrite::Any)`.
#
# See `EffectEdge` to learn about the available edges.
def effectR(successor : Rewriter, callable, *, edge = EffectEdge::In) : Rewriter
  edge = EffectEdge.new(edge)

  Rewriter.new do |ctx, staging|
    staging.reduce do |term|
      callable.call(Rewrite.one(term)) if edge.in?
      rewrite = successor.call(ctx, Rewrite.one(term))
      callable.call(rewrite) if edge.out?
      rewrite
    end
  end
end

# See the other overload.
def effectR(successor : Rewriter, *, edge = EffectEdge::In, &effect : Rewrite::Any ->) : Rewriter
  effectR(successor, effect, edge: edge)
end

# :nodoc:
struct RewriteApplier
  def initialize(@ctx : RewriterContext, @rewriter : Rewriter)
  end

  def apply(up, down, my, body)
    subctx = @ctx.copy_with(backpath: nil, envs: @ctx.envs.append(Term["$up": up, "$down": down, "$my": my]))
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

  # Maybe the observer shows envs for debugging; we don't know it here.
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
  # Keeping track of backpaths is expensive, so we do pattern matching without
  # backpaths; when we're sure we need backpaths we re-match with backpaths: true.
  #
  # TODO: we should probably use .probe? for this in the pattern set.
  unless pr.envs.all? &.includes?(:"(backpaths)")
    pr = pr.pattern.response(term, backpaths: true).as(Pr::Pos)
  end

  rewrite = M1.backmapr(pr.envs, rule.backspec, term, applier: RewriteApplier.new(ctx, backmapr))

  # The backmap rewriter may lie sometimes that there was a change, make sure
  # we're on track with Rewrites.
  ctx.observable(rewrite.diff(term)) { "replace with backmapped (one)" }
end

def ruleR(ctx, term, rule : Rule::BackmapMany, pr : Pr::Pos, templr, backmapr)
  # TODO: we should probably use .probe? for this in the pattern set.
  unless pr.envs.all? &.includes?(:"(backpaths)")
    pr = pr.pattern.response(term, backpaths: true).as(Pr::Pos)
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

def rulesetR(ctx, term, ruleset, templr, backmapr, elser, env : Term::Dict)
  cursor = ruleset.responses(term, env: env)
  cursor.each do |pr, rule|
    rewrite = ruleR(ctx, term, rule, pr, templr, backmapr).diff(term)

    case rewrite
    in Rewrite::Some
      return rewrite
    in Rewrite::None
    end
  end

  elser.call(ctx, Rewrite.one(term))
end

def rulesetR(ruleset, ruler, backmapr, elser, *, envopt : Term? = nil) : Rewriter
  Rewriter.new do |ctx, staging|
    env = envopt.try { |key| ctx.options[key]? }.try(&.as_d?) || Term[]

    staging.reduce do |term|
      rulesetR(ctx, term, ruleset, ruler, backmapr, elser, env)
    end
  end
end

def metaR(ctx, term term0 : Term, primaryr, metar, successor)
  progress = Rewrite.one(term0)

  primaryr.call(ctx, progress).reduce do |term1|
    progress = Rewrite.one(term1)

    event0 = Term.of(term0, term1)

    metarw = metar.call(ctx, Rewrite.one(event0)).reduce do |event1|
      next Rewrite.none unless event1 = event1.as_itemsonly_d?
      next Rewrite.none unless event1.size == 2

      _, term2 = event1

      Rewrite.one(term2)
    end

    # Even if the successor returns none, we still succeeded rewriting
    # into term1. Similarly, even if metar returns none.

    progress = metarw.as?(Rewrite::Some) || progress
    progress = successor.call(ctx, progress).as?(Rewrite::Some) || progress
    progress
  end
end

def metaR(primaryr : Rewriter, metar : Rewriter, successor : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| metaR(ctx, term, primaryr, metar, successor) }
  end
end

# :nodoc:
def wrapR(ctx, pdisasm, reshape, punwrap, assemble, term0, successor)
  unless ienv = M1::Operator.match?(Term[], pdisasm, term0)
    return Rewrite.none
  end

  filled0 = M1.bsubst(reshape, ienv)

  progress = Rewrite.one(filled0)
  progress = successor.call(ctx, progress).as?(Rewrite::Some) || progress
  progress.reduce do |filled1|
    next Rewrite.none unless oenv = M1::Operator.match?(Term[], punwrap, filled1)
    next Rewrite.none unless term1 = oenv[:out]?

    term2 = M1.bsubst(assemble, ienv | oenv)

    # This is a point of introduction, so we have to be careful about
    # Rewrite.one right here, hence a defensive diff().
    Rewrite.one(term2).diff(term0)
  end
end

# Wrap rewriter enables the disassemble - reshape - unwrap - assemble interaction
# between an outer and inner (*successor*) rewriters.
#
# - *pdisasm* is a pattern that can break the input term down into its constituents.
# - *reshape* is a template term. Blanks in it are substituted with captures from
#   *pdisasm*'s match env.
# - The result of reshaping is fed to *successor*.
# - The output of successor is matched using the given *punwrap* pattern;
# - *punwrap*'s match env is merged with *pdisasm*'s, giving way to *punwrap*'s.
# - Blanks *assemble* are substituted with captures from the merged match env.
# - The resulting term is output as the rewritten term.
#
# TODO: *pdisasm* and *punwrap* currently do not support sources (such as `%item°`);
# only the first match env will be considered, and remaining ones discarded.
def wrapR(pdisasm : M1::Operator::Any, reshape : Term, successor : Rewriter, punwrap : M1::Operator::Any, assemble : Term) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| wrapR(ctx, pdisasm, reshape, punwrap, assemble, term, successor) }
  end
end

# See the main overload.
def wrapR(pdisasm : Term, reshape : Term, successor : Rewriter, punwrap : Term, assemble : Term) : Rewriter
  wrapR(M1.operator(pdisasm), reshape, successor, M1.operator(punwrap), assemble)
end

# See the main overload.
def wrapR(pdisasm : String, reshape : String, successor : Rewriter, punwrap : String, assemble : String) : Rewriter
  wrapR(ML.term(pdisasm), ML.term(reshape), successor, ML.term(punwrap), ML.term(assemble))
end

# Same as `wrapR` but with hard-coded noop disassemble `in_` and assemble `out_`.
def wrapR(reshape, successor : Rewriter, punwrap) : Rewriter
  wrapR(%[in_], reshape, successor, punwrap, %[out_])
end

# :nodoc:
def multipartR(ctx, term0, pdisasm, successors, assemble)
  unless env = M1::Operator.match?(Term[], pdisasm, term0)
    return Rewrite.none
  end

  successors.each do |capture, successor|
    next unless value = env[capture]?

    case rewrite = successor.call(ctx, Rewrite.one(value))
    in Rewrite::None
    in Rewrite::Some
      env = env.with(capture, rewrite.term?)
    end
  end

  term1 = M1.bsubst(assemble, env)

  Rewrite.one(term1).diff(term0)
end

# Multipart rewriter enables the disassemble - process - assemble interaction
# between one outer and many inner rewriters (*successors*).
#
# - Inner rewriters are labeled according to the capture of *pdisasm* pattern
#   they process.
# - *assemble* is a template that assembles the output term from captures made
#   by *pdisasm* and possibly modified by *successors*.
# - *successors* are not required to modify all captures. Some captures may
#   be made simply to be able to reconstruct the shape of the input term
#   in *assemble*.
#
# TODO: *pdisasm* currently does not support sources (such as `%item°`); only
# the first match env will be considered, and remaining ones discarded.
def multipartR(pdisasm : M1::Operator::Any, successors : Enumerable({Term, Rewriter}), assemble : Term) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce { |term| multipartR(ctx, term, pdisasm, successors, assemble) }
  end
end

# See the main overload.
def multipartR(pdisasm : Term, successors : Enumerable({Term, Rewriter}), assemble : Term) : Rewriter
  multipartR(M1.operator(pdisasm), successors, assemble)
end

# See the main overload.
def multipartR(pdisasm : String, successors : Enumerable({Term, Rewriter}), assemble : String) : Rewriter
  multipartR(ML.term(pdisasm), successors, ML.term(assemble))
end

# Same as the main overload, where both *pdisasm* and *assemble* are set
# to *schema*. This is useful in simple cases where the disassemble pattern
# and the assemble template are the same.
def multipartR(schema : Term | String, successors : Enumerable({Term, Rewriter})) : Rewriter
  multipartR(schema, successors, schema)
end

# A very simple memoizer for the *successor* rewriter.
#
# Must be put in "strategic" and, more importantly, *context-independent* places.
# This usually means some kind of "master recursive step" somewhere in the rewriter
# circuit.
def memoR(memo : SyncCache(Term, Rewrite::Any), successor : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce do |term|
      if rewrite = memo.load?(term)
        ctx.observable(rewrite) { "loaded from cache" }

        next rewrite
      end

      rewrite = successor.call(ctx, Rewrite.one(term))

      memo.store(term, rewrite)

      rewrite
    end
  end
end

def cueR(cues : Enumerable(Term::Sym), successor : Rewriter) : Rewriter
  Rewriter.new do |ctx, staging|
    staging.reduce do |term|
      next Rewrite.none unless dict = term.as_d?
      next Rewrite.none unless cues.any? { |cue| dict.probably_includes?(cue) }

      successor.call(ctx, Rewrite.one(term))
    end
  end
end

# Pushes *env* for *successor*. Pops once *successor* is done.
def using(env : Term::Dict, successor : Rewriter)
  Rewriter.new do |ctx, staging|
    # NOTE: we actually do not pop since ctx is immutable. An illusion!
    successor.call(ctx.copy_with(envs: ctx.envs.append(env)), staging)
  end
end

def using(env : RewriterContext -> Term, successor : Rewriter)
  Rewriter.new do |ctx, staging|
    successor.call(ctx.copy_with(envs: ctx.envs.append(env.call(ctx))), staging)
  end
end

def plug(key)
  key = Term.of(key)

  ->(ctx : RewriterContext) { ctx.options[key] }
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

def preview1(term, backpath : Term::Dict, leaf)
  preview1(term, backpath.items, leaf)
end

REWRITE_SEEDER      = Random::PCG32.new
REWRITE_SEEDER_LOCK = Mutex.new

def rewrite0(term : Term, rewriter : Rewriter, **options) : Rewrite::Any
  seed = REWRITE_SEEDER_LOCK.synchronize { REWRITE_SEEDER.rand(UInt64) }
  rng = Random::PCG32.new(seed)
  ctx = RewriterContext.new(rng, backpath: nil, options: Term[options])
  rewriter.call(ctx, Rewrite.one(term))
end

def rewrite(term : Term, rewriter : Rewriter, **options) : Term
  rewrite = rewrite0(term, rewriter, **options)
  rewrite.term? || term
end

def rewrite(term : Term, rewriter : Rewriter, observer : Observer, **options) : Term
  seed = REWRITE_SEEDER_LOCK.synchronize { REWRITE_SEEDER.rand(UInt64) }
  rng = Random::PCG32.new(seed)
  ctx = RewriterContext.new(rng, backpath: Backpath::Appender.new, observer: observer, options: Term[options])

  rewrite = rewriter.call(ctx, Rewrite.one(term))
  rewrite.term? || term
end

def rewrite(term : Term, rewriter : Rewriter, observer : Tick, **options) : Term
  seed = REWRITE_SEEDER_LOCK.synchronize { REWRITE_SEEDER.rand(UInt64) }
  rng = Random::PCG32.new(seed)
  ctx = RewriterContext.new(rng, backpath: nil, observer: observer, options: Term[options])

  rewrite = rewriter.call(ctx, Rewrite.one(term))
  rewrite.term? || term
end

{% if flag?(:qux) %}
  # mod = ProcRuleset.build do
  #   rulepi1 %[(node (+ (literal a_number) (literal b_number)))] { {:node, {:literal, a + b}} }
  #   rulepi1 %[(node (- a_ b_))] { {:node, a - b} }
  #   rulepi1 %[(node (* a_ b_))] { {:node, a * b} }
  # end

  mod1 = ProcRuleset.build do
    rulepi1 %[(+ a_number b_number)] { a + b }
  end

  mod2 = ProcRuleset.build do
    rulepi1 %[(* a_number b_number)] { a * b }
  end

  changed = ProcRuleset.build do
    rulepi1 %[(v0←(+ a_ b_) c_number)] do
      {v0, {:result, {:+, a, b}, c}}
    end

    rulepi1 %[(v0←(- a_ b_) c_number)] do
      {v0, {:result, {:-, a, b}, c}}
    end
  end

  passable = PatternSet.select(ML.term(%[pattern_]), ML.terms(<<-WWML
  (passage_* ¦ _)
  {¦ qux: passage_}
  WWML
  ))

  impassable = PatternSet.select(ML.term(%[pattern_]), ML.terms(<<-WWML
  WWML
  ))

  # (<pred> child)
  # ((<pred> parent) child)
  # (((<pred> grandparent) parent) child)
  # ... etc -- edge rewrite

  pp rewrite(Term.of(:qux, {:+, 1, 2}, {:*, 3, 4}, a: 100, b: 200, qux: 300),  chainR(wrapR(%[in_], %[in_], noR, %[out_], %[(out_)]), edgeR(effectR(noR) { |re| pp re})))

  # puts rewrite(Term.of(:+, 1, 2), wrapR(%[(+ a_ b_)], %[(node (+ (literal a_) (literal b_)))], %[(node (literal out_))], callR(mod)))
  # puts rewrite(Term.of(:*, 5, 3), wrapR(%[(node in_)], callR(mod), %[(node out_)]))
  # puts rewrite(Term.of(:+, 4, 5), metaR(effectR(callR(mod), edge: {:in, :out}) { |re| pp re }, callR(changed), effectR(noR, edge: {:in, :out}) { |re| pp re }))
  # puts rewrite(Term.of(:-, 4, 5), metaR(effectR(callR(mod), edge: {:in, :out}) { |re| pp re }, callR(changed), effectR(noR, edge: {:in, :out}) { |re| pp re }))
  # puts rewrite(Term.of(:*, 4, 5), metaR(effectR(callR(mod), edge: {:in, :out}) { |re| pp re }, callR(changed), effectR(noR, edge: {:in, :out}) { |re| pp re }))
  # puts rewrite(Term.of(:/, 4, 5), metaR(effectR(callR(mod), edge: {:in, :out}) { |re| pp re }, callR(changed), effectR(noR, edge: {:in, :out}) { |re| pp re }))
{% end %}

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
# [x] metaR "rewrite rewriter", detects modifications of successor1, passes to successor2, result is successor2
# [x] wrapR "template rewriter", accepts a template, plugs in the input term, rewrites with successor, extracts back.
# [ ] pathR
#    - rewrites a path from root to some offspring,
#    - using passable and impassable psets,
#    - successor rewrites path,
#    - allows to specify max path depth (overall) and path view range (how much of path to give to successor)
#    - we'd like to trace where we are; a cursor of some kind would be useful. the question is, how
#      do we allow to ignore the cursor as well? if path elements can be (noncurrent <elem>) or (current <elem>),
#      how can we NOT force it onto every pattern but only onto patterns that care about current/noncurrent?
#         we can do this naively by sending both to the successor, but then this raises questions so as to
#         what to do if both a cursor and a non-cursor versions are defined.
# [ ] we should be able to combine pathR and metaR to trace changes across the hierarchy in "two dimensions"
#      this can be done by the circuit pathR -> ??? -> multipartR(%[(parent_ child_)], parent: metaR(...), child: metaR(...), %[(parent_ child_)])
# [x] multipartR
#     - disassemble, map, assemble
#     - disassemble = assemble, map
# [ ] how to combine pathR, metaR, and wrapR to run edt3 and related?
# [ ] building rewriter circuits with Terms
# [ ] improve debugging: instead of sending keypath/etc. to Observer, send Reports of some kind,
#     including those with keypath/etc (as now). One of such messages would be NewRewritee, very very
#     useful for debugging backmaps (since they, unlike rules, cannot be readily/easily embedded into
#     the original term. It is possible but needlessly hard. BUT THEN, maybe it is possible to embed
#     backmaps into the original term? That'd be much more visual! We need backmaps to give us keypath
#     into the original term in BackmapApplier for that to work.
#    - in the simplest version, to highlight rewrites we'd need the parser to cooperate: the parser
#      can give us kp => b...e map; we take a term, ML.string it, ML.term_with_keypaths the resulting
#      string, and then lookup the rewrite's keypath in ML.term_with_keypaths; this will give us b...e
#      which we can highlight in the string by e.g. surrounding b...e with highlighting escape sequences.
