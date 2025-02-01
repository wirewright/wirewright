require "./wirewright"
require "./baz5_common"

SELECTOR = ML.parse1(%[(%any° (rule pattern_ template_) (backmap pattern_ backspec_))])
BASE = ML.parse(File.read("#{__DIR__}/editor.soma.wwml"))
RULESET = Ruleset.select(SELECTOR, BASE)

module Changes
  alias Any = Preview | Accept
  alias Preview = Term, Rewrite::Some ->
  alias Accept = ->
end

alias Rewriter = Changes::Any, Rewrite::Any -> Rewrite::Any

# TODO: merge itemsR and pairsR into a single entriesR with an optional
# part arg.
# TODO: add a flag to use each_entry_randomized

# FIXME: crazy crazy shitcode
# :nodoc:
#
# Less efficient `itemsR` implementation for `Changes::Preview` procs.
def itemsR(changes : Changes::Preview, term : Term, successor, *, limit) : Rewrite::Any?
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
# More efficient `itemsR` implementation for `Changes::Accept` procs.
def itemsR(changes : Changes::Accept, term : Term, successor, *, limit) : Rewrite::Any?
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
def itemsR(successor, *, limit = nil) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce do |term|
      itemsR(changes, term, successor, limit: limit)
    end
  end
end

# FIXME: crazy crazy shitcode
# :nodoc:
#
# Less efficient `pairsR` implementation for `Changes::Preview` procs.
def pairsR(changes : Changes::Preview, term : Term, successor, *, limit) : Rewrite::Any?
  return Rewrite.none unless dict0 = term.as_d?

  dict1 = dict0
  changed = false

  dict0.pairspart.each_entry do |key, value|
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
# More efficient `pairsR` implementation for `Changes::Accept` procs.
def pairsR(changes : Changes::Accept, term : Term, successor, *, limit) : Rewrite::Any?
  return Rewrite.none unless dict0 = term.as_d?

  changed = false

  dict1 = dict0.transaction do |commit|
    dict0.pairspart.each_entry do |key, value|
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
def pairsR(successor, *, limit = nil) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce do |term|
      pairsR(changes, term, successor, limit: limit)
    end
  end
end

# Rewrites one item of a dictionary.
#
# Shorthand for `itemsR(successor, limit: 1)`
def itemR(successor) : Rewriter
  itemsR(successor, limit: 1)
end

# Rewrites one pair value of a dictionary.
#
# Shorthand for `pairsR(successor, limit: 1)`
def pairR(successor) : Rewriter
  pairsR(successor, limit: 1)
end

# Rewrites one entry (an item or a pair value) of a dictionary.
def entryR(successor) : Rewriter
  choiceR(itemR(successor), pairR(successor))
end

# A rewriter that does nothing.
def noR : Rewriter
  Rewriter.new { Rewrite.none }
end

# :nodoc:
def callR(changes : Changes::Any, term : Term, callable)
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
def callR(callable) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce { |term| callR(changes, term, callable) }
  end
end

# :nodoc:
def chainR(changes : Changes::Any, term : Term, a, b)
  lhs = a.call(changes, Rewrite.one(term))
  rhs = b.call(changes, lhs.as?(Rewrite::Some) || Rewrite.one(term))

  {lhs, rhs}.rightmost?(Rewrite::Some) || Rewrite.none
end

# Rewrites a term first using *a*, then the result of that using *b*, etc.
def chainR(a, b) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce { |term| chainR(changes, term, a, b) }
  end
end

# :ditto:
def chainR(a, b, *cs) : Rewriter
  chainR(chainR(a, b), *cs)
end

# :nodoc:
def choiceR(changes : Changes::Any, term : Term, a, b)
  lhs = a.call(changes, Rewrite.one(term))
  lhs.as?(Rewrite::Some) || b.call(changes, Rewrite.one(term))
end

# Picks the first successful rewriter out of *a*, *b*, etc.
def choiceR(a, b) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce { |term| choiceR(changes, term, a, b) }
  end
end

# :ditto:
def choiceR(a, b, *cs) : Rewriter
  choiceR(choiceR(a, b), *cs)
end

# Rewrites the items and pair values of a dictionary term using *successor*.
def entriesR(successor) : Rewriter
  chainR(itemsR(successor), pairsR(successor))
end

# Allows you to set up a rewriter that can reference itself. Returns a pair
# of procs: the first one is for setting the rewriter that will be used for
# recursion, and the second one represents the recursive rewriter itself.
#
# Here is an example dfsR definition:
#
# ```
# # successor = <some rewriter>
#
# set, rec = recR
# set.call choiceR(successor, entriesR(rec))
#
# # You can use `rec` or the rewriter returned by `set` now. They're
# # not identical (`rec` introduces one level of indirection); but
# # they do the same thing.
# ```
def recR : {(Rewriter -> Rewriter), Rewriter}
  slot = nil

  set = ->(rewriter : Rewriter) { slot = rewriter }
  rec = Rewriter.new do |changes, operand|
    unless successor = slot
      next Rewrite.none
    end

    successor.call(changes, operand)
  end

  {set, rec}
end

# Rewrites a term using successor; if that produces no change recurses on
# the items and pair values of a dictionary term. Items are visited in reverse.
def dfsR(successor) : Rewriter
  set, rec = recR
  set.call choiceR(successor, entriesR(rec))
end

# :nodoc:
def selR(changes : Changes::Any, term : Term, selector, successor)
  if env = M1::Operator.match?(Term[], selector, term)
    if rewritee = env[:rewritee]?
      return successor.call(changes, Rewrite.one(rewritee))
    end
  end

  Rewrite.none
end

# :nodoc:
def selR(selector : M1::Operator::Any, successor) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce { |term| selR(changes, term, selector, successor) }
  end
end

# Selective rewriter.
#
# *selector* pattern is used to match a term, and if a match is found, the capture
# `rewritee` is passed to the *successor* rewriter.
def selR(selector : Term, successor) : Rewriter
  selR(M1.operator(selector), successor)
end

# :ditto:
def selR(selector : String, successor) : Rewriter
  selR(ML.parse1(selector), successor)
end

# :nodoc:
def exhR(changes : Changes::Any, term : Term, successor)
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
def exhR(successor) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce { |term| exhR(changes, term, successor) }
  end
end

# Absolute rewriter. Performs absolute rewriting of a term using *successor*.
#
# *Absolute rewriting* is similar to depth-first search rewriting `dfsR`;
# the notable difference is that `absR` rewrites just one entry at any depth
# (the first one it can reach); whereas `dfsR` rewrites all entries at any
# depth, minus those that are themselves results of previous rewriting.
#
# Absolute rewriting is useful when we want to prioritize context in rules.
# That is, rules with most context must be tried first. With `absR`, any
# rewrite can subsequently be assessed in context.
#
# `absR` is an intrinsically inefficient way to rewrite, especially for very
# deep terms (since for any smallest change `absR` will backjump to the root).
# It is often used in combination with `relr`, which allows to set the "ceiling"
# for `absR` to jump to based on some floor pattern.
def absR(successor) : Rewriter
  set, rec = recR
  set.call choiceR(successor, entryR(rec))
end

module Relr
  record Ready, rewrite : Rewrite::Any
  record Ascend, ascent : Int32
  record None
end

# FIXME: crazy crazy shitcode
def relr0(changes : Changes::Preview, floor, term, ascent, successor)
  if M1::Operator.probe?(Term[], floor, term)
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
  if M1::Operator.probe?(Term[], floor, term)
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

def relR(floor : M1::Operator::Any, successor, *, ascent : Int32 = 0) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce do |term|
      case response = relr0(changes, floor, term, ascent, successor)
      in Relr::None   then Rewrite.none
      in Relr::Ready  then response.rewrite
      in Relr::Ascend then successor.call(changes, Rewrite.one(term))
      end
    end
  end
end

def relR(floor : Term, successor, **kwargs) : Rewriter
  relR(M1.operator(floor), successor, **kwargs)
end

def relR(floor : String, successor, **kwargs) : Rewriter
  relR(ML.parse1(floor), successor, **kwargs)
end

# :nodoc:
def pbranchR(changes, term, pset, a, b)
  case pset.response(term)
  in Pr::Pos then a.call(changes, Rewrite.one(term))
  in Pr::Neg then b.call(changes, Rewrite.one(term))
  end
end

# Rewrites using *a* any term to which pattern set *pset* responds positively.
# Rewrites using *b* any other term.
def pbranchR(pset : PatternSet, a, b) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce { |term| pbranchR(changes, term, pset, a, b) }
  end
end

# TODO: support Changes::Preview
struct RewriteApplier(T)
  def initialize(@changes : Changes::Accept, @rewriter : T)
  end

  def call(up0, up1, down, my, matchee0, body)
    app = M1::DefaultApplier.new
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

def rulesetR(changes, term, ruleset, ruler, backmapr, elser)
  apply = ->(pr : Pr::Pos, rule : Rule::Any) do
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

      result = M1.backmap(pr.envs, rule.backspec, term, applier: RewriteApplier.new(subchanges, backmapr))
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
        result = M1.backmap(pr.envs, rule.backspec, term, applier: RewriteApplier.new(subchanges, backmapr))

        if rule.backspec.includes?({rule.toplevel}) && (list = result.as_d?)
          list == Term[{term}] ? Rewrite.none : Rewrite.many(list)
        else
          result == term ? Rewrite.none : Rewrite.one(result)
        end
      in Pr::Many
        list = Term::Dict.build do |commit|
          pr.ones do |one|
            commit << M1.backmap(one.envs, rule.backspec, term, applier: RewriteApplier.new(subchanges, backmapr))
          end
        end

        list == Term[{term}] ? Rewrite.none : Rewrite::Many.new(list)
      end
    end
  end

  needle = ruleset.responses(term).compact_map do |pr, rule|
    rewrite = apply.call(pr, rule)

    case rewrite.diff(term)
    in Rewrite::None
    in Rewrite::Some
      rewrite
    end
  end

  needle.first? || elser.call(changes, Rewrite.one(term))
end

def rulesetR(ruleset, ruler, backmapr, elser) : Rewriter
  Rewriter.new do |changes, operand|
    operand.reduce { |term| rulesetR(changes, term, ruleset, ruler, backmapr, elser) }
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

# pp itemsR(successor).call(preview, Rewrite.one(term))
# pp itemsR(successor).call(accept, Rewrite.one(term))
# pp pairsR(successor).call(preview, Rewrite.one(term))
# pp pairsR(successor).call(accept, Rewrite.one(term))

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
CURSORPE = M1.operator(ML.parse1(%([_string (%any° | (| _string)) _string (_*) @edge_])))

# TODO: Currently reads&writes very obscurely. We'd want to be able to define these
# more, how shall I say... "elegantly". These look like grammars or some kind
# of reverse grammars to me. So maybe looking into the grammar description languages
# out there, we can take some inspiration. In my mind they form a kind of bottom-up
# graph or more precisely, tree. But trees are unreadable as S-expressions, what you
# see below is basically a tree, and it'd look almost the same in Sexps (and similarly unreadable).
def editR
  dollarr = exhR(dfsR(callR(NATRS)))
  backmapr = dfsR(
    choiceR(
      selR(%[($ rewritee_)], dollarr),
      selR(%[($once rewritee_)], callR(NATRS)),
    ),
  )
  exhR(relR(CURSORP, absR(rulesetR(RULESET, noR, backmapr, noR)), ascent: 2))
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

# pp relR(%[($ _)], exhR(absR(selR(%[($ rewritee_)], callR(successor)))), ascent: 3).call(preview, Rewrite.one(term))
# pp relR(%[($ _)], callR(successor), ascent: 1).call(preview, Rewrite.one(term))

# pp exhR(absR(selR(%[($ rewritee_)], exhR(dfsR(callR(successor)))))).call(preview, Rewrite.one(term))
# pp itemsR(callR(successor)).call(preview, Rewrite.one(term))

# [x] itemsR
# [x] pairsR
# [x] chainR
# [x] choiceR
# [x] entriesR
# [x] dfsR
# [x] callR
# [x] selR
# [x] exhR
# [x] noR
# [x] itemR -> itemsR(limit: 1)
# [x] pairR -> pairsR(limit: 1)
# [x] entryR -> choiceR(itemR, pairR)
# [x] absR
# [x] relr

# TODO: make these into actual rewriters that fit with the rest.
#       1. Either orthor (orthogonal rewriter) must classify rules in a ruleset in buckets
#          by their size (aka depth) or we should be able to compose two orthors at the rewriter
#          circuit level. I'm sort of leaning towards the former since it automates away a
#          choice that is too boring to make.
#       2. Note that orthor, like rulesetR, accepts a ruleset. Perhaps a modified/wrapped ruleset
#          based on the above.

def orthor1(parent0, phase, child0, callable)
  while true
    parent1, child1 = callable.call(parent0, phase, child0)
    break if {parent0, child0} == {parent1, child1}
    parent0, child0 = parent1, Term.of(child1)
  end

  {parent0, child0}
end

def orthor(dict0 : Term::Dict, callable)
  dict1 = dict0
  dict0.items.each_with_index do |v0, k|
    dict1, v0 = orthor1(dict1, Term.of(:teach), Term.of(v0), callable)
    if vd = v0.as_d?
      v0 = orthor(vd, callable)
    end
    dict1, v1 = orthor1(dict1, Term.of(:learn), Term.of(v0), callable)
    dict1 = dict1.with(k, v1)
  end
  (0..).each do |i|
    pre = dict1
    dict1.items.each_with_index do |v0, k|
      dict1, v1 = orthor1(dict1, Term.of(:refine, i), Term.of(v0), callable)
      dict1 = dict1.with(k, v1)
    end
    if pre == dict1
      break
    end
  end
  dict1
end

def effectR(successor, &fn : Term ->)
  ->(changes : Changes::Any, operand : Rewrite::Any) do
    operand.reduce do |term|
      fn.call(term)

      successor.call(changes, operand)
    end
  end
end
