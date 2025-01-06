# `M0` is a tiny internal pattern matching engine we're using to implement
# the main pattern matching engine `M1`.
#
# See `match?` for more details.
module Ww::Term::M0
  extend self

  # :nodoc:
  def match?(commit, pattern : Term::Sym, matchee : ITerm) : Bool
    unless (blank = pattern.blank?) && blank.single?
      return pattern == matchee
    end

    return false unless matchee.type.subtype?(blank.type)
    return true unless name = blank.name?

    unless prev = commit[name]?
      commit.with(name, matchee)
      return true
    end

    prev == matchee
  end

  # :nodoc:
  SYMBOL_LITERAL = Term.of(:"%literal")

  # :nodoc:
  SYMBOL_PARTITION = Term.of(:"%partition")

  # :nodoc:
  SYMBOL_BLANK_STAR = Term.of(:"_*")

  # :nodoc:
  def match?(commit, pattern : Term::Dict, matchee : ITerm) : Bool
    if pattern.itemsonly? && pattern.size > 1
      hi = pattern.size - 1

      case {pattern[0], hi}
      when {SYMBOL_PARTITION, 2}
        return false unless matchee.is_a?(Term::Dict)

        items, pairs = matchee.partition
        items = items.collect
        itemspart = pattern[1].downcast
        pairspart = pattern[2].downcast

        return match?(commit, itemspart, items) && match?(commit, pairspart, pairs)
      when {SYMBOL_LITERAL, 1}
        return pattern[1] == matchee
      end

      if pattern[hi] == SYMBOL_BLANK_STAR
        return false unless matchee.is_a?(Term::Dict) && matchee.itemsonly?
        return false if matchee.size < hi

        # In practice `hi` is very small; most often it's 1.
        matchee = matchee.items.begin.grow(hi).collect
        pattern = pattern.without(hi)
      end
    end

    return false unless matchee.is_a?(Term::Dict)
    return false unless pattern.size == matchee.size

    pattern.ee.all? do |k, v0|
      (v1 = matchee[k]?) && match?(commit, v0.downcast, v1.downcast)
    end
  end

  # :nodoc:
  def match?(commit, pattern : ITerm, matchee : ITerm) : Bool
    pattern == matchee
  end

  # Returns a match env if *pattern* matches *matchee*. Returns `nil` otherwise.
  #
  # M0 implements a tiny subset of constructs from the main pattern matching language
  # `M1`: *all* patterns that M0 can match, M1 will match; and *some* patterns that M1
  # can match, M0 will match.
  #
  # The following constructs are supported in M0:
  #
  # - Literals: `100 "hello" xyzzy true ...`.
  # - Blanks (named, unnamed, typed, untyped): `_ _number x_number`.
  # - Partition: split a dictionary into its items and pairs partition, for
  #   example `(%partition itemspart_ pairspart_)`.
  # - Match p1-N on the first N items of an itemspart, correspondingly, for
  #   example `(a b c _*)`.
  # - `(%literal _)`: match literally, mainly for escaping the above and itself.
  # - Recursive application of all of the above and itself on dictionary items
  #   and pairs, for example: `(+ a_number 100 x: x_string)`.
  #
  # The returned match env contains blank names mapped to the term that they have captured
  #  E.g. `(+ a_ b_)` on `(+ 1 2)` will return the following match env: `{a: 1, b: 2}`.
  #
  # The semantics for using the same blank across the pattern is preserved, in that such
  # uses would be treated as an assertion of equality. And for instance running `(+ a_ a_)`
  # on `(+ x y)` will fail, whereas running the same pattern on `(+ x x)` will succeed with
  # a match env `{a: x}`.
  #
  # M0 exists to simplify the implementation of the compiler for M1, since the latter
  # needs to do lots of `Term` pattern matching itself.
  #
  # Since I did not want to run into the various paradoxes arising from self-reference,
  # I've decided to have a separate, much dumber implementation of an entire engine.
  # I could have made M1 depend on M1 with careful control flow etc.; but there's too
  # much circularity anyway.
  def match?(pattern : Term, matchee : Term) : Term::Dict?
    Term::Dict.build do |commit|
      return unless match?(commit, pattern.downcast, matchee.downcast)
    end
  end
end
