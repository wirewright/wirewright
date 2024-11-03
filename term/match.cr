struct Ww::Term
  module Match
    def self.compile(object) : Pattern
      compile(parse(object))
    end

    def self.compile(parseout : Parseout) : Pattern
      parseout.pattern.simple
    end

    def self.parse(object) : Parseout
      Parseout.from(Term[object])
    end
  end

  def self.match?(pattern, matchee, env = Term[]) : Dict?
    return unless response = Match.compile(pattern).match?(env, self[matchee])

    env, _ = response
    env
  end

  def self.matchp?(pattern : String, matchee : String, env = Term[]) : Dict?
    match?(ML.parse(pattern), ML.parse(matchee), env)
  end
end

require "./match/pattern"
require "./match/parseout"

# module Ww
#   struct Term
#     private alias Items = Dict::ItemsView

#     # Matches one of special @-patterns. Currently supported are:
#     #
#     # - `(bind@ name pattern)`: bind the matchee of `pattern` to `name`.
#     # - `(held@ pattern)`: if the matchee is held applies `pattern` on the held
#     #   value; otherwise, applies pattern to matchee directly.
#     private def self.matchspecial?(env, pattern : Items, term : ITerm)
#       args = pattern.move(1)

#       case pattern.first?
#       when SYMBOL_BIND_AT
#         return :malformed unless args.size == 2
#         name, subpattern = args
#         return :matchless unless env = unify?(env, subpattern.downcast, term.downcast)
#         return :matchless unless env = env.unify?(name, term)
#         env
#       when SYMBOL_HELD_AT
#         return :malformed unless args.size == 1
#         subpattern = args[0]
#         if term.is_a?(Term::Dict)
#           items, entries = term.partition
#           if entries.empty? && items.size == 2 && items[0] == SYMBOL_HOLD
#             term = items[1]
#           end
#         end
#         return :matchless unless env = unify?(env, subpattern.downcast, term.downcast)
#         env
#       else
#         :matchless
#       end
#     end

#     private def self.unify?(env, a : Sym, b : ITerm) : Dict?
#       unless blank = a.blank?
#         return unifydefault?(env, a, b)
#       end
#       return unless b.type.subtype?(blank.type)
#       unless name = blank.name?
#         return env
#       end
#       env.unify?(name, blank.poly? ? {b} : b)
#     end

#     private def self.unify?(env, a : Dict, b : ITerm) : Dict?
#       a_part = a.partition
#       a_items, a_entries = a_part

#       # None of @-patterns accept entries, save time.
#       if a_entries.empty?
#         case result = matchspecial?(env, a_items, b)
#         when Dict       then return result
#         when :malformed then return # Malformed @-patterns must not match anything!
#         end
#       end

#       return unless b.is_a?(Dict)

#       b_part = b.partition
#       b_items, b_entries = b_part
#       return unless a_entries.size == b_entries.size

#       # Check the shortest of items, entries in pattern first.
#       minfirst(a: a_items, b: a_entries) do |step|
#         case step
#         when :a
#           return unless env = unify?(env, a_items, b_items)
#         when :b
#           a_entries.each_entry do |key, pattern|
#             return unless value = b_entries[key]?
#             return unless env = unify?(env, pattern.downcast, value.downcast)
#           end
#         end
#       end

#       env
#     end

#     private def self.unify?(env, a : ITerm, b : ITerm) : Dict?
#       unifydefault?(env, a, b)
#     end

#     # Default/fallback action for `unify?`. Catches all patterns except for
#     # symbol and dictionary patterns.
#     private def self.unifydefault?(env, a : ITerm, b : ITerm) : Dict?
#       a == b ? env : nil
#     end

#     private def self.unify?(env, pattern : Items, matchee : Items) : Dict?
#       if pattern.empty? && matchee.empty?
#         return env
#       end
#       return unless a = pattern.first?
#       return unless rest = unify?(env, pattern, matchee, a.downcast)
#       unify?(*rest)
#     end

#     # This method is an inline, non-allocating version of:
#     #
#     # ```
#     # chunks = neighbors.chunk(&.type)
#     # chunks.map_with_index do |(type, chunk), index|
#     #   if nxt = neighbors[index + 1]?
#     #     yield type, nxt.type, chunk
#     #   else
#     #     yield type, nil, chunk
#     #   end
#     # end
#     # ```
#     private def self.each_chunk_by_type(neighbors, &) : Nil
#       b = 0
#       while b < neighbors.size
#         type = neighbors[b].type
#         e = (b + 1...neighbors.size).find { |i| type != neighbors[i].type }
#         unless e
#           yield type, nil, b...neighbors.size
#           break
#         end
#         yield type, neighbors[e].type, b...e
#         b = e
#       end
#     end

#     private def self.unify?(env, pattern : Items, matchee : Items, a : Sym) : {Dict, Items, Items}?
#       # Capture all consecutive polyblanks as well. If none at all we bail out.
#       polyblanks, _ = pattern.thru(Sym) do |sym|
#         next unless blank = sym.blank?
#         next unless blank.poly?
#         blank
#       end

#       unless polyblanks
#         return dictstep(env, pattern, matchee, a)
#       end

#       return if polyblanks.size > matchee.size

#       # Define the gap as starting at matchee, and ending where the pattern after
#       # all the polyblanks matched.
#       #
#       # foo   xs_+ ys_+      bar baz
#       #       ---------      ^
#       #       polyblanks     find where bar matches
#       #
#       # E.g.:
#       #
#       # foo 1 2 3 4 5 bar baz
#       #    [         ] gap for the polyblanks to fight over!
#       #     ---
#       #     ^   ~~~~~
#       #     |       ^-- 'bar' did not match for all of those
#       #     + at least two items reserved because we have two one-or-more polyblanks

#       remaining_pattern = pattern.move(polyblanks.size)
#       remaining_matchee = matchee.move(polyblanks.size)

#       until envp = unify?(env, remaining_pattern, remaining_matchee) # TODO: memoize?
#         remaining_matchee = remaining_matchee.move(1)
#         if remaining_matchee.empty?
#           envp = env
#           break
#         end
#       end

#       env = envp.not_nil!
#       gap = matchee.upto(remaining_matchee)

#       # No way an empty gap can match. We already know there is at least one
#       # polyblank and each polyblank matches one or more item.
#       return if gap.empty?

#       # Split polyblanks even further into chunks of the same type.
#       #
#       # Chunks of polyblanks with the same type will split their part of the gap
#       # into the corresponding number of pieces.
#       #
#       # We "cut" the gap at type edges to form the chunks. For instance, string-
#       # number edge (`xs_number+ ys_string+` or `xs_number+ ys_number+ zs_string+`),
#       # string - not string edge (e.g. `xs_string+ ys_+`), any - string edge
#       # (`xs_+ ys_string+`) and so on.
#       each_chunk_by_type(polyblanks) do |innercls, endcls, range|
#         n = 0
#         b = gap
#         e = gap.walk do |el|
#           next false unless el.type.subtype?(innercls)
#           next true unless endcls
#           next false if el.type.subtype?(endcls) && n > 0
#           n += 1
#           true
#         end
#         return if b == e
#         b.upto(e).split(range.size) do |part, i|
#           blank = polyblanks[range.begin + i]
#           next unless name = blank.name?
#           return unless env = env.unify?(name, part.collect)
#         end
#         gap = e
#       end

#       # Gap must have been entirely consumed by now. If not, there are terms that
#       # were not consumed by the polyblanks -- and that's an error, since all terms
#       # in the gap must belong to one of the polyblanks.
#       return unless gap.empty?

#       {env, remaining_pattern, remaining_matchee}
#     end

#     private def self.unify?(env, pattern : Items, term : Items, a : ITerm) : {Dict, Items, Items}?
#       dictstep(env, pattern, term, a)
#     end

#     # Default/fallback for dictionary matching: step through one pattern. Catches
#     # all patterns except for polyblanks.
#     private def self.dictstep(env, pattern : Items, term : Items, a : ITerm) : {Dict, Items, Items}?
#       return unless b = term.first?
#       return unless env = unify?(env, a.downcast, b.downcast)
#       {env, pattern.move(1), term.move(1)}
#     end

#     # Runs the pattern matching engine on *matchee*. The pattern matching engine
#     # attempts to unify *pattern* and *matchee*, returning a dictionary containing
#     # the necessary assignments. If no such dictionary can be found, returns `nil`.
#     #
#     # Both *pattern* and *matchee* are converted to terms if they're not ones already,
#     # using `Term.[]`.
#     #
#     # Main features supported by the pattern matching engine:
#     #
#     # - Literal match (e.g. `match?(100, 100)`, `match?(:x, :x)`, results in the assignment `{}`)
#     # - Single-value blank (e.g. `match?(:x_, 100)` results in the assignment `{ x: 100 }`)
#     # - Dictionary match (e.g. `match?({:foo, :x_, :y_}, {:foo, 100, 200})` results
#     #   in the assignment `{ x: 100, y: 200 }`)
#     # - Multi-value blank (referred to as *polyblank* in most of the code)
#     #   (e.g. `match?({:foo, :"xs_+"}, {:foo, 1, 2, 3})` results in the assignment `{ xs: {1, 2, 3} }`)
#     # - Blanks and polyblanks can be typechecked: `:x_number`, `:x_string`, `:"xs_string+"`
#     # - Bind special pattern: `match?({:"bind@", :x, 100}, 100)` results in the assignment `{ x: 100 }`.
#     # - N consecutive polyblanks can be used to split a list into N parts
#     #   (e.g. `match?({:"l_+", :"r_+"}, {1, 2, 3, 4})` results in the assignment `{ l: {1, 2}, r: {3, 4} }`.
#     def self.match?(pattern, matchee) : Dict?
#       unify?(Term[], Term[pattern], Term[matchee])
#     end
#   end
# end
