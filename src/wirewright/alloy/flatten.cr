module Ww::Alloy
  # Base dictionary rewriting.
  private def flatten(dict0 : Term::Dict, issues : Issue::Sink, & : Term, Issue::Sink -> _) : Expansion
    indicesbuf = uninitialized Int32[64]
    expansionsbuf = uninitialized Expansion[64]
    indices = stack_alloc Pf::Kit::HybridArray(Int32, 64).new(indicesbuf.to_unsafe)
    expansions = stack_alloc Pf::Kit::HybridArray(Expansion, 64).new(expansionsbuf.to_unsafe)

    rebuild = false

    # Process items.
    dict0.items.each_with_index do |item, index|
      issues.adjoin(key: index, detail: "item") do |issues|
        expansion = yield item, issues

        # Common case: no change at all.
        next if expansion.is_a?(Assign) && item == expansion.term

        # Let them be careless.
        if expansion.is_a?(Splice) && expansion.offspring.size == 1
          expansion = Assign.new(expansion.offspring[0])
        end

        indices << index
        expansions << expansion
        rebuild ||= expansion.is_a?(Err) || expansion.is_a?(Splice)
      end
    end

    dict1 = dict0

    unless dict0.itemsonly?
      # Process pairs.
      dict1 = dict0.transaction do |commit|
        dict0.each_pair do |key, value|
          issues.adjoin(key: key, detail: "pair with key") do |issues|
            case expansion = yield value, issues
            in Err
              commit.without(key) # omit
            in Assign
              commit.with(key, expansion.term)
            in Splice
              # We're in a pair, as in:
              #
              #   x: (^* (1 2 3))
              #
              # There are only two possible states for a pair if it is treated like
              # a container:
              #
              #   zero terms -- as in an empty splice or an error
              #   one term   -- as in Assign
              #
              # A splice with more than one term does not fit in a pair -- the extra terms
              # have nowhere to go. We handle this by wrapping such cases in `()`,
              # but, unfortunately, just like at the top-level, this generates a nasty,
              # unpredictable interface; not something as clean as zero/one/many.
              # Anything else would be worse, though; we do normalize e.g. in `^render`,
              # but here, there'd be no easy way to extract vs `^render` (besides, all
              # Alloy templates written so far would be broken!) I am therefore in favor
              # of this behavior as it is an OK compromise between purity and practice.
              case expansion.offspring.size
              when 0 then commit.without(key)
              when 1 then commit.with(key, expansion.offspring[0])
              else
                commit.with(key, expansion.offspring)
              end
            end
          end
        end
      end
    end

    # dict1 : old itemspart, new pairspart

    if rebuild
      dict1 = dict1.pairspart.transaction do |commit|
        (0...dict0.itemsize).segments(indices) do |segment, index|
          unless index
            commit.concat(segment) { |index| dict0[index] }
            next
          end

          case expansion = expansions[index]
          in Err # omit
          in Assign then commit << expansion.term
          in Splice then commit.concat(expansion.offspring.items)
          end
        end
      end
    else
      dict1 = dict1.transaction do |commit|
        expansions.zip(indices) do |expansion, index|
          assert expansion.is_a?(Assign)

          commit.with(index, expansion.term)
        end
      end
    end

    Assign.new(Term.of(dict1))
  end

  # Base dictionary rewriting. Passthrough for non-dictionary terms.
  private def flatten(term : Term, issues : Issue::Sink, & : Term, Issue::Sink -> _) : Expansion
    unless term.type.dict?
      return Assign.new(term)
    end

    flatten(term.as_d, issues) { |value, issues| yield value, issues }
  end
end
