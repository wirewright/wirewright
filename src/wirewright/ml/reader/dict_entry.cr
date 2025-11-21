struct Ww::ML::Reader
  # :nodoc:
  #
  # IR for dict entries.
  alias DictEntry = DictRule | DictItem | DictPair

  # :nodoc:
  record DictItem,
    term : Term,
    termsrc : StringView,
    state : Symbol

  # :nodoc:
  record DictPair,
    key : Term,
    keysrc : StringView,
    value : Term,
    valuesrc : StringView,
    state : Symbol

  # :nodoc:
  defcase DictRule, term : Term, termsrc : StringView, doc : Term?, state : Symbol do
    # Returns the 64-bit rule hash of this rule.
    getter rhash64 : UInt64 do
      hasher = Digest::Blake3.new

      to_hash_s(IO::ByteStream.new { |slice| hasher.update(slice) }, term)

      scratch = uninitialized UInt8[32]
      hasher.final(scratch.to_slice)

      IO::ByteFormat::BigEndian.decode(UInt64, scratch.to_slice)
    end

    # Returns the id of this rule (i.e. `◇`).
    getter id : Term::Sym do
      Term::Sym.new(Alpha48.encode(rhash64))
    end

    # Returns the id blank of this rule (i.e. `◇_`).
    getter id_blank : Term::Sym do
      Term::Sym.new("#{Alpha48.encode(rhash64)}_")
    end

    # Returns a *patched* copy of this rule.
    #
    # *Patching* refers to the process of replacing `◇` and `◇_` (rule
    # id ideographs) with the actual rule id, derived from the rule's hash.
    def patched : DictRule
      term1 = Term.patch(@term) do |leaf|
        unless symbol = leaf.as_sym?
          next Term::Patch::Skip.new
        end

        case symbol
        when .rule_id_blank_sentinel?
          Term::Patch::ReplaceSkip.new(Term.of(id_blank))
        when .rule_id_sentinel?
          Term::Patch::ReplaceSkip.new(Term.of(id))
        else
          Term::Patch::Skip.new
        end
      end

      copy_with(term: term1)
    end

    private def to_hash_s(io, term : Term) : Nil
      to_hash_s(io, Term[term])
    end

    private def to_hash_s(io, term : ITerm) : Nil
      ML.compact(io, term)
    end

    private def to_hash_s(io, term : Term::Sym) : Nil
      if id = term.rule_id_sentinel? ||
              term.rule_id_blank_sentinel? ||
              term.rule_block_id_sentinel? ||
              term.rule_block_id_blank_sentinel?
        io << id.name
        return
      end

      ML.compact(io, term)
    end

    private def to_hash_s(io, term : Term::Dict) : Nil
      io << '('

      term.ee(ordered: true).join(io, ' ') do |(k, v)|
        if (i = k.to?(Int32)) && i < term.itemsize
          to_hash_s(io, v)
        else
          to_hash_s(io, k)
          io << ": "
          to_hash_s(io, v)
        end
      end

      io << ')'
    end
  end

  # :nodoc:
  defcase DictEntryBlock, entries : Array(DictEntry) do
    # Returns the 64-bit rule hash of this entry block.
    getter rhash64 : UInt64 do
      rule_hashes = [] of UInt64

      entries.each do |entry|
        case entry
        in DictRule then rule_hashes << entry.rhash64
        in DictEntry
        end
      end

      scratch = uninitialized UInt8[32]
      hasher = Digest::Blake3.new

      rule_hashes.sort!
      rule_hashes.each do |rule_hash|
        IO::ByteFormat::BigEndian.encode(rule_hash, scratch.to_slice)

        hasher.update(scratch.to_slice[0, 8])
      end

      hasher.final(scratch.to_slice)

      IO::ByteFormat::BigEndian.decode(UInt64, scratch.to_slice)
    end

    # Returns the id of this entry block (i.e. `▢`).
    getter id : Term::Sym do
      Term::Sym.new(Alpha48.encode(rhash64))
    end

    # Returns the id blank of this entry block (i.e. `▢_`).
    getter id_blank : Term::Sym do
      Term::Sym.new("#{Alpha48.encode(rhash64)}_")
    end

    # Returns a *patched* copy of this entry block.
    #
    # *Patching* refers to the process of replacing `▢` and `▢_` in rules
    # (rule block id ideographs) with the actual rule block id, derived
    # from the rule block's hash.
    def patched : DictEntryBlock
      entries1 = entries.map do |entry|
        case entry
        in DictRule
          term1 = Term.patch(entry.term) do |leaf|
            unless symbol = leaf.as_sym?
              next Term::Patch::Skip.new
            end

            case symbol
            when .rule_block_id_sentinel?
              Term::Patch::ReplaceSkip.new(Term.of(id))
            when .rule_block_id_blank_sentinel?
              Term::Patch::ReplaceSkip.new(Term.of(id_blank))
            else
              Term::Patch::Skip.new
            end
          end

          entry.copy_with(term: term1)
        in DictEntry
          entry # Leave as-is
        end
      end

      DictEntryBlock.new(entries1)
    end
  end
end
