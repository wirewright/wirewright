module Ww::Meridium
  # Ubases are tiny gate-keeper nodes for `Utrie` and the internal term trie
  # created by `AppearanceRegistry`.
  #
  # Arbitrary M1 patterns are broken down into `BranchList` (so DNF, which has
  # terrible scaling characteristics but still works!) Each branch in the branch
  # list is a `StrandList` (so a conjunction of strands; we're DNF, remember?)
  # A strand is a sequence of Ubases that create, in effect, a "chain of filters".
  # Each Ubase, then, is such a filter. E.g. `IsNum` filters number terms; `Literal`
  # filters literal matches. The `At` Ubase, on the other hand, is interesting
  # because its output is different from its input.
  #
  # See `Utrie` to learn more.
  module Ubase
    alias Any = Begin | End | At | IsSym | IsStr | IsNum | IsBool | IsDict | Literal

    # Passes a dictionary term's value for key *term* forward.
    record At, term : Term

    # Anchor put at the beginning of all strands.
    record Begin

    # Indicates an abrupt (non-literal) stop. This base is not emitted if the strand
    # ends with `Literal`.
    record End

    # Passes only symbol terms forward.
    record IsSym

    # Passes only string terms forward.
    record IsStr

    # Passes only number terms forward.
    record IsNum

    # Passes only boolean terms forward.
    record IsBool

    # Passes only dictionary terms forward.
    record IsDict

    # Passes foward only terms that match *term* exactly.
    record Literal, term : Term
  end

  # A chain of ubases is called a *strand*, and is represented simply by an array
  # of ubases.
  alias Strand = Array(Ubase::Any)
  alias StrandList = Array(Strand)
  alias BranchList = Array(StrandList)

  # :nodoc:
  enum Uopcode : UInt8
    Begin
    End
    IsSym
    IsStr
    IsNum
    IsBool
    IsDict
    HashedAt
    QuotedAt
    HashedLiteral
    QuotedLiteral
  end

  module Ubase
    extend self

    # Converts *keypath* and *leaf* (produced by e.g. `Term.each_keypath_and_leaf`)
    # to a strand of ubases.
    def strand(keypath : Array(Term), leaf : Term) : Strand
      strand = [] of Ubase::Any
      strand << Ubase::Begin.new

      keypath.each do |key|
        strand << Ubase::IsDict.new
        strand << Ubase::At.new(key)
      end

      case leaf.type
      in .any?     then unreachable
      in .symbol?  then strand << Ubase::IsSym.new
      in .string?  then strand << Ubase::IsStr.new
      in .number?  then strand << Ubase::IsNum.new
      in .boolean? then strand << Ubase::IsBool.new
      in .dict?    then strand << Ubase::IsDict.new
      end

      strand << Ubase::Literal.new(leaf)
      strand
    end

    # :nodoc:
    def upack(io, ubase : Ubase::Begin) : Nil
      io.write_byte(Uopcode::Begin.value)
    end

    # :nodoc:
    def upack(io, ubase : Ubase::End) : Nil
    end

    {% for base in %w[IsSym IsStr IsNum IsBool IsDict] %}
      # :nodoc:
      def upack(io, ubase : Ubase::{{base.id}}) : Nil
        io.write_byte(Uopcode::{{base.id}}.value)
      end
    {% end %}

    {% for base in %w[At Literal] %}
      # :nodoc:
      def upack(io, ubase : Ubase::{{base.id}}) : Nil
        bytesize = ML.compact_bytesize(ubase.term)

        # Do not hash if the hash would be longer than raw value.
        if bytesize <= Atom::BYTESIZE - 1
          io.write_byte(Uopcode::Quoted{{base.id}}.value)
          io.write_byte(bytesize.to_u8)
          ML.compact(io, ubase.term)
          return
        end

        io.write_byte(Uopcode::Hashed{{base.id}}.value)

        hasher = Atom::Hasher.new
        scratch = uninitialized UInt8[Atom::BYTESIZE]
        updater = IO::ByteStream.new { |slice| hasher.update(slice) }

        ML.compact(updater, ubase.term)

        hasher.final(scratch.to_slice)

        io.write(scratch.to_slice)
      end
    {% end %}

    {% if flag?(:docs) %}
      # Appends the **lossily** packed, encoded version of *ubase* to *io*.
      def upack(io, ubase : Ubase::Any) : Nil
      end
    {% end %}

    # Returns a byteslice for the **lossily** packed, encoded version of *strand*.
    def upack(strand : Strand) : Bytes
      io = IO::Memory.new
      strand.each do |base|
        upack(io, base)
      end
      io.to_slice
    end

    {% for base in %w[Begin End IsSym IsStr IsNum IsBool IsDict] %}
      # :nodoc:
      def update(hasher, ubase : Ubase::{{base.id}}) : Nil
        scratch = uninitialized UInt8[1]
        scratch[0] = Uopcode::{{base.id}}.value

        hasher.update(scratch.to_slice)
      end
    {% end %}

    {% for base in %w[At Literal] %}
      # :nodoc:
      def update(hasher, ubase : Ubase::{{base.id}}) : Nil
        scratch = uninitialized UInt8[1]
        scratch[0] = Uopcode::Hashed{{base.id}}.value
        hasher.update(scratch.to_slice)

        digester = IO::ByteStream.new { |slice| hasher.update(slice) }

        ML.compact(digester, ubase.term)
      end
    {% end %}

    {% if flag?(:docs) %}
      # Updates the hasher (e.g. `Blake3`) pointed to by *hasher* with *ubase*.
      def update(hasher : Pointer, ubase : Ubase::Any) : Nil
      end
    {% end %}
  end
end
