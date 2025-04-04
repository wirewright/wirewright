module Ww::Meridium
  alias Strand = Slice(Ubase::Any)
  alias StrandList = Slice(Strand)
  alias BranchList = Slice(StrandList)

  # Ubases are tiny gate-keeper nodes for `Utrie`.
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
    alias Any = At | Trunk | IsSym | IsStr | IsNum | IsBool | IsDict | Literal

    # Passes a dictionary term's value for *key* forward.
    record At, key : Term

    # Positioned at the beginning of all valid strands. Relied upon by
    # match-any patterns such as `_` or `x_`, since they aren't matching
    # anything in particular (so `Trunk` is a NOP in terms of filtering
    # or transformation).
    record Trunk

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

    # Passes foward only terms that match *value* exactly.
    record Literal, value : Term

    # Returns the is-type `Ubase` (e.g. `IsNum`) that corresponds to the given
    # `TermType` *type*.
    #
    # Raises `ArgumentError` if *type* is `TermType::Any`.
    def self.from(type : TermType) : Ubase::Any
      case type
      in .any?     then raise ArgumentError.new
      in .boolean? then IsBool.new
      in .number?  then IsNum.new
      in .string?  then IsStr.new
      in .symbol?  then IsSym.new
      in .dict?    then IsDict.new
      end
    end
  end

  # Encode to / decode from terms

  struct ::Ww::Term
    # WARNING: both encode and decode MUST be compatible with M1's normal form,
    # since we're feeding the normal form directly to `decode` to obtain
    # the corresponding Ubases occasionally.

    def self.encode(src : Ubase::At) : Term
      Term.of(:"%value", {:"%literal", src.key})
    end

    def self.decode?(dst : Ubase::At.class, term : Term) : Ubase::At?
      Term.matchpi?(term, %{(%'%value (%'%literal key_))}) do
        Ubase::At.new(key)
      end
    end

    # :nodoc:
    #
    # NOTE: To retain compatibility with M1's normal form we encode/decode
    # Trunk as %any.
    ENCODED_TRUNK = Term.of({:"%any"})

    # :nodoc:
    ENCODED_IS_SYM = Term.of({:"%symbol"})

    # :nodoc:
    ENCODED_IS_NUM = Term.of({:"%number", :_})

    # :nodoc:
    ENCODED_IS_STR = Term.of({:"%string"})

    # :nodoc:
    ENCODED_IS_DICT = Term.of({:"%dict"})

    # :nodoc:
    ENCODED_IS_BOOL = Term.of({:"%boolean"})

    {% for base in %w(Trunk IsSym IsNum IsStr IsDict IsBool) %}
      def self.encode(src : Ubase::{{base.id}}) : Term
        ENCODED_{{base.underscore.upcase.id}}
      end

      def self.decode?(dst : Ubase::{{base.id}}.class, term : Term) : Ubase::{{base.id}}?
        if term == ENCODED_{{base.underscore.upcase.id}}
          return Ubase::{{base.id}}.new
        end
      end
    {% end %}

    def self.encode(src : Ubase::Literal) : Term
      Term.of(:"%literal", src.value)
    end

    def self.decode?(dst : Ubase::Literal.class, term : Term) : Ubase::Literal?
      Term.matchpi?(term, %{(%'%literal value_)}) do
        Ubase::Literal.new(value)
      end
    end
  end
end

