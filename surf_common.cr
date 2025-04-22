# Binarizes and simplifies nested/long `%all` *node*.
def all2(node) : Term
  Term.case(node) do
    matchpi %{(%'%all)} { M1::Normal::NORMAL_PASS }
    matchpi %{(%'%all a_)} { a }
    matchpi %{(%'%all a_ %'(%pass))} { a }
    matchpi %{(%'%all %'(%pass) b_)} { b }
    matchpi %{(%'%all _ _)} { Term.of(node) }
    matchpi %{(%'%all a_ b_ rest_+)} do
      a1 = Term.of(:"%all", a, b)
      b1 = rest.prepend(:"%all")

      all2(Term.of(:"%all", all2(a1), all2(b1)))
    end
  end
end

# *Pattern skeleton* is a restricted, more open subset of pattern matching constructs
# that we are able to index efficiently. Any M1 pattern can be converted into its skeleton
# with more or less loss.
#
# Pattern skeleton is guaranteed to consist only of the following nodes:
#
# - `(%'%value (%'%literal _) _)`
# - `(%'%any/source _+)`
# - `(%'%all a_ b_)`
# - `%'(%pass)`
# - `%'(%symbol)`
# - `%'(%string)`
# - `%'(%boolean)`
# - `%'(%dict)`
# - `%'(%number _)`
# - `(%'%literal X_)` with non-dict X
module ::Ww::M1::Skeleton
  extend self

  # Generates a sequence of *subject* itemseq calls repeated *n* times.
  private def repeated(prefix, key, subject, n, ahead0, rear) : Term
    if n.zero?
      return ahead0.call(prefix, key)
    end

    ahead1 = ->(prefix : Term::Dict, key : Term::Num) do
      repeated(prefix, key, subject, n - 1, ahead0, rear)
    end

    itemseq(prefix, key, subject, ahead1, rear)
  end

  # Returns the skeleton of an itemseq *item*.
  private def itemseq(prefix, key, item : Term, ahead, rear) : Term
    Term.of_case(item) do
      # Fetch successor.
      matchpi %{(%'%singular successor_)} do
        prefix = prefix.append({:"%value", {:"%literal", key}, pattern(successor)})

        ahead.call(prefix, key + 1)
      end

      # Expand small bounded %plural's into a disjunction with each possible length.
      matchpi(
        %{((%any %plural %plural/min %plural/max) (%optional untitled _) ¦
            min_: (%number 0 <= (whole _) <= 8)
            max_: (%number 1 <= (whole _) <= 8)
            type_symbol)}
      ) do
        case type.blank.type
        in .any?     then unit = M1::Normal::NORMAL_PASS
        in .number?  then unit = M1::Normal::NORMAL_BLANK_NUMBER
        in .string?  then unit = M1::Normal::NORMAL_BLANK_STRING
        in .symbol?  then unit = M1::Normal::NORMAL_BLANK_SYMBOL
        in .dict?    then unit = M1::Normal::NORMAL_BLANK_DICT
        in .boolean? then unit = M1::Normal::NORMAL_BLANK_BOOLEAN
        end

        Term::Dict.build do |disj|
          disj << :"%any/source"

          (min.to(Int32)..max.to(Int32)).each do |hi|
            variant = prefix

            hi.times do |length|
              variant = variant.append({:"%value", {:"%literal", key + length}, unit})
            end

            disj << ahead.call(variant, key + hi + 1)
          end
        end
      end

      # Dive into %group's.
      matchpi %{(%'%group _ children_+)} do
        itemseq(prefix, key, children.items, ahead, rear)
      end

      # Expand small bounded %past and %many's into a disjunction with each
      # possible length.
      matchpi(
        %{(%'%past children_+ ¦
            min_: (%number 0 <= (whole _) <= 8)
            max_: (%number 1 <= (whole _) <= 8)
            greedy_boolean: _)},
        %{(%'%many _ children_+ ¦
            min_: (%number 0 <= (whole _) <= 8)
            max_: (%number 1 <= (whole _) <= 8))}
      ) do
        Term::Dict.build do |disj|
          disj << :"%any/source"

          (min.to(Int32)..max.to(Int32)).each do |hi|
            disj << repeated(prefix, key, children.items, hi, ahead, rear)
          end
        end
      end

      # Optionals expand into a disjunction with and without the item.
      matchpi %{(%'%optional _ body_)} do
        variant0 = ahead.call(prefix, key)
        variant1 = ahead.call(prefix.append({:"%value", {:"%literal", key}, pattern(body)}), key + 1)

        Term.of(:"%any/source", variant0, variant1)
      end

      otherwise { rear.call(prefix) }
    end
  end

  # Returns the skeleton of an itemseq in *feed*.
  private def itemseq(prefix, key, feed : Term::Dict::ItemsView, ahead0, rear) : Term
    unless item = feed.first?
      return ahead0.call(prefix, key)
    end

    ahead1 = ->(prefix : Term::Dict, key : Term::Num) do
      itemseq(prefix, key, feed.move(1), ahead0, rear)
    end

    itemseq(prefix, key, item, ahead1, rear)
  end

  # Returns the skeleton of an itemseq *seq*.
  def itemseq(seq : Term::Dict)
    rear = ->(prefix : Term::Dict) { all2(prefix) }
    ahead = ->(prefix : Term::Dict, key : Term::Num) { rear.call(prefix) }

    itemseq(Term.dict(:"%all"), Term[0], seq.items, ahead, rear)
  end

  private def entry(prefix, key, value, ahead) : Term
    Term.of_case(value) do
      matchpi %{(%'%entry/required successor_)} do
        prefix = prefix.append({:"%value", {:"%literal", key}, pattern(successor)})

        ahead.call(prefix)
      end

      matchpi %{(%'%entry/optional _ successor_)} do
        variant0 = ahead.call(prefix)
        variant1 = ahead.call(prefix.append({:"%value", {:"%literal", key}, pattern(successor)}))

        {:"%any/source", variant0, variant1}
      end

      otherwise do
        ahead.call(prefix)
      end
    end
  end

  private def entries(prefix : Term::Dict, n, entries, ahead0) : Term
    unless entry = entries.nth?(n)
      return ahead0.call(prefix)
    end

    ahead1 = ->(prefix : Term::Dict) do
      entries(prefix, n + 1, entries, ahead0)
    end

    entry(prefix, *entry, ahead1)
  end

  # Returns the skeleton of *entries*.
  def entries(entries : Term::Dict) : Term
    if entries.empty?
      return Term.of({:"%dict"})
    end

    ahead = ->(prefix : Term::Dict) { all2(prefix) }

    entries(Term.dict(:"%all"), 0, entries, ahead)
  end

  # Returns the skeleton of a normal pattern *normp*.
  def pattern(normp : Term) : Term
    Term.of_case(normp) do
      matchpi %{(%'%pass)} { normp }
      matchpi %{(%'%symbol)} { normp }
      matchpi %{(%'%string)} { normp }
      matchpi %{%'(%number _)} { normp }
      matchpi %{(%'%boolean)} { normp }
      matchpi %{(%'%dict)} { normp }

      matchpi %{(%'%literal x_dict)} { pattern(M1.normal_escaped(x)) }
      matchpi %{(%'%literal _)} { normp }

      matchpi %{(%'%let _ successor_)} do
        pattern(successor)
      end

      matchpi %{(%'%itemseq successors_+)} do
        itemseq(successors.unsafe_as_d)
      end

      matchpi %{(%'%partition itemspart_ pairspart_)} do
        all2(Term.of(:"%all", pattern(itemspart), pattern(pairspart)))
      end

      # In pattern skeleton, all layers are always open. So we cannot make
      # literal belows closed. However we still account them during matching
      # for precision.
      begin
        # Do not emit useless %dict checks for below.
        matchpi %{(%'%layer (%'%literal ()) side_dict)} do
          entries(side.unsafe_as_d)
        end

        matchpi %{(%'%layer below_ side_dict)} do
          all2(Term.of(:"%all", pattern(below), entries(side.unsafe_as_d)))
        end
      end

      matchpi %{(%'%any/source successors_+)} do
        Term::Dict.build do |commit|
          commit << :"%any/source"

          successors.each_item_unordered do |item|
            commit << pattern(item)
          end
        end
      end

      matchpi %{(%'%any/literal options_+)} do
        Term::Dict.build do |commit|
          commit << :"%any/source"

          options.each_item_unordered do |item|
            commit << pattern(M1.normal_escaped(item))
          end
        end
      end

      matchpi %{(%'%all a_ b_)} do
        all2(Term.of(:"%all", pattern(a), pattern(b)))
      end

      matchpi(
        %{(%'%number %'(whole _))},
        %{(%'%number _ _ _)},
        %{(%'%number _ _ _ _ _)},
        %{(%'%pipe (%barrier (+ _number)) _)},
        %{(%'%pipe (%barrier (- _number)) _)},
        %{(%'%pipe (%barrier (* _number)) _)},
        %{(%'%pipe (%barrier (/ _number)) _)},
        %{(%'%pipe (%barrier (div _number)) _)},
        %{(%'%pipe (%barrier (mod _number)) _)},
        %{(%'%pipe (%barrier (** _number)) _)},
      ) { M1::Normal::NORMAL_BLANK_NUMBER }

      matchpi(
        %{(%'%pipe (map _) _)},
        %{(%'%pipe type _)},
      ) { M1::Normal::NORMAL_PASS }

      matchpi %{(%'%pipe span _)} { M1::Normal::NORMAL_BLANK_STRING }
      matchpi %{(%'%pipe tally _)} { M1::Normal::NORMAL_BLANK_DICT }

      matchpi %{(%'%symbol nonblank)}, %{(%'%symbol blank _ _)} do
        M1::Normal::NORMAL_BLANK_SYMBOL
      end

      matchpi %{(%'%terminal node_)} do
        pattern(node)
      end

      otherwise do
        M1::Normal::NORMAL_PASS
      end
    end
  end
end

module ::Ww::M1
  def self.skeleton(normp : Term)
    Skeleton.pattern(normp)
  end

  private def self.branches(skeleton : Term, ahead0 : Term ->) : Nil
    Term.case(skeleton) do
      matchpi %{(%'%value (%'%literal _) value_)} do
        ahead1 = ->(branch : Term) do
          ahead0.call(Term.of(skeleton.with(2, branch)))
        end

        branches(value, ahead1)
      end

      matchpi %{(%'%all a_ b_)} do
        ahead2 = ->(branch0 : Term) do
          ahead1 = ->(branch1 : Term) do
            ahead0.call(all2(Term.of(:"%all", branch0, branch1)))
          end

          branches(b, ahead1)
        end

        branches(a, ahead2)
      end

      matchpi %{(%'%any/source children_+)} do
        children.items.each do |child|
          branches(child, ahead0)
        end
      end

      otherwise do
        ahead0.call(skeleton)
      end
    end
  end

  # Normalizes pattern skeleton to DNF. Calls *sink* with each toplevel branch.
  #
  # As long as *skeleton* is a pattern skeleton, branches given to *sink* are guaranteed
  # to be pattern skeletons without `%any/source`.
  #
  # Non-skeleton nodes are unexpected and will not be processed.
  def self.branches(skeleton : Term, &sink : Term ->) : Nil
    branches(skeleton, sink)
  end

  private def self.strands(prefix : Term::Dict, branch : Term, sink) : Nil
    Term.case(branch) do
      matchpi %{(%'%pass)} { sink.call(prefix) }

      matchpi %{%'(%number _)}, %{%'(%string)}, %{%'(%symbol)}, %{%'(%boolean)}, %{%'(%dict)} do
        sink.call(prefix.append(branch))
      end

      matchpi %{(%'%literal _number)} do
        sink.call(prefix.append(M1::Normal::NORMAL_BLANK_NUMBER).append(branch))
      end

      matchpi %{(%'%literal _string)} do
        sink.call(prefix.append(M1::Normal::NORMAL_BLANK_STRING).append(branch))
      end

      matchpi %{(%'%literal _symbol)} do
        sink.call(prefix.append(M1::Normal::NORMAL_BLANK_SYMBOL).append(branch))
      end

      matchpi %{(%'%literal _boolean)} do
        sink.call(prefix.append(M1::Normal::NORMAL_BLANK_BOOLEAN).append(branch))
      end

      matchpi %{(%'%all a_ b_)} do
        strands(prefix, a, sink)
        strands(prefix, b, sink)
      end

      matchpi %{(%'%value (%'%literal _) successor_)} do
        prefix = prefix
          .append(M1::Normal::NORMAL_BLANK_DICT)
          .append(branch.without(2))

        strands(prefix, successor, sink)
      end
    end
  end

  # Calls *sink* with each strand (represented as an itemsonly dict) of *branch*.
  #
  # A strand is an exhaustive path through `%all` nodes in *branch*.
  #
  # Each strand consists of *bases*. The following list is an exhaustive list
  # of bases:
  #
  # - `(%'%value (%'%literal _))`
  # - `%'(%any)`
  # - `%'(%symbol)`
  # - `%'(%string)`
  # - `%'(%number _)`
  # - `%'(%boolean)`
  # - `%'(%dict)`
  # - `(%'%literal _)`
  def self.strands(branch : Term, &sink : Term::Dict ->)
    strands(Term.dict({:"%any"}), branch, sink)
  end
end

alias Fingerprint = Bytes

FINGERPRINT_BYTESIZE = 32
LABEL_BYTESIZE = 16

record Label, value : UInt128 do
  include Comparable(Label)

  class_getter zero = Label.new(0u128)

  def <=>(other : Label)
    value <=> other.value
  end

  # Returns the Big Endian encoding of the raw label value.
  def to_slice_be : Bytes
    slice = Bytes.new(value.byte_size)

    append_be(slice)

    slice
  end

  def append_be(dst : Bytes) : Bytes
    IO::ByteFormat::BigEndian.encode(value, dst)

    dst + Label.bytesize
  end

  def append_be(dst : Array(UInt8)) : Nil
    buffer = uninitialized UInt8[16]

    IO::ByteFormat::BigEndian.encode(value, buffer.to_slice)

    dst.concat(buffer)
  end

  def self.bytesize
    16
  end

  def self.from_slice_be?(bytes : Bytes) : Label?
    return unless bytes.size == Label.bytesize

    new(IO::ByteFormat::BigEndian.decode(UInt128, bytes))
  end

  def self.from_slice_be(bytes : Bytes) : Label
    from_slice_be?(bytes) || raise ArgumentError.new
  end

  def complete(digit, *, base, index)
    Label.new(value &+ (digit &* base**index))
  end

  def each_prefix_with_index(*, base, max, &)
    state = 0u128

    # NOTE: since we're using UUIDs (ish) as opposed to a counter, we don't
    # have the "long zeros prefix" problem where we store lots of zeros redundantly.
    # So we don't have to think about using a variable length encoding.
    (0...max).reverse_each do |index|
      digit = (value // (base ** index)) % base
      state &+= digit &* base**index
      yield Label.new(state), index.to_u8
    end
  end

  def inspect(io)
    io << "#'"
    value.to_s(io, base: 62, precision: 22)
  end

  def to_s(io)
    inspect(io)
  end

  # Reference: https://crypto.stackexchange.com/questions/109848/what-is-the-fastest-stable-128-bit-non-cryptographic-hash-function#comment235778_109848
  # Reference: https://crypto.stackexchange.com/questions/109848/what-is-the-fastest-stable-128-bit-non-cryptographic-hash-function#comment235789_109848
  def hash(hasher)
    a = (@value >> 64).to_u64
    b = (@value << 64 >> 64).to_u64

    (a.rotate_left(1) &+ b).hash(hasher)
  end
end

alias LabelGenerator = (-> Label) | ILabelGenerator

module ILabelGenerator
  abstract def call : Label
end

# An extremely simple globally unique id source.
#
# - The first 64 bits are used for nanoseconds since the Unix epoch.
# - The second 64 bits are randomness.
#
# Time comes first (most significant) to allow for better packing, since
# prefix digits are likely to be very similar if not exactly the same.
#
# An obvious problem is maliciously rolling the time back. However, we still
# have randomness to protect us in terms of uniqueness. The time component
# is used to check if one surface (e.g. a sensor) was added before another.
# Let's say if a sensor S sees an appearance A was added after S was inserted,
# then S won't report A and instead rely on A finding S. So if the time
# component of S or A is incorrect, S will see A when it shouldn't have --
# not a *huge* problem since A is matching S anyway, and has a lot of filtering
# to go through.
#
# TODO: use something more battle-tested
struct WWID
  extend ILabelGenerator

  def self.makes_sense?(label : Label)
    t_lo = (Time.utc - 1.year).to_unix_ns.to_u128
    t_hi = (Time.utc + 1.day).to_unix_ns.to_u128

    t = label.value >> 64
    t_lo <= t <= t_hi
  end

  def self.call : Label
    order = Time.utc.to_unix_ns.to_u128
    randomness = Random::Secure.rand(UInt64)

    Label.new((order << 64) | randomness)
  end
end

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
  alias Any = Begin | End | At | IsSym | IsStr | IsNum | IsBool | IsDict | Literal

  # Passes a dictionary term's value for *key* forward.
  record At, key : Term do
    # FIXME: rename key to term
    def term : Term
      key
    end
  end

  # Anchor put at the beginning of all strands.
  record Begin

  # Indicates an abrupt (non-literal) stop. This base is not emitted if the strand
  # ends with `Literal`.
  record End

  # Passes only symbol terms forward.
  record IsSym do
    def self.code
      Bytes[0]
    end
  end

  # Passes only string terms forward.
  record IsStr do
    def self.code
      Bytes[1]
    end
  end

  # Passes only number terms forward.
  record IsNum do
    def self.code
      Bytes[2]
    end
  end

  # Passes only boolean terms forward.
  record IsBool do
    def self.code
      Bytes[3]
    end
  end

  # Passes only dictionary terms forward.
  record IsDict do
    def self.code
      Bytes[4]
    end
  end

  # Passes foward only terms that match *value* exactly.
  record Literal, value : Term do
    # FIXME: rename value to term
    def term : Term
      value
    end
  end

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

  def self.update(digest, base : End)
    digest.update(Bytes[0])
  end


  def self.update(digest, base : Begin)
    digest.update(Bytes[1])
  end

  def self.update(digest, base : IsSym)
    digest.update(Bytes[2])
  end

  def self.update(digest, base : IsStr)
    digest.update(Bytes[3])
  end

  def self.update(digest, base : IsNum)
    digest.update(Bytes[4])
  end

  def self.update(digest, base : IsBool)
    digest.update(Bytes[5])
  end

  def self.update(digest, base : IsDict)
    digest.update(Bytes[6])
  end

  # TODO: optimize
  def self.update(digest, base : Literal)
    digest.update(Bytes[7])
    digest.update(ML.compact(base.value))
  end

  # TODO: optimize
  def self.update(digest, base : At)
    digest.update(Bytes[8])
    digest.update(ML.compact(base.key))
  end
end

