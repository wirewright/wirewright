module Ww::ML
  module Tree
    extend self

    alias Expr = ExprNode | Location(ExprNode)
    alias ExprNode = Leaf |
                     Stitch |
                     Itemspattern |
                     Pairspattern |
                     EntriesPattern |
                     SameValueSet |
                     Multiset |
                     Keypool |
                     AllItem |
                     AllLeaf |
                     PairDict |
                     Split |
                     Dict |
                     DictExtend |
                     Sigil |
                     Placeholder |
                     PatternLet |
                     LayerIgnoreResidue |
                     Tuck

    # A special node that marks the location of some child T, usually something
    # like `Expr`. The location itself is encoded in *text*, which is a view of
    # the original source from which the tree was produced by the reader.
    #
    # `T` should not include `Location(T)`; that would make little sense. Hence
    # you'll see unions such as `Expr` (`ExprNode` or located `ExprNode`)
    # vs. `ExprNode`.
    defcase Location(T), child : T, text : StringView do
      def pretty_print(pp)
        pp.surround("Location(", ")", left_break: "", right_break: nil) do
          {% for ivar, i in @type.instance_vars.map(&.name).sort %}
            {% if i > 0 %}
              pp.comma
            {% end %}

            pp.group do
              pp.text "@{{ivar.id}}="
              pp.nest do
                pp.breakable ""
                @{{ivar.id}}.pretty_print(pp)
              end
            end
          {% end %}
        end
      end
    end

    defcase Leaf, term : Term
    defcase Stitch, segments : Array(Expr)
    defcase Itemspattern, items : Array(Expr)
    defcase SameValueSet, items : Array(Expr), value : Term
    defcase Multiset, entries : Array(MultisetEntry)

    alias MultisetEntry = MultisetEntryOne | MultisetEntryN

    defcase MultisetEntryOne, item : Expr
    defcase MultisetEntryN, count : Expr, item : Expr

    defcase Keypool, items : Array(Expr)

    defcase AllItem, items : Array(Expr), pairside : Layer?, source : Bool
    defcase AllLeaf, items : Array(Expr), pairside : Layer?, source : Bool

    defcase PairDict, pairs : Array(DictPair)

    alias Sigil = Edge |
                  Up |
                  Dn |
                  My |
                  AlloyExpr |
                  AlloySpliceExpr |
                  Literal |
                  Dollar |
                  DollarOnce |
                  PatternLiteral |
                  PatternNonself |
                  PatternSlot |
                  PatternNumber |
                  Positive |
                  Negative

    defcase Edge, arg : Expr
    defcase Up, arg : Expr
    defcase Dn, arg : Expr
    defcase My, arg : Expr
    defcase AlloyExpr, arg : Expr
    defcase AlloySpliceExpr, arg : Expr
    defcase Literal, arg : Expr
    defcase Dollar, arg : Expr
    defcase DollarOnce, arg : Expr
    defcase Positive, arg : Expr
    defcase Negative, arg : Expr

    defcase PatternLet, left : Expr, right : Expr
    defcase PatternLiteral, arg : Expr
    defcase PatternNonself, arg : Expr
    defcase PatternSlot, arg : Expr
    defcase PatternNumber, arg : Expr

    alias Selector = SelectorNode | Location(SelectorNode)
    alias SelectorNode = SelectorKey |
                         SelectorMaybeKey |
                         SelectorLetTrue |
                         SelectorLetFalse |
                         SelectorPairDefault |
                         SelectorPairRequired |
                         SelectorNegative |
                         SelectorNumber |
                         SelectorEdge

    defcase SelectorKey, key : Expr
    defcase SelectorMaybeKey, key : Expr
    defcase SelectorLetTrue, key : Expr
    defcase SelectorLetFalse, key : Expr
    defcase SelectorPairDefault, key : Expr, value : Expr
    defcase SelectorPairRequired, key : Expr, value : Expr
    defcase SelectorNegative, key : Expr, capture : Expr?
    defcase SelectorNumber, key : Expr
    defcase SelectorEdge, key : Expr

    defcase Pairspattern, itemsname : Expr?, selection : Array(Selector)

    defcase EntriesPattern, selection : Array(Selector)

    defcase Layer, residue : Expr?, selection : Array(Selector)
    defcase LayerIgnoreResidue, child : Expr

    defcase Split, parts : Array(SplitPart), pairside : Layer?, source : Bool

    alias SplitPart = SplitPartNode | Location(SplitPartNode)

    defcase SplitPartNode, items : Array(Expr)

    alias ToggleableEntry = DisabledEntry | FocusedEntry | EnabledEntry

    defcase DisabledEntry, entry : DictEntry
    defcase FocusedEntry, entry : DictEntry
    defcase EnabledEntry, entry : DictEntry

    alias DictEntry = DictEntryNode | Location(DictEntryNode)
    alias DictEntryNode = DictItem | DictPair | DictRule

    alias DictRule = TemplateRule | BackmapRule

    defcase TemplateRule, pattern : Expr, template : Expr, comments : Array(StringView), caches_hash: true
    defcase BackmapRule, pattern : Expr, backspec : Expr, comments : Array(StringView), caches_hash: true

    alias DictPair = DictAlloyPair | DictEdgePair | DictKVPair

    defcase DictItem, item : Expr
    defcase DictKVPair, key : Expr, value : Expr
    defcase DictAlloyPair, key : Expr
    defcase DictEdgePair, key : Expr

    defcase DictEntryBlock, entries : Array(ToggleableEntry), caches_hash: true
    defcase DictSection, blocks : Array(DictEntryBlock)

    defcase Dict, itemside : DictSection, pairside : Layer?
    defcase DictExtend, itemside : DictSection, extra : Expr

    defcase DocumentDict, default : DictSection, sections : Array(DocumentSection)
    defcase DocumentSection, name : Expr, body : DictSection, text : StringView

    alias Placeholder = RuleId | RuleBlockId | RuleIdBlank | RuleBlockIdBlank

    defcase RuleId, equality: :ref
    defcase RuleIdBlank, equality: :ref
    defcase RuleBlockId, equality: :ref
    defcase RuleBlockIdBlank, equality: :ref

    alias PlaceholderContainer = DictRule | DictEntryBlock

    defcase Tuck, offsets : Array(Int32), arg : Expr

    alias HasLocation = Expr | Selector | DictEntry | SplitPart

    # :nodoc:
    def location(node : Location, text : StringView)
      node
    end

    {% for type in [ExprNode, SelectorNode, DictEntryNode, SplitPartNode] %}
      # :nodoc:
      def location(node : {{type}}, text : StringView)
        Location({{type}}).new(node, text)
      end
    {% end %}

    {% if flag?(:docs) %}
      # Wraps *node* in a `Location` node, if it is one of the `HasLocationNode`s.
      # Otherwise, returns *node* unchanged.
      def location(node, text : StringView)
      end
    {% end %}

    # :nodoc:
    def topmost?(node : Location, *, as cls : T.class) : T? forall T
      node.child.as?(T)
    end

    # Equivalent to `node.as?(T)`, except skips a wrapping `Location` node,
    # if any.
    def topmost?(node, *, as cls : T.class) : T? forall T
      node.as?(T)
    end

    # :nodoc:
    def topmost(node : Location, *, as cls : T.class) : T forall T
      node.child.as(T)
    end

    # Equivalent to `node.as(T)`, except skips a wrapping `Location` node,
    # if any.
    def topmost(node, *, as cls : T.class) : T forall T
      node.as(T)
    end
  end
end

require "./tree/hashrepr"
