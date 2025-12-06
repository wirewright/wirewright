module Ww::Microfold::Pass
  # Performs the lookabove pass on *root*.
  #
  # During the lookabove pass, Microfold resolves `items-*` mixins by making
  # each node look above at its nearest Microfold-administered ancestor;
  # collecting and collect `items-*` mixins from it, if any. The mixins are
  # appended to `µ-style` with the rank `Rank::Item`.
  def lookabove(root root0 : Term) : Term
    root1 = root0

    Term.each_keypath_and_itemnode(root0) do |keypath, node0|
      Term.matchpi?(node0, %[{¦ µ-style: style0_}]) do
        style1 = style0.transaction do |commit|
          each_item_mixin(root0, keypath) do |mixin|
            commit << mixin
          end
        end

        next if style0 == style1

        node1 = node0.as_d(&.morph({:"µ-style", style1}))
        root1 = root1.as_d(&.follow(keypath) { node1 })
      end

      true # descend
    end

    root1
  end

  private def each_item_mixin(root, keypath, & : Term ->)
    ancestors = Term.ancestors(root, keypath)
    ancestors.reverse_each do |ancestor|
      Term.matchpi?(ancestor, %[{¦ µ-preset: preset_ µ-style: style_}]) do
        {preset, style}.each do |up|
          up.items.each do |item|
            Term.matchpi?(item, %[(items mixins_dict+)]) do
              mixins.items.each do |mixin|
                yield mixin.as_d(&.with(:rank, Rank::Item))
              end
            end
          end
        end

        return
      end
    end
  end

  # Performs the cascade pass on *root*.
  #
  # During the cascade pass, each node is given an opportunity to look
  # at its ancestors. It can then inherit mixins targeting boxes that have
  # opted into cascade. Such inheritance happens in the order of eldest-to-
  # youngest ancestor; thus, ancestors lower down the tree have the ability
  # to override mixins of those above them.
  #
  # Mixins inherited during the cascade pass are marked with the rank `Rank::Cascade`.
  #
  # Whereas we often imagine "cascade" to be top-down, here it is implemented
  # in a bottom-up manner.
  def cascade(root root0 : Term, theme : Theme) : Term
    root1 = root0

    Term.each_keypath_and_itemnode(root0) do |keypath, node0|
      Term.matchpi?(node0, %[{¦ µ-style: style0_}]) do
        style1 = style0.transaction do |commit|
          each_cascaded_mixin(theme, root0, keypath) do |mixin|
            commit << mixin
          end
        end

        unless style0 == style1
          node1 = Term.of(node0.morph({:"µ-style", style1}))
          root1 = root1.as_d(&.follow(keypath) { node1 })
        end
      end

      true # descend
    end

    root1
  end

  private def each_cascaded_mixin(theme : Theme, root : Term, keypath : Array(Term), & : Term ->) : Nil
    ancestors = Term.ancestors(root, keypath)

    {% for source in %w[preset style] %}
      ancestors.each do |ancestor|
        Term.matchpi?(ancestor, %[{¦ µ-{{source.id}}: mixins_dict}]) do
          mixins.items.each do |mixin|
            Term.matchpi?(mixin, %[(mixin ⍊ box_)]) do
              next unless theme.cascade?(box)

              yield mixin.as_d(&.with(:rank, Rank::Cascade))
            end
          end
        end
      end
    {% end %}
  end
end
