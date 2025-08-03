module Ww::Soma::Microfold::Pass
  # :nodoc:
  module Designate
    extend self

    record Cmd, rank : Int32, box : Term, key : Term, value : Term?

    def designate(root : Term) : Term
      Pass.mapwalk(root) do |node|
        Term.of_case(node) do
          matchpi %[{¦ µ-preset: preset_dict µ-style: style_dict}] do
            commands = [] of {Int32, Cmd}

            each_cmd(preset.unsafe_as_d) { |command| commands << {0, command} }
            each_cmd(style.unsafe_as_d) { |command| commands << {1, command} }

            designations = Term[]

            commands.sort_by! { |ord, cmd| {ord, cmd.rank} }
            commands.each do |_, command|
              designations = designations.morph({command.box, command.key, command.value})
            end

            # Force flow-box if the number of children exceeds 1. We're saying `2`
            # here because there is also the tag.
            if node.itemsize > 2
              designations = designations.morph({:"flow-box", designations[:"flow-box"]? || Term[]})
            end

            node.morph(
              {:"µ-preset", nil},
              {:"µ-style", nil},
              {:"µ-designations", designations},
            )
          end

          otherwise { node }
        end
      end
    end

    private def each_cmd(mixins : Term::Dict, &) : Nil
      mixins.items.each do |mixin|
        Term.case(mixin) do
          matchpi %[(mixin ⍊ box_ rank_: (%number +i32) plus⋮ {} minus⋮ {}))] do
            minus.each_entry do |key, _|
              yield Cmd.new(rank.to(Int32), box, key, nil)
            end

            plus.each_entry do |key, value|
              yield Cmd.new(rank.to(Int32), box, key, value)
            end
          end

          otherwise { }
        end
      end
    end
  end

  # Performs the designation pass on *root*.
  #
  # During the designation pass, `µ-preset` and `µ-style` holding box-targeting
  # mixins are converted to `µ-designations`, a dict mapping boxes to their fully
  # computed "styles" (i.e. full and properly ordered "mix" from mixins in
  # the node's preset and style).
  def designate(root : Term) : Term
    Designate.designate(root)
  end
end
