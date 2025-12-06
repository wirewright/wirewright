module Ww::Microfold::Pass
  # :nodoc:
  module Cull
    # Returns `true` if the `present`/`absent` pseudo-utilities in *node*'s style
    # resolve to presence. Returns `false` otherwise.
    def self.present?(node : Term) : Bool
      present = true

      Term.case(node) do
        matchpi %[{¦ µ-preset: preset←(_*)}] do
          preset.items.each do |utility|
            case utility
            when Term.of(:present) then present = true
            when Term.of(:absent)  then present = false
            end
          end

          continue
        end

        matchpi %[{¦ µ-style: style←(_*)}] do
          style.items.each do |utility|
            case utility
            when Term.of(:present) then present = true
            when Term.of(:absent)  then present = false
            end
          end

          continue
        end

        otherwise { }
      end

      present
    end
  end

  # Performs the culling pass.
  #
  # During the culling pass, Microfold evaluates the `present` and `absent`
  # pseudo-utilities and eliminates nodes that resolve to absence.
  #
  # On those nodes that evaluate to presence, the pseudo-utilities are
  # not removed; they will simply be ignored by the latter passes.
  def cull(node : Term) : Term
    node.as_d do |nodedict|
      next nodedict unless nodedict.itemsize >= 1

      nodedict.pairspart.transaction do |commit|
        tag = nodedict.items.first
        commit << tag

        children = nodedict.items.move(1)
        children.each do |child|
          next unless Cull.present?(child)
          commit << cull(child)
        end
      end
    end
  end
end
