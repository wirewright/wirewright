module Ww::Soma::Microfold::Pass
  # Performs the specialization pass on *root*.
  #
  # Resolves dynamic style references, enabling context-aware styling based
  # on node attributes similar to Tailwind's utility class customization.
  #
  # Reports any issues that arise during the pass to *issues*.
  def specialize(root : Term, issues : Issue::Sink) : Term
    mapwalk_preset_and_style(root, issues) do |_, pairs, style, issues|
      next style unless style.type.string?

      specialize(pairs, style.to(StringView), issues)
    end
  end

  private def specialize(pairs, style0, issues) : StringView
    style0.reader do |r|
      prefix = Rtk.view(r) { Rtk.skip_to(r, '{') }
      if Rtk.at_end?(r)
        return style0
      end

      style1 = String.build do |io|
        io << prefix

        specialize(pairs, r, io, issues)
      end

      style1.view
    end
  end

  private def specialize(pairs, r, io, issues) : Nil
    loop do
      io << Rtk.view(r) { Rtk.skip_to(r, '{') }
      break unless Rtk.past?(r, '{')

      name = Rtk.view(r) { Rtk.skip_to(r, " }") }

      unless Rtk.past?(r, '}')
        issues.minor("continuing despite missing `}`")
      end

      case key = Parse.symbol(name, issues)
      in Term::Sym
      in Parse::Err
        next # Consume fully
      end

      unless value = pairs[key]?
        issues.minor("ignoring `{#{key}}` because it is missing from the pairspart")
        next # Consume fully
      end

      unless value.type.string? || value.type.symbol? || value.type.number?
        issues.major("ignoring `{#{key}}`: only string, symbol, and number values allowed")
        next # Consume fully
      end

      if value.type.string?
        io << value.to(StringView)
        next # Replace with value
      end

      ML.compact(io, value)
      # Replace with value
    end
  end
end
