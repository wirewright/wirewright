module Ww::Unicode::Index
  extend self

  {% begin %}
    {%
      aliases_data = read_file("#{__DIR__}/NameAliases.txt")
      aliases_rows = aliases_data
        .lines
        .map(&.strip)
        .reject { |line| line.empty? || line.starts_with?("#") }
        .map do |line|
          codepoint_hex, name, _category = line.split(";")
          codepoint = codepoint_hex.to_i(16)
          {codepoint, name}
        end

      aliases = {} of NoReturn => NoReturn
      aliases_rows.each do |(k, v)|
        if ary = aliases[k]
          aliases[k] << v
        else
          aliases[k] = [v]
        end
      end

      db_data = read_file("#{__DIR__}/UnicodeData.txt")
      db_rows = db_data.lines.map do |line, index|
        codepoint_hex, name = line.split(";")
        codepoint = codepoint_hex.to_i(16)

        if name == "<control>"
          unless queries = aliases[codepoint]
            raise "could not find name for codepoint #{codepoint_hex} in database nor aliases file"
          end
        else
          queries = [name]
        end

        {queries, codepoint}
      end
    %}

    # :nodoc:
    NAME2CODE = {
      {% for row in db_rows %}
        {% queries, codepoint = row %}
        {% for query in queries %}
          {{query}} => {{codepoint}},
        {% end %}
      {% end %}
    }
  {% end %}

  {% begin %}
    # :nodoc:
    EMOJI2CODE = {
      {% for line in read_file("#{__DIR__}/emoji.txt").lines %}
        {% name, emoji = line.split(";") %}
        {{name}} => {{emoji}},
      {% end %}
    }
  {% end %}

  # Returns the codepoint associated with the given Unicode *name*.
  # Returns `nil` if found no such association.
  def codepoint?(name : String) : Int32?
    NAME2CODE[name.upcase]?
  end

  # Returns the emoji associated with the given *name*. Returns `nil`
  # if found no such emoji.
  #
  # Reference: https://github.com/github/gemoji/blob/0eca75db9301421efc8710baf7a7576793ae452a/db/emoji.json
  def emoji?(name : String) : String?
    EMOJI2CODE[name.underscore]?
  end

  # :nodoc:
  NAME2GREEK = {
    "Alpha"   => 'Α',
    "alpha"   => 'α',
    "Beta"    => 'Β',
    "beta"    => 'β',
    "Delta"   => 'Δ',
    "delta"   => 'δ',
    "Epsilon" => 'Ε',
    "epsilon" => 'ε',
    "Phi"     => 'Φ',
    "phi"     => 'φ',
    "Gamma"   => 'Γ',
    "gamma"   => 'γ',
    "Eta"     => 'Η',
    "eta"     => 'η',
    "Theta"   => 'Θ',
    "theta"   => 'θ',
    "Iota"    => 'Ι',
    "iota"    => 'ι',
    "Kappa"   => 'Κ',
    "kappa"   => 'κ',
    "Lambda"  => 'Λ',
    "lambda"  => 'λ',
    "Mu"      => 'Μ',
    "mu"      => 'μ',
    "Nu"      => 'Ν',
    "nu"      => 'ν',
    "Xi"      => 'Ξ',
    "xi"      => 'ξ',
    "Omicron" => 'Ο',
    "omicron" => 'ο',
    "Pi"      => 'Π',
    "pi"      => 'π',
    "Rho"     => 'Ρ',
    "rho"     => 'ρ',
    "Sigma"   => 'Σ',
    "sigma"   => 'σ',
    "sigmaf"  => 'ς',
    "Tau"     => 'Τ',
    "tau"     => 'τ',
    "Upsilon" => 'Υ',
    "upsilon" => 'υ',
    "Omega"   => 'Ω',
    "omega"   => 'ω',
    "Chi"     => 'Χ',
    "chi"     => 'χ',
    "Psi"     => 'Ψ',
    "psi"     => 'ψ',
    "Zeta"    => 'Ζ',
    "zeta"    => 'ζ',
  }

  # Returns the greek letter associated with *name*. To get an uppercase letter,
  # capitalize the first letter of the name, e.g. `"Alpha"` gives `'Α'` and `"alpha"`
  # gives `'α'`
  def greek?(name : String) : Char?
    NAME2GREEK[name]?
  end
end
