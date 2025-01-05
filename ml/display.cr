@[Flags]
enum Ww::ML::Style : UInt8
  HiddenMeta
end

module Ww::ML::Formatter
  extend self

  def format(pp, term : Term, style : Style)
    Term.case(term) do
      givenp %(edge (%either _number _symbol _string)) do
        pp.text("@")
        format(pp, term[1], style)
      end

      givenp %(hold _) do
        pp.text("'")
        format(pp, term[1], style)
      end

      givenp %(embed _) do
        pp.text("`")
        format(pp, term[1], style)
      end

      givenp %(place (leaf _)) do
        pp.text("^")
        format(pp, term[1, 1], style)
      end

      givenp %(paste (leaf _)) do
        pp.text("^:")
        format(pp, term[1, 1], style)
      end

      givenp %(place _) do
        pp.text("\\")
        format(pp, term[1], style)
      end

      givenp %(paste _) do
        pp.text("\\:")
        format(pp, term[1], style)
      end

      givenp %() do
        pp.text("()")
      end

      givenp %(%partition () _dict) do
        pp.group(1, "{", "}") do
          index = 0
          term.ee.each do |k, v|
            next if style.hidden_meta? && ML.meta?(k)
            pp.comma if index > 0
            format(pp, k, style)
            pp.text(":")
            pp.group(1) do
              pp.breakable
              format(pp, v, style)
            end
            index += 1
          end
        end
      end

      givenp %(%partition _dict ()) do
        pp.group(1, "(", ")") do
          term.ie.each_with_index do |item, index|
            pp.breakable if index > 0
            format(pp, item, style)
          end
        end
      end

      matchp %(_dict) do
        pp.group(1, "(", ")") do
          term.ie.each_with_index do |item, index|
            pp.breakable if index > 0
            format(pp, item, style)
          end
          term.pe.each do |k, v|
            next if style.hidden_meta? && ML.meta?(k)
            pp.breakable
            format(pp, k, style)
            pp.text(":")
            pp.group(1) do
              pp.breakable
              format(pp, v, style)
            end
          end
        end
      end

      matchp %((%either _number _string _boolean _symbol)) do
        pp.text(term)
      end
    end
  end
end

module Ww::ML
  def self.display(io : IO, term : Term, *, endl : Bool = true, maxwidth = 60, style = Style::None)
    pp = PrettyPrint.new(io, maxwidth: maxwidth)
    Formatter.format(pp, term, style)
    pp.flush
    io.puts if endl
  end

  def self.display(io : IO, term : ITerm, **kwargs)
    display(io, term.upcast, **kwargs)
  end

  def self.display(term : Term, **kwargs)
    String.build { |io| display(io, term, **kwargs) }
  end

  def self.display(term : ITerm, **kwargs)
    display(term.upcast, **kwargs)
  end

  def self.compact(io : IO, term : Term::Dict)
    io << "("
    term.ee.join(io, " ") do |(k, v)|
      compact(io, k)
      io << ":"
      compact(io, v)
    end
    io << ")"
  end

  def self.compact(io : IO, term : ITerm)
    term.inspect(io)
  end

  def self.compact(io : IO, term : Term)
    compact(io, term.downcast)
  end
end
