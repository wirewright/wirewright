@[Flags]
enum Ww::ML::Style : UInt8
  Indent2

  def indent
    indent2? ? 2 : 1
  end
end

module Ww::ML::Formatter
  extend self

  def format(pp, term : Term, style : Style)
    Term.case(term) do
      matchp %[(edge _)] do
        pp.text("@")
        format(pp, term[1], style)
      end

      matchpi %[()] do
        pp.text("()")
      end

      matchpi %[(¦ _)] do
        pp.group(style.indent, "{", "}") do
          index = 0
          term.ee.each do |k, v|
            pp.comma if index > 0
            format(pp, k, style)
            pp.text(":")
            pp.group(style.indent) do
              pp.breakable
              format(pp, v, style)
            end
            index += 1
          end
        end
      end

      matchpi %[(_* ¦)] do
        pp.group(style.indent, "(", ")") do
          term.ie.each_with_index do |item, index|
            pp.breakable if index > 0
            format(pp, item, style)
          end
        end
      end

      matchpi %[_dict] do
        pp.group(style.indent, "(", ")") do
          term.ie.each_with_index do |item, index|
            pp.breakable if index > 0
            format(pp, item, style)
          end
          term.pe(ordered: true).each do |k, v|
            pp.breakable
            format(pp, k, style)
            pp.text(":")
            pp.group(style.indent) do
              pp.breakable
              format(pp, v, style)
            end
          end
        end
      end

      matchpi %((%any° _number _string _boolean _symbol)) do
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
    io << '('

    term.ie.join(io, ' ') do |item|
      compact(io, item)
    end

    unless term.itemsonly? || term.pairsonly?
      io << ' '
    end

    term.pe(ordered: true).join(io, ' ') do |(k, v)|
      compact(io, k)
      io << ":"
      compact(io, v)
    end

    io << ')'
  end

  def self.compact(io : IO, term : ITerm)
    term.inspect(io)
  end

  def self.compact(io : IO, term : Term)
    compact(io, term.downcast)
  end

  def self.compact(term : Term | ITerm) : String
    String.build { |io| compact(io, term) }
  end

  def self.compact_bytesize(term : Term | ITerm) : Int32
    io = IO::BytesizeCounter.new

    compact(io, term)

    io.bytesize
  end
end
