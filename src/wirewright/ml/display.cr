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
          term.each_entry_ord do |k, v|
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

      otherwise do
        pp.text(ML.compact(term))
      end
    end
  end
end

module Ww::ML
  def display(io : IO, term : Term, *, endl : Bool = true, maxwidth = 60, style = Style::None)
    pp = PrettyPrint.new(io, maxwidth: maxwidth)
    Formatter.format(pp, term, style)
    pp.flush
    io.puts if endl
  end

  def display(io : IO, term : ITerm, **kwargs)
    display(io, Term.of(term), **kwargs)
  end

  def display(term : Term, **kwargs)
    String.build { |io| display(io, term, **kwargs) }
  end

  def display(term : ITerm, **kwargs)
    display(Term.of(term), **kwargs)
  end

  # :nodoc:
  def compact(io : IO, term : Term::Dict) : Nil
    lbracket, rbracket = '(', ')'

    if !term.empty? && term.pairsonly?
      lbracket, rbracket = '{', '}'
    end

    io << lbracket

    term.ee(ordered: true).join(io, ' ') do |(k, v)|
      if (i = k.to?(Int32)) && i < term.itemsize
        compact(io, v)
      else
        compact(io, k)
        io << ": "
        compact(io, v)
      end
    end

    io << rbracket
  end

  # :nodoc:
  def compact(io : IO, term : ITerm) : Nil
    term.inspect(io)
  end

  # :nodoc:
  def compact(io : IO, term : Term::Sym) : Nil
    name = term.to(String)

    if ML.symbol_bare?(name)
      io << name
    else
      io << "⸍" << name << "⸝"
    end
  end

  # Appends the compact WwML representation of *term* to *io*.
  def compact(io : IO, term : Term) : Nil
    compact(io, Term[term])
  end

  # Returns the compact WwML representation of *term*.
  def compact(term : Term | ITerm) : String
    String.build { |io| compact(io, term) }
  end

  # Returns the bytesize of *term*'s compact WwML representation.
  def compact_bytesize(term : Term | ITerm) : Int32
    io = IO::BytesizeCounter.new

    compact(io, term)

    io.bytesize
  end
end
