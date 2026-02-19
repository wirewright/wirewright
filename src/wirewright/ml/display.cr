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
      matchpi %[(^ arg_)] do
        pp.text("^")
        format(pp, arg, style)
      end

      matchpi %[(~ args_+)] do
        continue unless args.items.count(&.type.string?) >= args.size*0.5 # ?!

        pp.text("\"")

        args.items.each do |arg|
          if str = arg.as_s?
            pp.text(str.escaped)
          else
            pp.text("⸢")
            format(pp, arg, style)
            pp.text("⸣")
          end
        end

        pp.text("\"")
      end

      matchpi %[(%'edge arg_)] do
        pp.text("@")
        format(pp, arg, style)
      end

      matchpi %[(literal arg_)] do
        pp.text("'")
        format(pp, arg, style)
      end

      matchpi %[(%'%let name_ successor_)] do
        format(pp, name, style)
        pp.text("←")
        format(pp, successor, style)
      end

      matchpi %[(%'%item seq_+)] do
        pp.group(style.indent, "⟨", "⟩") do
          seq.items.each_with_index do |item, i|
            pp.breakable if i > 0
            format(pp, item, style)
          end
        end
      end

      matchpi %[(%'%item° seq_+)] do
        pp.group(style.indent, "⟨", "⟩°") do
          seq.items.each_with_index do |item, i|
            pp.breakable if i > 0
            format(pp, item, style)
          end
        end
      end

      matchpi %[(%'%layer %'_ side_dict)] do
        pp.group(style.indent, "{¦ ", "}") do
          index = 0

          side.each_entry(in: Term::Dict.entries_ord) do |k, v|
            pp.breakable if index > 0
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

      matchpi %[()] do
        pp.text("()")
      end

      matchpi %[(¦ _)] do
        pp.group(style.indent, "{", "}") do
          index = 0
          term.each_entry(in: Term::Dict.entries_ord) do |k, v|
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
          term.items.each_with_index do |item, index|
            pp.breakable if index > 0
            format(pp, item, style)
          end
        end
      end

      matchpi %[_dict] do
        pp.group(style.indent, "(", ")") do
          term.items.each_with_index do |item, index|
            pp.breakable if index > 0
            format(pp, item, style)
          end
          term.each_entry(in: Term::Dict.pairspart_ord) do |k, v|
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

  def display(io : IO, term : Term::Any, **kwargs)
    display(io, Term.of(term), **kwargs)
  end

  def display(term : Term, **kwargs)
    String.build { |io| display(io, term, **kwargs) }
  end

  def display(term : Term::Any, **kwargs)
    display(Term.of(term), **kwargs)
  end

  # :nodoc:
  def compact(io : IO, term : Term::Num) : Nil
    if term.approx?
      io << "≈"
    end

    term.decimal(io)
  end

  # :nodoc:
  def compact(io : IO, term : Term::Str) : Nil
    io << '"'
    term.escaped.to_s(io)
    io << '"'
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

  # :nodoc:
  def compact(io : IO, term : Term::Boolean) : Nil
    if term.true?
      io << "true"
    else
      io << "false"
    end
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

  # Appends the compact WwML representation of *term* to *io*.
  #
  # See also: `compact(term : Term | Term::Any)`.
  def compact(io : IO, term : Term) : Nil
    compact(io, Term[term])
  end

  # Returns the compact WwML representation of *term*.
  #
  # `ML.compact` is a foundational printer for Wirewright terms.
  #
  # - It is slightly less fancier than `display`.
  # - Its output is single-line. `ML.compact` guarantees to produce single-line
  #   output. Therefore, it can be used on the write end of newline-delimited
  #   text protocols.
  # - The output of `ML.compact` is guaranteed to be valid `ML`; therefore,
  #   it is also a way to serialize terms.
  # - All `Term::Any#inspect`s delegate to `ML.compact` immediately. This is
  #   done deliberately, respecting the design of Wirewright: `Term::Any` members
  #   and `Term` itself are concerned with *in-memory representation of terms*
  #   (how the bits and bytes are arranged). `ML`, in turn, is responsible for
  #   representing terms using human-readable text.
  def compact(term : Term | Term::Any) : String
    String.build { |io| compact(io, term) }
  end

  # Returns the bytesize of *term*'s compact WwML representation.
  def compact_bytesize(term : Term | Term::Any) : Int32
    io = IO::BytesizeCounter.new

    compact(io, term)

    io.bytesize
  end
end
