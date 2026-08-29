@[Flags]
enum Ww::ML::Style
  # The pretty-printer is allowed to make syntax errors and omissions for
  # clarity, brevity, etc.
  Brief

  # The top-level term is interpreted as a document dict if possible.
  Document
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
        pp.group(1, "⟨", "⟩") do
          seq.items.each_with_index do |item, i|
            pp.breakable if i > 0
            format(pp, item, style)
          end
        end
      end

      matchpi %[(%'%item° seq_+)] do
        pp.group(1, "⟨", "⟩°") do
          seq.items.each_with_index do |item, i|
            pp.breakable if i > 0
            format(pp, item, style)
          end
        end
      end

      matchpi %[(%'%layer %'_ side_dict)] do
        pp.group(3, "{¦ ", "}") do
          index = 0

          side.each_entry(in: Term::Dict.entries_ord) do |k, v|
            pp.breakable if index > 0
            format(pp, k, style)
            pp.text(":")
            pp.group(2) do
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
        pp.group(1, "{", "}") do
          index = 0
          term.each_entry(in: Term::Dict.entries_ord) do |k, v|
            pp.comma if index > 0
            format(pp, k, style)
            pp.text(":")
            pp.group(2) do
              pp.breakable
              format(pp, v, style)
            end
            index += 1
          end
        end
      end

      matchpi %[((%plural type: _number min: 8))] do
        # Calculate width.
        n = term.itemsize
        width = Math.sqrt(n).ceil.to_i

        # Compute rows.
        row = [] of String
        rows = [] of Array(String)

        term.items.each_with_index do |item, index|
          row << ML.compact(item)

          if row.size == width
            rows << row
            row = [] of String
          end
        end

        if row.present?
          # Pad so that all rows have the same size.
          (width - row.size).times do
            row << ""
          end

          rows << row
          row = [] of String
        end

        # Compute column widths.
        col_widths = rows.transpose.map do |col|
          col.max_of(&.size)
        end

        # FIXME: I think Crystal's PrettyPrint is unable to do what I want it to do here which
        # is more like what we do in prettyR. We expect:
        #
        # (cell @data
        #   (()
        #    0
        #    (0 0 0 0 0 0 0 0
        #     0 0 0 0 0 0 0 0
        #     0 0 0 0 0 0 0 0
        #     0 0 0 0 0 0 0 0
        #     0 0 0 0 0 0 0 0
        #     0 0 0 0 0 0 0 0
        #     0 0 0 0 0 0 0 0)))
        #
        # What this renders instead is utterly insane. However it still parses & is more compact
        # than the alternative, so for now we'll keep it. The solution though is our own pretty-printer.
        pp.group do
          pp.break
          pp.text("(")

          pp.nest do
            rows.each_with_index do |row, row_index|
              pp.break if row_index > 0

              row.each_with_index do |cell, col_index|
                break if cell.empty?

                pp.text(" ") if col_index > 0

                col_width = col_widths[col_index]
                pp.text(cell.rjust(col_width))
              end
            end

            pp.text(")")
          end
        end
      end

      matchpi %[(_* ¦)] do
        indent = 1
        if (head = term.items.first?) && head.type.symbol?
          indent = 2
        end

        pp.group(indent, "(", ")") do
          term.items.each_with_index do |item, index|
            pp.breakable if index > 0
            format(pp, item, style)
          end
        end
      end

      matchpi %[_dict] do
        indent = 1
        if (head = term.items.first?) && head.type.symbol?
          indent = 2
        end

        pp.group(indent, "(", ")") do
          term.items.each_with_index do |item, index|
            pp.breakable if index > 0
            format(pp, item, style)
          end
          term.each_entry(in: Term::Dict.pairspart_ord) do |k, v|
            pp.breakable
            format(pp, k, style)
            pp.text(":")
            pp.group(2) do
              pp.breakable
              format(pp, v, style)
            end
          end
        end
      end

      matchpi %{_blob} do
        continue unless style.brief?

        blob = term.as_blob
        width = 0
        if blob.ubytesize64 > 0
          width = blob.ubytesize64*2 # Each byte is two hex digits
          width += (width - 1)*1     # Also one whitespace between digits
          width += 2                 # Also a pair of brackets
        end

        continue if width < 80

        if classif = blob.classif?
          pp.text("⟬… #{blob.ubytesize64.humanize_bytes} / #{blob.digest.trim(4).hexstring} ⁑ #{classif} …⟭")
        else
          pp.text("⟬… #{blob.ubytesize64.humanize_bytes} / #{blob.digest.trim(4).hexstring} …⟭")
        end
      end

      otherwise do
        pp.text(ML.compact(term))
      end
    end
  end

  def format_top(pp, term : Term, style : Style) : Nil
    unless style.document? && (docdict = term.as_d?)
      return format(pp, term, style)
    end

    docdict.each_entry(in: Term::Dict.pairspart_ord) do |key, value|
      format(pp, key, style)
      pp.text(":")
      pp.group(2) do
        pp.breakable
        format(pp, value, style)
      end

      pp.break
    end

    docdict.items.each do |item|
      format(pp, item, style)
      pp.break
    end
  end
end

module Ww::ML
  def display(io : IO, term : Term, *, endl : Bool = true, maxwidth = 60, style : Style = Style::None)
    pp = PrettyPrint.new(io, maxwidth: maxwidth)
    Formatter.format_top(pp, term, style)
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

  private enum BlobPrintState
    Outer
    Utf8
  end

  # :nodoc:
  def compact(io : IO, term : Term::Blob) : Nil
    io << '⟬'

    byte_io = term.to_io
    segment_count = 0u64

    if term.utf8?
      state = BlobPrintState::Outer

      byte_io.each_char do |chr|
        case chr
        when '\a' then escape_seq = "\\a"
        when '\b' then escape_seq = "\\b"
        when '\e' then escape_seq = "\\e"
        when '\t' then escape_seq = "\\t"
        when '\n' then escape_seq = "\\n"
        when '\f' then escape_seq = "\\f"
        when '\r' then escape_seq = "\\r"
        when '‸'  then escape_seq = "20 e2 80 b8"
        end

        if escape_seq
          case state
          in .outer?
          in .utf8?
            io << "‸ "
            state = BlobPrintState::Outer
            segment_count += 1
          end

          io << escape_seq
          next
        end

        case state
        in .outer?
          io << ' ' if segment_count > 0
          io << '‸'
          state = BlobPrintState::Utf8
          segment_count += 1
        in .utf8?
        end

        io << chr
      end

      case state
      in .outer?
      in .utf8?
        io << '‸'
        state = BlobPrintState::Outer
      end
    else
      byte_io.each_byte do |byte|
        io << ' ' if segment_count > 0

        digit0 = byte >> 4
        digit1 = byte & 0xf
        io.write_byte(to_hex(digit0))
        io.write_byte(to_hex(digit1))

        segment_count += 1
      end
    end

    if classif = term.classif?
      io << " ⁑ " << classif
    end

    io << '⟭'
  end

  # https://github.com/crystal-lang/crystal/blob/a3178c32b00565fff87ec3375882bfd42a7cb11c/src/slice.cr#L794-L796
  private def to_hex(digit)
    ((digit < 10 ? 48u8 : 87u8) + digit)
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
