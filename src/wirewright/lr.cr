# WwLR (Wirewright Linear Representation) is minimal, stack-based representation
# for Wirewright terms. It is designed with long-term stability in mind; in this
# sense, it is a stable, *barely* human-writable and readable compnaion of WwML.
#
# The format starts with a version signature line ("WwLR MAJOR.MINOR.PATCH")
# followed by a sequence of newline-delimited commands that populate and
# manipulate a stack of terms.
#
# ```wwsr
# WwLR 1.0.0
# ;; WwLR representation of (print "Hello, World!")
# ;; This is a comment.
# dict
# sym
# print
# item
# str
# cat
# Hello,
# chrcat16
# 20
# cat
# World
# chrcat16
# 21
# item
# ```
#
# NOTE: That WwLR is simple does not mean it's fast (unfortunately). The main
# bottleneck is the dict implementation, since WwLR is mainly about constructing
# dicts; everything else is such a minor cost that it is barely noticeable.
#
# NOTE: You are recommended to use some kind of compression algorithm on WwLR for
# storage (e.g. gzip); it is a fairly "dump"-y format on its own but compresses nicely.
#
# Encoding:
#
# ```
# input = Term.of(:+, 1, 2, 3)
#
# File.open("path/to/output.wwlr", "w") do |io|
#   LR.encode(io, input)
# end
# ```
#
# Decoding:
#
# ```
# output = File.open("path/to/input.wwlr", "r") do |io|
#   LR.decode(io)
# end
# ```
#
# ### Commands
#
#
# | (stack) input                      | Effect         | Description                                                                                           |
# | ---------------------------------- | -------------- | ----------------------------------------------------------------------------------------------------- |
# | `(_*) sym ␤ N ␤`                   | `(_* _symbol)` | Pushes a symbol **N**                                                                                 |
# | `(_*) str ␤`                       | `(_* "")`      | Pushes an empty string                                                                                |
# | `(_* A_string) cat ␤ B ␤`          | `(_* _string)` | Stitches strings `A` and `B`                                                                          |
# | `(_* A_string) chrcat16 ␤ C ␤`     | `(_* _string)` | Stitches string `A` and a Unicode character given its hex codepoint `C`                               |
# | `(_*) int ␤ L ␤`                   | `(_* _number)` | Pushes an arbitrarily sized base-10 integer literal `L`                                               |
# | `(_* A_number B_number) ratio ␤`   | `(_* _number)` | Pushes the result of dividing `A` by `B`                                                              |
# | `(_* A_number) negate ␤`           | `(_* _number)` | Multiplies `A` by `-1`                                                                                |
# | `(_*) true ␤`                      | `(_* true)`    | Pushes boolean `true`                                                                                 |
# | `(_*) false ␤`                     | `(_* false)`   | Pushes boolean `false`                                                                                |
# | `(_*) dict ␤`                      | `(_* {})`      | Pushes an empty dictionary                                                                            |
# | `(_* D_dict T_) item ␤`            | `(_* _dict)`   | Appends an item term `T` to the dictionary `D`. Pushes the resulting dictionary.                      |
# | `(_* D_dict K_ V_) pair ␤`         | `(_* _dict)`   | Inserts a pair with the key `K` and value `V` to the dictionary `D`. Pushes the resulting dictionary. |
#
# Lines that start with `;;` where a command is expected are treated as comments
# and ignored.
module Ww::LR
  extend self

  # Defines the version of WwLR supported by this implementation.
  VERSION = SemanticVersion.parse("1.0.0")

  # Raised by `decode` on malformed input.
  class DecodeError < Exception
  end

  private def encode0(io, term : Term::Sym) : Nil
    name = term.to(String)

    io << "sym\n"
    io << name << "\n"
  end

  private def encode0(io, term : Term::Str) : Nil
    content = term.to(String)

    start = 0
    size = 0

    io << "str\n"

    content.view.each_split do |l, m, r|
      chr = m.first_char

      if chr == ' ' && (l.empty? || l.ends_with?('\n') || r.empty? || r.starts_with?('\n'))
        # Avoid printing leading and trailing spaces as-is because most editors
        # will remove them. Instead, stitch them to the base string using `catchr`
        # like we stitch non-printable characters.
      elsif chr.printable?
        size += chr.bytesize
        next
      end

      unless size.zero?
        io << "cat\n"
        io.write(content.to_slice[start, size])
        io << "\n"
      end

      io << "chrcat16\n"
      chr.ord.to_s(io, base: 16)
      io << "\n"

      start += size + chr.bytesize
      size = 0
    end

    return if size.zero?

    io << "cat\n"
    io.write(content.to_slice[start, size])
    io << "\n"
  end

  private def each_base10_digit(n : Term::Num, & : Term::Num ->)
    divisor = Term[1]
    while divisor <= n//Term[10]
      divisor *= Term[10]
    end

    until divisor.zero?
      yield n // divisor
      n %= divisor
      divisor //= Term[10]
    end
  end

  private def encode0int(io, int : Term::Num)
    io << "int\n"
    each_base10_digit(int) do |digit|
      io << digit
    end
    io << "\n"
  end

  private def encode0(io, term : Term::Num) : Nil
    if term.whole?
      encode0int(io, term.abs)
    else
      rat = term.abs.to(BigRational)

      encode0int(io, Term[rat.numerator])
      encode0int(io, Term[rat.denominator])
      io << "ratio\n"
    end

    if term.negative?
      io << "negate\n"
    end
  end

  private def encode0(io, term : Term::Boolean) : Nil
    if term.true?
      io << "true\n"
    else
      io << "false\n"
    end
  end

  private def encode0(io, term : Term::Dict) : Nil
    io << "dict\n"

    term.items.each do |item|
      encode0(io, item)
      io << "item\n"
    end

    term.pairspart.each_entry_ord do |key, value|
      encode0(io, key)
      encode0(io, value)
      io << "pair\n"
    end
  end

  private def encode0(io, term : Term) : Nil
    encode0(io, Term[term])
  end

  # Appends the WwLR encoding of *term* to *io*.
  def encode(io, term : Term) : Nil
    io << "WwLR " << VERSION << "\n"

    encode0(io, term)
  end

  # Encodes *term* using WwLR and returns the resulting string.
  def encode(term : Term) : String
    String.build { |io| encode(io, term) }
  end

  # :nodoc:
  struct Decoder
    def initialize
      @state = :initial
      @stack = [] of Term
    end

    def next(command : String) : Nil
      case {@state, command}
      when {:initial, _}
        decl = command.split(' ', 2, remove_empty: true)
        unless decl
          raise DecodeError.new("malformed signature `#{command}`")
        end

        sign, suffix = decl
        unless sign == "WwLR"
          raise DecodeError.new("expected WwLR to begin signature, but got: `#{sign}`")
        end

        begin
          version = SemanticVersion.parse(suffix)
        rescue e : ArgumentError
          raise DecodeError.new("invalid WwLR version `#{suffix}`", cause: e)
        end

        unless version.major == VERSION.major
          raise DecodeError.new("WwLR #{VERSION} cannot decode WwLR produced by WwLR #{version} (encoder)")
        end

        @state = :base
      when {:base, "sym"}
        @state = :sym
      when {:base, "int"}
        @state = :int
      when {:base, "ratio"}
        unless (b = @stack.pop?) && (a = @stack.pop?)
          raise DecodeError.new("ratio: expected >=1 items on the stack")
        end

        unless num = a.as_n?
          raise DecodeError.new("ratio: numerator is not a number")
        end

        unless den = b.as_n?
          raise DecodeError.new("ratio: denominator is not a number")
        end

        if den.zero?
          raise DecodeError.new("ratio: denominator is zero")
        end

        @stack << Term.of(num / den)
      when {:base, "negate"}
        unless arg = @stack.pop?
          raise DecodeError.new("negate: expected >=1 items on the stack")
        end

        unless n = arg.as_n?
          raise DecodeError.new("negate: argument is not a number")
        end

        @stack << Term.of(-n)
      when {:base, "true"}
        @stack << Term.of(true)
      when {:base, "false"}
        @stack << Term.of(false)
      when {:base, "str"}
        @stack << Term.of("")
      when {:base, "dict"}
        @stack << Term.of
      when {:base, "item"}
        unless (item = @stack.pop?) && (dst = @stack.pop?)
          raise DecodeError.new("item: expected >=2 items on the stack")
        end

        unless dict = dst.as_d?
          raise DecodeError.new("item: DST is not a dict")
        end

        dict = dict.append(item)
        @stack << Term.of(dict)
      when {:base, "pair"}
        unless (value = @stack.pop?) && (key = @stack.pop?) && (dst = @stack.pop?)
          raise DecodeError.new("pair: expected >=3 items on the stack")
        end

        unless dict = dst.as_d?
          raise DecodeError.new("pair: DST is not a dict")
        end

        if dict[key]?
          raise DecodeError.new("pair: duplicate key `#{ML.compact(key)}`")
        end

        dict = dict.with(key, value)
        @stack << Term.of(dict)
      when {:base, "cat"}
        @state = :cat
      when {:base, "chrcat16"}
        @state = :chrcat16
      when {:sym, _}
        @stack << Term.of(Term::Sym.new(command))
        @state = :base
      when {:int, _}
        value = Term[0]

        command.each_char do |chr|
          unless '0' <= chr <= '9'
            raise DecodeError.new("could not parse integer `#{command}`")
          end

          value = value.append(Term[chr - '0'], radix: Term[10])
        end

        @stack << Term.of(value)
        @state = :base
      when {:cat, _}
        unless a = @stack.pop?
          raise DecodeError.new("cat: expected >=1 items on the stack")
        end

        unless prefix = a.as_s?
          raise DecodeError.new("cat: prefix is not a string")
        end

        @stack << Term.of(prefix.stitch(Term[command]))
        @state = :base
      when {:chrcat16, _}
        codepoint = command.to_i?(base: 16, whitespace: false) || raise DecodeError.new("could not parse integer `#{command}`")

        unless a = @stack.pop?
          raise DecodeError.new("chrcat16: expected >=1 items on the stack")
        end

        unless prefix = a.as_s?
          raise DecodeError.new("chrcat16: prefix is not a string")
        end

        @stack << Term.of(prefix.stitch(Term[codepoint.chr]))
        @state = :base
      else
        # Ignore comments
        if @state == :base && command.starts_with?(";;")
          return
        end

        raise DecodeError.new("invalid command `#{command}`")
      end
    end

    def final : Term
      unless @stack.size == 1
        raise DecodeError.new("stack is empty or contains >1 item")
      end

      @stack.pop
    end
  end

  # Constructs a term based on its WwLR-encoded representation read from *io*.
  #
  # Raises `DecodeError` on malformed input.
  def decode(io) : Term
    decoder = Decoder.new

    io.each_line do |command|
      decoder.next(command)
    end

    decoder.final
  end

  # Constructs a term based on its WwLR-encoded representation *string*.
  #
  # Raises `DecodeError` on malformed input.
  def decode(string : String) : Term
    decoder = Decoder.new

    string.each_line do |line|
      decoder.next(line)
    end

    decoder.final
  end
end
