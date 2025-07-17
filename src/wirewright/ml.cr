module Ww::ML
  extend self

  class SyntaxError < Exception
    # Returns the byte index near which the error occured in the source string.
    getter byte_index : Int32

    def initialize(@message : String, @byte_index : Int32)
    end

    def line(source : String) : StringView
      source.each_line_view do |line|
        next unless line.byte_start <= @byte_index <= line.byte_end
        return line
      end

      raise IndexError.new
    end

    def lineno(source : String) : Int32
      lineno = 0
      source.each_line_view do |line|
        unless line.byte_start <= @byte_index <= line.byte_end
          lineno += 1
          next
        end
        return lineno
      end

      raise IndexError.new
    end

    def column(line : StringView) : Int32
      column = 0

      line.each_char_with_abs_byte_index do |char, byte_index|
        if @byte_index == byte_index
          return column
        end
        column += 1
      end

      raise ArgumentError.new("byte start points to a character's interior")
    end

    def humanize(io, source : String)
      line = line(source + "$".colorize.dark_gray.to_s)
      lineno = lineno(source)
      col = column(line)
      linestr = line.to_s

      subt = {
        ' '  => "·".colorize.dark_gray.to_s,
        '\t' => "↹".colorize.dark_gray.to_s,
        '\n' => "⏎".colorize.dark_gray.to_s,
      }

      io.puts "syntax error: #{lineno + 1}:#{col + 1}: #{message}"
      io.puts "  >>> #{linestr.gsub(subt)}"
      io.puts "      #{linestr.fill(' ').insert(col, "^")}"
    end
  end

  # Parses and returns multiple top-level WwML expressions from *source*,
  # wrapping them in an itemsonly dictionary.
  #
  # Raises `SyntaxError` in case *source* contains a syntax error.
  def terms(source : String) : Term
    lexer = Text::Lexer.new(source)
    parser = Text::Parser.new(lexer)
    parser.expressions
  rescue e : SyntaxError
    {% if flag?(:mlerr) %}
      e.humanize(STDERR, source)
    {% end %}

    raise e
  end

  # Same as `terms`, but downcasts the resulting term to the dictionary type.
  def dict(source : String) : Term::Dict
    terms(source).as_d
  end

  # Parses and returns a single top-level WwML expression term from *source*.
  #
  # Raises `SyntaxError` in case *source* contains a syntax error.
  def term(source : String) : Term
    lexer = Text::Lexer.new(source)
    parser = Text::Parser.new(lexer)
    parser.expression
  rescue e : SyntaxError
    {% if flag?(:mlerr) %}
      e.humanize(STDERR, source)
    {% end %}

    raise e
  end

  # :nodoc:
  private def edge?(term : Term::Dict, type : TermType) : Bool
    return false unless term.itemsonly?
    return false unless term.size == 2
    return false unless term.probably_includes?(SYM_EDGE)

    term[0] == SYM_EDGE && term[1].type.subtype?(type)
  end

  # :nodoc:
  private def edge?(term : ITerm, type : TermType) : Bool
    false
  end

  # Returns `true` if *term* is a well-formed edge. Returns `false` otherwise.
  #
  # This is just a "hand-optimized" version of the pattern `(%'edge _)`.
  def edge?(term : Term, *, type : TermType = TermType::Any) : Bool
    return false unless term.type.dict?

    edge?(term.unsafe_as_d, type)
  end

  # Returns the initial value for the given term *type*.
  #
  # See also the WwML spec, section "M1 Key-value pair shorthands", subsection
  # "Initial values".
  #
  # Raises `ArgumentError` if *type* is `any`.
  def initial(type : TermType) : Term
    case type
    in .any?
      raise ArgumentError.new("TermType::Any initial value is undefined")
    in .boolean? then Term.of(false)
    in .dict?    then Term.of
    in .number?  then Term.of(0)
    in .string?  then Term.of("")
    in .symbol?  then Term.of(:unset)
    end
  end
end

require "./ml/rune"
require "./ml/grammar"
require "./ml/text"
require "./ml/display"
