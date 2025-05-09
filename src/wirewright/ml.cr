module Ww::ML
  extend self

  class SyntaxError < Exception
    # Returns the byte index near which the error occured in the source string.
    getter byte_index : Int32

    def initialize(@message : String, @byte_index : Int32)
    end

    def line(source : String) : String
      r = Char::Reader.new(source, pos: @byte_index)

      String.build do |io|
        while r.has_previous? && r.current_char != '\n'
          r.previous_char
        end
        while r.has_next?
          r.next_char
          break if r.current_char == '\n'
          io << r.current_char
        end
      end
    end

    def lineno(source : String) : Int32
      r = Char::Reader.new(source, pos: @byte_index)
      line = 0

      while r.has_previous?
        if r.current_char == '\n'
          line += 1
        end
        r.previous_char
      end

      line
    end

    def column(source : String) : Int32
      r = Char::Reader.new(source, pos: @byte_index)
      column = 0

      while r.has_previous? && r.current_char != '\n'
        column += 1
        r.previous_char
      end

      column
    end

    def humanize(io, source : String)
      col = column(source)
      io.puts "SyntaxError: #{lineno(source) + 1}:#{col + 1}: #{message}"
      io.puts "  >>> #{line(source).insert(col, "‸")}"
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
end

require "./ml/text"
require "./ml/display"
