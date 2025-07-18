module Ww::ML
  extend self


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

  # Returns `true` if a symbol with the given *name* has a representation in WwML.
  # Returns `false` otherwise.
  def can_represent_symbol?(name : String) : Bool
    case name
    when "true", "false",
         .prefixed_by?('\''),
         .starts_with?('0'..'9')
      false
    else
      true
    end
  end
end

require "./ml/syntax_error"
require "./ml/rune"
require "./ml/kit"
require "./ml/lexeme"
require "./ml/reader"

require "./ml/text"

require "./ml/display"
