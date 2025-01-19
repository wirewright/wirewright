module Ww::ML
  extend self

  class SyntaxError < Exception
    # Returns the byte index near which the error occured in the source string.
    getter byte_index : Int32

    def initialize(@message : String, @byte_index : Int32)
    end
  end

  # Parses and returns multiple top-level WwML expressions from *source*,
  # wrapping them in an itemsonly dictionary.
  #
  # Raises `SyntaxError` in case *source* contains a syntax error.
  def parse(source : String) : Term
    lexer = Text::Lexer.new(source)
    parser = Text::Parser.new(lexer)
    parser.expressions
  end

  # Parses and returns a single top-level WwML expression term from *source*.
  #
  # Raises `SyntaxError` in case *source* contains a syntax error.
  def parse1(source : String) : Term
    lexer = Text::Lexer.new(source)
    parser = Text::Parser.new(lexer)
    parser.expression
  end

  EDGE_ALLOWED_DEFAULT = {TermType::Number, TermType::String, TermType::Symbol}

  # :nodoc:
  def edge?(term : Term::Dict, *, allowed = EDGE_ALLOWED_DEFAULT) : Bool
    return false unless term.itemsonly?
    return false unless term.size == 2
    return false unless term.probably_includes?(SYM_EDGE)

    term.ee.all? do |k, v|
      case k
      when Term[0] then v == SYM_EDGE
      when Term[1] then v.type.in?(allowed)
      else
        unreachable
      end
    end
  end

  # :nodoc:
  def edge?(term : ITerm, *, allowed = EDGE_ALLOWED_DEFAULT) : Bool
    false
  end

  # Returns `true` if *term* is a well-formed edge. Returns `false` otherwise.
  #
  # *allowed* lets you specify the allowed edge name `TermType`s. By default
  # that's number, string, and symbol, but you can e.g. restrict to only symbol
  # edge names by setting *allowed* to `{TermType::Symbol}`.
  #
  # - With the default *allowed* a well-formed edge corresponds to the pattern
  #   `((%literal edge) (%any° _number _string _symbol))`.
  def edge?(term : Term, allowed = EDGE_ALLOWED_DEFAULT) : Bool
    return false unless term.type.dict?

    edge?(term.unsafe_as_d, allowed: allowed)
  end

  # :nodoc:
  def hold?(term : Term::Dict) : Bool
    term.itemsonly? && term.size == 2 && term[0] == SYMBOL_HOLD
  end

  # :nodoc:
  def hold?(term : ITerm) : Bool
    false
  end

  # Returns `true` if *term* matches the following pattern: `(hold _)`.
  def hold?(term : Term) : Bool
    return false unless term.type.dict?

    hold?(term.unsafe_as_d)
  end

  # :nodoc:
  def meta?(term : Term::Sym) : Bool
    string = term.to(String)
    string.starts_with?('(') && string.ends_with?(')')
  end

  # :nodoc:
  def meta?(term : ITerm) : Bool
    false
  end

  # Returns `true` if *term* is a *meta-symbol*. Returns `false` otherwise.
  #
  # If used as a key in a dictionary, the corresponding value is presumably
  # some sort of metadata about that dictionary.
  #
  # Meta-symbols can also be used as general-purpose "special symbols" that
  # the user must not be able to synthesize/edit/etc.; only obtainable from/
  # by machine code.
  #
  # Meta-symbols are symbols that begin with `(` and end with `)`; so they are
  # imparseable for µsoma and WwML. Therefore, a human cannot type them. For
  # instance, `Term[:"(x)"]` is a meta-symbol.
  def meta?(term : Term) : Bool
    meta?(term.downcast)
  end
end

require "./ml/text"
require "./ml/display"
