# WwML stands for **W**ire**w**right **M**arkup **L**anguage. WwML turns terms
# into a purely functional programming language.
#
# WwML is a *true* purely functional programming language. There are no escape
# hatches as far as WwML is concerned. You cannot print, cannot save files to
# disk, etc.
#
# To be practical, though, one must make effects work somehow. So, we employ
# two general techniques:
#
#   1. Inspecting the WwML source code before or after pure interpretation, or both;
#      in effect, observing it from the outside and acting upon observations impurely.
#   2. Producing effects based on the output of pure WwML code. While the first technique
#      is only possible in homoiconic languages such as Lisp or WwML, the second one
#      is used in the functional programming world in one form or another. We represent
#      effects purely using "commands" or something similar, return them, and the impure
#      outside world interprets them.
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

  def edge?(term : Term::Dict, *, allowed = EDGE_ALLOWED_DEFAULT) : Bool
    return false unless term.itemsonly?
    return false unless term.size == 2
    return false unless term.probably_includes?(SYM_EDGE.unsafe_as_sym)

    term.ee.all? do |k, v|
      case k
      when Term[0] then v == SYM_EDGE
      when Term[1] then v.type.in?(allowed)
      else
        unreachable
      end
    end
  end

  def edge?(term : ITerm, *, allowed = EDGE_ALLOWED_DEFAULT) : Bool
    false
  end

  # Returns `true` if *term* is a well-formed edge. Returns `false` otherwise.
  def edge?(term : Term, allowed = EDGE_ALLOWED_DEFAULT) : Bool
    return false unless term.type.dict?

    edge?(term.unsafe_as_d, allowed: allowed)
  end

  def hold?(term : Term::Dict) : Bool
    term.itemsonly? && term.size == 2 && term[0] == SYMBOL_HOLD
  end

  def hold?(term : ITerm) : Bool
    false
  end

  def hold?(term : Term) : Bool
    return false unless term.type.dict?

    hold?(term.unsafe_as_d)
  end
end

require "./ml/text"
