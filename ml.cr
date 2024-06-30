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
  class SyntaxError < Exception
  end

  # Parses and returns multiple top-level WwML expressions from *source*,
  # wrapping them in an itemsonly dictionary.
  #
  # Raises `SyntaxError` in case *source* contains a syntax error.
  def self.parse(source : String) : Term
    lexer = Text::Lexer.new(source)
    parser = Text::Parser.new(lexer)
    parser.expressions
  end

  # Parses and returns a single top-level WwML expression term from *source*.
  #
  # Raises `SyntaxError` in case *source* contains a syntax error.
  def self.parse1(source : String) : Term
    lexer = Text::Lexer.new(source)
    parser = Text::Parser.new(lexer)
    parser.expression
  end
end

require "./ml/text"
