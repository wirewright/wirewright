module Ww::ML
  extend self

  class SyntaxError < Exception
    # Returns a string that explains the error.
    getter detail : String

    # Returns the offending text -- a view of the original source string.
    getter text : StringView

    def initialize(@detail : String, @text : StringView)
    end

    # Returns the starting byte index for the offending text.
    def byte_index : Int32
      @text.byte_start
    end

    # :nodoc:
    struct StyleStack
      def initialize(&@fn : Symbol, Symbol ->)
        @stack = Stack(Symbol).new
      end

      def push(style style1 : Symbol)
        style0 = @stack.last? || :initial
        @stack << style1
        @fn.call(style0, style1)
      end

      def push(style : Symbol, &)
        push(style)

        yield
      ensure
        pop(style)
      end

      def pop(style : Symbol) : Nil
        style0 = @stack.pop

        unless style0 == style
          raise ArgumentError.new("expected style #{style.inspect}, but got: #{style0.inspect}")
        end

        @fn.call(style0, @stack.last? || :initial)
      end
    end

    # Appends a human-readable error message to *io*.
    #
    # - *styled* can be used to enable/disable emission of ANSI escape
    #   sequences for colors, emphasis, etc.
    # - *filename* defines the filename printed in front of the line and column.
    def humanize(io, *, filename = "scratch", styled : Bool = Colorize.enabled?) : Nil
      extended = @text
        .reverse_extend { |chr| chr != '\n' }
        .extend { |chr| chr != '\n' }

      column_index = @text
        .before_begin
        .reverse_extend { |chr| chr != '\n' }
        .size

      column = column_index + 1

      line_index = extended.prior_string.count('\n')
      line = line_index + 1

      styles = StyleStack.new do |style0, style1|
        next unless styled

        case {style0, style1}
        when {_, :initial}
          io << "\e[0m"
        when {_, :normal}
          io << "\e[0;38;5;252m" # reset, grey82
        when {_, :focus}
          io << "\e[0;97;1m" # reset, white, bold
        when {_, :error}
          io << "\e[0;33;1m" # reset, light yellow, bold
        when {_, :dark_error}
          io << "\e[0;93m" # reset, dark yellow
        when {_, :link}
          io << "\e[0;97;4m" # reset, white, underline
        when {:focus, :dim}
          io << "\e[0;38;5;244m" # reset, grey50
        when {_, :dim}, {_, :fg}
          io << "\e[0;38;5;240m" # reset, grey35
        else
          raise ArgumentError.new("unexpected style transition #{style0.inspect} -> #{style1.inspect}")
        end
      end

      io << "In "

      styles.push(:link) do
        io << filename << ":" unless filename.empty?
        io << line << ":" << column
      end

      io.puts
      io.puts

      hand = "  #{line} | "
      line_prefix = "  #{" " * line.to_s.size} | "

      styles.push(:fg) { io << hand }
      styles.push(:normal) do
        extended.each_line_with_index do |line, index|
          if index > 0
            styles.push(:fg) { io << line_prefix }
          end

          line.each_char_with_abs_byte_index do |chr, byte_index|
            if byte_index.entering?(@text.byte_bounds)
              styles.push(:focus)
            end

            if @text.empty? && byte_index == @text.byte_start
              styles.push(:error) { io << '⏏' }
            end

            case chr
            when ' '
              styles.push(:dim) { io << '·' }
            when '\n'
              styles.push(:dim) { io << '⏎' }
              io.puts
            when '\t'
              styles.push(:dim) { io << '⭾' }
            when '\r'
              styles.push(:dim) { io << '␍' }
            else
              io << chr
            end

            if byte_index.leaving?(@text.byte_bounds)
              styles.pop(:focus)
            end
          end
        end

        if @text.empty? && extended.byte_end == @text.byte_start
          styles.push(:error) { io << '⏏' }
        end
      end

      io.puts
      io.puts

      styles.push(:dark_error) do
        io << "syntax error: " << @detail
      end

      io.puts
      io.puts
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

require "./ml/rune"
require "./ml/kit"
require "./ml/lexeme"

require "./ml/text"

require "./ml/display"
