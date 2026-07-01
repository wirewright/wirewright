module Ww::ML
  # Raised when WwML lexeme or term reader fails to read a string.
  class SyntaxError < Exception
    # Returns a string that explains the error.
    getter detail : String

    # Returns the offending text -- a view of the original source string.
    getter text : StringView

    # Gets or sets the name of the file associated with the offending text.
    property? filename : String?

    # :nodoc:
    def initialize(@detail : String, @text : StringView, @filename : String? = nil)
    end

    def message : String
      _, line, column = SyntaxError.lookaround(@text)
      "#{@detail} (#{@filename || "<unknown>"}:#{line}:#{column})"
    end

    # Performs canonical ML lookaround, where *text* is the offending
    # snippet of source code.
    #
    # Returns three things:
    # - *text* extended to newlines on both ends (left newline is not included
    #   but the one on the right is)
    # - Line number (counting from 1).
    # - Column (counting from 1).
    def self.lookaround(text : StringView) : {StringView, Int32, Int32}
      extended = text
        .reverse_extend { |chr| chr != '\n' }
        .extend(exclusive: false) { |chr| chr != '\n' }

      column_index = text
        .before_begin
        .reverse_extend { |chr| chr != '\n' }
        .size

      column = column_index + 1

      line_index = extended.prior_string.ee.count('\n')
      line = line_index + 1

      {extended, line, column}
    end

    # Returns the starting byte index for the offending text.
    def byte_index : Int32
      @text.byte_start
    end

    def inline(io : IO) : Nil
      b = text.char_start
      e = text.char_end

      if b == e
        io << text.@string.insert(b, "⏏")
      else
        io << text.@string.insert(e, "⏏").insert(b, "⏏")
      end
    end

    def inline : String
      String.build { |io| inline(io) }
    end

    # :nodoc:
    struct StyleStack
      def initialize(&@fn : Symbol, Symbol ->)
        @stack = [] of Symbol
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
    # *styled* can be used to enable/disable emission of ANSI escape
    # sequences for colors, emphasis, etc.
    def humanize(io, *, styled : Bool = Colorize.enabled?) : Nil
      extended, line, column = SyntaxError.lookaround(@text)

      styles = StyleStack.new do |style0, style1|
        next unless styled

        case {style0, style1}
        when {_, :initial}
          io << "\e[0m"
        when {_, :normal}
          io << "\e[0;37m" # reset, light gray
        when {_, :focus}
          io << "\e[0;4:3;97;1m" # reset, undercurl, white, bold
        when {_, :error}
          io << "\e[0;33;1m" # reset, light yellow, bold
        when {_, :dark_error}
          io << "\e[0;93m" # reset, dark yellow
        when {_, :link}
          io << "\e[0;97;4m" # reset, white, underline
        when {:focus, :dim}
          io << "\e[0;4:3;37m" # reset, undercurl, light gray
        when {_, :dim}, {_, :fg}
          io << "\e[0;90m" # reset, dark gray
        else
          raise ArgumentError.new("unexpected style transition #{style0.inspect} -> #{style1.inspect}")
        end
      end

      io << "In "

      styles.push(:link) do
        if (filename = @filename) && !filename.blank?
          io << filename << ":"
        end
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
            if !@text.empty? && byte_index == @text.byte_start
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

            if byte_index.in?(@text.byte_bounds) && !(byte_index + chr.bytesize).in?(@text.byte_bounds)
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
    end

    # Returns a human-readable error message.
    #
    # See the other overload to learn about *kwargs*
    def humanize(**kwargs) : String
      String.build { |io| humanize(io, **kwargs) }
    end
  end
end
