module Ww
  # Capable of storing up to 8 ranges.
  alias Charset8 = Charset(10)

  # Capable of storing up to 16 ranges.
  alias Charset16 = Charset(18)

  # Capable of storing up to 32 ranges.
  alias Charset32 = Charset(34)

  # A charset implementation in the spirit of `Char#in_set?`, but precompiled;
  # and thus ~4x faster for sets such as `"\n\t\r\u0020-\u{10FFFF}^\"⸢\\\\"`.
  #
  # `N2` specifies the amount of stack memory that the charset will be
  # allowed to use. It must be `N + 2`, hence the name; the first two memory
  # cells are used to store auxiliary data.
  module Charset(N2)
    extend self

    # Compiles a character *set* and yields a pointer to the resulting `Compiled`
    # charset object.
    #
    # WARNING: the yielded pointer is a *pointer into stack memory*! It **must not**
    # outlive the block, since the memory it points to is likely to become invalid
    # after the block returns. You should copy the pointer's contents into memory
    # that you own by dereferencing it there. Closures will store it for
    # you provided you dereference into the captured scope. Raw charsets are
    # *very heavy* structs. Try to not pass them around in their raw form a huge lot;
    # there's copying overhead and I am not sure how well LLVM is able to manage it.
    # That's why a *pointer* is yielded rather than a raw static array.
    #
    # *set* can include:
    #
    # - Individual characters (e.g., "abc" matches 'a', 'b', or 'c').
    # - Ranges indicated by hyphens (e.g., "a-z" matches all lowercase letters).
    # - Escape sequences prefixed with backslashes to include special characters
    #   literally (e.g., "\\-" matches a hyphen).
    # - A caret '^' at the beginning, or after a character or range, indicates negation;
    #   all subsequent characters or ranges are excluded (e.g., "a^b" matches 'a' but not 'b').
    def compile(set : String, & : Int32[N2]* -> T) : T forall T
      b = e = '\0'
      state0 = state1 = :initial

      mem = uninitialized Int32[N2]
      cursor = 2
      negbegin = nil

      reader = Char::Reader.new(set)

      loop do
        char = reader.current_char

        case {state0, char}
        when {:initial, '\0'}
          state0 = :final
          next
        when {:initial, '\\'}
          state1 = :escape
        when {:initial, '^'}
          if negbegin
            raise ArgumentError.new("negative set after negative set disallowed")
          end

          negbegin = cursor
        when {:escape, '\0'}
          raise ArgumentError.new("unexpected end-of-input in escape sequence")
        when {:initial, _}, {:escape, _}
          b = e = char
          state1 = :begin
        when {:begin, '-'}
          state1 = :end
        when {:begin, '\0'}
          state0 = :commit
          state1 = :final
          next
        when {:begin, _}
          state0 = :commit
          state1 = :initial
          next
        when {:end, '\0'}
          raise ArgumentError.new("unexpected end-of-input while waiting for end-of-range")
        when {:end, _}
          e = char
          state0 = :commit
          state1 = :advance
          next
        when {:commit, _}
          start = b.ord

          if b == e
            if cursor + 1 > N2
              raise ArgumentError.new("charset `#{set}` exceeds memory limit for ranges (#{N2})")
            end
            mem[cursor] = -start
            cursor += 1
          else
            if cursor + 2 > N2
              raise ArgumentError.new("charset `#{set}` exceeds memory limit for ranges (#{N2})")
            end
            mem[cursor] = start
            mem[cursor + 1] = e.ord
            cursor += 2
          end

          state0 = state1
          next
        when {:advance, _}
          state1 = :initial
        when {:final, '\0'}
          break
        else
          raise "BUG: unexpected state-char tuple {#{state0}, #{char}}"
        end

        state0 = state1
        reader.next_char
      end

      mem[0] = cursor
      mem[1] = negbegin || cursor

      yield pointerof(mem)
    end

    # Returns `true` if *ch* is a member of the compiled *charset*.
    # Returns `false` otherwise.
    def match?(charset : Int32[M]*, ch : Char) : Bool forall M
      size = charset.value.unsafe_fetch(0)
      negbegin = charset.value.unsafe_fetch(1)
      cursor = 2
      pos = cursor == negbegin

      while cursor < size
        el = charset.value.unsafe_fetch(cursor)

        if el < 0
          b = e = -el
          stride = 1
        else
          b = el
          e = charset.value.unsafe_fetch(cursor + 1)
          stride = 2
        end

        # Check if it is in the positive set.
        if b <= ch.ord <= e
          if cursor >= negbegin
            # In negative set.
            return false
          end

          # In positive set.
          pos = true
          cursor = negbegin
          next
        end

        cursor += stride
      end

      pos
    end
  end
end
