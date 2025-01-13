# Provides utilities for DIY substring implementation, and the official substring
# implementation (`words`, `runes`).
module Ww::Term::Str::Substring
  extend self

  # :nodoc:
  #
  # Reads words and spaces between them.
  abstract struct WordReader
    # Moves to the next character.
    abstract def forward : Nil

    # Returns `true` if this reader is positioned at the end of the underlying string.
    abstract def end? : Bool

    # Returns the byte index of the current character.
    def pos : Int32
      @reader.pos
    end

    # Returns the current character.
    def chr : Char
      @reader.current_char
    end

    # Returns `true` if the current character is whitespace.
    def space?(char : Char) : Bool
      char.whitespace?
    end

    # Skips zero or more whitespace characters.
    def spaces : Nil
      until end? || !space?(chr)
        forward
      end
    end

    # Returns `true` if the current character is a word character (alphanumeric
    # or subword separator).
    def word?(char : Char) : Bool
      char.alphanumeric? || char.in?('_', '-', '/')
    end

    # Skips one character unconditionally, followed by zero or more word characters.
    def word : Nil
      until end?
        forward
        break unless word?(chr)
      end
    end
  end
  
  # Left-to-right word reader.
  struct WordReader::LtoR < WordReader
    def initialize(string : String)
      @reader = Char::Reader.new(string)
    end

    def forward : Nil
      @reader.next_char
    end
  
    def end? : Bool
      @reader.current_char == '\0'
    end
  end
  
  # Right-to-left word reader.
  struct WordReader::RtoL < WordReader
    def initialize(string : String)
      @boi = false
      @reader = Char::Reader.new(at_end: string)
      @reader.next_char # Move to EOI '\0'
    end
  
    def forward : Nil
      if @reader.pos.zero?
        @boi = true
      else
        @reader.previous_char
      end
    end
  
    def end? : Bool
      @boi
    end

    def space?(char : Char) : Bool
      super || char == '\0'
    end
  end

  # Drops N words from left.
  def lwdrop(string : Term::Str, nwords : Int) : Term::Str
    string = string.to(String)

    r = WordReader::LtoR.new(string)
  
    nwords.times do
      break if r.end?
      r.word
      r.spaces
    end
  
    Term[string.byte_slice(r.pos..)]
  end

  # Takes N words from left.
  def lwtake(string : Term::Str, nwords : Int) : Term::Str
    string = string.to(String)

    r = WordReader::LtoR.new(string)
  
    nwords.times do
      break if r.end?
      r.word
      r.spaces
    end
  
    Term[string.byte_slice(0...r.pos)]
  end

  # Drops N words from right.
  def rwdrop(string : Term::Str, nwords : Int) : Term::Str
    string = string.to(String)

    r = WordReader::RtoL.new(string)
  
    nwords.times do
      break if r.end?
      r.spaces
      r.word
    end
  
    Term[r.end? ? "" : string.byte_slice(0..r.pos)]
  end

  # Takes N words from right.
  def rwtake(string : Term::Str, nwords : Int) : Term::Str
    string = string.to(String)

    r = WordReader::RtoL.new(string)
  
    nwords.times do
      break if r.end?
      r.spaces
      r.word
    end
  
    Term[r.end? ? string : string.byte_slice(r.pos + 1..)]
  end

  # Takes a word substring, both ends included. `0` means the first word.
  # Negative numbers count from the last word. `-1` means the last word.
  def words(string : Term::Str, b0 : Int32, e0 : Int32) : Term::Str
    string = b0.negative? ? Substring.rwtake(string, b0.abs) : Substring.lwdrop(string, b0)
    string = e0.negative? ? Substring.rwdrop(string, e0.abs - 1) : Substring.lwtake(string, e0 - b0 + 1)
    string
  end

  # Takes a rune (char) substring, both ends included. `0` means the first rune.
  # Negative numbers count from the last rune. `-1` means the last rune.
  def runes(string : Term::Str, b0 : Int32, e0 : Int32) : Term::Str
    Term[string.to(String)[b0..e0]? || ""]
  end
end
