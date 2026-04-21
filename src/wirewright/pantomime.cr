module Ww
  # A tiny MIME type detector based on file signatures.
  #
  # Reference: https://en.wikipedia.org/wiki/List_of_file_signatures
  module PantoMIME
    extend self

    # :nodoc:
    defrecord ByteRule, offset : Int32, signature : Bytes, type : String, params = {} of String => String
    # :nodoc:
    defrecord PlaintextRule, pattern : StringPattern, type : String, params = {} of String => String
    # :nodoc:
    defrecord PlaintextGroup, type : String, rules : Array(PlaintextRule)
    # :nodoc:
    defrecord BinarySink, type : String

    # :nodoc:
    alias StringPattern = Includes | StartsWith | AfterWhitespace | Choice

    # :nodoc:
    defcase Includes, seq : String

    # :nodoc:
    defcase StartsWith, seq : String

    # :nodoc:
    defcase AfterWhitespace, child : StringPattern

    # :nodoc:
    defcase Choice, branches : Slice(StringPattern) do
      def self.new(*branches : StringPattern) : Choice
        new(branches.to_readonly_slice(&.as(StringPattern)))
      end
    end

    # :nodoc:
    DB = {
      ByteRule.new(
        offset: 0,
        signature: Bytes[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A],
        type: "image/png",
      ),
      ByteRule.new(
        offset: 0,
        signature: Bytes[0xFF, 0xD8, 0xFF],
        type: "image/jpeg",
      ),
      ByteRule.new(
        offset: 0,
        signature: Bytes[0x42, 0x4D],
        type: "image/bmp",
      ),
      ByteRule.new(
        offset: 0,
        signature: Bytes[0x47, 0x49, 0x46, 0x38, 0x37, 0x61],
        type: "image/gif",
      ),
      ByteRule.new(
        offset: 0,
        signature: Bytes[0x47, 0x49, 0x46, 0x38, 0x39, 0x61],
        type: "image/gif",
      ),
      ByteRule.new(
        offset: 0,
        signature: Bytes[0x50, 0x33, 0x0A],
        type: "image/x-portable-pixmap",
        params: {"format" => "ascii"},
      ),
      ByteRule.new(
        offset: 0,
        signature: Bytes[0x50, 0x36, 0x0A],
        type: "image/x-portable-pixmap",
        params: {"format" => "binary"},
      ),
      ByteRule.new(
        offset: 0,
        signature: Bytes[0x00, 0x01, 0x00, 0x00, 0x00],
        type: "font/ttf",
      ),
      ByteRule.new(
        offset: 0,
        signature: Bytes[0x4F, 0x54, 0x54, 0x4F],
        type: "font/otf",
      ),
      PlaintextGroup.new(
        type: "text/plain",
        rules: [
          PlaintextRule.new(
            pattern: Includes.new("<svg"),
            type: "image/svg+xml",
          ),
          PlaintextRule.new(
            pattern: AfterWhitespace.new(
              Choice.new(
                StartsWith.new("{"),
                StartsWith.new("["),
              )
            ),
            type: "application/json",
          ),
        ]
      ),
      BinarySink.new(type: "application/octet-stream"),
    }

    PLAINTEXT_BUFFER_SIZE = 512

    CONTROL_SET = Pf::BitSet32[
      0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, # 0x09, 0x0a
      0x0b, 0x0c,                                           # 0x0d
      0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15,
      0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d,
      0x1e, 0x1f,
    ]

    private def control?(byte : UInt8) : Bool
      CONTROL_SET.includes?(byte.to_u32) || byte == 0x7F
    end

    private def includes_control?(bytes : Bytes) : Bool
      bytes.any? { |byte| control?(byte) }
    end

    private def matches?(pattern : Includes, string : StringView) : Bool
      string.each_inflection do |l, r|
        if r.starts_with?(pattern.seq)
          return true
        end
      end

      false
    end

    private def matches?(pattern : StartsWith, string : StringView) : Bool
      string.starts_with?(pattern.seq)
    end

    private def matches?(pattern : AfterWhitespace, string : StringView) : Bool
      matches?(pattern.child, string.lstrip(" \n\t"))
    end

    private def matches?(pattern : Choice, string : StringView) : Bool
      pattern.branches.any? do |branch|
        matches?(branch, string)
      end
    end

    private def matches?(pattern : StringPattern, string : String) : Bool
      matches?(pattern, string.view)
    end

    def detect(buffer : Bytes) : MIME::MediaType
      DB.each do |rule|
        case rule
        in ByteRule
          next unless buffer.size >= rule.offset
          next unless (buffer + rule.offset).starts_with?(rule.signature)
          return MIME::MediaType.new(rule.type, rule.params.dup)
        in PlaintextGroup
          # TODO: Support other encodings
          # TODO: Language detection: text/plain encoding=___ lang=___
          # TODO: Programming language detection: text/x-___, e.g. text/x-crystal, text/x-wwml

          # It can't be plaintext if it's not valid UTF-8 (currently we only support UTF-8).
          next unless ::Unicode.valid?(buffer)

          # It can't be plaintext if the UTF-8 contains ASCII control characters such as NUL.
          next if includes_control?(buffer)

          if buffer.size <= PLAINTEXT_BUFFER_SIZE
            prefix = String.new(buffer)
          else
            prefix = String.new(buffer.trim(PLAINTEXT_BUFFER_SIZE))
          end

          type = rule.rules.leftmost? do |rule_case|
            next unless matches?(rule_case.pattern, prefix)

            MIME::MediaType.new(rule_case.type, rule_case.params.dup)
          end

          type ||= MIME::MediaType.new(rule.type)

          if buffer.all? { |byte| byte < 0x80 }
            type["charset"] = "us-ascii"
          else
            type["charset"] = "utf-8"
          end

          return type
        in BinarySink
          return MIME::MediaType.new(rule.type)
        end
      end

      raise ArgumentError.new
    end
  end
end
