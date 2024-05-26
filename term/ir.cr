module Ww
  # A simpler, serialization-friendly companion of `Term`.
  #
  # - `Term` can be converted to `IR` using `Term#to_ir`.
  # - `IR` can be converted to `Term` using the universal constructor `Term.[]`.
  struct Term::IR
    include JSON::Serializable

    # Lists the possible types of terms.
    enum Type : UInt8
      Dict   = 0u8
      Number
      String
      Symbol
      True
      False
    end

    def initialize(type : Type, @payload : String?, @children : Array(IR)?)
      @type = type.to_u8
    end

    private def type : Type
      Type.new(@type)
    end

    # :nodoc:
    def to_term
      case type
      in .string? then Term[@payload.as(String)]
      in .symbol? then Term::Sym.new(@payload.as(String))
      in .true?   then Term[true]
      in .false?  then Term[false]
      in .number?
        args = @payload.as(String).split(' ')
        case args[0]
        when "I" then Term[args[1].to_i]
        when "R" then Term[BigRational.new(args[1].to_big_i, args[2].to_big_i)]
        else
          raise ""
        end
      in .dict?
        dict = Term[]
        @children.try &.each_slice(2, reuse: true) do |(k, v)|
          dict = dict.with(k.to_term, v.to_term)
        end
        dict
      end
    end
  end
end
