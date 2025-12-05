module Ww
  # Represents a boolean.
  @[Term::Assoc(TermType::Boolean, :unsafe_as_b)]
  struct Term::Boolean
    include Equality
    include AutoUpcast
    include TypeConversion

    def initialize(@value : Bool)
    end

    # Compares this and *other* booleans.
    #
    # - `false` comes first because it's like `0`.
    # - `true` comes  later because it's like `1`.
    def <=>(other : Boolean) : Int32
      case {@value, other.@value}
      in {false, false} then 0
      in {false, true}  then -1
      in {true, false}  then +1
      in {true, true}   then 0
      end
    end

    def to?(type : Bool.class) : Bool
      true?
    end

    # Returns Crystal `true` if this boolean is Wirewright `true`. Effectively,
    # converts this boolean to a Crystal boolean.
    @[Dncast]
    @[AlwaysInline]
    def true? : Bool
      @value
    end

    # Returns Crystal `true` if this boolean is Wirewright `false`.
    @[Dncast]
    @[AlwaysInline]
    def false? : Bool
      !@value
    end

    # :nodoc:
    #
    # TODO: Move to `ML.compact`
    delegate :inspect, to: @value

    def to_s(io)
      inspect(io)
    end

    def_equals @value
  end
end
