module Ww
  # Represents a boolean.
  struct Term::Boolean
    include ITerm
    include Equality
    include TypeConversion

    def initialize(@value : Bool)
    end

    def to?(type : Bool.class) : Bool
      true?
    end

    # Returns Crystal `true` if this boolean is Wirewright `true`. Effectively,
    # converts this boolean to a Crystal boolean.
    @[AlwaysInline]
    def true? : Bool
      @value
    end

    # Returns Crystal `true` if this boolean is Wirewright `false`.
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
