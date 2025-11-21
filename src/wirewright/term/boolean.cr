module Ww
  # Represents a boolean.
  struct Term::Boolean
    include ITerm
    include TypeConversion

    def initialize(@value : Bool)
    end

    # :nodoc:
    delegate :inspect, to: @value

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

    def_equals @value
  end
end
