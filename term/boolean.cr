module Ww
  # Represents a boolean.
  struct Term::Boolean
    include ITerm

    def initialize(@value : Bool)
    end

    # :nodoc:
    delegate :inspect, to: @value

    def to?(type : Bool.class) : Bool
      true?
    end

    # Returns Crystal `true` if this boolean is Wirewright `true`. Effectively,
    # converts this boolean to a Crystal boolean.
    def true? : Bool
      @value
    end

    def_equals_and_hash @value
  end
end
