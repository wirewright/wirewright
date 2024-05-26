module Ww
  # Represents a boolean.
  struct Term::Boolean
    include ITerm

    def initialize(@value : Bool)
    end

    # :nodoc:
    delegate :inspect, to: @value

    # Returns Crystal `true` if this boolean is Wirewright `true`. Effectively,
    # converts this boolean to a Crystal boolean.
    def true? : Bool
      @value
    end

    # :inherit:
    def to_ir : IR
      IR.new(@value ? IR::Type::True : IR::Type::False, payload: nil, children: nil)
    end

    def_equals_and_hash @value
  end
end
