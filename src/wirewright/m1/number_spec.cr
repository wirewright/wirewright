module Ww::M1
  # :nodoc:
  #
  # Functions for parsing and working with the `(%number ...)` operator.
  module NumberSpec
    extend self

    INT = Term.of(
      u8: {:"%number", UInt8::MIN, :<=, {:whole, :_}, :<=, UInt8::MAX},
      u16: {:"%number", UInt16::MIN, :<=, {:whole, :_}, :<=, UInt16::MAX},
      u32: {:"%number", UInt32::MIN, :<=, {:whole, :_}, :<=, UInt32::MAX},
      u64: {:"%number", UInt64::MIN, :<=, {:whole, :_}, :<=, UInt64::MAX},
      u128: {:"%number", UInt128::MIN, :<=, {:whole, :_}, :<=, UInt128::MAX},
      i8: {:"%number", Int8::MIN, :<=, {:whole, :_}, :<=, Int8::MAX},
      "-i8": {:"%number", Int8::MIN, :<=, {:whole, :_}, :<=, -1},
      "+i8": {:"%number", 0, :<=, {:whole, :_}, :<=, Int8::MAX},
      "+i8!": {:"%number", 1, :<=, {:whole, :_}, :<=, Int8::MAX},
      i16: {:"%number", Int16::MIN, :<=, {:whole, :_}, :<=, Int16::MAX},
      "-i16": {:"%number", Int16::MIN, :<=, {:whole, :_}, :<=, -1},
      "+i16": {:"%number", 0, :<=, {:whole, :_}, :<=, Int16::MAX},
      "+i16!": {:"%number", 1, :<=, {:whole, :_}, :<=, Int16::MAX},
      i32: {:"%number", Int32::MIN, :<=, {:whole, :_}, :<=, Int32::MAX},
      "-i32": {:"%number", Int32::MIN, :<=, {:whole, :_}, :<=, -1},
      "+i32": {:"%number", 0, :<=, {:whole, :_}, :<=, Int32::MAX},
      "+i32!": {:"%number", 1, :<=, {:whole, :_}, :<=, Int32::MAX},
      i64: {:"%number", Int64::MIN, :<=, {:whole, :_}, :<=, Int64::MAX},
      "-i64": {:"%number", Int64::MIN, :<=, {:whole, :_}, :<=, -1},
      "+i64": {:"%number", 0, :<=, {:whole, :_}, :<=, Int64::MAX},
      "+i64!": {:"%number", 1, :<=, {:whole, :_}, :<=, Int64::MAX},
      i128: {:"%number", Int128::MIN, :<=, {:whole, :_}, :<=, Int128::MAX},
      "-i128": {:"%number", Int128::MIN, :<=, {:whole, :_}, :<=, -1},
      "+i128": {:"%number", 0, :<=, {:whole, :_}, :<=, Int128::MAX},
      "+i128!": {:"%number", 1, :<=, {:whole, :_}, :<=, Int128::MAX},
    )

    # Parses a subject *term* (e.g. `_` or `(whole _)`).
    def subject?(term : Term) : Op::Num::Spec?
      Term.case(term, engine: M0) do
        matchpi %{(whole %'_)}, cue: :whole { Op::Num::Spec::Whole }
        matchpi %{%'_}, cue: :_ { Op::Num::Spec::None }
        otherwise { }
      end
    end

    # Parses a comparison argument *term*.
    def arg?(term : Term) : Op::Num::Arg?
      Term.case(term, engine: M0) do
        matchpi %{(var name_)}, cue: :var { Op::Num::Var.new(name) }
        matchpi %{_number} { term.as_n }
        otherwise { }
      end
    end

    SYM_LT  = Term[:<]
    SYM_GT  = Term[:>]
    SYM_LTE = Term[:<=]
    SYM_GTE = Term[:>=]

    # Parses a number spec *term* and returns the resulting number operator `Op::Op::Num`.
    # Returns `nil` for unrecognized terms.
    def op?(term : Term) : Op::Num?
      Term.case(term, engine: M0) do
        # (%number (whole _))
        matchpi %{(%number subject_)}, cue: :"%number" do
          return unless spec = subject?(subject)

          Op::Num.new(spec, nil, nil)
        end

        # (%number _ < 100)
        # (%number (whole _) <= 1/3)
        # (%number _ > (var lo))
        matchpi %{(%number subject_ cmp_ arg_)}, cue: :"%number" do
          return unless spec = subject?(subject)
          return unless r = arg?(arg)

          case cmp
          when SYM_LT
            # _ < 100
            spec = spec.max_excluded
            max = r
          when SYM_LTE
            # _ <= 100
            max = r
          when SYM_GT
            # _ > 100
            spec = spec.min_excluded
            min = r
          when SYM_GTE
            # _ >= 100
            min = r
          else
            return
          end

          Op::Num.new(spec, min, max)
        end

        # (%number 0 < (whole _) < 10)
        # (%number (var lo) <= (whole _) <= 24)
        matchpi %{(%number larg_ lop_ subject_ rop_ rarg_)}, cue: :"%number" do
          return unless spec = subject?(subject)
          return unless min = arg?(larg)
          return unless max = arg?(rarg)

          case lop
          when SYM_LTE
          when SYM_LT
            spec = spec.min_excluded
          else
            return
          end

          case rop
          when SYM_LTE
          when SYM_LT
            spec = spec.max_excluded
          else
            return
          end

          Op::Num.new(spec, min, max)
        end

        otherwise { }
      end
    end

    # Parses a number spec *term* and returns the resulting number operator `Op::Num`.
    # Raises `ArgumentError` on unrecognized terms.
    def op(term : Term) : Op::Num
      op?(term) || raise ArgumentError.new
    end
  end
end
