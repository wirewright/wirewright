module Ww::M1
  # At the most basic level, *specificity* tells one how *detailed* a pattern is --
  # how much the pattern knows about the matchee. This is particularly useful when
  # comparing two patterns to decide which one to try first. One expects the most
  # knowledgeable pattern to win.
  #
  # There are three specificity tiers:
  #
  # - Toplevel literals such as `100` or `(+ 1 2)` occupy the first, highest
  #   specificity tier.
  # - Toplevel literal choices (`%any`, e.g. `(%any 1 2 3)`) occupy the second
  #   specificity tier. The specificity of such choices *decreases* as the number of
  #   options increases. In a sense, a wider choice is a looser, less knowledgeable choice.
  # - All other patterns take the third specificity tier.
  #
  # The following is a non-exhaustive list of rules that apply to the third tier.
  #
  # We rank patterns in the third tier, in order, by the number of *literals*, *choices*,
  # *constraints*, *details*, *rfield*, and *members* they have.
  #
  # - `%literal`s count toward *literals* (dict %literals count N times where N
  #   is the number of their non-dict leaves).
  # - `%any`s count toward *choices*.
  # - Same-named captures count toward constraints (*constraints*).
  # - Type blanks such as `_number`, `_string` etc., as well as keys, as in `{¦ x y}`,
  #   among other things, count toward *details*.
  # - `%number` counts toward *details*; if whole, +1 detail; if left-hand side
  #   is a number, +1 detail, if right-hand side is a number, +1 detail; vars
  #   count toward *constraints*. Min-included counts +1 detail, max-included
  #   counts +1 detail (e.g. `<=` is considered more detailed than `<`).
  # - `%any°` takes min of its branches as specificity.
  # - `%keypool`, `%keytest` contribute +N *details*, where N is the number of
  #   specified keys.
  # - `%number`s bounded like `(%number 0 <= _ <= 10)` add their size to *rfield*
  #   (short for *receptive field*). The *smaller* the *rfield*, the more specific
  #   a pattern. For instance, `(%number 0 <= _ <= 3)` is more specific than
  #   `(%number -5 <= _ <= 10)`.
  # - Other operators count toward *members*, as in, how many unaccounted operators
  #   the pattern consists of. That is, the algorithm considers patterns with many
  #   unaccounted operators as more specific than those with less ones.
  # - Some operators do not contribute to specificity. Such operators include
  #   `%pass`, `%never`, `%let`.
  struct Specificity
    include Comparable(Specificity)

    # :nodoc:
    def initialize(@tier : Literal | Choice | Pattern)
    end

    # :nodoc:
    def self.compare(a : Pattern, b : Pattern)
      a <=> b
    end

    # :nodoc:
    def self.compare(a : Choice, b : Choice)
      a <=> b
    end

    # :nodoc:
    #
    # (in increasing order)
    PRIORITY = {Pattern, Choice, Literal}

    # :nodoc:
    def self.compare(a, b)
      PRIORITY.index!(a.class) <=> PRIORITY.index!(b.class)
    end

    def <=>(other : Specificity)
      Specificity.compare(@tier, other.@tier)
    end

    def to_s(io)
      io << "Specificity(" << @tier << ")"
    end
  end

  # :nodoc:
  #
  # Requires an exact match, so in some (degenerate?) sense, its specificity
  # is "infinite".
  defrecord Specificity::Literal

  struct Specificity::Literal
    def to_s(io)
      io << "literal"
    end
  end

  # :nodoc:
  #
  # Top-level choices are less specific than literals, but more specific than
  # patterns (since they basically say, *pick one of these literals*).
  #
  # Importantly, *the less literals there are, the more specific the choice*.
  defrecord Specificity::Choice, options : Int32

  struct Specificity::Choice
    include Comparable(Choice)

    # :nodoc:
    def rank
      -options
    end

    def <=>(other : Choice)
      rank <=> other.rank
    end

    def to_s(io)
      io << "choice:" << options
    end
  end

  # :nodoc:
  #
  # Generic pattern specificity.
  defrecord Specificity::Pattern,
    literals : Magnitude,
    choices : Magnitude,
    constraints : Magnitude,
    details : Magnitude,
    rfield : Term::Num,
    members : Magnitude

  struct Specificity::Pattern
    include Comparable(Pattern)

    # :nodoc:
    def rank
      {literals, choices, constraints, details, rfield, members}
    end

    def <=>(other : Pattern)
      rank <=> other.rank
    end

    def to_s(io)
      io << {
        literals:    literals,
        choices:     choices,
        constraints: constraints,
        details:     details,
        rfield:      rfield,
        members:     members,
      }
    end
  end

  private def specificity(prod : Π::Pattern(Term::Dict)) : Specificity::Pattern
    op = prod.pattern

    literals = Magnitude.new(0)
    choices = Magnitude.new(0)
    constraints = Magnitude.new(0)
    details = Magnitude.new(0)
    rfield = Term[0]
    members = Magnitude.new(0)

    names = Set(Term).new

    Kit.walk(op) do |member|
      Term.case(member, engine: M0) do
        # These do not add anything or are auxiliary, so we skip them.
        matchpi(
          %{[%'%pass]},
          %{[%'%never]},
          %{[%'%let _ _]},
          %{[%'%ref _]},
          %{[%'%payload _]},
          cues: {:"%pass", :"%never", :"%let", :"%ref", :"%payload"},
        ) { }

        matchpi(
          %{[%'%number %'_]},
          %{[%'%string]},
          %{[%'%symbol]},
          %{[%'%boolean]},
          %{[%'%dict]},
          cues: {:"%number", :"%string", :"%symbol", :"%boolean", :"%dict"},
        ) do
          details += 1
        end

        # Duplicate name such as in (+ x_ x_) counts as a constraint. Constraints
        # are among the most valued things in a pattern.
        matchpi %{[%'%capture name_]}, cue: :"%capture" do
          next if names.add?(name)

          constraints += 1
        end

        matchpi %{[%'%literal ()]}, cue: :"%literal" do
          literals += 1
        end

        matchpiT %{[%'%literal term_dict]}, cue: :"%literal" do
          literals += term.summary.histogram.total
        end

        matchpi %{[%'%literal _]}, cue: :"%literal" do
          literals += 1
        end

        # Keys as in `{¦ x y}` aren't as valuable as literals but they're still
        # some "non-incidental" information, so they count as details.
        matchpi %{[%'%key _]}, cue: :"%key" do
          details += 1
        end

        # (edge _) aka @_ aka @x_ -> x←@_ matches (%'edge _). Notice the literal,
        # (⏏%'edge⏏ _). Pass (%'edge ⏏_⏏) adds no info. One could argue the dictionary
        # itself provides some value, but I don't think so; that we count it as
        # a literal gives it plenty of value by itself.
        matchpi %{[%'%edge %'_]}, cue: {:"%edge", :_} do
          literals += 1
        end

        # This covers things such as @x_number -> x←@_number which matches
        # (%'edge _number), notice the literal (⏏%'edge⏏ _number) and a bit
        # of detail on the type (%'edge ⏏_number⏏).
        matchpi %{[%'%edge _]}, cue: :"%edge" do
          literals += 1
          details += 1
        end

        matchpi %{[%'%any _*]}, cue: :"%any" do
          choices += 1
        end

        matchpi %{[%'%any° _ _*]}, cue: :"%any°" do
          min = nil

          Kit.each_member(op) do |arm|
            if min.nil?
              min = specificity(Π.pattern(arm))
              next
            end

            min = Math.min(min, specificity(Π.pattern(arm)))
          end

          assert min

          literals += min.literals
          choices += min.choices
          constraints += min.constraints
          details += min.details
          rfield += min.rfield
          members += min.members
        end

        matchpi %{[%'%number _*]}, cue: :"%number" do
          assert num = NumberSpec.op?(Term.of(member.itemspart))

          details += 1
          details += 1 if num.spec.whole?

          # Handle min
          pass do
            next unless min = num.min

            details += 1
            details += 1 if num.spec.min_included?

            next unless min.is_a?(Op::Num::Var)
            next if names.add?(min.name)

            constraints += 1
          end

          # Handle max
          pass do
            next unless max = num.max

            details += 1
            details += 1 if num.spec.max_included?

            next unless max.is_a?(Op::Num::Var)
            next if names.add?(max.name)

            constraints += 1
          end

          # Handle rfield
          pass do
            next unless min = num.min.as?(Term::Num)
            next unless max = num.max.as?(Term::Num)
            next unless num.spec.min_included? && num.spec.max_included?
            next unless min <= max

            # (%number 0 <= _ <= 10)
            # (%number 0 <= (whole _) <= 10)
            rfield += max - min
          end
        end

        matchpi %{[%'%keypool _*]}, %{[%'%keytest _*]}, cues: {:"%keypool", :"%keytest"} do
          keys = op.items.move(1)
          details += keys.size
        end

        otherwise do
          members += 1
        end
      end
    end

    Specificity::Pattern.new(literals, choices, constraints, details, rfield, members)
  end

  private def specificity(prod : Π::Toplevel(Term::Dict))
    op = prod.pattern

    Term.case(op, engine: M0) do
      matchpi %{[%'%literal _]}, cue: :"%literal" do
        Specificity.new(Specificity::Literal.new)
      end

      matchpi %{[%'%any _*]}, cue: :"%any" do
        Specificity.new(Specificity::Choice.new(options: op.itemsize - 1))
      end

      matchpi %{[%'%let _ successor_]}, cue: :"%let" do
        specificity(Π.toplevel(successor.as_d))
      end

      otherwise do
        Specificity.new(specificity(Π.pattern(op)))
      end
    end
  end

  # Returns the specificity of the given normal pattern *normp*.
  #
  # See `Specificity` for general info.
  def specificity(pattern : Normp) : Specificity
    pattern.unwrap { |op| specificity(Π.toplevel(op)) }
  end
end
