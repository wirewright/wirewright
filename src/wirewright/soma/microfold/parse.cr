module Ww::Soma::Microfold::Parse
  extend self

  alias Err = Refusal | Failure

  # Recoverable failure ("maybe pick a different option").
  record Refusal

  # Unrecoverable failure.
  record Failure

  # :nodoc:
  def refusal
    Refusal.new.as(Err)
  end

  # :nodoc:
  def failure
    Failure.new.as(Err)
  end

  record Reject

  # Returns the result of a successful parse *call*.
  #
  # WARNING: `return`s (exits from the current method) if *call* resulted
  # in an error `Err`.
  macro parse!(call)
    pass do
      case %result = {{call}}
      when {{@type}}::Err
        return {{@type}}.failure
      end

      %result
    end
  end

  # Yields the result of the first matching branch to the block.
  #
  # WARNING: `return`s (exits from the current method) if any branch
  # resulted in `Failure`.
  macro pick!(*branches, &block)
    pass do
      {% for branch in branches %}
        case %result = {{branch}}
        when Refusal
        when Failure
          return %result
        else
          pass(%result) {{block}}
        end
      {% end %}
    end
  end

  # Parses a Microfold style string *src*. Reports any issues to *issues*.
  def style(locus : Locus, pairs : Term::Dict, src : StringView, issues : Issue::Sink) : Term
    style = Term::Dict.build do |commit|
      src.split(' ') do |phrase|
        next if phrase.empty?

        issues.adjoin(Issue::Spot::Detail.new("phrase", phrase)) do |issues|
          parts = phrase.split(':').to_readonly_slice

          # We know for sure that the last part is a utility.
          case node0 = utility(parts[-1], issues)
          in Err then next
          in Term
          end

          parts = parts[...-1]

          # Everything before that is a condition. We'll evaluate conditions
          # immediately because this is easier at parse-time where we can backtrack
          # based on *pairs* instead of doing weird ad hoc `-`-joining later on.
          parts.each do |part|
            case result = condition(locus, pairs, part, node0, issues)
            in Term
              node0 = result
            in Err, Reject
              node0 = nil # Burn node, condition failed
              break
            end
          end

          next unless node0

          commit << node0
        end
      end
    end

    Term.of(style)
  end

  # Parses a utility string *src*, as in `style: "⏏bg-blue-500⏏ p-5".
  #
  # Reports any issues to *issues*.
  #
  # NOTE: parsing a utility is not the same as "resolving" it into constituent mixins --
  # *decomposition* must follow parsing. See also: `Microfold.decompose`.
  def utility(src : StringView, issues : Issue::Sink) : Term | Err
    issues.adjoin(Issue::Spot::Detail.new("utility", src)) do |issues|
      src.reader do |r|
        case
        when Rtk.pastsequ?(r, "items-")
          child = parse! utility(Rtk.rest(r), issues)

          # We consider prefixing a pseudo-utility with `items-` an error because
          # it is an instance of "spooky action at a distance"; moreover, supporting
          # that would complicate the implementation significantly for little to no
          # expressive benefit; since then we'd have to be able to handle `items-cue-*`
          # and so on, which would require moving this to fixpoint-side.
          Term.case(child) do
            matchpi %[(utility _)] do
              Term.of(:items, child)
            end

            otherwise do
              issues.adjoin(Issue::Spot::Detail.new("pseudo-utility", Rtk.rest(r))) do |issues|
                issues.major("cannot prefix a pseudo-utility with `items-`")
              end

              failure
            end
          end
        when Rtk.pastsequ?(r, "present\0")
          Term.of(:present)
        when Rtk.pastsequ?(r, "absent\0")
          Term.of(:absent)
        when Rtk.pastsequ?(r, "membrane\0")
          Term.of(:membrane)
        when Rtk.pastsequ?(r, "is-")
          name = parse! symbol(Rtk.rest(r), issues)

          Term.of(:"cue-up", name)
        when Rtk.pastsequ?(r, "cue-")
          name = parse! symbol(Rtk.rest(r), issues)

          Term.of(:"cue-dn", name)
        else
          Term.of(:utility, Rtk.rest(r))
        end
      end
    end
  end

  # Parses a condition, as in `style: "⏏hover⏏:bg-blue-500"`.
  #
  # Indicates rejection as opposed to failure (as in parse failure) by
  # returning `Reject`.
  #
  # Reports any issues to *issues*.
  def condition(locus : Locus, pairs : Term::Dict, src : StringView, child : Term, issues : Issue::Sink) : Term | Reject | Err
    issues.adjoin(Issue::Spot::Detail.new("condition", src)) do |issues|
      pick!(
        pseudo_condition(locus, pairs, src, issues),
        cue_condition(src, child, issues),
        pair_condition(pairs, src, issues)
      ) do |result|
        case result
        in Term then return result
        in true then return child
        in false
          issues.note("rejected by condition")
          return Reject.new
        end
      end

      refusal
    end
  end

  # Parses and evaluates a pseudo-condition, as in `style: "⏏(first)⏏:bg-blue-500"`
  #
  # Indicates acception or rejection by returning a boolean in the happy path.
  #
  # Reports any issues to *issues*.
  def pseudo_condition(locus : Locus, pairs : Term::Dict, src : StringView, issues : Issue::Sink) : Bool | Err
    unless src.surrounded_by?('(', ')')
      return refusal
    end

    src = src[1...-1]

    issues.adjoin(Issue::Spot::Detail.new("pseudo-condition", src)) do |issues|
      src.reader do |r|
        case
        when Rtk.pastsequ?(r, "alone\0")
          return locus.only_child?
        when Rtk.pastsequ?(r, "first")
          case
          when Rtk.at_end?(r)
            return locus.first_child?
          when Rtk.past?(r, '-')
            # E.g. bg-neutral-900 first-3:bg-neutral-800
            n = parse! number(Rtk.rest(r), issues)
            return locus.first_child?(n)
          end
        when Rtk.pastsequ?(r, "last")
          case
          when Rtk.at_end?(r)
            return locus.last_child?
          when Rtk.past?(r, '-')
            # E.g. bg-neutral-900 last-3:bg-neutral-800
            n = parse! number(Rtk.rest(r), issues)
            return locus.last_child?(n)
          end
        when Rtk.pastsequ?(r, "period")
          if Rtk.past?(r, '-')
            n = parse! number(Rtk.rest(r), issues)
            return locus.child_of_period?(n)
          end
        end

        issues.major("invalid pseudo-condition `#{src}`")
        failure
      end
    end
  end

  # Parses and evaluates a pair condition, as in `style: "⏏hover⏏:bg-blue-500"`
  #
  # Indicates acception or rejection by returning a boolean in the happy path.
  #
  # Reports any issues to *issues*.
  def pair_condition(pairs : Term::Dict, src : StringView, issues : Issue::Sink) : Bool | Err
    src.reader do |r|
      successors = pairs

      loop do
        committed = Rtk.txn(r) do
          segment = Rtk.view(r) { Rtk.skip_to(r, '-') }

          case segment
          when .empty?
            raise ArgumentError.new("empty src")
          when .starts_with?('0'..'9'), "true", "false"
            false # revert, it's definitely a value ahead
          else
            key = parse! symbol(segment, issues)

            if value = successors[key]?
              successors = value
            end

            # If key exists, then okay, it was a key all along. Otherwise, revert,
            # it's possibly a value -- certainly not a key.
            !!value
          end
        end

        break unless committed
        break unless Rtk.past?(r, '-')
      end

      # There is no `-` ahead but rather, EOI, as in `⏏hover⏏:bg-blue-500`.
      if Rtk.at_end?(r)
        return successors != Term[false]
      end

      # If we failed to commit, reinterpret the rest as an argument.
      rest = Rtk.rest(r)
      arg = parse! argument(rest, issues)

      # In degenerate cases such as `10:foo` or `true:bar` this will be reached
      # and will result in `false` -- which is expected behavior.
      successors == arg
    end
  end

  # Parses a cue condition, as in `style: "⏏in-foo⏏:bg-blue-500"` or
  # `style: "⏏has-bar⏏:bg-blue-300`.
  #
  # Reports any issues to *issues*.
  def cue_condition(src : StringView, child : Term, issues : Issue::Sink) : Term | Err
    src.reader do |r|
      case
      when Rtk.pastsequ?(r, "in-")
        name = parse! symbol(Rtk.rest(r), issues)

        Term.of(:"on-cue-dn", name, child)
      when Rtk.pastsequ?(r, "has-")
        name = parse! symbol(Rtk.rest(r), issues)

        Term.of(:"on-cue-up", name, child)
      else
        refusal
      end
    end
  end

  # Parses a utility argument (a number, boolean, or symbol).
  #
  # Reports any issues to *issues*.
  def argument(src : StringView, issues : Issue::Sink) : Term | Err
    pick!(
      number(src, issues),
      boolean(src),
      symbol(src, issues),
    ) { |value| return Term.of(value) }

    refusal
  end

  # Parses a number (usually number argument of a utility).
  #
  # Uses `ML::Kit.decimal` under the hood, filtering scientific notation out
  # to ensure only numbers are returned.
  #
  # Reports any issues to *issues*.
  def number(src : StringView, issues : Issue::Sink) : Term::Num | Err
    unless src.starts_with?('0'..'9')
      return refusal
    end

    begin
      value = ML::Kit.decimal(src)

      unless number = value.as_n?
        issues.minor("only fractions and decimal notation allowed, but got `#{ML.compact(value)}`")
        return failure
      end

      number
    rescue e : ML::SyntaxError
      issues.minor("syntax error while parsing decimal argument: #{e.detail} (`#{src.show(e.text)}`)")
      failure
    end
  end

  # Parses a boolean (usually boolean argument of a utility).
  def boolean(src : StringView) : Term::Boolean | Err
    case src
    when "true"  then Term[true]
    when "false" then Term[false]
    else
      refusal
    end
  end

  # Parses a symbol (usually symbol argument of a utility).
  #
  # Reports any issues to *issues*.
  def symbol(src : StringView, issues : Issue::Sink) : Term::Sym | Err
    id = src.to_s
    unless Term::Sym.valid?(id)
      issues.minor("`#{id}` is not a valid symbol")
      return failure
    end

    Term::Sym.new(id)
  end
end
