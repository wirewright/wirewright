# |@ duration
#
# |@summary
# The duration language is used to describe durations of time.
#
# |@example
# ```wwml
# (3 seconds)
# (5 ms)
# (3 weeks)
# ```
module Ww::DurationLanguage
  extend self

  # Passthrough for `nil` to simplify usage with a nilable *span*.
  def encode(span : Nil) : Nil
  end

  # Expresses *span* using the duration language.
  def encode(span : Time::Span) : Term
    if span < 1.nanosecond
      return Term.of(span.total_nanoseconds, :ns)
    end

    if span < 1.millisecond
      return Term.of(span.total_microseconds, :µs)
    end

    if span < 1.second
      return Term.of(span.total_milliseconds, :ms)
    end

    if span < 1.minute
      return Term.of(span.total_seconds, :s)
    end

    if span < 1.hour
      return Term.of(span.total_minutes, :m)
    end

    if span < 1.day
      return Term.of(span.total_hours, :h)
    end

    if span < 1.week
      return Term.of(span.total_days, :d)
    end

    Term.of(span.total_weeks, :w)
  end

  # Passthrough for `nil` to simplify usage with a nilable *term*.
  def decode?(term : Nil) : Nil
  end

  # Converts a duration language expression *term* to a `Time::Span`.
  def decode?(term : Term) : Time::Span?
    Term.case(term) do
      # |@ duration
      #
      # |@pattern
      # (±t ns)
      # (±t nanoseconds)
      #
      # |@block
      # Specifies a duration in nanoseconds.
      matchpi %{(±t ns)}, %{(±t nanoseconds)}, t: Float64 do
        t.nanoseconds
      end

      # |@ duration
      #
      # |@pattern
      # (±t µs)
      # (±t microseconds)
      #
      # |@block
      # Specifies a duration in microseconds.
      matchpi %{(±t µs)}, %{(±t microseconds)}, t: Float64 do
        t.microseconds
      end

      # |@ duration
      #
      # |@pattern
      # (±t ms)
      # (±t milliseconds)
      #
      # |@block
      # Specifies a duration in milliseconds.
      matchpi %{(±t ms)}, %{(±t milliseconds)}, t: Float64 do
        t.milliseconds
      end

      # |@ duration
      #
      # |@pattern
      # (±t s)
      # (±t seconds)
      #
      # |@block
      # Specifies a duration in seconds.
      matchpi %{(±t s)}, %{(±t seconds)}, t: Float64 do
        t.seconds
      end

      # |@ duration
      #
      # |@pattern
      # (±t m)
      # (±t minutes)
      #
      # |@block
      # Specifies a duration in minutes.
      matchpi %{(±t m)}, %{(±t minutes)}, t: Float64 do
        t.minutes
      end

      # |@ duration
      #
      # |@pattern
      # (±t h)
      # (±t hours)
      #
      # |@block
      # Specifies a duration in hours.
      matchpi %{(±t h)}, %{(±t hours)}, t: Float64 do
        t.hours
      end

      # |@ duration
      #
      # |@pattern
      # (±t days)
      #
      # |@block
      # Specifies a duration in days.
      matchpi %{(±t days)}, t: Float64 do
        t.days
      end

      # |@ duration
      #
      # |@pattern
      # (±t weeks)
      #
      # |@block
      # Specifies a duration in weeks.
      matchpi %{(±t weeks)}, t: Float64 do
        t.weeks
      end

      otherwise { }
    end
  end
end
