# A small parselet framework for use in Wirewright.
#
# `S` represents the parse state, and must respond to:
#
# - `#loc : Int32`
# - `#current_char? : Char?`
# - `#at_end? : Bool`
# - `#ahead?(chars : String | StringView) : Bool`
# - `#skip(bytesize : Int32) : S`
# - `#skip(stopword : String? = nil, & : Char -> Bool) : S`
# - `#view_upto(ahead : S) : StringView`
#
# It is an absolute requirement that `S` is immutable and persistent.
module Ww::Parselet(S)
  extend self

  # Represents a parselet instance. Holds a parselet function of type `T`.
  struct Pi(T)
    def initialize(@core : T)
    end

    def call(s : S) forall S
      @core.call(s)
    end
  end

  # Represents a parse error.
  alias Err = Refusal | Failure

  # Represents success without an associated object.
  record Ok

  # A parselet may refuse to match, this is considered "soft failure"; refusal
  # is the way a parselet can politely ask its caller to try something else;
  # it is a potential point-of-dispatch from the caller's point of view.
  record Refusal, byte : Int32, detail : String

  # A parselet may fail to match, this is considered "hard failure". A vast
  # majority of callers would simply propagate failure up, similarly to exceptions
  # but more lightweight.
  #
  # Parselets should emit failure when there is an "obvious" error; in the sense
  # that the parselet cannot allow / conceive of its callers *dispatching* on
  # that error. Failure is expected to be emitted when a certain parselet has
  # progressed too far to simply refuse; to that parselet, the input looks more
  # like genuine input with a mistake by the user rather than a point-of-dispatch
  # for the caller.
  #
  # Whether to refuse or fail is still a difficult question sometimes. When
  # parselets misbehave in this regard, callers can place `strict` appropriately
  # to convert all refusals to failures; or `relaxed` to do the opposite.
  record Failure, byte : Int32, detail : String

  # Shorthand to construct `OK`.
  def ok
    Ok.new
  end

  # Shorthand to construct `Refusal` at the current location.
  def refusal(s : S, detail : String)
    refusal(s.loc, detail)
  end

  # Shorthand to construct `Failure` at the current location.
  def failure(s : S, detail : String)
    failure(s.loc, detail)
  end

  # Shorthand to construct `Refusal` at *loc* (e.g. given by `loc`).
  def refusal(loc, detail : String) : Err
    Refusal.new(loc, detail).as(Err)
  end

  # Shorthand to construct `Failure` at *loc* (e.g. given by `loc`).
  def failure(loc, detail : String) : Err
    Failure.new(loc, detail).as(Err)
  end

  # :nodoc:
  def strict(response : Refusal)
    Failure.new(response.byte, response.detail)
  end

  # Converts refusals to failures.
  def strict(response)
    response
  end

  # :nodoc:
  def relaxed(response : Failure)
    Refusal.new(response.byte, response.detail)
  end

  # Converts failures to refusals.
  def relaxed(response)
    response
  end

  # See `parseout`.
  defcase MissingParseout < Exception, err : Err

  # Extracts the parseout object from a parselet *response*. Raises
  # `MissingParseout` in case of an error.
  def parseout(response : {T, S}) : T forall T
    parseout, s = response

    unless s.at_end?
      return parseout(refusal(s, "unexpected input"))
    end

    parseout
  end

  # :ditto:
  def parseout(response : Err)
    raise MissingParseout.new(response)
  end

  # Advances through a character from the given character *set*.
  #
  # End-of-input can be matched with `\0`. This parselet will not advance
  # on EOI.
  #
  # *detail* specifies the message to use for refusal.
  #
  # See `Charset` to learn about the set syntax.
  def chr(set : String, *, cls = Charset8, detail : String = "unexpected input") : Pi
    cls.compile(set) do |stackptr|
      # Copy into closure-seen, local scope.
      charset = stackptr.value

      core = ->(s : S) do
        char = s.current_char? || '\0'

        unless cls.match?(pointerof(charset), char)
          return refusal(s, detail)
        end

        # Do not advance on EOI.
        if char == '\0'
          return ok, s
        end

        {ok, s.skip(char.bytesize)}
      end

      Pi.new(core)
    end
  end

  # Skips through zero or more characters from the given character *set*. This
  # is likely to be faster than using `reduce`, since skipping with `past` is
  # much more local.
  #
  # End-of-input matching is not supported!
  #
  # See `Charset` to learn about the set syntax.
  def pastchr(
    set : String,
    stopword : String? = nil,
    *,
    cls = Charset8,
    min : Int32 = 0,
    detail : String = "unexpected end-of-input",
  ) : Pi
    cls.compile(set) do |stackptr|
      # Copy into closure-seen, local scope.
      charset = stackptr.value

      core = ->(s0 : S) do
        size = 0

        s1 = s0.skip(stopword) do |ch|
          if cls.match?(pointerof(charset), ch)
            size += 1
            true
          else
            false
          end
        end

        if size < min
          return refusal(s1, detail)
        end

        {ok, s1}
      end

      Pi.new(core)
    end
  end

  # Advances through a character sequence *seq*.
  #
  # *detail* specifies the message to use for refusal.
  def chrseq(seq : String, *, detail : String = "unexpected input") : Pi
    core = ->(s : S) do
      unless s.ahead?(seq)
        return refusal(s, detail)
      end

      {ok, s.skip(seq.bytesize)}
    end

    Pi.new(core)
  end

  # A parselet whose parseout is the fragment of the source string matched
  # by *successor*.
  #
  # *successor*'s own parseout is discarded.
  def view(successor a : Pi(_)) : Pi
    core = ->(s0 : S) do
      response = a.call(s0)
      if response.is_a?(Err)
        return response
      end

      _, s1 = response

      {s0.view_upto(s1), s1}
    end

    Pi.new(core)
  end

  # A parselet that converts parseouts of type `T` to parseouts of type `U`.
  # *fn* is also allowed to refuse or fail.
  def select(successor a : Pi(S -> {T, S} | Err), &fn : S, T -> U | Err) : Pi forall T, U
    {% if U <= Err %}
      {% raise "cannot use Err for U, you should wrap it" %}
    {% end %}

    core = ->(s : S) do
      response = a.call(s)
      if response.is_a?(Err)
        return response
      end

      pout0, s = response
      pout1 = fn.call(s, pout0)
      pout1.is_a?(Err) ? pout1 : {pout1, s}
    end

    Pi.new(core)
  end

  # A parselet that converts parseouts of type `T` to parseouts of type `U`.
  def map(a : Pi(S -> {T, S} | Err), &fn : T -> U) : Pi forall T, U
    self.select(a) { |_, object| fn.call(object) }
  end

  # Constructs a parselet that replaces upstream parseouts with `Ok`.
  def discard(successor) : Pi
    map(successor) { ok }
  end

  # Lets parselet *a* advance state; then lets parselet *b* advance state.
  # Reverts to the state before matching on refusal or failure of *a* or *b*,
  # The parseout is a tuple of *a*'s parseout followed by *b*'s parseout.
  def seq(a : Pi(_), b : Pi(_)) : Pi
    core = ->(s : S) do
      response0 = a.call(s)
      if response0.is_a?(Err)
        return response0
      end

      pout0, s = response0

      response1 = b.call(s)
      if response1.is_a?(Err)
        return response1
      end

      pout1, s = response1

      { {pout0, pout1}, s }
    end

    Pi.new(core)
  end

  # A sequence of N parselets. The parseout is a tuple of parseouts of *a*, *b*, *cs*.
  def seq(a : Pi(_), b : Pi(_), *cs : Pi(_)) : Pi
    parselet = seq(seq(a, b), *cs)

    core = ->(s : S) do
      response = parselet.call(s)
      if response.is_a?(Err)
        return response
      end

      results, state = response
      {results.flatten1, state}
    end

    Pi.new(core)
  end

  # :nodoc:
  def seq(a : Pi(_))
    a
  end

  # Shorthand for `discard(seq)`.
  def dseq(*args, **kwargs) : Pi
    discard(seq(*args, **kwargs))
  end

  # Includers can instantiate a builder object of type `B` that `selcat` can
  # append parseouts to.
  module Builder(B)
    # Constructs a builder object and yields it to the block. Returns
    # the object that was built.
    abstract def build(& : B ->)
  end

  # Select-concat: appends *min* to *max* parseouts of *successor* to the given
  # *builder* using *fn*.
  #
  # *mindetail* specifies the message to use for refusal if the amount of matches
  # is less than *min*.
  def selcat(
    builder : Builder(B),
    successor a : Pi(S -> {U, S} | Err),
    *,
    min : Int32,
    max : Int32? = nil,
    mindetail = "unexpected end-of-input",
    &fn : S, B, U -> Ok | Err
  ) : Pi forall B, T, U
    core = ->(s : S) do
      built = builder.build do |memo|
        (0...max).each do |index|
          response = a.call(s)

          if response.is_a?(Failure)
            return response
          end

          if response.is_a?(Refusal)
            if index < min
              return refusal(s, "unexpected end-of-input")
            end
            break
          end

          pout0, s = response
          pout1 = fn.call(s, memo, pout0)
          if pout1.is_a?(Err)
            return pout1
          end

          # pout1 : Ok
        end
      end

      {built, s}
    end

    Pi.new(core)
  end

  # Same as `selcat`, but returns `ok` and discards parse state for you.
  def cat(builder : Builder(B), successor a : Pi(S -> {U, S} | Err), **kwargs, &fn : B, U ->) : Pi forall B, T, U
    selcat(builder, a, **kwargs) do |_, memo, object|
      fn.call(memo, object)
      ok
    end
  end

  # Same as `cat`, but calls `<<` on the builder to append for you.
  def cat(*args, **kwargs) : Pi
    cat(*args, **kwargs) do |dst, object|
      dst << object

      nil # < sometimes this helps prevent weird Crystal compiler bugs
    end
  end

  # :nodoc:
  struct ReduceSlot(T, A)
    struct Appender(T, A)
      def initialize(@state : A*, @fn : A, T -> A)
      end

      def <<(object : T) : Nil
        @state.value = @fn.call(@state.value, object)
      end
    end

    include Builder(Appender(T, A))

    def initialize(@initial : A, @fn : A, T -> A)
    end

    def build(& : Appender(T, A) ->) : A
      state = @initial.dup
      yield Appender.new(pointerof(state), @fn)
      state
    end
  end

  # Same as `cat`, but expects *fn* to return the next accumulator instead
  # of relying on a mutable builder.
  def reduce(initial : A, successor a : Pi(S -> {T, S} | Err), **kwargs, &fn : A, T -> A) : Pi forall A, T
    cat(ReduceSlot.new(initial, fn), a, **kwargs)
  end

  # Same as `reduce`, but discards all parseouts of *successor*.
  def reduce(successor a : Pi(_), **kwargs) : Pi
    reduce(ok, a, **kwargs) { |memo, _| memo }
  end

  private def attempt(s : S, a, &)
    response = a.call(s)

    unless response.is_a?(Err)
      yield response
      unreachable("block must not return")
    end

    response
  end

  # A parselet whose parseout is that of the first successful branch. The parseout's
  # type is a union of the types of *branches*.
  def choice(*branches : Pi(_)) : Pi
    core = ->(s : S) do
      maxrefusal = nil

      branches.each do |branch|
        err = attempt(s, branch) { |response| return response }

        case err
        in Failure
          return err
        in Refusal
          if maxrefusal.nil? || err.byte >= maxrefusal.byte
            maxrefusal = err
          end
        end
      end

      maxrefusal || raise ArgumentError.new("choice without branches")
    end

    Pi.new(core)
  end

  # A parselet that performs what amounts to negative lookahead on *successor*.
  # If *successor* succeeds, the parselet outputs refusal with *detail*.
  # Failures are propagated.
  def not(successor a : Pi(_), *, detail : String = "unexpected input") : Pi
    core = ->(s : S) do
      response = a.call(s)
      if response.is_a?(Refusal)
        return ok, s
      end

      if response.is_a?(Failure)
        return response
      end

      refusal(s, detail)
    end

    Pi.new(core)
  end

  # A parselet that tries to match *a*, and maps refusals to *default*.
  # Failures are propagated.
  def optional(a : Pi(S -> {T, S} | Err), *, default = nil) : Pi forall T
    {% if T <= Err %}
      {% raise "cannot use Err for T, you should wrap it" %}
    {% end %}

    core = ->(s : S) do
      response = a.call(s)
      response.is_a?(Refusal) ? {default, s} : response
    end

    Pi.new(core)
  end

  # A parselet that performs character-based dispatch. Similar to `choice`.
  # Used mainly during grammar optimization to avoid pointer-hopping.
  #
  # *dispatch* can return `nil` to signal refusal. *detail* is used as
  # the message in such case.
  def dispatcher(*, detail : String = "unexpected input", &dispatch : Char -> Pi(_)?) : Pi
    core = ->(s : S) do
      unless subject = s.current_char?
        return refusal(s, "unexpected end-of-input")
      end

      unless successor = dispatch.call(subject)
        return refusal(s, detail)
      end

      successor.call(s)
    end

    Pi.new(core)
  end

  # Same as *successor*, but discards any state advancement.
  def ahead(successor a : Pi(S -> {T, S} | Err)) : Pi forall T
    core = ->(s0 : S) do
      response = a.call(s0)
      if response.is_a?(Err)
        return response
      end

      pout, _ = response
      {pout, s0}
    end

    Pi.new(core)
  end

  # A parselet that converts refusals of *successor* into failures.
  def strict(successor a : Pi(_)) : Pi
    core = ->(s : S) { strict(a.call(s)) }

    Pi.new(core)
  end

  # Constructs a parselet that converts refusals of *successor* into
  # failure with *detail*.
  def strict(successor a : Pi(_), *, detail : String)
    choice(strict(a), fail(detail))
  end

  # A parselet that converts failures of *successor* into refusals.
  def relaxed(successor a : Pi(_)) : Pi
    core = ->(s : S) { relaxed(a.call(s)) }

    Pi.new(core)
  end

  # A parselet that unconditionally refuses with *detail*. Can be
  # used e.g. in `choice` as the last "alternative".
  def refuse(detail : String) : Pi
    core = ->(s : S) { refusal(s, detail) }

    Pi.new(core)
  end

  # A parselet that unconditionally fails with *detail*. Can be
  # used e.g. in `choice` as the last "alternative".
  def fail(detail : String)
    core = ->(s : S) { failure(s, detail) }

    Pi.new(core)
  end

  # A parselet whose parseout is the current byte index.
  def loc
    core = ->(s : S) { {s.loc, s} }

    Pi.new(core)
  end

  # Returns a parselet and a proc to define it at a later point.
  #
  # *pout* specifies the parseout type of the parselet to be defined.
  def placeholder(pout : T.class) forall T
    slot = nil

    core = ->(s : S) do
      slot.not_nil!("empty placeholder").call(s)
    end

    define = ->(parselet : Pi(S -> {T, S} | Err)) do
      slot = parselet
      nil
    end

    {Pi.new(core), define}
  end

  # Prints the inputs and outputs to/from *a* using `pp`.
  def debug(a)
    core = ->(s : S) do
      pp s
      pp a.call(s)
    end

    Pi.new(core)
  end

  # Shorthand for `map(view(successor))`.
  def capture(successor a, &fn : StringView -> T) forall T
    map(view(a), &fn)
  end

  # Parses *prefix* followed by *operand*, selecting *operand*'s parseout
  # and discarding that of *prefix*.
  def prefixed(prefix, operand)
    map(seq(prefix, operand)) { |_, pout| pout }
  end

  # Parses *l* followed by *infix* followed by *r*, selecting the parseouts
  # of *l* and *r* (formatted as a tuple), and discarding *infix*'s parseout.
  def infixed(l, infix, r)
    map(seq(l, infix, r)) { |lpout, _, rpout| {lpout, rpout} }
  end

  # Parses *l* followed by *operand* followed by *r*, selecting only *operand*'s
  # parseout and discarding that of *l* and *r*.
  #
  # *strict* can be used to wrap both *operand* and *r* in a call to `strict`.
  def surrounded(l, operand, r, *, strict : Bool = false)
    if strict
      map(seq(l, strict(seq(operand, r)))) { |_, (pout, _)| pout }
    else
      map(seq(l, operand, r)) { |_, pout, _| pout }
    end
  end

  # Parses *operand* followed by *postfix*, selecting only *operand*'s
  # parseout and discarding that of *prefix*.
  def postfixed(operand, postfix)
    map(seq(operand, postfix)) { |pout, _| pout }
  end
end
