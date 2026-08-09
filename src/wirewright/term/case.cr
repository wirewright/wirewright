# Structural pattern matching DSL over `Term`s, used in `Term.case`.
#
# The `case` block contains one or more *branches*: `match`, `given`, and their
# variants. It can contain zero or one catch-all `otherwise` clauses as well.
#
# Each branch matches the matchee term against one or more patterns. When a pattern
# succeeds, any captured subterms are bound to local variables on the Crystal side,
# and the branch body executes. If no branch matches, the `otherwise` clause runs.
# If there is no `otherwise`, the matching process raises `ArgumentError`.
#
# ```text
# Term.case(matchee) do
#   # Matches a raw term. term0-N is a Crystal expression.
#   match term0, term1, ..., termN do
#     # branch body
#   end
#
#   # <cue> Optional: one or two cues *for all patterns* in the branch.
#   #  SymbolLiteral
#   #  Tuple(SymbolLiteral, SymbolLiteral)
#   #
#   # <cues> Optional: one cue for each pattern.
#   #   TupleLiteral(<cues element>+)
#   #
#   # <cues element>
#   #   SymbolLiteral
#   #   Nil
#   #     Hole: no cue for this pattern
#   match({:eval, :+, :a_, :b_}, {:eval, :-, :a_, :b_}, {:eval, :*, :a_, :b_}, cue: :eval, cues: {:+, nil, :*}) do |a, b|
#     # a : Term
#     # b : Term
#   end
#
#   # match* and givenp* variants accept the same keyword args as `match`
#   # (i.e. they also accept `cue`, `cues`, etc.)
#
#   # Parses patterns ml0-mlN using `ML.term` (*p* is for parse).
#   matchp %{(+ a_ b_)}, ml1, ..., mlN do |a, b|
#     #                                    ^^^^
#     # Declare captures you want to reference in block args.
#     # Referenced captures must be present in all of ml0-mlN.
#
#     # a : Term
#     # b : Term
#   end
#
#   # *i* is for infer.
#   matchpi %{(- a_ b_)} do # < Use block args to refer to captures that `case` can't detect.
#     # a : Term
#     # b : Term
#   end
#
#   # *piT variants can type cast based on common type-matching patterns.
#   #
#   # *T* is for type.
#   matchpiT %{(- a←(%number i32) b←(%number i32))} do
#     # a : Int32
#     # b : Int32
#   end
#
#   # All other keyword arguments to match* are type cast declarations:
#   matchpi %{(realpath a_string)}, a: Path do
#     # a : Path
#   end
#
#   # givenp, givenpi, givenpiT are the same as `match*`, but they use `ML.terms`
#   # instead of `ML.term` for parsing. The following:
#   givenpiT %{(double @xs_) (pulse @xs_ x←(%number +i32))} do
#     # x : Int32
#   end
#
#   # ... is the same as writing:
#   matchpiT %{((double @xs_) (pulse @xs_ x←(%number +i32)))} do
#     # x : Int32
#   end
#
#   # Since WwML uses S-expressions, there's lots of parens, and this can get
#   # intimidating. The less parens there are, the better; hence givenp*.
#
#   # Catch-all clause.
#   otherwise do
#     # ...
#   end
# end
# ```
#
# NOTE: `Term#to` is used for type casting.
module Ww::Term::Case
  # The regex used to find captures in `i` variants such as `matchpi`.
  RE_CAPTURES = /([#a-zA-Z_][\w-]*?)(?:_(?:any|number|symbol|string|boolean|dict|blob)?[+*⋮]?\b|←|⋮)|[±]([\w-]+)|\((?:%let)\s+([a-zA-Z][\w-]*)/

  # :nodoc:
  RE_TYPES_BLANK = /(?!<\w)([#a-zA-Z_][\w-]*?)(?:_(number|symbol|string|boolean|dict|blob)⋮?|⋮)(?![\w+-])/
  # :nodoc:
  RE_TYPES_PLUSMINUS = /(?!<\w)±([#a-zA-Z_][\w-]*)/
  # :nodoc:
  RE_TYPES_INT = /(?!<\w)([#a-zA-Z_][\w-]*)←\(%number\s[+-]?i(8|16|32|64|128)!?\)/
  # :nodoc:
  RE_TYPES_UINT = /(?!<\w)([#a-zA-Z_][\w-]*)←\(%number\su(8|16|32|64|128)\)/

  # :nodoc:
  #
  # MACRO-ONLY: maps match calls to descriptions/decompositions thereof.
  DESCTAB = {
    match:    {tag: :term},
    matchp:   {tag: :"ml/term", parse: true},
    matchpi:  {tag: :"ml/term", parse: true, infer: true},
    matchpiT: {tag: :"ml/term", parse: true, infer: true, typed: true},
    givenp:   {tag: :"ml/terms", parse: true},
    givenpi:  {tag: :"ml/terms", parse: true, infer: true},
    givenpiT: {tag: :"ml/terms", parse: true, infer: true, typed: true},
  }

  # :nodoc:
  #
  # MACRO-ONLY: describes how to extract types from ML string patterns given
  # to *iT variants.
  #
  # - `pattern` points to the regex to use.
  # - `name` specifies the regex group of the capture name (e.g. `x` in `x_number`).
  # - `cast` can be used to cast to that type without further deliberation.
  # - `options` can be used instead of `cast` to map pairs of `{<regex group>, <value>}`
  #   to the desired cast type.
  TYPETAB = {
    {
      pattern: RE_TYPES_BLANK,
      name:    1,
      options: {
        {2, "number"}  => ::Ww::Term::Num,
        {2, "string"}  => ::Ww::Term::Str,
        {2, "symbol"}  => ::Ww::Term::Sym,
        {2, "boolean"} => ::Ww::Term::Boolean,
        {2, "dict"}    => ::Ww::Term::Dict,
        {2, "blob"}    => ::Ww::Term::Blob,
      },
    },
    {
      pattern: RE_TYPES_PLUSMINUS,
      name:    1,
      cast:    ::Ww::Term::Num,
    },
    {
      pattern: RE_TYPES_INT,
      name:    1,
      options: {
        {2, "8"}   => ::Int8,
        {2, "16"}  => ::Int16,
        {2, "32"}  => ::Int32,
        {2, "64"}  => ::Int64,
        {2, "128"} => ::Int128,
      },
    },
    {
      pattern: RE_TYPES_UINT,
      name:    1,
      options: {
        {2, "8"}   => ::UInt8,
        {2, "16"}  => ::UInt16,
        {2, "32"}  => ::UInt32,
        {2, "64"}  => ::UInt64,
        {2, "128"} => ::UInt128,
      },
    },
  }

  # The low-level description of a `match` clause which all other variants
  # are reduced to in `Case.scan` (`Term.case`).
  defrecord MatchSpec,
    pattern : Term,
    cue0 : Term::Sym?,
    cue1 : Term::Sym?,
    cue2 : Term::Sym?

  struct MatchSpec
    # Returns `true` if *dict* has all the cues from *spec*, meaning a match
    # should be attempted.
    def self.match_possible?(spec : MatchSpec, dict : Term::Dict) : Bool
      return false if (cue0 = spec.cue0) && !dict.probably_includes?(cue0)
      return false if (cue1 = spec.cue1) && !dict.probably_includes?(cue1)
      return false if (cue2 = spec.cue2) && !dict.probably_includes?(cue2)

      true
    end
  end

  # A case matcher, *matcher* for short, is a collection of patterns associated
  # with a *pattern matching engine* such as `M0` or `M1`. A matcher is
  # constructed and cached globally by `Term.case`; the latter then repeatedly
  # calls `scan` on each new matchee.
  #
  # Due to caching, `scan` must be thread-safe.
  #
  # NOTE: this automatically makes the includer extend `MatcherClass`.
  module Matcher
    macro included
      extend ::Ww::Term::Case::MatcherClass
    end

    # Yields match envs and indices for each matching spec. Indices refer into
    # the slice passed to `MatcherClass.compile`.
    abstract def scan(matchee : Term, *, env : Term::Dict, & : Term::Dict, Int32 ->)
  end

  # Class-side requirements of `Matcher`.
  module MatcherClass
    abstract def compile(specs : Slice(MatchSpec)) : Matcher
  end

  # A case matcher that uses *Engine*.
  #
  # *Engine* must respond to `match?(pattern : Term, matchee : Term, *, env : Term::Dict)`.
  class MM(Engine)
    include Matcher

    # :nodoc:
    def initialize(@specs : Slice(MatchSpec))
      {% if Engine == M0 || Engine == M1 %}
        {% raise "MM(M0)/MM(M1) not allowed, use MM0 and MM1" %}
      {% end %}
    end

    def self.compile(specs : Slice(MatchSpec)) : Matcher
      new(specs)
    end

    def scan(matchee : Term, *, env : Term::Dict, &)
      dict = matchee.as_d?

      @specs.each_with_index do |spec, index|
        if dict
          next unless MatchSpec.match_possible?(spec, dict)
        end

        next unless env1 = Engine.match?(spec.pattern, matchee, env: env)

        result = yield env1, index
        unless result.is_a?(Continue.class)
          return result
        end
      end

      Continue
    end
  end

  # A case matcher that uses the M0 pattern matching engine, `Ww::M0`.
  class MM0
    include Matcher

    # :nodoc:
    def initialize(@arms : Slice({Slice(M0::Insn), MatchSpec}))
    end

    def self.compile(specs : Slice(MatchSpec)) : MM0
      new(arms: specs.to_readonly_slice { |spec| {M0.compile(spec.pattern), spec} })
    end

    def scan(matchee : Term, *, env : Term::Dict, &)
      dict = matchee.as_d?

      @arms.each_with_index do |(insns, spec), index|
        if dict
          next unless MatchSpec.match_possible?(spec, dict)
        end

        next unless env1 = M0.match?(env, insns, matchee)

        result = yield env1, index
        unless result.is_a?(Continue.class)
          return result
        end
      end

      Continue
    end
  end

  # A case matcher that uses the M1 pattern matching engine, `Ww::M1`.
  class MM1
    include Matcher

    # :nodoc:
    def initialize(@arms : Slice({M1::Op::Any, MatchSpec}))
    end

    # Compiles *specs* in single-threaded mode.
    def self.compile(specs : Slice(MatchSpec)) : MM1
      new(arms: specs.to_readonly_slice { |spec| {M1.operator(spec.pattern), spec} })
    end

    def scan(matchee : Term, *, env : Term::Dict, &)
      dict = matchee.as_d?

      @arms.each_with_index do |(op, spec), index|
        if dict
          next unless MatchSpec.match_possible?(spec, dict)
        end

        next unless M1.probably_matches?(op, matchee)
        next unless env1 = M1.match?(env, op, matchee)

        result = yield env1, index
        unless result.is_a?(Continue.class)
          return result
        end
      end

      Continue
    end
  end

  # See `continue`.
  module Continue
  end

  # :nodoc:
  MATCHERS = MatcherArray(MATCHERS_CAPACITY).new

  # :nodoc:
  #
  # You should increase this if the matcher array overflows. There's no good
  # way to do this automatically in Crystal (none that I can think of that is).
  MATCHERS_CAPACITY = 256

  # :nodoc:
  #
  # WARNING: Never ever access this at runtime. Only at compile-time.
  MATCHERS_SIZE = [0]

  # :nodoc:
  struct MatcherArray(N)
    alias MatcherRef = Case::Matcher*

    def initialize
      @matchers = Slice(MatcherRef).new(N, MatcherRef.null)
    end

    def put_if_absent(id : Int32, &fn : -> Case::Matcher) : Case::Matcher
      unless 0 <= id < @matchers.size
        raise IndexError.new
      end

      # See e.g. https://github.com/crystal-lang/crystal/issues/9078
      #
      # For now we'll use :nodoc: Atomic::Ops
      ref = Atomic::Ops.load(@matchers.to_unsafe + id, :acquire, volatile: true)
      unless ref.null?
        return ref.value
      end

      matcher = fn.call

      ref = MatcherRef.malloc(1)
      ref.value = matcher

      Atomic::Ops.store(@matchers.to_unsafe + id, ref, :release, volatile: true)

      matcher
    end
  end

  # A higher-order macro used to *define* a case macro like `Case.scan`. This
  # macro specifically can parse the body of a case, giving you an array of
  # branches at macro-time. The array of branches is saved under *cont*'s first
  # argument name. *cont* itself is a macro body. Due to how Crystal's parsing
  # works, you will probably need to wrap *cont*'s body in a macro begin-end
  # block anyway.
  #
  # Each branch is a named tuple of the following structure:
  #
  # ```text
  # <branch>
  #   {
  #     pattern:  <pattern>,
  #     location: String,
  #     captures: Hash(Symbol, Symbol),
  #     cast:     Hash(Symbol, _),
  #     cues:     Array(Symbol), # at most 2
  #     body:     <block body>,
  #   }
  #
  # <pattern>
  #   term <arg>
  #     Raw term (you should wrap <arg> in Term.of).
  #   ml/term <arg: String>
  #     ML string that should be parsed using ML.term.
  #   ml/terms <arg: String>
  #     ML string that should be parsed using ML.terms.
  # ```
  macro def_caselike(call, &cont)
    {%
      unless call.is_a?(Call)
        call.raise "expected a call"
      end

      unless cont
        raise "expected a block"
      end

      unless cont.args.size == 3
        raise "def_caselike() block takes exactly three arguments: id, branches, sink"
      end

      id, branches, sink = cont.args
    %}

    {{@caller.doc}}
    macro {{call.name}}({{call.args.splat(",")}} &block)
      \{%
        # Obtain a fresh case id.
        %matchers_size = ::Ww::Term::Case::MATCHERS_SIZE[0]
        %matchers_capacity = ::Ww::Term::Case::MATCHERS_CAPACITY
        if %matchers_size + 1 > %matchers_capacity
          ::Ww::Term::Case::MATCHERS_CAPACITY.raise "matcher array capacity too small (#{%matchers_size + 1} > #{%matchers_capacity}; increase capacity)"
        end

        %matcher_id = %matchers_size
        ::Ww::Term::Case::MATCHERS_SIZE[0] += 1

        # Normalize body to an array of "statements".
        %stmts = block.body
        if %stmts.is_a?(Expressions)
          %stmts = %stmts.expressions
        elsif %stmts.is_a?(Nop)
          %stmts = [] of ::NoReturn
        else
          %stmts = [%stmts]
        end

        %branches = [] of ::NoReturn
        %sink = nil

        # Parse and validate the body.
        %stmts.each do |%stmt|
          unless %stmt.is_a?(Call)
            %stmt.raise "expected a call"
          end

          unless %stmt.name.id =~ /otherwise|(?:matchp?|givenp)(?:i(?:T)?)?/
            %stmt.raise "unrecognized call `#{%stmt.name.id}`, expected one of: match[p[i[T]]], givenp[i[T]], otherwise"
          end

          unless %stmt.block
            %stmt.raise "#{%stmt.name.id}: missing block"
          end

          if %stmt.args.empty? && %stmt.name == :otherwise
            if %sink
              %stmt.raise "multiple otherwise clauses not allowed"
            end

            %sink = %stmt.block
          elsif %stmt.args.empty?
            %stmt.raise "#{%stmt.name.id}: expected at least one argument"
          else
            # Destructure keyword arguments.
            %kwarg_cue = nil
            %kwarg_cues = nil
            %kwarg_cast = {} of ::NoReturn => ::NoReturn

            if %kwargs = %stmt.named_args
              %kwargs.each do |%kwarg|
                if %kwarg.name == :cue
                  %kwarg_cue = %kwarg.value
                elsif %kwarg.name == :cues
                  %kwarg_cues = %kwarg.value
                else
                  %kwarg_cast[%kwarg.name.symbolize] = %kwarg.value
                end
              end
            end

            # Validate cue: _.
            if %kwarg_cue
              if %kwarg_cue.is_a?(SymbolLiteral)
                %kwarg_cue = { %kwarg_cue }
              elsif %kwarg_cue.is_a?(TupleLiteral)
                %kwarg_cue.each do |cue|
                  unless cue.is_a?(SymbolLiteral)
                    cue.raise "expected a symbol literal"
                  end
                end
              else
                %kwarg_cue.raise "expected a symbol literal"
              end
            end

            # Validate cues: _.
            if %kwarg_cues
              unless %kwarg_cues.is_a?(TupleLiteral)
                %kwarg_cues.raise "expected a tuple literal"
              end

              unless %kwarg_cues.size == %stmt.args.size
                %kwarg_cues.raise "cue arity mismatch: expected #{%stmt.args.size} cue(s), but got #{%kwarg_cues.size} (hint: you can use `nil` to pad)"
              end

              %kwarg_cues.each do |%cue|
                unless %cue.is_a?(SymbolLiteral) || %cue.is_a?(NilLiteral)
                  %cue.raise "expected a symbol literal or nil"
                end
              end
            end

            # Start parsing arguments. We do this for each argument because passing
            # multiple arguments to e.g. `matchpi`:
            #
            #   matchpi %{a}, %{b}, %{c} { puts "Hi" }
            #
            # ... is the same as writing:
            #
            #   matchpi %{a} { puts "Hi" }
            #   matchpi %{b} { puts "Hi" }
            #   matchpi %{c} { puts "Hi" }
            %stmt.args.each_with_index do |%arg, %index|
              %desc = ::Ww::Term::Case::DESCTAB[%stmt.name.symbolize]
              %location = "#{%arg.filename.id}:#{%arg.line_number}:#{%arg.column_number}"

              # Validate arguments of calls like `matchpi`, `givenpi` etc.
              if %desc[:parse]
                unless %arg.is_a?(StringLiteral)
                  %stmt.raise "expected a string literal argument"
                end
              end

              # Collect cues for this particular pattern arg. `cue: _` is shared
              # between all patterns and with `cues: {_, _, ..., _}`, we select
              # the cue corresponding to this particular pattern arg.
              %cues = [] of ::NoReturn
              %cues += %kwarg_cue if %kwarg_cue
              %cues << %kwarg_cues[%index] if %kwarg_cues && %kwarg_cues[%index]

              if %cues.size > 3
                %arg.raise "maximum number of cues per pattern is 3, got #{%cues.size} cues"
              end

              # Parse and infer captures. *icaps* refers to inferred captures, which
              # are captures we extract from the pattern string if inference is enabled.
              # *bcaps* refers to user-provided captures from the block's arg list.
              %bcaps = %stmt.block.args
              %icaps = [] of ::NoReturn

              if %desc[:infer]
                %icaps = %arg
                  .scan(::Ww::Term::Case::RE_CAPTURES)
                  .map { |%match| (%match[1] || %match[2]).id }
                  .uniq
              end

              %caps = {} of ::NoReturn => ::NoReturn

              # Normalize inferred captures though since WwML allows `-` etc. in symbols.
              %icaps.each do |%icap|
                %norm = %icap.gsub(/-/, "_").gsub(/#/, "").symbolize

                %caps.each do |%k, %v|
                  if %norm == %v
                    %arg.raise "Crystal representations of these captures collide: #{%icap}, #{%v}"
                  end
                end

                %caps[%icap.symbolize] = %norm
              end

              # Block captures come from Crystal and are already valid ids.
              %bcaps.each do |%bcap|
                %caps[%bcap.symbolize] = %bcap.symbolize
              end

              # Finally, detect the cast types of things if typing is enabled. Inherit
              # types from kwargs, so that e.g.:
              #
              #   givenpiT %{±x y_}, x: String, y: Path do
              #     typeof(x) # => Term::Num
              #     typeof(y) # => Path
              #   end
              %cast = {} of ::NoReturn => ::NoReturn
              %kwarg_cast.each do |%cap, %type|
                %cast[%cap] = %type
              end

              if %desc[:infer] && %desc[:typed]
                ::Ww::Term::Case::TYPETAB.each do |%row|
                  %arg.scan(%row[:pattern].resolve).each do |%match|
                    %icap = %match[%row[:name]].id
                    %norm = %icap.gsub(/-/, "_").gsub(/#/, "").symbolize

                    # Cast immediately, no more info in the pattern.
                    if %type = %row[:cast]
                      %cast[%norm] = %type
                    elsif %options = %row[:options]
                      %match.each do |%key, %value|
                        if !%cast[%norm] && (%type = %options[{ %key, %value }])
                          %cast[%norm] = %type
                        end
                      end
                    end
                  end
                end
              end

              if %desc[:tag] == :term
                %call = "::Ww::Term.of(#{%arg})".id
              elsif %desc[:tag] == :"ml/term"
                %call = "::Ww::ML.term(#{%arg})".id
              elsif %desc[:tag] == :"ml/terms"
                %call = "::Ww::ML.terms(#{%arg})".id
              else
                ::Ww::Term::Case::DESCTAB.raise "invalid tag: #{%tag}"
              end

              # Submit the branch.
              %branches << {
                pattern: {call: %call, src: %arg},
                location: %location,
                captures: %caps,
                cast: %cast,
                cues: %cues,
                body: %stmt.block.body,
              }
            end
          end
        end

        {{id}} = %matcher_id
        {{branches}} = %branches
        {{sink}} = %sink
      %}

      {{yield}}
    end
  end

  # The implementation of `Term.case`.
  #
  # - *matcher* is the case matcher *class* to use for compiling and matching patterns.
  # - *matchee* is the term to match.
  # - *env* is the base environment.
  # - *block_type* can be `:proc` or `:block`.
  def_caselike scan(matcher, matchee, env, block_type) do |id, branches, sink|
    {% begin %}
      {%
        if branches.empty?
          raise "empty case not allowed"
        end

        unless block_type == :proc || block_type == :block
          raise "block_type must be :proc or :block"
        end
      %}\

      %matcher = ::Ww::Term::Case::MATCHERS.put_if_absent({{id}}) do
        %specs = Slice[
          {% for branch in branches %}\
            {% call = branch[:pattern][:call]
               cue0 = branch[:cues][0]
               cue1 = branch[:cues][1]
               cue2 = branch[:cues][2] %}\
            ::Ww::Term::Case::MatchSpec.new({{call}}, Term[{{cue0}}], Term[{{cue1}}], Term[{{cue2}}]),
          {% end %}\
        ]

        {{matcher}}.compile(%specs)
      end

      %matchee = Term.of({{matchee}})

      {%
        # NOTE: The cast is necessary because otherwise, the thing would do
        # dynamic dispatch, copying the block some number of times (in our case
        # maybe 8-ish?). Inside each copy of the block at that point is a giant
        # `case` with dozens to hundreds of `when`s. Each `when` opens a scope
        # with `pass { }` and does its work. All of this is copied. That's
        # catastrophic. Thousands of locals. Hundreds of thousands of instructions.
        # It took me a few hours to debug this blowup; so that's the reason why
        # the cast must be here.
      %}
      %result = %matcher.as({{matcher}}).scan(%matchee, env: {{env}}) do |%env, %index|
        case %index
        {% for branch, i in branches %}\
        when {{i}}
          {% if block_type == :block %}
          pass do
          {% elsif block_type == :proc %}
          (-> do
            pass do
          {% end %}
            {% for capture, var in branch[:captures] %}\
              {% if type = branch[:cast][var] %}\
                {{var.id}} = (%env[{{capture}}]? || raise("#{ {{branch[:location]}} }: missing capture `{{capture.id}}`")).to({{type}})
              {% else %}\
                {{var.id}} = %env[{{capture}}]? || raise("#{ {{branch[:location]}} }: missing capture `{{capture.id}}`")
              {% end %}\
            {% end %}
            {{branch[:body]}}
          {% if block_type == :block %}
          end
          {% elsif block_type == :proc %}
            end
          end).call
          {% end %}
        {% end %}
        else
          unreachable
        end
      end

      if %result.is_a?(::Ww::Term::Case::Continue.class)
        {% if sink %}
        pass {{sink}}
        {% else %}
        raise ArgumentError.new("unrecognized term: #{ML.compact(%matchee)}")
        {% end %}
      else
        %result
      end
    {% end %}
  end
end

module Ww
  # Expands to `next` with a special sentinel module, `Continue`, for fallthrough to
  # the next case.
  #
  # You can return `Term::Case::Continue` manually if you dislike the `next` (e.g. if you are too
  # deep to simply "next out").
  #
  # ```
  # Term.case(term) do
  #   givenpi %{x_ y_} do
  #     continue unless good?(x)
  #     continue unless good?(y)
  #     puts({x, y})
  #   end
  # end
  # ```
  macro continue
    next ::Ww::Term::Case::Continue
  end
end
