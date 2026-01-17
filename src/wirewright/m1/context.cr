module Ww::M1next
  # :nodoc:
  #
  # We use *cst* as an abbreviation for *ConStrainT*. These objects are constraints
  # associated with a particular capture. Multiple constraints are ANDed using `Many`.
  module Cst
    extend self

    alias Any = One | Many
    alias One = Lookup | NegLookup | NegLookupRef | Compares

    defrecord Lookup, table : Tzip, op : O::Any
    defrecord NegLookup, table : Tzip
    defrecord NegLookupRef, table : Tzip, name : Term
    defrecord Many, children : Slice(One)
    defrecord Compares, op : Symbol, rhs : Term::Num

    def join(a : Any, b : Any)
      case {a, b}
      in {One, One}   then Many.new(Slice[a.as(One), b.as(One)])
      in {One, Many}  then Many.new(b.children.prepend(a))
      in {Many, One}  then Many.new(a.children.append(b))
      in {Many, Many} then Many.new(a.children + b.children)
      end
    end
  end

  # :nodoc:
  #
  # ### Selector mode
  #
  # Outside of *selector mode*, captures refine choices but *do not* trigger
  # mismatch if their proposal is not in the choice set.
  #
  # In selector mode, absence of proposal in the capture's choice set means
  # immediate mismatch.
  #
  # ### Refs
  #
  # *Refs* are, roughly speaking, named logs. Whereas normally logs are associated
  # with parts of a matchee, refs exist primarily for consumption by the backmap
  # engine and lack corresponding term(s) in the underlying matchee.
  #
  # For example, When you say `((%slot a))`, more commonly written as ``(`a)``,
  # *a* here is a ref: it refers to a log (i.e., we know how to get to *a*), but
  # there's no value for *a*. Something similar can be said about `{¦ -x_}`. There's
  # no value for *x*: in fact, that's exactly what the pattern is matching for. But
  # since we'd like to refer to "how to get to *x*" anyway, we have refs.
  struct Context
    alias EnvMap = ListMap(Term, Tzip)
    alias EnvMapArena = Arena(List({Term, Tzip}), 32)

    alias CstMap = ListMap(Term, Cst::Any)
    alias CstMapArena = Arena(List({Term, Cst::Any}), 8)

    alias RefMap = ListMap(Term, Log::Sealed)
    alias RefMapArena = Arena(List({Term, Log::Sealed}), 4)

    # Choices are relatively rare in practice so we're just using Pf::Map/Set
    # for them instead of the fancy Arena stuff.
    alias ChoiceMap = Pf::Map(Term, Pf::Set(Term))

    # Plans are by far the thing we allocate the most, so we give them
    # plenty of stack-space.
    alias PlanArena = Arena(List(Action::Any), 160)

    alias ContextArena = Arena(Context::Payload, 64)

    # :nodoc:
    defcase ArenaRow,
      cdatas : ContextArena,
      plans : PlanArena,
      envtabs : EnvMapArena,
      csttabs : CstMapArena,
      reftabs : RefMapArena,
      copying: false

    # :nodoc:
    defcase Payload,
      arenas : ArenaRow,
      envtab : EnvMap,
      csttab : CstMap,
      reftab : RefMap,
      choicetab : ChoiceMap,
      selector : Bool,
      copying: false

    # :nodoc:
    def initialize(@payload : Payload)
    end

    def self.new(env : Term::Dict, & : Context ->) : Nil
      # NOTE: Right now, measuring with LLDB, release mode, this takes -- rounding upwards by
      # a few KB to account for misunderstandings on my end -- about 32KB of stack memory.
      # Which is OK, I guess. We normally have 8 MB in Linux so 32KB is fine.
      nested_scopes(EnvMapArena, CstMapArena, RefMapArena, ContextArena, PlanArena) do |envtabs, csttabs, reftabs, cdatas, plans|
        envtab = EnvMap.new

        # NOTE: in the vast majority of cases *env* is empty. No work is done here.
        # The only major supplier of nonempty *env*s is `Alloy.render`.
        env.each_entry do |key, value|
          envtab = EnvMap.assoc(envtabs, envtab, key, Tzip.new(value, Log.none))
        end

        arenas = stack_alloc ArenaRow.new(cdatas, plans, envtabs, csttabs, reftabs)

        cdata = cdatas.construct(arenas, envtab,
          csttab: CstMap.new,
          reftab: RefMap.new,
          choicetab: ChoiceMap.new,
          selector: false,
        )

        yield new(cdata)
      end
    end

    def change(
      envtab = self.envtab,
      csttab = self.csttab,
      reftab = self.reftab,
      choicetab = self.choicetab,
      selector = self.selector,
    ) : Context
      cdata = cdatas.construct(arenas, envtab, csttab, reftab, choicetab, selector)

      Context.new(cdata)
    end

    def sibling(
      envtab = EnvMap.new,
      csttab = CstMap.new,
      reftab = RefMap.new,
      choicetab = ChoiceMap.new,
      selector = self.selector,
    ) : Context
      cdata = cdatas.construct(arenas, envtab, csttab, reftab, choicetab, selector)

      Context.new(cdata)
    end

    delegate :arenas, :envtab, :csttab, :reftab, :choicetab, :selector, to: @payload
    delegate :cdatas, :plans, :envtabs, :csttabs, :reftabs, to: arenas

    def interject(plan : Plan, action : Action::Any) : Plan
      List.append(plans, plan, action)
    end

    def interject(plan : Plan, *actions : Action::Any)
      actions.reverse_each do |action|
        plan = interject(plan, action)
      end

      plan
    end

    def action?(plan : List(Action::Any))
      plan.last
    end

    def action?(plan : Nil)
    end

    def dequeue(plan : List(Action::Any))
      plan.prior?
    end

    def dequeue(plan : Nil)
      raise IndexError.new
    end

    def capture?(capture : Term) : Tzip?
      envtab[capture]?
    end

    def has_capture?(capture : Term) : Bool
      !!capture?(capture)
    end

    def has_captures?(captures : Enumerable(Term)) : Bool
      captures.all? { |capture| has_capture?(capture) }
    end

    def choices?(capture : Term) : Pf::Set(Term)?
      choicetab[capture]?
    end

    # Returns a sub-map of the current envtab, leaving only entries for captures
    # in the given list of *captures*. If some captures are missing, they are
    # left out.
    def envtab(captures : Enumerable(Term)) : EnvMap
      captures.reduce(EnvMap.new) do |memo, capture|
        value = capture?(capture) || next memo

        EnvMap.assoc(envtabs, memo, capture, value)
      end
    end

    # Returns a sub-map of the current choicetab, leaving only entries for captures
    # in the given list of *captures*. If some captures are missing, they are left out.
    def choicetab(captures : Enumerable(Term)) : ChoiceMap
      captures.reduce(ChoiceMap.new) do |memo, capture|
        value = choices?(capture) || next memo

        memo.assoc(capture, value)
      end
    end

    def cst?(capture : Term) : Cst::Any?
      csttab[capture]?
    end

    def update?(capture : Term, &) : {Context, Bool}
      value0 = envtab.fetch(capture) { return self, false }
      value1 = yield value0
      if value0 == value1
        return self, true
      end

      {change(envtab: EnvMap.assoc(envtabs, envtab, capture, value1)), true}
    end

    # WARNING: overwrites the current capture at *key*. This probably isn't
    # what you're looking for.
    def assoc(key : Term, value : Tzip)
      change(envtab: EnvMap.assoc(envtabs, envtab, key, value))
    end

    def join(key : Term, cst : Cst::Any)
      cst0 = csttab.fetch(key) do
        return change(csttab: CstMap.assoc(csttabs, csttab, key, cst))
      end

      cst1 = Cst.join(cst0, cst)

      change(csttab: CstMap.assoc(csttabs, csttab, key, cst1))
    end

    def join(key : Term, ref : Log::None)
      self
    end

    def join(key : Term, ref : Log::Sealed)
      ref0 = reftab.fetch(key) do
        return change(reftab: RefMap.assoc(reftabs, reftab, key, ref))
      end

      ref1 = Log.seal(Log.join(ref0, ref))

      change(reftab: RefMap.assoc(reftabs, reftab, key, ref1))
    end

    def intersect(key : Term, choices : Pf::Set(Term) | Set(Term)) : Context?
      current = choicetab[key]?
      if current.nil?
        return change(choicetab: choicetab.assoc(key, choices.to_pf_set))
      end

      mid = current & choices.to_pf_set
      change(choicetab: choicetab.assoc(key, mid))
    end
  end
end
