require "./src/wirewright"

module Rewrite
  alias Any = Some | None
  alias Some = One | Many

  record None do
    def term?
    end

    def each(&)
    end

    def reduce(& : Term -> Rewrite::Any)
      self
    end

    def map(default, &)
      One.new(Term.of(yield default))
    end

    def map(&)
      self
    end

    def +(other : Any)
      other
    end

    def diff(orig : Term)
      self
    end
  end

  record One, term : Term do
    def term?
      term
    end

    def each(&)
      yield term
    end

    def reduce(& : Term -> Rewrite::Any)
      yield term
    end

    def map(default, &)
      One.new(Term.of(yield term))
    end

    def map(&)
      One.new(Term.of(yield term))
    end

    def +(other : One)
      if term == other.term
        self
      else
        Many.new(Term[term, other.term])
      end
    end

    def +(other : Many)
      if other.list.itemsize == 1 && other[0] == term
        self
      else
        Many.new(other.prepend(term))
      end
    end

    def +(other : None)
      self
    end

    def diff(orig : Term)
      term == orig ? Rewrite.none : self
    end
  end

  record Many, list : Term::Dict do
    def term?
      Term.of(list)
    end

    def each(&)
      yield Term.of(list)
    end

    def reduce(& : Term -> Rewrite::Any)
      changed = false

      newlist = Term::Dict.build do |commit|
        list.items.each do |item|
          case offspring = yield item
          in Rewrite::None
            commit << item
          in Rewrite::One
            commit << offspring.term
            changed = true
          in Rewrite::Many
            commit.concat(offspring.list.items)
            changed = true
          end
        end
      end

      unless changed
        return None.new
      end

      if newlist.size == 1
        return One.new(newlist[0])
      end

      Many.new(newlist)
    end

    def map(default, &)
      map { |item| yield item }
    end

    def map(&)
      Many.new(list.transaction do |commit|
        list.each_item_with_index do |item, index|
          commit.with(index, yield item)
        end
      end)
    end

    def +(other : One)
      if list.empty? || (list.itemsize == 1 && list[0] == other.term)
        other
      else
        Many.new(list.append(other.term))
      end
    end

    def +(other : Many)
      if list.empty?
        other
      elsif other.list.empty?
        self
      else
        Many.new(list.transaction &.concat(other.list.items))
      end
    end

    def diff(orig : Term)
      if list == Term[{orig}]
        return Rewrite.none
      end
      if list.itemsize == 1
        return Rewrite.one(list[0])
      end
      self
    end
  end

  def self.none
    None.new
  end

  def self.one(term)
    One.new(Term.of(term))
  end

  def self.many(list)
    Many.new(Term[list])
  end
end

# In `ProcRuleset`, rules are Crystal procs (later, native code). Useful for
# implementing primitives.
#
# TODO: This is DEPRECATED and will be removed.
struct ProcRuleset
  alias ProcRule = Term::Dict -> Rewrite::Any
  alias ProcBackmap = Term::Dict -> Term::Dict

  struct Builder
    def initialize(@ruleary : Array({Term, ProcRule | ProcBackmap}))
    end

    def rule(pattern : Term, &fn : ProcRule) : Nil
      @ruleary << {pattern, fn}
    end

    def backmap(pattern : Term, &fn : ProcBackmap) : Nil
      @ruleary << {pattern, fn}
    end

    def rulep(ml : String, &fn : ProcRule) : Nil
      rule(ML.term(ml), &fn)
    end

    def backmapp(ml : String, &fn : ProcBackmap) : Nil
      backmap(ML.term(ml), &fn)
    end

    # :nodoc:
    macro pi(methodp, ml, &block)
      {% icaps = ml.scan(::Ww::Term::Case::RE_CAPTURES).map { |match| (match[1] || match[2]).id }.uniq %}
      {% location = "#{block.filename.id}:#{block.line_number}:#{block.column_number}" %}

      {{methodp}}({{ml}}) do |%env|
        {% for icap in icaps %}
          {% icap_id = icap.id.gsub(/-/, "_") %}
          {% unless block.args.any? { |arg| arg.id == icap_id } %}
            {{icap_id}} = (%env[{{icap.id.symbolize}}]? || raise "case: #{ {{location}} }: missing capture '{{icap.id}}'")
          {% end %}
        {% end %}

        pass(
          {% for capture in block.args %}
            (%env[{{capture.id.symbolize}}]? || raise "rulepi: #{ {{location}} }: missing capture '{{capture.id}}'"),
          {% end %}
        ) {{block}}
      end
    end

    macro rulepi(ml, &block)
      pi(rulep, {{ml}}) {{block}}
    end

    macro backmappi(ml, &block)
      pi(backmapp, {{ml}}) {{block}}
    end

    macro rulepi1(ml, &block)
      rulepi({{ml}}) do {% unless block.args.empty? %} |{{block.args.splat}}| {% end %}
        %result = pass do
          {{block.body}}
        end

        Rewrite::One.new(Term.of(%result))
      end
    end
  end

  # :nodoc:
  def initialize(
    @pset : M1::PatternSet(Term),
    @rules : Hash(UInt32, ProcRule)?,
    @backmaps : Hash(UInt32, ProcBackmap)?,
  )
  end

  def self.build(&)
    ruleary = [] of {Term, ProcRule | ProcBackmap}

    builder = Builder.new(ruleary)
    with builder yield builder

    decls = Term::Dict.build do |commit|
      ruleary.each_with_index do |(pattern, proc), index|
        case proc
        in ProcRule    then commit << {:rule, index, pattern}
        in ProcBackmap then commit << {:backmap, index, pattern}
        end
      end
    end

    selector = ML.term(%[[type←(%any rule backmap) index←(%number +i32) pattern_]])

    rules = backmaps = nil

    index = 0u32
    pset = M1::PatternSet(Term).select(selector, Term.of(decls)) do |_, env|
      _, proc = ruleary[env[:index].to(Int32)]

      case env[:type]
      when Term.of(:rule)
        rules ||= {} of UInt32 => ProcRule
        rules[index] = proc.as(ProcRule)
      when Term.of(:backmap)
        backmaps ||= {} of UInt32 => ProcBackmap
        backmaps[index] = proc.as(ProcBackmap)
      else
        unreachable
      end

      index += 1

      true
    end

    new(pset, rules, backmaps)
  end

  private def rule?(index) : ProcRule?
    @rules.try { |rules| rules[index]? }
  end

  private def backmap?(index) : ProcBackmap?
    @backmaps.try { |backmaps| backmaps[index]? }
  end

  def call(matchee matchee0 : Term) : Rewrite::Any
    @pset.query(matchee0) do |envs, index|
      next unless env = envs.single?

      if rule = rule?(index)
        offspring = rule.call(env)
      elsif backmap = backmap?(index)
        raise "not supported"
      end

      case offspring
      in Nil
        unreachable
      in Rewrite::One
        different = matchee0 != offspring.term
      in Rewrite::Many
        different = Term[{matchee0}] != offspring.list
      in Rewrite::None
        different = false
      end

      return different ? offspring : Rewrite::None.new
    end

    Rewrite::None.new
  end
end
