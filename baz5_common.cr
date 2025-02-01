require "./wirewright"

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

    def diff(orig : Term)
      list == Term[{orig}] ? Rewrite.none : self
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
      rule(ML.parse1(ml), &fn)
    end

    def backmapp(ml : String, &fn : ProcBackmap) : Nil
      backmap(ML.parse1(ml), &fn)
    end

    # :nodoc:
    macro pi(methodp, ml, &block)
      {% icaps = ml.scan(::Ww::Term::CaseContext::RE_CAPTURES).map { |match| (match[1] || match[2]).id }.uniq %}
      {% location = "#{block.filename.id}:#{block.line_number}:#{block.column_number}" %}

      {{methodp}}({{ml}}) do |%env|
        {% for icap in icaps %}
          {% unless block.args.any? { |arg| arg.id == icap.id } %}
            {{icap.id}} = (%env[{{icap.id.symbolize}}]? || raise "case: #{ {{location}} }: missing capture '{{icap.id}}'")
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
    @pset : PatternSet,
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

    selector = ML.parse1(%[(type←(%any rule backmap) index←(%number +i32) pattern_)])

    rules = backmaps = nil

    index = 0u32
    pset = PatternSet.select(selector, Term.of(decls)) do |_, env|
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
    case pr = @pset.response(matchee0)
    in Pr::One
      if rule = rule?(pr.pattern.index)
        offspring = rule.call(pr.env)
      end

      if backmap = backmap?(pr.pattern.index)
        unless pr.env.includes?(:"(keypaths)")
          pr = pr.pattern.response(matchee0, keypaths: true).as(Pr::One)
        end
        backspec = backmap.call(pr.env)
        matchee1 = M1.backmap(pr.envs, Term.of(backspec), matchee0)
        offspring = Rewrite::One.new(matchee1)
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

      different ? offspring : Rewrite::None.new
    in Pr::Many
      raise "pr::many not implemented"
    in Pr::Neg
      Rewrite::None.new
    end
  end
end

module Rule
  extend self

  alias Any = Template | BackmapOne | BackmapMany

  record Template, body : Term
  record BackmapOne, backspec : Term
  record BackmapMany, toplevel : Term, backspec : Term
end

struct Ruleset
  # :nodoc:
  def initialize(@pset : PatternSet, @rules : Slice(Rule::Any))
  end

  def self.select(selector, base)
    rules = [] of Rule::Any

    pset = PatternSet.select(selector, base) do |normp, env|
      if template = env[:template]?
        rule = Rule::Template.new(template)
      elsif backspec = env[:backspec]?
        rule = Term.case(normp) do
          matchpi %[((%literal %let) (%capture toplevel_) _)] { Rule::BackmapMany.new(toplevel, backspec) }
          otherwise { Rule::BackmapOne.new(backspec) }
        end
      else
        next
      end

      rules << rule

      true
    end

    new(pset, rules.to_readonly_slice.dup)
  end

  def responses(matchee : Term) : Iterator({Pr::Pos, Rule::Any})
    @pset.responses(matchee).map { |response| {response, @rules[response.pattern.index]} }
  end

  def call(matchee : Term) : {Pr::Pos, Rule::Any}?
    case res = @pset.response(matchee)
    in Pr::Pos
      {res, @rules[res.pattern.index]}
    in Pr::Neg
    end
  end
end

def orthor1(parent0, phase, child0, callable)
  while true
    parent1, child1 = callable.call(parent0, phase, child0)
    break if {parent0, child0} == {parent1, child1}
    parent0, child0 = parent1, Term.of(child1)
  end

  {parent0, child0}
end

def orthor(dict0 : Term::Dict, callable)
  dict1 = dict0
  dict0.items.each_with_index do |v0, k|
    dict1, v0 = orthor1(dict1, Term.of(:teach), Term.of(v0), callable)
    if vd = v0.as_d?
      v0 = orthor(vd, callable)
    end
    dict1, v1 = orthor1(dict1, Term.of(:learn), Term.of(v0), callable)
    dict1 = dict1.with(k, v1)
  end
  (0..).each do |i|
    pre = dict1
    dict1.items.each_with_index do |v0, k|
      dict1, v1 = orthor1(dict1, Term.of(:refine, i), Term.of(v0), callable)
      dict1 = dict1.with(k, v1)
    end
    if pre == dict1
      break
    end
  end
  dict1
end
