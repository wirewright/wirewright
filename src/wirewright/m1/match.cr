module Ww::M1
  # Short for *FeedBack*: represents match feedback.
  #
  # This object is meant to be viewed by clients simply as `Slice(Context)`,
  # with maybe some domain-specific methods.
  #
  # Internally, however, we differentiate between the three main variants:
  # *empty*, *one*, and *many*. This way, we don't have to heap-allocate in
  # the predominant case of *one*.
  struct Fb
    include Indexable(Context)

    # :nodoc:
    alias Any = Empty | One | Many

    # :nodoc:
    defrecord Empty
    # :nodoc:
    defrecord One, response : Context
    # :nodoc:
    defrecord Many, responses : Slice(Context)

    # :nodoc:
    def initialize(@kernel : Any)
    end

    def self.[] : Fb
      Fb.new(Empty.new)
    end

    def self.[](response : Context) : Fb
      Fb.new(One.new(response))
    end

    def self.[](*responses : Context) : Fb
      Fb[responses.to_readonly_slice(&.itself)]
    end

    def self.[](responses : Slice(Context)) : Fb
      Fb.new(Many.new(responses))
    end

    def self.[](ary : Indexable(T)) : Fb forall T
      case ary.size
      when 0 then Fb[]
      when 1 then Fb[ary.first]
      else
        Fb[ary.to_readonly_slice(&.itself)]
      end
    end

    # Concatenates two feedbacks.
    def self.join(a : Any, b : Any)
      case {a, b}
      in {_, Empty}   then a
      in {Empty, _}   then b
      in {One, One}   then Many.new(Slice[a.response, b.response])
      in {Many, One}  then Many.new(a.responses.append(b.response))
      in {One, Many}  then Many.new(b.responses.prepend(a.response))
      in {Many, Many} then Many.new(a.responses + b.responses)
      end
    end

    def size : Int32
      case k = @kernel
      in Empty then 0
      in One   then 1
      in Many  then k.responses.size
      end
    end

    def unsafe_fetch(index : Int) : Context
      case k = @kernel
      in Empty then raise ArgumentError.new
      in One   then k.response
      in Many  then k.responses.unsafe_fetch(index)
      end
    end

    def |(other : Fb)
      Fb.new(Fb.join(@kernel, other.@kernel))
    end

    def select(& : Context -> Bool) : Fb
      sink = Pf::Kit.stack_array(Context, 8)

      each do |context|
        next unless yield context

        sink << context
      end

      Fb[sink]
    end
  end

  # `nil` means end-of-plan (no more actions). Otherwise, a plan is simply
  # a list of actions.
  #
  # Note that even though `List` has a stack-like interface, to reinforce
  # the plan metaphor we reverse the words: we "interject" instead of "pushing"
  # actions, "dequeue" instead of "popping" them, and talk about the "first" action
  # and the "rest" of the plan rather than the "last" action/"prior" plan.
  alias Plan = List(Action::Any)?

  # Most if not all actions are thunks calling one of the overloads of `match`
  # or related.
  module Action
    extend self

    alias Any = Lzip | Rzip | Fzip | Match | MatchSeq | MatchCst | EntryBroadcast | MakeCapture | MatchDistribUnit | MatchSpatialSpine

    # :nodoc:
    struct Lzip
      def initialize(@ops : Slice(Op::Any), @matchees : Tzip::ItemsView | Slice(Tzip))
        assert @ops.size == @matchees.size
      end

      def call(ctx : Context, plan : Plan)
        unless op = @ops.first?
          # => @matchees.empty?
          return M1.fb(ctx, plan)
        end

        ahead = ctx.interject(plan, Action.lzip(@ops + 1, @matchees + 1))
        M1.match(ctx, op, @matchees.first, ahead)
      end
    end

    # Left-to-right zip-match on operators and matchees.
    #
    # *ops*/*matchees* can be empty.
    def lzip(ops, matchees)
      Lzip.new(ops, matchees)
    end

    # :nodoc:
    struct Rzip
      def initialize(@ops : Slice(Op::Any), @matchees : Tzip::ItemsView | Slice(Tzip))
        assert @ops.size == @matchees.size
      end

      def call(ctx : Context, plan : Plan)
        unless op = @ops.last?
          # => @matchees.empty?
          return M1.fb(ctx, plan)
        end

        ahead = ctx.interject(plan, Action.rzip(@ops - 1, @matchees - 1))
        M1.match(ctx, op, @matchees.last, ahead)
      end
    end

    # Right-to-left zip-match on operators and matchees.
    #
    # *ops*/*matchees* can be empty.
    def rzip(ops, matchees)
      Rzip.new(ops, matchees)
    end

    # :nodoc:
    struct Fzip
      def initialize(@ops : Slice(Op::Any), @matchees : Tzip::ItemsView | Slice(Tzip))
      end

      def call(ctx : Context, plan : Plan)
        unless op = @ops.first?
          # Fzip allows any number of items ahead. Proceed with a match.
          return M1.fb(ctx, plan)
        end

        remaining = @matchees

        loop do
          unless matchee = remaining.first?
            # Ran out of items but some operators left, mismatch.
            return Fb[]
          end

          # On success, skip the current op and matchee and proceed further.
          ahead = ctx.interject(plan, Action.fzip(@ops + 1, remaining + 1))
          fb = M1.eval(M1.match(ctx, op, matchee, ahead))
          return fb if fb.present?

          # On failure, skip to the next matchee but keep the operator.
          remaining += 1
        end
      end
    end

    # Find-zip.
    #
    # *ops*/*matchees* can be empty.
    def fzip(ops, matchees)
      Fzip.new(ops, matchees)
    end

    # :nodoc:
    struct Match
      def initialize(@op : Op::Any | Op::Entry::Any, @matchee : Tzip)
      end

      def call(ctx : Context, plan : Plan)
        M1.match(ctx, @op, @matchee, plan)
      end
    end

    # Direct call to match overloads for *op* with *matchee*.
    def match(op : Op::Any | Op::Entry::Any, matchee : Tzip)
      Match.new(op, matchee)
    end

    # :nodoc:
    struct MatchSpatialSpine
      def initialize(@spine : SpatialSpine)
      end

      def call(ctx : Context, plan : Plan)
        M1.match(ctx, @spine, plan)
      end
    end

    # Direct call to match overloads for *spine*.
    def match(spine : SpatialSpine)
      MatchSpatialSpine.new(spine)
    end

    # :nodoc:
    struct MatchSeq
      def initialize(@ops : Feed, @items : Tzip::ItemsView)
      end

      def call(ctx : Context, plan : Plan)
        M1.match(ctx, @ops, @items, plan)
      end
    end

    # Direct call to match sequence overloads for *ops* and *items*.
    def match(ops : Feed, items : Tzip::ItemsView)
      MatchSeq.new(ops, items)
    end

    # :nodoc:
    struct MatchDistribUnit
      def initialize(@unit : Op::Item::Distrib::Unit, @items : Tzip::ItemsView)
      end

      def call(ctx : Context, plan : Plan)
        M1.match(ctx, @unit, @items, plan)
      end
    end

    # Direct call to match distrib unit overloads for *unit* and *items*.
    def match(unit : Op::Item::Distrib::Unit, items : Tzip::ItemsView)
      MatchDistribUnit.new(unit, items)
    end

    # :nodoc:
    struct MatchCst
      def initialize(@capture : Term, @cst : Cst::Any, @matchee : Tzip)
      end

      def call(ctx : Context, plan : Plan)
        M1.match(ctx, @capture, @cst, @matchee, plan)
      end
    end

    # Direct call to match constraint overloads for *capture*, *cst* and *matchee*.
    def match(capture : Term, cst : Cst::Any, matchee : Tzip)
      MatchCst.new(capture, cst, matchee)
    end

    # :nodoc:
    struct EntryBroadcast
      def initialize(@entries : Slice(Op::Entry::Any), @matchee : Tzip)
      end

      def call(ctx : Context, plan : Plan)
        unless entry = @entries.first?
          return M1.fb(ctx, plan)
        end

        ahead = ctx.interject(plan, Action.entrybcast(@entries + 1, @matchee))

        M1.match(ctx, entry, @matchee, ahead)
      end
    end

    # Shows (broadcasts) the same *matchee* to each entry in turn.
    def entrybcast(entries : Slice(Op::Entry::Any), matchee : Tzip)
      EntryBroadcast.new(entries, matchee)
    end

    # :nodoc:
    struct MakeCapture
      def initialize(@capture : Term, @proposal : Tzip)
      end

      def call(ctx : Context, plan : Plan)
        M1.capture(ctx, @capture, @proposal, plan)
      end
    end

    # Direct call to `M1.capture` for *capture* and *proposal*.
    def capture(capture : Term, proposal : Tzip)
      MakeCapture.new(capture, proposal)
    end
  end

  # :nodoc:
  #
  # _
  def match(ctx, op : Op::Pass, matchee : Tzip, plan)
    cons(ctx, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Pass, matchee : Term) : Bool
    true
  end

  # :nodoc:
  #
  # (%never)
  def match(ctx, op : Op::Never, matchee : Tzip, plan)
    Fb[]
  end

  # :nodoc:
  def probably_matches?(op : Op::Never, matchee : Term) : Bool
    false
  end

  # :nodoc:
  #
  # 100  "hello world"  (+ 1 2)  (%literal qux)
  def match(ctx, op : Op::Literal, matchee : Tzip, plan)
    matchee.term == op.term ? cons(ctx, plan) : Fb[]
  end

  # :nodoc:
  def probably_matches?(op : Op::Literal, matchee : Term) : Bool
    matchee == op.term
  end

  # :nodoc:
  #
  # (%any a b c)
  def match(ctx, op : Op::LiteralWhitelist, matchee : Tzip, plan)
    op.whitelist.includes?(matchee.term) ? cons(ctx, plan) : Fb[]
  end

  # :nodoc:
  def probably_matches?(op : Op::LiteralWhitelist, matchee : Term) : Bool
    op.whitelist.includes?(matchee)
  end

  # :nodoc:
  #
  # (%not a b c)
  def match(ctx, op : Op::LiteralBlacklist, matchee : Tzip, plan)
    op.blacklist.includes?(matchee.term) ? Fb[] : cons(ctx, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::LiteralBlacklist, matchee : Term) : Bool
    !op.blacklist.includes?(matchee)
  end

  {% for cls, type in {Op::Str => :string, Op::Sym => :symbol, Op::Boolean => :boolean, Op::Dict => :dict} %}
    # :nodoc:
    #
    # _string  _symbol
    def match(ctx, op : {{cls}}, matchee : Tzip, plan)
      matchee.{{type.id}}? ? cons(ctx, plan) : Fb[]
    end

    # :nodoc:
    def probably_matches?(op : {{cls}}, matchee : Term) : Bool
      matchee.type.{{type.id}}?
    end
  {% end %}

  private def compare?(a : Term::Num, op, b : Term::Num)
    case op
    when :lt  then a < b
    when :lte then a <= b
    when :gt  then a > b
    when :gte then a >= b
    else
      raise ArgumentError.new
    end
  end

  # :nodoc:
  #
  # (%number _ < 100)  (%number (whole _))  (%number 0 < (whole _) < 100_000)
  # (%number _ < (var hi))  (%number (var lo) < (whole _) <= (var hi))
  def match(ctx, op : Op::Num, matchee : Tzip, plan)
    return Fb[] unless n = matchee.term.as_n?
    return Fb[] if op.spec.whole? && !n.integer?

    minop = op.spec.min_excluded? ? :lt : :lte
    maxop = op.spec.max_excluded? ? :gt : :gte

    if (min = op.min.as?(Term::Num)) && !compare?(min, minop, n)
      return Fb[]
    end

    if (max = op.max.as?(Term::Num)) && !compare?(max, maxop, n)
      return Fb[]
    end

    waiting = false

    # (%number ⏏(var lo)⏏ < _ < 100)
    if lvar = op.min.as?(Op::Num::Var)
      if value = ctx.capture?(lvar.name)
        # (lo_ (%number (var lo) < _ < 100))
        return Fb[] unless min = value.term.as_n?
        return Fb[] unless compare?(min, minop, n)
      else
        # ((%number (var lo) < _ < 100) lo_)
        ctx = ctx.join(lvar.name, Cst::Compares.new(minop, rhs: n))
        waiting = true
      end
    end

    # (%number 0 < _ < ⏏(var hi)⏏)
    if rvar = op.max.as?(Op::Num::Var)
      if value = ctx.capture?(rvar.name)
        # (hi_ (%number 0 < _ < (var hi)))
        return Fb[] unless max = value.term.as_n?
        return Fb[] unless compare?(max, maxop, n)
      else
        # ((%number 0 < _ < (var hi)) hi_)
        ctx = ctx.join(rvar.name, Cst::Compares.new(maxop, rhs: n))
        waiting = true
      end
    end

    # Do not increase recursion depth in the vast majority of cases, where
    # both bounds are known: (%number 0 < _ < 100)  (lo_ (%number (var lo) <= _))
    unless waiting
      return cons(ctx, plan)
    end

    # If the bounds are unknown, we have to wait for the rest of the pattern
    # to match, and then make sure to only pass those ctxs that defined our var(s)
    # and thus match our constraints. If var(s) are undefined, that's a mismatch,
    # because the result of comparison is indet.
    #
    # ((%number 0 < _ < (var hi)) ⏏hi_)
    # ((%number (var lo) < _ < 100) ⏏lo_)
    # ((%number (var lo) < _ < (var hi)) ⏏lo_ hi_)
    # etc...
    fb = eval(fb(ctx, plan))
    fb.select do |ctx|
      next false unless lvar.nil? || ctx.has_capture?(lvar.name)
      next false unless rvar.nil? || ctx.has_capture?(rvar.name)

      true
    end
  end

  # :nodoc:
  def match(ctx, capture : Term, cst : Cst::Compares, matchee : Tzip, plan)
    return Fb[] unless lhs = matchee.term.as_n?
    return Fb[] unless compare?(lhs, cst.op, cst.rhs)

    cons(ctx, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Num, matchee : Term) : Bool
    matchee.type.number?
  end

  # :nodoc:
  #
  # (%atom)
  def match(ctx, op : Op::Atom, matchee : Tzip, plan)
    matchee.type.atom? ? cons(ctx, plan) : Fb[]
  end

  # :nodoc:
  def probably_matches?(op : Op::Atom, matchee : Term) : Bool
    matchee.type.atom?
  end

  # :nodoc:
  #
  # (%symbol blank name_ type_)
  def match(ctx, op : Op::SymBlank, matchee : Tzip, plan)
    return Fb[] unless row = matchee.blank?

    zname, ztype = row

    ahead = ctx.interject(plan,
      Action.match(op.name, zname),
      Action.match(op.type, ztype),
    )

    cons(ctx, ahead)
  end

  # :nodoc:
  def probably_matches?(op : Op::SymBlank, matchee : Term) : Bool
    return false unless sym = matchee.as_sym?

    !!sym.blank?
  end

  # :nodoc:
  #
  # (%symbol nonblank)
  def match(ctx, op : Op::SymNonblank, matchee : Tzip, plan)
    return Fb[] unless symbol = matchee.term.as_sym?
    return Fb[] if symbol.blank?

    cons(ctx, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::SymNonblank, matchee : Term) : Bool
    return false unless sym = matchee.as_sym?

    !sym.blank?
  end

  # :nodoc:
  #
  # @x_  @x_number  (edge qux_dict)
  def match(ctx, op : Op::Edge, matchee : Tzip, plan)
    Term.edge?(matchee.term, type: op.type) ? cons(ctx, plan) : Fb[]
  end

  # :nodoc:
  def probably_matches?(op : Op::Edge, matchee : Term) : Bool
    Term.edge?(matchee)
  end

  # :nodoc:
  #
  # (_*)
  def match(ctx, op : Op::Itemsonly, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless dict.itemsonly?

    cons(ctx, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Itemsonly, matchee : Term) : Bool
    return false unless dict = matchee.as_d?

    dict.itemsonly?
  end

  # :nodoc:
  #
  # (¦ _)
  def match(ctx, op : Op::Pairsonly, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless dict.pairsonly?

    cons(ctx, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Pairsonly, matchee : Term) : Bool
    return false unless dict = matchee.as_d?

    dict.pairsonly?
  end

  # :nodoc:
  #
  # (%partition items_ pairs_)
  def match(ctx, op : Op::Partition, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?

    if op.pairspart.is_a?(Op::Pass)
      if op.seq && dict.itemsonly?
        # E.g. [/ x_ y_] on (/ 1 2)
        return match(ctx, op.itemspart, matchee, plan)
      end

      # E.g. [x_ y_] on (/ 1 2 precision: 3)
      return match(ctx, op.itemspart, matchee.itemspart, plan)
    end

    ahead = ctx.interject(plan, Action.match(op.pairspart, matchee.pairspart))
    match(ctx, op.itemspart, matchee.itemspart, ahead)
  end

  # :nodoc:
  def probably_matches?(op : Op::Partition, matchee : Term) : Bool
    return false unless dict = matchee.as_d?

    # Avoid allocation in the fast path. For editR, this branch is taken
    # about ~60% of the time. Term::Dict#itemspart and Term::Dict#pairspart
    # currently allocate, just as Tzip#itemspart and Tzip#pairspart, so we
    # consider them expensive.
    if op.pairspart.is_a?(Op::Pass) && op.seq && dict.itemsonly?
      return probably_matches?(op.itemspart, matchee)
    end

    probably_matches?(op.itemspart, Term.of(matchee.itemspart)) &&
      probably_matches?(op.pairspart, Term.of(matchee.pairspart))
  end

  # :nodoc:
  #
  # Auxiliary operator emitted with optimization level O1 to check the bounds
  # of a dict without any further matching (e.g. `(_ _ _)` is simply size=?3).
  def match(ctx, op : Op::Bounds, matchee : Tzip, plan)
    probably_matches?(op, matchee.term) ? fb(ctx, plan) : Fb[]
  end

  # :nodoc:
  def probably_matches?(op : Op::Bounds, matchee : Term) : Bool
    return false unless dict = matchee.as_d?

    op.min <= dict.size <= op.max
  end

  # :nodoc:
  #
  # Same as Bounds but has a successor.
  def match(ctx, op : Op::BoundsGuard, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless op.min <= dict.size <= op.max

    match(ctx, op.successor, matchee, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::BoundsGuard, matchee : Term) : Bool
    return false unless dict = matchee.as_d?
    return false unless op.min <= dict.size <= op.max

    probably_matches?(op.successor, matchee)
  end

  # :nodoc:
  #
  # Auxiliary operator emitted with optimization level O1 to check whether the dict's
  # depth is in some expected depth range (the expected depth range is computed from
  # the pattern).
  def match(ctx, op : Op::MaxDepth, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    # FIXME: currently we're unable to use #max of MaxDepth, since Dict#maxdepth
    # is maximum-ever depth rather than current maximum depth, so #max is
    # too strict.
    return Fb[] unless op.min <= dict.maxdepth

    match(ctx, op.successor, matchee, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::MaxDepth, matchee : Term) : Bool
    return false unless dict = matchee.as_d?
    # FIXME: Ditto
    return false unless op.min <= dict.maxdepth

    probably_matches?(op.successor, matchee)
  end

  # :nodoc:
  #
  # Auxiliary operator emitted with optimization level O1 to check whether
  # the dict, metaphorically speaking, "smells like" one that could match.
  #
  # The sketch is an impression of the content of the dict, its children, and
  # so on down to leaves (currently only symbols in entry values are accounted).
  #
  # Sketches are one of the strongest optimizations in M1, which makes the majority
  # of rejections extraordinarily cheap -- and from a pattern matching engine's
  # point of view, rejections are by far the most common occurence; matches are
  # so rare in comparison that they can sometimes even be pessimized, especially
  # for heavier operators (however, we do not do this here, in M1, at least
  # not intentionally). Think backtracking, as implemented here in M1: a stupendously
  # stupid search which is wrong almost all the time -- a dense forest of rejections.
  #
  # Sketches are basically Bloom filters in disguise. Just like they help one
  # search in absolutely terrific amounts of stored data, they can also be used
  # to help an absolutely terrible search algorithm avoid much of the repercussions
  # from its incorrect choices downstream.
  def match(ctx, op : Op::SketchSubset, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless dict.sketch_superset_of?(op.sketch)

    match(ctx, op.successor, matchee, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::SketchSubset, matchee : Term) : Bool
    return false unless dict = matchee.as_d?
    return false unless dict.sketch_superset_of?(op.sketch)

    probably_matches?(op.successor, matchee)
  end

  # :nodoc:
  #
  # Auxiliary operator emitted with optimization level O1. It is used, wherever
  # possible, to fuse the checks defined separately above to ensure they're local,
  # lack allocation overhead, and don't jump all over the place & call stuff
  # recursively -- confusing the CPU very much. This is M1's "rejection highway".
  def match(ctx, op : Op::DictGuard, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless dict.sketch_superset_of?(op.sketch)
    return Fb[] unless op.bounds[0] <= dict.size <= op.bounds[1]
    # FIXME: Ditto the MaxDepth FIXME above, here it's op.depth[1].
    return Fb[] unless op.depth[0] <= dict.maxdepth

    match(ctx, op.successor, matchee, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::DictGuard, matchee : Term) : Bool
    return false unless dict = matchee.as_d?
    return false unless dict.sketch_superset_of?(op.sketch)
    return false unless op.bounds[0] <= dict.size <= op.bounds[1]
    # FIXME: Ditto the MaxDepth FIXME above, here it's op.depth[1].
    return false unless op.depth[0] <= dict.maxdepth

    probably_matches?(op.successor, matchee)
  end

  # :nodoc:
  #
  # (`front _*)  (`front x_ y_ z_ _*)
  def match(ctx, op : Op::FrontRef, matchee : Tzip, plan)
    return Fb[] unless matchee.type.dict?

    front = matchee.items.before_begin
    ctx = ctx.join(op.name, front.ref(ord: Op::Item::ORD_FRONT))
    cons(ctx, plan)
  end

  # :nodoc:
  #
  # (_* `back)  (x_ y_ z_ _* `back)
  def match(ctx, op : Op::BackRef, matchee : Tzip, plan)
    return Fb[] unless matchee.type.dict?

    back = matchee.items.after_end
    ctx = ctx.join(op.name, back.ref(ord: Op::Item::ORD_BACK))
    cons(ctx, plan)
  end

  # :nodoc:
  #
  # (%let x _)
  def match(ctx, op : Op::Capture, matchee : Tzip, plan)
    capture(ctx, op.capture, matchee, op.successor, matchee, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Capture, matchee : Term)
    probably_matches?(op.successor, matchee)
  end

  # :nodoc:
  def capture(ctx, capture : Term, proposal : Tzip, successor, matchee : Tzip, plan)
    ahead = ctx.interject(plan, Action.match(successor, matchee))
    capture(ctx, capture, proposal, ahead)
  end

  # :nodoc:
  def capture(ctx, capture, proposal, plan)
    ctx, updated = ctx.update?(capture) do |capture|
      unless sum = Tzip.join?(capture, proposal)
        return Fb[] # Constraint not satisfied.
      end

      sum # Constraint satisfied.
    end

    if updated
      return cons(ctx, plan)
    end

    propose(ctx, capture, proposal, plan)
  end

  # :nodoc:
  def propose(ctx, capture : Term, proposal : Tzip, successor, matchee : Tzip, plan)
    ahead = ctx.interject(plan, Action.match(successor, matchee))
    propose(ctx, capture, proposal, ahead)
  end

  # :nodoc:
  #
  # NOTE: *capture* **must not** exist in the environment at the time of
  # calling this method.
  def propose(ctx, capture : Term, proposal : Tzip, plan)
    {% unless flag?(:release) %}
      assert !ctx.has_capture?(capture)
    {% end %}

    if choices = ctx.choices?(capture)
      if ctx.selector
        # If we are in selector mode, this means proposals must conform to their choice
        # set; otherwise it is a mismatch.
        return Fb[] unless choices.includes?(proposal.term)
      else
        # If there is a choice set, make sure to restrict it by the proposal.
        ctx = ctx.intersect(capture, choices & Pf::Set[proposal.term])
      end
    end

    ctx = ctx.assoc(capture, proposal)

    unless cst = ctx.cst?(capture)
      # No constraint. We can assign and proceed to the successor.
      return cons(ctx, plan)
    end

    # There is a constraint. We must make sure it is satisfied by the capture
    # before going to the successor with matchee.
    match(ctx, capture, cst, proposal, plan)
  end

  # :nodoc:
  #
  # Chain Many constraints to get an AND on them.
  def match(ctx, capture : Term, cst : Cst::Many, matchee : Tzip, plan)
    ahead = plan

    cst.children.each do |child|
      ahead = ctx.interject(ahead, Action.match(capture, child, matchee))
    end

    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # (x_ _*)
  def match(ctx, op : Op::ItemFirst, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_itemsonly_d?
    return Fb[] unless dict.itemsize > 0

    cons(ctx, op.successor, matchee[0], plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::ItemFirst, matchee : Term) : Bool
    return false unless dict = matchee.as_itemsonly_d?
    return false unless dict.itemsize > 0

    probably_matches?(op.successor, dict[0])
  end

  # :nodoc:
  #
  # (_* x_)
  def match(ctx, op : Op::ItemLast, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_itemsonly_d?
    return Fb[] unless dict.itemsize > 0

    cons(ctx, op.successor, matchee[dict.hi], plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::ItemLast, matchee : Term) : Bool
    return false unless dict = matchee.as_itemsonly_d?
    return false unless dict.itemsize > 0

    probably_matches?(op.successor, dict[dict.hi])
  end

  # :nodoc:
  #
  # (xs_*)
  def match(ctx, op : Op::CaptureItemsonly, matchee : Tzip, plan)
    return Fb[] unless matchee.term.as_itemsonly_d?

    capture(ctx, op.capture, Tzip.new(matchee.term, matchee.items.log), Op::INSTANCE_PASS, matchee, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::CaptureItemsonly, matchee : Term) : Bool
    !!matchee.as_itemsonly_d?
  end

  # :nodoc:
  #
  # (x_ y_ z_)  (x_ y_ z_ _*)  (_* x_ y_ z_)
  def match(ctx, op : Op::SingularSeq, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_itemsonly_d?

    # We expect more items than the dict can provide. No way the match
    # can succeed.
    return Fb[] if op.items.size > dict.itemsize

    # exhaustive=true: (x_ y_ z_)
    # exhaustive=false: (x_ y_ z_ _*) (_* x_ y_ z_)
    return Fb[] if op.exhaustive && op.items.size != dict.itemsize

    if op.reverse
      # (_* x_ y_)  (_* x_ y_ ⏏) (_* x_ ⏏ y_) (_* ⏏ x_ y_)
      items = matchee.items.last(op.items.size)
      action = Action.rzip(op.items, items)
    else
      # (x_ y_ _*)  (⏏ x_ y_ _*) (x_ ⏏ y_ _*) (x_ y_ ⏏ _*)
      items = matchee.items.first(op.items.size)
      action = Action.lzip(op.items, items)
    end

    ahead = ctx.interject(plan, action)
    cons(ctx, ahead)
  end

  # :nodoc:
  def probably_matches?(op : Op::SingularSeq, matchee : Term) : Bool
    return false unless dict = matchee.as_itemsonly_d?
    return false if op.items.size > dict.itemsize
    return false if op.exhaustive && op.items.size != dict.itemsize

    if op.reverse
      items = matchee.items.last(op.items.size)
    else
      items = matchee.items.first(op.items.size)
    end

    op.items.zip(items) do |item_op, item|
      return false unless probably_matches?(item_op, item)
    end

    true
  end

  # :nodoc:
  #
  # (%any° a_ b_ c_)
  def match(ctx, op : Op::ChoiceSource, matchee : Tzip, plan)
    eval(match(ctx, op.a, matchee, plan)) | eval(match(ctx, op.b, matchee, plan))
  end

  # :nodoc:
  def probably_matches?(op : Op::ChoiceSource, matchee : Term) : Bool
    probably_matches?(op.a, matchee) || probably_matches?(op.b, matchee)
  end

  # :nodoc:
  #
  # (%all a_ b_)
  def match(ctx, op : Op::Both, matchee : Tzip, plan)
    ahead = ctx.interject(plan, Action.match(op.b, matchee))
    match(ctx, op.a, matchee, ahead)
  end

  # :nodoc:
  def probably_matches?(op : Op::Both, matchee : Term) : Bool
    probably_matches?(op.a, matchee) && probably_matches?(op.b, matchee)
  end

  # :nodoc:
  #
  # (%keytest a b c) -- has any key of those listed
  def match(ctx, op : Op::Keytest, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?

    if dict.size < op.keys.size
      dict.each_entry do |key, _|
        next unless op.keys.includes?(key)
        return cons(ctx, plan)
      end
    else
      op.keys.each do |key|
        next unless dict.includes?(key)
        return cons(ctx, plan)
      end
    end

    Fb[]
  end

  # :nodoc:
  #
  # (%keypool a b c) -- dict has no keys other than those listed (i.e., no extra keys)
  def match(ctx, op : Op::Keypool, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] if dict.size > op.keys.size # Remember that all keys are unique!

    n = op.keys.count(&.in?(dict))
    if dict.size > n
      return Fb[] # Dict has extra keys.
    end

    cons(ctx, plan)
  end

  private def eligible?(op : Op::Scan, matchee : Term) : Bool
    return false unless dict = matchee.as_d?

    dict.itemsize >= op.seq.size
  end

  private def eligible?(op : Op::ScanAll, matchee : Term) : Bool
    return false unless dict = matchee.as_d?

    op.min.zero? || matchee.itemsize >= op.seq.size
  end

  private def eligible?(op : Op::Dfs | Op::Bfs, matchee : Term) : Bool
    return true if op.alg.mindepth.zero?
    return false unless dict = matchee.as_d?

    dict.size > 0
  end

  private def eligible?(op : Op::Entries, matchee : Term) : Bool
    matchee.type.dict?
  end

  private def eligible?(op : Op::Split, matchee : Term) : Bool
    return false unless dict = matchee.as_d?

    dict.itemsize >= op.focus.size
  end

  private def eligible?(op : Op::SplitAll, matchee : Term) : Bool
    return false unless dict = matchee.as_d?

    op.min.zero? || matchee.itemsize >= op.focus.size
  end

  private def search(op : Op::Scan, matchee : Tzip, & : Tzip::ItemsView -> Bool) : Nil
    current = matchee.items

    while current.size >= op.seq.size
      accepted = yield current.first(op.seq.size)
      current += accepted ? op.seq.size : 1
    end
  end

  # WARNING: The yielded slices are valid for the lifetime of that particular
  # (per-iteration) block yield!
  private def search(op : Op::Dfs | Op::Bfs, matchee : Tzip, & : Slice(Tzip) -> Bool) : Nil
    assert op.seq.size > 0

    # NOTE: HybridArray can't be used here because we need contiguous storage
    # to point to in our yields.
    if op.seq.size > 4
      bufferptr = Pointer(Tzip).malloc(op.seq.size)
    else
      buffer = uninitialized Tzip[4]
      bufferptr = buffer.to_unsafe
    end

    size = 0

    matchee.walk(op.alg) do |leaf|
      bufferptr[size] = leaf
      size += 1
      next unless size == op.seq.size

      accepted = yield Slice.new(bufferptr, size, read_only: true)
      if accepted
        bufferptr.clear(size)
        size = 0
        next
      end

      # bufferptr  A B C D
      # size       4
      (bufferptr + 1).move_to(bufferptr, size - 1)
      # bufferptr B C D D
      # size      4
      (bufferptr + size).clear(1)
      size -= 1
      # bufferptr B C D
      # size      3
    end

    # Whatever is at `bufferptr` does not matter because it's a guaranteed
    # mismatch: it's less than the needle's size.
    assert size < op.seq.size
  end

  private def search(ctx, op : Op::Scan | Op::Dfs | Op::Bfs, matchee : Tzip, plan, & : Fb -> Bool) : Nil
    search(op, matchee) do |chunk|
      action = Action.lzip(op.seq, chunk)
      ahead = ctx.interject(plan, action)
      yield eval(fb(ctx, ahead))
    end
  end

  private def search_with_sealed_log!(ctx, op : Op::Scan | Op::Dfs | Op::Bfs, matchee : Tzip, plan, & : Fb, Log::Sealed | Log::None -> Bool) : Nil
    search(op, matchee) do |chunk|
      action = Action.lzip(op.seq, chunk)
      ahead = ctx.interject(plan, action)
      yield eval(fb(ctx, ahead)), Log.seal(chunk, &.log)
    end
  end

  private def search(ctx, op : Op::Entries, matchee : Tzip, plan, & : Fb -> Bool) : Nil
    matchee.each_entry_ord do |key, value|
      ahead = ctx.interject(plan, Action.match(op.vop, value))
      _ = yield eval(match(ctx, op.kop, key, ahead))
    end
  end

  private def search_with_sealed_log!(ctx, op : Op::Entries, matchee : Tzip, plan, & : Fb, Log::Sealed | Log::None -> Bool) : Nil
    matchee.each_entry_ord do |key, value|
      ahead = ctx.interject(plan, Action.match(op.vop, value))
      _ = yield eval(match(ctx, op.kop, key, ahead)), Log.seal({key, value}, &.log)
    end
  end

  # NOTE: "Cutting" on match (so that lhs is erased) seems to provide better semantics
  # than simply iterating over splits left-to-right. This is called "narrow" split and
  # it's the default.
  private def each_split_narrow(n : Int32, matchee : Tzip, & : Tzip::ItemsView, Tzip::ItemsView, Tzip::ItemsView -> Bool) : Nil
    assert n >= 0

    feed = matchee.items

    loop do
      running = false

      feed.each_split(n) do |l, focus, r|
        accepted = yield l, focus, r
        next false unless accepted

        # We can't determine what's "before" and what's "after" focus when we have
        # no focus. Leaving this out means the very first case degenerates to an infinite
        # loop, with l=<empty view>, focus=<absent>, and r=<full view>. Hard-coding r + 1 feels
        # hacky and I feel it violates the semantics of %split°, although I can't
        # articulate why.
        #
        # Therefore, we don't drop "before" on successful match, as with nonempty
        # focus -- because, as I said above, we don't have a before!
        next true if n.zero?

        feed = r
        running = true
        break
      end

      break unless running
    end
  end

  private def each_split_wide(n : Int32, matchee : Tzip, & : Tzip::ItemsView, Tzip::ItemsView, Tzip::ItemsView -> Bool) : Nil
    assert n >= 0

    feed = matchee.items
    feed.each_split(n) do |l, focus, r|
      yield l, focus, r
    end
  end

  private def each_split(n, matchee, *, wide : Bool, &) : Nil
    if wide
      each_split_wide(n, matchee) { |l, focus, r| yield l, focus, r }
    else
      each_split_narrow(n, matchee) { |l, focus, r| yield l, focus, r }
    end
  end

  private def search(ctx, op : Op::Split, matchee : Tzip, plan, & : Fb -> Bool) : Nil
    each_split(op.focus.size, matchee, wide: op.wide) do |l, focus, r|
      yield eval(match(ctx, op, l, focus, r, plan))
    end
  end

  private def search_with_sealed_log!(ctx, op : Op::Split, matchee : Tzip, plan, & : Fb, Log::Sealed | Log::None -> Bool) : Nil
    each_split(op.focus.size, matchee, wide: op.wide) do |l, focus, r|
      fb = eval(match(ctx, op, l, focus, r, plan))

      # When you refer to an env in %splits, like here:
      #
      #   (%splits (⏏env0_⏏ _*) l_ qux r_)
      #
      # ... I think the only thing that makes sense for us to do is to use that to
      # refer to the range of the focus (here it is just `qux`). Therefore referring
      # to the envlist:
      #
      #   (%splits ⏏envs_⏏ l_ qux r_)
      #
      # ... means referring to all foci we've matched. Foci have the nice property
      # that they cannot overlap (left/right halves may include the next/previous
      # foci, but we  don't really care about that).
      _ = yield fb, Log.seal(Log.simplify(focus.log))
    end
  end

  private def search_with_sealed_log(ctx, op, matchee, plan, &) : Nil
    # Logging is disabled. Do not waste time managing the log.
    if matchee.log.is_a?(Log::None)
      search(ctx, op, matchee, plan) { |fb| yield fb, Log.none }
      return
    end

    search_with_sealed_log!(ctx, op, matchee, plan) do |fb, fblog|
      yield fb, fblog
    end
  end

  # :nodoc:
  def match(ctx, op : Op::Split, l : Tzip::ItemsView, focus : Tzip::ItemsView, r : Tzip::ItemsView, plan)
    ahead = plan

    # NOTE: order for sides doesn't matter; but maybe it should based on
    # some sort of cost?..

    case op.lhs
    when Op::Pass, Op::Dict, Op::Itemsonly
    else
      ahead = ctx.interject(ahead, Action.match(op.lhs, l.collect))
    end

    case op.rhs
    when Op::Pass, Op::Dict, Op::Itemsonly
    else
      ahead = ctx.interject(ahead, Action.match(op.rhs, r.collect))
    end

    ahead = ctx.interject(ahead, Action.lzip(op.focus, focus))
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # ⟨x_⟩  ⟨x_ y_ z_⟩
  def match(ctx, op : Op::First, matchee : Tzip, plan)
    return Fb[] unless eligible?(op, matchee.term)

    # Fast log with less indirection specifically for ⟨_⟩, because it
    # is used quite a lot in very hot places.
    if op.is_a?(Op::ScanFirst) && op.seq.size == 1
      needle = op.seq.unsafe_fetch(0)

      matchee.items.each do |item|
        fb = eval(match(ctx, needle, item, plan))
        next if fb.empty?
        return fb
      end

      return Fb[]
    end

    search(ctx, op, matchee, plan) do |fb|
      return fb if fb.present? # First match(es)

      false # reject
    end

    Fb[]
  end

  # :nodoc:
  def probably_matches?(op : Op::ScanFirst, matchee : Term) : Bool
    return false unless eligible?(op, matchee)

    assert dict = matchee.as_d?

    item_ops = op.seq
    dict.items.each do |item|
      next unless probably_matches?(item_ops.first, item)

      item_ops += 1
      if item_ops.empty?
        return true
      end
    end

    false
  end

  # :nodoc:
  def probably_matches?(op : Op::EntriesFirst, matchee : Term) : Bool
    return false unless eligible?(op, matchee)

    assert dict = matchee.as_d?

    dict.each_entry do |key, value|
      if probably_matches?(op.kop, key) && probably_matches?(op.vop, value)
        return true
      end
    end

    false
  end

  # :nodoc:
  #
  # ⟨x_⟩°  ⟨x_ y_ z_⟩°
  def match(ctx, op : Op::Source, matchee : Tzip, plan)
    return Fb[] unless eligible?(op, matchee.term)

    sink = Pf::Kit.stack_array(Context, 8)

    search(ctx, op, matchee, plan) do |fb|
      fb.each { |response| sink << response }
      fb.present? # accept if more than 0 responses
    end

    Fb[sink]
  end

  # :nodoc:
  #
  # (%items xs_ _)  (%items (a_ b_) _)  (%items ms←(a_ _* {¦ x: b_}) x_)
  def match(ctx, op : Op::All, matchee : Tzip, plan)
    return Fb[] unless eligible?(op, matchee.term)

    successor = Envlist.new(op.successor, op.min, op.max)

    match(ctx, successor, plan) do |push|
      # In `(a_ (%items xs_ ⏏a_ b_⏏) b_)`, `a_ b_` matches unconstrained by
      # the outer `a_` and `b_`. Such constraints can be established later,
      # in the successor (here it is `xs_`, which does not impose any constraints).
      search_with_sealed_log(ctx.sibling, op, matchee, plan: nil) do |fb, fblog|
        next false if fb.empty? # reject

        unless push.call?(fb, fblog)
          return Fb[] # does not fit (max exceeded)
        end

        true # accept
      end
    end
  end

  # :nodoc:
  #
  # (%matches () ⟨_number⟩°)  (%matches ({¦ x_} _* lst_) ⟨±x⟩°)
  def match(ctx, op : Op::Matches, matchee : Tzip, plan)
    handle = Log.seal(Log.simplify(matchee.log))
    successor = Envlist.new(op.successor, op.min, op.max, handle)

    match(ctx, successor, plan) do |push|
      # We evaluate the subpattern in an isolated context, with no plan ahead, but
      # notice how *matchee* still carries the provenance.
      fb = eval(match(ctx.sibling, op.subpattern, matchee, plan: nil))
      fb.each do |response|
        unless push.call?(Fb[response], handle)
          return Fb[] # does not fit (max exceeded)
        end
      end
    end
  end

  # :nodoc:
  class EnvlistPush(N)
    def initialize(
      @envs : Pf::Kit::HybridArray(Tzip, N),
      @logs : Pf::Kit::HybridArray(Log::SealedOne, N)?,
      @capacity : Magnitude,
    )
    end

    # Returns `true` if *fb*/*fblog* fits. Returns `false` if they do not fit
    # in the envlist.
    def call?(fb : Fb, fblog : Log::None | Log::Sealed) : Bool
      unless fb.present?
        return true # continue
      end

      fb.each do |response|
        # Halt everything if too many matches.
        if @envs.size + 1 > @capacity
          return false # halt
        end

        # When you match (%items (⏏x_⏏ y_) _ _) and change `x` (the first *env*), it
        # should change the first matched PAIR of items (generally, the first matched
        # sequence of items); which is exactly what *fblog* refers to, being
        # a collection of matched logs.
        #
        # NOTE: We knowingly discard imaginary/virtual responses here (e.g. `kp` in
        # `(%-value k kp)`), because it's hard and unnecessary to match on them
        # in practice.
        @envs << Tzip.mapping(response.envtab, fblog) { |entry, _| entry }
      end

      if logs = @logs
        Log.flatten(fblog) do |one|
          logs << Log.seal(one)
        end
      end

      true # continue
    end
  end

  # :nodoc:
  defrecord Envlist,
    op : Op::Any,
    min : Magnitude,
    max : Magnitude,
    handle : Log::Sealed | Log::None? = nil

  # :nodoc:
  def match(ctx, mod : Envlist, plan, &)
    envs = Pf::Kit.stack_array(Tzip, 8)
    # If it doesn't specify a concrete handle, then its handle is the union of
    # the handles of what it will push.
    if mod.handle.nil?
      logs = Pf::Kit.stack_array(Log::SealedOne, 8)
    end

    push = stack_alloc EnvlistPush(8).new(envs, logs, mod.max)
    yield push

    return Fb[] if envs.size < mod.min

    # Synthesize the environment list, containing environments pushed by
    # the block. Indices are mapped to environments, which are Tzips.
    #
    # *logs* contains flattened handles of environments. Therefore, *envlist*'s
    # handle is the union of those. This means it points to all elements matched by
    # environments in the envlist. So e.g. for %items, referring to `(%items ⏏xs_⏏ _number)`
    # means referring to all items that it matched (in this example, to all numbers
    # in the itemspart of the matchee).
    unless handle = mod.handle
      assert logs
      handle = Log.seal(logs, &.itself)
    end
    envlist = Tzip.mapping(envs, handle) do |env, index|
      {Term.of(index), env}
    end

    cons(ctx, mod.op, envlist, plan)
  end

  # :nodoc:
  #
  # (%value k v_)
  def match(ctx, op : Op::Value, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    table = matchee

    # Fast path on e.g. (k_ (%value k v_))
    if key = ctx.capture?(op.capture)
      unless value = table[key.term]?
        return Fb[] # Key not found
      end

      subctx = ctx.assoc(op.capture, key.also_key_in(table))
      return cons(subctx, op.tail, value, plan)
    end

    # Install a constraint on op.capture. Something (a %let of some sort,
    # perhaps) will have to propose a value for it and at that point, match
    # our op.tail. On our end, make sure all responses we get back know
    # op.capture. If any response does not, go the search route.
    #
    # ((%value k v_) k_)
    subctx = ctx.join(op.capture, Cst::Lookup.new(table, op.tail))
    fb = eval(fb(subctx, plan)).select(&.has_capture?(op.capture))
    if fb.present?
      return fb # Some responses produced a satisfactory key!
    end

    # Search: someone ahead doesn't know how to define op.capture, so we'll have
    # to find it with brute force.
    #
    # (%value k v_)
    matchee.each_entry do |key, value|
      fb = eval(propose(ctx, op.capture, key, op.tail, value, plan))
      next if fb.empty?
      return fb
    end

    Fb[] # E.g. (%value k v_) on empty dict {}.
  end

  # :nodoc:
  def match(ctx, capture : Term, cst : Cst::Lookup, matchee : Tzip, plan)
    unless value = cst.table[matchee.term]?
      return Fb[] # Key not found.
    end

    key = ctx.capture?(capture)
    assert key, "capture was not assigned before matching constraint on it"

    subctx = ctx.assoc(capture, key.also_key_in(cst.table))
    cons(subctx, cst.op, value, plan)
  end

  # :nodoc:
  #
  # (%-value k) -- inverse of %value, i.e., key at capture k must be absent in
  # matchee dict.
  def match(ctx, op : Op::NegativeValue, matchee : Tzip, plan)
    return Fb[] unless table = matchee.term.as_d?

    # Fast path on e.g. (k_ (%-value k)).
    if key = ctx.capture?(op.capture)
      if table.includes?(key.term)
        return Fb[] # Key found, value exists, %-value mismatch!
      end

      # Key not found, value does not exist, %-value match.
      return cons(ctx, plan)
    end

    # Establish a constraint on future values of key for situations like
    # ((%-value k) k_).
    subctx = ctx.join(op.capture, Cst::NegLookup.new(matchee))
    fb = eval(fb(subctx, plan)).select(&.has_capture?(op.capture))
    if fb.present?
      return fb # Some responses proposed a satisfactory key, nice!
    end

    # Someone ahead didn't propose a satisfactory key. Since, by definition,
    # every key *we* can propose from *matchee* exists, there's no way we can
    # propose a non-existent key to satisfy ourselves (%-value). This situation
    # is a nevermatch.
    #
    # (%-value k)
    Fb[]
  end

  # :nodoc:
  def match(ctx, capture : Term, cst : Cst::NegLookup, matchee : Tzip, plan)
    cst.table.term.includes?(matchee.term) ? Fb[] : cons(ctx, plan)
  end

  # :nodoc:
  #
  # (%-value k kp) -- key at capture k must be absent in matchee dict, imaginary
  # keypath stored at *kp*.
  def match(ctx, op : Op::NegativeValueKeypath, matchee : Tzip, plan)
    return Fb[] unless table = matchee.term.as_d?

    # (k_ (%-value k kp))
    if key = ctx.capture?(op.capture)
      ref = matchee.ref(key.term)

      if table.includes?(key.term)
        return Fb[] # Key found, value exists, %-value mismatch!
      end

      # Key not found, value does not exist, %-value match.
      return cons(ctx.join(op.name, ref), plan)
    end

    # ((%-value k kp) k_)
    subctx = ctx.join(op.capture, Cst::NegLookupRef.new(matchee, op.name))

    fb = eval(fb(subctx, plan)).select(&.has_capture?(op.capture))
    if fb.present?
      return fb # Some responses proposed a satisfactory key, nice!
    end

    Fb[]
  end

  # :nodoc:
  def match(ctx, capture : Term, cst : Cst::NegLookupRef, matchee : Tzip, plan)
    return Fb[] if cst.table.term.includes?(matchee.term)

    ref = cst.table.ref(matchee.term)
    cons(ctx.join(cst.name, ref), plan)
  end

  # :nodoc:
  #
  # (x_ _ _) -- when using O2 optimization level
  def match(ctx, op : Op::ValueLiteral, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?
    return Fb[] unless value = matchee[op.key]?

    cons(ctx, op.successor, value, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::ValueLiteral, matchee : Term) : Bool
    return false unless dict = matchee.as_d?
    return false unless value = dict[op.key]?

    probably_matches?(op.successor, value)
  end

  # :nodoc:
  #
  # (%pipe span 3) -- match char count of string
  def match(ctx, op : Op::Span, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_s?

    cons(ctx, op.successor, Tzip.new(Term.of(a.charcount), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe tally 3) -- match entry count of dict
  def match(ctx, op : Op::Tally, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_d?

    cons(ctx, op.successor, Tzip.new(Term.of(a.size), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe type _number)
  def match(ctx, op : Op::Type, matchee : Tzip, plan)
    cons(ctx, op.successor, Tzip.new(Term.of(matchee.term.type.blank), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe ml (+ a_ b_))
  def match(ctx, op : Op::ParseML, matchee : Tzip, plan)
    return Fb[] unless string = matchee.term.as_s?

    # TODO: In theory, we can use srcmaps to provide backmapping for `ml`, e.g.
    # (%pipe ml (+ a_ b_)) <> {a: ^b, b: ^a} on "(+ 1 2)" would give "(+ 2 1)".
    begin
      result = ML.term(string.to(String))
    rescue ML::SyntaxError
      return Fb[]
    end

    cons(ctx, op.successor, Tzip.new(result, Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe untracked x_) -- a utility operator used to remove log tracking. This lets
  # you set up equality constraints while not having backmaps manipulate both parties.
  def match(ctx, op : Op::Untracked, matchee : Tzip, plan)
    cons(ctx, op.successor, Tzip.new(matchee.term, Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (prepend a b c) x_)
  def match(ctx, op : Op::Prepend, matchee : Tzip, plan)
    return Fb[] unless dict0 = matchee.term.as_d?

    dict1 = dict0.replace(Term[0]...Term[0], &.concat(op.terms))

    cons(ctx, op.successor, Tzip.new(Term.of(dict1), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (+ 100) x_)
  def match(ctx, op : Op::Add, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    cons(ctx, op.successor, Tzip.new(Term.of(a + op.arg), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (- 100) x_)
  def match(ctx, op : Op::Sub, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    cons(ctx, op.successor, Tzip.new(Term.of(a - op.arg), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (* 2) dbl_)
  def match(ctx, op : Op::Mul, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    cons(ctx, op.successor, Tzip.new(Term.of(a * op.arg), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (/ 2) x_)
  def match(ctx, op : Op::Div, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    begin
      q = Term.of(a / op.arg)
    rescue DivisionByZeroError
      return Fb[] # (%pipe (/ 0) _) is a nevermatch.
    end

    cons(ctx, op.successor, Tzip.new(q, Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (// 2) x_)
  def match(ctx, op : Op::Idiv, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    begin
      q = Term.of(a // op.arg)
    rescue DivisionByZeroError
      return Fb[] # (%pipe (// 0) _) is a nevermatch
    end

    cons(ctx, op.successor, Tzip.new(q, Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (mod 2) x_)
  def match(ctx, op : Op::Mod, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    begin
      r = Term.of(a % op.arg)
    rescue DivisionByZeroError
      return Fb[] # (%pipe (% 0) _) is a nevermatch
    end

    cons(ctx, op.successor, Tzip.new(r, Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (** 2) x_)
  def match(ctx, op : Op::Pow, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    begin
      c = Term.of(a ** op.arg)
    rescue DivisionByZeroError # e.g. 0^-2
      return Fb[]
    end

    cons(ctx, op.successor, Tzip.new(c, Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (clamp 0 ..< 10) x_)
  def match(ctx, op : Op::Clamp, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    a = Math.min(Math.max(a, op.min), op.max)

    cons(ctx, op.successor, Tzip.new(Term.of(a), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (map {a: 100, b: 200}) x_)
  def match(ctx, op : Op::Map, matchee : Tzip, plan)
    return Fb[] unless v = op.arg[matchee.term]?

    cons(ctx, op.successor, Tzip.new(v, Log.none), plan)
  end

  # :nodoc:
  #
  # (%keypath kp)
  def match(ctx, op : Op::KeypathCapture, matchee : Tzip, plan)
    log = matchee.log
    if log.is_a?(Log::None)
      # Either logging is disabled, or there is no path to matchee. Raise
      # RequestLog. The caller will either enable logging for us, or trigger
      # mismatch on our behalf -- if there's no path to matchee KeypathCapture
      # is a nevermach.
      raise RequestLog.new
    end

    unless keypath = Log.keypath?(Log.simplify(log))
      return Fb[]
    end

    proposal = Tzip.new(Term.of(keypath), Log.none)

    capture(ctx, op.capture, proposal, Op::INSTANCE_PASS, matchee, plan)
  end

  # :nodoc:
  #
  # (+ a_ b_)  (_* x_ _* y_ _*)  (xs_* ys_*)  (+ (%group (a_ _* b_) _*) _)
  def match(ctx, op : Op::Seq, matchee : Tzip, plan)
    return Fb[] unless _ = matchee.term.as_itemsonly_d?

    match(ctx, op.items, matchee.items, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Seq, matchee : Term) : Bool
    return false unless dict = matchee.as_d?
    return false unless dict.itemsonly?

    # Size is probably checked by Bounds or BoundsGuard or similar, one of our
    # predecessors [in control flow]; don't bother checking size here.

    # We won't do anything smart here. We just want to make sure that all
    # Singulars can be found in the matchee, somewhere. Heavy work is left to
    # the main algorithm.
    op.singulars.each do |needle|
      found = false

      dict.each_item_unordered do |item|
        next unless probably_matches?(needle, item)
        found = true
        break
      end

      return false unless found
    end

    true
  end

  # :nodoc:
  #
  # Represent an *assignment* -- the result of *distribution*.
  alias SpatialEq = SingularEq | FlexRegionEq

  # :nodoc:
  #
  # An assignment of one operator (a Singular) to one item, *item*. We store
  # a view instead of the item itself (i.e., Tzip) to retain info about
  # the beginning and end within the enclosing dict.
  defrecord SingularEq, op : Op::Any, item : Tzip::ItemsView do
    assert item.size == 1
  end

  # :nodoc:
  #
  # An assignment of a flexible region to zero or more *items*.
  defrecord FlexRegionEq,
    ops : Array(Op::Item::Distrib | Op::Item::NonDistrib),
    items : Tzip::ItemsView

  # :nodoc:
  #
  # Part iterator over assignments generated by `distribute?`, part a closure over
  # *special* and *items*, part a record of claims over *items*.
  defcase SpatialSpine,
    spatial : Op::Item::Spatial,
    items : Tzip::ItemsView,
    eqs : Slice(SpatialEq),
    claims = Slice({UInt32, UInt32}).empty

  class SpatialSpine
    def claim(begin begin_ : UInt32, end end_ : UInt32)
      copy_with(claims: @claims.append({begin_, end_}))
    end

    def claim(view : Tzip::ItemsView) : SpatialSpine
      claim(view.begin, view.end)
    end
  end

  # :nodoc:
  #
  # This function is the entry point to Spatial matching. Its main goal is to
  # mechanically distribute *items* according to the shape in *spatial*; then,
  # construct a SpatialSpine, containing assigments resulting from distribution.
  # Finally, this function transfers control to SpatialSpine and lets assignments
  # in it continue matching.
  def match(ctx, spatial : Op::Item::Spatial, items : Tzip::ItemsView, plan)
    eqs = Pf::Kit.stack_array(SpatialEq, 8)
    unless distribute?(spatial.shape, items, sink: eqs)
      return Fb[]
    end

    spine = SpatialSpine.new(spatial, items, eqs: eqs.to_readonly_slice(&.itself))
    match(ctx, spine, plan)
  end

  # :nodoc:
  #
  # Drives the iterator of assignments in *spine* until exhausted. Matches each
  # assignment. When the iterator is exhausted, `commit`s claims.
  def match(ctx, spine : SpatialSpine, plan)
    unless eq = spine.eqs.first?
      return commit(ctx, spine, plan)
    end

    match(ctx, eq, spine.copy_with(eqs: spine.eqs + 1), plan)
  end

  # :nodoc:
  #
  # Processes a singular assignment -- one operator to one item.
  def match(ctx, eq : SingularEq, rest : SpatialSpine, plan)
    item = eq.item.first

    # Make a record about the range of the Singular. We'll need it later
    # to resolve Slots and Groups.
    rest = rest.claim(eq.item)

    ahead = ctx.interject(plan,
      Action.match(eq.op, item),
      Action.match(rest),
    )
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # This function acts as an "adapter" between Spatial matching and flex region
  # matching. Flex regions contain and handle things like %optional, %past/max,
  # %many, and so on. Whereas Spatial matches have in a certain sense "hard-coded"
  # meaning, and items are distributed mechanically (see `distribute?`), the distribution
  # of items within FlexRegions is, wait for it... flexible. It is found using
  # backtracking search.
  def match(ctx, eq : FlexRegionEq, rest : SpatialSpine, plan)
    match(ctx, Feed.new(eq.ops.to_readonly_slice, rest), eq.items, plan)
  end

  # + a_ b_  a_ b_ c_
  private def distribute?(op : Op::Item::Rigid, items : Tzip::ItemsView, sink) : Bool
    unless op.items.size == items.size
      return false # mismatch
    end

    op.items.each_with_index do |item_op, index|
      sink << SingularEq.new(item_op, (items + index).first(1))
    end

    true # proceed
  end

  # This is a special node emitted on e.g. (%past `x), (%many xs_ `x) etc., for which
  # the only valid match is the empty match.
  private def distribute?(op : Op::Item::Empty, items : Tzip::ItemsView, sink) : Bool
    unless items.empty?
      return false # mismatch
    end

    true # proceed
  end

  # l_* m_* r_*  l_* (%optional 0 m_) r_*  . . .
  private def distribute?(op : Op::Item::FlexRegion, items : Tzip::ItemsView, sink) : Bool
    sink << FlexRegionEq.new(op.items, items)

    true # proceed
  end

  # _* a_  _* a_ b_  stem_* + a_ b_  . . .
  private def distribute?(op : Op::Item::PaddedLeft, items : Tzip::ItemsView, sink) : Bool
    n = op.r.items.size
    if items.size < n
      return false # mismatch
    end

    l = items.before(items.size - n)
    r = items.last(n)

    distribute?(op.l, l, sink) && distribute?(op.r, r, sink)
  end

  # a_ _*  a_ b_ _*  + a_ b_ rest_*  . . .
  private def distribute?(op : Op::Item::PaddedRight, items : Tzip::ItemsView, sink) : Bool
    n = op.l.items.size
    if items.size < n
      return false # mismatch
    end

    l = items.first(n)
    r = items.starting_at(n)

    distribute?(op.l, l, sink) && distribute?(op.r, r, sink)
  end

  # l_* a_ r_*  l_* a_ b_ r_*  . . .
  private def distribute?(op : Op::Item::Padded, items : Tzip::ItemsView, sink) : Bool
    nm = op.m.items.size
    if items.size < nm
      return false # mismatch
    end

    spare = items.size - nm
    quo, rem = spare.divmod(2)
    nl = quo + (rem > 0 ? 1 : 0)
    # cr = q

    l = items.first(nl)
    m = items.starting_at(nl).first(nm)
    r = items.starting_at(nl + nm)

    distribute?(op.l, l, sink) && distribute?(op.m, m, sink) && distribute?(op.r, r, sink)
  end

  # a_ _* b_  a_ l_* r_* b_  . . .
  private def distribute?(op : Op::Item::MidGap, items : Tzip::ItemsView, sink) : Bool
    nl = op.l.items.size
    nr = op.r.items.size
    if items.size < nl + nr
      return false # mismatch
    end

    l = items.first(nl)
    m = items.starting_at(nl).first(items.size - nl - nr)
    r = items.last(nr)

    distribute?(op.l, l, sink) && distribute?(op.m, m, sink) && distribute?(op.r, r, sink)
  end

  # _* a_ _* b_ _*  l_* a_ b_ m_* c_ d_ r_*  . . .
  private def distribute?(op : Op::Item::PaddedMidGap, items : Tzip::ItemsView, sink) : Bool
    nml = op.ml.items.size
    nmr = op.mr.items.size
    if items.size < nml + nmr
      return false # mismatch
    end

    spare = items.size - nml - nmr
    quo, rem = spare.divmod(3)
    nl = quo + (rem > 0 ? 1 : 0)
    nmm = quo + (rem > 1 ? 1 : 0)
    # cr = q

    l = items.first(nl)
    ml = items.starting_at(nl).first(nml)
    mm = items.starting_at(nl + nml).first(nmm)
    mr = items.starting_at(nl + nml + nmm).first(nmr)
    r = items.starting_at(nl + nml + nmm + nmr)

    distribute?(op.l, l, sink) &&
      distribute?(op.ml, ml, sink) &&
      distribute?(op.m, mm, sink) &&
      distribute?(op.mr, mr, sink) &&
      distribute?(op.r, r, sink)
  end

  private def commit(ctx, spine : SpatialSpine, plan)
    # SANITY: catch bugs where Singular or Flex operators forget to append
    # their claim to `spine.claims`.
    assert spine.claims.size == spine.spatial.flatcount

    # SANITY: make claims are contiguous.
    spine.claims.each_cons_pair do |(b0, e0), (b1, e1)|
      assert b0 <= e0 && e0 == b1 && b1 <= e1
    end

    # This is very important for slots such as ``(+ a_ b_ ⏏`last⏏)``, or, say,
    # ``(`a `b `c)``. This is also important for groups that end at the end of
    # the pattern, e.g. `(+ (%group args_ a_ b_ c_))`. The Ref's end (or in case
    # of Slot both its begin and end) must have something to point to after
    # the end of the items.
    spine = spine.claim(spine.items.after_end)

    ahead = plan

    spine.spatial.refs.each do |ref|
      # ref.begin points at the item operator *before* which the ref begins.
      # ref.end points at the item operator *before* which the ref ends.
      b, _ = spine.claims[ref.begin]
      e, _ = spine.claims[ref.end]
      view = spine.items.reshape(b, e)

      case item = ref.item
      in Op::Item::Slot
        ctx = ctx.join(item.name, view.ref(ord: ref.ord))
      in Op::Item::Group
        ahead = ctx.interject(ahead, Action.match(item.successor, view.collect))
      end
    end

    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # {a: x_, b: y_} -> (%layer () {a: x_, b: y_})
  # {¦ a_ b_} -> (%layer _ {a: a_, b: b_})
  # (_ ¦ xs_ a b) -> (%layer xs_ {a: _, b: _})
  # etc...
  def match(ctx, op : Op::Layer, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    ahead = ctx.interject(plan, Action.entrybcast(op.side, matchee))

    case op.below
    when Op::Pass, Op::Dict, Op::Pairsonly
      # Match ahead immediately. Do not waste time on residue. Entry operators
      # don't care about entries other than themselves in the dict.
      #
      # {¦ a_ b_}  (/ a_ b_ ¦ _ ±precision)
      cons(ctx, ahead)
    else
      # Subtract keys from op.side to obtain residue. Match residue, then
      # go on to ahead.
      #
      # (/ a_ b_ ¦ rest_ ±precision)
      residue = matchee.without(op.side, &.key)
      cons(ctx, op.below, residue, ahead)
    end
  end

  # :nodoc:
  def probably_matches?(op : Op::Layer, matchee : Term) : Bool
    return false unless dict = matchee.as_d?

    op.side.each do |entry|
      return false unless probably_matches?(entry, dict)
    end

    true
  end

  # :nodoc:
  #
  # E.g., the pattern
  #
  #   ((%filter (k) {¦ k_ v_} (%flat (_ v) vs_)) k_)
  #
  # ... with the following matchee:
  #
  #   (({k: a, v: 100} {k: b, v: 200} {k: c, v: 300} {k: a, v: "Hello"}) a)
  #
  # ... gives the match env `{k: a, vs: (100 "Hello")}`
  #
  # Filter's successor runs in something that resembles an isolated "branch",
  # in that filter cannot participate in defining the original *plan*. This
  # is because if it did, we'd have the possibility of a feedback loop and
  # thus, we'd need fixpoint constraints, which is *monstrously unnecessary*
  # here. So we resort to this simpler behavior, as the diagram below shows:
  #
  # ```text
  #           │            │            │
  # matchee   │   filter   │    plan    │   successor
  #           │            │            │
  #   o───────┼─────►o     │            │
  #      constrain      match rest      │
  #           │      o─────┼─────►o     │
  #           │            │            │
  #           │      o◄────┼──────o     │
  #           │        eval feedback    │
  #   o◄──────┼──────o     │            │
  #     select matching    │            │
  #           │            │            │
  #           │            │            │
  #   o───────┼────────────┼────────────┼──────►o
  #          envlist with envs for matching
  #           │            │            │
  #
  # │
  # │ Notice how neither the matchee nor the successor
  # │ are interjected between filter and plan.
  # │
  # ```
  def match(ctx, op : Op::Filter, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?
    return Fb[] unless matchee.term.itemsonly?

    # Collect choices for dependencies.
    choices = {} of Term => Set(Term)
    matchee.items.each do |item|
      fb = eval(match(ctx.sibling, op.selector, item, nil))
      fb.each do |response|
        op.deps.each do |dep|
          tmp = nil

          # Look at the captures.
          if value = response.capture?(dep)
            tmp ||= choices.put_if_absent(dep) { Set(Term).new }
            tmp << value.term
          end

          # Intersect with choice set as well.
          if choiceset = response.choices?(dep)
            tmp ||= choices.put_if_absent(dep) { Set(Term).new }
            tmp.reject! { |option| !option.in?(choiceset) }
          end
        end
      end
    end

    # Intersect our choices with existing choices.
    op.deps.each do |dep|
      next unless choiceset = choices[dep]?

      ctx = ctx.intersect(dep, choiceset)
    end

    # Run the rest of the pattern.
    eval(fb(ctx, plan)) do |response|
      # Collect assignments and choices for our dependencies.
      envtab = response.envtab(op.deps)
      choicetab = response.choicetab(op.deps)

      # If some choices are empty, this means the rest of the pattern over-
      # restricted them (or it was us over-restricting or not proposing
      # any choices in the first place!)
      #
      # Report mismatch unless filter is set to have min: 0, in which case
      # we proceed.
      present = choicetab.all? { |_, choices| choices.present? }
      unless present || op.min.zero?
        return Fb[]
      end

      successor = Envlist.new(op.successor, min: op.min, max: op.max)

      match(response, successor, plan: nil) do |push|
        # If min: 0 and matches are absent, not pushing anything at all means
        # envlist is empty. This is an expected case for `match(..., Envlist, ...)`,
        # so we let it handle that.
        next unless present

        matchee.items.each do |item|
          # During this pass over items, we run in selector: true mode. See
          # `Context` to learn more about selector mode.
          itemctx = response.sibling(envtab: envtab, choicetab: choicetab, selector: true)

          fb = eval(match(itemctx, op.selector, item, plan: nil))
          fblog = Log.seal(Log.simplify(item.log))

          unless push.call?(fb, fblog)
            return Fb[] # does not fit (max envs exceeded)
          end
        end
      end
    end
  end

  # :nodoc:
  #
  # (%pluck (_ x) xs_)
  def match(ctx, op : Op::Pluck, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    cons(ctx, op.successor, matchee.pluck(op.spec), plan)
  end

  # :nodoc:
  #
  # ((%flat (_ k) (%items (k0_ _* kN_) k_)) _)
  def match(ctx, op : Op::Flat, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    cons(ctx, op.successor, matchee.flat(op.spec), plan)
  end

  # :nodoc:
  #
  # (%split _ a_ ⟨b_⟩)  (%split _ a_ (%split _ b_ _))  (%split (_ a_ (%split b_ ⟨c_⟩))) . . .
  def match(ctx, op : Op::Adjacent, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?
    return Fb[] unless matchee.term.itemsize >= op.members.size

    ahead = ctx.interject(plan, Action.fzip(op.members, matchee.items))
    cons(ctx, ahead)
  end

  #
  # Sequence operators
  #

  # :nodoc:
  record Feed, ops : Slice(Any), spine : SpatialSpine do
    alias Any = Op::Item::Flex | AssertAfter | ManyStop | PastTail

    record AssertAfter, index : UInt32
    record ManyStop,
      ctx : Context,
      many : Op::Item::ManyMax,
      plan : Plan,
      stops : Slice(UInt32),
      envs : Slice(Tzip)

    record PastTail, op : Op::Item::Past, n : Int32, fst : UInt32

    def self.new(ops : Indexable(Op::Item::Distrib | Op::Item::NonDistrib), spine : SpatialSpine)
      new(Slice(Any).empty, spine).prepend(ops)
    end

    def first? : Any?
      ops.first?
    end

    def +(offset : Int)
      Feed.new(ops + 1, spine)
    end

    def claim(*args)
      copy_with(spine: spine.claim(*args))
    end

    def prepend(prefix : Indexable(Op::Item::Distrib | Op::Item::NonDistrib))
      buffer = Pf::Kit.stack_array(Any, 16)

      prefix.each do |op|
        if op.is_a?(Op::Item::NonDistrib)
          op.items.each { |item| buffer << item }
          next
        end

        buffer << op
      end

      Feed.new(ops.prepend_many(buffer, &.itself), spine)
    end

    def prepend(op : Any)
      Feed.new(ops.prepend(op), spine)
    end

    # TODO: do it in a single allocation!!
    def prepend_all(*args)
      current = self
      args.reverse_each { |arg| current = current.prepend(arg) }
      current
    end
  end

  # :nodoc:
  def match(ctx, op : Feed::AssertAfter, feed : Feed, items : Tzip::ItemsView, plan)
    if items.begin <= op.index
      return Fb[] # No, not after!
    end

    # Yes, after, proceed.
    ahead = ctx.interject(plan, Action.match(feed, items))
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # (%flex a_)
  def match(ctx, op : Op::Item::FlexSingular, feed : Feed, items : Tzip::ItemsView, plan)
    return Fb[] unless item = items.first?

    ahead = ctx.interject(plan,
      Action.match(op.successor, item),
      Action.match(feed.claim(items.first(1)), items + 1),
    )
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # Defines the behavior of distrib regions such as a_ ⏏l_* m_* r_*⏏ b_. When
  # located alongside another Flex, as in a_ ⏏l_* m_* r_*⏏ (%past/max _ max: 3) b_,
  # Distribs behave *greedily* (i.e., as max item operators do).
  def match(ctx, op : Op::Item::Distrib, feed : Feed, items : Tzip::ItemsView, plan)
    items.each_partition_greedy do |before, after|
      fb = eval(match(ctx, op, feed, before, after, plan))
      return fb if fb.present?
    end

    Fb[]
  end

  # :nodoc:
  def match(ctx, op : Op::Item::PluralMinMax | Op::Item::GapFirstMinMax, feed : Feed, items : Tzip::ItemsView, plan)
    each_partition(op, items) do |before, after|
      fb = eval(match(ctx, op, feed, before, after, plan))
      return fb if fb.present?
    end

    Fb[]
  end

  # :nodoc:
  def match(ctx, op : Op::Item::GapSource, feed : Feed, items : Tzip::ItemsView, plan)
    sink = Pf::Kit.stack_array(Context, 8)

    each_partition(op, items) do |before, after|
      fb = eval(match(ctx, op, feed, before, after, plan))
      fb.each { |response| sink << response }
    end

    Fb[sink]
  end

  private def each_partition(op : Op::Item::Min, items : Tzip::ItemsView, &)
    items.each_partition_lazy { |before, after| yield before, after }
  end

  private def each_partition(op : Op::Item::Max, items : Tzip::ItemsView, &)
    items.each_partition_greedy { |before, after| yield before, after }
  end

  # :nodoc:
  def match(ctx, op : Op::Item::Distrib, feed : Feed, run : Tzip::ItemsView, rest : Tzip::ItemsView, plan)
    run.each_chunk(op.contenders.size, empty: true) do |chunk, index|
      feed = feed.claim(chunk)
    end

    ahead = ctx.interject(plan, Action.match(feed, rest))

    run.each_chunk(op.contenders.size, empty: true) do |chunk, index|
      contender = op.contenders[index]
      ahead = ctx.interject(ahead, Action.match(contender, chunk))
    end

    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # xs_*
  def match(ctx, op : Op::Item::PluralDistrib, run : Tzip::ItemsView, plan)
    return Fb[] unless op.min <= run.size <= op.max
    return Fb[] unless op.type.any? || run.all?(&.type.subtype?(op.type))

    ahead = plan

    # (%plural)  (%plural xs)  xs_*  _*
    if capture = op.capture
      # (%plural xs)  xs_*
      ahead = ctx.interject(plan, Action.capture(capture, run.collect))
    end

    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # (%plural/min xs max: 3)  (%plural/max)
  def match(ctx, op : Op::Item::PluralMinMax, feed : Feed, run : Tzip::ItemsView, rest : Tzip::ItemsView, plan)
    return Fb[] unless op.min <= run.size <= op.max
    return Fb[] unless op.type.any? || run.all?(&.type.subtype?(op.type))

    # Schedule matching the rest of items after claiming *run*.
    feed = feed.claim(run)
    ahead = ctx.interject(plan, Action.match(feed, rest))

    # (%plural/max)  (%plural/max xs)  xs_*  _*
    if capture = op.capture
      # (%plural/max xs)  xs_*
      ahead = ctx.interject(ahead, Action.capture(capture, run.collect))
    end

    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # (%gap n_)
  def match(ctx, op : Op::Item::GapFirstDistrib, run : Tzip::ItemsView, plan)
    # NOTE: It makes no sense to refer to the gap's n, hence Log.none:
    #
    #   ((%gap n_)) <> {n: 10}
    #
    # This backmap has no meaning.
    n = Tzip.new(Term.of(run.size), Log.none)

    ahead = ctx.interject(plan, Action.match(op.measurer, n))
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # (%gap/min n_)  (%gap/max (%number _ < (var hi)))  (%gap/min° n_)
  def match(ctx, op : Op::Item::GapMinMax, feed : Feed, run : Tzip::ItemsView, rest : Tzip::ItemsView, plan)
    # Ditto: it makes no sense to refer to the gap's n.
    n = Tzip.new(Term.of(run.size), Log.none)

    feed = feed.claim(run)
    ahead = ctx.interject(plan,
      Action.match(op.measurer, n),
      # Schedule matching the rest of items after claiming *run*.
      Action.match(feed, rest),
    )
    cons(ctx, ahead)
  end

  # :nodoc:
  def match(ctx, op : Op::Item::Past, feed : Feed, items : Tzip::ItemsView, plan)
    match(ctx, Feed::PastTail.new(op, 0, items.begin), feed, items, plan)
  end

  # :nodoc:
  def match(ctx, tail : Feed::PastTail, feed : Feed, items : Tzip::ItemsView, plan)
    match(ctx, tail.op, tail.n, tail.fst, feed, items, plan)
  end

  # :nodoc:
  #
  # ((%past a_ b_) and _*)  ((%past _number min: 3 max: 5))
  #
  # NOTE: One important edge case to keep in mind: (%past `slot) (i.e., slot
  # matches zero elements, and %past is a looping construct -- hazardous!)
  def match(ctx, op : Op::Item::PastMin, n : Int32, fst : UInt32, feed : Feed, items : Tzip::ItemsView, plan)
    return Fb[] if n > op.max

    # If this %past's min constraint is satisfied, then it can start removing
    # itself from the pattern.
    if n >= op.min
      fb = eval(match(ctx, feed.claim(fst, items.begin), items, plan))
      return fb if fb.present?
    end

    # If this %past's min constraint is unsatisfied, or if the pattern ahead
    # does not match, %past tries to help by prepending its children followed
    # by itself-advanced, taking as little items as possible.
    each_partition(op, items) do |before, after|
      successors = feed.prepend_all(
        Feed::AssertAfter.new(items.begin),
        Feed::PastTail.new(op, n + 1, fst),
      )

      ahead = ctx.interject(plan, Action.match(successors, after))
      fb = eval(match(ctx, op.children, before, ahead))
      return fb if fb.present?
    end

    Fb[]
  end

  # :nodoc:
  #
  # ((%past/max _ _ min: 2 max: 5))  (+ (%past/max _number min: 2))
  #
  # NOTE: One important edge case to keep in mind: (%past/max `slot) (i.e., slot
  # matches zero elements, and %past/max is a looping construct -- hazardous!)
  def match(ctx, op : Op::Item::PastMax, n : Int32, fst : UInt32, feed : Feed, items : Tzip::ItemsView, plan)
    return Fb[] if n > op.max

    # Greedy %past unconditionally prepends its children and then itself-advanced.
    each_partition(op, items) do |before, after|
      successors = feed.prepend_all(
        Feed::AssertAfter.new(items.begin),
        Feed::PastTail.new(op, n + 1, fst),
      )

      ahead = ctx.interject(plan, Action.match(successors, after))
      fb = eval(match(ctx, op.children, before, ahead))
      return fb if fb.present?
    end

    # If this %past's n is below min, it cannot help the matching process by removing
    # itself from the pattern -- this will cause the min constraint to be violated,
    # and %past doesn't want that.
    return Fb[] if n < op.min

    # This %past's n is above min -- its min constraint is satisfied. This means it
    # can try to remove itself from the pattern to see if that leads to
    # a successful match.
    ahead = ctx.interject(plan, Action.match(feed.claim(fst, items.begin), items))
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # (%optional 0 x_)  (%optional (0 0) (x_ y_))
  def match(ctx, op : Op::Item::Optional, feed : Feed, items : Tzip::ItemsView, plan)
    if items.present?
      ahead = ctx.interject(plan,
        Action.match(op.successor, items.first),
        Action.match(feed.claim(items.first(1)), items + 1),
      )
      fb = eval(fb(ctx, ahead))
      return fb if fb.present?
    end

    default = items.tzip.insert(op.default, before: items.begin, ord: op.ord)

    ahead = ctx.interject(plan,
      Action.match(op.successor, default),
      Action.match(feed.claim(items.before_begin), items),
    )
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # (_* x_ ⏏(%many {¦ x_ y_} _* x_ y_ _*)⏏ y_ _*)
  def match(ctx, op : Op::Item::ManyMax, feed : Feed, items : Tzip::ItemsView, plan)
    match(ctx.sibling, Feed::ManyStop.new(ctx, op, plan, stops: Slice(UInt32).empty, envs: Slice(Tzip).empty), feed, items, plan: nil)
  end

  # :nodoc:
  #
  # WARNING: this is called with ISOLATED ctx and plan. The original ctx and plan
  # are stored in *op*.
  def match(ctx, op : Feed::ManyStop, feed : Feed, items : Tzip::ItemsView, plan)
    return Fb[] if op.stops.size > op.many.max

    if b = op.stops.last?
      content = items.reshape(b, items.begin)
      handle = Log.seal(Log.simplify(content.log))

      # Construct a mapping log for each match of children so that e.g. in:
      #
      #   (%many (env←{¦ x: K_} _*) x_ y_)
      #
      # ... K_ points to x_ in the respective match. We set the mapping's handle
      # to the range matched by %many's current instance of children; each match's
      # *env* should refer to that range:
      #
      #    K₀   K₁   K₂
      #    v    v    v
      #   (1 2  3 4  5 6)
      #   [---][---][---]
      #    env₀ env₁ env₂
      #
      # Please note that in this example, Ks and envs are later unified which
      # leads to a mismatch. But at this point, we're looking at an array of
      # envs (hence the use of subscript).
      env = Tzip.mapping(ctx.envtab, handle, &.itself)
      op = op.copy_with(envs: op.envs.append(env))
    end

    if op.stops.size >= op.many.min
      # Construct the toplevel envs list lst←(env₀ env₁ ...) and its corresponding
      # mapping. lst should refer to all items matched.
      #
      # Each env's log is a Mapping (see above): its handle refers to the range
      # matched by env₀, env₁ and so on; we concatenate them to get a single handle.
      handle = Log.seal(op.envs, &.log)
      envlist = Tzip.mapping(op.envs, handle) { |env, index| {Term.of(index), env} }

      if lbound = op.stops.first?
        # Nonempty %many
        successors = feed.claim(lbound, items.begin)
      else
        # Empty %many (valid with min: 0)
        successors = feed.claim(items.before_begin)
      end

      # Run %many's successor and all the following items with the original context
      # and plan. ctx and plan, on the other hand, refer to the isolated ones.
      ahead = ctx.interject(op.plan,
        Action.match(op.many.successor, envlist),
        Action.match(successors, items),
      )

      fb = eval(fb(op.ctx, ahead))
      return fb if fb.present?
    end

    successors = feed.prepend_all(
      Feed::AssertAfter.new(items.begin),
      op.copy_with(stops: op.stops.append(items.begin))
    )

    assert plan.nil?

    each_partition(op.many, items) do |before, after|
      ahead = ctx.interject(plan, Action.match(successors, after))

      # .sibling gives us a fresh empty context to use for the next run of children.
      # We know that the plan is empty at this point so we can reuse it.
      fb = eval(match(ctx.sibling, op.many.members, before, ahead))
      return fb if fb.present?
    end

    Fb[]
  end

  # :nodoc:
  def match(ctx, ops : Feed, items : Tzip::ItemsView, plan)
    op = ops.first?
    if op.nil? && items.empty? # Matched all items, go back to spine.
      return match(ctx, ops.spine, plan)
    end

    return Fb[] if op.nil? # Ran out of items.

    match(ctx, op, ops + 1, items, plan)
  end

  #
  # Entry operators
  #

  # :nodoc:
  #
  # a: 100  a: a_
  def match(ctx, op : Op::Entry::Required, matchee : Tzip, plan)
    return Fb[] unless value = matchee[op.key]?

    cons(ctx, op.value, value, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Entry::Required, matchee : Term::Dict) : Bool
    return false unless value = matchee[op.key]?

    probably_matches?(op.value, value)
  end

  # :nodoc:
  #
  # {¦ ⏏a⏏ ⏏b⏏} {⏏a: _⏏, ⏏b: _number⏏}
  def match(ctx, op : Op::Entry::Present, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?

    # NOTE: Assume Dict#includes? is faster than fetching the value, although
    # it might not be in practice.
    if op.type.any?
      return dict.includes?(op.key) ? cons(ctx, plan) : Fb[]
    end

    return Fb[] unless value = dict[op.key]?
    return Fb[] unless value.type.subtype?(op.type)

    cons(ctx, plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Entry::Present, matchee : Term::Dict) : Bool
    matchee.includes?(op.key)
  end

  # :nodoc:
  #
  # {¦ ⏏-x⏏}  {a: (%- _)}
  def match(ctx, op : Op::Entry::Absent, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] if dict.includes?(op.key)

    cons(ctx, plan)
  end

  # :nodoc:
  #
  # {¦ ⏏-x_⏏}  {a: (%- _ a)}
  def match(ctx, op : Op::Entry::AbsentKeypath, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] if dict.includes?(op.key)

    ref = matchee.ref(op.key)
    cons(ctx.join(op.name, ref), plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Entry::Absent | Op::Entry::AbsentKeypath, matchee : Term::Dict) : Bool
    !matchee.includes?(op.key)
  end

  # :nodoc:
  #
  # {¦ ⏏x⋮ 100⏏}  x: (%optional (+ 1 2) (+ a_ b_))
  def match(ctx, op : Op::Entry::Optional, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    if value = matchee[op.key]?
      return cons(ctx, op.value, value, plan)
    end

    cons(ctx, op.value, matchee.with(op.key, op.default), plan)
  end

  # :nodoc:
  #
  # {¦ ⏏x: (%- _number)⏏} -- means x must be absent OR must NOT be a number.
  def match(ctx, op : Op::Entry::Negative, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    unless value = matchee[op.key]?
      return cons(ctx, plan)
    end

    # NOTE: This "contraption" here isolates the barrier pattern from the rest
    # of the pattern. It makes very little to no sense to execute the barrier
    # in the context of the larger pattern. Captures in the barrier:
    #
    #   x: (%- ⏏foo_⏏)
    #
    # ... make very little sense, for example. Okay, the barrier matches, the matching
    # term is captured, but then because the barrier matched, `%-` immediately aborts
    # *everything*: the entire pattern along with all captures the barrier and
    # the rest of the match process made are dropped on the floor, and a mismatch is
    # signalled. What was the point of capturing in the barrier, again?..
    fb = eval(match(ctx.sibling, op.barrier, value, plan: nil))
    fb.present? ? Fb[] : fb(ctx, plan)
  end

  # :nodoc:
  #
  # {¦ ⏏-x_number⏏} -- ditto.
  def match(ctx, op : Op::Entry::NegativeKeypath, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    if value = matchee[op.key]?
      fb = eval(match(ctx.sibling, op.barrier, value, plan: nil))
      return Fb[] if fb.present?
    end

    ref = matchee.ref(op.key)
    cons(ctx.join(op.name, ref), plan)
  end

  # :nodoc:
  def probably_matches?(op : Op::Entry::Any, matchee : Term::Dict) : Bool
    true
  end

  # :nodoc:
  #
  # Utility function to go to the next action planned. If none, this returns
  # *ctx* as feedback immediately. Hence the name: you are effectively asking
  # for feedback, either through further processing of *plan* or immediately
  # for *ctx*).
  def fb(ctx : Context, plan : Plan)
    unless action = ctx.action?(plan)
      return Fb[ctx]
    end

    action.call(ctx, ctx.dequeue(plan))
  end

  # :nodoc:
  #
  # See `eval(Cons)`.
  defrecord Cons, ctx : Context, plan : Plan

  # :nodoc:
  def cons(ctx : Context, plan : Plan)
    Cons.new(ctx, plan)
  end

  # # :nodoc:
  # def cons(ctx : Context, ops : Feed, items : Tzip::ItemsView, plan : Plan)
  #   ahead = ctx.interject(plan, Action.match(ops, items))
  #   cons(ctx, ahead)
  # end

  # :nodoc:
  def cons(ctx : Context, op : Op::Any | Op::Entry::Any, matchee : Tzip, plan : Plan)
    ahead = ctx.interject(plan, Action.match(op, matchee))
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # `Cons` and eval exist to reduce recursion depth during matching. Without cons
  # and eval, with this matching algorithm, it is trivial to obtain recursion depth so
  # large we SEGFAULT. Something along the lines of `(_ _ _ _ ...)`, `_`s repeated
  # a few thousand times, does the job: a SEGFAULT, recursion too deep, we're
  # out of stack space. This is because for each of those `_`s, we effectively recurse
  # on ahead, and each such recursion isn't just one call but in fact a "block" of
  # calls -- maybe 5 calls or so, per `_`, each call potentially containing stack-
  # allocated buffers and such.
  #
  # But notice that throughout the algorithm, the vast majority of operators
  # are tail-recursion-like: not exactly tail recursion since they all go through
  # the huge `match~` multi-dispatch method or `fb`, so it's not something LLVM
  # or the Crystal compiler can see; but something so close we can do this
  # classic trick of using a loop instead of recursion, manually.
  #
  # More practically, whenever a match() or an Action or an fb() wants to pass
  # control to another pattern or `fb` for the rest of the match, after it approved
  # the matchee and modified plan/context appropriately, it uses one of `cons`
  # overloads to construct and eventually return Cons.
  #
  # Whenever anyone needs an Fb, they can't just call match() or fb(), because
  # they'll get Fb | Cons. To get rid of Cons, you use eval(), which gives
  # you Fb in exchange for Cons by doing something very similar to a tail-
  # call loop.
  #
  # Cons doesn't make very deep recursions impossible -- not at all! But it is
  # much harder, with Cons, to generate such recursions. I am yet to come up
  # with a pattern that is trivially generatable and that hits a SEGFAULT
  # (not that I tried!)
  #
  # Simply put, `Fb` means "here is your result, no further work". On the other
  # hand, `Cons` means "here is the next step; please keep going". Both are told
  # to `eval` somewhere up the call chain.
  def eval(arg : Cons) : Fb
    loop do
      case result = fb(arg.ctx, arg.plan)
      in Fb
        return result
      in Cons
        arg = result
      end
    end
  end

  # :nodoc:
  def eval(arg : Fb) : Fb
    arg
  end

  # :nodoc:
  def eval(arg, & : Context -> Fb | Cons) : Fb
    fb = eval(arg)
    fb.reduce(Fb[]) { |memo, ctx| memo |= eval(yield ctx) }
  end

  # :nodoc:
  #
  # `%keypath` operators use this exception to request that `log:` should be set
  # to `true` and another round of `match` should be attempted.
  #
  # In practice, RequestLog is almost never raised (that's why it's modeled as
  # an exception, because RequestLog *is* exceptional, even though at the core
  # is a control flow construct).
  class RequestLog < Exception
    @callstack = CallStack.empty
  end

  # :nodoc:
  def match(env : Term::Dict, op : Op::Any, matchee : Term, *, log : Bool = false, & : Fb -> T) : T forall T
    2.times do # 2 means before and after RequestLog is raised.
      Context.new(env) do |ctx|
        tzip = Tzip.new(matchee, log ? Log.root : Log.none)

        begin
          fb = eval(match(ctx, op, tzip, plan: nil))
        rescue RequestLog
          unless log
            log = true
            next
          end

          # `%keypath` is expected to RequestLog again if it's in an unreachable spot
          # (because it doesn't see the difference between log: false and unavailability
          # of the log at its particular spot). `%keypath` will never match when it's in
          # an unreachable spot. So we trigger a mismatch immediately.
          fb = Fb[]
        end

        return yield fb
      end
    end

    Intrinsics.unreachable
  end
end
