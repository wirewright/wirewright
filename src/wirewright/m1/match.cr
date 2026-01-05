module Ww::M1next
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

    alias Any = Lzip | Rzip | Fzip | Match | MatchSeq | MatchCst | EntryBroadcast | MakeCapture

    # :nodoc:
    struct Lzip
      def initialize(@ops : Slice(O::Any), @matchees : Tzip::ItemsView | Slice(Tzip))
        assert @ops.size == @matchees.size
      end

      def call(ctx : Context, plan : Plan)
        unless op = @ops.first?
          # => @matchees.empty?
          return M1next.fb(ctx, plan)
        end

        ahead = ctx.interject(plan, Action.lzip(@ops + 1, @matchees + 1))
        M1next.match(ctx, op, @matchees.first, ahead)
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
      def initialize(@ops : Slice(O::Any), @matchees : Tzip::ItemsView | Slice(Tzip))
        assert @ops.size == @matchees.size
      end

      def call(ctx : Context, plan : Plan)
        unless op = @ops.last?
          # => @matchees.empty?
          return M1next.fb(ctx, plan)
        end

        ahead = ctx.interject(plan, Action.rzip(@ops - 1, @matchees - 1))
        M1next.match(ctx, op, @matchees.last, ahead)
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
      def initialize(@ops : Slice(O::Any), @matchees : Tzip::ItemsView | Slice(Tzip))
      end

      def call(ctx : Context, plan : Plan)
        unless op = @ops.first?
          # Fzip allows any number of items ahead. Proceed with a match.
          return M1next.fb(ctx, plan)
        end

        remaining = @matchees

        loop do
          unless matchee = remaining.first?
            # Ran out of items but some operators left, mismatch.
            return Fb[]
          end

          # On success, skip the current op and matchee and proceed further.
          ahead = ctx.interject(plan, Action.fzip(@ops + 1, remaining + 1))
          fb = M1next.eval(M1next.match(ctx, op, matchee, ahead))
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
      def initialize(@op : O::Any | O::Entry::Any, @matchee : Tzip)
      end

      def call(ctx : Context, plan : Plan)
        M1next.match(ctx, @op, @matchee, plan)
      end
    end

    # Direct call to match overloads for *op* with *matchee*.
    def match(op : O::Any | O::Entry::Any, matchee : Tzip)
      Match.new(op, matchee)
    end

    # :nodoc:
    struct MatchSeq
      def initialize(@ops : Feed, @items : Tzip::ItemsView)
      end

      def call(ctx : Context, plan : Plan)
        M1next.match(ctx, @ops, @items, plan)
      end
    end

    # Direct call to match sequence overloads for *ops* and *items*.
    def match(ops : Feed, items : Tzip::ItemsView)
      MatchSeq.new(ops, items)
    end

    # :nodoc:
    struct MatchCst
      def initialize(@capture : Term, @cst : Cst::Any, @matchee : Tzip)
      end

      def call(ctx : Context, plan : Plan)
        M1next.match(ctx, @capture, @cst, @matchee, plan)
      end
    end

    # Direct call to match constraint overloads for *capture*, *cst* and *matchee*.
    def match(capture : Term, cst : Cst::Any, matchee : Tzip)
      MatchCst.new(capture, cst, matchee)
    end

    # :nodoc:
    struct EntryBroadcast
      def initialize(@entries : Slice(O::Entry::Any), @matchee : Tzip)
      end

      def call(ctx : Context, plan : Plan)
        unless entry = @entries.first?
          return M1next.fb(ctx, plan)
        end

        ahead = ctx.interject(plan, Action.entrybcast(@entries + 1, @matchee))

        M1next.match(ctx, entry, @matchee, ahead)
      end
    end

    # Shows (broadcasts) the same *matchee* to each entry in turn.
    def entrybcast(entries : Slice(O::Entry::Any), matchee : Tzip)
      EntryBroadcast.new(entries, matchee)
    end

    # :nodoc:
    struct MakeCapture
      def initialize(@capture : Term, @proposal : Tzip)
      end

      def call(ctx : Context, plan : Plan)
        M1next.capture(ctx, @capture, @proposal, plan)
      end
    end

    # Direct call to `M1next.capture` for *capture* and *proposal*.
    def capture(capture : Term, proposal : Tzip)
      MakeCapture.new(capture, proposal)
    end
  end

  # :nodoc:
  #
  # _
  def match(ctx, op : O::Pass, matchee : Tzip, plan)
    cons(ctx, plan)
  end

  # :nodoc:
  #
  # (%never)
  def match(ctx, op : O::Never, matchee : Tzip, plan)
    Fb[]
  end

  # :nodoc:
  #
  # 100  "hello world"  (+ 1 2)  (%literal qux)
  def match(ctx, op : O::Literal, matchee : Tzip, plan)
    matchee.term == op.term ? cons(ctx, plan) : Fb[]
  end

  # :nodoc:
  #
  # (%any a b c)
  def match(ctx, op : O::LiteralWhitelist, matchee : Tzip, plan)
    op.whitelist.includes?(matchee.term) ? cons(ctx, plan) : Fb[]
  end

  # :nodoc:
  #
  # (%not a b c)
  def match(ctx, op : O::LiteralBlacklist, matchee : Tzip, plan)
    op.blacklist.includes?(matchee.term) ? Fb[] : cons(ctx, plan)
  end

  {% for cls, type in {O::Str => :string, O::Sym => :symbol, O::Boolean => :boolean, O::Dict => :dict} %}
    # :nodoc:
    #
    # _string  _symbol
    def match(ctx, op : {{cls}}, matchee : Tzip, plan)
      matchee.{{type.id}}? ? cons(ctx, plan) : Fb[]
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
  def match(ctx, op : O::Num, matchee : Tzip, plan)
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
    if lvar = op.min.as?(O::Num::Var)
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
    if rvar = op.max.as?(O::Num::Var)
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
  #
  # (%symbol blank name_ type_)
  def match(ctx, op : O::SymBlank, matchee : Tzip, plan)
    return Fb[] unless row = matchee.blank?

    zname, ztype = row

    ahead = ctx.interject(plan,
      Action.match(op.name, zname),
      Action.match(op.type, ztype),
    )

    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # (%symbol nonblank)
  def match(ctx, op : O::SymNonblank, matchee : Tzip, plan)
    return Fb[] unless symbol = matchee.term.as_sym?
    return Fb[] if symbol.blank?

    cons(ctx, plan)
  end

  # :nodoc:
  #
  # @x_  @x_number  (edge qux_dict)
  def match(ctx, op : O::Edge, matchee : Tzip, plan)
    ML.edge?(matchee.term, type: op.type) ? cons(ctx, plan) : Fb[]
  end

  # :nodoc:
  #
  # (_*)
  def match(ctx, op : O::Itemsonly, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless dict.itemsonly?

    cons(ctx, plan)
  end

  # :nodoc:
  #
  # (¦ _)
  def match(ctx, op : O::Pairsonly, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless dict.pairsonly?

    cons(ctx, plan)
  end

  # :nodoc:
  #
  # (%partition items_ pairs_)  (items_* ¦ pairs_)
  def match(ctx, op : O::Partition, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    ahead = ctx.interject(plan, Action.match(op.pairside, matchee.pairspart))
    match(ctx, op.itemside, matchee.itemspart, ahead)
  end

  # :nodoc:
  #
  # Auxiliary operator emitted with optimization level O1 to check the bounds
  # of a dict.
  def match(ctx, op : O::Bounds, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless op.min <= dict.size <= op.max

    cons(ctx, plan)
  end

  # :nodoc:
  #
  # Same as Bounds but has a successor.
  def match(ctx, op : O::BoundsGuard, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless op.min <= dict.size <= op.max

    cons(ctx, op.successor, matchee, plan)
  end

  # :nodoc:
  #
  # Auxiliary operator emitted with optimization level O1 to check whether the dict's
  # depth is in some expected depth range (the expected depth range is computed from
  # the pattern).
  def match(ctx, op : O::MaxDepth, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    # FIXME: currently we're unable to use #max of MaxDepth, since Dict#maxdepth
    # is maximum-ever depth rather than current maximum depth, so #max is
    # too strict.
    return Fb[] unless op.min <= dict.maxdepth

    cons(ctx, op.successor, matchee, plan)
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
  def match(ctx, op : O::SketchSubset, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless dict.sketch_superset_of?(op.sketch)

    cons(ctx, op.successor, matchee, plan)
  end

  # :nodoc:
  #
  # Auxiliary operator emitted with optimization level O1. It is used, wherever
  # possible, to fuse the checks defined separately above to ensure they're local,
  # lack allocation overhead, and don't jump all over the place & call stuff
  # recursively -- confusing the CPU very much. This is M1's "rejection highway".
  def match(ctx, op : O::DictGuard, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] unless dict.sketch_superset_of?(op.sketch)
    return Fb[] unless op.bounds[0] <= dict.size <= op.bounds[1]
    # FIXME: Ditto the MaxDepth FIXME above, here it's op.depth[1].
    return Fb[] unless op.depth[0] <= dict.maxdepth

    cons(ctx, op.successor, matchee, plan)
  end

  # :nodoc:
  #
  # (%let x _)
  def match(ctx, op : O::Capture, matchee : Tzip, plan)
    capture(ctx, op.capture, matchee, op.successor, matchee, plan)
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
  def match(ctx, op : O::ItemFirst, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_itemsonly_d?
    return Fb[] unless dict.itemsize > 0

    cons(ctx, op.successor, matchee[0], plan)
  end

  # :nodoc:
  #
  # (_* x_)
  def match(ctx, op : O::ItemLast, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_itemsonly_d?
    return Fb[] unless dict.itemsize > 0

    cons(ctx, op.successor, matchee[dict.hi], plan)
  end

  # :nodoc:
  #
  # (xs_*)
  def match(ctx, op : O::CaptureItemsonly, matchee : Tzip, plan)
    return Fb[] unless matchee.term.as_itemsonly_d?

    capture(ctx, op.capture, Tzip.new(matchee.term, matchee.items.log), O::INSTANCE_PASS, matchee, plan)
  end

  # :nodoc:
  #
  # (x_ y_ z_)  (x_ y_ z_ _*)  (_* x_ y_ z_)
  def match(ctx, op : O::SingularSeq, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_itemsonly_d?

    # We expect more items than the dict can provide. No way the match
    # can succeed.
    return Fb[] if op.items.size > dict.itemsize

    # exhaustive=true: (x_ y_ z_)
    # exhaustive=false: (x_ y_ z_ _*) (_* x_ y_ z_)
    return Fb[] if op.exhaustive && op.items.size != dict.itemsize

    items = matchee.items.trim(op.items.size)

    if op.reverse
      # (_* x_ y_)  (_* x_ y_ ⏏) (_* x_ ⏏ y_) (_* ⏏ x_ y_)
      action = Action.rzip(op.items, items)
    else
      # (x_ y_ _*)  (⏏ x_ y_ _*) (x_ ⏏ y_ _*) (x_ y_ ⏏ _*)
      action = Action.lzip(op.items, items)
    end

    ahead = ctx.interject(plan, action)
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # (%any° a_ b_ c_)
  def match(ctx, op : O::ChoiceSource, matchee : Tzip, plan)
    eval(match(ctx, op.a, matchee, plan)) | eval(match(ctx, op.b, matchee, plan))
  end

  # :nodoc:
  #
  # (%all a_ b_)
  def match(ctx, op : O::Both, matchee : Tzip, plan)
    ahead = ctx.interject(plan, Action.match(op.b, matchee))
    match(ctx, op.a, matchee, ahead)
  end

  # :nodoc:
  #
  # (%keytest a b c) -- has any key of those listed
  def match(ctx, op : O::Keytest, matchee : Tzip, plan)
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
  def match(ctx, op : O::Keypool, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] if dict.size > op.keys.size # Remember that all keys are unique!

    n = op.keys.count(&.in?(dict))
    if dict.size > n
      return Fb[] # Dict has extra keys.
    end

    cons(ctx, plan)
  end

  # :nodoc:
  #
  # (%-keypool a b c) -- dict has keys other than those listed (i.e., some extra keys)
  def match(ctx, op : O::NegativeKeypool, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?

    n = op.keys.count(&.in?(dict))
    if dict.size <= n
      return Fb[] # Dict does not have extra keys.
    end

    cons(ctx, plan)
  end

  # FIXME: Dfs/Bfs should already be equipped with alg, we shouldn't
  # have to compute it! This should be done during compilation. This'd
  # also give clients a lot more choice!
  private def alg(op : O::Dfs | O::Bfs)
    case op.part
    in .items?   then opts = {itemsord: Tzip::Order::Lexical, pairsord: Tzip::Order::Skip}
    in .pairs?   then opts = {itemsord: Tzip::Order::Skip, pairsord: Tzip::Order::Lexical}
    in .entries? then opts = {itemsord: Tzip::Order::Lexical, pairsord: Tzip::Order::Lexical}
    end

    opts = opts.merge(
      mindepth: op.depth0 ? 0u32 : 1u32,
      maxdepth: UInt32::MAX,
    )

    case op
    in O::Dfs then Tzip::DfsPreorder.new(**opts)
    in O::Bfs then Tzip::Bfs.new(**opts)
    end
  end

  private def eligible?(op : O::Scan, matchee : Tzip) : Bool
    matchee.dict? && matchee.term.itemsize >= op.seq.size
  end

  private def eligible?(op : O::ScanAll, matchee : Tzip) : Bool
    matchee.dict? && (op.minM.zero? || matchee.term.itemsize >= op.seq.size)
  end

  private def eligible?(op : O::Dfs | O::Bfs, matchee : Tzip) : Bool
    op.depth0 ? true : (matchee.dict? && matchee.term.size > 0)
  end

  private def eligible?(op : O::Entries, matchee : Tzip) : Bool
    matchee.dict?
  end

  private def eligible?(op : O::Split, matchee : Tzip) : Bool
    matchee.dict? && matchee.term.itemsize >= op.focus.size
  end

  private def eligible?(op : O::SplitAll, matchee : Tzip) : Bool
    matchee.dict? && (op.minM.zero? || matchee.term.itemsize >= op.focus.size)
  end

  private def search(op : O::Scan, matchee : Tzip, & : Tzip::ItemsView -> Bool) : Nil
    current = matchee.items

    while current.size >= op.seq.size
      accepted = yield current.trim(op.seq.size)
      current += accepted ? op.seq.size : 1
    end
  end

  # WARNING: The yielded slices are valid for the lifetime of that particular
  # (per-iteration) block yield!
  private def search(op : O::Dfs | O::Bfs, matchee : Tzip, & : Slice(Tzip) -> Bool) : Nil
    assert op.seq.size > 0

    alg = alg(op)

    # NOTE: HybridArray can't be used here because we need contiguous storage
    # to point to in our yields.
    if op.seq.size > 4
      bufferptr = Pointer(Tzip).malloc(op.seq.size)
    else
      buffer = uninitialized Tzip[4]
      bufferptr = buffer.to_unsafe
    end

    size = 0

    matchee.walk(alg) do |leaf|
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

  private def search(ctx, op : O::Scan | O::Dfs | O::Bfs, matchee : Tzip, plan, & : Fb -> Bool) : Nil
    search(op, matchee) do |itemseq|
      action = Action.lzip(op.seq, itemseq)
      ahead = ctx.interject(plan, action)
      yield eval(fb(ctx, ahead))
    end
  end

  private def search_with_sealed_log!(ctx, op : O::Scan | O::Dfs | O::Bfs, matchee : Tzip, plan, & : Fb, Log::Sealed | Log::None -> Bool) : Nil
    search(op, matchee) do |itemseq|
      action = Action.lzip(op.seq, itemseq)
      ahead = ctx.interject(plan, action)
      yield eval(fb(ctx, ahead)), Log.seal(itemseq, &.log)
    end
  end

  private def search(ctx, op : O::Entries, matchee : Tzip, plan, & : Fb -> Bool) : Nil
    matchee.each_entry_ord do |key, value|
      ahead = ctx.interject(plan, Action.match(op.vop, value))
      _ = yield eval(match(ctx, op.kop, key, ahead))
    end
  end

  private def search_with_sealed_log!(ctx, op : O::Entries, matchee : Tzip, plan, & : Fb, Log::Sealed | Log::None -> Bool) : Nil
    matchee.each_entry_ord do |key, value|
      ahead = ctx.interject(plan, Action.match(op.vop, value))
      _ = yield eval(match(ctx, op.kop, key, ahead)), Log.seal({key, value}, &.log)
    end
  end

  private def search(ctx, op : O::Split, matchee : Tzip, plan, & : Fb -> Bool) : Nil
    matchee.items.each_split(op.focus.size) do |l, focus, r|
      ahead = ctx.interject(plan,
        Action.lzip(op.focus, focus),
        Action.match(op.lhs, l.collect),
        Action.match(op.rhs, r.collect),
      )
      _ = yield eval(fb(ctx, ahead))
    end
  end

  private def search_with_sealed_log!(ctx, op : O::Split, matchee : Tzip, plan, & : Fb, Log::Sealed | Log::None -> Bool) : Nil
    matchee.items.each_split(op.focus.size) do |l, focus, r|
      ahead = ctx.interject(plan,
        Action.lzip(op.focus, focus),
        Action.match(op.lhs, l.collect),
        Action.match(op.rhs, r.collect),
      )

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
      _ = yield eval(fb(ctx, ahead)), Log.seal(Log.simplify(focus.log))
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
  #
  # ⟨x_⟩  ⟨x_ y_ z_⟩
  def match(ctx, op : O::First, matchee : Tzip, plan)
    return Fb[] unless eligible?(op, matchee)

    # Fast log with less indirection specifically for ⟨_⟩, because it
    # is used quite a lot in very hot places.
    if op.is_a?(O::ScanFirst) && op.seq.size == 1
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
  #
  # ⟨x_⟩°  ⟨x_ y_ z_⟩°
  def match(ctx, op : O::Source, matchee : Tzip, plan)
    return Fb[] unless eligible?(op, matchee)

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
  def match(ctx, op : O::All, matchee : Tzip, plan)
    return Fb[] unless eligible?(op, matchee)

    successor = Envlist.new(op.successor, op.minM, op.maxM)

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
  class EnvlistPush(N)
    def initialize(
      @envs : Pf::Kit::HybridArray(Tzip, N),
      @logs : Pf::Kit::HybridArray(Log::SealedOne, N),
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

      Log.flatten(fblog) do |one|
        @logs << Log.seal(one)
      end

      true # continue
    end
  end

  # :nodoc:
  defrecord Envlist, op : O::Any, min : Magnitude, max : Magnitude

  # :nodoc:
  def match(ctx, mod : Envlist, plan, &)
    envs = Pf::Kit.stack_array(Tzip, 8)
    logs = Pf::Kit.stack_array(Log::SealedOne, 8)

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
    envlist = Tzip.mapping(envs, handle: Log.seal(logs, &.itself)) do |env, index|
      {Term.of(index), env}
    end

    cons(ctx, mod.op, envlist, plan)
  end

  # :nodoc:
  #
  # (%value k v_)
  def match(ctx, op : O::Value, matchee : Tzip, plan)
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
  def match(ctx, op : O::NegativeValue, matchee : Tzip, plan)
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
    subctx = ctx.join(op.capture, Cst::NegLookup.new(table))
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
    cst.table.includes?(matchee.term) ? Fb[] : cons(ctx, plan)
  end

  # :nodoc:
  #
  # (%-value k kp) -- key at capture k must be absent in matchee dict, imaginary
  # keypath stored at *kp*.
  def match(ctx, op : O::NegativeValueKeypath, matchee : Tzip, plan)
    return Fb[] unless table = matchee.term.as_d?

    log = Log.seal(Log.simplify(matchee.log))

    # (k_ (%-value k kp))
    if key = ctx.capture?(op.capture)
      if table.includes?(key.term)
        return Fb[] # Key found, value exists, %-value mismatch!
      end

      if log.is_a?(Log::None)
        return cons(ctx, plan)
      end

      # Key not found, value does not exist, %-value match.
      ref = Ref::Entry.new(log, key.term)
      return cons(ctx.join(op.name, ref), plan)
    end

    # ((%-value k kp) k_)

    if log.is_a?(Log::None)
      subctx = ctx.join(op.capture, Cst::NegLookup.new(table))
    else
      subctx = ctx.join(op.capture, Cst::NegLookupRef.new(table, op.name, log))
    end

    fb = eval(fb(subctx, plan)).select(&.has_capture?(op.capture))
    if fb.present?
      return fb # Some responses proposed a satisfactory key, nice!
    end

    Fb[]
  end

  # :nodoc:
  def match(ctx, capture : Term, cst : Cst::NegLookupRef, matchee : Tzip, plan)
    return Fb[] if cst.table.includes?(matchee.term)

    ref = Ref::Entry.new(cst.log, matchee.term)
    cons(ctx.join(cst.name, ref), plan)
  end

  # :nodoc:
  #
  # (x_ _ _) -- when using O2 optimization level
  def match(ctx, op : O::ValueLiteral, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?
    return Fb[] unless value = matchee[op.key]?

    cons(ctx, op.successor, value, plan)
  end

  # :nodoc:
  #
  # (%pipe span 3) -- match char count of string
  def match(ctx, op : O::Span, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_s?

    cons(ctx, op.successor, Tzip.new(Term.of(a.charcount), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe tally 3) -- match entry count of dict
  def match(ctx, op : O::Tally, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_d?

    cons(ctx, op.successor, Tzip.new(Term.of(a.size), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe type _number)
  def match(ctx, op : O::Type, matchee : Tzip, plan)
    cons(ctx, op.successor, Tzip.new(Term.of(matchee.term.type.blank), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe ml (+ a_ b_))
  def match(ctx, op : O::ParseML, matchee : Tzip, plan)
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
  # (%pipe (+ 100) x_)
  def match(ctx, op : O::Add, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    cons(ctx, op.successor, Tzip.new(Term.of(a + op.arg), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (- 100) x_)
  def match(ctx, op : O::Sub, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    cons(ctx, op.successor, Tzip.new(Term.of(a - op.arg), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (* 2) dbl_)
  def match(ctx, op : O::Mul, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    cons(ctx, op.successor, Tzip.new(Term.of(a * op.arg), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (/ 2) x_)
  def match(ctx, op : O::Div, matchee : Tzip, plan)
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
  def match(ctx, op : O::Idiv, matchee : Tzip, plan)
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
  def match(ctx, op : O::Mod, matchee : Tzip, plan)
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
  def match(ctx, op : O::Pow, matchee : Tzip, plan)
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
  def match(ctx, op : O::Clamp, matchee : Tzip, plan)
    return Fb[] unless a = matchee.term.as_n?

    a = Math.min(Math.max(a, op.min), op.max)

    cons(ctx, op.successor, Tzip.new(Term.of(a), Log.none), plan)
  end

  # :nodoc:
  #
  # (%pipe (map {a: 100, b: 200}) x_)
  def match(ctx, op : O::Map, matchee : Tzip, plan)
    return Fb[] unless v = op.arg[matchee.term]?

    cons(ctx, op.successor, Tzip.new(v, Log.none), plan)
  end

  # :nodoc:
  #
  # (%keypath kp)
  def match(ctx, op : O::KeypathCapture, matchee : Tzip, plan)
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

    capture(ctx, op.capture, proposal, O::INSTANCE_PASS, matchee, plan)
  end

  # :nodoc:
  #
  # (_* x_ _* y_ _*)  (xs_* ys_*)  (+ (%group (a_ _* b_) _*) _)
  def match(ctx, op : O::ItemSeq, matchee : Tzip, plan)
    return Fb[] unless _ = matchee.term.as_itemsonly_d?

    match(ctx, Feed.new(op.items), matchee.items, plan)
  end

  # :nodoc:
  #
  # {a: x_, b: y_} -> (%layer () {a: x_, b: y_})
  # {¦ a_ b_} -> (%layer _ {a: a_, b: b_})
  # (_ ¦ xs_ a b) -> (%layer xs_ {a: _, b: _})
  # etc...
  def match(ctx, op : O::Layer, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    ahead = ctx.interject(plan, Action.entrybcast(op.side, matchee))

    case op.below
    when O::Pass, O::Pairsonly
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
  #
  # E.g., the pattern
  #
  # ```wwml
  # ((%filter (k) {¦ k_ v_} (%flat (_ v) vs_)) k_)
  # ```
  #
  # ... with the following matchee:
  #
  # ```wwml
  # (({k: a, v: 100} {k: b, v: 200} {k: c, v: 300} {k: a, v: "Hello"}) a)
  # ```
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
  def match(ctx, op : O::Filter, matchee : Tzip, plan)
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
  def match(ctx, op : O::Pluck, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    cons(ctx, op.successor, matchee.pluck(op.spec), plan)
  end

  # :nodoc:
  #
  # ((%flat (_ k) (%items (k0_ _* kN_) k_)) _)
  def match(ctx, op : O::Flat, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    cons(ctx, op.successor, matchee.flat(op.spec), plan)
  end

  # (%split _ a_ ⟨b_⟩)  (%split _ a_ (%split _ b_ _))  (%split (_ a_ (%split b_ ⟨c_⟩))) . . .
  def match(ctx, op : O::Adjacent, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?
    return Fb[] unless matchee.term.itemsize >= op.members.size

    ahead = ctx.interject(plan, Action.fzip(op.members, matchee.items))
    cons(ctx, ahead)
  end

  #
  # Sequence operators
  #

  # :nodoc:
  record Feed, ord : UInt32, ops : Slice(Feed::Any) do
    alias Any = O::Item::Any | AssertAfter | GroupStop | ManyStop

    record AssertAfter, index : UInt32
    record GroupStop, successor : O::Any, begin : UInt32
    record ManyStop, ctx : Context, many : O::Item::Many, plan : Plan, stops : Slice(UInt32), envs : Slice(Tzip)

    def self.new(ops : Slice(O::Item::Any))
      new(0u32, ops.to_readonly_slice(&.as(Any)))
    end

    def first? : Any?
      ops.first?
    end

    def +(offset : Int)
      Feed.new(ord + 1, ops + 1)
    end

    def prepend(prefix : Slice(O::Item::Any))
      Feed.new(ord, ops.prepend_many(prefix, &.as(Any)))
    end

    def prepend(op : Any)
      Feed.new(ord, ops.prepend(op))
    end

    # TODO: do it in a single allocation!!
    def prepend(*args : Any | Slice(O::Item::Any))
      current = self
      args.reverse_each { |arg| current = current.prepend(arg) }
      current
    end
  end

  # :nodoc:
  #
  # (⏏+⏏ ⏏a_⏏ ⏏b_⏏) when running with optimization levels O0-O1. More generally
  # e.g. (⏏+⏏ ⏏x_⏏ xs_* ⏏y_⏏ ys_*)  (⏏+⏏ ⏏x_⏏ `y)
  def match(ctx, op : O::Item::Singular, ops : Feed, items : Tzip::ItemsView, plan)
    return Fb[] unless matchee = items.first?

    ahead = ctx.interject(plan, Action.match(ops, items + 1))
    cons(ctx, op.tail, matchee, ahead)
  end

  # :nodoc:
  #
  # (+ a_ b_ ⏏`c⏏)
  def match(ctx, op : O::Item::Slot, ops : Feed, items : Tzip::ItemsView, plan)
    unless ref = items.before_begin.span?(ord: ops.ord - 1)
      # This happens if logging is disabled, or if we have no info about where
      # we are (for whatever reason :/).
      return cons(ctx, ops, items, plan)
    end

    cons(ctx.join(op.name, ref), ops, items, plan)
  end

  # :nodoc:
  #
  # ((%past a_ b_) and _*)  ((%past _number min: 3 max: 5))
  #
  # NOTE: One important edge case to keep in mind: (%past/max `slot) (i.e., slot
  # matches zero elements, and %past is a looping construct -- hazardous!)
  def match(ctx, op : O::Item::PastLazy, ops : Feed, items : Tzip::ItemsView, plan)
    return Fb[] if op.n > op.maxM

    # If this %past's min constraint is satisfied, then it can start removing
    # itself from the pattern.
    if op.n >= op.minM
      fb = eval(match(ctx, ops, items, plan))
      return fb if fb.present?
    end

    # If this %past's min constraint is not satisfied, or if the pattern ahead
    # does not match, %past tries to help by prepending its children followed
    # by itself-advanced.
    successors = ops.prepend(
      op.children,
      Feed::AssertAfter.new(items.begin),
      op.copy_with(n: op.n + 1),
    )
    cons(ctx, successors, items, plan)
  end

  # :nodoc:
  #
  # ((%past/max _ _ min: 2 max: 5))  (+ (%past/max _number min: 2))
  #
  # NOTE: One important edge case to keep in mind: (%past/max `slot) (i.e., slot
  # matches zero elements, and %past is a looping construct -- hazardous!)
  def match(ctx, op : O::Item::PastGreedy, ops : Feed, items : Tzip::ItemsView, plan)
    return Fb[] if op.n > op.maxM

    # Greedy %past unconditionally prepends its children and then itself-advanced.
    successors = ops.prepend(
      op.children,
      Feed::AssertAfter.new(items.begin),
      op.copy_with(n: op.n + 1),
    )
    fb = eval(match(ctx, successors, items, plan))
    return fb if fb.present?

    # If this %past's n is below min, it cannot help the matching process by removing
    # itself from the pattern -- this will cause the min constraint to be violated,
    # and %past doesn't want that.
    return Fb[] if op.n < op.minM

    # This %past's n is above min -- its min constraint is satisfied. This means it
    # can try to remove itself from the pattern to see if that leads to
    # a successful match.
    cons(ctx, ops, items, plan)
  end

  # :nodoc:
  def match(ctx, op : Feed::AssertAfter, ops : Feed, items : Tzip::ItemsView, plan)
    items.begin > op.index ? cons(ctx, ops, items, plan) : Fb[]
  end

  # :nodoc:
  #
  # (%group xs_ x_ y_ _*)
  def match(ctx, op : O::Item::Group, ops : Feed, items : Tzip::ItemsView, plan)
    successors = ops.prepend(op.children, Feed::GroupStop.new(op.successor, items.begin))
    cons(ctx, successors, items, plan)
  end

  # :nodoc:
  def match(ctx, op : Feed::GroupStop, ops : Feed, items : Tzip::ItemsView, plan)
    content = items.reshape(op.begin, items.begin)

    # Continue on to ops after the group's successor runs!
    ahead = ctx.interject(plan, Action.match(ops, items))
    cons(ctx, op.successor, content.collect, ahead)
  end

  # :nodoc:
  #
  # (%optional 0 x_)  (%optional (0 0) (x_ y_))
  def match(ctx, op : O::Item::Optional, ops : Feed, items : Tzip::ItemsView, plan)
    fb = eval(match(ctx, ops.prepend(O::Item::Singular.new(op.body)), items, plan))
    return fb if fb.present?

    default = items.tzip.insert(op.default, before: items.begin, ord: ops.ord - 1)

    ahead = ctx.interject(plan,
      Action.match(op.body, default),
      Action.match(ops, items)
    )
    cons(ctx, ahead)
  end

  # :nodoc:
  #
  # ⏏xs_*⏏ y_  xs_* xs_*   xs_* ys_*  xs_+ ys_number zs_*  (%plural/max xs)
  # (%plural/min xs) (plural/max ys type: _number)
  def match(ctx, op : O::Item::Plural, ops : Feed, items : Tzip::ItemsView, plan)
    pivot = (items.size / op.frac).ceil.to_i

    strategy = op.strategy
    if op.strategy.auto?
      # xs_* y_
      strategy = O::Item::ExpandStrategy::Sway

      unless op.follower.none?
        # xs_* ys_number*
        # xs_* (%group G_ ys_number*)
        # xs_boolean_* ys_number*
        if !op.type.any? && op.follower.any?
          # xs_number* ys_*
          #
          # When we are a typed polyblank and our follower is untyped, we become Greedy,
          # because we're more specific about our type.
          strategy = O::Item::ExpandStrategy::Greedy
        elsif op.type.any? && !op.follower.any? && op.min <= pivot <= op.max
          # xs_* ys_number*
          #
          # When we are an untyped polyblank and our follower is typed, we start
          # expelling items from right (*giving* them to *ys*) while they match
          # ys' type and our own min-max constraints are satisfied.
          run = items.before(pivot)
          while run.ends_with?(&.type.subtype?(op.follower.type)) && op.min <= run.size - 1 <= op.max
            run = run.prior
          end

          fb = eval(match(ctx, op, ops, run, run.ahead, plan))
          return fb if fb.present?

          # Otherwise become lazy because we're less specific than our successor.
          strategy = O::Item::ExpandStrategy::Lazy
        end
      end
    end

    match(ctx, op, pivot, strategy, ops, items, plan)
  end

  # :nodoc:
  def match(ctx, op : O::Item::Gap, ops : Feed, items : Tzip::ItemsView, plan)
    pivot = (items.size / op.frac).ceil.to_i

    strategy = op.strategy
    if op.strategy.auto?
      # (%gap n_) x_
      strategy = O::Item::ExpandStrategy::Sway
    end

    match(ctx, op, pivot, strategy, ops, items, plan)
  end

  # :nodoc:
  def match(ctx, op : O::Item::Plural | O::Item::GapFirst, pivot : Int32, strategy : O::Item::ExpandStrategy, ops : Feed, items : Tzip::ItemsView, plan)
    case strategy
    in .auto?
      raise ArgumentError.new
    in .sway?
      items.each_bisplit_sway(pivot) do |before, after|
        fb = eval(match(ctx, op, ops, before, after, plan))
        return fb if fb.present?
      end
    in .lazy?
      items.each_bisplit_lazy do |before, after|
        fb = eval(match(ctx, op, ops, before, after, plan))
        return fb if fb.present?
      end
    in .greedy?
      items.each_bisplit_greedy do |before, after|
        fb = eval(match(ctx, op, ops, before, after, plan))
        return fb if fb.present?
      end
    end

    Fb[]
  end

  # :nodoc:
  def match(ctx, op : O::Item::GapSource, pivot : Int32, strategy : O::Item::ExpandStrategy, ops : Feed, items : Tzip::ItemsView, plan)
    sink = Pf::Kit.stack_array(Context, 8)

    case strategy
    in .auto?
      raise ArgumentError.new
    in .sway?
      items.each_bisplit_sway(pivot) do |before, after|
        fb = eval(match(ctx, op, ops, before, after, plan))
        fb.each { |response| sink << response }
      end
    in .lazy?
      items.each_bisplit_lazy do |before, after|
        fb = eval(match(ctx, op, ops, before, after, plan))
        fb.each { |response| sink << response }
      end
    in .greedy?
      items.each_bisplit_greedy do |before, after|
        fb = eval(match(ctx, op, ops, before, after, plan))
        fb.each { |response| sink << response }
      end
    end

    Fb[sink]
  end

  # :nodoc:
  #
  # NOTE: All contending plurals have the same min-max (here referred to
  # as min1, max1). op.min and op.max are that min/max1 multiplied by
  # the no. of contenders.
  def match(ctx, op : O::Item::Plural, ops : Feed, run : Tzip::ItemsView, rest : Tzip::ItemsView, plan)
    # min, max is min1, max1 of contenders * no. of contenders
    return Fb[] unless op.min <= run.size <= op.max
    return Fb[] unless op.type.any? || run.all?(&.type.subtype?(op.type))

    ahead = ctx.interject(plan, Action.match(ops, rest))

    run.each_chunk_of(op.contenders.size, empty: op.min.zero?) do |chunk, index|
      return Fb[] unless op.min1 <= chunk.size <= op.max1

      # capture : Term if named, e.g. (%plural xs) aka xs_*
      # capture : Nil if unnamed, e.g. (%plural) aka _*
      next unless capture = op.contenders[index]

      ahead = ctx.interject(ahead, Action.capture(capture, chunk.collect))
    end

    cons(ctx, ahead)
  end

  # :nodoc:
  def match(ctx, op : O::Item::Gap, ops : Feed, run : Tzip::ItemsView, rest : Tzip::ItemsView, plan)
    # NOTE: It makes no sense to refer to the gap's n, hence Log.none:
    #
    #   ((%gap n_)) <> {n: 10}
    #
    # This backmap has no meaning.
    n = Tzip.new(Term.of(run.size), Log.none)

    ahead = ctx.interject(plan, Action.match(ops, rest))
    eval(match(ctx, op.measurer, n, ahead))
  end

  # :nodoc:
  #
  # (_* x_ ⏏(%many {¦ x_ y_} _* x_ y_ _*)⏏ y_ _*)
  def match(ctx, op : O::Item::Many, ops : Feed, items : Tzip::ItemsView, plan)
    match(ctx.sibling, Feed::ManyStop.new(ctx, op, plan, stops: Slice(UInt32).empty, envs: Slice(Tzip).empty), ops, items, plan: nil)
  end

  # :nodoc:
  #
  # WARNING: this is called with ISOLATED ctx and plan. The original ctx and plan
  # are stored in *op*.
  def match(ctx, op : Feed::ManyStop, ops : Feed, items : Tzip::ItemsView, plan)
    return Fb[] if op.stops.size > op.many.maxM

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

    if op.stops.size >= op.many.minM
      # Construct the toplevel envs list lst←(env₀ env₁ ...) and its corresponding
      # mapping. lst should refer to all items matched.
      #
      # Each env's log is a Mapping (see above): its handle refers to the range
      # matched by env₀, env₁ and so on; we concatenate them to get a single handle.
      handle = Log.seal(op.envs, &.log)
      envlist = Tzip.mapping(op.envs, handle) { |env, index| {Term.of(index), env} }

      # Run %many's successor and all the following items with the original context
      # and plan. ctx and plan, on the other hand, refer to the isolated ones.
      ahead = ctx.interject(op.plan,
        Action.match(op.many.successor, envlist),
        Action.match(ops, items),
      )

      fb = eval(fb(op.ctx, ahead))
      return fb if fb.present?
    end

    successors = ops.prepend(
      op.many.children,
      Feed::AssertAfter.new(items.begin),
      op.copy_with(stops: op.stops.append(items.begin))
    )

    assert plan.nil?

    # .sibling gives us a fresh empty context to use for the next run of children.
    # We know that the plan is empty at this point so we can reuse it.
    cons(ctx.sibling, successors, items, plan)
  end

  # :nodoc:
  def match(ctx, ops : Feed, items : Tzip::ItemsView, plan)
    op = ops.first?
    if op.nil? && items.empty? # Matched all items.
      return cons(ctx, plan)
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
  def match(ctx, op : O::Entry::Required, matchee : Tzip, plan)
    return Fb[] unless value = matchee[op.key]?

    cons(ctx, op.value, value, plan)
  end

  # :nodoc:
  #
  # {¦ ⏏a⏏ ⏏b⏏} {⏏a: _⏏, ⏏b: _number⏏}
  def match(ctx, op : O::Entry::Present, matchee : Tzip, plan)
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
  #
  # {¦ ⏏-x⏏}  {a: (%- _)}
  def match(ctx, op : O::Entry::Absent, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] if dict.includes?(op.key)

    cons(ctx, plan)
  end

  # :nodoc:
  #
  # {¦ ⏏-x_⏏}  {a: (%- _ a)}
  def match(ctx, op : O::Entry::AbsentKeypath, matchee : Tzip, plan)
    return Fb[] unless dict = matchee.term.as_d?
    return Fb[] if dict.includes?(op.key)

    log = Log.seal(Log.simplify(matchee.log))

    if log.is_a?(Log::None)
      return cons(ctx, plan)
    end

    ref = Ref::Entry.new(log, op.key)
    cons(ctx.join(op.name, ref), plan)
  end

  # :nodoc:
  #
  # {¦ ⏏x⋮ 100⏏}  x: (%optional (+ 1 2) (+ a_ b_))
  def match(ctx, op : O::Entry::Optional, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    if value = matchee[op.key]?
      return cons(ctx, op.value, value, plan)
    end

    cons(ctx, op.value, matchee.with(op.key, op.default), plan)
  end

  # :nodoc:
  #
  # {¦ ⏏x: (%- _number)⏏} -- means x must be absent OR must NOT be a number.
  def match(ctx, op : O::Entry::Negative, matchee : Tzip, plan)
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
  def match(ctx, op : O::Entry::NegativeKeypath, matchee : Tzip, plan)
    return Fb[] unless matchee.dict?

    if value = matchee[op.key]?
      fb = eval(match(ctx.sibling, op.barrier, value, plan: nil))
      return Fb[] if fb.present?
    end

    log = Log.seal(Log.simplify(matchee.log))

    if log.is_a?(Log::None)
      return cons(ctx, plan)
    end

    ref = Ref::Entry.new(log, op.key)
    cons(ctx.join(op.name, ref), plan)
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

  # :nodoc:
  def cons(ctx : Context, ops : Feed, items : Tzip::ItemsView, plan : Plan)
    ahead = ctx.interject(plan, Action.match(ops, items))
    cons(ctx, ahead)
  end

  # :nodoc:
  def cons(ctx : Context, op : O::Any | O::Entry::Any, matchee : Tzip, plan : Plan)
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
  def match(env : Term::Dict, op : O::Any, matchee : Term, *, log : Bool = false, & : Fb -> T) : T forall T
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
          # an unreachable spot. So we trigger a mismatch immediately, while also preventing
          # an infinite loop.
          fb = Fb[]
        end

        return yield fb
      end
    end

    Intrinsics.unreachable
  end
end
