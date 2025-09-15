# Wirewright Delta7 (D7) is a symbolic physics engine that is at its core my
# humble attempt to model *autopoiesis* as described by Maturana, Varela, and others.
#
# There's section 9, "Key", from *Autopoiesis: the organization of living systems,
# its characterization and a model* by Varela, Maturana & Uribe (1974). D7 appears
# to pass points 1 and 2. Point 3 we seem to pass -- D7 programs are mechanistic
# systems, with the note that they require custom physics to run (D7 itself is
# such physics). Points 4-6 I fail to parse, but let's say we appear to pass
# them as well :^).
#
# From the user's point of view, the main interface to D7 is its `repair` and
# `damage` functions. For interactive editing, there are also `Cursor.repair`
# and `Cursor.post`. For traversal over D7 programs, there's `walk`.
#
# *Nodes* in D7 programs specify how other nodes must be fixed. In other words,
# nodes in a D7 program fix each other through a network of interactions instantiated
# by D7 hyperdges, known simply as *edges*. As long as enough connections and
# information is available, D7 will be able to "heal" the program to a computed state.
#
# The core idea with D7 is that it is a network of bindings between places in
# the program itself. You can "fence off" and name parts of the program with `cell`
# or `frag`, creating a kind of "membrane" around them. This lets other nodes such
# as `map` assign or observe their content.
#
# More importantly, nodes like `map` can serve as "bridges" between parts of the same
# program. A `map` can translate the content of one cell in the program in such a way
# that another cell picks it up. This means that with nodes like `map`, `wire`, and
# others, you can establish bindings between parts of the same program.
#
# You can establish bidirectional bindings between two cells with two reciprocal `fill`
# nodes to achieve "healing" of sorts (in simple cases; in more complex cases, there
# may be multiple pathways to recompute one from the other and vice versa, perhaps of
# varying length). `fill` nodes are friends of `map` nodes which do not introduce oscillation
# on conflict. This way, as long as at least one cell contains information,
# the other cell will be recomputed or "completed" based on that information.
#
# Cells are assumed not to conflict. How conflict will be resolved in practice
# depends on how it is caused in the first place. Internally, D7 forbids conflict
# by preventing overwrite of successful cell proposals.
#
# I'm bored, so this is where this description is going to end. Read the code if
# you're interested.
#
# On the API end, the main functions are all public. You create a spec, and then you
# call in your mainloop, or in a loop somewhere else, the function `repair`, followed
# by rounds of damage, including D7's own `damage`.
#
# ```
# # specdoc : Term, most likely you want `runtime/spec.d7.wwml`
# spec = D7.spec(specdoc)
#
# program0 : Term
#
# loop do
#   program1 = D7.repair(spec, program0)
#
#   # Show program1 to the user.
#   puts ML.display(program1)
#
#   # Apply rounds of damage to program1:
#   program1 = D7.damage(spec, program1)
#
#   # Stop on fixpoint.
#   break if program0 == program1
#
#   program0 = program1
# end
# ```
module Ww::D7
  extend self

  Log = ::Log.for(self)

  # Raised on errors in spec. Currently we're supposed to crash on those
  # since the spec is assumed to be non-user-editable.
  class SpecError < Exception
  end

  # Represents a replacement.
  module Rep
    alias Any = None | Some
    alias Some = Zero | One | Many

    # Do not replace.
    struct None
    end

    # Replace with nothing.
    struct Zero
    end

    # Replace with *term*.
    struct One
      getter term : Term

      # :nodoc:
      def initialize(@term : Term)
      end
    end

    # Replace with many *terms*.
    struct Many
      getter terms : ThinArray(Term)

      # :nodoc:
      def initialize(@terms : ThinArray(Term))
      end
    end

    # Shorthand for constructing `None`.
    def self.none : None
      None.new
    end

    # Shorthand for constructing `One`.
    def self.one(object)
      One.new(Term.of(object))
    end

    # Shorthand for constructing one of `Some` based on an indexable of *terms*.
    #
    # WARNING: *terms* will be reused if it is a `ThinArray` of two or
    # more elements.
    def self.some(terms : Indexable(Term)) : Some
      case terms.size
      when 0 then Zero.new
      when 1 then One.new(terms.unsafe_fetch(0))
      else
        if terms.is_a?(ThinArray(Term))
          return Many.new(terms)
        end

        ary = ThinArray(Term).new(capacity: terms.size)
        ary.concat(terms)

        Many.new(ary)
      end
    end
  end

  # Hosts functions to transform and iterate over node edges based on edge
  # declarations extracted from the spec.
  module EdgeDetection
    extend self

    private alias Keypath = ThinArray(Term)
    private alias KeypathBucket = ThinArray(Keypath)
    private alias KeypathBucketArray = ThinArray(KeypathBucket)

    # :nodoc:
    record Index, patterns : PatternSet, buckets : KeypathBucketArray

    # :nodoc:
    EDGES_SELECTOR = ML.term(%{(edges pattern_ captures←(_*) example_)})

    # Constructs an index object from a D7 *spec*.
    #
    # Raises `SpecError` on error in the spec.
    def index(spec : Term) : Index
      buckets = KeypathBucketArray.new

      patterns = PatternSet.select(EDGES_SELECTOR, spec) do |_, env|
        pattern, captures, example = env[:pattern], env[:captures], env[:example]
        bucket = KeypathBucket.new

        envs = M1.matches(pattern, example, backpaths: true)
        unless envs.size == 1
          raise SpecError.new("spec: example #{example} does not match #{pattern} or is a source pattern (source patterns are not allowed)")
        end

        env = envs[0]

        captures.items.each do |capture|
          backpaths = env[:"(backpaths)", capture]
          unless backpaths.size == 1
            raise SpecError.new("spec: capture #{capture} in #{pattern} must have exactly one backpath")
          end

          # Convert backpath to keypath.
          backpath, _ = backpaths.nth(0)
          keypath = Keypath.new
          backpath.items.each do |step|
            Term.case(step) do
              matchpi %{value} { }
              matchpi %{(pair key_)} { keypath << key }
            end
          end

          bucket << keypath
        end

        buckets << bucket

        true # add to set
      end

      Index.new(patterns, buckets)
    end

    private def bucket?(index : Index, node : Term) : KeypathBucket?
      return unless node.type.dict?

      case pr = index.patterns.response(node)
      in Pr::Neg
      in Pr::One  then index.buckets[pr.pattern.index]
      in Pr::Many then unreachable
      end
    end

    # Transforms edges in *node* using the block.
    def map(index : Index, node : Term, & : Term -> Term) : Term
      return node unless bucket = bucket?(index, node)

      bucket.reduce(node) do |memo, keypath|
        Term.morph(memo, keypath) { |tip| yield tip }
      end
    end

    # Yields edges in *node* to the block.
    def each(index : Index, node : Term, & : Term ->) : Nil
      return node unless bucket = bucket?(index, node)

      bucket.each do |keypath|
        yield node.follow(keypath)
      end
    end
  end

  # Represents the context of a node.
  #
  # - *scope* is the node's scope.
  # - *l* is the node located at the passable spot to the left, if any.
  # - *r* is the node located at the passable spot to the right, if any.
  record NodeContext, scope : Scope::Path, l : Term?, r : Term?

  # Permeation aka *passability* refers to the act of finding and replacing
  # passable or *permeable* child nodes of a parent node.
  #
  # A node can have *impassable* children which the D7 engine does not descend
  # into; and *passable* ones, which are found and handled by functions found
  # in this module.
  #
  # Permeability is declared for each interested node in the spec.
  module Permeation
    extend self

    # Represents a passable range, which is a range of node items D7 is allowed
    # to descend into.
    record PassableRange,
      span : Range(Int32, Nil) | Range(Int32, Int32),
      quoted : Bool

    alias PassableRangeArray = ThinArray(PassableRange)

    # :nodoc:
    record Decl,
      ranges : PassableRangeArray,
      bindings_capture : Term::Sym?

    # :nodoc:
    record Index, patterns : PatternSet, decls : ThinArray(Decl)

    # :nodoc:
    PERMEABLE_SELECTOR = ML.term(%{(permeable pattern_ parts←(_*) ¦ opts_)})

    # Constructs an index object from a D7 *spec*.
    #
    # Raises `SpecError` on error in the spec.
    def index(spec : Term) : Index
      decls = ThinArray(Decl).new

      patterns = PatternSet.select(PERMEABLE_SELECTOR, spec) do |_, env|
        if (bindings_capture = env[:opts, :bindings]?) && (bindings_capture = bindings_capture.as_sym?).nil?
          raise SpecError.new("expected a symbol for bindings capture")
        end

        ranges = PassableRangeArray.new

        env[:parts].items.each do |part|
          Term.case(part) do
            matchpi %{(range (b←(%number i32) ..) ¦ () quoted⋮ false)} do
              ranges << PassableRange.new(b.to(Int32)..., quoted: quoted.to(Bool))
            end

            matchpi %{(range (b←(%number i32) ..< e←(%number i32)) ¦ () quoted⋮ false)} do
              ranges << PassableRange.new(b.to(Int32)...e.to(Int32), quoted: quoted.to(Bool))
            end

            otherwise do
              raise SpecError.new("spec: invalid permeable part spec #{part}")
            end
          end
        end

        decls << Decl.new(ranges, bindings_capture)

        true # add to set
      end

      Index.new(patterns, decls)
    end

    private def decl_and_env?(index : Index, node : Term) : {Decl, Term::Dict}?
      return unless node.type.dict?

      case pr = index.patterns.response(node)
      in Pr::Neg
      in Pr::One  then {index.decls[pr.pattern.index], pr.env}
      in Pr::Many then unreachable
      end
    end

    # Returns an array of passable ranges in *node*.
    def ranges(index : Index, node : Term) : PassableRangeArray
      unless decl_and_env = decl_and_env?(index, node)
        return PassableRangeArray.new
      end

      decl, _ = decl_and_env
      decl.ranges
    end

    # Returns an array of passable ranges in *node*, followed by *scope* extended
    # with bindings according to *node* (if necessary).
    def ranges(index : Index, scope : Scope::Path, node : Term) : {PassableRangeArray, Scope::Path}
      unless decl_and_env = decl_and_env?(index, node)
        return PassableRangeArray.new, scope
      end

      decl, env = decl_and_env

      unless bindings_capture = decl.bindings_capture
        return decl.ranges, scope
      end

      unless bindings = env[bindings_capture]?
        Log.error { "invalid spec: bindings capture `#{bindings_capture}` not found" }
        return PassableRangeArray.new, scope
      end

      unless bindings = bindings.as_d?
        Log.error { "invalid spec: cannot use non-dict capture `#{bindings_capture}` for bindings" }
        return PassableRangeArray.new, scope
      end

      {decl.ranges, Scope.append(scope, Scope::Module.new(bindings))}
    end
  end

  # Place(ment) rating machinery.
  module Rating
    extend self

    # Lists the available placement ratings, ordered worst (least preferred) to
    # best (most preferred).
    enum Option
      ImpassableDependent
      PassableDependent
      Impassable
      PassableIndependent
    end

    # :nodoc:
    record Index, patterns : PatternSet, ratings : ThinArray(Option)

    # :nodoc:
    RATING_SELECTOR = ML.term(%{(rating pattern_ rating_)})

    # Constructs an index object from a D7 *spec*.
    #
    # Raises `SpecError` on error in the spec.
    def index(spec : Term) : Index
      ratings = ThinArray(Option).new

      patterns = PatternSet.select(RATING_SELECTOR, spec) do |_, env|
        # TODO:
        # if M1.source?(normp)
        #   raise SpecError.new("source patterns disallowed in rating")
        # end

        rating = Term.case(env[:rating]) do
          matchpi %{impassable-dependent} { Option::ImpassableDependent }
          matchpi %{passable-dependent} { Option::PassableDependent }
          matchpi %{impassable} { Option::Impassable }
          matchpi %{passable-independent} { Option::PassableIndependent }

          otherwise do
            raise SpecError.new("invalid rating #{env[:rating]}")
          end
        end

        ratings << rating

        true # confirm
      end

      Index.new(patterns, ratings)
    end

    # Returns the rating of a cursor found inside *node*.
    def rating?(index : Index, node : Term) : Option?
      case pr = index.patterns.response(node)
      in Pr::Neg
      in Pr::One  then index.ratings[pr.pattern.index]
      in Pr::Many then unreachable
      end
    end
  end

  # Hosts functions related to self-editing/cursor support in D7. This is where
  # most of "stigmeric" communication between D7 and inputR is implemented.
  module Cursor
    extend self

    # Replaces input kernels found in *node* and controlled by *edge* using *fn*.
    # *edge* can be omitted to run on all input kernels.
    #
    # Returns a modified copy of *node*.
    #
    # *fn* is called with each input kernel and its hitcount, and is expected to return
    # a replacement term.
    def replace(node : Term, edge : Term? = nil, &fn : Term, Int32, Int32 -> Rep::None | Rep::One) : Term
      case rep = replace0(node, edge, fn, depth: 0)
      in Rep::None then node
      in Rep::One  then rep.term
      end
    end

    private def replace0(node0 : Term, edge : Term?, fn, depth : Int32)
      return Rep.none unless node0.type.dict?
      return Rep.none unless node0.probably_includes?(Input::CUE)

      if Input.kernel?(node0)
        Term.matchpi?(node0, %[{¦ control: @control_ hitcount_: (%optional 0 (%number +i32))}]) do
          if edge.nil? || edge == control
            return fn.call(node0, depth, hitcount.to(Int32))
          end
        end
      end

      # Do not count nesting into input structure as depth.
      unless Input.input?(node0)
        depth += 1
      end

      replaced = false

      node1 = node0.transaction do |commit|
        node0.each_entry do |key, value|
          case rep = replace0(value, edge, fn, depth)
          in Rep::None
          in Rep::Some
            replaced = true
            commit.with(key, rep.term)
          end
        end
      end

      replaced ? Rep.one(node1) : Rep.none
    end

    # Returns `true` if *term* contains a cursor. Returns `false` otherwise.
    #
    # *only_active* must be set to determine whether this function should search
    # only for active cursors (the ones that the user or D7 can interact with),
    # or any cursors (any term that looks like a cursor).
    def in?(term : Term, *, only_active : Bool) : Bool
      Term.each_keypath_and_node(term) do |_, node|
        next false unless dict = node.as_d?
        next false unless dict.probably_includes?(Input::CUE)
        next true unless Input.kernel?(node)
        next false if only_active && dict[:active]? != Term.of(true)
        return true # found
      end

      false # not found
    end

    # Represents a cursor rank.
    #
    # A cursor with the highest hitcount is a hard win. We treat such cursors as ones
    # with most interaction history, and therefore, they are presumably the most expected
    # ones to win from the user's perspective.
    #
    # If hitcount is the same (most likely due to cursor duplication, e.g. the cursor
    # was logged so there are two cursors now), use place rating.
    #
    # If place ratings are the same, we have distance. For distance, the closer the cursor
    # is, the better it is. The physical analogy is clear here to help us resolve which
    # cursor to pick; we want to pick the cursor "closest" to us in space -- in the most
    # abstract sense.
    #
    # If all three are the same, which cursor will be picked is implementation-defined.
    # Only one will be picked, but which one we do not know.
    record Rank, id : Int32, hitcount : Int32, rating : Rating::Option, distance : Int32 do
      include Comparable(Rank)

      def <=>(other : Rank)
        {hitcount, rating, -distance} <=> {other.hitcount, other.rating, -other.distance}
      end
    end

    # Repairs cursors and other nodes administered by `Cursor` in a D7 *program*.
    # *Repair* here means mainly annotation with up-to-date data. Repair is necessary
    # to react to perturbations around the cursor term and to the cursor term itself
    # that happend "while the engine wasn't looking".
    #
    # NOTE: You **must** call `repair` on a D7 program before `post`ing a motion. Otherwise,
    # you risk getting into an out of sync mess. Visually, if you do not `repair` before
    # `post`ing, there is the chance of damaging the program so much it will not recover.
    def repair(spec : Spec, program : Term) : Term
      seq = 0
      ranks = {} of Term => Rank

      program = D7.walk(spec, program, conf: DEFAULT_WALK_CONF | WalkConf::EmitEdited) do |ctx, node0|
        qualdepth = Scope.qualdepth(ctx.scope)
        scope_rating = Scope.rating(ctx.scope)

        node1 = replace(node0) do |cursor, depth, hitcount|
          next Rep.none if cursor[:id]? # seen

          unless control = cursor[:control]?
            # Make sure to clear active! Maybe it lost its control.
            next Rep.one(cursor.without(:active))
          end

          rating = scope_rating
          if scope_rating.passable_independent? && depth > 0
            # No way. Either the recursive walk() marked it properly with `id`
            # so this is not reached, or the cursor is at an impassable spot.
            rating = Rating::Option::Impassable
          end

          id = seq
          seq += 1

          rank1 = Rank.new(id, hitcount, rating, distance: qualdepth + depth)

          if rank0 = ranks[control]?
            if rank1 > rank0
              ranks[control] = rank1
            end
          else
            ranks[control] = rank1
          end

          Rep.one(cursor.with(:id, id).without(:active))
        end

        Rep.some({node1})
      end

      program = D7.walk(spec, program, conf: DEFAULT_WALK_CONF | WalkConf::EmitEdited) do |ctx, node0|
        node1 = replace(node0) do |kernel, _|
          next Rep.none if kernel[:active]? # seen
          next Rep.none unless id = kernel[:id]?

          kernel = Term.of(kernel.without(:id))
          next Rep.one(kernel) unless control = kernel[:control]?
          next Rep.one(kernel) unless best = ranks[control]?
          next Rep.one(kernel) unless id == Term.of(best.id)

          ranks.delete(control)

          Rep.one(kernel.with(:active, true))
        end

        Rep.some({node1})
      end

      program
    end

    # Sends *motion* to cursors in *program* that are controlled at *edge*.
    # This function only considers visible cursors, that is, cursors that
    # appear immediately in *program* and not through expansion. This is done
    # to simplify implementation and also to provide sensible behavior, with
    # no hidden effects.
    #
    # NOTE: You must always call `repair` before calling `post`.
    def post(program : Term, edge : Term, motion : Term) : Term
      replace(program) do |kernel, _|
        next Rep.none unless kernel[:active]? == Term.of(true)
        next Rep.none unless kernel[:control]? == edge

        hitcount = kernel[:hitcount]?.try(&.as_n?) || Term[0]
        hitcount += Term[1]

        Rep.one(kernel.append(motion).with(:hitcount, hitcount))
      end
    end
  end

  # Hosts functions for manipulating and resolving edge scopes in a D7 document.
  #
  # Conceptually, all edges are global, or, more precisely, they are *unscoped*.
  # But by tweaking edge names, namely by making a part of the edge's name hold
  # a scope label, we are able to emulate local scopes that are good enough, or
  # even indistinguishable, from traditional scoping.
  module Scope
    extend self

    alias Path = Slice(Item)
    alias Item = Qualifier | Rating::Option | Module

    # Represents a part of the qualified path to a node in a D7 document.
    record Qualifier, id : Int32

    # Represents an embedding of a bindings table right in the path
    # to a node.
    record Module, bindings : Term::Dict

    # Returns the root scope marker, represented by an empty path.
    def toplevel : Path
      Path.empty
    end

    # Returns the path to the innermost scope for *path*.
    def current(path : Path) : Path
      loop do
        break unless tip = path[-1]?
        break if tip.is_a?(Module)

        path = path[...-1]
      end

      path
    end

    # Extends a scope path with an additional item. Used to build the path
    # to a node.
    def append(path : Path, item : Item) : Path
      path.append(item)
    end

    # Returns the scope path where *edge* is defined, followed by the corresponding
    # edge in that scope. Although we do not have the concept of "bound" or "unbound"
    # edges, in practice, "unbound" edges will be scoped locally (in the enclosing scopes).
    def find(path : Path, edge : Term) : {Path, Term}
      # Pop until we see a bindings table (aka a scope delimiter).
      loop do
        case tip = path[-1]?
        in Nil
          # We're scoped under toplevel, there is nothing to do.
          return path, edge
        in Module
          # We've found a scope. Let's see if edge is in there and thus escapes.
          if exterior = tip.bindings[edge]?
            # It escapes into the outer scope.
            return find(path[...-1], exterior)
          end

          # It doesn't escape, then, path is its scope.
          return path, edge
        in Qualifier, Rating::Option
          path = path[...-1]
        end
      end
    end

    # Resolves *edge* against the given scope *path*. Returns the resulting
    # *qualified edge*.
    def resolve(path : Path, edge : Term) : Term
      qualified(*find(path, edge))
    end

    # Attaches the qualpath to *edge*.
    private def qualified(path : Path, edge : Term) : Term
      Term.case(edge) do
        matchpi %{(%'edge name_)} do
          qualpath = Term::Dict.build do |commit|
            commit.selected(path, Qualifier, &.id)
            commit << name
          end

          Term.of(:edge, qualpath)
        end

        otherwise { edge }
      end
    end

    # Returns the placement rating of *path*.
    #
    # NOTE: we assume this function is called with a known passable path.
    def rating(path : Path) : Rating::Option
      path.reverse_each do |item|
        next unless item.is_a?(Rating::Option)
        return item
      end

      Rating::Option::PassableIndependent
    end

    # Retunrs the qualifier depth of *path*. This is used in D7 as
    # a "distance metric".
    def qualdepth(path : Path) : Int32
      path.count(&.is_a?(Qualifier))
    end
  end

  # :nodoc:
  defcase Spec,
    proposals : Ruleset,
    splits : Ruleset,
    split_cache : Alloy::ExpansionCache,
    mixR : Rewriter,
    fixR : Rewriter,
    damageR : Rewriter,
    edges : D7::EdgeDetection::Index,
    permeation : D7::Permeation::Index,
    ratings : D7::Rating::Index

  # Parses *spec* into an internal D7 spec object.
  #
  # Raises `SpecError` if an error is found in *spec*.
  def spec(spec : Term) : Spec
    ruledbR = backmapR(Ruleset.select(ML.term(%{[backmap pattern_ backspec_]}), spec))

    Spec.new(
      proposals: Ruleset.select(ML.term(%{[rule (propose pattern_*) template_]}), spec),
      splits: Ruleset.select(ML.term(%{[rule (split pattern_) template_]}), spec),
      split_cache: SyncCache(Term, Alloy::Expansion).new(1024, preallocate: true),
      mixR: ruledbR,
      fixR: ruledbR,
      damageR: ruledbR,
      edges: EdgeDetection.index(spec),
      permeation: Permeation.index(spec),
      ratings: Rating.index(spec),
    )
  end

  private def backmapR(ruleset : Ruleset)
    onceR = callR(PRIMITIVES)

    # First rewrite entries, then rewrite self.
    set, exhevalR = recR
    set.call chainR(entriesR(exhevalR), onceR)

    evalR = dfsR(
      switchR(
        { %[($ rewritee_)], exhevalR },
        { %[($once rewritee_)], onceR },
      )
    )

    refR = dfsR(
      switchR(
        { %[($my rewritee_)], envR(Term.of(:"$my")) },
        { %[($up rewritee_)], choiceR(envR(Term.of(:"$up")), envR(Term.of(:"$my"))) },
        { %[($down rewritee_)], choiceR(envR(Term.of(:"$down")), envR(Term.of(:"$my"))) },
      )
    )

    rulesetR(ruleset, noR, chainR(refR, evalR), noR)
  end

  # Replacement function for `walk`. It is called with a node context followed
  # by the node itself. It is expected to return a replacement -- one of `Rep::Any`.
  alias WalkFn = NodeContext, Term -> Rep::Any

  # Iterates over a sub-*range* of items of *dict*, constructing the appropriate
  # node context, and replacing items using the block (see `Rep::Any`). Returns
  # the modified copy of *dict*.
  #
  # This is the core of `D7.walk`.
  #
  # This function cannot give you absolute guarantees that you will get reference-
  # equal *dict* back if you make no replacements; however, in practice, this will
  # almost always be the case.
  #
  # *range* must be exclusive.
  private def slide(scope : Scope::Path, dict : Term::Dict, range : Range, &) : Term::Dict
    assert range.exclusive?

    if dict.items.empty?
      return dict
    end

    # Rep::Some is currently 16 bytes. 16 * 16 = 256 bytes of replacement
    # cache. Plus 16 * 4 = 64 bytes of indices. 320 bytes of stack memory
    # on these in total.
    reps = StackArray(Rep::Some, 16).new
    indices = StackArray(Int32, 16).new

    all_assigns = true

    dict.items.each_with_index do |node, index|
      next unless index.in?(range)

      l = (index - 1).in?(range) ? dict[index - 1]? : nil
      r = (index + 1).in?(range) ? dict[index + 1]? : nil
      subscope = Scope.append(scope, Scope::Qualifier.new(index))

      rep = yield NodeContext.new(subscope, l, r), node

      case rep
      in Rep::None
        next
      in Rep::One
      in Rep::Some
        all_assigns = false
      end

      reps << rep
      indices << index
    end

    # Fast path: no changes.
    if reps.empty?
      return dict
    end

    # Slower path: all replacements are assigns.
    if all_assigns
      dict1 = dict.transaction do |commit|
        reps.zip(indices) do |rep, index|
          commit.with(index, rep.as(Rep::One).term)
        end
      end

      return dict1
    end

    # Slow path: some replacements aren't assigns.
    Term::Dict.build do |commit|
      dict.items.each_with_index do |item, index|
        # This search right here could be expensive -- O(N) -- but only in theory.
        # In practice, we rarely expect this path to be hit; even tinier is the chance
        # of a dict hitting this that contains so many replacements that calling `index`
        # becomes expensive!
        unless rep_index = indices.index(index)
          commit << item
          next
        end

        case rep = reps[rep_index]
        in Rep::Zero
        in Rep::One  then commit << rep.term
        in Rep::Many then commit.concat(rep.terms)
        end
      end
    end
  end

  # Split-rewrite-mix of *node*.
  private def expand(spec : Spec, node : Term, & : Term -> Rep::Any)
    instance = split(spec, node)

    rep = yield instance

    # If splitting didn't change anything, mixing won't, either. Moreover,
    # mixing would be faulty since mix with no difference gives us the original
    # node, not its possibly modified instance which we actually want in
    # this case.
    if node == instance
      return rep
    end

    mix(spec, node, rep)
  end

  # Breaks *node* apart into its constituents, collectively known as
  # its *instance*. Nodes that have no constituents (atomic) are
  # returned as-is.
  private def split(spec : Spec, node : Term) : Term
    Alloy.response?(spec.splits, node, cache: spec.split_cache) || node
  end

  # Instance was not modified, there is no reason node should get modified.
  private def mix(spec : Spec, node : Term, offspring : Rep::None)
    Rep.none
  end

  # Strange, offspring annihilated itself. Keep node unchanged.
  private def mix(spec : Spec, node : Term, offspring : Rep::Zero)
    Rep.none
  end

  private def mix(spec : Spec, node : Term, offspring : Rep::One) : Rep::Any
    mix(spec, node, {offspring.term})
  end

  private def mix(spec : Spec, node : Term, offspring : Rep::Many) : Rep::Any
    mix(spec, node, offspring.terms)
  end

  # Mixing is the inverse of splitting. We try to assemble a node from
  # its modified constituents. If we fail, we leave the original node
  # rather than its constituents.
  private def mix(spec : Spec, node : Term, offspring : Indexable(Term))
    case r = rewrite0(Term.of(:mix, {node}, offspring), spec.mixR)
    in Rewrite::None
      Rep.none
    in Rewrite::Many
      Log.error { "confused: mix rule was rewritten to multiple offspring" }
      Rep.none
    in Rewrite::One
      Term.case(r.term) do
        matchpi %{(mix (product_*) _)} do
          Rep.some(product.items)
        end

        otherwise do
          Log.error { "confused: mix rule was rewritten to `#{ML.compact(r.term)}` which is meaningless" }
          Rep.none
        end
      end
    end
  end

  private def editing?(ranges : Permeation::PassableRangeArray, instance : Term) : Bool
    # Fast path
    unless instance.probably_includes?(Input::CUE)
      return false # not editing
    end

    # Cursors in passable spots should not count as editing. So first,
    # check if we have permeation ranges aka passable spots.

    if ranges.empty?
      # If we have no permeation ranges, then simply delegate the remainder
      # of checks to `Cursor.in?`.
      return Cursor.in?(instance, only_active: true)
    end

    # If we have some permeation ranges aka passable spots, skip them
    # and check for cursors in all other items.
    instance.each_item_with_index do |item, index|
      next if ranges.any? { |range| range.span.includes?(index) }
      next unless Cursor.in?(item, only_active: true)
      return true # editing
    end

    false # not editing
  end

  # Filter configuration for `D7.walk`.
  @[Flags]
  enum WalkConf
    # If present, enables emission of parent nodes, i.e., nodes whose passable
    # children were emitted already.
    EmitParents

    # If present, enables emission of nodes that are currently being edited.
    EmitEdited
  end

  DEFAULT_WALK_CONF = WalkConf::EmitParents

  # Traverses a D7 *program*: allows the caller to inspect, replace, or modify
  # nodes using *fn*, according to permeability, split, mix, and other rules
  # defined in *spec*.
  def walk(spec : Spec, program : Term, conf : WalkConf = DEFAULT_WALK_CONF, &fn : WalkFn) : Term
    program.as_d { |dict| walk(spec, dict, conf, &fn) }
  end

  # :ditto:
  def walk(spec : Spec, program : Term::Dict, conf : WalkConf = DEFAULT_WALK_CONF, &fn : WalkFn) : Term::Dict
    slide(Scope.toplevel, program, 0...program.itemsize) do |ctx, node|
      walk(spec, ctx, node, conf, fn)
    end
  end

  private def walk(spec : Spec, ctx : NodeContext, node : Term, conf : WalkConf, fn : WalkFn) : Rep::Any
    # Cursor in pairspart is a hard no because we don't know what kind of
    # abomination it would expand into.
    if node.type.dict? && Cursor.in?(Term.of(node.pairspart), only_active: false)
      if conf.emit_edited?
        return fn.call(ctx, node)
      end
      return Rep.none
    end

    expand(spec, node) do |instance|
      walk1(spec, ctx, instance, conf, fn)
    end
  end

  private def walk1(spec : Spec, ctx : NodeContext, instance : Term, conf : WalkConf, fn : WalkFn) : Rep::Any
    subscope = ctx.scope

    if rating = Rating.rating?(spec.ratings, instance)
      subscope = Scope.append(subscope, rating)
    end

    ctx = ctx.copy_with(scope: subscope)

    unless instance.type.dict?
      return fn.call(ctx, instance)
    end

    ranges, subscope = Permeation.ranges(spec.permeation, subscope, instance)

    # If the caller does not want edited nodes, let's filter those out.
    if !conf.emit_edited? && editing?(ranges, instance)
      return Rep.none
    end

    offspring = ranges.reduce(instance) do |memo, range|
      walk1(spec, subscope, memo, range, conf, fn)
    end

    if conf.emit_parents? || ranges.empty?
      fn.call(ctx, offspring)
    else
      Rep::One.new(offspring)
    end
  end

  private def walk1(spec : Spec, scope : Scope::Path, instance : Term, range : Permeation::PassableRange, conf : WalkConf, fn : WalkFn) : Term
    instance_dict = instance.as_d

    if range.quoted
      offspring = instance_dict.pairspart.transaction do |commit|
        instance_dict.items.each_with_index do |item, index|
          if index.in?(range.span)
            subscope = Scope.append(scope, Scope::Qualifier.new(index))
            commit << walkq1(subscope, item, fn)
          else
            commit << item
          end
        end
      end

      return Term.of(offspring)
    end

    offspring_dict = slide(scope, instance_dict, range.span) do |ctx, node|
      walk(spec, ctx, node, conf, fn)
    end

    Term.of(offspring_dict)
  end

  private def walkq1(scope : Scope::Path, term : Term, fn : WalkFn) : Term
    Term.of_case(term) do
      matchpi %{[unquote _*]} do
        term.as_d { |dict| slide(scope, dict, 1...term.itemsize, &fn) }
      end

      matchpi %{_dict} do
        term.pairspart.transaction do |commit|
          term.items.each_with_index do |item, index|
            subscope = Scope.append(scope, Scope::Qualifier.new(index))

            commit << walkq1(subscope, item, fn)
          end
        end
      end

      otherwise { term }
    end
  end

  private def infer(spec : Spec, query : Term, workspace : Term::Dict) : Term::Dict
    unless proposal = Alloy.response?(spec.proposals, query) || proposal?(query)
      return workspace
    end

    accept(workspace, proposal)
  end

  private def proposal?(query : Term) : Term?
    Term.case(query) do
      givenpi %{cycle ([fill (@a_ to @b_) body_] ⍊ m: [fill ((%'edge var_symbol) to _) _]) (%all (%value a (some x_)) (%value b none))} do
        value = Alloy.render(Term.entries({var, x}), body)

        Term.of(:proposal, b, {:some, value})
      end

      givenpi %{([(%any map latest) (@a_ to @b_) body_] ⍊ m: [(%any map latest) ((%'edge var_symbol) to _) _]) (%value a (some x_)) (%-value b)} do
        value = Alloy.render(Term.entries({var, x}), body)

        Term.of(:proposal, b, {:some, value})
      end

      givenpi %{[[map (@a_ pattern_ to @b_) body_]] (%value a (some x_)) (%-value b)} do
        # TODO: if pattern is a source pattern (implement M1.source?(normp)), emit
        # a possibly empty list with `body` for each match env
        unless env = M1.match?(pattern, x)
          next Term.of(:proposal, b, :none)
        end

        value = Alloy.render(env, body)

        Term.of(:proposal, b, {:some, value})
      end

      givenpi %{[[latest (@a_ pattern_ to @b_) body_]] (%value a (some x_)) (%-value b)} do
        # TODO: if pattern is a source pattern (implement M1.source?(normp)), emit
        # a possibly empty list with `body` for each match env
        next unless env = M1.match?(pattern, x)

        value = Alloy.render(env, body)

        Term.of(:proposal, b, {:some, value})
      end

      givenpi %{([ramp (@a_ to @b_) body_] ⍊ m: [ramp ((%'edge var_symbol) to _) _]) (%value a (some x_)) (%-value b)} do
        value = Alloy.render(Term.entries({var, x}), body)

        Term.of(:proposals,
          Term.of(:proposal, a, :none),
          Term.of(:proposal, b, {:some, value}))
      end

      givenpi %{[[ramp (@a_ pattern_ to @b_) body_]] (%value a (some x_)) (%-value b)} do
        # TODO: if pattern is a source pattern (implement M1.source?(normp)), emit
        # a possibly empty list with `body` for each match env
        next unless env = M1.match?(pattern, x)

        value = Alloy.render(env, body)

        Term.of(:proposals,
          Term.of(:proposal, a, :none),
          Term.of(:proposal, b, {:some, value}))
      end

      otherwise { }
    end
  end

  private def accept(workspace : Term::Dict, proposal : Term) : Term::Dict
    Term.case(proposal) do
      matchpi %{(proposal @edge_ v←(some _))}, %{(proposal @edge_ v←none)} do
        u = workspace[edge]?

        if u.in?(nil, Term.of(:none))
          workspace.with(edge, v)
        else
          workspace
        end
      end

      matchpi %{(proposals proposals_*)} do
        proposals.items.reduce(workspace) do |memo, item|
          accept(memo, item)
        end
      end

      otherwise do
        Log.error { "invalid proposal #{ML.compact(proposal)}" }
        workspace
      end
    end
  end

  private def boot(spec, program : Term) : Term::Dict
    workspace0 = Term[]

    refs = [] of {Scope::Path, Term, Term}
    matches = {} of Term => Array(Term::Dict)

    walk(spec, program) do |ctx, node|
      Term.case(node) do
        matchpi %{[ref @edge_ pattern_]} do
          qualedge = Scope.resolve(ctx.scope, edge)
          refs << {ctx.scope, qualedge, pattern}
          workspace0 = workspace0.with(qualedge, :none)
        end

        otherwise do
          EdgeDetection.each(spec.edges, node) do |edge|
            qualedge = Scope.resolve(ctx.scope, edge)
            workspace0 = workspace0.with(qualedge, :none)
          end
        end
      end

      Rep.none
    end

    walk(spec, program) do |ctx, node|
      scope0 = Scope.current(ctx.scope)

      refs.each do |refscope, edge, pattern|
        # Queries have the same scope as their edge.
        scope1, _ = Scope.find(refscope, edge)
        next unless scope0 == scope1

        envs = M1.matches(pattern, node)
        next if envs.empty?

        bucket = matches.put_if_absent(edge) { [] of Term::Dict }
        bucket.concat(envs)
      end

      Rep.none
    end

    refs.each do |_, target, _|
      if bucket = matches[target]?
        workspace0 = workspace0.with(target, {:some, bucket})
      else
        workspace0 = workspace0.with(target, {:some, Term[]})
      end
    end

    workspace0
  end

  private def saturate(spec, program : Term, workspace0 : Term::Dict, cycle : Bool) : Term::Dict
    loop do
      workspace1 = workspace0

      walk(spec, program) do |ctx, node|
        scoped = EdgeDetection.map(spec.edges, node) do |edge|
          Scope.resolve(ctx.scope, edge)
        end

        query = Term.of(scoped, l: ctx.l, m: node, r: ctx.r)
        workspace1 = infer(spec, cycle ? Term.of(:cycle, query, workspace1) : Term.of(query, workspace1), workspace1)

        Rep.none
      end

      break if workspace0 == workspace1

      workspace0 = workspace1
    end

    workspace0
  end

  private def saturate(spec, program : Term, workspace0 : Term::Dict) : Term::Dict
    cycle = false

    loop do
      workspace1 = saturate(spec, program, workspace0, cycle)
      break if cycle && workspace0 == workspace1

      cycle = !cycle
      workspace0 = workspace1
    end

    workspace0
  end

  private def transfer(spec, program : Term, workspace0 : Term::Dict) : Term::Dict
    workspace1 = Term[]

    walk(spec, program) do |ctx, node|
      scoped = EdgeDetection.map(spec.edges, node) do |edge|
        Scope.resolve(ctx.scope, edge)
      end

      query = Term.of(scoped, l: ctx.l, m: node, r: ctx.r)
      workspace1 = infer(spec, Term.of(query, workspace0, workspace1), workspace1)

      Rep.none
    end

    workspace1
  end

  private def fix(spec, program : Term, workspace0 : Term::Dict, workspace1 : Term::Dict) : Term
    walk(spec, program) do |ctx, node|
      locals = Term[]

      EdgeDetection.each(spec.edges, node) do |edge|
        qualedge = Scope.resolve(ctx.scope, edge)
        next unless value = workspace1[qualedge]? || workspace0[qualedge]?

        locals = locals.with(edge, value)
      end

      query = Term.of(:fix, Term.of(node, l: ctx.l, m: node, r: ctx.r), locals)
      response = rewrite(query, spec.fixR)

      Term.case(response) do
        matchpi %{(fix [offspring_*] _)} do
          Rep.some(offspring.items)
        end

        otherwise do
          Log.error { "meaningless response to (fix _*): #{ML.compact(response)}" }
          Rep.none
        end
      end
    end
  end

  # Applies a round of repair to a D7 *program*. The repair rules are given
  # by *spec*. Returns a modified copy of *program*.
  #
  # You are recommended to show repaired versions of *program* to the user
  # (i.e., the results of running this function). Show damaged versions only if
  # you absolutely cannot get a repaired one. This is because damaged versions
  # can cause (significant) confusion, since they may consist of parts that
  # are out of date relative to each other, i.e., constraints defined by the program
  # may be violated and that would look strange.
  #
  # NOTE: you must call `repair` before every round of damage (your own or
  # D7's own `damage`).
  def repair(spec : Spec, program : Term) : Term
    workspace0 = boot(spec, program)
    workspace0 = saturate(spec, program, workspace0)
    workspace1 = transfer(spec, program, workspace0)

    fix(spec, program, workspace0, workspace1)
  end

  # Applies a round of D7's own damage rules to *program*. The damage rules
  # are given by *spec*. Returns a modified copy of *program*.
  #
  # The order of your own damage functions and this one is generally not
  # important, as long as there are no clashes (as in, you do not damage
  # e.g. `decay` nodes yourself, which you probably wouldn't do).
  #
  # NOTE: you must only call damage after *program* was `repair`ed.
  def damage(spec : Spec, program : Term) : Term
    walk(spec, program) do |ctx, node|
      query = Term.of(:damage, Term.of(node, l: ctx.l, m: node, r: ctx.r))
      response = rewrite(query, spec.damageR)

      Term.case(response) do
        matchpi %{(damage [offspring_*])} do
          Rep.some(offspring.items)
        end

        otherwise do
          Log.error { "meaningless response to (damage _*): #{ML.compact(response)}" }
          Rep.none
        end
      end
    end
  end
end
