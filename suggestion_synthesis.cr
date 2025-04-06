require "./src/wirewright"

module NodeCompletion
  extend self

  USER_CURSOR_PATTERN = ML.term %{(_string | _string () @user ¦ _ suggestions: (%- _))}

  record Group, intro : String, members : Array(Term::Dict)

  # Parses *spec* into groups.
  def groups(spec : Term::Dict) : Hash(Term, Group)
    # Each node is a group. Each group is a group. Variants are not groups.
    # Maybe initializing at spec.itemsize is an overkill, but that's fine.
    groups = Hash(Term, Group).new(initial_capacity: spec.itemsize)

    spec.items.each do |spec|
      Term.case(spec) do
        matchpi %{(node name_symbol intro_string params_*)} do
          if groups.has_key?(name)
            raise "group '#{name}' already exists"
          end

          groups[name] = Group.new(intro.to(String), [params.unsafe_as_d])
        end

        matchpi %{(group name_symbol intro_string)} do
          if groups.has_key?(name)
            raise "group '#{name}' already exists"
          end

          groups[name] = Group.new(intro.to(String), [] of Term::Dict)
        end

        matchpi %{(variant name_symbol params_*)} do
          unless group = groups[name]?
            raise "group '#{name}' does not exist"
          end

          group.members.push(params.unsafe_as_d)
        end

        otherwise do
          raise "invalid spec item"
        end
      end
    end

    groups
  end

  private def pattern_and_cursorpath(cursorpath, commit, param : Term, pivot : Int32, ord : Int32)
    Term.case(param) do
      matchpi %{((%any pulse const broadcast) _ _string)} do
        if pivot == ord
          commit << USER_CURSOR_PATTERN

          {ord, true}
        else
          commit << {:edge, :_}

          {ord + 1, false}
        end
      end

      matchpi %{(literal _ _string)} do
        if pivot == ord
          commit << USER_CURSOR_PATTERN

          {ord, true}
        else
          commit << :_

          {ord + 1, false}
        end
      end

      matchpi %{(comma term_)} do
        if pivot == ord
          commit << USER_CURSOR_PATTERN

          {ord, true}
        else
          commit << {:"%literal", term}

          {ord + 1, false}
        end
      end

      matchpi %{(nested children_*)} do
        if pivot == ord
          commit << USER_CURSOR_PATTERN

          {ord, true}
        else
          ord += 1 # Since `nested` itself is addressable

          stopped = false
          commit << Term::Dict.build do |nested|
            ord, stopped = pattern_and_cursorpath(cursorpath, nested, children.unsafe_as_d, pivot, ord)
          end

          {ord, stopped}
        end
      end

      matchpi %{(prop? _ _)} do
        {ord, false}
      end
    end
  end

  private def pattern_and_cursorpath(cursorpath, commit, params : Term::Dict, pivot : Int32, ord : Int32, *, offset = 0)
    params.items.each_with_index do |param, index|
      cursorpath.push(index + offset)

      ord, stopped = pattern_and_cursorpath(cursorpath, commit, param, pivot, ord)
      if stopped
        return ord, true
      end

      cursorpath.pop
    end

    {ord, false}
  end

  # Synthesizes a suggestion-triggering pattern based on *params*. *pivot*
  # specifies which parameter should be "active" in the suggestion.
  def pattern_and_cursorpath(name : Term, params : Term::Dict, pivot : Int32) : {Term, Term::Dict}
    cursorpath = Stack(Int32).new

    pattern = Term::Dict.build do |commit|
      commit << name

      ord, stopped = pattern_and_cursorpath(cursorpath, commit, params, pivot, ord: 0, offset: 1)
      unless stopped
        raise IndexError.new("pivot out of bounds")
      end
    end

    {Term.of(:"%partition", pattern, :_), Term[cursorpath]}
  end

  private def maxpivot(param : Term, ord : Int32) : Int32
    Term.case(param) do
      matchpi(
        %{((%any const pulse broadcast) _ _string)},
        %{(literal _ _string)},
        %{(comma term_)},
      ) { ord + 1 }

      matchpi %{(prop? _ _string)} do
        ord
      end

      matchpi %{(nested children_*)} do
        maxpivot(children.unsafe_as_d, ord) + 1
      end
    end
  end

  # Returns the exclusive pivot bound for the given *params*.
  def maxpivot(params : Term::Dict, ord : Int32 = 0) : Int32
    params.items.each do |item|
      ord = maxpivot(item, ord)
    end

    ord
  end

  private def long_head(io, param : Term, pivot : Int32, ord : Int32) : Int32
    Term.case(param) do
      matchpi %{((%any const pulse broadcast) id_ _string)} do
        if pivot == ord
          io << "*@" << id << "_*"
        else
          io << "@" << id << "_"
        end

        ord + 1
      end

      matchpi %[(literal id_ _string)] do
        if pivot == ord
          io << "*" << id << "_*"
        else
          io << id << "_"
        end

        ord + 1
      end

      matchpi %{(comma term_)} do
        if pivot == ord
          io << "*" << term << "*"
        else
          io << term
        end

        ord + 1
      end

      matchpi %{(prop? id_ _string)} do
        io << id << "?: ..."

        ord
      end

      matchpi %{(nested children_*)} do
        if pivot == ord
          io << "*(*"
          ord = long_head(io, children.unsafe_as_d, pivot, ord + 1)
          io << "*)*"
        else
          io << "("
          ord = long_head(io, children.unsafe_as_d, pivot, ord + 1)
          io << ")"
        end

        ord
      end
    end
  end

  private def long_head(io, params : Term::Dict, pivot : Int32, ord : Int32) : Int32
    params.items.each_with_index do |param, index|
      io << " " if index > 0

      ord = long_head(io, param, pivot, ord)
    end

    ord
  end

  # Synthesizes the head of a long suggestion based on node *name* and the list
  # of its *params*. *pivot*-th parameter is highlighted.
  def long_head(name : String, params : Term::Dict, pivot : Int32) : String
    String.build do |io|
      io << "["
      io << name
      io << " "
      long_head(io, params, pivot, ord: 0)
      io << "]"
    end
  end

  private def long_body(io, param : Term, pivot : Int32, ord : Int32) : Int32
    Term.case(param) do
      matchpi %{(const id_ desc_string)} do
        if pivot == ord
          io << "*@" << id << "*"
        else
          io << "@" << id
        end
        io << " const - " << desc.to(String)
        io.puts

        ord + 1
      end

      matchpi %{(pulse id_ desc_string)} do
        if pivot == ord
          io << "*@" << id << "*"
        else
          io << "@" << id
        end
        io << " pulse - " << desc.to(String)
        io.puts

        ord + 1
      end

      matchpi %{(broadcast id_ desc_string)} do
        if pivot == ord
          io << "*@" << id << "*"
        else
          io << "@" << id
        end
        io << " broadcast - " << desc.to(String)
        io.puts

        ord + 1
      end

      matchpi %{(literal id_ desc_string)} do
        if pivot == ord
          io << "*" << id << "*"
        else
          io << id
        end
        io << " literal - " << desc.to(String)
        io.puts

        ord + 1
      end

      matchpi %{(comma _)} do
        ord + 1
      end

      matchpi %{(prop? id_ desc_string)} do
        io << "property " << id << " -  " << desc.to(String)

        ord
      end

      matchpi %{(nested children_*)} do
        long_body(io, children.unsafe_as_d, pivot, ord + 1)
      end
    end
  end

  private def long_body(io, params : Term::Dict, pivot : Int, ord : Int) : Int32
    params.items.each do |param|
      ord = long_body(io, param, pivot, ord)
    end

    ord
  end

  # Synthesizes the body of a long suggestion based on node introduction message
  # *intro* and the list of its *params*. *pivot*-th parameter is highlighted.
  def long_body(intro : String, params : Term::Dict, pivot : Int) : String
    String.build do |io|
      io << intro
      io.puts
      io.puts

      long_body(io, params, pivot, ord: 0)
    end
  end

  # Renders the long suggestion for a node with the given *name*, introduction
  # message *intro*, and *params*; *pivot*-th parameter is highlighted.
  #
  # Raises `ArgumentError` if *params* is invalid.
  def long(name : String, intro : String, params : Term::Dict, pivot = nil) : Term
    Term.of(long_head(name, params, pivot), long_body(intro, params, pivot))
  end

  # Represents a number of completion options.
  #
  # ```text
  # (cell |)
  #     +-^----- cursorpath points to the cursor
  #     |
  #     | - ("(cell *@cout_*)" "...")  } options
  #     | - ("(cell *v_* @cin)" "...") }
  #
  # ```
  class CompletionOptions
    getter cursorpath, options

    def initialize(@cursorpath : Term::Dict, @options : Array(Term))
    end

    # Returns the suggestion node corresponding to this completion.
    getter! suggestions : Term?

    # Renders this completion into a (suggestions/group ...) suggestion node.
    def render! : Nil
      if @options.empty?
        raise "BUG: completion options must not be empty! Did you create an empty group?"
      end

      @suggestions ||= Term.of(:"suggestions/group", Term[], @options)
    end
  end

  def intros_and_completions(groups : Hash(Term, Group)) : {Array(Term), Hash(Term, CompletionOptions)}
    intros = Array(Term).new(groups.size)

    # Associate each pattern with a list of completion options. Note that
    # cursorpaths correspond 1:1 to patterns; so we store them in the value.
    completions = {} of Term => CompletionOptions

    groups.each do |name, group|
      intros << Term.of(name.to(String), group.intro)

      group.members.each do |params|
        (0...maxpivot(params)).each do |pivot|
          pattern, cursorpath = pattern_and_cursorpath(name, params, pivot)

          completion = completions.put_if_absent(pattern) { CompletionOptions.new(cursorpath, [] of Term) }
          completion.options << long(name.to(String), group.intro, params, pivot)
        end
      end
    end

    {intros, completions}
  end

  struct CompletionManager
    # :nodoc:
    def initialize(@intros : Array(Term), @pset : PatternSet, @options : Array(CompletionOptions))
    end

    # Constructs a completion manager for the given *spec*.
    def self.new(spec : Term::Dict) : CompletionManager
      intros, completions = pipe(
        spec,
        NodeCompletion.groups,
        NodeCompletion.intros_and_completions
      )

      # To make sense of the suggestions we need a pattern set; it also helps
      # optimize queries (especially negative ones) to a large number of patterns.
      #
      # Pattern set's interface is dict/pattern-driven; so we'll have to render
      # completions as a dict first.
      patterns = Term::Dict.build do |commit|
        completions.each_key { |pattern| commit << pattern }
      end

      # Maps pattern index to competion options for that pattern.
      options = Array(CompletionOptions).new(completions.size)

      pset = PatternSet.select(Term.of(:pattern_), Term.of(patterns)) do |_, env|
        completion = completions[env[:pattern]]
        completion.render!

        options << completion

        true # Add to the set
      end

      new(intros, pset, options)
    end

    private def suggest?(query : String, candidate : String) : Bool
      query.empty? || candidate.downcase.starts_with?(query.downcase)
    end

    # If *node* contains an appropriately positioned `@user` cursor, enhances
    # that cursor with suggestions and returns the modified copy of *node*.
    #
    # Otherwise, returns `nil`.
    def complete?(node : Term) : Term?
      Term.case(node) do
        matchpi %{[(lhs_string | rhs_string () @user ¦ _ suggestions: (%- _))]} do
          query = lhs.to(String) + rhs.to(String)

          prefix = Term[]
          visible = Term[]
          postfix = Term[]

          @intros.each do |candidate|
            name, intro = candidate
            next unless suggest?(query, name.to(String))

            if visible.itemsize < 5
              visible = visible.append(candidate)
            else
              postfix = postfix.append(candidate)
            end
          end

          return unless visible.size >= 1

          suggestions = Term.of(:"suggestions/list", prefix, visible, postfix)

          Term.of(node.morph({0, :suggestions, suggestions}))
        end

        # Try the narrow, group completion.
        otherwise do
          case pr = @pset.response(node)
          in Pr::Neg
            return
          in Pr::Many
            unreachable
          in Pr::One
          end

          completion = @options[pr.pattern.index]

          Term.of(node.as_d.follow(completion.cursorpath.items) { |it| it.with(:suggestions, completion.suggestions).upcast })
        end
      end
    end
  end
end

# params = ML.terms <<-WWML
#   (nested
#     (pulse pin "source of terms to transform")
#     (literal pattern "pattern that terms to transform must match"))
#   (comma to)
#   (pulse pout "sink for transformed terms")
#   (literal body "Nitrene program that will do the transformation; has access to captures made by pattern")
# WWML

# (0...NodeCompletion.maxpivot(params.as_d)).each do |pivot|
#   pattern, cursorpath = NodeCompletion.pattern_and_cursorpath(Term.of(:transform), params.as_d, pivot)
#   long = NodeCompletion.long("transform", "lorem ipsum", params.as_d, pivot)
#   puts long[0].to(String)
#   puts long[1].to(String)
#   puts ML.display(pattern)
#   puts ML.display(cursorpath)
# end
# src = ML.term <<-WWML
# (transform ("" | "" () @user))
# WWML

# puts mgr.complete?(src)
