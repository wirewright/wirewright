module MuSoma
  defrecord Editor, key : Term?, pending : Bool

  # :nodoc:
  SYM_EDITOR_KERNEL = Term[:I]

  # :nodoc:
  #
  # Growth Point
  SYM_EDITOR_GP = Term[:*]

  # :nodoc:
  SYM_EDITOR_KERNEL_SUMMARY = Term::Dict::Summary.of(Term.of(SYM_EDITOR_KERNEL))

  # :nodoc:
  SYM_EDITOR_SUMMARY = Term::Dict::Summary.union(
    Term::Dict::Summary.of(Term.of(SYM_EDITOR_KERNEL)),
    Term::Dict::Summary.of(Term.of(SYM_EDITOR_GP)),
  )

  def editor?(term : Term) : Bool
    Term.case(term) do
      matchpi %{[[I _*] (_string * _string) (%number +i32)]} { true }
      otherwise { false }
    end
  end

  # Returns `true` if there is at least one editR editor kernel `[I _*]` in *world*.
  # This function inspects *world* itself, its entries if it is a dict and so on
  # recursively.
  def editing?(world : Term) : Bool
    unless world_dict = world.as_d?
      return false # not editing
    end

    unless world_dict.probably_includes?(SYM_EDITOR_KERNEL)
      return false # not editing
    end

    Term.matchpi?(world, %{[I _*]}) do
      return true # editing
    end

    # Use guided descent to speed up search.
    world_dict.each_entry(SYM_EDITOR_KERNEL_SUMMARY) do |_, value|
      # editR can't edit keys so we can safely discard them.
      if editing?(value)
        return true # editing
      end
    end

    false # not editing
  end

  # Finds all editR editor kernels in *world*. This function inspects *world*
  # itself, its entries if it is a dict and so on recursively.
  def editors(world : Term) : Slice(Editor)
    return Slice(Editor).empty unless world_dict = world.as_d?
    return Slice(Editor).empty unless world_dict.probably_includes?(SYM_EDITOR_KERNEL)

    Term.matchpi?(world, %{[I _*]}) do
      key = nil
      pending = world_dict.itemsize > 1 # [I _ _*]
      return Slice[Editor.new(key, pending)]
    end

    sink = Pf::Kit.stack_array(Editor, 4)

    # Use guided descent to speed up search.
    world_dict.each_entry(SYM_EDITOR_KERNEL_SUMMARY) do |key, value|
      editors = editors(value)
      editors.each do |editor|
        # Editor#key is the outermost key.
        sink << Editor.new(key, editor.pending)
      end
    end

    sink.to_unsafe_readonly_slice!
  end

  # Qualifies an editor motion.
  #
  # See the editR codex at runtime/codices for more info on motions.
  defrecord Motion, term : Term

  # Constructs a `Motion`.
  def motion(term : Term) : Motion
    Motion.new(term)
  end

  # Delivers *motion* to all editR editor kernels in *world*.
  def dispatch(world : Term, motion : Motion) : Term
    return world unless world_dict = world.as_d?
    return world unless world_dict.probably_includes?(SYM_EDITOR_KERNEL)

    Term.matchpi?(world, %{[I _*]}) do
      return Term.of(world_dict.append(motion.term))
    end

    world_dict = world_dict.transaction do |commit|
      # Use guided descent to speed up search.
      world_dict.each_entry(SYM_EDITOR_KERNEL_SUMMARY) do |key, value|
        commit.with(key, dispatch(value, motion))
      end
    end

    Term.of(world_dict)
  end

  private def editor_subst(world : Term, &fn : Term -> Term::Rep?) : Term::Rep
    unless world_dict = world.as_d?
      return Term.rep(world)
    end

    unless world_dict.probably_includes?(SYM_EDITOR_KERNEL, SYM_EDITOR_GP)
      return Term.rep(world)
    end

    if rep = yield world
      return rep
    end

    changes = Pf::Kit.stack_array({Term, Term, Term::Rep}, 4)

    # Use guided descent to speed up search.
    world_dict.each_entry(SYM_EDITOR_SUMMARY) do |key, value|
      rep = editor_subst(value, &fn)
      next unless Term.changes?(value, after: rep)

      changes << {key, value, rep}
    end

    # Apply changes.
    world_dict = Term.patch(world_dict, changes)

    Term.rep_of(world_dict)
  end

  # Omits editors in *world*.
  #
  # `curate` simply erases all editors. If an editor is the value of a pair,
  # the pair is removed; if it is an item, the item is removed and items
  # ahead are shifted to the left.
  def curate(world : Term) : Term::Rep
    editor_subst(world) do |candidate|
      Term.matchpi?(candidate, %{[[I _*] (_string * _string) (%number +i32)]}) do
        Term.rep
      end
    end
  end

  # Same as `curate`, but expects the result of curation to be a single term
  # (which is predominantly true except for a small number of edge cases);
  # otherwise, falls back to no curation.
  def curate_single(world : Term) : Term
    rep = curate(world)
    rep.single? || world
  end

  # Hides the existence of editors in *world* where possible. If an editor's
  # content can be parsed, the editor is replaced with the resulting term;
  # otherwise it is removed like in `curate`.
  def hide(world : Term) : Term::Rep
    editor_subst(world) do |candidate|
      Term.matchpi?(candidate, %{[[I _*] (l_string * r_string) (%number +i32)]}) do |l, r|
        l = l.to(String)
        r = r.to(String)
        next Term.rep if l.empty? && r.empty?

        begin
          term = ML.term(l + r)
        rescue ML::SyntaxError
          next Term.rep # Behave like curate() in case of a syntax error.
        end

        Term.rep(term)
      end
    end
  end

  # Same as `hide`, but expects the result of concealment to be a single
  # term (which is predominantly true except for a small number of edge cases);
  # otherwise, falls back to no concealment.
  def hide_single(world : Term) : Term
    rep = hide(world)
    rep.single? || world
  end

  private def incomplete?(clf, feature : D7::Inert, strict : Bool) : Bool
    if editor?(feature.node)
      return false # If it is the editor itself, then it's complete.
    end

    # If the editor is deeper, then it is incomplete.
    editing?(feature.node)
  end

  private def incomplete?(clf, feature : D7::Gnd, strict : Bool) : Bool
    Term.case(feature.node) do
      # A pool cell is passable. Prevent MuSoma from disabling it while it
      # is edited.
      matchpi %{[cell (pool @_) _*]} do
        false
      end

      otherwise do
        editing?(feature.node)
      end
    end
  end

  private def incomplete?(clf, feature : D7::Parent, strict : Bool) : Bool
    impassable_ranges = {0...feature.range.begin, feature.range.end...feature.node.itemsize}
    impassable_ranges.each do |impassable_range|
      impassable_range.each do |index|
        item = feature.node[index]
        if editing?(item)
          return true # incomplete
        end
      end
    end

    unless strict
      return false # complete
    end

    # If strict, check passable spots.
    feature.range.each do |index|
      item = feature.node[index]
      next unless item_dict = item.as_d?
      next unless item_dict.probably_includes?(SYM_EDITOR_KERNEL)

      subfeature = clf.call(item)
      if incomplete?(clf, subfeature, strict)
        return true # incomplete
      end
    end

    false # complete
  end

  private def incomplete?(clf, feature : D7::Circuit, strict : Bool) : Bool
    incomplete?(clf, D7.parent(feature.node, feature.range), strict)
  end

  private def incomplete?(clf, feature : D7::Scope, strict : Bool) : Bool
    incomplete?(clf, feature.cont, strict)
  end

  private def incomplete?(clf, feature : D7::Mixture, strict : Bool) : Bool
    incomplete?(clf, clf.call(feature.defn), strict: true)
  end

  # Returns `true` if *feature* contains an active editor (i.e., editing is
  # in progress for *feature* or one of its subterms; meaning it must not
  # yet be allowed to exist as a node).
  #
  # `incomplete?` is designed for deciding which nodes to disable and when.
  # If you need to check whether a node is being edited in general (for instance,
  # to suppress a widget of some sort), you must use `editing?`.
  def incomplete?(clf : D7::Classifier, feature : D7::Feature) : Bool
    incomplete?(clf, feature, strict: false)
  end
end
