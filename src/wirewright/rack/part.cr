# The implementation of the `part` node. See also: `rack.part`.
module Ww::Rack::Part
  extend self

  # :nodoc:
  SYM_PART = Term.of(:part)

  def prepass(hg : D7::Hypergraph, &fn : D7::Hypergraph -> D7::Patch) : D7::Patch
    # Fast path.
    unless hg.has_head?(SYM_PART)
      return fn.call(hg)
    end

    roots, forms, layers, rejected_abs_parts = collect(hg)
    replacements, replaced = submit(forms, rejected_abs_parts)
    patch = fn.call(hg.gnd_map(replacements))
    updates = absorb(layers, rejected_abs_parts, patch)
    merge(patch, replaced, roots, updates)
  end

  # :nodoc:
  alias Form = ReadWriteForm | WriteOnlyForm

  # :nodoc:
  defrecord ReadWriteForm,
    id : D7::NodeId,
    addr : D7::NodeAddr,
    abs_whole : D7::AbsEdge,
    abs_part : D7::AbsEdge,
    rel_part : Term,
    update : Term::Rep -> Term::Rep?,
    reference : Term,
    value : Term

  # :nodoc:
  defrecord WriteOnlyForm,
    id : D7::NodeId,
    addr : D7::NodeAddr,
    abs_whole : D7::AbsEdge,
    abs_part : D7::AbsEdge,
    rel_part : Term,
    update : Term::Rep -> Term::Rep?,
    reference : Term

  # :nodoc:
  alias Variant = PatternVariant | BacksysVariant

  # :nodoc:
  defcase PatternVariant,
    whole_edge : Term,
    part_edge : Term,
    whole_capture : Term,
    part_capture : Term,
    pattern : M1::Op::Any

  # :nodoc:
  defcase BacksysVariant,
    whole_edge : Term,
    part_edge : Term,
    whole_capture : Term,
    part_capture : Term,
    backsys : Slice({M1::Op::Any, Term::Dict})

  private def recognize?(term : Term) : Variant?
    Term.case(term) do
      matchpi %{[part (whole←(%'edge input_) part←(%'edge output_)) [backmap _ _] _*]} do
        backmaps = term.items.move(2)
        backsys = backmaps.to_compact_readonly_slice do |backmap|
          Term.matchpiT?(backmap, %{[backmap pattern_ backspec_dict]}) do
            {M1.operator(pattern), backspec}
          end
        end

        BacksysVariant.new(whole, part, input, output, backsys)
      end

      matchpi %{[part (whole←(%'edge input_) part←(%'edge output_)) pattern_]} do
        PatternVariant.new(whole, part, input, output, M1.operator(pattern))
      end

      otherwise { }
    end
  end

  private def value_of_cell?(patch : D7::Patch, id : D7::NodeId) : Term::Rep?
    return unless rep = patch[id]?

    Term.case(rep) do
      matchpi %{[cell _]} { Term.rep }
      matchpi %{[cell _ value_]} { Term.rep(value) }
      otherwise { }
    end
  end

  # :nodoc:
  MAX_COLLECT_ITERATIONS = 128

  private def collect(hg : D7::Hypergraph)
    forms = {} of D7::AbsEdge => Form
    staging_forms = {} of D7::AbsEdge => Form
    rejected_abs_parts = Set(D7::AbsEdge).new
    roots = {} of D7::NodeId => {D7::AbsEdge, Cell}
    layers = [] of Array(Form)
    seen = Set(D7::NodeId).new

    MAX_COLLECT_ITERATIONS.times do
      hg.each_node_with_head(SYM_PART) do |node|
        next if seen.includes?(node.id)
        next unless variant = recognize?(node.term)

        abs_part = hg.resolve(node.addr, variant.part_edge)
        next if rejected_abs_parts.includes?(abs_part)
        abs_whole = hg.resolve(node.addr, variant.whole_edge)

        whole_form = forms[abs_whole]?

        case whole_form
        in Nil
          root = Rack.cell?(hg, abs_whole)
          unless matchee = root.try(&.value?)
            # An unconditional skip:
            #
            #   (cell @root)
            #   ⏏(part (@root @a) (+ a_ _))⏏
            #
            # There's no way the pattern can match -- neither in the backsys, nor in
            # the pattern variant.
            next
          end

          # (cell @root (+ 1 2))
          # ⏏(part (@root @a) (+ a_ _))⏏
        in ReadWriteForm
          # Parts from previous layers are treated as cells. For example:
          #
          #   (cell @root (+ (1) 2))
          #   (part (@root @lhs) (+ a_ _))
          #   ⏏(part (@lhs @x) (x_))⏏
          #
          # Here, the first `part` is already transformed to:
          #
          #   (cell @root (+ (1) 2))
          #   (cell @lhs (1))
          #              ---
          #              whole_form.value
          #   ⏏(part (@lhs @x) (x_))⏏
          #
          # via `forms`, which we read here and treat as the matchee.
          matchee = whole_form.value
        in WriteOnlyForm
          # In the following:
          #
          #   (cell @root ())
          #   (part (@root @tail) (_* `tail))
          #   ⏏(part (@tail @a) (+ a_ _))⏏
          #
          # @tail does not actually exist. It is a write-only form:
          #
          #   (cell @root ())
          #   (cell @tail)
          #   ⏏(part (@tail @a) (+ a_ _))⏏
          #
          # So we skip it immediately. There is no way a pattern or backsys variant
          # can match.
          next
        end

        # Mark as seen parts that had a chance to examine a value.
        seen << node.id

        case variant
        in BacksysVariant
          # Ask the backsystem {whole: _}, it should reply with at least {part: _}, but usually
          # it will reply with {whole: _, part: _} (i.e. extend our query).
          query = Term.of(Term[].with(variant.whole_capture, matchee))
          reply = M1.backmap(variant.backsys, query)
          next unless output = reply.as_d?

          update = updatef(variant, matchee)

          if value = reply[variant.part_capture]?
            part_form = ReadWriteForm.new(node.id, node.addr, abs_whole, abs_part, variant.part_edge, update, matchee, value)
          else
            part_form = WriteOnlyForm.new(node.id, node.addr, abs_whole, abs_part, variant.part_edge, update, matchee)
          end
        in PatternVariant
          env_log_lists = M1.matches_and_logs(Term[], variant.pattern, matchee)
          # Skip this `part` if there are no matches whatsoever.
          next if env_log_lists.empty?

          update = updatef(variant, matchee, env_log_lists)

          part_form = pass do
            next unless env_log_list = env_log_lists.single?

            env, _ = env_log_list
            next unless value = env[variant.part_capture]?

            # We can only make a read-write cell if there's just one match.
            #
            #   (cell @root (+ 1 2))
            #   (part (@root @a) (+ a_ _))
            ReadWriteForm.new(node.id, node.addr, abs_whole, abs_part, variant.part_edge, update, matchee, value)
          end

          # Otherwise we're confused so we make a write-only cell.
          #
          #   (cell @root (1 2 3 4 5))
          #   (part (@root @n) ⟨±n⟩°)
          #
          # In this example, we'd like writes to @n to succeed and replace numbers
          # in @root, but allowing *reads* from @n does not make any sense.
          part_form ||= WriteOnlyForm.new(node.id, node.addr, abs_whole, abs_part, variant.part_edge, update, matchee)
        end

        # If the pattern matches, only then do we reject conflicting parts, i.e.,
        # we reject *conflicting matching parts*, and not e.g. mutually exclusive
        # parts sitting on the same part edge. That is:
        #
        #   (part (@xs @x) (`x))
        #   (part (@xs @x) (x_))
        #
        # ... is valid, whereas:
        #
        #   (part (@xs @x) (x_ _))
        #   (part (@xs @x) (_ x_))
        #
        # ... is not.
        if forms.has_key?(abs_part) || staging_forms.has_key?(abs_part)
          rejected_abs_parts << abs_part
          next
        end

        staging_forms[abs_part] = part_form

        if root
          roots[root.node.id] = {abs_whole, root}
        end
      end

      break if staging_forms.empty?

      # Merge
      layer = [] of Form
      staging_forms.each do |addr, form|
        layer << form
        forms[addr] = form
      end
      layers << layer
    ensure
      staging_forms.clear
    end

    {roots, forms, layers, rejected_abs_parts}
  end

  private def updatef(variant : BacksysVariant, matchee : Term)
    ->(rep : Term::Rep) do
      query = Term.of(
        Term[]
          .with(variant.whole_capture, matchee)
          .with(variant.part_capture, Term.collapse(rep))
      )
      reply = M1.backmap(variant.backsys, query)
      return unless output = reply.as_d?

      value = output[variant.whole_capture]?

      # If we query:
      #
      #   {whole: _, part: _}
      #
      # ... and the backsystem responds with something that lacks `whole`, this means,
      # from our point-of-view, that the backsystem removed whole in response to part.
      # We map this to clearing of the corresponding cell.
      value ? Term.rep(value) : Term.rep
    end
  end

  private def updatef(variant : PatternVariant, matchee : Term, env_log_lists : M1::EnvLogList)
    ->(rep : Term::Rep) do
      backspec = pass do
        # This should remove `1`:
        #
        #   (cell @root (+ 1 2))
        #   (part (@root @a) (+ a_ _))
        #   (discard @a)
        #
        # like so:
        #
        #   (cell @root (+ 2))
        if rep.empty?
          next Term[].with({variant.part_capture}, Term[])
        end

        value = Term.collapse(rep)

        if value.type.dict? || value.type.symbol?
          Term[].with(variant.part_capture, {:"^verbatim", value})
        else
          # Do not waste time doing ^verbatim stuff on terms that cannot cause
          # us trouble: booleans, numbers, etc.
          Term[].with(variant.part_capture, value)
        end
      end

      backmap = {env_log_lists, backspec}
      M1.backmapR({backmap}, matchee).as(Term::Rep?)
    end
  end

  private def submit(forms : Hash(D7::AbsEdge, Form), rejected_abs_parts)
    replacements = {} of D7::NodeAddr => D7::Gnd
    replaced = Set(D7::NodeId).new

    forms.each do |_, form|
      next if form.abs_part.in?(rejected_abs_parts)

      case form
      in WriteOnlyForm
        replacements[form.addr] = D7.gnd(Term.of(:cell, form.rel_part), form.rel_part)
      in ReadWriteForm
        replacements[form.addr] = D7.gnd(Term.of(:cell, form.rel_part, form.value), form.rel_part)
      end

      replaced << form.id
    end

    {replacements, replaced}
  end

  # `part`s must now "absorb" their corresponding changes bottom-up.
  private def absorb(layers : Array(Array(Form)), rejected_abs_parts, patch : D7::Patch) : Hash(D7::AbsEdge, Term::Rep)
    updates = {} of D7::AbsEdge => Term::Rep

    staging_updates = {} of D7::AbsEdge => {Term, Array(Term::Rep)}
    staging_updates_clash = false

    layers.reverse_each do |layer|
      layer.each do |form|
        next if form.abs_part.in?(rejected_abs_parts)

        rep = value_of_cell?(patch, form.id) || updates[form.abs_part]?
        next if rep.nil?
        next unless update = form.update.call(rep)

        reference, bucket = staging_updates.put_if_absent(form.abs_whole) { {form.reference, [] of Term::Rep} }
        assert reference == form.reference

        bucket << update
        if bucket.size > 1
          staging_updates_clash = true
        end
      end

      # Fast path: merge immediately if all buckets contain just one replacement.
      unless staging_updates_clash
        staging_updates.each do |abs_whole, (_, bucket)|
          updates[abs_whole] = bucket.single
        end
        next
      end

      # Here we "cheat" a little and just use our new Term diff algorithm and D7.merge
      # to do the hard work of reconciling the changes.
      #
      # NOTE: I'm not sure this much machinery is justified... On one hand we do backmaps,
      # on another we do a full-blown `D7.merge`... I suspect we'll use the term diff
      # algorithm in backmaps at some point in the future, though. The current impementation
      # of backmaps is rather... strange, to say the least, and I find it just barely
      # comprehensible.

      staging_updates.each do |abs_whole, (reference, bucket)|
        if rep = bucket.single? # Fast path
          updates[abs_whole] = rep
          next
        end

        # We translate the change bucket to a list of conflicting patches to a virtual
        # `(wrapper _?)` node. We need a wrapper node to represent erasure (`rep.empty?`)
        proposals = bucket.map do |member_rep|
          if member_rep.empty?
            D7::Patch.assoc(0u32, Term.of(:wrapper, nil))
          else
            D7::Patch.assoc(0u32, Term.of(:wrapper, Term.collapse(member_rep)))
          end
        end

        merged_patch, _ = D7.merge(proposals) do |id|
          assert id == 0u32

          # Use the `space` merge policy to merge deeply.
          {Term.of(:wrapper, reference), D7::MergeDiff.new(UInt32::MAX)}
        end

        next unless wrapper_rep = merged_patch[0u32]?

        Term.case(wrapper_rep) do
          matchpi %{(wrapper)} do
            updates[abs_whole] = Term.rep
          end

          matchpi %{(wrapper value_)} do
            updates[abs_whole] = Term.rep(value)
          end
        end
      end
    ensure
      staging_updates.clear
    end

    updates
  end

  private def merge(
    patch : D7::Patch,
    replaced : Set(D7::NodeId),
    roots : Hash(D7::NodeId, {D7::AbsEdge, Cell}),
    updates : Hash(D7::AbsEdge, Term::Rep),
  ) : D7::Patch
    # Remove patches to `cell`s we've replaced `part`s with.
    replaced.each do |id|
      patch = patch.dissoc(id)
    end

    # Update roots.
    roots.each do |_, (edge, root)|
      next unless update = updates[edge]?

      # Map empty term replacements to clearing of the root cell. E.g.:
      #
      #   (cell @root (+ 1 2))
      #   (part (@root @x) x_)
      #   (discard @x)
      #
      # ... should result in:
      #
      #   (cell @root)
      value = nil
      if update.present?
        value = Term.collapse(update)
      end

      patch = D7.patches(patch, D7.patch(root.node, {2, value}))
    end

    patch
  end
end
