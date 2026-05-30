module Ww::Microfold
  alias UpboundDesignation = RootDesignation
  alias DownboundDesignation = ItemDesignation | CascadingDesignation

  # Just so I don't forget:
  #
  # - *Downflow* is the flow of *downbound* designations from parent(s).
  # - *Upflow* is the flow of *upbound* designations from children.
  # - *Owned* refers to designations assigned by the node to itself (e.g.
  #   in `(n style: "bg-neutral")`, `bg-neutral` will produce an
  #   owned designation)
  #
  # Then:
  #
  # - Finding selfbound() designations means finding designations for
  #   the current node, from the current node's point of view, according
  #   to the downflow and owned designations.
  # - Finding upbound() designations means finding designations for
  #   the parent node(s), from the current node's point of view, according
  #   to the downflow and owned designations.
  # - Finding downbound() designations means finding designations for
  #   children nodes, according to the downflow and owned designations.
  #
  # selfbound(), upbound(), and downbound() designations are *undirected*
  # (they are simply `Designation`s rather than `DirectedDesignation`s)
  # because the whole point of finding them is to strip them of their
  # direction. That is, the act of finding, as described here, is as
  # much an act of *filtering* (or *selection*) as that of finding; as is
  # evident from the code.

  private def selfbound(head : Term::Sym, dnflow : Slice(DownboundDesignation), owned : Slice(DirectedDesignation)) : Hash(Term, Term::Dict)
    seq = Pf::Kit.stack_array(Designation)

    dnflow.each do |dn_designation|
      case dn_designation
      in ItemDesignation
        seq << dn_designation.payload.successor
      in CascadingDesignation
        next if head.in?(dn_designation.exceptions)

        seq << dn_designation.successor
      end
    end

    owned.each do |directed_designation|
      case directed_designation
      in SelfDesignation, CascadingDesignation
        # CascadingDesignations here are *our* ones, ones *we* introduce
        # & expect to apply on children as well as on ourselves if possible.
        # Importantly, CascadingDesignation#exceptions are only about *receiving*
        # such designations, i.e., *accepting them from above*.
        seq << directed_designation.successor
      in ItemDesignation, RootDesignation
      end
    end

    # See `StyleOrigin`.
    seq.sort_by!(&.origin)

    # Merge designations targeting the same box using `Term.union`, following
    # first `StyleOrigin` order (as in inherited text color loses to owned text
    # color), then user-order (as in `text-neutral text-red`, where neutral
    # loses to red).
    designations = {} of Term => Term::Dict

    seq.each do |designation|
      settings0 = designations[designation.box]? || Term[]
      settings1 = Term.union(settings0, designation.settings)
      designations[designation.box] = settings1
    end

    designations
  end

  private def upbound(owned : Slice(DirectedDesignation)) : Slice(UpboundDesignation)
    seq = Pf::Kit.stack_array(UpboundDesignation, 4)

    owned.each do |directed_designation|
      case directed_designation
      in SelfDesignation, CascadingDesignation, ItemDesignation
      in RootDesignation
        seq << directed_designation
      end
    end

    seq.to_unsafe_readonly_slice!
  end

  # NOTE: downbound() lets CascadingDesignations through even if the head of
  # the current node is an exception according to CascadingDesignation#exceptions.
  private def downbound(dnflow : Slice(DownboundDesignation), owned : Slice(DirectedDesignation)) : Slice(DownboundDesignation)
    seq = Pf::Kit.stack_array(DownboundDesignation, 8)

    dnflow.each do |designation|
      case designation
      in ItemDesignation
        case payload = designation.payload
        in SelfDesignation
          # In e.g. `... item-p-5 ...`, p-5 targets each child, but does not
          # cascade. In a sense, it decays. We do not need to add it to the downflow.
        in CascadingDesignation
          # In e.g. `... item-font-bold ...`, font-bold targets each child, and
          # then, cascades. We need to add it to the downflow.
          seq << payload
        end
      in CascadingDesignation
        # A cascading designation such as `... font-bold ...` continues its descent
        # down the tree unless overridden by something in *owned*.
        seq << designation
      end
    end

    owned.each do |designation|
      case designation
      in SelfDesignation, RootDesignation
      in CascadingDesignation, ItemDesignation
        seq << designation
      end
    end

    seq.to_compact_readonly_slice(&.itself)
  end

  private def instantiate!(codex : Codex, node : Term::Dict, mu : DirectedDesignationNode, dnflow : Slice(DownboundDesignation)) : {Outcome::Accepted(Term), Slice(UpboundDesignation)}
    properties = nil

    designations = pass do
      unless owned = mu.designations
        next {selfbound: {} of Term => Term::Dict,
              downbound: dnflow,
              upbound:   Slice(UpboundDesignation).empty}
      end

      # We'll do it the easy way ("trust me bro") instead of passing head through
      # the intermediate trees to prove the following is always true to the compiler.
      # During recognize, we only generate features if the head is a symbol, and
      # features end up as designations; so if there are designations, then there
      # were zero or more features, and thus, the head was a symbol.
      head = node[0].as_sym

      # We can't do this in designate() because designate() doesn't have access
      # to the node dict. We also don't want to nuke cache hits by letting it
      # know about the designate() dict. Some parts of Microfold are well-cached
      # while others aren't. We don't want all parts to be poorly cached.
      node.each_entry(in: Term::Dict.pairspart) do |key, value|
        next unless defn = codex.property?(key)

        properties ||= Set(Term).new
        properties << key

        settings = Term[].with(defn.dst, value)

        if codex.cascade?(defn.box)
          designation = Designation.new(defn.box, settings, :style)
          directed_designation = CascadingDesignation.new(designation, exceptions: Slice(Term::Sym).empty)
        elsif defn.box == SYM_ROOT_BOX
          directed_designation = RootDesignation.new(settings)
        else
          designation = Designation.new(defn.box, settings, :style)
          directed_designation = SelfDesignation.new(designation)
        end

        owned = owned.append(directed_designation)
      end

      {selfbound: selfbound(head, dnflow, owned),
       downbound: downbound(dnflow, owned),
       upbound:   upbound(owned)}
    end

    upflow = Slice(UpboundDesignation).empty

    instance_out = Outcome.accumulate do |acc|
      mu.diagnostics.each do |diagnostic|
        acc << diagnostic
      end

      removals = Pf::Kit.stack_array(Int32, 8)

      base = node.transaction do |commit|
        offset = 0
        mu_index = 0
        if mu.designations
          commit.without(:style)
          offset = 1
        end

        if properties
          properties.each { |key| commit.without(key) }
        end

        args = node.items.move(offset)
        args.each_with_index(offset) do |arg, key|
          # Instantiate text children, as in:
          #   (group
          #     "Line 1"
          #     "Line 2"
          #     "Line 3")
          unless child = arg.as_d?
            next unless mu.designations

            repr = arg
            unless arg.type.string?
              repr = Term.of(ML.compact(arg))
            end

            # Instantiate text-box.
            settings = designations[:selfbound][Term.of(:"text-box")]? || Term[]
            text = codex.instantiate(Term.of(:"text-box"), Term.of(node, repr), settings, mode: :call)

            # If there are designations for x-sel-box, we must broadcast them to
            # extra selections in *text*.
            pass do
              next unless x_sel_settings = designations[:selfbound][Term.of(:"x-sel-box")]?
              next unless x_sels = text[:selections]?.as_d?

              x_sels.items.each_with_index do |x_sel, x_sel_index|
                next unless x_sel = x_sel.as_d?

                text = Term.morph(text, {:selections, x_sel_index, Term.union(x_sel_settings, x_sel)})
              end
            end

            commit.with(key, text)
            next
          end

          next if mu.children.empty? # leaf

          # Instantiate all other children.
          mu_child = mu.children[mu_index]
          mu_index += 1

          if mu_child.nil? # style: "... absent ..."
            removals << key
            next
          end

          child_instance_out, child_upflow = instantiate(codex, child, mu_child, designations[:downbound])
          child_instance = acc.unwrap(child_instance_out.at(key))
          upflow += child_upflow
          commit.with(key, child_instance)
        end
      end

      # Append so that our upbound designations win over those of our children
      # (remember that designations are merged left-to-right).
      upflow = upflow + designations[:upbound]

      # Removals (`absent`) are uncommon, so we have a fast replacement pass followed
      # by a slower copy-without pass if removals are present.
      if removals.present?
        base = base.pairspart.transaction do |commit|
          base.items.each_with_index do |item, index|
            next if index.in?(removals)

            commit << item
          end
        end
      end

      if base.itemsize == 1 && base[0] != Term.of(:window) # ?!?!?!
        mode = Codex::InstantiateMode::Surround
      else
        mode = Codex::InstantiateMode::Nest
      end

      base = Term.of(base)

      # Instantiate the rest of boxes according to the hierarchy, bottom-up.
      codex.each_box_bottom_up do |box|
        next unless designation = designations[:selfbound][box]?

        base = codex.instantiate(box, base, designation.as_d, mode)
      end

      Outcome.ok(base)
    end

    {instance_out, upflow}
  end

  private def instantiate(codex : Codex, node : Term::Dict, mu : DirectedDesignationNode, dnflow : Slice(DownboundDesignation)) : {Outcome::Accepted(Term), Slice(UpboundDesignation)}
    codex.instantiate_cache.put_if_absent({node, mu, dnflow}) do
      instantiate!(codex, node, mu, dnflow)
    end
  end

  # Instantiates boxes in *node* according to *mu*, the directed designation
  # root node for *node*. Returns the resulting modified version of *node*.
  #
  # NOTE: This function assumes implicitly that *mu* is isomorphic to *node*.
  def instantiate(codex : Codex, node : Term::Dict, mu : DirectedDesignationNode) : Outcome::Accepted(Term)
    codex.instantiate_cache.epoch do
      instance_out, upflow = instantiate(codex, node, mu, dnflow: Slice(DownboundDesignation).empty)
      instance_out.map do |instance|
        # Process upbound designations targeting the root.
        upflow.each do |designation|
          case designation
          in RootDesignation
            instance = Term.merge(instance, Term.of(designation.settings))
          end
        end

        instance
      end
    end
  end
end
