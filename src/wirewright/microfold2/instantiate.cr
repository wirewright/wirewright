module Ww::Microfold2
  alias UpboundDesignation = RootDesignation
  alias DownboundDesignation = ItemDesignation | CascadingDesignation

  private def selfbound(dnflow : Slice(DownboundDesignation), received : Slice(DirectedDesignation)) : Hash(Term, Term::Dict)
    seq = Pf::Kit.stack_array(Designation)

    dnflow.each do |downbound_designation|
      case downbound_designation
      in ItemDesignation
        seq << downbound_designation.payload.successor
      in CascadingDesignation
        seq << downbound_designation.successor
      end
    end

    received.each do |directed_designation|
      case directed_designation
      in SelfDesignation, CascadingDesignation
        seq << directed_designation.successor
      in ItemDesignation, RootDesignation
      end
    end

    # See `StyleOrigin`.
    seq.sort_by!(&.origin)

    designations = {} of Term => Term::Dict

    seq.each do |designation|
      settings0 = designations[designation.box]? || Term[]
      settings1 = Term.union(settings0, designation.settings)
      designations[designation.box] = settings1
    end

    designations
  end

  private def upbound(received : Slice(DirectedDesignation)) : Slice(UpboundDesignation)
    seq = Pf::Kit.stack_array(UpboundDesignation, 4)

    received.each do |directed_designation|
      case directed_designation
      in SelfDesignation, CascadingDesignation, ItemDesignation
      in RootDesignation
        seq << directed_designation
      end
    end

    seq.to_unsafe_readonly_slice!
  end

  private def downbound(dnflow : Slice(DownboundDesignation), received : Slice(DirectedDesignation)) : Slice(DownboundDesignation)
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
        # down the tree unless overridden by something in *received*.
        seq << designation
      end
    end

    received.each do |designation|
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
      unless received_designations = mu.designations
        next {selfbound: {} of Term => Term::Dict,
              downbound: dnflow,
              upbound:   Slice(UpboundDesignation).empty}
      end

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
          directed_designation = CascadingDesignation.new(designation)
        elsif defn.box == SYM_ROOT_BOX
          directed_designation = RootDesignation.new(settings)
        else
          designation = Designation.new(defn.box, settings, :style)
          directed_designation = SelfDesignation.new(designation)
        end

        received_designations = received_designations.append(directed_designation)
      end

      {selfbound: selfbound(dnflow, received_designations),
       downbound: downbound(dnflow, received_designations),
       upbound:   upbound(received_designations)}
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

            settings = designations[:selfbound][Term.of(:"text-box")]? || Term[]
            child_instance = codex.instantiate(Term.of(:"text-box"), Term.of(node, repr), settings, mode: :call)
            commit.with(key, child_instance)
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
