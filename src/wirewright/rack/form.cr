# The implementation of the `rack.form` and `rack.field` nodes.
module Ww::Rack::Form
  extend self

  # :nodoc:
  SYM_FORM = Term.of(:form)

  # :nodoc:
  SYM_FIELD = Term.of(:field)

  # :nodoc:
  SYM_OUTLET = Term.of(:outlet)

  alias Field = OutletField | NamedField

  defrecord OutletField,
    addr : D7::NodeAddr,
    value : Term?,
    set : Term? -> Term,
    readonly : Bool,
    smart: true

  defrecord NamedField,
    addr : D7::NodeAddr,
    name : Term,
    value : Term?,
    set : Term? -> Term,
    readonly : Bool,
    smart: true

  def field?(node : D7::Node) : Field?
    nodeQ = node.term # so that the set() procs below capture only this and not the entire Node

    Term.case(nodeQ) do
      matchpi(
        %{[outlet (readonly _)]},
        %{[outlet (readonly _) _]},
      ) do
        value0 = nodeQ[2]?
        set = ->(value1 : Term?) do
          Term.morph(nodeQ, {2, value1})
        end
        OutletField.new(node.addr, value0, set, readonly: true)
      end

      matchpi(
        %{[outlet _]},
        %{[outlet _ _]},
      ) do
        value0 = nodeQ[2]?
        set = ->(value1 : Term?) do
          Term.morph(nodeQ, {2, value1})
        end
        OutletField.new(node.addr, value0, set, readonly: false)
      end

      matchpi(
        %{[field (readonly (%'edge name_))]},
        %{[field (readonly (%'edge name_)) _]},
        %{[field (readonly (_symbol (%'edge name_)))]},
        %{[field (readonly (_symbol (%'edge name_))) _]},
        %{[field (readonly @_ name_)]},
        %{[field (readonly @_ name_) _]},
        %{[field (readonly (_symbol @_) name_)]},
        %{[field (readonly (_symbol @_) name_) _]},
      ) do
        value0 = nodeQ[2]?
        set = ->(value1 : Term?) do
          Term.morph(nodeQ, {2, value1})
        end
        NamedField.new(node.addr, name, value0, set, readonly: true)
      end

      matchpi(
        # Short form
        %{[field (%'edge name_)]},
        %{[field (%'edge name_) _]},
        # Short form with policy
        %{[field (_symbol (%'edge name_))]},
        %{[field (_symbol (%'edge name_)) _]},
        # Full form exclusive
        %{[field (@_ name_)]},
        %{[field (@_ name_) _]},
        # Full form with policy
        %{[field ((_symbol @_) name_)]},
        %{[field ((_symbol @_) name_) _]},
      ) do
        value0 = nodeQ[2]?
        set = ->(value1 : Term?) do
          Term.morph(nodeQ, {2, value1})
        end
        NamedField.new(node.addr, name, value0, set, readonly: false)
      end

      otherwise { }
    end
  end

  alias Form = SingleFieldForm | MultiFieldForm

  # ```wwml
  # (form @form
  #   (outlet true))
  # ```
  defrecord SingleFieldForm,
    subtree : D7::GroupNode,
    field : Field,
    repr : Term?

  # ```wwml
  # (form @form
  #   (outlet "Field one")
  #   (outlet "Field two"))
  # ```
  defrecord MultiFieldForm,
    subtree : D7::GroupNode,
    fields : Array(Field),
    repr : Term

  def form(subtree : D7::GroupNode, fields : Array(Field), *, collapsible : Bool) : Form
    if collapsible && (field = fields.single?) && field.is_a?(OutletField)
      return SingleFieldForm.new(subtree, field, repr: field.value?)
    end

    conflicts = Set(Term).new
    active_indices = Pf::USet32.new

    repr = Term::Dict.build do |commit|
      fields.zip(0u32..fields.size.to_u32) do |field, index|
        case field
        in OutletField
          next unless value = field.value?

          commit << value
          active_indices = active_indices.add(index)
        in NamedField
          next if field.name.in?(conflicts)

          if field.name.in?(commit)
            commit.without(field.name)
            active_indices = active_indices.delete(index)
            next
          end

          commit.with(field.name, field.value?)
          active_indices = active_indices.add(index)
        end
      end
    end

    active_fields = Array(Field).new(active_indices.size)

    fields.zip(0u32...fields.size.to_u32) do |field, index|
      next unless active_indices.includes?(index)

      active_fields << field
    end

    MultiFieldForm.new(subtree, active_fields, Term.of(repr))
  end

  alias FieldAdjustment = SetField | UnsetField

  defrecord SetField, field : Field, value : Term
  defrecord UnsetField, field : Field

  # A lightweight prepass overload for reading only (*fn* can inspect the resulting
  # hypergraph but has no way to propose patches to it).
  def prepass(hg : D7::Hypergraph, &fn : D7::Hypergraph ->) : Nil
    unless hg.has_head?(SYM_FORM)
      fn.call(hg)
      return
    end

    form_ids, forms, replacements = collect(hg)
    fn.call(hg.gnd_map(replacements))
  end

  def prepass(hg : D7::Hypergraph, proposals : Array(D7::Patch), &fn : D7::Hypergraph, Array(D7::Patch) ->) : Nil
    unless hg.has_head?(SYM_FORM)
      fn.call(hg, proposals)
      return
    end

    form_ids, forms, replacements = collect(hg)

    buffer = [] of D7::Patch
    fn.call(hg.gnd_map(replacements), buffer)

    buffer.each do |proposal|
      # Passthrough of proposals which do not modify any form.
      unless proposal.intersects?(form_ids)
        proposals << proposal
        next
      end

      accepted = true

      proposal.each(in: form_ids) do |form_id, rep|
        form = forms[form_id]
        plan = [] of FieldAdjustment

        case form
        in SingleFieldForm
          Term.case(rep) do
            matchpi %{[cell _ modified_]} do
              if form.field.readonly
                accepted = false
                break
              end

              plan << SetField.new(form.field, modified)
            end

            matchpi %{[cell _]} do
              if form.field.readonly
                accepted = false
                break
              end

              plan << UnsetField.new(form.field)
            end

            otherwise do
              accepted = false
            end
          end
        in MultiFieldForm
          Term.case(rep) do
            matchpi %{[cell _ modified_dict]} do
              unless changes = Term.diff?(form.repr, modified, depth_limit: 1u32)
                accepted = false
                next
              end

              changes.each do |action|
                assert action.prefix.empty?

                case action
                in Term::Diff::WithItem
                  field = form.fields[action.index]

                  if field.readonly
                    accepted = false
                    break
                  end

                  plan << SetField.new(field, action.item)
                in Term::Diff::InsertItem
                  accepted = false
                  break
                in Term::Diff::DeleteItem
                  field = form.fields[action.index]

                  if field.readonly
                    accepted = false
                    break
                  end

                  plan << UnsetField.new(field)
                in Term::Diff::WithPair
                  field = form.fields.find do |field|
                    field.is_a?(NamedField) && field.name == action.key
                  end
                  # Some unknown pair was added, just ignore it.
                  next if field.nil?

                  if field.readonly
                    accepted = false
                    break
                  end

                  plan << SetField.new(field, action.value)
                in Term::Diff::WithoutPair
                  field = form.fields.find do |field|
                    field.is_a?(NamedField) && field.name == action.key
                  end
                  # Some unknown pair was removed, just ignore it.
                  next if field.nil?

                  if field.readonly
                    accepted = false
                    break
                  end

                  plan << UnsetField.new(field)
                end
              end
            end

            # Clearing the form cell can be intuitively interpreted as clearing all fields
            # ("we're done with all values in the fields, give us the next ones").
            matchpi %{[cell _]} do
              form.fields.each do |field|
                if field.readonly
                  accepted = false
                  break
                end

                plan << UnsetField.new(field)
              end
            end

            # This handles an odd edge case where a completely legitimate reference form
            # is replaced by the circuit by an atom, e.g. (form 1 2 3) -> 100; or something
            # similar. There is no point in doing that in practice. We reject such proposals.
            otherwise do
              accepted = false
            end
          end
        end

        break unless accepted

        guidance = D7.guidance(plan) do |adjustment|
          case adjustment
          in UnsetField
            {adjustment.field.addr, adjustment.field.set.call(nil)}
          in SetField
            {adjustment.field.addr, adjustment.field.set.call(adjustment.value)}
          end
        end

        # Proposal did form_id => (cell _ _), now we turn that into form_id => (form _ _*).
        proposal = proposal.assoc(form_id, D7.apply(form.subtree, guidance))
      end

      next unless accepted

      proposals << proposal
    end
  end

  private def collect(hg : D7::Hypergraph) : {Pf::USet32, Hash(UInt32, Form), Hash(D7::NodeAddr, D7::Gnd)}
    replacements = {} of D7::NodeAddr => D7::Gnd

    form_ids = Pf::USet32.new
    forms = {} of UInt32 => Form

    hg.each_node_with_head(SYM_FORM) do |node|
      nodeQ = node.term

      # [form _ _ _*]
      next unless nodeQ.itemsize >= 3

      _, subtree = hg.follow(node.addr)
      assert subtree.is_a?(D7::CircuitNode)
      leaf = subtree.leaf
      assert leaf.is_a?(D7::GndLeaf)
      subtree = subtree.to_group
      fields = fields(node.addr, subtree)

      header, form = Term.case(nodeQ) do
        matchpi %{[form @header_ _*]}, %{[form header←(_symbol @_) _*]} do
          {header, form(subtree, fields, collapsible: true)}
        end

        matchpi %{[form (@header_) _*]}, %{[form (header←(_symbol @_)) _*]} do
          {header, form(subtree, fields, collapsible: false)}
        end
      end

      assert forms.put?(node.id, form)
      form_ids = form_ids.add(node.id)

      # Replace the form node by a cell containing the form's representation.
      cell = Term.of(:cell, header, form.repr)
      replacements[node.addr] = D7.gnd(cell, leaf.feature.edges, merge_policy: leaf.feature.merge_policy)
    end

    {form_ids, forms, replacements}
  end

  private def fields(addr : D7::NodeAddr, subtree : D7::GroupNode) : Array(Field)
    fields = [] of Field

    hg = D7::Hypergraph.new(addr, subtree, level_query: D7::Hypergraph::AnyLevel.new)
    hg.each_node_with_head(SYM_FIELD, SYM_OUTLET, noentry: {SYM_FORM}) do |node|
      next unless field = field?(node)

      fields << field
    end

    fields
  end
end
