# Implements the `ensemble` node.
# TODO: Write access (each member must have bidi access to item!!)
module Ww::Rack::Ensemble
  extend self

  def step(& : Proposer -> T) : T forall T
    yield Proposer.new
  end

  struct Proposer
    def propose(hg : D7::Hypergraph, proposals) : Nil
      Ensemble.propose(hg, proposals)
    end
  end

  defrecord Standard,
    values : D7::AbsEdge,
    value : Term,
    pattern : Term,
    pool : D7::AbsEdge,
    template : Term

  def propose(hg : D7::Hypergraph, proposals)
    hg.propose(proposals, :ensemble) do |node|
      Term.matchpi?(node.term, %{[ensemble (@values_ @value_ pattern_ - @pool_) template_*]}) do
        variant = Standard.new(node.resolve(values), value, pattern, node.resolve(pool), template)
        step(hg, node, variant)
      end
    end
  end

  defrecord Pool, node : D7::Node, contents : Term::Dict

  private def step(hg : D7::Hypergraph, node : D7::Node, variant : Standard) : D7::Patch?
    # Find the associated values cell.
    return unless values_cell = Rack.cell?(hg, variant.values)
    return unless values = values_cell.value?.as_d?

    # Find the associated pool cell.
    pools = Pf::Kit.stack_array(Pool, 1)
    hg.each_node_with_head(Term.of(:pool), memberof: {variant.pool}) do |node|
      Term.matchpiT?(node.term, %{[pool @_ contents_dict]}) do
        pools << Pool.new(node, contents)
      end
    end

    return unless pool = pools.single?

    item_buckets = {} of Term => Set(Term)

    values.items.each_with_index do |value, index|
      next unless env = M1.match?(variant.pattern, value)
      next unless env.size == 1

      _, key = env.ee.first
      item_bucket = item_buckets.put_if_absent(key) { Set(Term).new }
      item_bucket << value
    end

    assignments = {} of Term => Term

    item_buckets.each do |key, bucket|
      next unless bucket.size == 1

      assignment = bucket.first
      assignments[key] = assignment
    end

    pool_buckets = {} of Term => Set(Term)
    pool_indices = {} of Term => Array(Int32)

    pool.contents.items.each do |content|
      next unless member = member?(variant, content)

      pool_bucket = pool_buckets.put_if_absent(member.key) { Set(Term).new }
      pool_bucket << member.value

      index_bucket = pool_indices.put_if_absent(member.key) { [] of Int32 }
      index_bucket << member.key_index
    end

    contents1 = Term::Dict.build do |contents_commit|
      pool.contents.items.each do |content|
        Term.case(content) do
          matchpi %{[device _*]} do
            next unless member = member?(variant, content)
            # Do not add if this key was removed.
            next unless assignment = assignments[member.key]?

            contents_commit << Term.morph(content, {member.value_index, 2, assignment})
          end

          # Pass through things we don't understand.
          otherwise do
            contents_commit << content
          end
        end
      end

      # Append newly added keys.
      assignments.each do |key, assignment|
        next if pool_buckets.has_key?(key)

        instance = Term::Dict.build do |instance_commit|
          instance_commit << :device
          instance_commit << {:cell, {:edge, :key}, key}
          instance_commit << {:cell, variant.value, assignment}
          instance_commit.concat(variant.template.items)
        end

        contents_commit << instance
      end
    end

    D7.patch(pool.node, {2, contents1})
  end

  defrecord MemberDevice,
    key : Term,
    value : Term,
    key_index : Int32,
    value_index : Int32

  def member?(variant : Standard, content : Term) : MemberDevice?
    Term.matchpi?(content, %{[device _*]}) do
      key = value = nil

      children = content.items.move(1)
      children.each_with_index(offset: 1) do |child, child_key|
        Term.case(child) do
          matchpi %{[cell @key term_]} do
            key = {term: term, index: child_key}
          end

          matchpi %{[cell @edge_ proposal_]} do
            next unless edge == variant.value

            value = {term: proposal, index: child_key}
          end

          otherwise { }
        end
      end

      next unless key && value

      MemberDevice.new(key[:term], value[:term], key[:index], value[:index])
    end
  end
end
