# Implements the `supervisor` node.
module Ww::Rack::Supervisor
  extend self

  def step(& : Proposer -> T) : T forall T
    yield Proposer.new
  end

  struct Proposer
    def propose(hg : D7::Hypergraph, proposals) : Nil
      Supervisor.propose(hg, proposals)
    end
  end

  defrecord Standard,
    values : D7::AbsEdge,
    value : Term,
    pattern : Term,
    pool : D7::AbsEdge,
    template : Term

  def propose(hg : D7::Hypergraph, proposals)
    hg.propose(proposals, :supervisor) do |node|
      Term.matchpi?(node.term, %{[supervisor (@values_ @value_ pattern_ - @pool_) template_*]}) do
        variant = Standard.new(hg.resolve(node.addr, values), value, pattern, hg.resolve(node.addr, pool), template)
        step(hg, node, variant)
      end
    end
  end

  private def step(hg : D7::Hypergraph, node : D7::Node, variant : Standard) : D7::Patch?
    return unless pool = Rack.pool?(hg, variant.pool)

    # Empty the pool if the value cell is not found.
    unless values = Rack.cell?(hg, variant.values).try(&.value?).as_d?
      return D7.patch(pool.node, {2, Term[]})
    end

    item_buckets = {} of Term => Set(Term)

    values.items.each_with_index do |value, index|
      next unless key = key?(variant.pattern, value)

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

    pool.contents.items.each do |content|
      next unless member = member?(variant, content)

      pool_bucket = pool_buckets.put_if_absent(member.key) { Set(Term).new }
      pool_bucket << member.value
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
          instance_commit << {:cell, variant.value, assignment}
          instance_commit.concat(variant.template.items)
        end

        contents_commit << instance
      end
    end

    D7.patch(pool.node, {2, contents1})
  end

  defrecord MemberDevice, key : Term, value : Term, value_index : Int32

  def member?(variant : Standard, content : Term) : MemberDevice?
    Term.matchpi?(content, %{[device _*]}) do
      children = content.items.move(1)
      children.each_with_index(offset: 1) do |child, index|
        Term.matchpi?(child, %{[cell @edge_ proposal_]}) do
          next unless edge == variant.value
          next unless key = key?(variant.pattern, proposal)

          return MemberDevice.new(key, proposal, index)
        end
      end
    end
  end

  private def key?(pattern : Term, value : Term) : Term?
    return unless env = M1.match?(pattern, value)
    return unless env.size == 1

    _, key = env.ee.first
    key
  end
end
