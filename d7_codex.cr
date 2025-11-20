module ::Ww::D7::Codex
  extend self

  def classifier : Classifier
    cache = SyncCache(Term, Feature).new(512, preallocate: true)
    ->(node : Term) { cache.put_if_absent(node) { classify(node) } }
  end

  def classify(node : Term) : Feature
    Term.case(node) do
      matchpi %{[frag @_ _]} do
        D7.circuit(node, 2...3) do |node1|
          _, edge, value0 = node1

          D7.mixture(node, Term.of(:cell, edge, value0)) do |orig, view|
            Term.of_case(view) do
              matchpi %{(cell @_)} { orig.morph({2, nil}) }
              matchpi %{(cell @_ value1_)} { orig.morph({2, value1}) }
            end
          end
        end
      end

      matchpi %{[frag @edge_]} do
        D7.mixture(node, Term.of(:cell, edge)) do |orig, view|
          Term.of_case(view) do
            matchpi %{(cell @_)} { orig }
            matchpi %{(cell @_ value1_)} { orig.morph({2, value1}) }
          end
        end
      end

      matchpi %{[cell @edge_ _?]} do
        D7.gnd(node, D7.edge(edge, 1))
      end

      matchpi %{[feed @u_ @v_]} do
        D7.gnd(node, D7.edge(u, 1), D7.edge(v, 2))
      end

      matchpi %{[feed (%group edges (%past @_ min: 3))]} do |edges|
        defn = Term::Dict.build do |commit|
          commit << :group
          edges.items.each_cons_pair do |a, b|
            commit << Term.of(:feed, a, b)
          end
        end

        D7.mixture(node, defn) { |orig, _| orig }
      end

      matchpi %{[feed @u_ (over @v_)]} do
        D7.gnd(node, D7.edge(u, 1), D7.edge(v, 2, 1))
      end

      matchpi %{[feed (copy @u_) @v_]} do
        D7.gnd(node, D7.edge(u, 1, 1), D7.edge(v, 2))
      end

      matchpi %{[feed (copy @u_) (over @v_)]} do
        D7.gnd(node, D7.edge(u, 1, 1), D7.edge(v, 2, 1))
      end

      matchpi %{[feed @u_ (pulse @_)]} do
        D7.gnd(node, D7.edge(u, 1))
      end

      matchpi %{[feed (pulse @_) @u_]} do
        D7.gnd(node, D7.edge(u, 2))
      end

      matchpi %{[discard @edge_]} do
        D7.gnd(node, D7.edge(edge, 1))
      end

      matchpi %{[group _*]} do
        D7.parent(node, 1...node.itemsize)
      end

      matchpi %{[unit _*]} do
        D7.circuit(node, 1...node.itemsize) { |node1| D7.inert(node1) }
      end

      matchpi %{[delay (%number +i32) _]} do
        D7.gnd(node)
      end

      matchpi %{[module bindings_dict _*]} do
        D7.scope(bindings, D7.parent(node, 2...node.itemsize))
      end

      matchpi %{[transfer (@u_ _ @v_) _]} do
        D7.gnd(node, D7.edge(u, 1, 0), D7.edge(v, 1, 2))
      end

      matchpi %{[view (@u_ _ @v_) _]} do
        D7.gnd(node, D7.edge(u, 1, 0), D7.edge(v, 1, 2))
      end

      matchpi %{[view (@u_ _ (@v_ _)) _]} do
        D7.gnd(node, D7.edge(u, 1, 0), D7.edge(v, 1, 2, 0))
      end

      matchpi %{[log (@u_ _ @v_) _]} do
        D7.gnd(node, D7.edge(u, 1, 0), D7.edge(v, 1, 1))
      end

      matchpi %{[fb (@edge_ _) _]}, %{[fb (@edge_ _) _ _]} do
        D7.gnd(node, D7.edge(edge, 1, 0))
      end

      matchpi %{[queue (@front_ @back_ ⍊ min_: (%optional 1 (%number +i32!)) max_: (%optional ∞ (%any° (%number +i32!) ∞))) queue_dict]}, min: Int32 do
        defn = Term::Dict.build do |commit|
          commit << :group

          # Check if we are allowed to dequeue.
          if 0 < min <= queue.itemsize
            commit << Term.of(:cell, front, queue.items.first, front: true)
          end

          # Check if we are allowed to enqueue.
          if max == Term.of(:∞) || queue.itemsize < max.to(Int32)
            commit << Term.of(:cell, back, back: true)
          end
        end

        D7.mixture(node, Term.of(defn)) do |orig, view|
          rest = queue

          Term.case(view) do
            # Dequeue.
            matchpi %{⟨(cell @_ ⍊ front)⟩} do
              rest = rest.items.move(1).collect
              continue
            end

            # Sync.
            matchpi %{⟨(cell @_ x_ ⍊ front)⟩} do
              rest = rest.morph({0, x})
              continue
            end

            # Enqueue.
            matchpi %{⟨(cell @_ x_ ⍊ back)⟩} do
              rest = rest.append(x)
            end

            otherwise { }
          end

          Term.of(orig.morph({2, rest}))
        end
      end

      matchpi %{[sensor (_ _ @u_) _]} do
        D7.gnd(node, D7.edge(u, 1, 2))
      end

      matchpi %{[appearance _ @u_]} do
        D7.gnd(node, D7.edge(u, 2))
      end

      # |@ d7.node.chat
      #
      # |@pattern
      # [chat queue_dict children_*]
      #
      # |@key queue
      # The queue where the chat will store unread messages or messages that are
      # currently being read.
      #
      # |@key children d7.node.chat
      #
      # |@block
      # A `chat` node serves as a "chat" for its *children*. Children of a chat may send
      # messages to the chat's *queue*, and the chat itself is responsible for allowing other
      # children (or the same ones) to read them, one per tick, starting from the front.
      #
      # This particular form of the chat node is *synchronous*. It will prevent enqueues
      # that can be meaningfully prevented until the message queue is empty (i.e., through
      # backpressure rather than loss of data).
      #
      # Pulse messages are not dequeued until they are acknowledged ("read"). All other
      # messages are dequeued after one tick.
      matchpi %{[chat queue0_dict _*]} do
        D7.chat(node, queue0.itemspart, D7.parent(node, 2...node.itemsize), asc: queue0[:asc]? || Term.of({:"%never"}), desc: queue0[:desc]? || Term.of({:"%never"}), enq: queue0.itemsize.zero?) do |node1, queue1|
          Term.of(node1.morph({1, queue1 | queue0.pairspart}))
        end
      end

      # |@ d7.node.chat
      #
      # |@pattern
      # [(async chat) queue_dict children_*]
      #
      # |@key queue
      # The queue where the chat will store unread messages or messages that are
      # currently being read.
      #
      # |@key children d7.node.chat
      #
      # |@block
      # A `chat` node serves as a "chat" for its *children*. Children of a chat may send
      # messages to the chat's *queue*, and the chat itself is responsible for allowing other
      # children (or the same ones) to read them, one per tick, starting from the front.
      #
      # This is the *asynchronous* form of the chat node. It always allows enqueues.
      #
      # Pulse messages are not dequeued until they are acknowledged ("read"). All other
      # messages are dequeued after one tick.
      matchpi %{[(async chat) queue0_dict _*]} do
        D7.chat(node, queue0.itemspart, D7.parent(node, 2...node.itemsize), asc: queue0[:asc]? || Term.of({:"%never"}), desc: queue0[:desc]? || Term.of({:"%never"}), enq: true) do |node1, queue|
          Term.of(node1.morph({1, queue | queue0.pairspart}))
        end
      end

      matchpi %{[blast @u_ (pulse @_)]} do
        D7.gnd(node, D7.edge(u, 1))
      end

      matchpi %{[uniq @u_ (pulse @_)]} do
        D7.gnd(node, D7.edge(u, 1))
      end

      matchpi %{[uniq (@u_ _) (pulse @_)]} do
        D7.gnd(node, D7.edge(u, 1, 0))
      end

      matchpi %{[(%any ml/term ml/terms ml/document) @_ (result @_)]} do
        D7.gnd(node)
      end

      matchpi %{[m1/pattern @_ @_]} do
        D7.gnd(node)
      end

      matchpi %{[m1/backmap @_ (optional @_)]} do
        D7.gnd(node)
      end

      matchpi %{[repr (result @_) (ok @_)]} do
        D7.gnd(node)
      end

      otherwise { D7.inert(node) }
    end
  end

  def patch(grp : D7::Regime::NodeCaptureGroup, &)
    mem = Pointer({UInt32, Reaction}).malloc(grp.size)
    cursor = 0

    grp.each do |id, capture|
      mem[cursor] = {id, Reaction.new(Term.of(yield capture.node), Term[])}
      cursor += 1
    end

    Slice({UInt32, Reaction}).new(mem, grp.size, read_only: true)
  end

  def emit(grp : D7::Regime::NodeCaptureGroup, msgs : Enumerable(Term))
    unless grp.size == 1
      raise ArgumentError.new
    end

    id, capture = grp.first

    Slice[{id, Reaction.new(capture.node, Term[msgs])}]
  end

  def patches(*lists)
    size = lists.sum(&.size)
    mem = Pointer({UInt32, Reaction}).malloc(size)

    cursor = 0
    lists.each do |list|
      list.each do |el|
        mem[cursor] = el
        cursor += 1
      end
    end

    Slice({UInt32, Reaction}).new(mem, size, read_only: true)
  end

  def first(grp : D7::Regime::NodeCaptureGroup) : D7::Regime::NodeCapture
    grp.first[1]
  end

  def id(grp : D7::Regime::NodeCaptureGroup) : D7::NodeId
    grp.first[0]
  end

  def first(grp : D7::Regime::NodeCaptureGroup, key)
    grp.leftmost? { |_, capture| capture.env[key] } || raise Enumerable::EmptyError.new
  end

  def first(grp : D7::Regime::NodeCaptureGroup, *keys)
    keys.map { |key| first(grp, key) }
  end

  def chat(grp : D7::Regime::NodeCaptureGroup)
    first(grp).chat
  end

  module Sink::Bag
  end

  def collect(grp : D7::Regime::NodeCaptureGroup, key, sink : Sink::Bag.class, &)
    Term::Dict.build do |commit|
      grp.each do |_, capture|
        orig = capture.env[key]
        next unless value = yield orig

        commit.with(value, (commit[value]? || 0) + 1)
      end
    end
  end

  def collect(grp, key, sink)
    collect(grp, key, sink, &.itself)
  end

  def tick
    ->(node : Term, chat : NodeChat) do
      Term.case({node, chat.msg}) do
        givenpi %{[head←(%any ml/term ml/terms ml/document) @srcs_ (result @dsts_)] (pulse @srcs_ ml_string)}, ml: String do
          begin
            case head
            when Term.of(:"ml/term")
              term = ML.term(ml)
            when Term.of(:"ml/terms")
              term = ML.terms(ml)
            when Term.of(:"ml/document")
              term = ML.document(ml)
            else
              unreachable
            end

            result = Term.of(:ok, term)
          rescue e : ML::SyntaxError
            excerpt, line, column = ML::SyntaxError.lookaround(e.text)

            result = Term.of(:err,
              tags: {ml: true},
              detail: e.detail,
              excerpt: excerpt,
              line: line,
              column: column,
              "byte-start": e.text.byte_start,
              "byte-end": e.text.byte_end,
            )
          end

          D7.rxn(node, {:ack, {:pulse, dsts, result}})
        end

        givenpi %{[m1/pattern @submissions_ @envlists_] (pulse @submissions_ {¦ pattern_ matchee_ env⋮ {}})} do
          envlist = M1.matches(pattern, matchee, env: env.as_d)

          D7.rxn(node, {:ack, Term.of(:pulse, envlists, Term[envlist])})
        end

        givenpi %{[m1/backmap @submissions_ (optional @terms_)] (pulse @submissions_ {¦ pattern_ backspec_ matchee_ env⋮ {}})} do
          if term = M1.backmap?(pattern, backspec, matchee, env: env.as_d)
            D7.rxn(node, {:ack, {:pulse, terms, {:some, term}}})
          else
            D7.rxn(node, {:ack, {:pulse, terms, :none}})
          end
        end

        givenpi %{[repr (result @results_) (ok @oks_)] (pulse @results_ (ok term_))} do
          D7.rxn(node, {:ack, {:pulse, oks, term}})
        end

        givenpi %{[repr (result @results_) (ok @_)] (pulse @results_ err←[err])} do
          D7.rxn(node, {:ack, err})
        end

        givenpi %{[delay 1 child_] _} do
          D7.rxn(child)
        end

        givenpiT %{[delay n←(%number +i32!) _] _} do
          D7.rxn(Term.of(node.morph({1, n - 1})))
        end

        otherwise do
          D7.rxn(node)
        end
      end
    end
  end

  def tspace
    sensors = [] of {Term, D7::NodeAddr, (Term::Dict -> Term)}
    appearances = [] of {Term, D7::NodeAddr, (-> Term)}

    regime = D7.regime do
      # A sensor-cell complex acts as a "sensor channel", meaning the sensor finds a
      # suitable appearance and moves its value @edge.
      rule %{(one dev [sensor (tspace_ pattern_ @edge_) template_]) (one mem [cell @edge_])} do
        tspace, pattern, template = first(dev, :tspace, :pattern, :template)
        cell = first(mem)
        patch = ->(env : Term::Dict) do
          Term.of(cell.node.morph({2, Alloy.render(env, template)}))
        end
        sensors << {pattern, cell.addr, patch}

        nil # No patches
      end

      # An appearance-cell complex acts as an "appearance channel", meaning the appearance
      # finds suitable sensor-cell complex(es) and moves its value to their cells.
      rule %{(one dev [appearance tspace_ @edge_]) (one mem [cell @edge_ value_])} do
        tspace = first(dev, :tspace)
        value = first(mem, :value)
        cell = first(mem)

        patch = -> do
          Term.of(cell.node.morph({2, nil}))
        end

        appearances << {value, cell.addr, patch}

        nil # No patches
      end
    end

    ->(clf : D7::Classifier, circuit : Term) do
      begin
        _, circuit1 = D7.unit(Top.new(Term[]), circuit) do |unit|
          reactions = D7.relate(D7.fold_context(clf), regime, D7.parent(unit, 1...unit.itemsize), subcircuits: true)
          assert reactions.empty?

          patches = {} of D7::NodeAddr => Term

          appearances.each do |matchee, srcaddr, take|
            sent = false

            sensors.each do |pattern, dstaddr, put|
              next unless env = M1.match?(pattern, matchee)

              patches[dstaddr] = put.call(env)
              sent = true
            end

            next unless sent

            patches[srcaddr] = take.call
          end

          D7.fold(D7.fold_context(clf), unit) do |ctx, feature, rec, default|
            case feature
            when Gnd
              if patch = patches[ctx.addr]?
                D7.rxn(D7.unscope(ctx, patch))
              else
                D7.rxn(feature.node)
              end
            when Chat    then D7.fold(ctx, feature.cont, rec)
            when Circuit then D7.fold(ctx, D7.parent(feature.node, feature.range), rec)
            else
              default.call
            end
          end
        end

        circuit1
      ensure
        sensors.clear
        appearances.clear
      end
    end
  end

  def regime
    D7.regime do
      rule %{(one dev [feed @src_ (pulse @srcs_)]) (one src [cell @src_ x_])} do
        next unless chat(dev).enq

        srcs, x = first(dev, :srcs), first(src, :x)

        patches(
          emit(dev, {Term.of(:pulse, srcs, x)}),
          patch(src, &.morph({2, nil})),
        )
      end

      rule %{(one dev [feed (pulse @srcs_) @dst_]) (one dst [cell @dst_])} do
        srcs = first(dev, :srcs)

        Term.matchpi?(chat(dev).msg, %{(pulse @srcs_ msg_)}, env: Term[srcs: srcs]) do
          patches(
            emit(dev, {Term.of(:ack)}),
            patch(dst, &.morph({2, msg})),
          )
        end
      end

      rule %{(one dev [blast @src_ (pulse @els_)]) (one src [cell @src_ list_dict])} do
        next unless chat(dev).enq

        els, list = first(dev, :els), first(src, :list)

        grp = Term::Dict.build do |commit|
          commit << :group
          list.items.each do |el|
            commit << {:pulse, els, el}
          end
        end

        patches(
          emit(dev, {Term.of(:ack), Term.of(grp)}),
          patch(src, &.morph({2, nil})),
        )
      end

      rule %{(one dev [uniq @src_ (pulse @srcs_)]) (one src [cell @src_ term_])} do
        src_edge, srcs, term = first(dev, :src), first(dev, :srcs), first(src, :term)

        Slice[{id(dev), Reaction.new(Term.of(first(dev).node.morph({1, {src_edge, term}})), Term[{ {:pulse, srcs, term} }])}]
      end

      rule %{(one dev [uniq (@src_ term_) (pulse @srcs_)]) (one src [cell @src_ term_])} do
        src_edge, srcs, term0, term1 = first(dev, :src), first(dev, :srcs), first(dev, :term), first(src, :term)
        next if term0 == term1

        Slice[{id(dev), Reaction.new(Term.of(first(dev).node.morph({1, {src_edge, term1}})), Term[{ {:pulse, srcs, term1} }])}]
      end

      rule %{(one dev [log (@src_ pattern_ @log_) template_]) (one src [cell @src_ input_]) (one log [cell @log_ entries_dict])} do
        pattern, template, input, entries = first(dev, :pattern), first(dev, :template), first(src, :input), first(log, :entries)
        next unless vars = M1.match?(pattern, input)

        entry = Alloy.render(vars, template)
        next if entry == entries.items.last?

        patch(log, &.morph({2, entries.append(entry)}))
      end

      # This rule handles the following situations:
      #
      #   Unicast (move to dst):
      #     (cell @src <value>)
      #     (feed @src @dst)
      #     (cell @dst)
      #
      #   Multicast:
      #     (cell @src <value>)
      #     (feed @src @dst)
      #     (cell @dst)
      #     (cell @dst)
      #     (cell @dst)
      #     ;; ...
      #
      #   Aggregate:
      #     (cell @src <value0>)
      #     (cell @src <value1>)
      #     (cell @src <value2>)
      #     ;; ...
      #     (feed @src @dst)
      #     (cell @dst)
      rule(
        %{(one dev [feed @src_ @dst_]) (many src [cell @src_ x_]) (many dst [cell @dst_])},
        %{(one dev [feed @src_ (over @dst_)]) (many src [cell @src_ x_]) (many dst [cell @dst_ _?])},
      ) do
        case {src.size, dst.size}
        when {1, _} # distribute
          x = first(src, :x)

          patches(
            patch(src, &.morph({2, nil})),
            patch(dst, &.morph({2, x})),
          )
        when {_, 1} # aggregate
          x = collect(src, :x, Sink::Bag)

          patches(
            patch(src, &.morph({2, nil})),
            patch(dst, &.morph({2, x})),
          )
        end

        # TODO: What to do on M:M? Move srcs to dsts? What if there are less dsts
        # that srcs? Who wins? What's the order?
      end

      # This rule handles the following situation:
      #
      #   (cell @src0 <value0>)
      #   (cell @src1 <value1>)
      #   (cell @src2 <value2>)
      #   ;; ...
      #   (feed @src0 @dst)
      #   (feed @src1 @dst)
      #   (feed @src2 @dst)
      #   ;; ...
      #   (cell @dst)
      #
      # This is nondeterministic in principle. We resolve the conflict deterministically from
      # dst's point of view by picking an src edge that is the lexicographic minimum among all
      # available src edges.
      rule %{(one dst [cell @dst_]) (link (many mid [feed @src_ @dst_] min: 2) (many src [cell @src_ x_]))} do
        id, capture = src.min_by { |_, capture| ML.compact(capture.env[:src]) }
        x = capture.env[:x]

        patches(
          Slice[{id, Reaction.new(Term.of(capture.node.morph({2, nil})), Term[])}],
          patch(dst, &.morph({2, x})),
        )
      end

      rule(
        %{(one dev [feed (copy @src_) @dst_]) (many src [cell @src_ x_]) (many dst [cell @dst_])},
        %{(one dev [feed (copy @src_) (over @dst_)]) (many src [cell @src_ x_]) (many dst [cell @dst_ _?])},
      ) do
        case {src.size, dst.size}
        when {1, _} # distribute
          x = first(src, :x)

          patch(dst, &.morph({2, x}))
        when {_, 1} # aggregate
          x = collect(src, :x, Sink::Bag)

          patch(dst, &.morph({2, x}))
        end
      end

      rule %{(one dev [discard @tgt_]) (many tgt [cell @tgt_ _])} do
        patch(tgt, &.morph({2, nil}))
      end

      rule %{(one dev [transfer (@src_ pattern_ @dst_) template_]) (one src [cell @src_ x_]) (one dst [cell @dst_])} do
        pattern, template = first(dev, :pattern, :template)

        case {src.size, dst.size}
        when {1, _} # distribute
          x = first(src, :x)
          next unless vars = M1.match?(pattern, x)

          instance = Alloy.render(vars, template)

          patches(
            patch(src, &.morph({2, nil})),
            patch(dst, &.morph({2, instance})),
          )
        when {_, 1} # aggregate
          instances = collect(src, :x, Sink::Bag) do |x|
            next unless vars = M1.match?(pattern, x)

            Alloy.render(vars, template)
          end

          patches(
            patch(src, &.morph({2, nil})),
            patch(dst, &.morph({2, instances})),
          )
        end
      end

      # If src changes, calculate dst.
      rule %{(one dev [view (@src_ pattern_ @dst_) template_]) (one src [cell @src_ x_]) (one dst [cell @dst_ _?])} do
        pattern, template = first(dev, :pattern, :template)
        x = first(src, :x)
        next unless vars = M1.match?(pattern, x)

        instance = Alloy.render(vars, template)

        patch(dst, &.morph({2, instance}))
      end

      # If src disappears, dst disappears.
      rule %{(one dev [view (@src_ _ @dst_) _]) (one src [cell @src_]) (one dst [cell @dst_ _])} do
        patch(dst, &.morph({2, nil}))
      end

      rule %{(one dev [fb (@edge_ pattern_) backspec_]) (one tgt [cell @edge_ value0_])} do
        pattern, backspec = first(dev, :pattern, :backspec)
        value0 = first(tgt, :value0)
        next unless value1 = M1.backmap?(pattern, backspec, value0)

        patch(tgt, &.morph({2, value1}))
      end

      rule %{(one dev [fb (@edge_ pattern_) template_ backspec_]) (one tgt [cell @edge_ value0_])} do
        pattern, template, backspec = first(dev, :pattern, :template, :backspec)
        value0 = first(tgt, :value0)
        next unless env = M1.match?(pattern, value0, backpaths: true)
        next unless backpaths = env[:"(backpaths)"]?

        env1 = Alloy.render(env.without(:"(backpaths)"), template)
        next unless env1.type.dict?

        value1 = M1.backmap({env1.with(:"(backpaths)", backpaths)}, backspec, value0)

        patch(tgt, &.morph({2, value1}))
      end
    end
  end
end
