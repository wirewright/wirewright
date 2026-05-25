module MuSoma
  # *Perturbations* are the primitive unit of *entanglement*. *Entanglement*
  # is a process, and *perturbations* are actions through which entanglement
  # is carried out.
  alias Perturbation = GlobalPerturbation | TargetedPerturbation

  # Global perturbations do target any node in particular.
  alias GlobalPerturbation = UpdateMice | UpdateRefs | FinishWrites | Scheduler::Event

  # Targeted perturbations target a specific node, identified by its `addr`.
  alias TargetedPerturbation = UpdateInput

  defrecord UpdateMice,
    prev : Slice(MediaService::Mouse),
    now : Slice(MediaService::Mouse)

  defrecord UpdateRefs, ext : ExtrinsicMap
  defrecord FinishWrites, path : NormalPath, content : Term::Blob | Term::Str

  alias UpdateInput = UpdateFocus | UpdateKeyboardState

  defrecord UpdateFocus, addr : D7::NodeAddr, focus : Term
  defrecord UpdateKeyboardState, addr : D7::NodeAddr, keys : Pf::Set(Term)

  def perturb(tree : D7::ParseTree, plan : Enumerable(Perturbation)) : Term
    D7.perturb(tree) do |node, addr|
      plan.each do |perturbation|
        case perturbation
        in GlobalPerturbation
        in TargetedPerturbation
          next unless addr == perturbation.addr
        end

        node = perturb(node, perturbation)
      end

      node
    end
  end

  private def perturb(node : Term, action : UpdateKeyboardState) : Term
    head, *_ = node.items

    result = node.pairspart.transaction do |commit|
      commit << head
      commit.concat(action.keys)
    end

    Term.of(result)
  end

  private def perturb(node : Term, action : UpdateFocus) : Term
    Term.morph(node, {:focus, action.focus == Term.of(false) ? nil : action.focus})
  end

  private def perturb(node : Term, action : UpdateMice) : Term
    Term.case(node) do
      matchpi %{[mouse buttons_*]} do
        # NOTE: Currently, only one mouse is actually handled, even though we could
        # handle more. The API for this could be a `mouse-list` node which lists mouse
        # info including ids, and `mouse` with an `id: _` filter.
        mouse = action.now.first?

        result = node.pairspart.transaction do |commit|
          commit << :mouse
          if mouse.nil?
            if node.includes?(:anchor)
              commit.with(:anchor, :"?")
            end
            if node.includes?(:focus)
              commit.with(:focus, :"?")
            end
            if node.includes?(:mode)
              commit.with(:mode, :"?")
            end

            next
          end

          if node.includes?(:anchor)
            commit.with(:anchor, MuSoma.translate(mouse.position.anchor))
          end
          if node.includes?(:focus)
            commit.with(:focus, MuSoma.translate(mouse.position.focus))
          end
          if node.includes?(:mode)
            commit.with(:mode, MuSoma.translate(mouse.position.mode))
          end

          prev_mouse_state = MediaService::Mouse::State::None
          if prev_mouse = action.prev.find { |candidate| candidate.id == mouse.id }
            prev_mouse_state = prev_mouse.state
          end

          known = MediaService::Mouse::State.parse(buttons.items)
          pressed = mouse.state - prev_mouse_state
          released = prev_mouse_state - mouse.state
          present = (known - released) | pressed

          present.each do |button|
            commit << button.term
          end
        end

        Term.of(result)
      end

      otherwise do
        node
      end
    end
  end

  private def perturb(node : Term, action : UpdateRefs) : Term
    Term.case(node) do
      matchpi %{[path-report path_string _?]}, path: NormalPath do
        state = action.ext[ReportRef.new(path)]?

        Term.morph(node, {2, MuSoma.translate(state)})
      end

      matchpi %{[path-reading path_string _?]}, path: NormalPath do
        state = action.ext[ReadingRef.new(path)]?

        Term.morph(node, {2, MuSoma.translate(state)})
      end

      matchpi %{[resource term_ _?]} do
        continue unless query = ResourceService.query?(term)

        state = action.ext[ResourceRef.new(query)]?

        Term.morph(node, {2, MuSoma.translate(state)})
      end

      otherwise do
        node
      end
    end
  end

  private def perturb(node : Term, action : FinishWrites) : Term
    Term.case(node) do
      matchpi %{[file-sink path_string content_]}, path: NormalPath do
        continue unless path == action.path
        continue unless content == action.content

        # If a sink observed a write to its desired location, with its desired
        # content, then the write is complete, regardless of whether this particular
        # sink ordered the write.
        Term.morph(node, {2, nil})
      end

      otherwise do
        node
      end
    end
  end

  private def perturb(node : Term, action : Scheduler::Tick) : Term
    Term.case(node) do
      matchpi %{[ticker duration-term_ ±ticks]} do
        continue unless duration = MuSoma.duration?(duration_term)
        continue if duration.negative? # ?!
        continue unless action.period == duration

        Term.morph(node, {2, ticks + action.crossings})
      end

      matchpi %{[sequencer duration-term_ seq_+]} do
        continue unless duration = MuSoma.duration?(duration_term)
        continue if duration.negative? # ?!
        continue unless action.period == duration

        bits = [] of Bool

        seq.items.each do |item|
          Term.case(item) do
            matchpi %{(> _)} { bits << true }
            otherwise { bits << false }
          end
        end

        bits.rotate!(-action.crossings) # shl()

        result = node.transaction do |commit|
          seq.items.zip(bits, 2...node.itemsize) do |item, active, key|
            if active
              Term.case(item) do
                matchpi %{(> _)} { }

                otherwise do
                  commit.with(key, Term.of(:>, item))
                end
              end
            else
              Term.case(item) do
                matchpi %{(> arg_)} do
                  commit.with(key, arg)
                end

                otherwise { item }
              end
            end
          end
        end

        Term.of(result)
      end

      otherwise { node }
    end
  end

  private def perturb(node : Term, action : Scheduler::Expire) : Term
    node
  end
end
