module MuSoma
  alias Msg = MediaService::WindowDescriptionChanged | ReloadRefs | WriteFinished | Scheduler::Event | DatabaseTaskCompleted
  alias Plan = Set(Perturbation)

  defrecord ReloadRefs

  # Emitted for each term in state's `requests: (_*)`.
  defrecord AppRequest, term : Term

  defrecord WriteFinished, path : NormalPath, content : Term::Blob | Term::Str

  defrecord DatabaseTaskCompleted, task : DatabaseAgent::Task, result : Term

  struct EntangleContinuation
    def initialize(@cont : (Term, D7::NodeAddr ->) ->)
    end

    def self.entangle(workspace, plan, agent, &fn : (Term, D7::NodeAddr ->) ->)
      _ = agent.entangle?(workspace, plan, new(fn))
    end

    def self.entangle(workspace, plan, agent, *agents, &fn : (Term, D7::NodeAddr ->) ->)
      cont = ->(first : (Term, D7::NodeAddr ->)) do
        entangle(workspace, plan, *agents) do |rest|
          composite = ->(feature : Term, addr : D7::NodeAddr) do
            first.call(feature, addr)
            rest.call(feature, addr)
          end
          fn.call(composite)
        end
      end

      case agent.entangle?(workspace, plan, new(cont))
      in true  # Executed the continuation
      in false # Did not
        entangle(workspace, plan, *agents, &fn)
      end
    end

    def each_with_addr(&fn : Term, D7::NodeAddr ->)
      @cont.call(fn)
    end
  end

  class EditorAgent
    def initialize(@codex_ref : ReadingRef)
      @seen = Bytes.empty
      @editR = Rho.noR
    end

    def boot(ws : Workspace) : Nil
      ws.extrinsics.add(@codex_ref)
    end

    def sync(ws : Workspace) : Nil
      return unless reading = ws.extrinsics[@codex_ref]?

      sync(ws, reading)
    end

    private def sync(ws : Workspace, reading : PathService::ContentReading) : Nil
      return if @seen == reading.blob.digest

      ws.console.send(InfoLog.new("Reload editR"))

      @seen = reading.blob.digest

      begin
        document = ML.document(reading.blob.to_string)
      rescue e : ML::SyntaxError
        # TODO: show what the error is!!
        ws.console.send(ErrLog.new("editR on disk is invalid, running from memory..."))
      else
        @editR = Rho.rewriter(document)
      end
    end

    private def sync(ws : Workspace, reading : PathService::DigestReading | PathService::Absent) : Nil
      ws.console.send(ErrLog.new("editR on disk is absent, running from memory..."))
    end

    def step(ws : Workspace) : Nil
      ws.state.update do |state|
        # If the circuit is running, and we are at the end of history, accept
        # motions and rewrite the circuit.
        #
        # If the circuit is paused and we are at the end of history, allow to edit it.
        #
        # Otherwise, discard motions and do not edit. The UI should prevent
        # this from ever being reached, with a helpful message. This is simply
        # our last line of defense.
        Term.matchpi?(state, %{{¦ motions_: (_+) timeline: (behind←(_*) I * status←(%any ... -) draft_)}}) do |draft|
          motions.items.each do |motion|
            draft = MuSoma.dispatch(draft, MuSoma.motion(motion))
          end

          draft = Rho.rewrite(@editR, draft)
          state = state
            .with(:motions, Term[])
            .with(:timeline, {behind, :I, :*, status, draft})
        end

        state
      end
    end
  end

  # Pretty-printing and interaction with the pretty-printed circuit snapshot
  # or draft. Contributes and maintains the `circuit-µ` variable in the state.
  class PrettyAgent
    def initialize
      @cache = GenerationalCache(Term, D7::UnaugmentedParseTree).new
    end

    # The feedback classifier is used for pretty-printing nodes in the circuit
    # and for delivering user interaction feedback back (such as clicking
    # the "expand" button).
    #
    # Some nodes are classified differently during pretty-printing / fbR compared
    # with step. For example, some visually closed parent nodes (e.g. `section`)
    # are classified as ground nodes to avoid pointless processing their insides.
    #
    # Moreover, some nodes are mixtures, so they are "ephemeral": they aren't actual
    # nodes, just "bidi macros". That's a problem if we want to deliver something
    # to such a node, or, more importantly, to something inside it; and not the thing(s)
    # it expands to. For this reason fbclf re-classifies such nodes as ground or
    # parent nodes, if possible. If this is impossible, then that node can't be
    # interactive; it's as simple as that.
    def self.fbclf : D7::Classifier
      successor = MuSoma.clf

      ->(node : Term) do
        feature = successor.call(node)

        Term.case(node) do
          matchpi %{(slot _ _? ⍊ -open)} do
            continue if feature.is_a?(D7::Inert)
            continue if MuSoma.editing?(node)

            D7.gnd(node)
          end

          matchpi %{[slot _ _?]} do
            continue if feature.is_a?(D7::Inert)

            D7.parent(node.as_d, 2u32...node.uitemsize)
          end

          matchpi %{[frag @_ _]} do
            continue if feature.is_a?(D7::Inert)

            D7.parent(node.as_d, 2u32...node.uitemsize)
          end

          matchpi %{(section _string _* ⍊ open)} do
            continue if feature.is_a?(D7::Inert)

            D7.parent(node.as_d, 2u32...node.uitemsize)
          end

          matchpi %{(section _string _* ⍊ -open)} do
            continue if feature.is_a?(D7::Inert)
            continue if MuSoma.editing?(node)

            D7.gnd(node)
          end

          matchpi %{(rule _ _ ⍊ open)} do
            D7.parent(node.as_d, 2u32...3u32)
          end

          matchpi %{(rule _ _ ⍊ -open)} do
            continue if MuSoma.editing?(node)

            D7.gnd(node)
          end

          matchpi %{[rewriter _*]} do
            continue if feature.is_a?(D7::Inert)

            D7.gnd(node)
          end

          otherwise do
            feature
          end
        end
      end
    end

    private def self.repr(tree : D7::InertLeaf, addr : D7::NodeAddr) : Term
      repr = Term::Dict.build do |commit|
        commit << :inert << tree.feature.node

        tree.feature.annotations.each do |ann|
          case ann
          when .incomplete? then commit.with(:incomplete, true)
          end
        end
      end

      Term.of(repr)
    end

    private def self.repr(tree : D7::GndLeaf, addr) : Term
      node = tree.feature.node

      Term.case(node) do
        matchpi %{(backsys header_dict _* ⍊ -open)} do
          Term.of(:"closed-backsys-widget", addr, header)
        end

        matchpi %{(backsys _* ⍊ open)} do
          Term.of(:"open-backsys-widget", addr, node)
        end

        matchpi %{(rewriter header_dict _* ⍊ -open)} do
          Term.of(:"closed-rewriter-widget", addr, header)
        end

        matchpi %{(rewriter _* ⍊ open)} do
          Term.of(:"open-rewriter-widget", addr, node)
        end

        matchpi %{(slot call_ _ ⍊ -open)} do
          Term.of(:"closed-slot-widget", addr, call)
        end

        matchpi %{[path-reading path_string _]} do
          Term.of(:"path-reading-widget", addr, node)
        end

        matchpi %{[path-report path_string _]} do
          Term.of(:"path-report-widget", addr, node)
        end

        matchpi %{(section title_string _* ⍊ -open)} do
          Term.of(:"closed-section-widget", addr, title)
        end

        matchpi %{(rule pattern_ _ ⍊ doc⋮ () -open)} do
          Term.of(:"closed-rule-widget", addr, doc, pattern)
        end

        otherwise do
          Term.of(:gnd, node)
        end
      end
    end

    private def self.repr(tree : D7::UnaugmentedParentNode, addr) : Term
      parent = tree.feature

      repr = parent.node.pairspart.transaction do |commit|
        commit << :parent

        parent.node.items.each_with_index do |item, key|
          key = key.to_u32

          unless key.in?(parent.range)
            commit << Term.of(:inert, item)
            next
          end

          index = key - parent.range.begin
          child = tree.children[index]

          commit << repr(child, addr.append(key))
        end
      end

      Term.case(parent.node) do
        matchpi %{[slot _ _]} do
          Term.of(:"open-slot-widget", addr, repr)
        end

        matchpi %{[section title_string _*]} do
          Term.of(:"open-section-widget", addr, title, repr)
        end

        matchpi %{(rule _ _ ⍊ doc⋮ () open)} do
          Term.of(:"open-rule-widget", addr, doc, repr)
        end

        otherwise do
          Term.of(repr)
        end
      end
    end

    # Returns the representation tree for *tree*. This tree is ready for
    # pretty-printing.
    def self.repr(tree : D7::UnaugmentedParseTree) : Term
      repr = repr(tree, D7::NodeAddr.empty)

      # Mark the topmost parent as root for styling in prettyR.
      Term.matchpi(repr, %{[parent _*]}) do
        Term.morph(repr, {0, :root})
      end
    end

    def present(ws : Workspace) : Nil
      force = Var.pending?(ws.codex)
      return unless Var.pending?({ws.state, :timeline}, or_if: force)

      ws.state.update do |state|
        present(ws.codex.get.pretty, state, force)
      end
    end

    @seen_circuit : Term?
    @seen_repr : Term?

    def present(pretty : Codex::Pretty, state : Term::Dict, force : Bool) : Term::Dict
      circuit = Term.case(state) do
        # Show the draft if at end-of-history.
        matchpi %{{¦ timeline: (_ I * _ draft_)}} do
          draft
        end

        # Show the current snapshot if navigating history.
        matchpi %{{¦ timeline: ((_* snapshot_) I _ _ _)}} do
          snapshot
        end
      end

      # Fast path if the circuit did not change.
      if !force && @seen_circuit == circuit
        return state
      end

      @seen_circuit = circuit

      tree = @cache.epoch do
        D7.parse(PrettyAgent.fbclf, circuit, reply: D7::UnaugmentedParseTree, cache: @cache)
      end

      repr = PrettyAgent.repr(tree)

      # Fast path if the representation did not change. This accounts for
      # things like folds (e.g. `section`, `slot`). If something is inside
      # a fold, and it changed, then we don't care, because no one will see
      # it anyway.
      if !force && @seen_repr == repr
        return state
      end

      @seen_repr = repr

      print = pretty.rewrite(repr)

      state.with(:"circuit-µ", print)
    end

    # FIXME: refactor
    def step(ws : Workspace) : Nil
      fb = ws.state.get[:fb]
      return if fb.empty?

      fb_dsts = fb.items.compact_map do |call|
        Term.matchpi?(call, %{(dst←((%past (%number +i32))) msg_)}) do
          {D7::NodeAddr.new(dst.items, &.to(UInt32)), msg}
        end
      end

      ws.state.set(ws.state.get.with(:fb, Term[]))

      Term.case(ws.state.get) do
        # Interact with draft.
        #
        # Note that regardless of what we do, we cannot *really* interact with the live draft.
        # We are always competing with time.
        matchpi %{{¦ timeline: (_ I * _ draft_)}} do
          feature_tree = @cache.epoch do
            D7.parse(PrettyAgent.fbclf, draft, cache: @cache, reply: D7::UnaugmentedParseTree)
          end

          draft1 = D7.perturb(feature_tree) do |node, addr|
            fb_dsts.each do |dst, msg|
              next unless addr == dst

              # if fbR probably rewrites node, msg (noalloc!):
              #   ...
              node, _ = Rho.rewrite(ws.codex.get.fbR, Term.of(node, msg))
            end

            node
          end

          ws.state.set(Term.morph(ws.state.get, {:timeline, 4, draft1}))
        end

        # Interact with a past version of the circuit.
        matchpi %{{¦ timeline: (behind←(_* current_dict) I _dict _ _)}} do
          feature_tree = @cache.epoch do
            D7.parse(PrettyAgent.fbclf, current, cache: @cache, reply: D7::UnaugmentedParseTree)
          end

          new_current = D7.perturb(feature_tree) do |node, addr|
            fb_dsts.each do |dst, msg|
              next unless addr == dst

              # if fbR probably rewrites node, msg (noalloc!):
              #   ...
              node, _ = Rho.rewrite(ws.codex.get.fbR, Term.of(node, msg))
            end

            node
          end

          ws.state.set(Term.morph(ws.state.get, {:timeline, 0, behind.itemsize - 1, new_current}))
        end

        otherwise { }
      end
    end
  end

  class DistillAgent
    @seen : {Term, Bool}?
    @mu : Microfold::SyncCodex?

    def present(ws : Workspace)
      force = false

      # Fork off of new Microfold codices. We could just use the codex at mu_var,
      # but then we'd be sharing its generational caches with everyone else, which
      # would almost always mean everyone loses.
      if Var.pending?(ws.mu_codex)
        @mu = ws.mu_codex.get.fork

        force = true
      end

      assert mu = @mu

      if Var.pending?({ws.state, :rem})
        force = true
      end

      return unless Var.pending?(ws.codex, {ws.state, :timeline}, {ws.state, :hide}, or_if: force)

      frame = Term.case(ws.state.get) do
        # Show the draft if at end-of-history.
        matchpi %{{¦ hide_boolean timeline: (_ I * _ draft_)}} do
          {draft, hide.true?}
        end

        # Show the current snapshot if navigating history.
        matchpi %{{¦ hide_boolean timeline: ((_* snapshot_) I _dict _ _)}} do
          {snapshot, hide.true?}
        end
      end

      return if !force && @seen == frame

      ws.state.update do |state|
        window_specs = Pf::Kit.stack_array(Term, 8)

        circuit, hide = frame
        if hide
          circuit = MuSoma.hide_single(circuit)
        end

        tree = ws.parser.parse(circuit)

        window_infos = MuSoma.window_infos(mu, tree)
        window_infos.each do |window_info|
          window_render = Microfold.render(mu, window_info.defn).unwrap
          next unless window_spec = MediaService.window_spec?(window_render)

          window_specs << Term.of(:spec, window_spec.content,
            id: window_info.id,
            open: window_info.open,
            title: window_spec.title,
            width: window_spec.width,
            height: window_spec.height,
            backdrop: window_spec.backdrop,
            cursor: window_spec.cursor.to_term,
            input: window_spec.input,
          )
        end

        active_window_spec = state[:"active-window-spec"]
        if window_specs.present? && active_window_spec == Term.of(:none)
          active_window_spec = Term.of(:some, window_infos.first.id)
        end

        Term.morph(state,
          {:"window-specs", window_specs},
          {:"active-window-spec", active_window_spec},
        )
      end
    end
  end

  class AppAgent
    @mu : Microfold::SyncCodex?

    def initialize
      @monitoring_vantages = Set(Term::Dict).new
    end

    def receive(ws : Workspace, request : AppRequest) : Nil
      Term.case(request.term) do
        matchpi %{(log msg_string)} do
          deadline = ws.scheduler.now + 5.seconds
          ws.scheduler.add(Scheduler::Deadline.new(deadline))
          ws.state.update do |state|
            state.with(:log, {deadline.repr, msg})
          end
        end

        otherwise { }
      end
    end

    def receive(ws : Workspace, plan : Plan, msg : Scheduler::Expire)
      ws.state.update do |state|
        # Expire log.
        Term.matchpi?(state, %{{¦ log: (deadline_ _)}}) do
          next unless deadline == Term.of(msg.deadline.repr)

          state = state.without(:log)
        end

        state
      end
    end

    def receive(ws : Workspace, plan, msg : MediaService::WindowDescriptionChanged) : Nil
      ws.state.update do |state|
        # Read vantages.
        vantages = Slice(Term).empty
        if description = msg.description?
          vantages = description.vantages

          # Keep window size in sync.
          state = Term.morph(state,
            {:width, description.width},
            {:height, description.height},
          )
        end

        seen = Set(Term::Dict).new
        hover = Set(Term).new

        # Add or update vantages in state, update hover-over.
        vantages.each do |vantage|
          Term.case(vantage) do
            matchpi %{(vantage {¦ hit-hover} ⍊ id: (hoverable key_))} do
              hover << key
            end

            matchpi %{(vantage _* ⍊ id: (state keys_*))} do
              state = Term.assign(state, keys.items, to: vantage)
              seen << keys.as_d
            end

            otherwise { }
          end
        end

        # Delete vantages.
        (@monitoring_vantages - seen).each do |keys|
          state = Term.assign(state, keys.items, to: nil)
        end

        @monitoring_vantages = seen

        state.with(:"hover-over", hover)
      end
    end

    def receive(ws, plan, msg) : Nil
    end

    def present(ws : Workspace) : Nil
      force = false

      if Var.pending?(ws.mu_codex)
        @mu = ws.mu_codex.get.fork
        force = true
      end

      mu = @mu
      assert mu

      return unless Var.pending?(ws.state, ws.codex, or_if: force)

      ui_mu = ws.codex.get.app.render(ws.state.get)
      ui_scenery = Microfold.render(mu, ui_mu).unwrap
      if spec = MediaService.window_spec?(ui_scenery)
        MediaService.publish(Term.of(:app), spec)
      else
        # FIXME: ?! We must recover somehow!
        ws.console.send(CriticalLog.new("Invalid spec"))
        MediaService.withdraw(Term.of(:app), MediaService::WindowSpec).wait
      end
    end
  end

  class ExtrinsicsAgent
    def initialize
      @refs = Set(ExtrinsicRef).new
    end

    def receive(ws, plan, msg : ReloadRefs) : Nil
      plan << UpdateRefs.new(ws.extrinsics)
    end

    def receive(ws, plan, msg) : Nil
    end

    def entangle?(ws : Workspace, plan, nodes) : Bool
      return false unless Var.pending?({ws.state, :timeline})

      wants_refs = Set(ExtrinsicRef).new
      missing_refs = false

      nodes.each_with_addr do |node, _|
        Term.case(node) do
          matchpi %{[path-report _string]} do
            missing_refs = true
            continue
          end

          matchpi %{[path-report path_string _?]}, path: NormalPath do
            wants_refs << ReportRef.new(path)
          end

          matchpi %{[path-reading _string]} do
            missing_refs = true
            continue
          end

          matchpi %{[path-reading path_string _?]}, path: NormalPath do
            wants_refs << ReadingRef.new(path)
          end

          matchpi %{[resource _]} do
            missing_refs = true
            continue
          end

          matchpi %{[resource term_ _?]} do
            next unless query = ResourceService.query?(term)

            wants_refs << ResourceRef.new(query)
          end

          otherwise { }
        end
      end

      # Ref diff.
      added_refs = wants_refs - @refs
      discarded_refs = @refs - wants_refs

      discarded_refs.each { |ref| ws.extrinsics.delete(ref) }
      added_refs.each { |ref| ws.extrinsics.add(ref) }
      @refs = wants_refs

      if missing_refs
        plan << UpdateRefs.new(ws.extrinsics)
      end

      true
    end
  end

  class WriteAgent
    def receive(ws, plan, msg : WriteFinished) : Nil
      plan << FinishWrites.new(msg.path, msg.content)
    end

    def receive(ws, plan, msg) : Nil
    end

    def entangle?(ws : Workspace, plan, nodes) : Bool
      return false unless Var.pending?({ws.state, :timeline})

      writes = {} of NormalPath => Term::Blob | Term::Str

      nodes.each_with_addr do |node, _|
        Term.case(node) do
          matchpi %{[file-sink path_string content_blob]}, path: NormalPath do
            writes[path] = content.as_blob
          end

          matchpi %{[file-sink path_string content_string]}, path: NormalPath do
            writes[path] = content.as_s
          end

          otherwise { }
        end
      end

      return true unless writes.present?

      schedule(ws.msgq, ws.alarm, writes)

      true
    end

    def schedule(msgq, alarm, writes)
      writes.each do |path, content|
        spawn do
          content_ = content
          if content_.is_a?(Term::Str)
            # NOTE: Converting Str to Blob is cheap except for digest generation,
            # which is O(N).
            content_ = Term[content_.as_s.to_slice]
          end

          PathService.write(path, content_).wait

          msgq << WriteFinished.new(path, content)
          alarm.call
        end
      end
    end
  end

  class AssemblerAgent
    @seen : Bytes?

    def initialize(@library_ref : ReadingRef)
      @state = Rack::Assembler.state
    end

    # The classifier used specifically for *rules* when you hide the editor.
    # It calls `hide_single` on incomplete nodes and re-classifies them.
    #
    # This classifier exists specifically to allow hiding the editor while
    # editing rules, to see how the rule instantiates.
    def self.hideclf : D7::Classifier
      successor = MuSoma.clf

      ->(node : Term) do
        feature = successor.call(node)
        unless feature.is_a?(D7::Inert)
          return feature
        end

        unless feature.annotations.incomplete?
          return feature
        end

        successor.call(MuSoma.hide_single(feature.node))
      end
    end

    def boot(ws : Workspace)
      ws.extrinsics.add(@library_ref)
    end

    def sync(ws : Workspace)
      return unless reading = ws.extrinsics[@library_ref]?

      sync(ws, reading)
    end

    def sync(ws : Workspace, reading : PathService::ContentReading)
      return if @seen == reading.blob.digest

      ws.console.send(InfoLog.new("Reload library"))

      @seen = reading.blob.digest

      begin
        document = ML.document(reading.blob.to_string)
      rescue e : ML::SyntaxError
        ws.console.send(ErrLog.new("Library source on disk is invalid, running from memory..."))
      else
        ws.library.set(Rack::Assembler.library(document))
      end
    end

    def sync(ws : Workspace, reading : PathService::DigestReading | PathService::Absent)
      ws.console.send(ErrLog.new("Library source on disk is absent, running from memory..."))
    end

    def step(ws : Workspace)
      return unless Var.pending?({ws.state, :timeline}, {ws.state, :hide}, ws.library)

      ws.state.update do |state|
        Term.case(state) do
          matchpi %{{¦ hide_boolean timeline: (_ I _ (%any . ...) draft_)}} do
            if hide.true?
              rclf = AssemblerAgent.hideclf
              rtree = D7.parse(rclf, draft, reply: D7::ParseTree)
              wtree = ws.parser.parse(draft)
            else # hide = false
              rclf = MuSoma.clf
              rtree = wtree = ws.parser.parse(draft)
            end

            draft1 = Rack::Assembler.step(rclf, rtree, wtree, @state, ws.library.get)

            Term.morph(state, {:timeline, 4, draft1})
          end

          otherwise { state }
        end
      end
    end
  end

  class CircuitParser
    def initialize
      @cache = GenerationalCache(Term, D7::ParseTree).new
    end

    # NOTE: Assumes the caller will deduplicate consecutive frames.
    def step(clf : D7::Classifier, circuit : Term, prepass, steppers : Tuple) : Slice(Term)
      @cache.epoch do
        subframes = Slice(Term).empty

        steppers.each do |stepper|
          subframes += stepper.step(clf, circuit, prepass, cache: @cache)
          circuit = subframes.last
        end

        subframes
      end
    end

    def parse(circuit : Term) : D7::ParseTree
      @cache.epoch do
        D7.parse(MuSoma.clf, circuit, reply: D7::ParseTree, cache: @cache)
      end
    end
  end

  class CircuitAgent
    def initialize(@seed_ref : ReadingRef)
      @vantages = VarHash(D7::NodeAddr, Term).new
    end

    def boot(ws : Workspace)
      ws.extrinsics.add(@seed_ref)
      ws.state.update(&.with(:"seed-path", @seed_ref.path))
    end

    def sync(ws : Workspace)
      return unless reading = ws.extrinsics[@seed_ref]?

      sync(ws, reading)
    end

    def sync(ws : Workspace, reading : PathService::ContentReading)
      return if @seen == reading.blob.digest

      ws.console.send(InfoLog.new("Reload seed"))

      @seen = reading.blob.digest

      begin
        document = ML.document(reading.blob.to_string)
      rescue e : ML::SyntaxError
        ws.console.send(ErrLog.new("Seed source on disk is invalid, running from memory..."))
      else
        ws.state.update do |state|
          Term.morph(state, {:"seed'", document})
        end
      end
    end

    def sync(ws : Workspace, reading : PathService::DigestReading | PathService::Absent)
      ws.console.send(ErrLog.new("Seed source on disk is absent, running from memory..."))
    end

    def receive(ws, plan, msg : MediaService::WindowDescriptionChanged)
      # TODO: What do we need to do if the window is closed?
      return unless description = msg.description?

      state = {} of D7::NodeAddr => Term

      description.vantages.each do |vantage|
        Term.matchpi?(vantage, %{(vantage _* ⍊ id: (figure path←(_*)))}) do
          indices = path.items.compact_map(&.to?(UInt32))
          assert indices.size == path.size

          addr = D7::NodeAddr.new(indices, &.itself)
          observation = Term.morph(vantage, {0, :figure}, {:id, nil})
          state[addr] = observation
        end
      end

      @vantages.sync(state)
    end

    def receive(ws, plan, msg : Scheduler::Event)
      plan << msg
    end

    def receive(ws, plan, msg)
    end

    def receive(ws : Workspace, request : AppRequest)
      Term.case(request.term) do
        matchpi %{(write-document document_)} do
          blob = Term::Blob.new(ML.display(document, style: ML::Style::Document, maxwidth: 80))
          PathService.write(@seed_ref.path, blob)
        end

        otherwise { }
      end
    end

    def step(ws : Workspace)
      return unless Var.pending?({ws.state, :timeline}, {ws.state, :hide}, ws.codex) || @vantages.pending?

      history_limit = ws.codex.get.history_limit

      ws.state.update do |state|
        Term.case(state) do
          matchpiT %{{¦ hide_boolean timeline: (behind_dict I ahead_ status←(%any . ...) draft_)}} do |behind|
            prepass = FigurePrepass.new(@vantages, successor: Rack::Prepass)

            drafts1 = ws.parser.step(MuSoma.clf, draft, prepass, steppers: {Rack::Tspace, Rack})
            draft1 = drafts1.last

            status1 = status
            if status == Term.of(:".")
              status1 = Term.of(:"-")
            end

            pass do
              # They are navigating history while we're running. Just update
              # the draft and move on.
              next unless ahead == Term.of(:"*")

              # Time is running.

              drafts1.each do |subframe|
                # Do not insert duplicate consecutive entries.
                next if behind.items.last? == subframe

                if behind.itemsize < history_limit
                  behind = behind.append(subframe)
                else
                  behind = behind.rest.append(subframe)
                end
              end
            end

            state.with(:timeline, {behind, :I, ahead, status1, draft1})
          end

          otherwise { state }
        end
      end
    end
  end

  # FIXME: Each window should have its own @mice and should only update mouse
  # nodes in it.
  class MouseAgent
    def initialize
      @mice = Slice(MediaService::Mouse).empty
      @hovered = false
    end

    def receive(ws : Workspace, plan, msg : MediaService::WindowDescriptionChanged) : Nil
      mice0 = @mice
      mice1 = Slice(MediaService::Mouse).empty
      if description = msg.description?
        mice1 = description.mice
      end

      hovered0 = @hovered
      hovered1 = false
      Term.matchpi?(ws.state.get, %{{¦ window-view-vantage: [vantage {¦ hit-hover}]}}) do
        hovered1 = true
      end

      @mice = mice1
      @hovered = hovered1

      case {hovered0, hovered1}
      in {false, false}
      in {false, true}, {true, false}
        # Update mice in the circuit on window-view pane hover / unhover transition.
        plan << UpdateMice.new(mice0, mice1)
      in {true, true}
        # Update mice in the circuit while window-view is hovered.
        unless mice0 == mice1
          plan << UpdateMice.new(mice0, mice1)
        end
      end

      state0 = MediaService::Mouse::State::None
      if mouse0 = mice0.first?
        state0 = mouse0.state
      end

      state1 = MediaService::Mouse::State::None
      if mouse1 = mice1.first?
        state1 = mouse1.state
      end

      update(ws.state, state0, state1)
    end

    def receive(ws, plan, msg) : Nil
    end

    def update(state_var : Var, state0, state1) : Nil
      state_var.update do |state|
        pressed = state1 - state0
        released = state0 - state1

        mouse = state[:mouse].transaction do |commit|
          released.each do |button|
            commit.without(button.term)
          end
          pressed.each do |button|
            commit.with(button.term, true)
          end
        end

        state.with(:mouse, mouse)
      end
    end
  end

  # FIXME: Each window should have its own @input, @keyboard, @exchange, and should
  # only update keyboard nodes in it!
  class KeyboardAgent
    def initialize
      @input = InputTransition.new
      @keyboard = InputTransition.new
      @exchange = InputExchange.new
    end

    def entangle?(ws : Workspace, plan, nodes) : Bool
      return false unless Var.pending?({ws.state, :timeline})

      @exchange = InputExchange.new

      nodes.each_with_addr do |node, addr|
        Term.case(node) do
          matchpi %{(input _* ⍊ focus_⋮ false)} do |focus|
            next unless focus = InputFocus.parse?(focus)

            keys = node.items.move(1)
            @exchange = @exchange.register(addr, InputModel.new(focus, keys.to_pf_set))
          end

          matchpi %{(keyboard _* ⍊ focus_⋮ false)} do |focus|
            next unless focus = InputFocus.parse?(focus)

            keys = node.items.move(1)
            @exchange = @exchange.register(addr, KeyboardModel.new(focus, keys.to_pf_set))
          end

          otherwise { }
        end
      end

      true
    end

    def receive(ws : Workspace, plan, msg : MediaService::WindowDescriptionChanged)
      input1 = keyboard1 = Pf::Set(Term).new
      if description = msg.description?
        input1 = description.input
        keyboard1 = description.keyboard
      end

      @input = @input.to(input1)
      @keyboard = @keyboard.to(keyboard1)

      ws.state.update do |state|
        exchange0 = @exchange

        case state[:mode]
        when Term.of(:insert)
          @input.pressed.each do |key|
            Term.case(key) do
              matchpi %{(key escape)} do
                state = state.with(:mode, :normal)
              end

              matchpi %{(key tab)} do
                # Ignore
              end

              matchpi %{(key name_)} do |name|
                ctrl = {
                  Term.of(:key, {:ctrl, :left}),
                  Term.of(:key, {:ctrl, :right}),
                }.any?(&.in?(input1))

                shift = {
                  Term.of(:key, {:shift, :left}),
                  Term.of(:key, {:shift, :right}),
                }.any?(&.in?(input1))

                alt = {
                  Term.of(:key, {:alt, :left}),
                  Term.of(:key, {:alt, :right}),
                }.any?(&.in?(input1))

                if name.type.string?
                  name_string = name.to(String)
                  # Convert names for keys such as (key "d") to symbol because
                  # editR only understands symbol keys.
                  if name_string.each_char.all?(&.ascii_letter?)
                    name = Term::Sym.new(name_string)
                  end
                end

                motion = Term.of(:keyboard, :key, name, :dn,
                  ctrl: ctrl ? true : nil,
                  shift: shift ? true : nil,
                  alt: alt ? true : nil,
                )

                state = state.with(:input, Term[]).with(:keyboard, Term[])
                state = state.with(:motions, state[:motions].append(motion))
              end

              matchpi %{(rune text_string)} do
                motion = Term.of(:keyboard, :input, text)
                state = state.with(:input, Term[]).with(:keyboard, Term[])
                state = state.with(:motions, state[:motions].append(motion))
              end

              otherwise { }
            end
          end
        when Term.of(:grab)
          @input = @input.handle({Term.of(:key, {:shift, :left})}, Term.of(:key, :tab)) do
            @keyboard = @keyboard.handle(Term.of(:key, :tab))
            @exchange = @exchange.backward
          end

          @input = @input.handle(Term.of(:key, :tab)) do
            @keyboard = @keyboard.handle(Term.of(:key, :tab))
            @exchange = @exchange.forward
          end

          if @exchange.active?
            @input = @input.handle(Term.of(:key, :escape)) do
              @keyboard = @keyboard.handle(Term.of(:key, :escape))
              @exchange = @exchange.blur
            end
          end
        end

        input_known = state[:input].items.to_pf_set
        keyboard_known = state[:keyboard].items.to_pf_set
        state = state
          .with(:input, @input.update(input_known))
          .with(:keyboard, @keyboard.update(keyboard_known))

        # Perform focus exchange regardless of mode.
        @exchange = @exchange.step

        # Keys are synced only in grab mode.
        if state[:mode] == Term.of(:grab)
          keyboard_sync = @keyboard
          input_sync = @input
        else
          keyboard_sync = @keyboard.to(Pf::Set(Term).new)
          input_sync = @input.to(Pf::Set(Term).new)
        end

        InputExchange.sync(exchange0, @exchange, input_sync, keyboard_sync) do |action|
          plan << action
        end

        # Assume they are holding the keys until the next update arrives.
        @input = @input.hold
        @keyboard = @keyboard.hold

        state
      end
    end

    def receive(ws, plan, msg)
    end
  end

  class SchedulerAgent
    def initialize
      @periods = Set(Time::Span).new
    end

    def entangle?(ws : Workspace, plan, nodes) : Bool
      return false unless Var.pending?({ws.state, :timeline})

      seen = Set(Time::Span).new

      nodes.each_with_addr do |node, _|
        Term.case(node) do
          matchpi %{[sequencer duration-term_ _+]} do
            next unless duration = MuSoma.duration?(duration_term)
            next if duration.negative? # ?!

            seen << duration
          end

          matchpi %{[ticker duration-term_ _number]} do
            next unless duration = MuSoma.duration?(duration_term)
            next if duration.negative? # ?!

            seen << duration
          end

          otherwise { }
        end
      end

      (@periods - seen).each do |duration|
        ws.scheduler.delete(Scheduler::Period.new(duration))
      end
      (seen - @periods).each do |duration|
        ws.scheduler.add(Scheduler::Period.new(duration))
      end

      @periods = seen

      true
    end
  end

  class CodexAgent
    def initialize(@codex_ref : ReadingRef)
      @seen = Bytes.empty
    end

    RE_CONTROL = /^;;\h+\\control\h+(?<head>[^\v]*)\v(?<body>(?:;;[^\v]*(?:\v|$))*)/m

    def self.control_docs(source : String) : Term::Dict
      Term::Dict.build do |commit|
        source.scan(RE_CONTROL) do |match|
          begin
            head = ML.term(match["head"])
          rescue e : ML::SyntaxError
            next
          end

          body = match["body"].view

          paragraphs = [] of Term
          paragraph = [] of Term

          body.each_line do |line|
            assert line.starts_with?(";;")

            line = line.lskip(nchars: 2).strip(charset: " \n")

            # Paragraph boundary
            #
            #   ;; Foo bar baz
            #   ;; ⏏
            #   ;; Qux
            if line.empty?
              paragraphs << Term.of(paragraph)
              paragraph.clear
              next
            end

            line.split(' ') do |word|
              paragraph << Term.of(word)
            end
          end

          paragraphs << Term.of(paragraph)
          paragraph.clear

          commit << Term.of(head: head, body: paragraphs)
        end
      end
    end

    def sync(ws : Workspace) : Nil
      return unless reading = ws.extrinsics[@codex_ref]?

      sync(ws, reading)
    end

    private def sync(ws : Workspace, reading : PathService::ContentReading) : Nil
      return if @seen == reading.blob.digest

      @seen = reading.blob.digest

      ws.console.send(InfoLog.new("Reload MuSoma codex"))

      source = reading.blob.to_string

      begin
        document = ML.document(source)
      rescue e : ML::SyntaxError
        ws.console.send(ErrLog.new("MuSoma codex on disk is invalid, running from memory..."))
        return
      end

      ws.codex.set(MuSoma.codex(document))

      # Find inline documentation of the form `;; /control ...` and expose
      # it to the state.
      control_docs = CodexAgent.control_docs(source)

      ws.state.update do |state|
        Term.morph(state, {:"control-docs", control_docs})
      end
    end

    private def sync(ws : Workspace, reading : PathService::DigestReading | PathService::Absent)
      ws.console.send(ErrLog.new("MuSoma codex on disk is absent, running from memory..."))
    end
  end

  class DatabaseAgent
    alias Stmt = Exec | Query

    defrecord Exec, sql : String, args : Slice(DB::Any)
    defrecord Query, sql : String, args : Slice(DB::Any)

    def self.stmt?(query : Term) : Stmt?
      Term.case(query) do
        matchpi %{(exec sql_string rest_*)}, sql: String do
          Exec.new(sql, transcribe(rest.items))
        end

        matchpi %{(query sql_string rest_*)}, sql: String do
          Query.new(sql, transcribe(rest.items))
        end

        otherwise { }
      end
    end

    def self.transcribe(terms : Enumerable(Term)) : Slice(DB::Any)
      terms.to_readonly_slice { |term| transcribe(term) }
    end

    # Converts `Term` to `DB::Any`.
    def self.transcribe(term : Term) : DB::Any
      result = Term.case(term) do
        matchpi %{_string} { term.to(String) }
        matchpi %{_boolean} { term.to(Bool) }
        matchpi %{(%number i32)} { term.to(Int32) }
        matchpi %{(%number i64)} { term.to(Int64) }

        matchpi %{_number} do
          n = term.as_n
          continue unless n.approx?

          # There's little point going through Float64 as well because we store
          # approx as Float32 so there's no more "precision" or anything to gain.
          n.to(Float32)
        end

        matchpi %{_blob} do
          term.as_blob.to_slice
        end

        otherwise do
          ML.compact(term) # Fallback
        end
      end

      result.as(DB::Any)
    end

    def self.transcribe?(object : DB::Any) : Term?
      # Term.of handles all members of DB::Any.
      Term.of(object)
    end

    defrecord Task, uri : String, stmt : Stmt

    def initialize
      @dbs = {} of String => DB::Database
      @running = Set(Task).new
      @running_lock = Sync::Mutex.new
      @pending = true
    end

    def entangle?(ws : Workspace, plan : Plan, nodes) : Bool
      return false unless @pending || Var.pending?({ws.state, :timeline})

      seen_uris = Set(String).new
      seen_stmts = Set(Task).new

      nodes.each_with_addr do |node, addr|
        Term.case(node) do
          # This one simply asks us to keep the connection open.
          matchpi %{[db uri_string]}, uri: String do
            seen_uris << uri
          end

          # This one asks us to keep the connection but also, simultaneously, to
          # execute the given *stmt* on the connection.
          matchpi %{[db (uri_string stmtQ_)]}, uri: String do
            seen_uris << uri
            next unless stmt = DatabaseAgent.stmt?(stmtQ)

            seen_stmts << Task.new(uri, stmt)
          end

          otherwise { }
        end

        node
      end

      # Disconnect from discarded uris.
      @dbs.select! do |uri, db|
        if uri.in?(seen_uris)
          next true # keep
        end

        db.close

        ws.console.send(InfoLog.new("Closed database #{uri}"))

        false # reject
      end

      # Connect to new uris.
      seen_uris.each do |uri|
        next if @dbs.has_key?(uri)

        @dbs[uri] = DB.open(uri)
        @pending = true

        ws.console.send(InfoLog.new("Opened database #{uri}"))
      end

      added = @running_lock.synchronize do
        running0 = @running
        running1 = seen_stmts
        # This discards in-progress statements/fibers that were dropped in
        # the current cycle. We rely on periodic polling from those fibers
        # so that they know when to cancel themselves.
        @running = running1

        running1 - running0
      end

      # Schedule new statements.
      added.each do |task|
        next unless db = @dbs[task.uri]?

        schedule(ws, db, task)
      end

      true
    end

    # :nodoc:
    class Canceled < Exception
      @callstack = CallStack.empty
    end

    private def schedule(ws : Workspace, db : DB::Database, task : Task)
      spawn(name: "MuSoma database retriever") do
        db.using_connection do |connection|
          result = run(connection, task)
          ws.msgq << DatabaseTaskCompleted.new(task, result)
          ws.alarm.call
        end
      rescue Canceled
        # Do nothing.
      rescue e : DB::Error | SQLite3::Exception
        ws.msgq << DatabaseTaskCompleted.new(task, Term.of(:err, e.message))
        ws.alarm.call
      end
    end

    private def run(connection : DB::Connection, task : Task) : Term
      case stmt = task.stmt
      in Exec
        result = connection.exec(stmt.sql, args: stmt.args)

        Term.of(:ok, result.rows_affected)
      in Query
        rows = Term::Dict.build do |results_commit|
          connection.query(stmt.sql, args: stmt.args) do |rs|
            rs.each do
              unless @running_lock.synchronize { task.in?(@running) }
                raise Canceled.new
              end

              result = Term::Dict.build do |result_commit|
                (0...rs.column_count).each do |index|
                  key = Term.of(rs.column_name(index))
                  value = DatabaseAgent.transcribe?(rs.read)
                  result_commit.with(key, value)
                end
              end

              results_commit << result
            end
          end
        end

        Term.of(rows)
      end
    end

    def receive(ws, plan, msg : DatabaseTaskCompleted) : Nil
      plan << msg
    end

    def receive(ws, plan, msg : Msg) : Nil
    end
  end
end
