module Ww::Rack::Extrinsics
  extend self

  # :nodoc:
  defcase State,
    epoch : Automaton::Epoch,
    tasks : D7::TaskBoard(Automaton::Epoch, Task, Result),
    extrinsics : ExtrinsicMap,
    transcriptions : GenerationalCache(PathService::Reading | PathService::Report | ResourceService::Response, Term)

  alias Task = WriteFile | RemoveFile
  alias Result = WriteResult | RemoveResult

  defrecord WriteFile, path : NormalPath, content : Term::Str | Term::Blob
  defrecord RemoveFile, path : NormalPath

  alias WriteResult = WriteCompleted | WriteFailed

  defrecord WriteCompleted, content : Term::Str | Term::Blob
  defrecord WriteFailed, detail : String

  alias RemoveResult = RemoveCompleted | RemoveFailed

  defrecord RemoveCompleted
  defrecord RemoveFailed, detail : String

  def state(epoch : Automaton::Epoch) : State
    extrinsics = ExtrinsicMap.new(epoch)

    tasks = D7::TaskBoard(Automaton::Epoch, Task, Result).new(epoch) do |task, ping|
      execute(task, ping)
    end

    transcriptions = GenerationalCache(PathService::Reading | PathService::Report | ResourceService::Response, Term).new

    State.new(epoch, tasks, extrinsics, transcriptions)
  end

  private def execute(task : WriteFile, ping) : Result
    ping.call
    blob = Term[task.content.to_slice]
    ping.call

    result = PathService.write(task.path, blob).wait.unwrap

    case result
    in PathService::Present
      # Use original content, not blob. Original content is what publishers
      # are going to be searching for.
      WriteCompleted.new(task.content)
    in PathService::Absent
      WriteFailed.new(result.detail)
    end
  end

  private def execute(task : RemoveFile, ping) : Result
    ping.call

    begin
      Log.debug { "removing file #{task.path}" }
      File.delete(task.path.unwrap)
      Log.debug { "removed file #{task.path}" }

      RemoveCompleted.new
    rescue e : File::Error
      Log.debug(exception: e) { "file removal failed" }

      RemoveFailed.new(e.message || "internal error")
    end
  end

  # NOTE: As a curious curiosity (and an important fact!), if there's one or
  # more `path`s or `resource`s in the circuit, then it is always pending. It
  # cannot truly reach quiescence because the corresponding path can change
  # at any moment.
  def pending?(state : State) : Bool
    state.extrinsics.size > 0 || state.tasks.pending?
  end

  # :nodoc:
  SYM_PATH = Term[:path]
  # :nodoc:
  SYM_RESOURCE = Term[:resource]

  # :nodoc:
  defrecord StepContext,
    refs : Set(ExtrinsicMap::Ref),
    tasks : D7::TaskBoard::Rdv(Automaton::Epoch, Task, Result),
    proposals : Hash(NormalPath, Set(Task))

  def step(state : State, parser : D7::Parser, circuit circuit0 : Term, prepass) : Slice(Term)
    # Despawn the fibers associated with ExtrinsicMap to avoid leaks. Showing an empty
    # circuit to Automaton is the teardown pattern we use (and it also makes sense
    # semantically for subsystems that do not really know what "shutdown" means).
    #
    # We don't do this on delete (anymore) because that's rather expensive, if e.g.
    # a path report is continuously added and removed, we're hitting the worst
    # case all the time (spawn + do work + despawn).
    if circuit0 == Term.of
      state.extrinsics.shutdown
    end

    seen_refs = Set(ExtrinsicMap::Ref).new

    circuit1 = state.transcriptions.epoch do
      state.tasks.rdv do |tasks_rdv|
        proposals = {} of NormalPath => Set(Task)

        tree = parser.parse(circuit0, reply: D7::ParseTree)
        result = D7.perturb(tree, cue_disj: {SYM_PATH, SYM_RESOURCE}) do |node, _|
          ctx = StepContext.new(seen_refs, tasks_rdv, proposals)
          perturb(state, ctx, node)
        end

        # Sync tasks.
        proposals.each do |path, bucket|
          next unless bucket.size == 1

          proposal = bucket.first
          tasks_rdv.publish(proposal)
        end

        result
      end
    end

    # Fast path if there was no change.
    if circuit0 == circuit1 && state.extrinsics.size == seen_refs.size
      return Slice[circuit0]
    end

    # Let's have `added` for symmetry, I know we don't really need it...
    added = Pf::Kit.stack_array(ExtrinsicMap::Ref, 8)
    removed = Pf::Kit.stack_array(ExtrinsicMap::Ref, 8)

    seen_refs.each do |ref|
      next if ref.in?(state.extrinsics)

      added << ref
    end

    state.extrinsics.each_ref do |ref|
      next if ref.in?(seen_refs)

      removed << ref
    end

    # Sync extrinsics.
    added.each { |ref| state.extrinsics.add(ref) }
    removed.each { |ref| state.extrinsics.delete(ref) }

    # If anything we've added is immediately available (e.g. from cache), bump
    # epoch immediately. Otherwise there'd be no one to notify us, and we'd
    # hang forever waiting for something that's already there.
    available = added.any? { |ref| !!state.extrinsics[ref]? }
    if available
      state.epoch.call
    end

    Slice[circuit1]
  end

  private def perturb(state : State, ctx : StepContext, node : Term) : Term
    Term.case(node) do
      matchpi(
        %{[path (path_string reading)]},
        %{[path (path_string reading) _]},
        path: NormalPath,
      ) do
        ref = ExtrinsicMap::ReadingRef.new(path)
        ctx.refs << ref

        unless reading = state.extrinsics[ref]?
          return Term.morph(node, {2, nil})
        end

        transcription = state.transcriptions.put_if_absent(reading) do
          transcribe(reading)
        end
        Term.morph(node, {2, transcription})
      end

      matchpi(
        %{[path (path_string report)]},
        %{[path (path_string report) _]},
        path: NormalPath,
      ) do
        ref = ExtrinsicMap::ReportRef.new(path)
        ctx.refs << ref

        unless report = state.extrinsics[ref]?
          return Term.morph(node, {2, nil})
        end

        transcription = state.transcriptions.put_if_absent(report) do
          transcribe(report)
        end
        Term.morph(node, {2, transcription})
      end

      matchpi %{[path (path_string sink) (present content_)]}, path: NormalPath do |content|
        continue unless content = content.as_s? || content.as_blob?

        task = WriteFile.new(path, content)
        result = ctx.tasks.result?(task)
        assert result.is_a?(Nil) || result.is_a?(WriteResult)

        case result
        in Nil # Not available, propose for publishing.
          bucket = ctx.proposals.put_if_absent(path) { Set(Task).new }
          bucket << task

          node
        in WriteCompleted
          Term.morph(node, {2, nil})
        in WriteFailed
          Term.morph(node, {2, {:err, result.detail}})
        end
      end

      matchpi %{[path (path_string sink) absent]}, path: NormalPath do
        task = RemoveFile.new(path)
        result = ctx.tasks.result?(task)
        assert result.is_a?(Nil) || result.is_a?(RemoveResult)

        case result
        in Nil # Not available, propose for publishing.
          bucket = ctx.proposals.put_if_absent(path) { Set(Task).new }
          bucket << task

          node
        in RemoveCompleted
          Term.morph(node, {2, nil})
        in RemoveFailed
          Term.morph(node, {2, {:err, result.detail}})
        end
      end

      matchpi %{[resource queryQ_]}, %{[resource queryQ_ _]} do
        continue unless query = ResourceService.query?(queryQ)

        ref = ExtrinsicMap::ResourceRef.new(query)
        ctx.refs << ref

        unless resource = state.extrinsics[ref]?
          return Term.morph(node, {2, nil})
        end

        transcription = state.transcriptions.put_if_absent(resource) do
          transcribe(resource)
        end
        Term.morph(node, {2, transcription})
      end

      otherwise { node }
    end
  end

  private def transcribe(object report : PathService::DirListing) : Term
    result = Term::Dict.build do |commit|
      commit << :dir
      commit.with(:timestamp, report.timestamp.to_s)
      commit.concat(report.entries) { |entry| transcribe(entry) }
    end

    Term.of(result)
  end

  private def transcribe(object report : PathService::FileListing) : Term
    Term.of(:file,
      timestamp: report.timestamp.to_s,
      size: transcribe(Bytesize.new(report.bytesize)),
    )
  end

  private def transcribe(object report : PathService::Absent) : Term
    Term.of(:absent, report.detail)
  end

  private def transcribe(object report : PathService::DirEntry) : Term
    Term.of(:dir, report.path.basename)
  end

  private def transcribe(object report : PathService::FileEntry) : Term
    Term.of(:file, report.path.basename)
  end

  private def transcribe(object report : PathService::ContentReading) : Term
    Term.of(:present, transcribe(report.blob))
  end

  # FIXME: This isn't a good idea. We must let the user choose to interpret
  # blobs this way if UTF-8.
  private def transcribe(object blob : Term::Blob) : Term
    if blob.classif.utf8?
      return Term.of(blob.to_string)
    end

    Term.of(blob)
  end

  private def transcribe(object report : PathService::DigestReading) : Term
    Term.of(:digest, report.digest.hexstring, size: transcribe(Bytesize.new(report.bytesize)))
  end

  defrecord Bytesize, value : Int64

  private def transcribe(object bytesize : Bytesize) : Term
    Term.of(bytes: bytesize.value, human: bytesize.value.humanize_bytes)
  end

  private def transcribe(object response : ResourceService::Present) : Term
    Term.of(:present, transcribe(response.content))
  end

  private def transcribe(object response : ResourceService::Absent) : Term
    Term.of(:absent, response.detail)
  end
end
