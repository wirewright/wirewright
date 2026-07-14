module Ww::Rack::Extrinsics
  extend self

  # :nodoc:
  defcase State,
    epoch : Automaton::Epoch,
    extrinsics : ExtrinsicMap,
    write_statuses : Hash(NormalPath, WriteStatus),
    write_statuses_lock : Sync::Mutex,
    transcriptions : GenerationalCache(PathService::Reading | PathService::Report | ResourceService::Response, Term)

  alias WriteStatus = WritePending | WriteCompleted

  defrecord WritePending
  defrecord WriteCompleted, content : Term::Str | Term::Blob

  def state(epoch : Automaton::Epoch) : State
    extrinsics = ExtrinsicMap.new(epoch)
    write_statuses = {} of NormalPath => WriteStatus
    write_statuses_lock = Sync::Mutex.new
    transcriptions = GenerationalCache(PathService::Reading | PathService::Report | ResourceService::Response, Term).new

    State.new(epoch, extrinsics, write_statuses, write_statuses_lock, transcriptions)
  end

  # NOTE: As a curious curiosity (and an important fact!), if there's one or
  # more `path`s or `resource`s in the circuit, then it is always pending. It
  # cannot truly reach quiescence because the corresponding path can change
  # at any moment.
  def pending?(state : State) : Bool
    state.extrinsics.size > 0 || state.write_statuses_lock.synchronize { state.write_statuses.size > 0 }
  end

  # :nodoc:
  SYM_PATH = Term[:path]
  # :nodoc:
  SYM_RESOURCE = Term[:resource]

  # :nodoc:
  defrecord StepContext,
    seen : Set(ExtrinsicMap::Ref),
    write_progress : Hash(NormalPath, WriteStatus),
    write_proposals : Hash(NormalPath, Set(Term::Str | Term::Blob))

  def step(state : State, parser : D7::Parser, circuit circuit0 : Term, prepass) : Slice(Term)
    seen = Set(ExtrinsicMap::Ref).new

    # Prepare write progress. I'd expect the counts here to be single-digit,
    # so let's stay dumb for now.
    write_progress = {} of NormalPath => WriteStatus
    state.write_statuses_lock.synchronize do
      state.write_statuses.each do |path, status|
        write_progress[path] = status
      end
    end

    write_proposals = {} of NormalPath => Set(Term::Str | Term::Blob)

    circuit1 = state.transcriptions.epoch do
      tree = parser.parse(circuit0, reply: D7::ParseTree)
      D7.perturb(tree, cue_disj: {SYM_PATH, SYM_RESOURCE}) do |node, _|
        ctx = StepContext.new(seen, write_progress, write_proposals)
        perturb(state, ctx, node)
      end
    end

    # Fast path if circuit did not change.
    if circuit0 == circuit1 && state.extrinsics.size == seen.size && write_proposals.empty?
      return Slice[circuit0]
    end

    # Let's have `added` for symmetry, I know we don't really need it...
    added = Pf::Kit.stack_array(ExtrinsicMap::Ref, 8)
    removed = Pf::Kit.stack_array(ExtrinsicMap::Ref, 8)

    seen.each do |ref|
      next if ref.in?(state.extrinsics)

      added << ref
    end

    state.extrinsics.each_ref do |ref|
      next if ref.in?(seen)

      removed << ref
    end

    # Sync extrinsics.
    added.each { |ref| state.extrinsics.add(ref) }
    removed.each { |ref| state.extrinsics.delete(ref) }

    # Sync writes.
    wsync(state.epoch,
      write_progress,
      write_proposals,
      state.write_statuses,
      state.write_statuses_lock,
    )

    Slice[circuit1]
  end

  private def wsync(epoch, progress, proposals, statuses, lock) : Nil
    lock.synchronize do
      # For WriteCompleted, assume the circuit saw them. We can "garbage collect"
      # them from the write_statuses map.
      #
      # NOTE: We currently cannot cancel writes. They must complete before they
      # are "GCd".
      progress.each do |path, status|
        next unless status.is_a?(WriteCompleted)

        statuses.delete(path)
      end

      proposals.each do |path, proposals|
        next if statuses.has_key?(path) # Already writing
        next unless proposals.size == 1

        proposal = proposals.first

        statuses[path] = WritePending.new

        spawn(name: "Rack file writing worker") do
          write(epoch, statuses, lock, path, proposal)
        end
      end
    end
  end

  private def write(epoch, statuses, lock, path : NormalPath, content : Term::Str) : Nil
    # Converting Str to Blob is cheap except for digest generation,
    # which is O(N).
    blob = Term[content.to_slice]
    PathService.write(path, blob).wait

    lock.synchronize do
      statuses[path] = WriteCompleted.new(content)
    end
    epoch.call
  end

  private def write(epoch, statuses, lock, path : NormalPath, content : Term::Blob) : Nil
    PathService.write(path, content).wait

    lock.synchronize do
      statuses[path] = WriteCompleted.new(content)
    end
    epoch.call
  end

  private def perturb(state : State, ctx : StepContext, node : Term) : Term
    Term.case(node) do
      matchpi(
        %{[path (path_string reading)]},
        %{[path (path_string reading) _]},
        path: NormalPath,
      ) do
        ref = ExtrinsicMap::ReadingRef.new(path)
        ctx.seen << ref

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
        ctx.seen << ref

        unless report = state.extrinsics[ref]?
          return Term.morph(node, {2, nil})
        end

        transcription = state.transcriptions.put_if_absent(report) do
          transcribe(report)
        end
        Term.morph(node, {2, transcription})
      end

      matchpi %{[path (path_string sink) content_string]}, path: NormalPath, content: Term::Str do
        case status = ctx.write_progress[path]?
        in Nil
          proposals = ctx.write_proposals.put_if_absent(path) { Set(Term::Str | Term::Blob).new }
          proposals << content

          node
        in WritePending
          # If it's our write then there's no point in proposing, we've already
          # scheduled it. If it's someone else's write, then by proposing we'd
          # simply cause a conflict. In that case, let's just wait until the
          # write completes before scheduling our own instead.
          node
        in WriteCompleted
          unless status.content == content
            # Wait until next tick for the WriteCompleted to expire, then
            # we'll propose.
            return node
          end

          Term.morph(node, {2, nil})
        end
      end

      matchpi %{[path (path_string sink) content_blob]}, path: NormalPath, content: Term::Blob do
        case status = ctx.write_progress[path]?
        in Nil
          proposals = ctx.write_proposals.put_if_absent(path) { Set(Term::Str | Term::Blob).new }
          proposals << content

          node
        in WritePending
          node
        in WriteCompleted
          unless status.content == content
            return node
          end

          Term.morph(node, {2, nil})
        end
      end

      matchpi %{[resource queryQ_]}, %{[resource queryQ_ _]} do
        continue unless query = ResourceService.query?(queryQ)

        ref = ExtrinsicMap::ResourceRef.new(query)
        ctx.seen << ref

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
