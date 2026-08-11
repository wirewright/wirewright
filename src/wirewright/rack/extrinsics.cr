module Ww::Rack::Extrinsics
  extend self

  # :nodoc:
  defcase State,
    epoch : Automaton::Epoch,
    extrinsics : ExtrinsicMap,
    transcriptions : GenerationalCache(PathService::Reading | PathService::Report | ResourceService::Response, Term)

  def state(epoch : Automaton::Epoch) : State
    extrinsics = ExtrinsicMap.new(epoch)
    transcriptions = GenerationalCache(PathService::Reading | PathService::Report | ResourceService::Response, Term).new

    State.new(epoch, extrinsics, transcriptions)
  end

  # NOTE: As a curious curiosity (and an important fact!), if there's one or
  # more `path`s or `resource`s in the circuit, then it is always pending. It
  # cannot truly reach quiescence because the corresponding path can change
  # at any moment.
  def pending?(state : State) : Bool
    state.extrinsics.size > 0
  end

  # :nodoc:
  SYM_PATH = Term[:path]
  # :nodoc:
  SYM_RESOURCE = Term[:resource]

  # :nodoc:
  defrecord StepContext, refs : Set(ExtrinsicMap::Ref)

  def step(state : State, & : Propose -> T) : T forall T
    seen_refs = Set(ExtrinsicMap::Ref).new

    result = state.transcriptions.epoch do
      ctx = StepContext.new(seen_refs)
      propose = Propose.new do |hg, proposals|
        propose(state, ctx, hg, proposals)
      end
      yield propose
    end

    # Despawn the fibers associated with ExtrinsicMap to avoid leaks.
    if seen_refs.empty?
      state.extrinsics.shutdown
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

    result
  end

  defrecord PathReading, node : D7::Node, path : NormalPath
  defrecord PathReport, node : D7::Node, path : NormalPath
  defrecord Resource, node : D7::Node, query : ResourceService::Query

  private def propose(state : State, ctx : StepContext, hg : D7::Hypergraph, proposals) : Nil
    hg.propose(proposals, :path, :resource) do |node|
      variant = nil

      Term.case(node.term) do
        matchpi(
          %{[path (path_string reading)]},
          %{[path (path_string reading) _ ]},
          path: NormalPath,
        ) do
          variant = PathReading.new(node, path)
        end

        matchpi(
          %{[path (path_string report)]},
          %{[path (path_string report) _ ]},
          path: NormalPath,
        ) do
          variant = PathReport.new(node, path)
        end

        matchpi %{[resource queryQ_]}, %{[resource queryQ_ _]} do
          continue unless query = ResourceService.query?(queryQ)

          variant = Resource.new(node, query)
        end

        otherwise { }
      end

      next if variant.nil?

      step(state, ctx, variant)
    end
  end

  private def step(state : State, ctx : StepContext, variant : PathReading) : D7::Patch?
    ref = ExtrinsicMap::ReadingRef.new(variant.path)
    ctx.refs << ref

    # If reading is not yet available, keep the old one (if any).
    return unless reading = state.extrinsics[ref]?

    transcription = state.transcriptions.put_if_absent(reading) do
      transcribe(reading)
    end

    if prev = variant.node.term[2]?
      return if Term.extension?(prev, of: transcription)
    end

    D7.patch(variant.node, {2, transcription})
  end

  private def step(state : State, ctx : StepContext, variant : PathReport) : D7::Patch?
    ref = ExtrinsicMap::ReportRef.new(variant.path)
    ctx.refs << ref

    # If report is not yet available, keep the old one (if any).
    return unless report = state.extrinsics[ref]?

    transcription = state.transcriptions.put_if_absent(report) do
      transcribe(report)
    end

    if prev = variant.node.term[2]?
      return if Term.extension?(prev, of: transcription)
    end

    D7.patch(variant.node, {2, transcription})
  end

  private def step(state : State, ctx : StepContext, variant : Resource) : D7::Patch?
    ref = ExtrinsicMap::ResourceRef.new(variant.query)
    ctx.refs << ref

    # If resource is not yet available, keep the old one (if any).
    return unless resource = state.extrinsics[ref]?

    transcription = state.transcriptions.put_if_absent(resource) do
      transcribe(resource)
    end
    D7.patch(variant.node, {2, transcription})
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

  # |@ rack.path.reading
  #
  # |@summary
  # The result of attempting to read a file or a file system entry.

  # |@ rack.path.reading
  #
  # |@pattern
  # (absent detail_string)
  #
  # |@key detail
  # Tells the reason why reading failed.
  #
  # |@block
  # Appears if the file does not exist or any other OS error occurs while
  # reading it. *detail* should tell the reason.

  # |@ rack.path.reading
  #
  # |@pattern
  # (present content_string)
  #
  # |@key content
  # The content of the file as a string.
  #
  # |@block
  # Appears if the file exists, is readable, and its *content* is valid UTF-8.

  # |@ rack.path.reading
  #
  # |@pattern
  # (present content_blob)
  #
  # |@key content
  # The content of the file as a blob.
  #
  # |@block
  # Appears if the file exists, is readable, but its content does not appear
  # to be UTF-8. That is, the file *content* is treated as an opaque blob.

  private def transcribe(object report : PathService::ContentReading) : Term
    Term.of(:present, transcribe(report.blob))
  end

  # |@ rack.path.reading
  #
  # |@pattern
  # (digest hash_string ⍊ size_)
  #
  # |@key hash
  # The SHA-256 digest of the file content.
  #
  # |@key size rack.path.size
  # The size of the file.
  #
  # |@block
  # Appears if the file exists, is readable, but is too large to safely load
  # into memory. The cutoff is currently 64 MiB.
  private def transcribe(object report : PathService::DigestReading) : Term
    Term.of(:digest, report.digest.hexstring, size: transcribe(Bytesize.new(report.bytesize)))
  end

  # FIXME: This isn't a good idea. We must let the user choose to interpret
  # blobs this way if UTF-8.
  private def transcribe(object blob : Term::Blob) : Term
    if blob.classif.utf8?
      return Term.of(blob.to_string)
    end

    Term.of(blob)
  end

  defrecord Bytesize, value : Int64

  # |@ rack.path.size
  #
  # |@pattern
  # {¦ ±bytes human_string}
  #
  # |@key bytes
  # The size in bytes.
  #
  # |@key human
  # The size as a human-readable string, for example, `"32 MiB"`.
  #
  # |@summary
  # Represents the size of a file or a file system entry.
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
