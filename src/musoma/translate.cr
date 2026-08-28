# Functions in this file, especially the translate() overloads, deal with converting
# Crystal-side objects to Terms and vice versa.

module MuSoma
  def translate(object : Nil) : Nil
  end

  def translate(object point : Scenery::Point) : Term
    Term.of(x: Scenery.describe(point.x), y: Scenery.describe(point.y))
  end

  def translate(object mode : Scenery::HitQuery::Mode) : Term
    case mode
    in .point?  then Term.of(:point)
    in .single? then Term.of(:single)
    in .double? then Term.of(:double)
    in .triple? then Term.of(:triple)
    end
  end

  def translate(object report : PathService::DirListing) : Term
    result = Term::Dict.build do |commit|
      commit << :dir
      commit.with(:timestamp, report.timestamp.to_s)
      commit.concat(report.entries) { |entry| translate(entry) }
    end

    Term.of(result)
  end

  def translate(object report : PathService::FileListing) : Term
    Term.of(:file,
      timestamp: report.timestamp.to_s,
      size: translate(Bytesize.new(report.bytesize)),
    )
  end

  def translate(object report : PathService::Absent) : Term
    Term.of(:absent, report.detail)
  end

  def translate(object report : PathService::DirEntry) : Term
    Term.of(:dir, report.path.basename)
  end

  def translate(object report : PathService::FileEntry) : Term
    Term.of(:file, report.path.basename)
  end

  def translate(object report : PathService::ContentReading) : Term
    Term.of(:present, translate(report.blob))
  end

  # FIXME: This isn't a good idea. We must let the user choose to interpret
  # blobs this way if UTF-8.
  def translate(object blob : Term::Blob) : Term
    if blob.utf8?
      return Term.of(blob.to_string)
    end

    Term.of(blob)
  end

  def translate(object report : PathService::DigestReading) : Term
    Term.of(:digest, report.digest.hexstring, size: translate(Bytesize.new(report.bytesize)))
  end

  defrecord Bytesize, value : Int64

  def translate(object bytesize : Bytesize) : Term
    Term.of(bytes: bytesize.value, human: bytesize.value.humanize_bytes)
  end

  def translate(object response : ResourceService::Present) : Term
    Term.of(:present, translate(response.query), translate(response.content))
  end

  def translate(object response : ResourceService::Absent) : Term
    Term.of(:absent, response.detail)
  end

  def translate(object response : ResourceService::FileQuery) : Term
    Term.of(:file, response.path)
  end

  def translate(object response : ResourceService::HTTPQuery) : Term
    Term.of(:uri, response.uri)
  end

  def translate(object response : ResourceService::IdQuery) : Term
    Term.of(:id)
  end

  def duration?(term : Term) : Time::Span?
    Term.case(term) do
      matchpi %{(±n minutes)}, %{(±n minute)}, %{(±n m)}, n: Float64 do
        n.minutes
      end

      matchpi %{(±n seconds)}, %{(±n second)}, %{(±n s)}, n: Float64 do
        n.seconds
      end

      matchpi %{(±n milliseconds)}, %{(±n millisecond)}, %{(±n ms)}, n: Float64 do
        n.milliseconds
      end

      matchpi %{(±n microseconds)}, %{(±n microsecond)}, %{(±n us)}, %{(±n µs)}, n: Float64 do
        n.microseconds
      end

      otherwise { }
    end
  end
end
