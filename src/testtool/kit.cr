module Testtool
  # Represents a measurement.
  record Mmt, ttotal : Float64, memtotal : Int64 do
    def self.zero
      Mmt.new(0.0f64, 0i64)
    end

    def <=>(other : Mmt)
      score <=> other.score
    end

    def score
      ttotal * memtotal.to_f64
    end

    def +(other : Mmt) : Mmt
      Mmt.new(ttotal + other.ttotal, memtotal + other.memtotal)
    end

    def to_s(io)
      io << "Time: "
      ttotal.seconds.humanize(io)
      io << "  Memory: "
      memtotal.humanize_bytes(io)
    end
  end

  # Measures the block. Returns a tuple of `{Mmt, _}`, where `_` is the block's
  # return value.
  def measure(&)
    mem = result = nil

    tms = Benchmark.measure do
      mem = Benchmark.memory do
        result = {yield}
      end
    end

    assert mem && result

    {Mmt.new(tms.total, mem), *result}
  end

  # Measures the block "into" *stat* (a closure keeping a total of measurements)
  # and returns the block's result.
  def measure(stat : Mmt -> Mmt, &)
    mmt, result = measure { yield }
    stat.call(mmt)

    result
  end

  alias AttachmentRow = {String, Attachment}
  alias Attachment = Term | Exception

  defrecord Complaint,
    title : String,
    attachments : Array(AttachmentRow)

  # Constructs a complaint.
  def complaint(title : String, attachments : Array(AttachmentRow))
    Complaint.new(title, attachments)
  end

  # :ditto:
  def complaint(title : String, **attachments : Attachment)
    Complaint.new(title, attachments.map { |key, value| {key.to_s, value.as(Attachment)} })
  end

  # :ditto:
  def complaint(title : String)
    Complaint.new(title, [] of AttachmentRow)
  end

  # Annotates assertion node *asn* with location based on *srcmap*.
  def loc(asn : AssertionNode, srcmap : ML::SrcMap)
    return asn unless text = srcmap[Tpath[]]?

    _, line, col = ML::SyntaxError.lookaround(text)
    if asn.is_a?(AssertionLoc) && asn.linecol == {line, col}
      return asn
    end

    AssertionLoc.new(asn, {line, col})
  end

  # Enhances assertions from *asns* with location and term info.
  def annotated(asns : Array(AssertionNode), term : Term, srcmap : ML::SrcMap) : Array(AssertionNode)
    asns.map do |asn|
      # NOTE: The order isn't relevant here.
      loc(AssertionTerm.new(asn, term), srcmap).as(AssertionNode)
    end
  end

  # Calls the assert function.
  def call(assets : AssertionAssets, asn : Assertion | AssertionLoc) : AssertionResult
    call(assets, asn.successor)
  end

  # :ditto:
  def call(assets : AssertionAssets, asn : AssertionTerm) : AssertionResult
    call(assets, asn.successor)
  end

  # :ditto:
  def call(assets : AssertionAssets, asn : AssertionFn) : AssertionResult
    mmt, result = measure do
      asn.call(assets)
    rescue e : Exception
      e
    end

    if result.is_a?(Exception)
      complaint = complaint("💥 Assertion crashed", exception: result)
      result = AssertionResult.new(mmt, [complaint])
    end

    # result : AssertionResult
    result
  end

  # Returns the location ref for *asn*. The location ref includes the filename,
  # line, and column of origin for *asn*.
  def location(asn : Assertion(Test)) : String
    path = asn.topic.path
    unless point = point?(asn.successor)
      return "#{path}:???:???"
    end

    line, column = point

    "#{path}:#{line}:#{column}"
  end

  # Returns the location ref for *asn*. It refers to the line in the index file
  # that requested comparison.
  def location(asn : Assertion(Comparison)) : String
    asn.topic.ref
  end

  def point?(asn : AssertionLoc) : {Int32, Int32}?
    point?(asn.successor) || asn.linecol
  end

  def point?(asn : AssertionTerm) : {Int32, Int32}?
    point?(asn.successor)
  end

  def point?(asn : AssertionFn) : {Int32, Int32}?
  end

  # Returns the term associated with assertion *asn*.
  def term(asn : Assertion(Comparison)) : Term
    asn.topic.term
  end

  # Returns the term associated with assertion *asn*, if any.
  def term?(asn : Assertion(Test) | AssertionLoc) : Term?
    term?(asn.successor)
  end

  # :ditto:
  def term?(asn : AssertionTerm) : Term?
    asn.term
  end

  # :ditto:
  def term?(asn : AssertionFn) : Term?
  end
end
