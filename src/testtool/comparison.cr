module Testtool
  alias Comparand = TermComparand | ImageComparand

  alias TermComparand = MLdoc | LRdoc

  defrecord MLdoc, source : String
  defrecord LRdoc, source : Bytes

  alias ImageComparand = DwDoc | UIRdoc | Ppm
  alias ImageComparandWithTemp = DwDoc | UIRdoc

  defrecord Ppm, source : Bytes
  defrecord DwDoc, source : String, vars : Term::Dict, temp : String
  defrecord UIRdoc, source : String, globals : Term::Dict, temp : String

  # Parses and resolves a comparand *term*.
  #
  # May raise `ResourceService::Error`.
  # May raise `ArgumentError`.
  def comparand(base : Path, term : Term) : Comparand
    Term.case(term, engine: M0) do
      matchpi %{(ml path_string)}, path: Path do
        MLdoc.new(pipe(base / path, ResourceService.file, ResourceService.read_string))
      end

      matchpi %{(lr.gz path_string)}, path: Path do
        LRdoc.new(pipe(base / path, ResourceService.file, ResourceService.read_blob, Compress::Gzip.decompress))
      end

      matchpi %{(dwuir path_string ¦ vars_)}, path: Path, vars: Term::Dict do
        DwDoc.new(pipe(base / path, ResourceService.file, ResourceService.read_string), vars, temp: "dwuir")
      end

      matchpi %{(uir path_string ¦ globals_)}, path: Path, globals: Term::Dict do
        UIRdoc.new(pipe(base / path, ResourceService.file, ResourceService.read_string), globals, temp: "uir")
      end

      matchpi %{(ppm path_string)}, path: Path do
        Ppm.new(pipe(base / path, ResourceService.file, ResourceService.read_blob).to_slice)
      end

      matchpi %{(ppm.gz path_string)}, path: Path do
        Ppm.new(pipe(base / path, ResourceService.file, ResourceService.read_blob, Compress::Gzip.decompress))
      end
    end
  end

  # :nodoc:
  def comparison?(a : TermComparand, b : TermComparand) : Comparison::Any?
    TermComparison.new(a, b)
  end

  # :nodoc:
  def comparison?(a : ImageComparand, b : ImageComparand) : Comparison::Any?
    ImageComparison.new(a, b)
  end

  # Constructs a comparison object if comparison is possible between *a* and *b*.
  def comparison?(a, b) : Comparison::Any?
  end

  def termcmp(comparand : MLdoc) : Term
    ML.document(comparand.source, doc: false)
  end

  def termcmp(comparand : LRdoc) : Term
    io = IO::Memory.new(comparand.source)

    LR.decode(io)
  end

  # NOTE: The current implementation of DwUIR is really really messy so the best
  # we can do is compare PPMs of images. There's no good way to encode Ww-specific
  # "image" right now. There's PixelRect which is supposed to be readonly (but of course
  # it isn't because its *very* well-designed). Then there's Layer which is a mess of
  # its own. We should obviously have some entity that represents a generic, immutable image,
  # a "Frame", perhaps, that we can convert to various image formats and so on like normal
  # people. But currently that's only a far-away dream.

  def ppmcmp(dw, uiR, comparand : DwDoc) : Bytes
    doc = ML.document(comparand.source, doc: false)
    dwuir = Alloy.render(comparand.vars, doc)

    conf = Term.matchpiT(dwuir, %[{¦ initial-w: w←(%number +i32) initial-h: h←(%number +i32) backdrop_}]) do
      DwUIR::ShowConf.new(w, h,
        backdrop: Pigment.rgba(backdrop, fallback: Pigment.named("white")),
        content: Term.of(dwuir.itemspart),
      )
    end

    data = Sync::Future(Bytes).new
    dw << DwUIR::SnapRequest.new(conf, DwUIR::SnapFormat["ppm"], data)
    data.get
  end

  def ppmcmp(dw, uiR, comparand : UIRdoc) : Bytes
    doc = ML.document(comparand.source, doc: false)
    ruleset, rest = Ruleset.ruleset_and_rest(Ruleset::DEFAULT_SELECTOR, doc)
    uir = Alloy.compose(ruleset, comparand.globals, Alloy.template(Term[], Term.of(rest)))
    dwuir = rewrite(uir, uiR)

    conf = Term.matchpiT(dwuir, %[{¦ content-w: w←(%number +i32) content-h: h←(%number +i32) fill_}]) do
      DwUIR::ShowConf.new(w, h,
        backdrop: Pigment.rgba(fill, fallback: Pigment.named("white")),
        content: Term.of(dwuir.itemspart),
      )
    end

    data = Sync::Future(Bytes).new
    dw << DwUIR::SnapRequest.new(conf, DwUIR::SnapFormat["ppm"], data)
    data.get
  end

  def ppmcmp(dw, uiR, comparand : Ppm) : Bytes
    comparand.source
  end
end
