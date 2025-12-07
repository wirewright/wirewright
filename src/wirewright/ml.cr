module Ww::ML
  extend self

  @[Flags]
  enum Addons
    # Attach comments immediately preceding the rule/backmap shorthand syntax
    # to those rules/backmaps under `doc`, as a list of string lines.
    #
    # ```wwml
    # ;; Lorem ipsum dolor sit amet, qui minim labore adipisicing minim
    # ;; sint cillum sint consectetur cupidatat.
    # (frobnicate x_) => (* x 2)
    # ```
    #
    # Is read as:
    #
    # ```wwml
    # (rule doc: ("Lorem ipsum dolor sit amet, qui minim labore adipisicing minim"
    #             "sint cillum sint consectetur cupidatat.")
    #   (frobnicate x_)
    #   (* x 2))
    # ```
    DocComment

    # Track locations of nodes. AST nodes will be wrapped in special location
    # nodes. Usually this happens only during a reparse on syntax error, but
    # with the `Location` addon, this will always happen.
    #
    # This addon is a pre-requisite for using `SrcMap`.
    Location

    # Returns the recommended set of addons.
    def self.recommended : Addons
      DocComment
    end
  end

  # **The main interface to the WwML lexer**. Converts a source *string* to
  # a read-only slice of lexical atoms, ready to be used by `Reader`.
  #
  # ```
  # ML.lexemes("(+ 1 2)").map(&.text) # => Slice[…""…, …"("…, …"+"…, …"1"…, …"2"…, …")"…, …""…]
  # ```
  def lexemes(string : String) : Slice(Lexeme::Atom)
    pipe(string, Lexeme.lexemes, Lexeme.atoms)
  end

  # **The main interface to the WwML reader (parser)**. Constructs a reader and
  # yields it to the block, letting it read using its method of choice
  # (e.g. `document`, `slot`, etc.) Returns the resulting tree. If parsing fails
  # with `Err`, converts it to a `SyntaxError` and raises.
  #
  # ```
  # lexemes = ML.lexemes("100")
  # tree = ML.tree(lexemes, ML::Addons::None, &.slot)
  # tree # => #<Ww::ML::Tree::Leaf:0x... @term=100>
  # ```
  def tree(lexemes : Slice(Lexeme::Atom), addons : Addons, & : Reader -> _)
    reader = Reader.new(lexemes, addons)

    π = reader.top { yield reader }
    if π.is_a?(Reader::Err)
      raise SyntaxError.new(π.detail, π.text)
    end

    π
  end

  # Shorthand for the sequence `lexemes` -> `tree`.
  #
  # ```
  # tree = ML.tree("100", ML::Addons::None, &.slot)
  # tree # => #<Ww::ML::Tree::Leaf:0x... @term=100>
  # ```
  def tree(source : String, addons : Addons, & : Reader -> _)
    tree(lexemes(source), addons) { |reader| yield reader }
  end

  # Renders *node* (one of `Tree` nodes) without source-mapping. Returns
  # the resulting term.
  def render(node) : Term
    tsrc = Renderer(UntrackedTsrc).render(node)
    tsrc.term
  end

  # Renders *node* (one of `Tree` nodes) with source-mapping. Returns
  # the resulting term and source map.
  def render_with_srcmap(node) : {Term, SrcMap}
    tsrc = Renderer(TrackedTsrc).render(node)

    {tsrc.term, SrcMap.new(tsrc.srcmap)}
  end

  # Keyword arguments (configuration) shared between all of `term*`, `terms*`,
  # `document*` and derived.
  #
  # - *filename* specifies the file name to use in syntax errors.
  # - *doc* enables or disables doc parsing. This acts as a toggle for
  #   `Addons::DocComment`. See `Addons::DocComment` for more info.
  record Conf, filename = "scratch", doc = true do
    # :nodoc:
    def addons
      doc ? Addons::DocComment | Addons::None : Addons::None
    end
  end

  {% for suffix in ["", "_with_srcmap"] %}
    private def parse{{suffix.id}}(filename : String, source : String, addons : Addons, &fn : Reader -> _)
      {% if suffix == "_with_srcmap" %}
        addons |= Addons::Location
      {% end %}

      tree = tree(source, addons, &fn)

      begin
        begin
          render{{suffix.id}}(tree)
        rescue e : Renderer::RenderError
          raise SyntaxError.new(e.detail, e.text? || source.view)
        end
      rescue e : SyntaxError
        # Re-parse with location turned on if it was turned off. This way, we'll get
        # proper error message.
        unless addons.location?
          _ = parse(filename, source, addons | Addons::Location, &fn)
          # ... Re-raises. We do not expect it to succeed. If it does, we
          # re-raise `e`.
        end

        raise e
      end
    rescue e : SyntaxError
      e.filename = filename
      raise e
    end

    private def parse{{suffix.id}}(conf : Conf, source : String, &fn : Reader -> _)
      parse{{suffix.id}}(conf.filename, source, conf.addons, &fn)
    end
  {% end %}

  # Constructs a term from the given WwML *source* string.
  #
  # See `Conf` to learn about *kwargs*.
  #
  # Raises `SyntaxError` on invalid input.
  def term(source : String, **kwargs) : Term
    conf = Conf.new(**kwargs)
    row = parse(conf, source, &.section(allow_empty: true))
    unless row.itemsonly? && row.itemsize == 1
      raise SyntaxError.new("expected a single top-level term", source.view, filename: conf.filename)
    end

    row[0]
  end

  # Constructs a term from the given WwML *source* string. Supplements it with
  # a source map mapping termpaths into the returned term to corresponding views
  # of *source* code.
  #
  # See `Conf` to learn about *kwargs*.
  #
  # Raises `SyntaxError` on invalid input.
  def term_and_srcmap(source : String, **kwargs) : {Term, SrcMap}
    conf = Conf.new(**kwargs)
    row, srcmap = parse_with_srcmap(conf, source, &.section(allow_empty: true))
    unless row.itemsonly? && row.itemsize == 1
      raise SyntaxError.new("expected a single top-level term", source.view, filename: conf.filename)
    end

    {row[0], srcmap.cd(0)}
  end

  # Constructs an itemsonly dict of terms read from the given WwML
  # *source* string.
  #
  # Raises `SyntaxError` on invalid input.
  def terms(source : String, **kwargs) : Term
    parse(Conf.new(**kwargs), source, &.section(allow_empty: true))
  end

  # Constructs an itemsonly dict of terms read from the given WwML *source*
  # string. Supplements it with a source map mapping termpaths into the returned
  # term to corresponding views of *source* code.
  #
  # See `Conf` to learn about *kwargs*.
  #
  # Raises `SyntaxError` on invalid input.
  def terms_and_srcmap(source : String, **kwargs) : {Term, SrcMap}
    parse_with_srcmap(Conf.new(**kwargs), source, &.section(allow_empty: true))
  end

  # Same as `terms`, but downcasts the resulting term to a dictionary.
  #
  # See `Conf` to learn about *kwargs*.
  def dict(source : String, **kwargs) : Term::Dict
    terms(source, **kwargs).as_d
  end

  # Constructs a document term from the given WwML *source* string.
  #
  # See `Conf` to learn about *kwargs*.
  #
  # Raises `SyntaxError` on invalid input.
  def document(source : String, **kwargs) : Term
    parse(Conf.new(**kwargs), source, &.document)
  end

  # Constructs a document term from the given WwML *source* string. Supplements
  # it with a source map mapping termpaths into the returned term to corresponding
  # views of *source* code.
  #
  # See `Conf` to learn about *kwargs*.
  #
  # Raises `SyntaxError` on invalid input.
  def document_and_srcmap(source : String, **kwargs) : {Term, SrcMap}
    parse_with_srcmap(Conf.new(**kwargs), source, &.document)
  end

  # :nodoc:
  private def edge?(term : Term::Dict, type : TermType) : Bool
    return false unless term.itemsonly?
    return false unless term.size == 2
    return false unless term.probably_includes?(SYM_EDGE)

    term[0] == SYM_EDGE && term[1].type.subtype?(type)
  end

  # :nodoc:
  private def edge?(term : Term::Any, type : TermType) : Bool
    false
  end

  # Returns `true` if *term* is a well-formed edge. Returns `false` otherwise.
  #
  # This is just a "hand-optimized" version of the pattern `(%'edge _)`.
  #
  # TODO: move to `Term.edge?`
  def edge?(term : Term, *, type : TermType = TermType::Any) : Bool
    return false unless term.type.dict?

    edge?(term.unsafe_as_d, type)
  end

  # Returns the initial value for the given term *type*.
  #
  # See also the WwML spec, section "M1 Key-value pair shorthands", subsection
  # "Initial values".
  #
  # Raises `ArgumentError` if *type* is `any`.
  def initial(type : TermType) : Term
    case type
    in .any?     then raise ArgumentError.new("TermType::Any initial value is undefined")
    in .boolean? then Term.of(false)
    in .dict?    then Term.of
    in .number?  then Term.of(0)
    in .string?  then Term.of("")
    in .symbol?  then Term.of(:unset)
    end
  end

  # Returns `true` if a symbol with the given *name* must be represented without using
  # the raw symbol literal, `⸍...⸝`. Returns `false` if the raw string literal must be used.
  def symbol_bare?(name : String) : Bool
    # NOTE: Unfortunately, in WwML, symbols are *very* ambiguous in terms of parsing.
    # So we have to resort to a series of fast paths which are hit maybe in 90% of
    # the cases, if not more; followed by a general slow path: parse *name* and see
    # if the result is a symbol with the same name.

    case name
    when .empty?
      false
    when "true", "false",
         .prefixed_by?('\''),
         .starts_with?('0'..'9')
      false
    when "$", "%", "+", "-", "^", "<", ">", "="
      true
    when "%-", "$my", "$up", "$down", "$once"
      true
    else
      reader = Char::Reader.new(name)

      # If it starts with symbolic strong, use bare.
      if Rune.new(reader.current_char).symbolic_strong?
        if reader.all? { |chr| Rune.new(chr).symbolic? }
          return true
        end

        reader.pos = 0
      end

      # If it starts with '%' and consists of symbolic strong, use bare.
      if reader.current_char == '%'
        reader.next_char
        if reader.all? { |chr| Rune.new(chr).symbolic_strong? }
          return true
        end

        reader.pos = 0
      end

      # Slow path.
      begin
        term = term(name)
      rescue e : SyntaxError
        # Lexical error, can't go bare.
        return false
      end

      term.type.symbol? && term.to(String) == name
    end
  end
end

require "./ml/syntax_error"
require "./ml/rune"
require "./ml/kit"
require "./ml/lexeme"
require "./ml/lexer"
require "./ml/tree"
require "./ml/reader"
require "./ml/srcmap"
require "./ml/tsrc"
require "./ml/renderer"
require "./ml/display"
