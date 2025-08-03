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

    # Attach location info under symbol keys `Term::Sym.byte_start` and `Term::Sym.byte_end`
    # to all dictionary terms of the normal form except those found inside entry keys.
    #
    # If an interfix is used, location info is attached to the outer dict.
    # For instance, in `(+ 1 2)`, one will have `(+ 1 2 <byte-start>: _ <byte-end>: _)`,
    # but in `(+ 1 2 ¦ _)` one will have `(%partition (+ 1 2) _ <byte-start>: _ <byte-end>: _)`
    #
    # Locations are irrepresentible and intrusive symbols. ML assumes the client
    # will first and foremost prune them from the term. The location addon is
    # the most powerful and "bare bones" way to do this. Use e.g. `ML.term_and_srcmap`
    # and related if you don't want to bother implementing location collection
    # and pruning yourself.
    Location

    # Returns the recommended set of addons.
    def self.recommended : Addons
      DocComment
    end
  end

  # Constructs an itemsonly dict of terms read from the given WwML
  # *source* string.
  #
  # Raises `SyntaxError` on invalid input.
  def terms(source : String, *, filename : String = "scratch", addons : Addons = Addons.recommended) : Term
    begin
      atoms = Lexeme.atoms(source)
    rescue e : SyntaxError
      # Lexical error
      e.filename = filename
      raise e
    end

    reader = Reader.new(atoms, addons: addons)

    case π = Reader.validated(source, reader.section)
    in Reader::Parseout::Ok
      π.term
    in Reader::Parseout::Err
      # Parse error
      raise SyntaxError.new(π.detail, π.text, filename: filename)
    end
  end

  # Same as `terms`, but downcasts the resulting term to a dictionary.
  def dict(source : String, **kwargs) : Term::Dict
    terms(source, **kwargs).as_d
  end

  # Constructs a term from the given WwML *source* string.
  #
  # Raises `SyntaxError` on invalid input.
  def term(source : String, *, filename : String = "scratch", addons : Addons = Addons.recommended) : Term
    begin
      atoms = Lexeme.atoms(source)
    rescue e : SyntaxError
      # Lexical error
      e.filename = filename
      raise e
    end

    reader = Reader.new(atoms, addons: addons)

    case π = Reader.validated(source, reader.item)
    in Reader::Parseout::Ok
      π.term
    in Reader::Parseout::Err
      # Parse error
      raise SyntaxError.new(π.detail, π.text, filename: filename)
    end
  end

  # Constructs a document term from the given WwML *source* string.
  #
  # Raises `SyntaxError` on invalid input.
  def document(source : String, *, filename : String = "scratch", addons : Addons = Addons.recommended) : Term
    begin
      atoms = Lexeme.atoms(source)
    rescue e : SyntaxError
      # Lexical error
      e.filename = filename
      raise e
    end

    reader = Reader.new(atoms, addons: addons)

    case π = Reader.validated(source, reader.document)
    in Reader::Parseout::Ok
      π.term
    in Reader::Parseout::Err
      # Parse error
      raise SyntaxError.new(π.detail, π.text, filename: filename)
    end
  end

  alias SrcMap = Hash(Term::Dict, StringView)

  {% for method in %w[term terms document] %}
    # Same as `{{method.id}}`, but also builds a *source map*. The source map
    # maps keypaths into the returned term to corresponding views of *source*.
    #
    # NOTE: the source map may be missing some keypaths; be prepared to handle
    # that if you must.
    #
    # NOTE: this method is not expected to be fast.
    def {{method.id}}_and_srcmap(source : String, **kwargs) : {Term, SrcMap}
      addons = kwargs[:addons]? || Addons.recommended
      if addons.location?
        raise ArgumentError.new("use of location addon conflicts with `term_and_srcmap`")
      end
      addons |= ML::Addons::Location

      term = {{method.id}}(source, **kwargs.merge({addons: addons}))
      term, srcmap = pruned_term_and_srcmap(source, term)

      {term, srcmap}
    end
  {% end %}

  private def pruned_term_and_srcmap(source : String, term : Term)
    srcmap = {} of Term::Dict => StringView

    # Read locations off the parsed term and bind under their corresponding keypath.
    Term.each_keypath_and_node(term) do |keypath, node|
      # Do not emit location for root, too coarse.
      next true if keypath.empty?

      next true unless nodedict = node.as_d?
      next true unless b = nodedict[Term::Sym.byte_start]?
      next true unless e = nodedict[Term::Sym.byte_end]?
      next true unless b = b.to?(Int32)
      next true unless e = e.to?(Int32)

      srcmap[Term[keypath]] = source.view(b, byte_end: e)

      true # continue
    end

    # Prune locations.
    term = Term.patch(term) do |leaf|
      unless leafdict = leaf.as_d?
        next Term::Patch::Skip.new
      end

      rep = leafdict
        .without(Term::Sym.byte_start)
        .without(Term::Sym.byte_end)

      Term::Patch::ReplaceDescend.new(Term.of(rep))
    end

    {term, srcmap}
  end

  # :nodoc:
  private def edge?(term : Term::Dict, type : TermType) : Bool
    return false unless term.itemsonly?
    return false unless term.size == 2
    return false unless term.probably_includes?(SYM_EDGE)

    term[0] == SYM_EDGE && term[1].type.subtype?(type)
  end

  # :nodoc:
  private def edge?(term : ITerm, type : TermType) : Bool
    false
  end

  # Returns `true` if *term* is a well-formed edge. Returns `false` otherwise.
  #
  # This is just a "hand-optimized" version of the pattern `(%'edge _)`.
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

  # Returns `true` if a symbol with the given *name* has a representation in WwML.
  # Returns `false` otherwise.
  def can_represent_symbol?(name : String) : Bool
    case name
    when "true", "false",
         .prefixed_by?('\''),
         .starts_with?('0'..'9')
      false
    else
      true
    end
  end
end

require "./ml/syntax_error"
require "./ml/rune"
require "./ml/kit"
require "./ml/lexeme"
require "./ml/reader"

require "./ml/display"
