module Ww::Sparse::TermAST
  # Parses *source*, assumed to be a Sparse query. Returns the resulting Term AST.
  # The format of the AST is `(sparse ...)`. If could not parse, returns `nil`.
  #
  # A Sparse query is a subtraction (set difference) of one or more patterns. To
  # parse a single pattern, use `pattern?`.
  def query?(source : String) : Term?
    return unless tree = source.apply?(Sparse::Grammar.query, root: "sparse", exact: true)

    transcribe?(source, tree)
  end

  # Same as `query?`, but raises `SyntaxError` if could not parse.
  def query(source : String) : Term
    begin
      tree = source.apply!(Sparse::Grammar.query, root: "sparse", exact: true)
    rescue e : Sthx::SyntaxError
      raise SyntaxError.new
    end

    transcribe(source, tree)
  end

  # Parses *source*, assumed to be a Sparse pattern. Returns the resulting Term AST.
  # The format of the AST depends on the pattern that was parsed. If could not parse,
  # returns `nil`.
  #
  # Normally Sparse patterns are parts of a larger `query?`.
  def pattern?(source : String) : Term?
    return unless tree = source.apply?(Sparse::Grammar.pattern, exact: true)

    transcribe?(source, tree.children[0])
  end

  # Same as `pattern?`, but raises `SyntaxError` if could not parse.
  def pattern(source : String) : Term
    begin
      tree = source.apply!(Sparse::Grammar.pattern, exact: true)
    rescue e : Sthx::SyntaxError
      raise SyntaxError.new
    end

    transcribe(source, tree.children[0])
  end
end

require "./sparse/grammar"
require "./sparse/transcribe"
require "./sparse/translate"
require "./sparse/instance"
require "./sparse/xgraph"
