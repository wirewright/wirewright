module Ww::Sparse::Grammar
  extend self
  include Sthx
  include Sthx::DSL

  # :nodoc:
  EXPORTS = grammar

  # Returns the Synthax rule for a `Sparse::Query`.
  def query : Sthx::Rule
    EXPORTS[:query]
  end

  # Returns the Synthax rule for `Sparse::AST::Node` and its descendants.
  def pattern : Sthx::Rule
    EXPORTS[:pattern]
  end

  # Returns the Syntax rule for `Sparse::AST::Dict` (e.g. `{ x 100, y 200 }`).
  def dict : Sthx::Rule
    EXPORTS[:dict]
  end

  # Returns the Syntax rule for list `Sparse::AST::Dict` (e.g. `[100, 200]`).
  def list : Sthx::Rule
    EXPORTS[:list]
  end

  private def grammar
    _ws = some(' ' | '\r' | '\n' | '\t')
    _wsep = many(' ' | '\r' | '\n' | '\t')

    _digit = '0'..'9'
    _nonzero = '1'..'9'

    _alpha = ('a'..'z') | ('A'..'Z')
    _alnum = _alpha | _digit
    _id = _alpha & some(_alnum | '_')
    _id_ext = '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | '<' | '=' | '>' | '?' | '@' | '_' | '~' | 'λ'
    _id_symbol = (_alpha | _id_ext) & some(_alnum | _id_ext)

    _comma = _ws & ',' & _ws

    _positive_integer = '0' | _nonzero & some(maybe('_') & _digit)
    _integer = maybe('-') & _positive_integer
    _fractional = '.' & _digit & some(maybe('_') & _digit)
    _exponent = ('E' | 'e') & ('+' | '-') & many(_digit)

    _frac = keep(_integer, "num") & '/' & keep(_positive_integer - '0', "den")

    number = capture(_frac, "fraction") | capture(_integer & maybe(_fractional) & maybe(_exponent), "number")

    _hex = _digit | ('A'..'F') | ('a'..'f')
    _escape = '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | ('u' & _hex & _hex & _hex & _hex & _hex & _hex)
    _char = ((0x0020..0x10FFFF) - '"' - '\\') | ('\\' & _escape)

    string = '"' & capture(some(_char), "string") & '"'

    expr = ahead
    hole = ahead
    atom = ahead

    symbol = capture(_id, "symbol")
    literal = ("'" & capture(_id_symbol, "symbol")) | string | number | lit("true") | lit("false") | hole

    key = ('(' & _ws & literal & _ws & ')') | number | string | symbol | hole
    keys = sep(key, by: '.')

    hole_default = "(" & _ws & keys & _wsep & "or" & _wsep & expr & _ws & ")"
    hole_nodefault = keys | ("(" & _ws & keys & _ws & ")")
    hole_self = maybe(_wsep & "or" & _wsep & atom)
    hole.put \
      capture(keep("\\@" | '@', "kind") & hole_default, "hole/default") |
      capture(keep("\\@" | '@', "kind") & hole_nodefault, "hole/nodefault") |
      capture(keep("\\@" | '@', "kind") & hole_self, "hole/self")

    paren_hole = '(' & _ws & hole & _ws & ')' | hole

    range_b = capture(number | paren_hole, ":begin:") & _ws & maybe(keep('<' | '=', "bop"))
    range_e = keep('<' | '=', "eop") & _ws & capture(number | paren_hole, ":end:")
    range = capture((range_b & ".." & range_e) | (range_b & "..") | (".." & range_e) | "..", "range")

    div_by = capture("/?" & _ws & (capture(_integer - '0', "number") | paren_hole), "div_by")

    capsule = '(' & _ws & expr & _ws & ')'

    call_head_named = keep(_id, "name") & '(' & (sep(literal, by: _comma) | _ws) & ')'
    call_head_operator = keep("**" | '+' | '-' | '*' | '/', "name") & _wsep & (number | paren_hole)
    call_head = call_head_named | call_head_operator

    call = capture(capture(call_head, ":head:") & _wsep & (sep(capsule, by: _wsep) | expr), "call")

    conjunct = call | atom
    conjunction = capture(sep(conjunct, by: _wsep), "conjunction")

    pair_key = capture(keys, "pair/key")
    pair_hole = capture(hole, "pair/hole")
    pair_entry = capture(keys & _ws & maybe(':') & _ws & expr, "pair/entry")
    pair = pair_key | pair_entry | pair_hole
    pair_nokey = pair_entry | pair_hole

    pairs = some((pair_key & '.' & _ws) | (pair_nokey & _comma)) & maybe(pair_nokey)

    dict_multi = capture('{' & _ws & pairs & _ws & '}', "dict")
    dict_empty = capture('{' & _ws & '}', "emptydict")
    dict = dict_empty | dict_multi

    list = capture('[' & (_ws & sep(expr, by: _comma) & (_comma | _ws) | _ws) & ']', "list")
    _empty_list = '[' & _ws & ']'

    # Easy cases are listed before more ambiguous ones.
    atom.put \
      capture("any" | '_', "any") |
      capture("number" | "string" | "boolean" | "symbol", "typename") |
      capture("dict" | _empty_list, "emptydict") |
      capsule | div_by | dict | list | # all have unambiguous first character(s)
      range |                          # can begin with literal, must come before it
      literal

    rescuer = capture(_ws & sep(conjunction, by: _ws & "rescue" & _ws) & _ws, "rescuer")

    expr.put(rescuer)

    toplevel = expr | capture("all" | "*", "any")

    pattern = _ws & expr & _ws
    query = _ws & sep(toplevel, by: _ws & '\\' & _ws) & _ws

    {query: query, pattern: pattern, dict: dict, list: list}
  end
end
