require "csv"
require "benchmark"
require "./wirewright"
require "../rack10-clf"

module Testtool
  include Ww

  alias Topic = Test | Comparison

  defcase Test,
    path : Path,
    document : Term,
    srcmap : ML::SrcMap,
    color : Pigment::RGBA,
    ref : String

  defcase Comparison,
    title : String,
    op : Any,
    color : Pigment::RGBA,
    ref : String,
    term : Term

  alias Comparison::Any = TermComparison | ImageComparison

  defcase AssertionAssets,
    vars : Hash(Term, Term),
    theme : Microfold::Theme,
    editR : Rewriter,
    uiR : Rewriter,
    files : FileServer,
    dw : Channel(DwUIR::Request)

  alias AssertionNode = AssertionLoc | AssertionTerm | AssertionFn
  alias AssertionFn = AssertionAssets -> AssertionResult

  defcase Assertion(T), successor : AssertionNode, topic : T
  defcase AssertionLoc, successor : AssertionNode, linecol : {Int32, Int32}
  defcase AssertionTerm, successor : AssertionNode, term : Term

  defrecord AssertionResult, mmt : Mmt, complaints : Array(Complaint)
end

# Utility methods and data structures.
require "./testtool/kit"

# Display methods. This file contains methods that write to STDERR/STDOUT. We
# use them all throughout the testtool.
require "./testtool/display"

# `Testtool.run` is the "meat" of testtool. Its various overloads run `Leaf` tests.
require "./testtool/run"

# Methods related to comparison tests (e.g. term to term or image to image).
require "./testtool/comparison"

# The overloads of `Testtool.assertions` descend recursively into `Test`s until
# they hit `Leaf` tests, which are then passed to `Testtool.run`.
require "./testtool/assertions"

# Command-line interface.
require "./testtool/main"

Testtool.main(ARGV)
