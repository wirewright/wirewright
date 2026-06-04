require "csv"
require "benchmark"
require "./wirewright"

module Testtool
  include Ww

  alias Topic = Test | SceneryGroup | Comparison

  defcase Test,
    path : NormalPath,
    document : Term,
    srcmap : ML::SrcMap,
    color : Pigment::RGBA,
    ref : String

  defcase SceneryGroup,
    path : NormalPath,
    color : Pigment::RGBA,
    ref : String,
    term : Term

  defcase Comparison,
    title : String,
    op : Any,
    color : Pigment::RGBA,
    ref : String,
    term : Term

  alias Comparison::Any = TermComparison | ImageComparison

  {% if flag?(:dwuir) %}
    defcase AssertionAssets,
      mu_codex : Microfold::SyncCodex?,
      editR : Rho::Rewriter?,
      uiR : Rewriter?,
      dw : Channel(DwUIR::Request)
  {% else %}
    defcase AssertionAssets,
      mu_codex : Microfold::SyncCodex?,
      editR : Rho::Rewriter?
  {% end %}

  alias AssertionNode = AssertionLoc | AssertionTerm | AssertionFn
  alias AssertionFn = AssertionAssets -> AssertionResult

  defcase Assertion(T), successor : AssertionNode, topic : T
  defcase AssertionLoc, successor : AssertionNode, linecol : {Int32, Int32}
  defcase AssertionTerm, successor : AssertionNode, term : Term

  defrecord AssertionResult, mmt : Mmt, complaints : Array(Complaint)
end

# Utility functions and data structures.
require "./testtool/kit"

# Display functions. This file contains functions that write to STDERR/STDOUT. We
# use them all throughout the testtool.
require "./testtool/display"

# `Testtool.run` is the "meat" of testtool. Its various overloads run `Leaf` tests.
require "./testtool/run"

# Functions related to comparison tests (e.g. term to term or image to image).
require "./testtool/comparison"

# The overloads of `Testtool.assertions` look for assertions: they descend
# recursively into `Test`s until they hit `Leaf` tests, which are then passed
# to `Testtool.run`.
require "./testtool/assertions"

# Command-line interface.
require "./testtool/main"

Testtool.main(ARGV)
