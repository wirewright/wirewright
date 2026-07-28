# |@ alloy
#
# |@summary
# A structural templating language.
#
# |@block
# Alloy is a structural templating language for Wirewright.
#
# *Structural templating* is like Lisp's `quote`, `unquote`, `quasiquote`, etc.
# Alloy can also be compared with something like Handlebars, except Alloy operates
# on terms rather than strings. Another comparison one could make is to JSX. Basically,
# Alloy lets you "plug" things into a prepared literal term.
#
# The easiest way to access Alloy directly is through Rack, using nodes such
# as `rack.transfer`, `rack.view`, `rack.extension`.
#
# |@example
# Consider the following *template*:
#
# ```wwml
# (^let names: ("Alice" "Bob" "Dave" "Jane")
#   (^each (names as name_)
#     (Person ^name)))
# ```
#
# It *expands* into multiple things at once:
#
# ```wwml
# (Person "Alice")
# (Person "Bob")
# (Person "Dave")
# (Person "Jane")
# ```
module Ww::Alloy
  extend self

  defcase CompiledTemplate, unit : Unit, reftab : Reftab do
    # Returns the **approximate** bytesize of this compiled template.
    #
    # See also: `Unit#approx_bytesize`.
    def bytesize : UInt64
      # Assume, crudely, that each entry in Reftab is 8 bytes for key (which is a ptr),
      # and 32 bytes for value (Set(Ref); assuming Ref is ptr-sized, we get ~4 elements
      # per set).
      bytesize = unit.approx_bytesize
      if reftab = @reftab.as?(Reftab)
        bytesize += reftab.size*(8 + instance_sizeof(Hash(Ref, Nil)) + 32)
      end

      bytesize
    end
  end

  # Compiles the given Alloy *sheet*.
  #
  # NOTE: Sheets compilation is not cached on the Alloy side.
  def compile(sheet : Sheet) : CompiledTemplate
    unit = recognize(sheet)
    reftab = refs(unit)

    CompiledTemplate.new(unit, reftab)
  end

  @@bank = ThresholdLRU(Term, CompiledTemplate).new(
    capacity: 128,
    threshold: 256u64 * 1024 * 1024, # 256 MiB
  )

  @@lock = Sync::Mutex.new

  # Compiles the given Alloy *template*.
  #
  # Template compilation is cached. Repeated calls to `compile` with the same
  # template are expected to hit the cache most of the time.
  def compile(template : Term) : CompiledTemplate
    if comp = @@lock.synchronize { @@bank.get?(template) }
      return comp
    end

    unit = recognize(template)
    comp = CompiledTemplate.new(unit, reftab: Reftab.new)

    @@lock.synchronize { @@bank.put(template, comp) }

    comp
  end

  # An Alloy *rewriter* exists to prevent the issue of "Alloy injection".
  #
  # Sometimes you only provide the set of *components*; and the template
  # is provided by the user. You might not want the user to have access
  # to Alloy constructs (e.g. `^each`) in the template. You can use an Alloy
  # rewriter for this.
  #
  # A `Rewriter` "drills" down the template to find component calls without
  # checking for Alloy template expressions such as `^each`, variables such
  # as `^x`, etc. Therefore, it becomes impossible to "inject" Alloy the way
  # it is possible if you used `render` instead.
  defrecord Rewriter, components : ComponentSet, reftab : Reftab

  # Constructs an Alloy `Rewriter` from *ruleset*.
  def rewriter(ruleset : Term) : Rewriter
    # TODO: avoid processing non-rules, avoid generating templates.
    unit = recognize(sheet(ruleset))
    reftab = refs(unit)

    Rewriter.new(unit.components, reftab)
  end

  def rewrite(ctx : RenderContext, rdata : RuleData, globals : Term::Dict, term : Term) : Term::Rep
    rep = call(ctx, rdata, globals, term)
    if Term.changes?(term, after: rep)
      return rep
    end

    # No component responded positively.
    unless dict = term.as_d?
      return Term.rep(term)
    end

    # Rewrite entry values recursively (descR).
    dict = Term.flatten(dict, part: Term::Dict.entries) do |_, value|
      rewrite(ctx, rdata, globals, value)
    end

    Term.rep_of(dict)
  end

  def rewrite(ctx : RenderContext, term : Term, components : ComponentSet) : Term::Rep
    rdata = RuleData.new(components, MAXDEPTH)
    rewrite(ctx, rdata, Term[], term)
  end

  def rewrite_rep(rewriter : Rewriter, term : Term, *, cache : RenderCache = Uncached(RenderKey, Term::Rep).new) : Term::Rep
    reftab = rewriter.reftab
    ctx = RenderContext.new(cache, reftab, Nitrene.primitive, Nitrene.composite)

    rewrite(ctx, term, rewriter.components)
  end

  def rewrite(rewriter : Rewriter, term : Term, **kwargs) : Term
    Term.collapse(rewrite_rep(rewriter, term, **kwargs))
  end

  # Renders a compiled *template*, returns the resulting term replacement.
  #
  # - *globals* is a dict containing global variables. Globals are accessible
  #   from all rules. Globals are inaccessible from rules and templates inside
  #   `^module`s which do not explicitly include them.
  # - *locals* is a dict containing local variables. Locals are only accessible
  #   in *template* but not in rules called by *template*; they must be passed
  #   to the rules explicitly.
  # - *cache* is the cache to store renders in (this requires *template* to have
  #   been compiled with ref tracking enabled).
  # - *primitive* provides an **extension** of Nitrene's `Nitrene.primitive`
  #   (the former runs before the latter).
  # - *composite* provides an **extension** of Nitrene's `Nitrene.composite`
  #   (the former runs before the latter).
  def render_rep(
    template : CompiledTemplate, *,
    globals : Term::Dict = Term[],
    locals : Term::Dict = Term[],
    cache : RenderCache = Uncached(RenderKey, Term::Rep).new,
    primitive : Nitrene::Eval = Nitrene::Eval.new { Nitrene.inert },
    composite : Nitrene::Eval = Nitrene::Eval.new { Nitrene.inert },
  ) : Term::Rep
    ctx = RenderContext.new(cache, template.reftab,
      Nitrene.either(primitive, Nitrene.primitive),
      Nitrene.either(Alloy.composite, composite, Nitrene.composite),
    )

    render(ctx, globals, locals, template.unit)
  end

  # Alloy's contribution to the composite eval of Nitrene. In particular,
  # here we make `(^ _)` not evaluate so that expressions such as `^^(+ x 1)`
  # evaluate to `^(+ x 1)`, "peeling off" `^`s.
  protected def composite : Nitrene::Eval
    ->(it : Nitrene::Interpreter, vars : Term::Dict, expr : Term) do
      Term.case(expr) do
        matchpi %{(^ _)} do
          expr
        end

        otherwise do
          Nitrene.inert
        end
      end
    end
  end

  # A shorthand for `render_rep` that compiles *template* before rendering it
  # according to *kwargs*.
  def render_rep(template : Term | Sheet, **kwargs) : Term::Rep
    render_rep(compile(template), **kwargs)
  end

  # A shorthand for `render_rep` that collapses the resulting replacement.
  def render(*args, **kwargs) : Term
    Term.collapse(render_rep(*args, **kwargs))
  end

  # Renders *template* based on the locals present in *vars*. Returns
  # the resulting term.
  def render(vars : Term::Dict, template : Term) : Term
    render(template, locals: vars)
  end
end

require "./alloy/template"
require "./alloy/recognize"
require "./alloy/register"
require "./alloy/render"
