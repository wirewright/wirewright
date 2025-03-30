require "./src/wirewright"

module Alloy
  extend self

  # :nodoc:
  record Context, vars : Term::Dict, templateR : Rewriter, exprR : Rewriter, errors = Stack(String).new do
    def error(&) : Nil
      errors << yield
    end
  end

  private def lookup(ctx : Context, term : Term) : Rewrite::Any
    value = ctx.vars[term]?
    value ? Rewrite.one(value) : Rewrite.none
  end

  # Attempts to resolve *term* as a variable.
  private def var(ctx : Context, term : Term) : Rewrite::Any
    return Rewrite.none unless term.type.symbol?

    id = term.to(String)

    if id.size > 2 && (suffix = id.lchop?("^*"))
      var = Term::Sym.new(suffix)

      unless value = ctx.vars[var]?
        ctx.error { "variable '#{var}' does not exist" }
        return Rewrite.none
      end

      unless list = value.as_itemsonly_d?
        ctx.error { "'#{var}' is not an itemsonly dict: '#{value}'" }
        return Rewrite.none
      end

      Rewrite.many(list)
    elsif id.size > 1 && (suffix = id.lchop?("^"))
      var = Term::Sym.new(suffix)

      unless value = ctx.vars[var]?
        ctx.error { "variable '#{var}' does not exist" }
        return Rewrite.none
      end

      Rewrite.one(value)
    else
      Rewrite.none
    end
  end

  # Attempts to parse *term* as `^paste` expression.
  private def paste(ctx : Context, term : Term) : Rewrite::Any
    variants = [] of String

    Term.case(term, patterns: variants) do
      matchpi %{(^paste var_symbol index←(%number i32))} do
        unless value = ctx.vars[var]?
          ctx.error { "variable '#{var}' does not exist" }
          continue
        end

        unless dict = value.as_d?
          ctx.error { "'#{var}' is not a dict: '#{value}'" }
          continue
        end

        unless item = dict.items[index.to(Int32)]?
          ctx.error { "'#{var}' dict does not have an item with index #{index}: #{dict}" }
          continue
        end

        Rewrite.one(item)
      end

      otherwise do
        ctx.error { "invalid ^paste expression, expected one of:\n#{variants.join('\n', &.li(bullet: "-", indent: 2))}" }

        Rewrite.none
      end
    end
  end

  # Attempts to parse *term* as `^*paste` expression.
  private def multipaste(ctx : Context, term : Term) : Rewrite::Any
    variants = [] of String

    Term.case(term, patterns: variants) do
      matchpi %{(^*paste var_symbol b←(%number i32) ..< e←(%number -i32))} do
        unless value = ctx.vars[var]?
          ctx.error { "variable '#{var}' does not exist" }
          continue
        end

        unless dict = value.as_d?
          ctx.error { "'#{var}' is not a dict: '#{value}'" }
          continue
        end

        Rewrite.many(dict.items.move(b.to(Int32)).grow(e.to(Int32)).collect)
      end

      matchpi %{(^*paste var_symbol b←(%number i32) ..= e←(%number -i32))} do
        unless value = ctx.vars[var]?
          ctx.error { "variable '#{var}' does not exist" }
          continue
        end

        unless dict = value.as_d?
          ctx.error { "'#{var}' is not a dict: '#{value}'" }
          continue
        end

        Rewrite.many(dict.items.move(b.to(Int32)).grow(e.to(Int32) + 1).collect)
      end

      otherwise do
        ctx.error { "invalid ^*paste expression, expected one of:\n#{variants.join('\n', &.li(bullet: "-", indent: 2))}" }

        Rewrite.none
      end
    end
  end

  # Attempts to parse *term* as `^*` expression.
  private def splice(ctx : Context, term : Term) : Rewrite::Any
    variants = [] of String

    Term.case(term, patterns: variants) do
      matchpi %{(^* children_*)} do
        Rewrite.many(children.unsafe_as_d)
      end

      otherwise do
        ctx.error { "invalid ^* expression, expected one of:\n#{variants.join('\n', &.li(bullet: "-", indent: 2))}" }

        Rewrite.none
      end
    end
  end

  # Attempts to parse *term* as `^match` expression.
  private def match(ctx : Context, term : Term) : Rewrite::Any
    variants = [] of String

    Term.case(term, patterns: variants) do
      matchp %{(^match cond_ (%many options (when pattern_ children_+)))} do |cond, options|
        matchee = rewrite(cond, ctx.exprR)

        rewrite = options.items.leftmost? do |optenv|
          next unless M1.probe?(optenv[:pattern], matchee, env: ctx.vars)

          Rewrite.many(optenv[:children].unsafe_as_d)
        end

        unless rewrite
          ctx.error { "^match does not cover this case: #{term}" }
        end

        rewrite || Rewrite.none
      end

      otherwise do
        ctx.error { "invalid ^match expression, expected one of:\n#{variants.join('\n', &.li(bullet: "-", indent: 2))}" }

        Rewrite.none
      end
    end
  end

  # Attempts to parse *term* as `^if` expression.
  private def mif(ctx : Context, term : Term) : Rewrite::Any
    variants = [] of String

    Term.case(term, patterns: variants) do
      matchpi %{(^if cond_ children_+)} do
        result = rewrite(cond, ctx.exprR)
        result == Term[false] ? Rewrite.many(Term[]) : Rewrite.many(children.unsafe_as_d)
      end

      otherwise do
        ctx.error { "invalid ^if expression, expected one of:\n#{variants.join('\n', &.li(bullet: "-", indent: 2))}" }

        Rewrite.none
      end
    end
  end

  # Attempts to parse *term* as `^unless` expression.
  private def munless(ctx : Context, term : Term) : Rewrite::Any
    variants = [] of String

    Term.case(term, patterns: variants) do
      matchpi %{(^unless cond_ children_+)} do
        result = rewrite(cond, ctx.exprR)
        result == Term[false] ? Rewrite.many(children.unsafe_as_d) : Rewrite.many(Term[])
      end

      otherwise do
        ctx.error { "invalid ^unless expression, expected one of:\n#{variants.join('\n', &.li(bullet: "-", indent: 2))}" }

        Rewrite.none
      end
    end
  end

  # Attempts to parse *term* as `^expr` expression.
  private def expr(ctx : Context, term : Term) : Rewrite::Any
    variants = [] of String

    Term.case(term, patterns: variants) do
      matchpi %{(^expr arg_)} do
        rewrite0(arg, ctx.exprR)
      end

      otherwise do
        ctx.error { "invalid ^expr expression, expected one of:\n#{variants.join('\n', &.li(bullet: "-", indent: 2))}" }

        Rewrite.none
      end
    end
  end

  def render(vars : Term::Dict, template : Term, *, strict : Bool = true)
    set_template, rec_template = recR
    set_expr, rec_expr = recR

    ctx = Context.new(vars, templateR: rec_template, exprR: rec_expr)

    exprR = set_expr.call switchR(
      { %{rewritee_dict}, chainR(entriesR(rec_expr), callR(PRIMITIVES)) },
      { %{rewritee_symbol}, callR(->lookup(Context, Term).partial(ctx)) },
    )

    rewriter = set_template.call switchR(
      { %{rewritee_symbol}, callR(->var(Context, Term).partial(ctx))},
      { %{rewritee←[^paste _*]}, callR(->paste(Context, Term).partial(ctx))},
      { %{rewritee←[^*paste _*]}, callR(->multipaste(Context, Term).partial(ctx))},
      { %{rewritee←[^* _*]}, chainR(callR(->splice(Context, Term).partial(ctx)), rec_template)},
      { %{rewritee←[^match _*]}, chainR(callR(->match(Context, Term).partial(ctx)), rec_template)},
      { %{rewritee←[^if _*]}, chainR(callR(->mif(Context, Term).partial(ctx)), rec_template)},
      { %{rewritee←[^unless _*]}, chainR(callR(->munless(Context, Term).partial(ctx)), rec_template)},
      { %{rewritee←[^expr _*]}, chainR(callR(->expr(Context, Term).partial(ctx)), rec_template)},
      { %{rewritee_dict}, entriesR(rec_template)},
    )

    render = rewrite(template, rewriter)

    if strict && ctx.errors.present?
      raise ctx.errors.join('\n')
    end

    render
  end
end

# {% skip_file %}

# templ = ML.term <<-WWML
# (^match (lhs rhs)
#   ;; If both are empty w-0 won't work so we have to create a rectangle
#   ;; that is explicitly w-px.
#   (when ("" "")
#     ((self rect) style: "w-px h-max bg-blue-500"))
#   (when _
#     (group style: "content"
#       ;; We could just leave either code empty here but to ease the load on
#       ;; uiR we'll avoid generating an extra empty code.
#       (^unless (= lhs "")
#         (code ^lhs style: "text-neutral-400 bg-neutral-700"))
#       ((self rect) style: "w-0 z-10 h-max bg-blue-500 ring-l ring-blue-500")
#       (^unless (= rhs "")
#         (code ^rhs style: "text-neutral-400 bg-neutral-700")))))
# WWML

# require "benchmark"

# Benchmark.ips do |x|
#   x.report("render first") do
#     Alloy.render(Term[lhs: "", rhs: ""], templ)
#   end
#   x.report("render second") do
#    Alloy.render(Term[lhs: "hello", rhs: ""], templ)
#   end
#   x.report("render third") do
#    Alloy.render(Term[lhs: "", rhs: "world"], templ)
#   end
#   x.report("render fourth") do
#    Alloy.render(Term[lhs: "hello", rhs: "world"], templ)
#   end
# end
# require "benchmark"

# Benchmark.ips do |x|
#   x.report("render") do
#     Alloy.render(Term[x: 100, y: 200, xs: {1, 2, 3, 4, 5}, hovered: true], templ, strict: true)
#   end
# end
