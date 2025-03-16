require "./src/wirewright"

templ = ML.term <<-WWML
(qux a: ^x b: ^y
  (^case hovered
    true: (^* (^*part xs 0 ..< -1) (last (^part xs -1)))
    false: (^part xs -1)))
WWML

module Alloy
  record Context, vars : Term::Dict, errors = Stack(String).new do
    def error(&)
      errors << yield
    end
  end

  def self.var(ctx : Context, term : Term) : Rewrite::Any
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

  def self.flow(ctx : Context, successor : Rewriter, term : Term)
    Term.case(term) do
      matchpi %{(^* children_*)} do
        list = Term::Dict.build do |commit|
          children.items.each do |child|
            case rewrite = rewrite0(child, successor)
            in Rewrite::None
              commit << child
            in Rewrite::One
              commit << rewrite.term
            in Rewrite::Many
              commit.concat(rewrite.list.items)
            end
          end
        end

        Rewrite.many(list)
      end

      matchpi %{(^part var_symbol index←(%number i32))} do
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

      matchpi %{(^*part var_symbol b←(%number i32) ..< e←(%number -i32))} do
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

      matchpi %{(^*part var_symbol b←(%number i32) ..= e←(%number -i32))} do
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

      matchpi %{(^case var_symbol ¦ branches_)} do
        unless value = ctx.vars[var]?
          ctx.error { "variable '#{var}' does not exist" }
          continue
        end

        unless branch = branches[value]?
          ctx.error { "unhandled case: #{value}" }
          continue
        end

        rewrite0(branch, successor)
      end

      otherwise do
        Rewrite.none
      end
    end
  end

  # TODO: the precompilation of these should be automatic!!
  TRIGGER_FLOW = pipe(%{rewritee←[(%any ^* ^case ^part ^*part) _*]}, ML.term, M1.operator)
  TRIGGER_VAR  = pipe(%{rewritee_symbol}, ML.term, M1.operator)
  TRIGGER_REC  = pipe(%{rewritee_dict}, ML.term, M1.operator)

  def self.render(vars : Term::Dict, template : Term, *, strict : Bool = true)
    ctx = Context.new(vars)

    # TODO: we should allow passing an arbitrary object "payload" to rewriter context.
    # Then we won't have to create the rewriter every time like this.
    set, rec = recR
    rewriter = set.call switchR(
      { TRIGGER_FLOW, callR(->flow(Context, Rewriter, Term).partial(ctx, rec)) },
      { TRIGGER_VAR, callR(->var(Context, Term).partial(ctx)) },
      { TRIGGER_REC, entriesR(rec) },
    )

    render = rewrite(template, rewriter)

    if strict && ctx.errors.present?
      raise ctx.errors.join('\n')
    end

    render
  end
end

{% skip_file %}

# require "benchmark"

# Benchmark.ips do |x|
#   x.report("render") do
#     Alloy.render(Term[x: 100, y: 200, xs: {1, 2, 3, 4, 5}, hovered: true], templ, strict: true)
#   end
# end
