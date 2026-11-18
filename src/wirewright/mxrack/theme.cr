module Ww::MxRack
  defrecord ThemeMixin, dict : Term::Dict

  # Recognizes a theme mixin definition *defn*.
  #
  # Theme mixins are of the form:
  #
  # ```wwml
  # var0: expr0 ;; Nitrene expression
  # var1: expr1 ;; Nitrene expression
  # ...
  # varN: exprN ;; Nitrene expression
  #
  # (keypath0_*): expr0 ;; Nitrene expression
  # (keypath1_*): expr1 ;; Nitrene expression
  # ...
  # (keypathN_*): exprN ;; Nitrene expression
  # ```
  #
  # For example:
  #
  # ```wwml
  # base: "#191724"
  # surface: "#1f1d2e"
  #
  # ((table colors) "ui-base"): base
  # ((table colors) "ui-surface"): surface
  # ```
  def theme_mixin(defn : Term) : ThemeMixin
    unless defn = defn.as_d?
      return ThemeMixin.new(Term[])
    end

    vars = Term::Dict.build do |commit|
      defn.each_entry(in: Term::Dict.pairspart) do |key, value|
        next unless key.type.symbol?

        commit.with(key, Nitrene.eval(Term[], value))
      end
    end

    mixin = Term::Dict.build do |commit|
      defn.each_entry do |key, expr|
        next unless key.type.dict?

        commit.with(key, Nitrene.eval(vars, expr))
      end
    end

    ThemeMixin.new(mixin)
  end

  # Mixes a Microfold codex from *base* and zero or more *mixins*.
  def mix(cls : Microfold::Codex.class, base : Term::Dict, mixins : Enumerable(ThemeMixin)) : Microfold::Codex
    mixins.each do |mixin|
      mixin.dict.each_entry do |key, value|
        next unless key = key.as_itemsonly_d?

        base = Term.assign(base, key.items, to: value)
      end
    end

    # NOTE: The unwrap always succeeds but can provide diagnostics. Currently we discard
    # them here but in the future we'd probably want to print them somehow.
    sync_codex = Microfold.codex(base).unwrap
    sync_codex.to_unsafe
  end

  # :nodoc:
  def theme_mixins(tree : D7::InertLeaf) : Slice(ThemeMixin)
    Slice(ThemeMixin).empty
  end

  # :nodoc:
  def theme_mixins(tree : D7::GndLeaf) : Slice(ThemeMixin)
    node = tree.feature.node

    Term.case(node) do
      matchpi %{[theme]} do
        Slice[theme_mixin(node)]
      end

      otherwise do
        Slice(ThemeMixin).empty
      end
    end
  end

  # :nodoc:
  def theme_mixins(tree : D7::MixtureNode | D7::ScopeNode) : Slice(ThemeMixin)
    theme_mixins(tree.child)
  end

  # :nodoc:
  def theme_mixins(tree : D7::CircuitNode) : Slice(ThemeMixin)
    theme_mixins(tree.to_group)
  end

  # :nodoc:
  SYM_THEME = Term.of(:theme)

  # :nodoc:
  def theme_mixins(tree : D7::GroupNode) : Slice(ThemeMixin)
    unless tree.summary.has_head?(SYM_THEME)
      return Slice(ThemeMixin).empty
    end

    mixins = Pf::Kit.stack_array(ThemeMixin, 4)

    tree.children.zip(tree.feature.range) do |child, key|
      mixins.concat(theme_mixins(child))
    end

    mixins.to_unsafe_readonly_slice!
  end

  {% if flag?(:docs) %}
    # Returns the Microfold theme mixins in the given circuit's *tree*.
    def theme_mixins(tree : D7::ParseTree) : Slice(Automaton)
    end
  {% end %}
end
