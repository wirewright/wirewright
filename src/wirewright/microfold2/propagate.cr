module Ww::Microfold
  defrecord ScopedCue, scope : Pf::UPath32, cue : UpCue | DnCue

  # NOTE: Since cues require effectively global knowledge, they are very badly or
  # un- cacheable. So we do not cache them. This means that this function should
  # be as quick as possible. Not in the nanoseconds sense, of course, as we only
  # run it on the Cued part of the tree, which is tiny in standard usage. But
  # still, we must not be too loose.
  private def solve(node : CuedStyleNode, dnflow : Pf::Set(ScopedCue), scope : Pf::UPath32, path : Pf::UPath32) : Pf::Set(ScopedCue)
    upbound = Pf::Set(ScopedCue).new
    dnbound = Pf::Set(ScopedCue).new

    child_scope = scope
    if node.cue_membrane
      child_scope = path
    end

    if features = node.features
      features.each do |feature|
        case feature
        when UpCue # is-qux
          upbound = upbound.add(ScopedCue.new(scope, feature))
        when DnCue # cue-qux
          dnbound = dnbound.add(ScopedCue.new(child_scope, feature))
        when CueCond
          satisfied = feature.requirements.all? do |requirement|
            case requirement
            in UpCue # has-qux:...
              ScopedCue.new(child_scope, requirement).in?(dnflow)
            in DnCue # in-qux:...
              ScopedCue.new(scope, requirement).in?(dnflow)
            end
          end

          next unless satisfied

          case body = feature.body
          in UncuedFeature, Present, Absent
          in UpCue # has-qux:is-foobar  in-qux:is-foobar
            upbound = upbound.add(ScopedCue.new(scope, body))
          in DnCue # has-qux:cue-foobar  in-qux:cue-foobar
            dnbound = dnbound.add(ScopedCue.new(child_scope, body))
          end
        end
      end
    end

    dnflow += dnbound

    node.children.each_with_index do |child, index|
      upbound += solve(child, dnflow, child_scope, path.append(index.to_u32))
    end

    # Everything that we've contributed upbound, everything that children
    # contributed, everything that we've contributed dnbound.
    upbound + dnbound
  end

  private def solve(node : UncuedStyleNode, dnflow : Pf::Set(ScopedCue), scope : Pf::UPath32, path : Pf::UPath32) : Pf::Set(ScopedCue)
    Pf::Set(ScopedCue).new
  end

  # :nodoc:
  def solve(node : CuedStyleNode) : Pf::Set(ScopedCue)
    upflow0 = Pf::Set(ScopedCue).new

    loop do
      upflow1 = solve(node, upflow0, Pf::UPath32[], Pf::UPath32[])
      break if upflow0 == upflow1

      upflow0 = upflow1
    end

    upflow0
  end

  # :nodoc:
  def solve(node : UncuedStyleNode) : Pf::Set(ScopedCue)
    Pf::Set(ScopedCue).new
  end

  {% if flag?(:docs) %}
    # Calculates the set of cues currently active in *node*.
    def solve(node : StyleNode) : Pf::Set(ScopedCue)
    end
  {% end %}

  private def propagate(node : CuedStyleNode, cues : Pf::Set(ScopedCue), scope : Pf::UPath32, path : Pf::UPath32)
    present = true

    child_scope = scope
    if node.cue_membrane
      child_scope = path
    end

    if features = node.features
      uncued_features = features.to_compact_readonly_slice do |feature|
        case feature
        in UncuedFeature
          feature # keep
        in Present
          # absent ⏏present⏏ in-error:bg-red-500
          present = true
          nil # remove
        in Absent
          # ⏏absent⏏ in-error:bg-red-500
          present = false
          nil # remove
        in UpCue, DnCue
          nil # remove
        in CueCond
          satisfied = feature.requirements.all? do |requirement|
            case requirement
            in UpCue # has-qux:...
              ScopedCue.new(child_scope, requirement).in?(cues)
            in DnCue # in-qux:...
              ScopedCue.new(scope, requirement).in?(cues)
            end
          end

          next unless satisfied # remove

          case body = feature.body
          in UncuedFeature
            # in-error:⏏bg-red-500⏏
            body # replace with
          in Present
            # in-error:⏏present⏏
            present = true
            nil # remove
          in Absent
            # in-error:⏏absent⏏
            present = false
            nil # remove
          in UpCue, DnCue
            nil # remove
          end
        end
      end
    end

    uncued_children = node.children.to_readonly_slice do |child, index|
      propagate(child, cues, child_scope, path.append(index.to_u32))
    end

    UncuedStyleNode.new(present, uncued_features, uncued_children)
  end

  private def propagate(root : UncuedStyleNode, cues : Pf::Set(ScopedCue), scope : Pf::UPath32, path : Pf::UPath32)
    root
  end

  # Converts `CuedStyleNode`s to `UncuedStyleNode`s by choosing to interpret it
  # in a particular way, according to *cues*.
  #
  # *cues* is a kind of "key" to a particular interpretation of a cued node.
  # The search for this key is done in `solve` such that all constraints are
  # satisfied. Finally, `propagate` "unlocks" the interpretation of a cued
  # node using the key.
  #
  # For `UncuedStyleNode`s, `propagate` is a noop.
  def propagate(root : StyleNode, cues : Pf::Set(ScopedCue)) : UncuedStyleNode
    propagate(root, cues, scope: Pf::UPath32[], path: Pf::UPath32[])
  end
end
