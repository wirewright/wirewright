module Ww::ML::Tree
  # Nodes listed here will receive an automatic implementation of `hashrepr`.
  alias AutoRepr = ExprNode | DictSection | DictEntryBlock | DictEntry | SelectorNode | MultisetEntry | Layer | SplitPartNode

  macro finished
    {% verbatim do %}
      private def autorepr(io, node) : Nil
        {% begin %}
          case node
          {% for type in AutoRepr.union_types %}\
            {% fields = type.methods
                 .select(&.annotation(::DefcaseField))
                 .map { |method| "node.#{method.name.id}".id }
                 .sort %}\
            in {{type}}
              io << {{type.id.split("::").last}} << "("
              Tuple.new({{fields.splat}}).join(io, ",") do |value|
                hashrepr(io, value)
              end
              io << ")"
          {% end %}\
          end
        {% end %}
      end

      # :nodoc:
      def hashrepr(io, object : T) : Nil forall T
        # In theory we'd be doing autorepr here, right away. But in practice, we hit
        # a roadblock with how Crystal dispatches unions:
        #
        # ```
        # def m(a : Int32) # Like the other overloads of hashrepr.
        #   typeof(a)
        # end
        #
        # def m(a) # Like this overload of hashrepr.
        #   typeof(a)
        # end
        #
        # pp! m(100)                     # => Int32
        # pp! m("hi")                    # => String
        # pp! m(100.as(Int32 | String))  # => Int32
        # pp! m("hi".as(Int32 | String)) # => Int32 | String   ?!
        # ```
        #
        # This is "fixed" by this macro def:
        #
        # ```
        # # EXPECTED
        #
        # def dispatch(a)
        #   if a.is_a?(Int32)
        #     m(a)
        #   else
        #     m(a)
        #   end
        # end
        #
        # pp! dispatch(100.as(Int32 | String))  # => Int32
        # pp! dispatch("hi".as(Int32 | String)) # => String
        # ```
        #
        # We're basically "splatting" the union into method overloads right here. It
        # would be nice if Crystal had a way to opt into that. Both behaviors are correct,
        # whatever the word means. Sometimes (maybe most of the time!) you want to treat
        # a union as a distinct type vs. its actual type at runtime.
        {% begin %}
          {% if T.union? %}
            case object
            {% for member in T.union_types %}
            in {{member}} then return hashrepr(io, object)
            {% end %}
            end
          {% else %}
            autorepr(io, object)
          {% end %}
        {% end %}
      end
    {% end %}
  end

  # :nodoc:
  def hashrepr(io, object : Location) : Nil
    hashrepr(io, object.child)
  end

  # :nodoc:
  def hashrepr(io, object : ToggleableEntry) : Nil
    hashrepr(io, object.entry)
  end

  # :nodoc:
  def hashrepr(io, object : Nil) : Nil
    io << "Nil"
  end

  # :nodoc:
  def hashrepr(io, object : Int32) : Nil
    io << object
  end

  # :nodoc:
  def hashrepr(io, objects : Array) : Nil
    io << "["
    objects.join(io, ",") do |object|
      hashrepr(io, object)
    end
    io << "]"
  end

  # :nodoc:
  def hashrepr(io, object : Term) : Nil
    io << "Term("
    ML.compact(io, object)
    io << ")"
  end

  # :nodoc:
  def hashrepr(io, object : Bool) : Nil
    io << (object ? "True" : "False")
  end

  # :nodoc:
  def hashrepr(io, object : DocumentSection) : Nil
    io << "DocumentSection("
    hashrepr(io, object.name)
    io << ","
    hashrepr(io, object.body)
    io << ")"
  end

  # :nodoc:
  def hashrepr(io, object : TemplateRule) : Nil
    io << "TemplateRule("
    hashrepr(io, object.pattern)
    io << ","
    hashrepr(io, object.template)
    io << ")"
  end

  # :nodoc:
  def hashrepr(io, object : BackmapRule) : Nil
    io << "BackmapRule("
    hashrepr(io, object.pattern)
    io << ","
    hashrepr(io, object.backspec)
    io << ")"
  end

  {% if flag?(:docs) %}
    # Appends the *hash representation* of *object* to *io*.
    #
    # See `hashrepr(object)`.
    def hashrepr(io, object) : Nil
    end
  {% end %}

  # Returns a string of the hash representation of *object*.
  #
  # A hash representation is like what you get by calling `Object#inspect`, except
  # a bit simplified and tailored for hashing. It captures the essential ("hash-important")
  # properties of *object* and leaves unimportant ones out (for example, node
  # `Location`s are skipped as we don't want them to affect the hash).
  #
  # Hash representations are used to implement rule and rule block ids. when you
  # say `(+ a_ b_ ⍊ ◇_) <> {a: ^b, b: ^a, ◇: true}`, `◇` will be replaced by the hashcode
  # of the hash representation of `(+ a_ b_ ⍊ ◇_) <> {a: ^b, b: ^a, ◇: true}`.
  # Notably, `◇` and `▢` themselves (and their variants) are treated *symbolically*,
  # i.e., as `◇` or `▢`, not what they're replaced with; otherwise, we'd have infinite
  # regress -- not something we'd want.
  def hashrepr(object) : String
    String.build { |io| hashrepr(io, object) }
  end

  # Returns the hashcode of the hash representation of *object*.
  #
  # See `hashrepr` for more info.
  def hashcode(object) : Term::H256
    Term::H256.new { |io| hashrepr(io, object) }
  end
end
