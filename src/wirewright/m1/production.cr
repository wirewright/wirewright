module Ww::M1
  # :nodoc:
  #
  # *Π* is short for *production*, as in grammars. Due to its frequency I'm using
  # just one letter, a Greek one for distinctiveness.
  #
  # These objects define the vocabulary which M1 compiler functions use to describe/
  # annotate raw Terms.
  module Π
    extend self

    defrecord Toplevel(T), pattern : T

    def toplevel(object)
      Toplevel.new(object)
    end

    defrecord Pattern(T), pattern : T
    defrecord PatternList(T), patterns : T

    def pattern(object)
      Pattern.new(object)
    end

    def patterns(objects)
      PatternList.new(objects)
    end

    defrecord Dict(T), dict : T

    def dict(object)
      Dict.new(object)
    end

    defrecord Item(T), item : T

    def item(object)
      Item.new(object)
    end

    defrecord ItemOrd(T), ordsrc : (-> UInt32), item : T
    defrecord ItemOrdList(T), ordsrc : (-> UInt32), items : T

    def item(ordsrc : -> UInt32, object)
      ItemOrd.new(ordsrc, object)
    end

    def items(ordsrc : -> UInt32, objects)
      ItemOrdList.new(ordsrc, objects)
    end

    defrecord Entry(T), entry : T

    def entry(object)
      Entry.new(object)
    end

    defrecord EntryKV(K, V), key : K, value : V

    def entry(key, value)
      EntryKV.new(key, value)
    end

    defrecord Normal(T), op : T

    def normal(op)
      Normal.new(op)
    end

    defrecord Guarded(T), op : T

    def guarded(op)
      Guarded.new(op)
    end
  end
end
