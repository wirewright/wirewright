module Ww::M1
  # :nodoc:
  def compile(prod : Π::ItemOrd(Term)) : Op::Item::Any
    ordsrc, item = prod.ordsrc, prod.item

    Term.case(item, engine: M0) do
      matchpi %{[%'%singular successor_]}, cue: :"%singular" do
        Op::Item::Singular.new(compile(Π.pattern(successor)))
      end

      matchpi %{[%'%flex successor_]}, cue: :"%flex" do
        Op::Item::FlexSingular.new(compile(Π.pattern(successor)))
      end

      matchpi %{[%'%slot [%'%ref name_]]}, cue: {:"%slot", :"%ref"} do
        Op::Item::Slot.new(ordsrc.call, name)
      end

      matchpi %{[%'%group successor_ _*]}, cue: :"%group" do
        members = item.items.move(2)

        Op::Item::Group.new(ordsrc.call,
          compile(Π.pattern(successor)),
          compile(Π.items(ordsrc, members)),
        )
      end

      matchpi %{(%'%plural ⍊ type_symbol min_ max_)}, cue: :"%plural" do
        Op::Item::PluralDistrib.new(
          capture: nil,
          type: TermType.parse(type.as_sym),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%plural [%'%capture capture_] ⍊ type_symbol min_ max_)}, cue: {:"%plural", :"%capture"} do
        Op::Item::PluralDistrib.new(
          capture: capture,
          type: TermType.parse(type.as_sym),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%plural/min ⍊ type_symbol min_ max_)}, cue: :"%plural/min" do
        Op::Item::PluralMin.new(
          capture: nil,
          type: TermType.parse(type.as_sym),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%plural/min [%'%capture capture_] ⍊ type_symbol min_ max_)}, cue: {:"%plural/min", :"%capture"} do
        Op::Item::PluralMin.new(
          capture: capture,
          type: TermType.parse(type.as_sym),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%plural/max ⍊ type_symbol min_ max_)}, cue: :"%plural/max" do
        Op::Item::PluralMax.new(
          capture: nil,
          type: TermType.parse(type.as_sym),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%plural/max [%'%capture capture_] ⍊ type_symbol min_ max_)}, cue: {:"%plural/max", :"%capture"} do
        Op::Item::PluralMax.new(capture,
          type: TermType.parse(type.as_sym),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%many/max successor_ _* ⍊ min_ max_)}, cue: :"%many/max" do
        members = item.items.move(2)
        member_ops = compile(Π.items(ordsrc, members))

        Op::Item::ManyMax.new(compile(Π.pattern(successor)),
          Op::Item.spatial(member_ops),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%past/min _* ⍊ min_ max_)}, cue: :"%past/min" do
        members = item.items.move(1)
        member_ops = compile(Π.items(ordsrc, members))

        Op::Item::PastMin.new(Op::Item.spatial(member_ops),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%past/max _* ⍊ min_ max_)}, cue: :"%past/max" do
        members = item.items.move(1)
        member_ops = compile(Π.items(ordsrc, members))

        Op::Item::PastMax.new(Op::Item.spatial(member_ops),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{[%'%optional [%'%payload default_] successor_]}, cue: :"%optional" do
        Op::Item::Optional.new(ordsrc.call, default, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%gap measurer_]}, cue: :"%gap" do
        Op::Item::GapFirstDistrib.new(compile(Π.pattern(measurer)))
      end

      matchpi %{[%'%gap/min measurer_]}, cue: :"%gap/min" do
        Op::Item::GapFirstMin.new(compile(Π.pattern(measurer)))
      end

      matchpi %{[%'%gap/min° measurer_]}, cue: :"%gap/min°" do
        Op::Item::GapSourceMin.new(compile(Π.pattern(measurer)))
      end

      matchpi %{[%'%gap/max measurer_]}, cue: :"%gap/max" do
        Op::Item::GapFirstMax.new(compile(Π.pattern(measurer)))
      end

      matchpi %{[%'%gap/max° measurer_]}, cue: :"%gap/max°" do
        Op::Item::GapSourceMax.new(compile(Π.pattern(measurer)))
      end
    end
  end

  # :nodoc:
  def compile(prod : Π::Entry(Term)) : Op::Entry::Any
    entry = prod.entry

    Term.case(entry, engine: M0) do
      matchpi(
        %{[%'%entry/required [%'%key key_] [%'%pass]]},
        cue: {:"%entry/required", :"%key", :"%pass"},
      ) do
        Op::Entry::Present.new(key, type: :any)
      end

      matchpi(
        %{[%'%entry/required [%'%key key_] [%'%number %'_]]},
        cue: {:"%entry/required", :"%key", :"%number"},
      ) do
        Op::Entry::Present.new(key, type: :number)
      end

      matchpi(
        %{[%'%entry/required [%'%key key_] [%'%symbol]]},
        cue: {:"%entry/required", :"%key", :"%symbol"},
      ) do
        Op::Entry::Present.new(key, type: :symbol)
      end

      matchpi(
        %{[%'%entry/required [%'%key key_] [%'%string]]},
        cue: {:"%entry/required", :"%key", :"%string"},
      ) do
        Op::Entry::Present.new(key, type: :string)
      end

      matchpi(
        %{[%'%entry/required [%'%key key_] [%'%boolean]]},
        cue: {:"%entry/required", :"%key", :"%boolean"},
      ) do
        Op::Entry::Present.new(key, type: :boolean)
      end

      matchpi(
        %{[%'%entry/required [%'%key key_] [%'%dict]]},
        cue: {:"%entry/required", :"%key", :"%dict"},
      ) do
        Op::Entry::Present.new(key, type: :dict)
      end

      matchpi(
        %{[%'%entry/required [%'%key key_] successor_]},
        cue: {:"%entry/required", :"%key"},
      ) do
        Op::Entry::Required.new(key, compile(Π.pattern(successor)))
      end

      matchpi(
        %{[%'%entry/optional [%'%key key_] [%'%payload default_] successor_]},
        cue: {:"%entry/optional", :"%key", :"%payload"},
      ) do
        Op::Entry::Optional.new(key, default, compile(Π.pattern(successor)))
      end

      matchpi(
        %{[%'%entry/negative [%'%key key_] [%'%pass] [%'%ref name_]]},
        cue: {:"%entry/negative", :"%pass", :"%ref"}
      ) do
        Op::Entry::AbsentKeypath.new(key, name)
      end

      matchpi(
        %{[%'%entry/negative [%'%key key_] barrier_ [%'%ref name_]]},
        cue: {:"%entry/negative", :"%key", :"%ref"}
      ) do
        Op::Entry::NegativeKeypath.new(key, compile(Π.pattern(barrier)), name)
      end

      matchpi(
        %{[%'%entry/negative [%'%key key_] [%'%pass]]},
        cue: {:"%entry/negative", :"%key", :"%pass"},
      ) do
        Op::Entry::Absent.new(key)
      end

      matchpi(
        %{[%'%entry/negative [%'%key key_] barrier_]},
        cue: {:"%entry/negative", :"%key"},
      ) do
        Op::Entry::Negative.new(key, compile(Π.pattern(barrier)))
      end
    end
  end

  # :nodoc:
  def compile(prod : Π::Pattern(Term)) : Op::Any
    pattern = prod.pattern

    Term.case(pattern, engine: M0) do
      matchpi %{[%'%let [%'%capture name_] successor_]}, cue: {:"%let", :"%capture"} do
        Op::Capture.new(name, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pass]}, cue: :"%pass" do
        Op::INSTANCE_PASS
      end

      matchpi %{[%'%literal term_]}, cue: :"%literal" do
        Op::Literal.new(term)
      end

      matchpi %{(%'%guard successor_ ⍊ value-sketch_ symbol-sketch_ min-depth_ max-depth_ min-bounds_ max-bounds_)}, cue: :"%guard" do
        Op::Guard.new(
          value_sketch: Term::Dict::Sketch.new(value_sketch.to(Term::Dict::Sketch::Repr)),
          symbol_sketch: Term::Dict::Sketch.new(symbol_sketch.to(Term::Dict::Sketch::Repr)),
          bounds: {Kit.magn(min_bounds), Kit.magn(max_bounds)},
          depth: {Kit.magn(min_depth), Kit.magn(max_depth)},
          successor: compile(Π.pattern(successor)),
        )
      end

      matchpi %{[%'%layer below_ _*]} do
        rest = pattern.items.move(2)

        side = Slice(Op::Entry::Any).new(rest.size) do |index|
          compile(Π.entry(rest[index]))
        end

        # Sort ascending -- lowest cost goes first. The cost heuristic is a rough,
        # eyeballed  estimate of how "lightweight" an entry operator is.
        side.sort_by!(&.cost)

        Op::Layer.new(compile(Π.pattern(below)), side)
      end

      matchpi %{[%'%partition itemspart_ pairspart_]}, cue: :"%partition" do
        Op::Partition.new(compile(Π.pattern(itemspart)), compile(Π.pattern(pairspart)))
      end

      matchpi %{[%'%singular-seq _*]}, cue: :"%singular-seq" do
        members = pattern.items.move(1)

        Op::SingularSeq.new(compile(Π.patterns(members)),
          exhaustive: true,
          reverse: false,
        )
      end

      matchpi %{[%'%prefix successor_]}, cue: :"%prefix" do
        Op::ItemFirst.new(compile(Π.pattern(successor)))
      end

      matchpi %{[%'%postfix successor_]}, cue: :"%postfix" do
        Op::ItemLast.new(compile(Π.pattern(successor)))
      end

      matchpi %{[%'%prefix _ _*]}, cue: :"%prefix" do
        members = pattern.items.move(1)

        Op::SingularSeq.new(compile(Π.patterns(members)),
          exhaustive: false,
          reverse: false,
        )
      end

      matchpi %{[%'%postfix _ _*]}, cue: :"%postfix" do
        members = pattern.items.move(1)

        Op::SingularSeq.new(compile(Π.patterns(members)),
          exhaustive: false,
          reverse: true,
        )
      end

      # (`front _*)
      matchpi %{[%'%front [%'%ref ref_]]}, cue: {:"%front", :"%ref"} do
        Op::FrontRef.new(ref)
      end

      # (_* `back)
      matchpi %{[%'%back [%'%ref ref_]]}, cue: {:"%back", :"%ref"} do
        Op::BackRef.new(ref)
      end

      matchpi %{[%'%number %'_]}, cue: :"%number" do
        Op::INSTANCE_NUM
      end

      matchpi %{[%'%number %'(whole _)]}, cue: :"%number" do
        Op::INSTANCE_NUM_WHOLE
      end

      matchpi %{[%'%number _*]}, cue: :"%number" do
        NumberSpec.op(Term.of(pattern.itemspart))
      end

      matchpi %{[%'%string]}, cue: :"%string" do
        Op::INSTANCE_STR
      end

      matchpi %{[%'%symbol]}, cue: :"%symbol" do
        Op::INSTANCE_SYM
      end

      matchpi %{[%'%atom]}, cue: {:"%atom"} do
        Op::INSTANCE_ATOM
      end

      matchpi %{[%'%symbol nonblank]}, cue: {:"%symbol", :"nonblank"} do
        Op::SymNonblank.new
      end

      matchpi %{[%'%symbol blank name_ type_]}, cue: {:"%symbol", :"blank"} do
        Op::SymBlank.new(compile(Π.pattern(name)), compile(Π.pattern(type)))
      end

      matchpi %{[%'%boolean]}, cue: :"%boolean" do
        Op::INSTANCE_BOOLEAN
      end

      matchpi %{[%'%dict]}, cue: :"%dict" do
        Op::INSTANCE_DICT
      end

      matchpi %{[%'%itemsonly]}, cue: :"%itemsonly" do
        Op::Itemsonly.new
      end

      matchpi %{[%'%pairsonly]}, cue: :"%pairsonly" do
        Op::Pairsonly.new
      end

      matchpi %{[%'%itemsonly [%'%capture name_]]}, cue: {:"%itemsonly", :"%capture"} do
        Op::CaptureItemsonly.new(name)
      end

      matchpi %{[%'%edge %'_]}, cue: {:"%edge", :_} do
        Op::Edge.new(:any)
      end

      matchpi %{[%'%edge %'_number]}, cue: {:"%edge", :_number} do
        Op::Edge.new(:number)
      end

      matchpi %{[%'%edge %'_string]}, cue: {:"%edge", :_string} do
        Op::Edge.new(:string)
      end

      matchpi %{[%'%edge %'_symbol]}, cue: {:"%edge", :_symbol} do
        Op::Edge.new(:symbol)
      end

      matchpi %{[%'%edge %'_boolean]}, cue: {:"%edge", :_boolean} do
        Op::Edge.new(:boolean)
      end

      matchpi %{[%'%edge %'_dict]}, cue: {:"%edge", :_dict} do
        Op::Edge.new(:dict)
      end

      matchpi %{[%'%seq _*]} do
        members = pattern.items.move(1)

        ord = Op::Item::ORD_INITIAL
        ordsrc = -> { ord, _ = ord + 1, ord }

        items = members.map do |member|
          compile(Π.item(ordsrc, member))
        end

        singulars = items.compact_map { |op| op.as?(Op::Item::Singular).try(&.successor) }

        Op::Seq.new(Op::Item.spatial(items), singulars)
      end

      matchpi %{[%'%any _*]}, cue: :"%any" do
        options = pattern.items.move(1)

        Op::LiteralWhitelist.new(options.to_set)
      end

      matchpi %{[%'%not]}, cue: :"%not" do
        Op::INSTANCE_PASS
      end

      matchpi %{[%'%not _*]}, cue: :"%not" do
        options = pattern.items.move(1)

        Op::LiteralBlacklist.new(options.to_set)
      end

      matchpi %{[%'%never]}, cue: :"%never" do
        Op::INSTANCE_NEVER
      end

      matchpi %{[%'%keypool _*]}, cue: :"%keypool" do
        keys = pattern.items.move(1)

        Op::Keypool.new(keys.to_readonly_slice(&.itself))
      end

      matchpi %{[%'%keytest]}, cue: :"%keytest" do
        Op::INSTANCE_DICT
      end

      matchpi %{[%'%keytest _*]}, cue: :"%keytest" do
        keys = pattern.items.move(1)

        Op::Keytest.new(keys.to_readonly_slice(&.itself))
      end

      matchpi %{[%'%value [%'%key key_] successor_]}, cue: {:"%value", :"%key"} do
        Op::ValueLiteral.new(key, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%value [%'%capture capture_] successor_]}, cue: {:"%value", :"%capture"} do
        Op::Value.new(capture, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%-value [%'%capture capture_]]}, cue: {:"%-value", :"%capture"} do
        Op::NegativeValue.new(capture)
      end

      matchpi %{[%'%-value [%'%capture capture_] [%'%ref name_]]}, cue: {:"%-value", :"%capture", :"%ref"} do
        Op::NegativeValueKeypath.new(capture, name)
      end

      matchpi %{[%'%pipe [%'%payload (+ n_)] successor_]}, cue: {:"%pipe", :+} do
        Op::Add.new(n.as_n, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload (- n_)] successor_]}, cue: {:"%pipe", :-} do
        Op::Sub.new(n.as_n, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload (* n_)] successor_]}, cue: {:"%pipe", :*} do
        Op::Mul.new(n.as_n, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload (/ n_)] successor_]}, cue: {:"%pipe", :/} do
        Op::Div.new(n.as_n, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload (div n_)] successor_]}, cue: {:"%pipe", :div} do
        Op::Idiv.new(n.as_n, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload (mod n_)] successor_]}, cue: {:"%pipe", :mod} do
        Op::Mod.new(n.as_n, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload (** n_)] successor_]}, cue: {:"%pipe", :**} do
        Op::Pow.new(n.as_n, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload (clamp min_ ..= max_)] successor_]}, cue: {:"%pipe", :clamp, :"..="} do
        Op::Clamp.new(min.as_n, max.as_n, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload (map arg_)] successor_]}, cue: {:"%pipe", :map} do
        Op::Map.new(arg.as_d, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload charcount] successor_]}, cue: {:"%pipe", :charcount} do
        Op::Charcount.new(compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload tally] successor_]}, cue: {:"%pipe", :tally} do
        Op::Tally.new(compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload type] successor_]}, cue: {:"%pipe", :type} do
        Op::Type.new(compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload ml] successor_]}, cue: {:"%pipe", :ml} do
        Op::ParseML.new(compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload untracked] successor_]}, cue: {:"%pipe", :untracked} do
        Op::Untracked.new(compile(Π.pattern(successor)))
      end

      matchpi %{[%'%pipe [%'%payload fn←(prepend _*)] successor_]}, cue: {:"%pipe", :prepend} do
        terms = fn.items.move(1).to_readonly_slice(&.itself)

        Op::Prepend.new(terms, compile(Π.pattern(successor)))
      end

      matchpi %{[%'%all]}, cue: :"%all" do
        Op::INSTANCE_PASS
      end

      matchpi %{[%'%all a_]}, cue: :"%all" do
        compile(Π.pattern(a))
      end

      matchpi %{[%'%all a_ b_]}, cue: :"%all" do
        Op::Both.new(compile(Π.pattern(a)), compile(Π.pattern(b)))
      end

      matchpi %{[%'%all a_ _*]}, cue: :"%all" do
        rest = Term.of(pattern.replace(1, Term.rep))

        Op::Both.new(compile(Π.pattern(a)), compile(Π.pattern(rest)))
      end

      matchpi %{[%'%any°]}, cue: :"%any°" do
        Op::INSTANCE_PASS
      end

      matchpi %{[%'%any° a_]}, cue: :"%any°" do
        compile(Π.pattern(a))
      end

      matchpi %{[%'%any° a_ b_]}, cue: :"%any°" do
        Op::ChoiceSource.new(compile(Π.pattern(a)), compile(Π.pattern(b)))
      end

      matchpi %{[%'%any° a_ _*]}, cue: :"%any°" do
        rest = Term.of(pattern.replace(1, Term.rep))

        Op::ChoiceSource.new(compile(Π.pattern(a)), compile(Π.pattern(rest)))
      end

      matchpi %{[%'%item _*]}, cue: :"%item" do
        members = pattern.items.move(1)

        Op::ScanFirst.new(compile(Π.patterns(members)))
      end

      matchpi %{[%'%item° _*]}, cue: :"%item°" do
        members = pattern.items.move(1)

        Op::ScanSource.new(compile(Π.patterns(members)))
      end

      matchpi %{(%'%items successor_ _* ⍊ min_ max_)}, cue: :"%items" do
        members = pattern.items.move(2)

        Op::ScanAll.new(compile(Π.pattern(successor)),
          seq: compile(Π.patterns(members)),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{[%'%entry key_ value_]}, cue: :"%entry" do
        Op::EntriesFirst.new(compile(Π.pattern(key)), compile(Π.pattern(value)))
      end

      matchpi %{[%'%entry° key_ value_]}, cue: :"%entry°" do
        Op::EntriesSource.new(compile(Π.pattern(key)), compile(Π.pattern(value)))
      end

      matchpi %{(%'%entries successor_ key_ value_ ⍊ min_ max_)}, cue: :"%entries" do
        Op::EntriesAll.new(compile(Π.pattern(successor)),
          compile(Π.pattern(key)),
          compile(Π.pattern(value)),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%leaf _* ⍊ order: dfs in: part_ self: depth0_)}, cue: {:"%leaf", :dfs} do
        members = pattern.items.move(1)

        alg = Tzip::DfsPreorder.new(*Tzip::Order.parse(part),
          mindepth: depth0.true? ? 0u32 : 1u32,
          maxdepth: UInt32::MAX,
        )

        Op::DfsFirst.new(alg, seq: compile(Π.patterns(members)))
      end

      matchpi %{(%'%leaf° _* ⍊ order: dfs in: part_ self: depth0_)}, cue: {:"%leaf°", :dfs} do
        members = pattern.items.move(1)

        alg = Tzip::DfsPreorder.new(*Tzip::Order.parse(part),
          mindepth: depth0.true? ? 0u32 : 1u32,
          maxdepth: UInt32::MAX,
        )

        Op::DfsSource.new(alg, seq: compile(Π.patterns(members)))
      end

      matchpi %{(%'%leaves successor_ _* ⍊ order: dfs in: part_ self: depth0_ min_ max_)}, cue: {:"%leaves", :dfs} do
        members = pattern.items.move(2)

        alg = Tzip::DfsPreorder.new(*Tzip::Order.parse(part),
          mindepth: depth0.true? ? 0u32 : 1u32,
          maxdepth: UInt32::MAX,
        )

        Op::DfsAll.new(alg,
          successor: compile(Π.pattern(successor)),
          seq: compile(Π.patterns(members)),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%leaf _* ⍊ order: bfs in: part_ self: depth0_)}, cue: {:"%leaf", :bfs} do
        members = pattern.items.move(1)

        alg = Tzip::Bfs.new(*Tzip::Order.parse(part),
          mindepth: depth0.true? ? 0u32 : 1u32,
          maxdepth: UInt32::MAX,
        )

        Op::BfsFirst.new(alg, seq: compile(Π.patterns(members)))
      end

      matchpi %{(%'%leaf° _* ⍊ order: bfs in: part_ self: depth0_)}, cue: {:"%leaf°", :bfs} do
        members = pattern.items.move(1)

        alg = Tzip::Bfs.new(*Tzip::Order.parse(part),
          mindepth: depth0.true? ? 0u32 : 1u32,
          maxdepth: UInt32::MAX,
        )

        Op::BfsSource.new(alg, seq: compile(Π.patterns(members)))
      end

      matchpi %{(%'%leaves successor_ _* ⍊ order: bfs in: part_ self: depth0_ min_ max_)}, cue: {:"%leaves", :bfs} do
        members = pattern.items.move(2)

        alg = Tzip::Bfs.new(*Tzip::Order.parse(part),
          mindepth: depth0.true? ? 0u32 : 1u32,
          maxdepth: UInt32::MAX,
        )

        Op::BfsAll.new(alg,
          successor: compile(Π.pattern(successor)),
          seq: compile(Π.patterns(members)),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%split lhs_ _ _* ⍊ wide_)}, cue: :"%split" do
        mid = pattern.items.move(2).grow(-1)
        rhs = pattern.items.last

        Op::SplitFirst.new(
          compile(Π.pattern(lhs)),
          compile(Π.patterns(mid)),
          compile(Π.pattern(rhs)),
          wide: wide.true?,
        )
      end

      matchpi %{(%'%split° lhs_ _ _* ⍊ wide_)}, cue: :"%split°" do
        mid = pattern.items.move(2).grow(-1)
        rhs = pattern.items.last

        Op::SplitSource.new(
          compile(Π.pattern(lhs)),
          compile(Π.patterns(mid)),
          compile(Π.pattern(rhs)),
          wide: wide.true?,
        )
      end

      matchpi %{(%'%splits successor_ lhs_ _ _* ⍊ wide_ min_ max_)}, cue: :"%splits" do
        mid = pattern.items.move(3).grow(-1)
        rhs = pattern.items.last

        Op::SplitAll.new(compile(Π.pattern(successor)),
          compile(Π.pattern(lhs)),
          compile(Π.patterns(mid)),
          compile(Π.pattern(rhs)),
          min: Kit.magn(min),
          max: Kit.magn(max),
          wide: wide.true?,
        )
      end

      matchpi %{[%'%adjacent _*]}, cue: :"%adjacent" do
        members = pattern.items.move(1)

        Op::Adjacent.new(compile(Π.patterns(members)))
      end

      matchpi %{[%'%keypath [%'%capture capture_]]}, cue: :"%capture" do
        Op::KeypathCapture.new(capture)
      end

      matchpi %{[%'%pluck [%'%payload spec_] successor_]}, cue: :"%pluck" do
        Op::Pluck.new(pluck_spec(spec.as_d), compile(Π.pattern(successor)))
      end

      matchpi %{[%'%flat [%'%payload spec_] successor_]}, cue: :"%flat" do
        Op::Flat.new(flat_spec(spec.as_d), compile(Π.pattern(successor)))
      end

      matchpi %{(%'%filter (%'%payload deps_) selector_ successor_ ⍊ min_ max_)}, cue: :"%filter" do
        Op::Filter.new(compile(Π.pattern(successor)),
          deps: deps.items.to_pf_set,
          selector: compile(Π.pattern(selector)),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end

      matchpi %{(%'%matches successor_ subpattern_ ⍊ min_ max_)} do
        Op::Matches.new(compile(Π.pattern(successor)), compile(Π.pattern(subpattern)),
          min: Kit.magn(min),
          max: Kit.magn(max),
        )
      end
    end
  end

  # :nodoc:
  def compile(prod : Π::PatternList) : Slice(Op::Any)
    prod.patterns.to_readonly_slice do |pattern|
      compile(Π.pattern(pattern))
    end
  end

  # :nodoc:
  def compile(prod : Π::ItemOrdList) : Slice(Op::Item::Any)
    prod.items.to_readonly_slice do |item|
      compile(Π.item(prod.ordsrc, item))
    end
  end

  private def flat_spec(spec : Indexable(Term)) : Tzip::FlatSpec
    spec.to_readonly_slice do |item|
      step = Term.case(item, engine: M0) do
        matchpi %{%'_} do
          Tzip::InItemsStep.new
        end

        matchpi %{.} do
          Tzip::InPairsStep.new
        end

        matchpi %{*} do
          Tzip::InEntriesStep.new
        end

        matchpi %{(merge _ _ _*)} do
          args = item.items.move(1)
          continue unless args.all? { |term| term.type.dict? && term.itemsonly? }

          Tzip::MergeStep.new(args.to_readonly_slice { |arg| flat_spec(arg.items) })
        end

        matchpi %{(keys _ _*)} do
          Tzip::KeyListStep.new(item.items.move(1).to_readonly_slice(&.itself))
        end

        matchpi %{(key term_)} do
          Tzip::KeyStep.new(term)
        end

        otherwise do
          Tzip::KeyStep.new(item)
        end
      end

      step.as(Tzip::FlatStep)
    end
  end

  private def flat_spec(spec : Term::Dict) : Tzip::FlatSpec
    flat_spec(spec.items)
  end

  private def pluck_spec(spec : Indexable(Term)) : Tzip::PluckSpec
    spec.to_readonly_slice do |item|
      step = Term.case(item, engine: M0) do
        matchpi %{%'_} do
          Tzip::InItemsStep.new
        end

        matchpi %{.} do
          Tzip::InPairsStep.new
        end

        matchpi %{*} do
          Tzip::InEntriesStep.new
        end

        matchpi %{(keys _ _*)} do
          Tzip::KeyListStep.new(item.items.move(1).to_readonly_slice(&.itself))
        end

        matchpi %{(key term_)} do
          Tzip::KeyStep.new(term)
        end

        otherwise do
          Tzip::KeyStep.new(item)
        end
      end

      step.as(Tzip::PluckStep)
    end
  end

  private def pluck_spec(spec : Term::Dict) : Tzip::PluckSpec
    pluck_spec(spec.items)
  end
end
