require "./wirewright"
require "./baz4"
require "./libtermbox2"

# TODO: should we add these as official rewriters? Then we'll be able to use
# rulesets instead of manually ordered Term.cases (painful!) That'd be nice!
# And also we'd get all the optimizations that are to come.

# That all of this works (or at least appears to) still blows my mind, but the inefficiencies
# and combinatorial explosion are still an obvious problem. With the refine phase, for instance,
# almost never is it needed; but we force all nodes to be rewritten with the refine phase all the time.
# Even if we optimize for rejection (as we should!), having hundreds of patterns (as in any moderately
# interesting rule system) we'll have (if we're lucky) ~50ns to 1µs per rejection; adding up to not-so-
# nice numbers per SINGLE REWRITE ATTEMPT! And we do hundreds of those, in turn! I guess you see where
# I'm going. Some counter-measures:
#   1. Locality and [probabilistic, content-based] partitioning. Sketches will already help with
#      this; but more robust solutions are needed. Increasing the sketch to 16 bytes (u128) should
#      with larger rulebases and terms. If there's a u128 per level of depth, plus with some dict
#      integration (perhaps?), then we'll have an enormous chance of rejecting shallowly.
#   2. Min and max depth, a sketch of some sort of min and max deep bounds (bounds of self, self's children, etc.),
#      all of those would help as well, since we can compute them directly from the pattern.
#   3. Sketch tries. It is possible, with minor work, to optimize Rulesets so they build a sketch
#      trie. I'm not sure how much this would help, but in theory, it should partition the search
#      space a bit better than we currently do. We only partition on head; if we also partition on
#      deep symbols, that'd be good.
#   4. Value sketches. At some point we'll have to have those: strings, booleans, and numbers all
#      should be able to enter into a sketch themselves. Value sketch obviously has less precision
#      than symbol sketch. It should be possible to concatenate symbol and value sketches into one
#      big sketch that we'd be able to do operations on.
#
# Again, it is very important to note that we're optimizing for rejection. During our search, vast
# amounts of patterns/rules are rejected. Among hundreds of rules, only one is usually valid for a
# given term. All patterns must be optimized for rejections; and the search process itself must be. 
#
# That's battling with the symptoms, though, and I'm well aware of that. But I don't think it is possible
# to optimize away the problem itself -- us iterating a lot over a lot of combinations.
#
# Computers are good at doing lots of iterations, provided iteration is local (currently it is not!)
# and access to memory is rare (currently it is not!) So lots of moderately "low-hanging fruits" before
# trying to treat the underlying problem.

def edger1(parent0, phase, child0, callable)
  while true
    parent1, child1 = callable.call(parent0, phase, child0)
    break if {parent0, child0} == {parent1, child1}
    parent0, child0 = parent1, Term.of(child1)
  end

  {parent0, child0}
end

def edger(dict0 : Term::Dict, callable)
  dict1 = dict0
  dict0.items.each_with_index do |v0, k|
    dict1, v0 = edger1(dict1, Term.of(:enter), Term.of(v0), callable)
    if vd = v0.as_d?
      v0 = edger(vd, callable)
    end
    dict1, v1 = edger1(dict1, Term.of(:leave), Term.of(v0), callable)
    dict1 = dict1.with(k, v1)
  end
  (0..).each do |i|
    pre = dict1
    dict1.items.each_with_index do |v0, k|
      dict1, v1 = edger1(dict1, Term.of(:refine, i), Term.of(v0), callable)
      dict1 = dict1.with(k, v1)
    end
    if pre == dict1
      break
    end
  end
  dict1
end

def fit1(parent : Term::Dict, phase : Term, child : Term)
  Term.case({parent, phase, child}) do
    # If I have max size, pass it down to children.
    # NOTE: this is a hack. We should have max-width and max-height separately! Because e.g.
    # (col) passes down max-width but not max-height.
    givenpi %[(_+ ¦ _ max-size: bounds_) enter (_* ¦ _ max-size: (%- _))] do
      {parent, child.morph({:"max-size", bounds})}
    end

    # Compute size for text with size limitations.
    givenpi %[_ enter (text caption_string ¦ _ max-size: (wlimit←(%number u16) hlimit←(%number u16)) size: hug pre: true)] do
      reader = Char::Reader.new(caption.to(String))

      x = w = h = 0

      hlimit.to(Int32).times do
        h += 1

        wlimit.to(Int32).times do
          chr = reader.current_char

          case chr
          when '\0'
            break
          when '\n'
            reader.next_char?
            x += 1 # Oopsie doopsie, we have to count newline as well orelse everything falls apart.
            h += 1
            break
          else
            x += 1
          end

          break unless reader.next_char?
        end

        w = Math.max(w, x)
        x = 0

        break if reader.current_char == '\0'
      end

      {parent, child.morph({:size, {w, h}})}
    end

    # Inherit size from sized parent for single-child parents.
    givenpi %[(_ _ ¦ _ size: (w←(%number u16) h←(%number u16))) enter (_* ¦ _ padding: (%optional 0 p_number) size: fill)] do
      {parent, child.morph({:size, {w + p*2, h + p*2}})}
    end

    # Pass down size as max size to hug children. Subtract padding.
    givenpi %[(_ _ ¦ _ size: (w←(%number u16) h←(%number u16))) enter (_* ¦ _ padding: (%optional 0 p_number) size: hug)] do
      {parent, child.morph({:"max-size", {w - p*2, h - p*2}})}
    end

    # Each size: fill child of col will increment den (for "denominator").
    # Do not forget to mark the child; otherwise we'll increment forever.
    givenpi %[(col _+ ¦ _ den: (%optional 0 den_number)) enter (_* ¦ _ in-den: (%- _) size: fill)] do
      {parent.morph({:den, den + 1}), child.morph({:"in-den", true})}
    end

    # If col doesn't have avail on leave, assign it to its full height.
    givenpi %[(col _+ ¦ _ size: (_ h←(%number u16)) avail: (%- _)) leave _] do
      {parent.morph({:avail, h}), child}
    end

    # On leave we'll make have each sized child subtract its height from avail.
    # Do not forget to mark the child; otherwise we'll have infinite rewriting
    # since we can subtract h from avail forever.
    givenpi %[(col _+ ¦ _ avail: avail_number) leave (_* ¦ _ in-avail: (%- _) size: (_ h←(%number u16)))] do
      {parent.morph({:avail, avail - h}), child.morph({:"in-avail", true})}
    end

    # Use fractional height for fill children of col 
    givenpi %[(col _+ ¦ _ size: (w←(%number u16) _) avail: avail_number den: den_number) (refine 0) (_* ¦ _ in-den: true padding: (%optional 0 p_number) size: fill)] do
      {parent, child.morph({:size, {w + p*2, avail//den + p*2}})}
    end

    # Learn size from sized child for single-child parents.
    givenpi %[(_ _ ¦ _ padding: (%optional 0 p_number) size: hug) leave (_* ¦ _ size: (w←(%number u16) h←(%number u16)))] do
      {parent.morph({:size, {w + p*2, h + p*2}}), child}
    end

    otherwise { {parent, child} }
  end
end

def fix1(parent, phase, child)
  Term.case({parent, phase, child}) do
    # Positioned & sized column should arrange its children.
    givenpi %[
      (col _* ¦ _ position: (left←(%number u16) top←(%number u16))
                  state: (%optional 0 state_number))
      enter
      (_* ¦ _ position: (%- _)
              size: (w←(%number u16) h←(%number u16)))
    ] do
      {parent.morph({:state, state + h}),
       child.morph({:position, {left, top + state}})}
    end

    # Learn position from single-child parent
    givenpi %[(_ _ ¦ _ position: position_) enter (_* ¦ _ position: (%- _))] do
      {parent, child.morph({:position, position})}
    end

    # Apply padding to child on leave.
    givenpi %[(_ _ ¦ _ padding: p_number) leave (_* ¦ _ position: (left←(%number u16) top←(%number u16)))] do
      {parent.morph({:padding, nil}), child.morph({:position, {left + p, top + p}})}
    end

    otherwise { {parent, child} }
  end
end

def fit(frame : Term::Dict) : Term::Dict
  edger(frame, ->fit1(Term::Dict, Term, Term))
end

def fix(frame : Term::Dict) : Term::Dict
  edger(frame, ->fix1(Term::Dict, Term, Term))
end

NORD_BG = Termbox::Color.new(0x2e3440)
NORD_FG = Termbox::Color.new(0xeceff4)
NORD_FG_DIM = Termbox::Color.new(0xd8dee9)
NORD_BLUE_DARK = Termbox::Color.new(0x5e81ac)
NORD_BLUE_LIGHT = Termbox::Color.new(0x81a1c1)

def draw(frame, bbg)
  Term.case(frame) do
    matchpi %[(box child_ ¦ _
                   position: (x0←(%number u16) y0←(%number u16))
                   size: (w0←(%number u16) h0←(%number u16))
                   bg: (r0←(%number u8) g0←(%number u8) b0←(%number u8)))] do
      x, y, w, h = {x0, y0, w0, h0}.map(&.to(Int32))
      r, g, b = {r0, g0, b0}.map(&.to(UInt8))

      bg = Termbox::Color.rgb(r, g, b)

      # Draw frame & fill background
      (y...y + h).each do |py|
        (x...x + w).each do |px|
          case {py, px}
          when {y, x}     # Top-left corner
            ch = '╭'
          when {y, x + w - 1} # Top-right corner
            ch = '╮'
          when {y + h - 1, x} # Bottom-left corner
            ch = '╰'
          when {y + h - 1, x + w - 1} # Bottom-right corner
            ch = '╯'
          when {y, _}, {y + h - 1, _} # Top, bottom strip
            ch = '─'
          when {_, x}, {_, x + w - 1} # Left, right strip
            ch = '│'
          else
            ch = ' '
          end

          Termbox.write(ch, x: px, y: py, fg: NORD_FG, bg: bg)
        end
      end

      # Draw child
      draw(child, bg)
    end

    matchpi %[(text caption0_string ¦ _
                    position: (x0←(%number u16) y0←(%number u16))
                    size: (w0←(%number u16) h0←(%number u16))
                    fg: (fr0←(%number u8) fg0←(%number u8) fb0←(%number u8))
                    bg: (br0←(%number u8) bg0←(%number u8) bb0←(%number u8))
                    pre: (%optional false pre_boolean))] do
      x, y, w, h = {x0, y0, w0, h0}.map(&.to(Int32))
      fr, fg, fb, br, bg, bb = {fr0, fg0, fb0, br0, bg0, bb0}.map(&.to(UInt8))
      caption = caption0.to(String)

      fg = Termbox::Color.rgb(fr, fg, fb)
      bg = Termbox::Color.rgb(br, bg, bb) 

      reader = Char::Reader.new(caption)

      h.times do |j|
        w.times do |i|
          chr = reader.current_char

          case chr
          when '\0'
            break
          when '\n'
            reader.next_char
            break
          else
            Termbox.write(chr, x: x + i, y: y + j, fg: fg, bg: bg)
          end

          break unless reader.next_char?
        end

        while !pre.true? && reader.current_char == ' '
          reader.next_char
        end

        break if reader.current_char == '\0'
      end
    end

    matchpi %[_dict] do
      frame.items.each { |child| draw(child, bbg) }
    end

    otherwise {}
  end
end

frames = Channel(Term::Dict).new

spawn do
  while frame = frames.receive?
    frame = pipe(frame, fit, fix)

    Term.case(frame) do
      matchpi %[(viewport child_ ¦ _ bg: (r0←(%number u8) g0←(%number u8) b0←(%number u8)))] do
        r, g, b = {r0, g0, b0}.map(&.to(UInt8))

        bg = Termbox::Color.rgb(r, g, b)

        Termbox.clear(bg: bg, fg: NORD_FG)
        draw(child, bg)
        Termbox.present
      end

      otherwise {}
    end
  end
rescue e
  frames.close
end

frame = ML.parse1(<<-WWML
;; TODO: Obviously we'd like to have width/height fill/hug separately.
(viewport position: (0 0) size: ($<w> $<h>) max-size: ($<w> $<h>) bg: (0 0 0)
  (col size: fill
    (box bg: (0 0 0) size: hug padding: 1
      (text size: hug fg: (255 255 255) bg: (0 0 0) pre: true
        "Header"))
    (box bg: (0 0 0) size: fill padding: 1
      (text size: hug fg: (255 255 255) bg: (0 0 0) pre: true
        $<body>))
    (box bg: (0 0 0) size: hug padding: 1
      (text size: hug fg: (255 255 255) bg: (0 0 0) pre: true
        "Footer"))))
WWML
).as_d

def manipulator(frame, name)
  keypaths = [] of Term::Dict
  Term.each_keypath_and_leaf(Term.of(frame)) do |kp, leaf|
    if leaf == name
      keypaths << Term[kp]
    end
    true
  end
  ->(frame : Term::Dict, mapper : Term -> Term) do
    keypaths.each do |keypath|
      v0 = frame.follow(keypath.items)
      v1 = mapper.call(v0)
      frame = frame.where(keypath.not_nil!.items, eq: v1)
    end
    frame
  end
end

body = manipulator(frame, Term.of(:"$<body>"))
width = manipulator(frame, Term.of(:"$<w>"))
height = manipulator(frame, Term.of(:"$<h>"))

Termbox.init
begin
Termbox.input_mode = Termbox::InputMode::Alt
Termbox.output_mode = Termbox::OutputMode::Truecolor

running = true

root0 = ML.parse(%[
  (cell 0 @x)
  (cell 0 @y)
  (button "Increment" @actions ())
  (button "Decrement" @actions ())
  ("" | "" () @user)
])

# Now why the f*** is it - 2?
frame = body.call(frame, ->(state : Term) { Term.of(ML.display(root0, endl: false)) })
frame = width.call(frame, ->(state : Term) { Term.of(Termbox.width - 2) })
frame = height.call(frame, ->(state : Term) { Term.of(Termbox.height - 2) })

# raise ""
while running
  frames.send(frame)

  event = Termbox.poll

  root1 = root0

  case event.type
  when .resize?
    frame = width.call(frame, ->(state : Term) { Term.of(event.resize_w - 2) })
    frame = height.call(frame, ->(state : Term) { Term.of(event.resize_h - 2) })
    frames.send(frame)
  when .key?
    if event.ch.zero? # Non-character key
      case event.key
      when .esc?
        root1 = edit(root1, Term.of(:key, :escape))
      when .tab?
        root1 = edit(root1, Term.of(:key, :tab))
      when .home?
        root1 = edit(root1, Term.of(:key, :home))
      when .end?
        root1 = edit(root1, Term.of(:key, :end))
      when .enter?
        root1 = edit(root1, Term.of(:key, :enter))
      when .backspace?, .backspace2?
        root1 = edit(root1, Term.of(:key, :backspace))
      when .delete?
        root1 = edit(root1, Term.of(:key, :delete))
      when .arrow_left?
        root1 = edit(root1, Term.of(:key, :left))
      when .arrow_right?
        root1 = edit(root1, Term.of(:key, :right))
      when .arrow_up?
        root1 = edit(root1, Term.of(:key, :up))
      when .arrow_down?
        root1 = edit(root1, Term.of(:key, :down))
      when .ctrl_c?
        running = false
        break
      when .ctrl_v?
      when .ctrl_a?
      when .ctrl_q?
      end
    else
      chr = event.ch.chr
      next unless chr.printable?
      root1 = edit(root1, Term.of(:input, chr))
    end
  end

  unless root0.same?(root1)
    frame = body.call(frame, ->(state : Term) { Term.of(ML.display(root1, endl: false)) })
    frames.send(frame)
    root0 = root1
  end
end
ensure
Termbox.shutdown
# puts ML.display(pipe(frame, fit, fix))
end
