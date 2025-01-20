require "./wirewright"
require "./baz4"

MEASUREMENTS = {} of String => {Int32, Int32}

def measure(string : String) : {Int32, Int32}
  MEASUREMENTS.put_if_absent(string) do
    w = h = 0
    string.each_line(chomp: false) do |line|
      w = Math.max(w, line.size)
      h += 1
    end
    {w, h}
  end
end

def measure(string : String, wlimit : Int32)
  reader = Char::Reader.new(string)

  x = w = h = 0

  until reader.current_char == '\0'
    h += 1

    wlimit.times do
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
  end

  {w, h}
end

def fit1(parent, phase, child)
  Term.case({parent, phase, child}) do
    # Calculate the intrinsic width of a text.
    givenpi %[_ teach (text caption_string ¦ _ w: content)] do
      w, _ = measure(caption.to(String))

      {parent, child.morph({:w, {:intrinsic, w}})}
    end

    # Calculate the intrinsic height of a text.
    givenpi %[_ teach (text caption_string ¦ _ h: content)] do
      _, h = measure(caption.to(String))

      {parent, child.morph({:h, {:intrinsic, h}})}
    end

    # Calculate padded width from intrinsic width.
    givenpi %[_ learn (_* ¦ _ w: (intrinsic w←(%number u16)) pl: (%optional 0 pl←(%number u8)) pr: (%optional 0 pr←(%number u8)))] do
      {parent, child.morph({:w, {:"padded", w + pl + pr}})}
    end

    # Calculate padded height from intrinsic height.
    givenpi %[_ learn (_* ¦ _ h: (intrinsic h←(%number u16)) pt: (%optional 0 pt←(%number u8)) pb: (%optional 0 pb←(%number u8)))] do
      {parent, child.morph({:h, {:"padded", h + pt + pb}})}
    end

    # Clip the padded width of a text at max-width. This requires text wrapping
    # which also affects the height of the text.
    givenpi %[_ learn (text caption_string ¦ _ w: (padded w←(%number u16)) max-w: wlimit←(%number u16) h: (padded _))] do
      wwrapped, hwrapped = measure(caption.to(String), wlimit.to(Int32))

      {parent, child.morph({:w, wwrapped}, {:h, {:padded, hwrapped}})}
    end

    # Clip the padded width of a child at max-width to obtain computed width.
    givenpi %[_ learn (_* ¦ _ w: (padded w←(%number u16)) max-w: wlimit←(%number u16))] do
      wclipped = Math.min(w.unsafe_as_n, wlimit.unsafe_as_n)

      {parent, child.morph({:w, wclipped})}
    end

    # Clip the padded height of a child at max-height to obtain computed height.
    givenpi %[_ learn (_* ¦ _ h: (padded h←(%number u16)) max-h: hlimit←(%number u16))] do
      hclipped = Math.min(h.unsafe_as_n, hlimit.unsafe_as_n)

      {parent, child.morph({:h, hclipped})}
    end

    # Bordered box should propagate max-width constraint minus the border.
    givenpi %[(box _ ¦ _ border: true max-w: w←(%number u16)) teach (_* ¦ _ max-w: (%- _))] do
      {parent, child.morph({:"max-w", w - 2})}
    end

    # Bordered box should propagate max-height constraint minus the border.
    givenpi %[(box _ ¦ _ border: true max-h: h←(%number u16)) teach (_* ¦ _ max-h: (%- _))] do
      {parent, child.morph({:"max-h", h - 2})}
    end

    # Propagate max-width constraints to a single child.
    givenpi %[(_ _ ¦ _ max-w: w←(%number u16)) teach (_* ¦ _ max-w: (%- _))] do
      {parent, child.morph({:"max-w", w})}
    end

    # Propagate max-height constraints to a single child.
    givenpi %[(_ _ ¦ _ max-h: h←(%number u16)) teach (_* ¦ _ max-h: (%- _))] do
      {parent, child.morph({:"max-h", h})}
    end

    # Propagate max-width constraints to children of (col).
    givenpi %[(col _+ ¦ _ max-w: w←(%number u16)) teach (_* ¦ _ max-w: (%- _))] do
      {parent, child.morph({:"max-w", w})}
    end

    # Propagate even max-height constraints to children of (col)
    givenpi %[(col children_+ ¦ _ max-h: h←(%number u16)) teach (_* ¦ _ max-h: (%- _))] do
      {parent, child.morph({:"max-h", h//children.size})}
    end

    # Box with borders should add its borders to its learned intrinsic width.
    givenpi %[(box _ ¦ _ border: true w: content) learn (_* ¦ _ w: w←(%number u16))] do
      {parent.morph({:w, w + 2}), child}
    end

    # Box with borders should add its borders to its learned intrinsic height.
    givenpi %[(box _ ¦ _ border: true h: content) learn (_* ¦ _ h: h←(%number u16))] do
      {parent.morph({:h, h + 2}), child}
    end

    # Learn width from a single child.
    givenpi %[(_ _ ¦ _ w: content) learn (_* ¦ _ w: w←(%number u16))] do
      {parent.morph({:w, {:intrinsic, w}}), child}
    end

    # Learn height from a single child.
    givenpi %[(_ _ ¦ _ h: content) learn (_* ¦ _ h: h←(%number u16))] do
      {parent.morph({:h, {:intrinsic, h}}), child}
    end

    # Inherit max width as width from single-child parent if width: max.
    #
    # Let col children inherit max-width if their width: max.
    givenpi(
      %[(_ _ ¦ _ max-w: w←(%number u16)) teach (_* ¦ _ w: max)],
      %[(col _+ ¦ _ max-w: w←(%number u16)) teach (_* ¦ _ w: max)]
    ) do
      {parent, child.morph({:w, w})}
    end

    # Inherit max height as height from single-child parent if height: max.
    givenpi %[(_ _ ¦ _ max-h: h←(%number u16)) teach (_* ¦ _ h: max)] do
      {parent, child.morph({:h, h})}
    end

    # Track the number of "size: max" children under "contenders".
    # Mark the height as well -- orelse we will never stop.
    givenpi %[(col _+ ¦ _ contenders: (%optional 0 contenders_)) teach (_* ¦ _ h: max)] do
      {parent.morph({:contenders, contenders + 1}), child.morph({:h, :contending})}
    end

    # Initialize available height with full height. We assume it is computed by now,
    # in whichever way.
    givenpi %[(col _+ ¦ _ available: (%- _) h: h←(%number u16)) learn _] do
      {parent.morph({:available, h}), child}
    end

    # Each child that knows its height should subtract it from the available height.
    # We mark each height -- orelse we will never stop.
    givenpi %[(col _+ ¦ _ available: avh←(%number u16)) learn (_* ¦ _ h: h←(%number u16))] do
      {parent.morph({:available, avh - h}), child.morph({:h, {:reserved, h}})}
    end

    # Each contender will take an even share of the available height when there are
    # no more reservations on it.
    #
    # TODO: we should refrain from using refine. Instead, a content-based dependency
    # should be established (rather than a rigid, phase-based one). We should count
    # the number of reservations and make each reserved child subtract itself from
    # that number. When reservations: 0, this means we can start distributing the
    # remaining available size between contenders.
    givenpi %[(col _+ ¦ _ available: avh←(%number u16) contenders: cont←(%number u16)) (refine 0) (_* ¦ _ h: contending)] do
      {parent.morph({:available, avh - avh//cont}, {:contenders, cont - 1}),
       child.morph({:h, avh//cont})}
    end

    # Lift height reservations once the available size is exhausted.
    #
    # TODO: Again, we should not rely on refine phases here. refine phase is a hack.
    givenpi %[(col _+ ¦ _ available: 0 contenders: 0) (refine 1) (_* ¦ _ h: (reserved h←(%number u16)))] do
      {parent, child.morph({:h, h})}
    end

    # Cleanup available, contenders.
    #
    # TODO: the same thing about refine.
    givenpi %[(col _+ ¦ _ available: 0 contenders: 0) (refine 2) _] do
      {parent.morph({:available, nil}, {:contenders, nil}), child}
    end

    otherwise { {parent, child} }
  end
end

def fix1(parent, phase, child)
  Term.case({parent, phase, child}) do
    # Center should center the child horizontally.
    givenpi %[(center _ ¦ _ pl: (%optional 0 pl←(%number u8)) l: l←(%number u16) w: w←(%number u16)) teach (_* ¦ _ l: (%- _) w: chw←(%number u16))] do
      {parent, child.morph({:l, l + pl + (w - chw)//2})}
    end

    # Center should center the child vertically.
    givenpi %[(center _ ¦ _ pt: (%optional 0 pt←(%number u8)) t: t←(%number u16) h: h←(%number u16)) teach (_* ¦ _ t: (%- _) h: chh←(%number u16))] do
      {parent, child.morph({:t, t + pt + (h - chh)//2})}
    end

    # Teach learned left position from single-child parent.
    givenpi %[(_ _ ¦ _ pl: (%optional 0 pl←(%number u8)) l: l←(%number u16)) teach (_* ¦ _ l: (%- _))] do
      {parent, child.morph({:l, l + pl})}
    end

    # Teach learned top position from single-child parent.
    givenpi %[(_ _ ¦ _ pt: (%optional 0 pt←(%number u8)) t: t←(%number u16)) teach (_* ¦ _ t: (%- _))] do
      {parent, child.morph({:t, t + pt})}
    end

    # Teach col's children about their left position.
    givenpi %[(col _+ ¦ _ pl: (%optional 0 pl←(%number u8)) l: l←(%number u16)) teach (_* ¦ _ l: (%- _))] do
      {parent, child.morph({:l, l + pl})}
    end

    # Learn top from padded col.
    #
    # NOTE: we rely here on the fact that the itemspart is traversed inorder
    # by orthor. It would be better if we did not.
    givenpi %[(col _+ ¦ _ t: t←(%number u16) offset: (%optional 0 o←(%number u16))) teach (_* ¦ _ t: (%- _) h: h←(%number u16))] do
      {parent.morph({:offset, o + h}), child.morph({:t, t + o})}
    end

    otherwise { {parent, child} }
  end
end

# Computes the sizes / sizing-related properties of everything.
def fit(frame : Term::Dict) : Term::Dict
  orthor(frame, ->fit1(Term::Dict, Term, Term))
end

# Computes the positions / position-related properties of everything.
def fix(frame : Term::Dict) : Term::Dict
  orthor(frame, ->fix1(Term::Dict, Term, Term))
end

# frame = ML.parse1(<<-WWML
# (viewport l: 0 t: 0 w: 64 h: 32 max-w: 64 max-h: 32 bg: (0 0 0)
#   (col w: max h: max
#     (box border: true bg: (0 0 0) w: content h: content pl: 1 pr: 1 pt: 1 pb: 1
#       (text w: content h: content fg: (255 255 255) bg: (0 0 0) pre: true
#         "Header"))
#     (box border: true bg: (0 0 0) w: max h: max pl: 1 pr: 1 pt: 1 pb: 1
#       (text w: content h: content fg: (255 255 255) bg: (0 0 0) pre: true
#         "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat."))
#     (box border: true bg: (0 0 0) w: max h: max pl: 1 pr: 1 pt: 1 pb: 1
#       (text w: content h: content fg: (255 255 255) bg: (0 0 0) pre: true
#         "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat."))
#     (box border: true bg: (0 0 0) w: content h: content pl: 1 pr: 1 pt: 1 pb: 1
#       (text w: content h: content fg: (255 255 255) bg: (0 0 0) pre: true
#         "Footer"))))
# WWML
# ).as_d

# puts ML.display(pipe(frame, fit, fix))

# {% skip_file %}

require "./libtermbox2"

NORD_BG = Termbox::Color.new(0x2e3440)
NORD_FG = Termbox::Color.new(0xeceff4)
NORD_FG_DIM = Termbox::Color.new(0xd8dee9)
NORD_BLUE_DARK = Termbox::Color.new(0x5e81ac)
NORD_BLUE_LIGHT = Termbox::Color.new(0x81a1c1)

def draw(frame) : Nil
  Term.case(frame) do
    matchpi %[(box child_ ¦ _
                   border: border_boolean
                   l: l←(%number u16)
                   t: t←(%number u16)
                   w: w0←(%number u16)
                   h: h0←(%number u16)
                   bg: (r0←(%number u8) g0←(%number u8) b0←(%number u8)))] do
      x, y, w, h = {l, t, w0, h0}.map(&.to(Int32))
      r, g, b = {r0, g0, b0}.map(&.to(UInt8))

      bg = Termbox::Color.rgb(r, g, b)

      # Draw frame & fill background
      (y...y + h).each do |py|
        (x...x + w).each do |px|
          ch = ' '

          if border.true?
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
            end
          end

          Termbox.write(ch, x: px, y: py, fg: NORD_FG, bg: bg)
        end
      end

      # Draw child
      draw(child)
    end

    matchpi %[(text caption0_string ¦ _
                    l: l←(%number u16)
                    t: t←(%number u16)
                    w: w0←(%number u16)
                    h: h0←(%number u16)
                    fg: (fr0←(%number u8) fg0←(%number u8) fb0←(%number u8))
                    bg: (br0←(%number u8) bg0←(%number u8) bb0←(%number u8))
                    pre: (%optional false pre_boolean))] do
      x, y, w, h = {l, t, w0, h0}.map(&.to(Int32))
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
      frame.items.each { |child| draw(child) }
    end

    otherwise {}
  end
end

def show(frame : Term::Dict)
  fixed = pipe(frame, fit, fix)
  
  Term.case(fixed) do
    matchpi %[(viewport child_ ¦ _ bg: (r0←(%number u8) g0←(%number u8) b0←(%number u8)))] do
      r, g, b = {r0, g0, b0}.map(&.to(UInt8))
  
      bg = Termbox::Color.rgb(r, g, b)
  
      Termbox.clear(bg: bg, fg: NORD_FG)
      draw(child)
      Termbox.present
    end
  
    otherwise {}
  end
end

def manipulate(frame, name)
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

frame = ML.parse1(<<-WWML
(viewport l: 0 t: 0 w: max h: max max-w: $<w> max-h: $<h> bg: (0 0 0)
  (col w: max h: max
    (box border: false bg: (255 255 255) w: max h: content pl: 1 pr: 1 pt: 1 pb: 1
      (text w: content h: content fg: (0 0 0) bg: (255 255 255) pre: true
        "Header"))
    (box border: true bg: (0 0 0) w: max h: max pl: 1 pr: 1 pt: 1 pb: 1
      (center w: max h: max
        (text w: content max-w: 20 h: content fg: (255 255 255) bg: (0 0 0) pre: true
          "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.")))
    (box border: true bg: (0 0 0) w: max h: max pl: 1 pr: 1 pt: 1 pb: 1
      (text w: content h: content fg: (255 255 255) bg: (0 0 0) pre: true
        "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat."))
    (box border: true bg: (0 0 0) w: max h: content pl: 1 pr: 1 pt: 1 pb: 1
      (text w: content h: content fg: (255 255 255) bg: (0 0 0) pre: true
        "Footer"))))
WWML
).as_d

Termbox.init

at_exit { Termbox.shutdown }

Process.on_terminate do
  puts "Aborting..."

  # Make sure to run the at_exit handlers.
  abort
end

Termbox.input_mode = Termbox::InputMode::Alt
Termbox.output_mode = Termbox::OutputMode::Truecolor



# body = manipulate(frame, Term.of(:"$<body>"))
width = manipulate(frame, Term.of(:"$<w>"))
height = manipulate(frame, Term.of(:"$<h>"))

frame = width.call(frame, ->(state : Term) { Term.of(Termbox.width - 2) })
frame = height.call(frame, ->(state : Term) { Term.of(Termbox.height - 2) })

while true
  show(frame)

  event = Termbox.poll

  case event.type
  when .resize?
    frame = width.call(frame, ->(state : Term) { Term.of(event.resize_w - 2) })
    frame = height.call(frame, ->(state : Term) { Term.of(event.resize_h - 2) })
  when .key?
    if event.ch.zero? # Non-character key
      case event.key
      when .esc?
      when .tab?
      when .home?
      when .end?
      when .enter?
      when .backspace?, .backspace2?
      when .delete?
      when .arrow_left?
      when .arrow_right?
      when .arrow_up?
      when .arrow_down?
      when .ctrl_c?
        break
      when .ctrl_v?
      when .ctrl_a?
      when .ctrl_q?
      end
    else
      chr = event.ch.chr
      next unless chr.printable?
    end
  end
end
