# WwML

> [!WARNING]
> There are some inconsistencies with the actual grammar here and there. They
> remain to be fixed.

*WwML* stands for *Wirewright Main Language*. WwML is a textual representation
of Wirewright *terms*.

Essentially, WwML is a *theoretical* answer to the questions: "How do I convert
strings of characters to terms and back? And what kinds of strings can be converted
to terms?". WwML defines such kinds of strings; and WwML *readers* answer the practical
question of how to actually do the conversion.

Different subsystems of Wirewright may have their "presence" in (or "contribution" to)
WwML in the form of groups of shorthands that expand to that subsystem's preferred
term representation. Since they all use WwML, they all want a "comfortable way" to express
their term representations in it. For example, the edge shorthand `@x` represents
the contribution of Delta7 to WwML. By far, the most major contributor to WwML in this
sense is the M1 pattern matching engine.

The majority of WwML is about *shorthands* -- *conveniences*, *syntactic sugar*. In other
words, WwML should be thought of as a *system of interacting shorthands* on top of
a "basis language" -- S-expressions extended with key-value pairs. In other words, there
is little "language" or intentional "language design" above S-expressions; only shorthands
that manage to interact with each other in one way or another, forming larger, emergent
(in the simplest sense of the word) syntactic complexes.

WwML is a rather complicated *constructor* or *(de)serialization* engine for the six
kinds of terms: dictionary, number, string, symbol, boolean, and blob terms. There is no AST --
terms *are* the AST. Any dictionary representation can be rewritten in terms
of another; and any shorthand lowered to its expansion without leaving the representation.
This is because they are ultimately still describing the same data structure: a dictionary,
most of the times.

Interestingly, WwML can also be thought of as a "compressed representation" of terms, since
a term's text representation is usually much more efficient in terms of its byte size than
the corresponding in-memory term. Something like `(+ 1 2)` takes only a few bytes in ASCII
but may very well take hundreds of bytes in-memory with all the indexing and control structure
overhead (I am working actively on reducing the memory footprint of dictionaries specifically,
but there are limits).

> [!NOTE]
> Character sets are expressed in [Crystal character set notation](https://crystal-lang.org/api/1.16.2/Char.html#in_set%3F%28%2Asets%3AString%29%3ABool-instance-method).

## The characters you will need to be able to type

WwML uses a number of Unicode characters. You will have to configure your OS to be
able to type them (e.g. using the Compose key).

At first, I was very reluctant to use Unicode characters. They obviously make WwML look cryptic
and perhaps, APL-like; obscuring the simplicity of WwML's core. They can also be quite clumsy to
type, until you're accustomed to them; then, it's as easy as typing anything else.

The amounts of compression these characters provide, are sometimes substantial; and
worth the trade-off in my opinion. Moreover, this way, most of the easy-to-type ASCII
character sequences are left for the user to define and use in their names.

Some of the characters were picked due to their similarity to the underlying concept. Others,
like `◇`, are simply *ideograms* -- the glyphs themselves do not mean or remind one of anything
in particular; but rather, are used as "opaque pointers" to an idea.

I would very much like to design custom glyphs, specifically for WwML, which would make
much more sense visually. In fact, some prototypes exist already. However, committing to this
would complicate distribution and actual use of WwML significantly; much more so than
a small selection of Unicode characters. I'm not a font designer, too, and this brings in its
own complications.

| Character | Unicode codepoint | Recommended key combo                                         | Available in default XCompose config |
| --------- | ----------------- | ------------------------------------------------------------- | ------------------------------------ |
| `°`       | U+00B0            | <kbd>Compose</kbd> + <kbd>o</kbd> + <kbd>o</kbd>              | yes                                  |
| `∈`       | U+2208            | <kbd>Compose</kbd> + <kbd>m</kbd> + <kbd>m</kbd>              | **no**                               |
| `∉`       |                   | <kbd>Compose</kbd> + <kbd>M</kbd> + <kbd>m</kbd>              | **no**                               |
| `⊆`       | U+2286            | <kbd>Compose</kbd> + <kbd>m</kbd> + <kbd>s</kbd>              | **no**                               |
| `⊂`       | U+2282            | <kbd>Compose</kbd> + <kbd>m</kbd> + <kbd>S</kbd>              | **no**                               |
| `⸢`       | U+2E22            | <kbd>Compose</kbd> + <kbd>"</kbd> + <kbd>[</kbd>              | **no**                               |
| `⸣`       | U+2E23            | <kbd>Compose</kbd> + <kbd>"</kbd> + <kbd>]</kbd>              | **no**                               |
| `⟨`       | U+27E8            | <kbd>Compose</kbd> + <kbd>{</kbd> + <kbd>{</kbd>              | **no**                               |
| `⟩`       | U+27E9            | <kbd>Compose</kbd> + <kbd>}</kbd> + <kbd>}</kbd>              | **no**                               |
| `¦`       | U+00A6            | <kbd>Compose</kbd> + <kbd>\|</kbd> + <kbd>\|</kbd>            | **no**                               |
| `…`       | U+2026            | <kbd>Compose</kbd> + <kbd>.</kbd> + <kbd>.</kbd>              | yes                                  |
| `⁺`       | U+207A            | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>+</kbd>              | yes                                  |
| `⁻`       | U+207B            | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>-</kbd>              | yes                                  |
| `⋮`       | U+22EE            | <kbd>Compose</kbd> + <kbd>:</kbd> + <kbd>:</kbd>              | **no**                               |
| `×`       | U+00D7            | <kbd>Compose</kbd> + <kbd>x</kbd> + <kbd>x</kbd>              | yes                                  |
| `≡`       | U+2261            | <kbd>Compose</kbd> + <kbd>=</kbd> + <kbd>=</kbd>              | **no**                               |
| `←`       | U+2190            | <kbd>Compose</kbd> + <kbd><</kbd> + <kbd>-</kbd>              | yes                                  |
| `↑`       | U+2191            | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>\|</kbd>             | yes                                  |
| `↓`       | U+2193            | <kbd>Compose</kbd> + <kbd>v</kbd> + <kbd>\|</kbd>             | yes                                  |
| `→`       | U+2192            | <kbd>Compose</kbd> + <kbd>-</kbd> + <kbd>></kbd>              | yes                                  |
| `₀`..`₉`  | U+2080-U+2089     | <kbd>Compose</kbd> + <kbd>_</kbd> + <kbd>0</kbd>-<kbd>9</kbd> | yes                                  |
| `⁰`..`⁹`  |                   | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>0</kbd>-<kbd>9</kbd> | yes                                  |
| `⁺`       |                   | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>+</kbd>              | yes                                  |
| `⁻`       |                   | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>+</kbd>              | yes                                  |
| `₊`       |                   | <kbd>Compose</kbd> + <kbd>_</kbd> + <kbd>+</kbd>              | yes                                  |
| `₋`       |                   | <kbd>Compose</kbd> + <kbd>_</kbd> + <kbd>-</kbd>              | yes                                  |
| `⎡`       | U+23A1            | <kbd>Compose</kbd> + <kbd>[</kbd> + <kbd>\|</kbd>             | **no**                               |
| `⎤`       | U+23A4            | <kbd>Compose</kbd> + <kbd>\|</kbd> + <kbd>]</kbd>             | **no**                               |
| `±`       | U+00B1            | <kbd>Compose</kbd> + <kbd>+</kbd> + <kbd>-</kbd>              | yes                                  |
| `⸨`       |                   | <kbd>Compose</kbd> + <kbd>(</kbd> + <kbd>(</kbd>              | **no**                               |
| `⸩`       |                   | <kbd>Compose</kbd> + <kbd>)</kbd> + <kbd>)</kbd>              | **no**                               |
| `⟦`       |                   | <kbd>Compose</kbd> + <kbd>[</kbd> + <kbd>[</kbd>              | **no**                               |
| `⟧`       |                   | <kbd>Compose</kbd> + <kbd>]</kbd> + <kbd>]</kbd>              | **no**                               |
| `⫽`       |                   | <kbd>Compose</kbd> + <kbd>/</kbd> + <kbd>/</kbd>              | **no**                               |
| `⁑`       |                   | <kbd>Compose</kbd> + <kbd>*</kbd> + <kbd>*</kbd>              | **no**                               |
| `⍊`       |                   | <kbd>Compose</kbd> + <kbd>\|</kbd> + <kbd>_</kbd>             | **no**                               |
| `◇`       |                   | <kbd>Compose</kbd> + <kbd><</kbd> + <kbd>></kbd>              | **no**                               |
| `▢`       |                   | <kbd>Compose</kbd> + <kbd>(</kbd> + <kbd>)</kbd>              | **no**                               |
| `⟅`       |                   | <kbd>Compose</kbd> + <kbd>*</kbd> + <kbd>(</kbd>              | **no**                               |
| `⟆`       |                   | <kbd>Compose</kbd> + <kbd>*</kbd> + <kbd>)</kbd>              | **no**                               |
| `⸍`       |                   | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>/</kbd>              | **no**                               |
| `⸝`       |                   | <kbd>Compose</kbd> + <kbd>_</kbd> + <kbd>/</kbd>              | **no**                               |
| `≈`       |                   | <kbd>Compose</kbd> + <kbd>~</kbd> + <kbd>~</kbd>              | yes                                  |
| `⟪`       |                   | <kbd>Compose</kbd> + <kbd>"</kbd> + <kbd>{</kbd>              | **no**                               |
| `⟫`       |                   | <kbd>Compose</kbd> + <kbd>"</kbd> + <kbd>}</kbd>              | **no**                               |
| `∥`       |                   | <kbd>Compose</kbd> + <kbd>\</kbd> + <kbd>\</kbd>              | **no**                               |
| `⟬`       |                   | <kbd>Compose</kbd> + <kbd>\</kbd> + <kbd>[</kbd>              | **no**                               |
| `⟭`       |                   | <kbd>Compose</kbd> + <kbd>\</kbd> + <kbd>]</kbd>              | **no**                               |

### XCompose

Here are the XCompose mappings for the table above. This should be put in `.XCompose` in the home directory.

```xcompose
<Multi_key> <m> <m> : "∈"
<Multi_key> <M> <m> : "∉"
<Multi_key> <m> <s> : "⊆"
<Multi_key> <m> <S> : "⊂"
<Multi_key> <asciicircum> <bracketleft> : "⸢"
<Multi_key> <asciicircum> <bracketright> : "⸣"
<Multi_key> <underscore> <bracketleft> : "⸤"
<Multi_key> <underscore> <bracketright> : "⸥"
<Multi_key> <braceleft> <braceleft> : "⟨"
<Multi_key> <braceright> <braceright> : "⟩"
<Multi_key> <bar> <bar> : "¦"
<Multi_key> <colon> <colon>  : "⋮"
<Multi_key> <equal> <equal> : "≡"
<Multi_key> <bar> <bracketleft> : "⎡"
<Multi_key> <bar> <bracketright> : "⎤"
<Multi_key> <parenleft> <parenleft> : "⸨"
<Multi_key> <parenright> <parenright> : "⸩"
<Multi_key> <bracketleft> <bracketleft> : "⟦"
<Multi_key> <bracketright> <bracketright> : "⟧"
<Multi_key> <slash> <slash> : "⫽"
<Multi_key> <asterisk> <asterisk> : "⁑"
<Multi_key> <bar> <underscore> : "⍊"
<Multi_key> <less> <greater> : "◇"
<Multi_key> <bracketleft> <bracketright> : "▢"
<Multi_key> <asterisk> <parenleft> : "⟅"
<Multi_key> <asterisk> <parenright> : "⟆"
<Multi_key> <asciicircum> <slash> : "⸍"
<Multi_key> <underscore> <slash> : "⸝"
<Multi_key> <quotedbl> <braceleft> : "⟪"
<Multi_key> <quotedbl> <braceright> : "⟫"
<Multi_key> <backslash> <backslash> : "∥"
<Multi_key> <backslash> <bracketleft> : "⟬"
<Multi_key> <backslash> <bracketright> : "⟭"

# Used sometimes in docs and comments. Not used in WwML.
<Multi_key> <asciicircum> <asciicircum> : "⏏"
```

## What fonts you are recommended to use with WwML

- [Julia Mono](https://juliamono.netlify.app/) and [DejaVu Sans Mono] have all of the glyphs that WwML needs.
- [Fira Code](https://github.com/tonsky/FiraCode) appears to be missing some but is usable otherwise.
- [IBM Plex](https://github.com/IBM/plex) and its Mono variant misses lots of the glyphs; on the other
  hand, the Plex family is the "official" font for Wirewright (in a sense); in addition, IBM Plex Mono
  has neat italics.

## Comma

The comma character is treated as whitespace and can be used where whitespace
can be used. E.g. `x,y` is the same as `x y`.

## Lexical choice over symbols

WwML provides several lexical choice operators over symbols.

Lexical choice can be of two general forms:

- `⫽` specifies a choice between individual symbolic characters
- `⸨⸩` specifies a comma-separated choice between symbolics

Both forms operate on *blocks*. Blocks are regions of source code delimited by
one or more blank lines, the document section delimiter `---`, the block delimiter
`∥`, or end-of-input.

> [!IMPORTANT]
> All choices in a block must have the same *arity* -- the same number of options.

For example, the following: `x⫽y` is the same as writing `x y`. `dn-w⫽h` is the same
as writing `dn-w dn-h`. Similarly, `pos-x⫽y⫽z` is the same as writing `pos-x pos-y pos-z`,
and `pl⫽r⫽t⫽b` is the same as writing `pl pr pt pb`.

Use the double parens to alternate over sequences of characters:
`⸨padding,margin⸩-top` is the same as writing `padding-top margin-top`.

Here is a (much?) more sophisticated example:

```wwml
(padding {¦ up-w⫽h: ±src} ⍊ ±px⫽y -up-w⫽h: dst)
  <> {dst: $'(+ →src →px⫽y), (src): ()}
```

...which expands to the following:

```wwml
(padding {¦ up-w: ±src} ⍊ ±px -up-w: dst)
  <> {dst: $'(+ →src →px), (src): ()}
(padding {¦ up-h: ±src} ⍊ ±py -up-h: dst)
  <> {dst: $'(+ →src →py), (src): ()}
```

Instead of paying attention to the cryptic glyphs, note instead how `⫽` is treated.

### Delayed choice

WwML supports *1-delayed choice* form of `⸨⸩`: `⟦⟧`. On the first expansion pass, `⫽` and `⸨⸩`
instantiate the block they are in; and `⟦⟧` is replaced with `⸨⸩`. On the second pass,
`⸨⸩` instantiates the block instances from the previous pass. This may lead to situations
on the extreme end of brevity, so to speak, such as in the following rule:

```wwml
(limit _ ⍊ up-w⫽h: (arg ±n ⍊ -◇_) ±⟦min,max⟧-w⫽h)
  <> {n: $'(⟦max,min⟧ →⟦min,max⟧-w⫽h →n), ◇: true}
```

This rule should be imagined to expand on the first pass to:

```wwml
(limit _ ⍊ up-w: (arg ±n ⍊ -◇_) ±⸨min,max⸩-w)
  <> {n: $'(⸨max,min⸩ →⸨min,max⸩-w →n), ◇: true}
(limit _ ⍊ up-h: (arg ±n ⍊ -◇_) ±⸨min,max⸩-h)
  <> {n: $'(⸨max,min⸩ →⸨min,max⸩-h →n), ◇: true}
```

Followed by the second pass, resulting in:

```wwml
(limit _ ⍊ up-w: (arg ±n ⍊ -◇_) ±min-w)
  <> {n: $'(max →min-w →n), ◇: true}
(limit _ ⍊ up-w: (arg ±n ⍊ -◇_) ±max-w)
  <> {n: $'(min →max-w →n), ◇: true}
(limit _ ⍊ up-h: (arg ±n ⍊ -◇_) ±min-h)
  <> {n: $'(max →min-h →n), ◇: true}
(limit _ ⍊ up-h: (arg ±n ⍊ -◇_) ±max-h)
  <> {n: $'(min →max-h →n), ◇: true}
```

WwML still has plenty of work to do. This is *not at all* the final expansion. In fact,
here it is, just to scare you:

```wwml
((backmap
  (%partition
   (limit _)
   (%layer
    _
    {up-w:
      (%partition
       (arg (%let n _number))
       (%layer _ {DvqKKxWNxnYd: (%- _ DvqKKxWNxnYd)})),
     min-w: (%let min-w _number)}))
  {DvqKKxWNxnYd: true,
   n: ($once (max ($my min-w) ($my n)))})
 (backmap
  (%partition
   (limit _)
   (%layer
    _
    {up-h:
      (%partition
       (arg (%let n _number))
       (%layer _ {QrgjxEcKbwAe: (%- _ QrgjxEcKbwAe)})),
     min-h: (%let min-h _number)}))
  {n: ($once (max ($my min-h) ($my n))),
   QrgjxEcKbwAe: true})
 (backmap
  (%partition
   (limit _)
   (%layer
    _
    {max-w: (%let max-w _number),
     up-w:
      (%partition
       (arg (%let n _number))
       (%layer _ {HbppyjUsWsqc: (%- _ HbppyjUsWsqc)}))}))
  {HbppyjUsWsqc: true,
   n: ($once (min ($my max-w) ($my n)))})
 (backmap
  (%partition
   (limit _)
   (%layer
    _
    {max-h: (%let max-h _number),
     up-h:
      (%partition
       (arg (%let n _number))
       (%layer _ {idFPwhUQXez: (%- _ idFPwhUQXez)}))}))
  {n: ($once (min ($my max-h) ($my n))),
   idFPwhUQXez: true}))
```

This demonstrates -- one may say *"very neatly"* -- just how much compression WwML provides.
Thanks to WwML, you will rarely, if ever, even *see* terms as scary as this; let alone write
them by hand. But remember always, that *this* is how most of them look like when expanded.

> [!NOTE]
> At the moment, 2-delayed choices and so on are *not* supported.

## Comments

### Lexical comments

Inline comments start with `;;` and extend to the end of the line or end-of-input.

```wwml
;; Lorem ipsum dolor sit amet

(+ 1  ;; qui minim labore
   2) ;; adipisicing minim sint
```

### Structural comments

An entry (item or pair) can be commented out using the `;` prefix.

> [!NOTE]
> The commented-out entry **must** be a syntactically valid.

Structural comments are very useful for experimentation, e.g. to disable test cases
or arguments quickly.

```wwml
;(+ 1 2)
;; Same as writing nothing.

(+ ;1 2)
;; Same as `(+ 2)`

(+ 1 ;2)
;; Same as `(+ 1)`
```

> [!NOTE]
> Structural comments work where whitespace works. Note, however, that you can only
> comment out *items*, *pairs*, or rules this way, as in `;x: 100`, or `;:x`, or
> `;M_ <> {M: qux}` (comments the entire rule, since commenting just `M_` would be
> syntactically invalid; it'd be a headless rule, and headless rules don't survive
> for long!)
>
> You cannot comment selectors, keys, etc. structurally. Use inline comments for this.

### Selection comments

Selection comments are analogous to structural comments, except this time you're
not commenting a specific entity out; but rather, commenting *all other entities* out.
This is also referred to as *focusing* the entity, e.g., focusing an item. Selection
comments are written using the `;,` prefix.

```wwml
(+ ;,1 2 x: 100 y: 200)
;; Same as: `(1 x: 100 y: 200)`

(+ 1 ;,2 x: 100 y: 200)
;; Same as: `(2 x: 100 y: 200)`

(;,+ 1 ;,2 x: 100 y: 200)
;; Same as: `(+ 2 x: 100 y: 200)`
```

Selection comments work on three distinct categories of entries: *rules*, *items*, and
*pairs*. When you focus a rule, WwML will blur all other rules. Items and pairs will
remain intact. Similarly for items and pairs.

Just like structural comments, selection comments are useful for experimentation;
especially isolation. Think isolating test cases: most often you don't need an entire
harness for this; a tiny syntactic feature suffices.

> [!NOTE]
> Selection comments are currently only supported in dictionaries of the general form
> (`(...)`) and sections (code between `---` in the document dict, or all code if the file
> is parsed as one huge section).
>
> Selection comments cannot be "commented out" with structural ones because selection
> comments aren't terms. They are interpreted by the dictionary that you place them
> in, because only that dictionary knows which entries to focus or blur.

## Boolean terms

```wwml
true  ;; boolean true
false ;; boolean false
```

## Symbol terms

Symbol terms are represented by combinations of one or more characters from
the set `0-9_'!$%&*+\-\^./#<=>?~|∞°∈⊆⊂∪∩\`, union Unicode letters (Unicode General Category L).
The characters in this set are called *symbolic*.

```wwml
abc
foo42
x'
divide-by-zero?
+
<=
π
⊆
λx
∞/2
%item°
```

### Minor notes

There are several unfortunate ambiguities with symbols, but they rarely if ever
show up in practice outside of Wirewright internals.

- Symbols cannot start with the single quote character `'`. This will invoke
  the quote prefix instead. However, symbols may contain the quote character
  elsewhere. In such cases, the quote character represents the [prime symbol](https://en.wikipedia.org/wiki/Prime_(symbol)).
  In Wirewright, we often use the prime symbol to "version" names, i.e. to denote
  successors: `x` means the first or base x, `x'` means its successor, `x''` means
  the successor of `x'` and so on.
- Symbols can start with `+`, `-`, `^`, and `$`. They are simultaneously prefix operators.
  They are known internally as *ambiguous prefixes*.

### Raw symbol literal

Use the raw symbol literal to represent terms that contain characters outside of the set
of symbolic characters, or to guarantee symbol treatment in case the symbol is read
ambiguously as a prefix followed by symbol etc.

```wwml
⸍hello⸝ ;; is the same as writing: hello
⸍123⸝   ;; symbol 123, not number
⸍%'qux⸝ ;; symbol %'qux, not prefix %' followed by symbol qux
```

You can use any character in the raw symbol literal, including properly
matched ⸍⸝:

```wwml
⸍⸍nested⸝ raw symbol brackets and
newline⸝

⸍(qux)⸝
⸍a: b⸝
⸍   ⸝
⸍⸝
```

These are all treated as symbols if written using the raw symbol literal.

## Number terms

Wirewright number terms can be *exact* or *approximate*.

Exact numbers are represented using a rational. Approximate (inexact) numbers
are represented using a 32-bit float.

Some operations maintain exactness of their arguments (e.g. `+`, `*`), while
others yield an approximate result (e.g. `sqrt`, rational raised to the power
of rational).

Approximate-ness is "contagious": any operation involving an approximate number
will itself return an approximate number.

Approximate numbers are prefixed with `≈` when displayed. Likewise, you can use
prefix `≈` to denote approximate-ness manually.

### Basic numbers

```wwml
0
42
12345
```

### Sign

You can optionally precede number terms with a `+` or `-` sign.

```wwml
+17
-8
```

Preceding a number written using radix notation with `-` will wrap the symbolic
representation in `(- _)`:

```wwml
-1011₂  ;; is the same as writing: (- (digits 1 0 1 1 radix: 2))
-beef₁₆ ;; is the same as writing: (- (digits 11 14 14 15 radix: 16))
```

#### Radix notation

Base (also referred to as radix to avoid confusion with the word base in general, which
is useful in other contexts) can be given by a sequence of one or two postfix subscript
digits. Minimum allowed radix is 1. Maximum allowed radix is 62.

Characters from the following set are used **inorder, as the radix increases**: `0-9A-Za-z`.
Note, however, that in unary, we use `1` for digits instead of `0`.

Radices less than or equal to 36 are case-insensitive.

```wwml
1011₂ ;; base-2 (binary)
755₈  ;; base-8 (octal)
1F₁₆  ;; base-16 (hexadecimal)
Z3₃₆  ;; base-36
Zz₆₂  ;; base-62
```

**Retention**: WwML is not a calculator, nor will it burn information for you. WwML
is a notation. It *does not* simplify radix notation to decimal. For example, `1f₁₆`
is the same as writing `(digits 1 15 radix: 16)`. It is up to the client to decide
what to do with this term. Some clients (analogous to "runtimes") may want to e.g.
support radix-aware addition or otherwise manipulate numbers with explicit base.
WwML will not interfere by reducing everything to decimal like traditional languages do.

### Decimal form for rationals

```wwml
;; INVALID: 3.
;; INVALID: .3
3.0
0.3
0.0
1.234
```

Use the fractional form to write non-terminating fractions such as `0.333...`.

### Fractional form for rationals

A fraction is a number of the form `a/b`, where `a` is an integer, and `b`
is a nonzero natural number.

```wwml
;; INVALID: 1/0
;; INVALID: 1/-5
1/3          ;; fraction (one third)
-3/4         ;; negative fraction
```

Fractions are *not* retained. This is because the number term is internally represented
as a fraction (a rational number); there is no point in retention here because numbers
already *are*, in a way, such a retention.

Decimal-representable fractions will be printed using the decimal form. E.g. `1/25`
will be printed as `0.04`.

Non-terminating fractions such as `1/3` will be printed using the fractional form.

### Scientific notation

```wwml
1e6          ;; scientific notation (1 million)
-2.5e-3      ;; scientific notation (-0.0025)
+4.0e+2      ;; scientific notation (400)
```

**Retention**: Scientific notation is *retained* similarly to radix notation, represented
as `(sci mantissa_ exponent_)`. For instance, writing `1e6` results in `(sci 1 6)`,
and `-2.5E-3` results in `(sci -2.5 -3)`.

### Separating blocks of digits with `_`

Non-leading and non-trailing underscore is ignored in all number literals.
It can be used to help separate blocks of digits.

```wwml
1001_1010₂
755_155₈
FFFF_FFFF₁₆

3.14_15_92

;; INVALID: 3._141592
;; INVALID: 3.141592_

100_000

;; WRONG: _100_000 (this creates a symbol!)
;; INVALID: 100_000_

100_000/200_000

HELLO_world₆₂
```

### Denoting approximate numbers

Use prefix `≈` to denote approximate numbers. This turns on approximate
arithmetic immediately.

```
123   ;; 123
≈123  ;; ≈123.0
1/3   ;; 1/3
≈1/3  ;; ≈0.3333333333333333

;; Signed

≈+123.456  ;; ≈123.456
≈-1/3      ;; ≈-0.3333333333333333

;; Scientific

≈1.23e-4  ;; (sci ≈1.23 ≈-4)
```

## String terms

### Standard form

Escape sequences are initiated by `\`.

```wwml
"Lorem ipsum dolor sit amet."
```

### Multiline form

> [!NOTE]
> Multiline form is planned but not implemented at the moment. Use the backslash-newline
> escape sequence detailed below.

### Escape sequences

- `\"` is the same as `\x22` (`"`)
- `\\` is the same as `\x5C` (`\`)
- `\b` is the same as `\x08` (ASCII backspace)
- `\f` is the same as `\x0C` (ASCII formfeed)
- `\n` is the same as `\x0A` (newline character)
- `\t` is the same as `\x09` (tab)
- `\r` is the same as `\x0D` (carriage return)
- `\⸢` is the same as `\u{2E22}` (top left half bracket).
- `\x__` inserts a Unicode codepoint given exactly two hex digits. Anything past the two digits is part of the string.
- `\u____` inserts a Unicode codepoint given exactly four hex digits. Anything past the four digits is part of the string.
- `\u{_}`, `\u{__}`, `\u{___}`, `\u{____}`, `\u{_____}`, `\u{______}` inserts a Unicode
  codepoint given up to 6 hex digits.
- `\u[name]` inserts a Unicode codepoint given its case-insensitive name.
- `\u[:name:]` inserts an emoji based on its [gemoji name](https://github.com/github/gemoji/blob/0eca75db9301421efc8710baf7a7576793ae452a/db/emoji.json).
- `\u[greek letter name]` inserts a lowercase Greek letter given its *case-sensitive* name (e.g. `"\u[delta]"` is the same as writing `"δ"`)
- `\u[Greek letter name]` (note the first letter is uppercase) inserts an uppercase Greek letter given
   its *case-sensitive* name (e.g. `"\u[Delta]"` is the same as writing `"Δ"`).

```wwml
"\x41x"  ;; "Ax"
"\x4142" ;; "A42"

"\u{0157}"
;; ŗ

"\u[LATIN SMALL leTter R with CEDILLA]"
;; ŗ

"\u[:poop:]"
;; 💩
```

You can escape newline *and the following horizontal whitespace* using `\␤`, as in:

```wwml
"Lorem ipsum dolor sit amet, qui minim labore adipisicing \
 minim sint cillum sint consectetur cupidatat."
```

### Interpolation

Interpolation uses the characters `⸢` and `⸣`. You can escape `⸢` with `\⸢` or one of
the `\u` escape sequences if necessary.

Interpolation is recursive: what goes inside is an entire *term*, which must be syntactically valid.
The term could be a dictionary, a string with more interpolations, etc.

Interpolation is syntactic sugar for dictionaries of the form `(~ s1 s2 ... sn)`.

```wwml
"Your name is: ⸢name⸣. Have a nice day!"
;; is the same as writing: (~ "Your name is: " name ". Have a nice day!")

"Your name is: ⸢first-name⸣ ⸢last-name⸣. Have a nice day!"
;; is the same as writing: (~ "Your name is: " first-name " " last-name ". Have a nice day!")

"1 + 1 = ⸢(+ 1 1)⸣"
;; is the same as writing: (~ "1 + 1 =" (+ 1 1))
```

### Raw strings

Raw strings can be expressed using `⎡` and `⎤`. Raw strings support nesting, but do not
support escape sequences or interpolation.

```wwml
⎡hello ⎡nested⎤ world⎤
```

## Blob terms

Blobs are very much like strings except strings are used for plaintext data,
and their design and optimizations bias strongly toward Unicode (UTF-8). Blobs,
on the other hand, are simply vectors of bytes, with no presuppositions about
their content.

Blobs start with `⟬` and end `⟭`. Blobs are written in hexadecimal: between `⟬⟭`s goes
a sequence of zero or more hexadecimal *digit*s. Whitespace can be used to delimit
digits into *digit blocks*.

```wwml
⟬deadbeef⟭

;; This is the recommended style. This style is also used when pretty-printing.
;; Byte boundaries are clearly visible.
⟬de ad be ef⟭

⟬d e a d b e e f⟭

⟬89 50 4e 47 0d 0a 1a 0a⟭

;; Long sequences can be put on their own lines.
⟬89 50 4e 47 0d 0a 1a 0a 00 00 00 0d 49 48 44 52 00 00 00 20 00 00
 00 19 08 02 00 00 00 df 6d bb c6 00 00 00 01 73 52 47 42 01 d9 c9
 2c 7f 00 00 00 04 67 41 4d 41 00 00 b1 8f 0b fc⟭
```

Blobs also store the [MIME media type](https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/MIME_types)
of their byte content. Unless explicitly specified, the media type is indeterminate.

There are two ways to specify the media type of a blob.

The first way is to write the media type explicitly using the double asterisk `⁑` syntax.

```wwml
⟬de ad be ef ⁑ application/octet-stream⟭
```

Or, for instance:

```wwml
⟬3c 70 3e 48 69 21 3c 2f 70 3e ⁑ text/html;charset=utf-8⟭
```

The segment coming after `⁑` must be a valid media type. Media types are
parsed according to RFC 1521.

The second way is to use `?`, which tells ML to guess the media type from the bytes.
This uses a component of Wirewright called PantoMIME, whose task is to do *MIME sniffing* --
to guess the MIME type from raw bytes. Its database is a lot *less* sophisticated than that
of e.g. libmagic, but it will detect the most common types of things just fine (TTF and OTF
fonts; major image formats such as PNG, JPEG; some plaintext formats, including JSON).
PantoMIME cannot "fail" in the usual sense of the word. If it doesn't have a guess as to
what the bytes are, it will say `application/octet-stream`, which means essentially
"opaque binary data".

```wwml
⟬de ad be ef ⁑ ?⟭ ;; The same as writing: ⟬de ad be ef ⁑ application/octet-stream⟭


⟬89 50 4e 47 0d 0a 1a 0a 00 00 00 0d 49 48 44 52 00 00 00 20 00 00
 00 19 08 02 00 00 00 df 6d bb c6 00 00 00 01 73 52 47 42 01 d9 c9
 2c 7f 00 00 00 04 67 41 4d 41 00 00 b1 8f 0b fc ⁑ ?⟭

;; The same as writing:
;; ⟬89 50 4e 47 0d 0a 1a 0a 00 00 00 0d 49 48 44 52 00 00 00 20 00 00
;;  00 19 08 02 00 00 00 df 6d bb c6 00 00 00 01 73 52 47 42 01 d9 c9
;;  2c 7f 00 00 00 04 67 41 4d 41 00 00 b1 8f 0b fc ⁑ image/png⟭
```

## Dictionary terms

The majority of WwML is focused on the representation of dictionary terms.

### General form

The general form for dictionaries is: `(item0 item1 ... itemN key0: value0 key1: value1 ... keyN: valueN)`.

- Zero or more items
- Zero or more key-value pairs
- Key-value pairs can be mixed with items.

It is called *general* because all dictionaries can be represented using this form.

For example, `(+ 1 2)`, `(/ 1 2 precision: 3)`, or:

```wwml
(text color: black
      width: 100
      height: 200
  "Hello World")
```

You are not advised to arbitrarily mix key-value pairs since this makes it very hard for a human
to read the dictionary. But you can do that: `(+ a: 100 qux b: 200)` is the same as the vastly more
readable `(+ qux a: 100 b: 200)`.

#### Curly brackets

You can use curly brackets `{}` to force key-value pairs only.

```wwml
;; Note the use of comma. It is treated like whitespace but may aid
;; readability sometimes.
{x: 100, y: 200}

;; INVALID: {1 2 x: 100, y: 200}
```

Note that this is not the same as forcing a pairsonly dictionary! In alignment with the nature of
Wirewright's dictionary terms, you can still create items even with curly brackets, like so:

```wwml
{0: +, 1: 100, 2: 200}

;; ... is **exactly** the same as writing:
(+ 100 200)

;; Or for instance:
{0: +, 1: 100, 2: 200, a: foo, b: bar}

;; ... is **exactly** the same as writing:
(+ 100 200 a: foo b: bar)
```

This demonstrates very well that WwML is indeed a system of shorthands.

### Document form

The same as the general form, except without surrounding parens `()`.

WwML files are usually read as document-form dicts (document dicts).

For example, the document dict `1 2 3` is the same as `(1 2 3)`.

#### Sections

Section syntax is allowed for document dicts using the `---` separator,
which must be positioned on its own line, immediately following the newline
character (plus optional prefix whitespace)

In effect, `---` lets you split one document dict into several smaller
document dicts. Section names are single-term headers, and all lines
until the next `---` are part of the current section’s body.

Using `---` turns the document dict into an "outline" mapping each section's
name to its body.

```wwml
a b c
---
d e f
```

This is the same as writing `{default: (a b c), aux: (d e f)}`

You can add multiple sections by naming them. Having two different sections with
the same name is a syntax error. Names are compared literally; no evaluation
or normalization is applied.

The section before the first `---` is always called `default`. You cannot change that.

If you do not use sections, the `default` section will not be created; its
body is going to be concatenated with the document dict.

Section names after `---` are optional, but then you would only be able to have
two sections: `default` and `aux` (short for *auxiliary*, the default name for
the second section).

```wwml
a b c

--- rewriter
d
e
f

--- help

"Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint \
 cillum sint consectetur cupidatat."
```

is the same as writing: `{default: (a b c), rewriter: (d e f), help: ("Lorem ipsum ...")}`

Note that you can use any term as a section name, even multiline dictionaries.
However, for readability, we advise using symbols. Use strings if you need
to include whitespace. Use short inline dicts if you need a more complex id.

```wwml
a b c

--- (article "1. Foo")
foo
bar
baz

--- (article "2. Qux")
qux
qyx
qoox
```

### General shorthands

### Key-value pair shorthands

- `:<term>` is the same as writing `<term>: <term>`. E.g. `:foo` is the same as writing `foo: foo`. **The absence of
  whitespace between colon and term is mandatory.**

### M1 (pattern matching)

#### Shorthands for `%let`

- `<name term>←<value term>` is the same as writing `(%let <name term> <value term>)`. **The absence of
  whitespace on both sides of the arrow is mandatory.**

#### Shorthands for `%keypool`

- `{% <term list>}` is the same as writing `(%keypool <term list>)`.

#### Shorthands for `%item` and `%item°`

- `⟨<term list>⟩` is the same as writing `(%item <term list>)`.
- `⟨<term list>⟩°` is the same as writing `(%item° <term list>)`.
- `⟨<term list> ¦ <pairspattern>⟩` is the same as writing `(%all (%item <term list>) <pairspattern>)`.
- `⟨<term list> ¦ <pairspattern>⟩°` is the same as writing `(%all (%item° <term list>) <pairspattern>)`.
- `⟨& x y z⟩` is the same as writing `(%all (%item x) (%item y) (%item z))`. Similarly to other forms,
  you can do `⟨& x y z⟩°` to use `%item°`: `(%all (%item° x) (%item° y) (%item° z))`. You can use interfixes
  as well: `⟨& x y z ¦ rest_⟩`, `⟨& x y z ⍊ a b⟩` etc. are the same as writing
  `(%all (%item x) (%item y) (%item z) (%partition _ rest_))` and `(%all (%item x) (%item y) (%item z) (%partition _ (%layer _ {a: a_, b: b_})))`,
  correspondingly.

#### Shorthands for `%leaf` and `%leaf°`

- `⟪x y z⟫` is the same as writing `(%all (%leaf x) (%leaf y) (%leaf z))`. Source `⟪x y z⟫°`
  and interfixes are supported as well (e.g. `⟪x y z ⍊ a b⟫`).

#### Shorthands for `%split` and `%split°`

The following defines a right-recursive `%split`: `⟨x y z … a b c⟩` and so on with
more *parts*, delimited by `…`. Similarly there exists the source version:
`⟨x y z … a b c⟩°`. Interfixes are supported as well: `⟨x y z … a b c ¦ pairs_⟩`.

Examples:
- `⟨a … b⟩` is the same as writing `(%split (a) (b))`.
- `⟨a … b … c⟩` is the same as writing `(%split (a) b (c))`.
- `⟨a … b c … d⟩` is the same as writing `(%split (a) b c (d))`.
- `⟨a … b c … d … e⟩` is the same as writing `(%split (a) b c (%split (d) (e)))`.
- `⟨a … b c … d … e … f⟩` is the same as writing `(%split (a) b c (%split (d) e (f)))`.

... and so on, rceursively. Parts designated for the middle (above that would be
`… b …`, `… b c …`, `… e …`) must be nonempty. Other parts (left and right parts)
may be empty.

#### Shorthands for itemspart `%partition`

- `[<term list>]` is the same as writing `(%partition (<term list>) _)`.

#### Shorthands for `%layer`

- `{¦ <selectors>}` is the same as writing `(%layer _ <selectors>)`.
- `{<term>¦ <selectors>}` is the same as writing `(%partition (%let <term> _) (%layer _ <selectors>))`.
- `{+¦ <term list>}` is the same as writing `(%layer _ {<each term from term list>: true})`
- `{-¦ <term list>}` is the same as writing `(%layer _ {<each term from term list>: false})`

#### Shorthands for `%past`

- `_?` is the same as writing `(%past _ min: 0 max: 1)`.

#### Selectors

*Selectors* are a group of syntactic shorthands for matching dictionary entries.

Selectors are generally written as key-value pairs: `<key>: <value>`; or expand
to key-value pairs. A variety of other shorthands is available.

##### Selectors in closed dict

Selectors can be used to write patterns for matching a *closed* dict, meaning
a dict that must not contain entries beyond those listed. The syntax is `{| ...}`.
For instance, `{| -x_ y_ z_: 10}` expands to `{x: (%- _ x), y: y_, z: z←10}`.

##### Selectors in open dict (`%layer`)

Selectors can be used in `%layer` shorthand patterns such as `{¦ ...}`, `{xyz¦ ...}`
and so on (see above).

##### Selectors in pairside

Selectors can be used after the `¦` interfix or the `⍊` interfix. Both divide
the surrounding expression to an "itemside" and a "pairside". For example,
`(+ a_ b_ ¦ <residue> <selectors here>)`, or `(+ a_ b_ ⍊ <selectors here>)`. `⍊`
sets the residue to `_` automatically.

A pairside can be empty, as in `(_* ¦)`. If interpreted as an M1 pattern, it would match
an itemsonly dictionary; this is because its expansion is `(%partition (_*) (%layer () ()))`.

Pairside optionally begins with a so-called *selection residue* term, which
corresponds to the first argument of `%layer`. If it is absent, it defaults to `()`.

- Pairside `_` is the same as writing `(%layer _ {})`
- Pairside `x_` is the same as writing `(%layer x_ {})`
- Pairside `_ x: 100 y: 200` is the same as writing `(%layer _ {x: 100, y: 200})`
- Pairside `() x: 100 y: 200` is the same as writing `(%layer () {x: 100, y: 200})`
- Pairside `x: 100 y: 200` is the same as writing `(%layer () {x: 100, y: 200})`
- Pairside `{% a b c} x: 100 y: 200` is the same as writing `(%layer (%keypool a b c) {x: 100, y: 200})`

The residue term is optional.

> [!WARNING]
> Since there is no way WwML can tell whether e.g. the pairside `x_` means `(%layer x_ {})`
> or `(%layer () {x: x_})` -- as in `(+ a_ b_ ¦ x_)` -- this is a case where you would
> have to write the residue `()` explicitly despite its stated optionality and reasonability
> of leaving it out. The ambiguity is otherwise resolved in favor of the first expansion.
> In other words, keep `x_` to get the first expansion; and use `() x_` to get
> the second expansion. Thus `(+ a_ b_ ¦ x_)` makes M1 capture the pairspart under `x`; and
> `(+ a_ b_ ¦ () x_)` makes M1 capture the value of key `x` under `x`; and ensure there are no other pairs.

##### M1 Key-value pair shorthands

- `-<name>` is the same as writing `<name>: (%- _)`.
- `-<name>_<type>` is the same as writing `<name>: (%- <type> <name>)`.
- `-<key>: <name>` is the same as writing `<key>: (%- _ <name>)`.
- `<name>⁺` is the same as writing `<name>: (%let <name> true)`
- `<name>⁻` is the same as writing `<name>: (%let <name> false)`
- `<name>_<type>` is the same as writing `<name>: <name>_<type>`.
- `<name>` is the same as writing `<name>: _`.
- `<name>_: (%optional <default> <value>)` is the same as writing `<name>: (%optional <default> (%let <name> <value>))`.
- `<name>_<type>: <value>` is the same as writing `<name>: (%let <name> (%all _<type> <value>))`.
- `<name>_: <value>` is the same as writing `<name>: (%let <name> <value>)`.
- `<name>⋮ <value>` is the same as writing `<name>: (%optional <value> <name>_<type of value>)` or
  `<name>: (%optional <value> (%let <name> <type of value>))` if *name* is not a symbol.
- `<name>_⋮ <value>` is the same as writing `<name>: (%optional <value> <name>_)`.
- `<name>_<type>⋮ <value>` is the same as writing `<name>: (%optional <initial value of type> (%let <name> (%all _<type> <value>)))`.
- `⋮<name>` is the same as writing `<name>: (%- (%never) <name>)`.
- `±<name>` is the same as writing `<name>: (%let <name> _number)`.
- `@<name>_` is the same as writing `<name>: @<name>_`.

###### Initial values

| Type      | Initial value (shown as WwML) |
| --------- | ----------------------------- |
| `boolean` | `false`                       |
| `dict`    | `()`                          |
| `number`  | `0`                           |
| `string`  | `""`                          |
| `symbol`  | `unset`                       |

#### Misc

**Absence of whitespace between prefix and term is mandatory**.

- `` `<term> `` is the same as writing `(%slot <term>)`.
- `≡<term>` is the same as writing `(%nonself <term>)`.
- `%'<term>` is the same as writing `(%literal <term>)`.
- `±<term>` is the same as writing `(%let <term> _number)`.
- `⁰x` is the same as writing `(x _*)`, `¹x` is `(_ x _*)`, `²x` is `(_ _ x _*)` and so on.
- `⁰⁻⁰x` is the same as writing `((x _*) _*)`, `¹⁻⁰x` is `(_ (x _*) _*)`, `¹⁻²x` is
  `(_ (_ _ x _*) _*)` and so on. Nesting can proceed indefinitely. For example, `⁰⁻¹⁻²x`
  is the same as writing `((_ (_ _ x _*) _*) _*)`.

### Dictionary set and multiset

- `{+ x y z}` is the same as writing `{x: true, y: true, z: true}`. Elements are arbitrary terms.
- `{- x y z}` is the same as writing `{x: false, y: false, z: false}`. Elements are arbitrary terms.
- `{# a a b c}` is the same as writing `{a: 2, b: 1, c: 1}`. Elements are arbitrary terms.
- `{# 100×a 5×b c}` is the same as writing `{a: 100, b: 5, c: 1}`. Elements are arbitrary terms. **Absence
  of whitespace between the amount, `×`, and the term is mandatory.**

### Alloy

- `^<term>` is the same as writing `(^ <term>)`.
- `^*<term>` is the same as writing `(^* <term>)`.
- `(<term list> ^… <arg>)` is the same as writing `(^extend (<term list>) <arg>)`.
- `^:<term>` short pair syntax is the same as writing `<term>: ^<term>`. For example,
  `{^:x ^:y ^:z}` is the same as writing `{x: ^x, y: ^y, z: ^z}`. Do not confuse with
  `:^x`, which expands to `^x: ^x`.

### Rulesets

- `<left term> => <right term>` is the same as writing `(rule <left term> <right term>)`.
- `<left term> <> <right term>` is the same as writing `(backmap <left term> <right term>)`.

#### Rule or backmap-local blank generation

The character `◇` is replaced with a symbol (`◇`) or blank (`◇_`) generated
specifically for the current rule/backmap at read-time.

```wwml
(foo ¦ _ v_number -◇_) <> {v: $'(+ →v 1), ◇: true}
(foo ¦ _ v_number -◇_) <> {v: $'(* →v 2), ◇: true}
(foo ¦ _ v_number -◇_) <> {v: $'(/ →v 3), ◇: true}
```

Treat `◇` as a "did I fire on this term" mark that the rule places on the underlying
term to not fire again (perhaps preventing infinite rewriting, which all rules in
the example above will display unless they prevent themselves from firing in some way,
in this case with `◇`).

#### Block-level blank generation

Similarly to `◇`, `▢` is replaced with a symbol (`▢`) or blank (`▢_`)
generated specifically for the current *block*.

```wwml
;; One of (their specificity is the same so any one of them can fire,
;; depending on rule lookup implementation):
(foo ¦ _ v_number -▢_) <> {v: $'(+ →v 1), ▢: true}
(foo ¦ _ v_number -▢_) <> {v: $'(* →v 2), ▢: true}
(foo ¦ _ v_number -▢_) <> {v: $'(/ →v 3), ▢: true}
```

#### Manually delimiting blocks

Manually delimiting blocks is most often useful when you're using lexical choice.
Lexical choice always instantiates within the same block. For example, the following:

```wwml
(⸨foo,bar⸩ -▢_) <> {▢: true}

(⸨alpha,beta⸩ -▢_) <> {▢: true}
```

... expands to:

```wwml
(foo -▢_) <> {▢: true}
(bar -▢_) <> {▢: true}

(alpha -▢_) <> {▢: true}
(beta -▢_) <> {▢: true}
```

... which then turns into something like:

```wwml
(backmap (foo (- BeCVbGdsfdeXca_)) {BeCVbGdsfdeXca: true})
(backmap (bar (- BeCVbGdsfdeXca_)) {BeCVbGdsfdeXca: true})
(backmap (alpha (- dgeMTbmQUcBbJ_)) {dgeMTbmQUcBbJ: true})
(backmap (beta (- dgeMTbmQUcBbJ_)) {dgeMTbmQUcBbJ: true})
```

Notice how `▢` is the same for `foo` and `bar`, and for `alpha` and `beta`.

This may not always be desired. That is, you may want to have each instance of
lexical choice be in its own block. This is especially useful when you use both 0-
and 1-delayed choice to generate rules in the block.

You can use `∥` brackets to delimit blocks.

```wwml
∥ (⸨foo,bar⸩ -▢_) <> {▢: true}
∥ (⸨alpha,beta⸩ -▢_) <> {▢: true}
```

This expands to the following:

```wwml
∥ (foo -▢_) <> {▢: true}
∥ (bar -▢_) <> {▢: true}

∥ (alpha -▢_) <> {▢: true}
∥ (beta -▢_) <> {▢: true}
```

Notice how `∥` is copied along with each instance. This turns to something like:

```wwml
(backmap (foo (- HeWdpdtcfbkbpdL_)) {HeWdpdtcfbkbpdL: true})
(backmap (bar (- fbecdgcZFeqce_)) {fbecdgcZFeqce: true})
(backmap (alpha (- fpbtRcRjcbCdw_)) {fpbtRcRjcbCdw: true})
(backmap (beta (- dpeZdqdcLdmbobt_)) {dpeZdqdcLdmbobt: true})
```

All `▢`s expanded into different block ids, meaning each backmap was its own block,
just as expected with `∥`.

You can also use `∥` to manually delimit blocks in general, for example, in situations
where newlines look ugly. Consider, for instance, the `rewriter` node in Rack:

```wwml
(rewriter (@in -> ascR -> @out)
  (⸨a,b⸩ -◇_) <> {◇: true}
  (⸨c,d⸩ -◇_) <> {◇: true})
```

Currently this will instantiate the entire rewriter like so:

```wwml
(rewriter (@in -> ascR -> @out)
  (a -◇_) <> {◇: true}
  (c -◇_) <> {◇: true})
(rewriter (@in -> ascR -> @out)
  (b -◇_) <> {◇: true}
  (d -◇_) <> {◇: true})
```

This is definitely not what we want. We can add whitespace:

```wwml
(rewriter (@in -> ascR -> @out)

  (⸨a,b⸩ -◇_) <> {◇: true}

  (⸨c,d⸩ -◇_) <> {◇: true}

)
```

But this looks very ugly. Sometimes it is acceptable though, as in:


```wwml
(rewriter (@in -> ascR -> @out)
  ;; Doc string 1
  (⸨a,b⸩ -◇_) <> {◇: true}

  ;; Doc string 2
  (⸨c,d⸩ -◇_) <> {◇: true}

  ;; Some non-templated rule that also happens to prevent `rewriter`s closing
  ;; paren from being duplicated during lexical choice instantiation.
  (foo) => (bar))
```

Here, other rules and doc comments "cushion" the newlines, making them look more
natural. In the original case, though, you can use `∥` like so:

```wwml
(rewriter (@in -> ascR -> @out)
  ∥(⸨a,b⸩ -◇_) <> {◇: true}∥
  ∥(⸨c,d⸩ -◇_) <> {◇: true}∥)
```

... which expands to something like:

```wwml
((rewriter
  (@in -> ascR -> @out)
  (backmap (a (- bqbXcmeaehYcNez_)) {bqbXcmeaehYcNez: true})
  (backmap (b (- eYbFtdXcwcudief_)) {eYbFtdXcwcudief: true})
  (backmap (c (- bfdQcdgfdevcjdp_)) {bfdQcdgfdevcjdp: true})
  (backmap (d (- cVdNGdWfnAcAT_)) {cVdNGdWfnAcAT: true})))
```

Notice that we're delimiting with `∥` both before and after each rule here. While
this is not strictly necessary for the rules before the last one, for the last one
it is. Otherwise, we'd be copying `rewriter`'s closing paren multiple times, which
is a syntax error:

```wwml
(rewriter (@in -> ascR -> @out)
  ∥(⸨a,b⸩ -◇_) <> {◇: true}
  ∥(⸨c,d⸩ -◇_) <> {◇: true})
```

... expands to:

```wwml
(rewriter (@in -> ascR -> @out)
  ∥(a -◇_) <> {◇: true}
  ∥(b -◇_) <> {◇: true}
  ∥(c -◇_) <> {◇: true}) ;; < OOPS, extra paren copied!
  ∥(d -◇_) <> {◇: true}) ;; <
```

Remember we're in lexical templating land. There is no syntax yet, so lexical
choices can make syntax errors if used carelessly.

Which is exactly what we want here.

#### Minor notes

- `◇` will be replaced by the current rule's id, which is a base-48 rendition
  of that rule's 64-bit hash. Collisions are possible but extremely unlikely
  with a 64-bit hash, assumming you're not creating millions if not billions
  of rules. A rule with exactly the same LHS and RHS as the original one will
  produce the same rule id, of course, but that's expected, and, in fact, intended
  in some way.
- `▢` will use the current rule block's hash in a similar manner.
- Whereas `◇` or `▢` is going to be replaced with something like `84GieP0DeGk`,
  a symbol; `◇_` or `▢_` are going to be replaced by a blank: `84GieP0DeGk_`.
- `◇_` and `▢_` are lexemes distinct from `◇` and `▢`.
- `◇/▢[_]` are valid where terms are valid. *They do not operate at the lexical level*.
  Writing something like `◇-foo` **is wrong**, and will confusingly enough result in `◇ -foo`.

### Backmaps and rewriter circuits

> [!WARNING]
> These shorthands are DEPRECATED. They are not used by anything anymore and
> are scheduled for removal.

- `→<term>` is the same as writing `($my <term>)`
- `↑<term>` is the same as writing `($up <term>)`
- `↓<term>` is the same as writing `($down <term>)`
- `$<term>` is the same as writing `($ <term>)`
- `$'<term>` is the same as writing `($once <term>)`

### Delta7

- `@<term>` is the same as writing `(edge <term>)`.
- `@:<term>` is the same as writing `<term>: @<term>`. For example, `{@:x @:y @:z}`
  is the same as writing `{x: @x, y: @y, z: @z}`. Do not confuse with `:@x`, which
  expands to `@x: @x`.

### Nitrene

- `'<term>` is the same as writing `(literal <term>)`.

## Precedence

Most WwML operators are prefixes.

Prefixes bind tighter than infixes. `≡x => a` is `(≡x) => a`, where parens indicate associativity.

However, there are currently three infixes in WwML -- their expansions were detailed above.

The following is a precedence table/diagram for WwML infix operators.

```text
<highest precedence>

←       ;; RIGHT ASSOCIATIVE. E.g. x←a←b means x←(a←b) where parens indicate assoc.

<>  =>  ;; RIGHT ASSOCIATIVE. E.g. x => y => z means x => (y => z) where parens indicate assoc.

<lowest precedence>
```

Notably, `a←b => c` or `a←b <> c` means `(a←b) => c` or `(a←b) <> c`, correspondingly,
where parens indicate associativity.

The default associativity of these operators is *extremely* unlikely to cause
trouble. They are rarely seen in chains; let-chains are used sometimes (e.g. in backmaps,
to have two or more handles of something); but the default associativity is exactly
what is needed in that case.

WwML a way to indicate grouping: `⸤` and `⸥`. For example, whereas `a←b←c` produces `a←(b←c)`
(parens indicate assoc), `⸤a←b⸥←c` produces `(a←b)←c`.
