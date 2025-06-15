# WwML

FIXME: there are some inconsistencies with the actual grammar here and there.

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
a "basis language" -- S-expressions extended with key-value pairs.

WwML is a rather complicated *constructor* or *(de)serialization* engine for the five
kinds of terms: dictionary, number, string, symbol, or boolean terms. No magic, no AST,
no nothing -- terms *are* the AST. Any dictionary representation can be rewritten in terms
of another; and any shorthand lowered to its expansion without leaving the representation.
This is because they are ultimately still describing the same data structure: a dictionary.

Interestingly, WwML can also be thought of as a "compressed representation" of terms, since
a term's text representation is usually much more efficient in terms of its byte size than
the corresponding in-memory term. Something like `(+ 1 2)` takes only a few bytes in ASCII
but may very well take hundreds of bytes in-memory with all the indexing and control structure
overhead (I am working actively on reducing the memory footprint of dictionaries specifically,
but there are limits). Compactness is traded off for the general inefficiency of parsing.

How could one build algorithms that use characters directly as their *only* memory/state,
without such constructions exploding with algorithmic and/or operational complexity. I suppose
it is an unsolved problem and an interesting avenue for exploration, very much in line
with Wirewright. But let's leave that aside for now :^)

NOTE: "expands to" below refers to expansion at *parse-time*.

NOTE: character sets are expressed in [Crystal character set notation](https://crystal-lang.org/api/1.16.2/Char.html#in_set%3F%28%2Asets%3AString%29%3ABool-instance-method).

## What you will need to be able to type

WwML uses a number of Unicode characters. You will have to configure your OS to be
able to type them (e.g. using the Compose key).


| Character | Unicode Codepoint | Combo                                              |
| --------- | ----------------- | -------------------------------------------------- |
| `°`       | U+00B0            | <kbd>Compose</kbd> + <kbd>o</kbd> + <kbd>o</kbd>   |
| `∈`       | U+2208            | <kbd>Compose</kbd> + <kbd>m</kbd> + <kbd>m</kbd>   |
| `⊆`       | U+2286            | <kbd>Compose</kbd> + <kbd>m</kbd> + <kbd>s</kbd>   |
| `⊂`       | U+2282            | <kbd>Compose</kbd> + <kbd>m</kbd> + <kbd>S</kbd>   |
| `⸢`       | U+2E22            | <kbd>Compose</kbd> + <kbd>[</kbd> + <kbd>[</kbd>   |
| `⸣`       | U+2E23            | <kbd>Compose</kbd> + <kbd>]</kbd> + <kbd>]</kbd>   |
| `⟨`       | U+27E8            | <kbd>Compose</kbd> + <kbd>{</kbd> + <kbd>{</kbd>   |
| `⟩`       | U+27E9            | <kbd>Compose</kbd> + <kbd>}</kbd> + <kbd>}</kbd>   |
| `¦`       | U+00A6            | <kbd>Compose</kbd> + <kbd>|</kbd> + <kbd>|</kbd> |
| `…`       | U+2026            | <kbd>Compose</kbd> + <kbd>.</kbd> + <kbd>.</kbd>   |
| `⁺`       | U+207A            | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>+</kbd>   |
| `⁻`       | U+207B            | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>-</kbd>   |
| `⋮`       | U+22EE            | <kbd>Compose</kbd> + <kbd>:</kbd> + <kbd>:</kbd>   |
| `×`       | U+00D7            | <kbd>Compose</kbd> + <kbd>x</kbd> + <kbd>x</kbd>   |
| `≡`       | U+2261            | <kbd>Compose</kbd> + <kbd>=</kbd> + <kbd>=</kbd>   |
| `←`       | U+2190            | <kbd>Compose</kbd> + <kbd><</kbd> + <kbd>-</kbd>  |
| `↑`       | U+2191            | <kbd>Compose</kbd> + <kbd>^</kbd> + <kbd>|</kbd>  |
| `↓`       | U+2193            | <kbd>Compose</kbd> + <kbd>v</kbd> + <kbd>|</kbd>  |
| `→`       | U+2192            | <kbd>Compose</kbd> + <kbd>-</kbd> + <kbd>></kbd>   |
| `₀`..`₉`  | U+2080-U+2089     | <kbd>Compose</kbd> + <kbd>_</kbd> + <kbd>0</kbd>-<kbd>9</kbd>   |

## Comma (+)

The comma character is treated as whitespace and can be used where whitespace
can be used. E.g. `x,y` is the same as `x y`.

## Comments (+)

### Inline comments (lexical comments) (+)

Inline comments start with `;;` and extend to the end of the line or EOF.

```wwml
;; Lorem ipsum dolor sit amet

(+ 1  ;; qui minim labore
   2) ;; adipisicing minim sint
```

### Term comments (syntactic comments) (+)

A single term can be commented out using the `;` prefix.

> [!NOTE]
> The commented-out term **must** be a syntactically valid term.

Term comments are very useful for experimentation, e.g. to disable test cases
or arguments quickly.

```wwml
;(+ 1 2)
;; Same as writing nothing.

(+ ;1 2)
;; Same as `(+ 2)`

(+ 1 ;2)
;; Same as `(+ 1)`
```

## Boolean terms (+)

```wwml
true  ;; boolean true
false ;; boolean false
```

## Symbol terms (+)

Symbol terms are represented by combinations of one or more characters from
the set: `a-zA-Z0-9_!$%&*+\-./#<=>?@~λ|∞°∈⊆⊂\`. Symbol terms that contain
characters not in this set cannot be represented by WwML. Using such characters
outside of strings is a syntax error.

NOTE: symbols **cannot** start with a digit `0-9`.

```wwml
abc
foo42
divide-by-zero?
+
<=
⊆
λx
∞/2
%item°
```

## Number terms (+)

Wirewright number terms are represented using rational numbers.

### Basic numbers (+)

```wwml
0
42
12345
```

### Sign (+)

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

#### Radix notation (+)

Base (also referred to as radix to avoid confusion with the word base in general, which
is useful in other contexts) can be given by a sequence of one or two postfix subscript
digits. Minimum allowed radix is 1. Maximum allowed radix is 62.

Characters from the following set are used **inorder, as the radix increases**: `0-9A-Za-z`.

Radices less than or equal to 36 are case-insensitive.

```wwml
1011₂ ;; base-2 (binary)
755₈  ;; base-8 (octal)
1F₁₆  ;; base-16 (hexadecimal)
Z3₃₆  ;; base-36
Zz₆₂  ;; base-62
```

**Retention**: WwML is not a calculator, it is a notation. It *does not* simplify
radix notation to decimal. For example, `1f₁₆` expands to `(digits 1 15 radix: 16)`. It is up
to the client to decide what to do with this term. Some clients ("runtimes") may want to
e.g. support radix-aware addition or otherwise manipulate numbers with explicit base. WwML
will not interfere by reducing everything to decimal like traditional languages do.

### Decimal form for rationals (+)

```wwml
;; INVALID: 3.
;; INVALID: .3
3.0
0.3
0.0
1.234
```

Use the fractional form to write non-terminating fractions such as `0.333...`.

### Fractional form for rationals (+)

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

### Scientific notation (+)

```wwml
1e6          ;; scientific notation (1 million)
-2.5e-3      ;; scientific notation (-0.0025)
+4.0e+2       ;; scientific notation (400)
```

**Retention**: Scientific notation is *retained* similarly to radix notation, represented
as `(sci mantissa_ exponent_)`. For instance, writing `1e6` results in `(sci 1 6)`,
and `-2.5E-3` results in `(sci -2.5 -3)`.

### Separating digit blocks with `_` (+)

Non-leading and non-trailing underscore is ignored in all number literals.
It can be used to help separate digit blocks.

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

## String terms

### Standard form (+)

Escape sequences are initiated by `\`.

```wwml
"Lorem ipsum dolor sit amet."
```

### Multiline form

TODO

### Escape sequences (+)

- `\"` is the same as `\x22` (`"`)
- `\\` is the same as `\x5C` (`\`)
- `\n` is the same as `\x0A` (newline character)
- `\t` is the same as `\x09` (tab)
- `\r` is the same as `\x0D` (carriage return)
- `\⸢` is the same as `\u{2E22}` (top left half bracket).
- `\x__` inserts a byte given exactly two hex digits. Anything past the two digits is part of the string.
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

\u[LATIN SMALL leTter R with CEDILLA]
;; ŗ
```

### Interpolation (+)

Interpolation uses the characters `⸢` and `⸣`. You can escape `⸢` with `\⸢` or one of
the `\u` escape sequences if necessary.

Interpolation is recursive: what goes inside is an entire *term*, which must be syntactically valid.
The term could be a dictionary, a string with more interpolations, etc.

Interpolation is syntactic sugar for dictionaries of the form `(~ s1 s2 ... sn)`.

```wwml
"Your name is: ⸢name⸣. Have a nice day!"
;; Expands to: (~ "Your name is: " name ". Have a nice day!")

"Your name is: ⸢first-name⸣ ⸢last-name⸣. Have a nice day!"
;; Expands to: (~ "Your name is: " first-name " " last-name ". Have a nice day!")

"1 + 1 = ⸢(+ 1 1)⸣"
;; Expands to: (~ "1 + 1 =" (+ 1 1))
```

> [!NOTE]
> When you use interpolation WwML will be conservative in terms of the amount
> of work it *itself* will do. Speaking less cryptically, it will *not* concatenate
> your escape sequences and Unicode insertions et cetera to the string; but rather,
> will leave it to the client which will interpret `~` anyway. For example, if you
> write `"\u[alpha] + 5 = ⸢(+ alpha 5)⸣"`, WwML will parse this as `(~ "α" " + 5 =" (+ alpha 5))`.
> Note how it did not concatenate `"α"` and `" + 5 ="`.

## Dictionary terms

The majority of WwML is focused on the representation of dictionary terms.

### General form (+)

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

#### Curly brackets (+)

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
(+ 1 2)

;; Or for instance:
{0: +, 1: 100, 2: 200, a: foo, b: bar}

;; ... is **exactly** the same as writing:
(+ 1 2 a: foo b: bar)
```

This demonstrates very well that WwML is indeed a system of shorthands.

### Document form (+)

The same as the general form, except without surrounding parens `()`.

WwML files are usually read as document-form dicts (document dicts).

For example, the document dict `1 2 3` is the same as `(1 2 3)`.

#### Sections (+)

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

This expands to `{default: (a b c), aux: (d e f)}`

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

"""
Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint
cillum sint consectetur cupidatat.
"""
```

Expands to: `{default: (a b c), rewriter: (d e f), help: ("Lorem ipsum ...")}`

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

### Key-value pair shorthands (+)

- `:<term>` expands to `<term>: <term>`. E.g. `:foo` expands to `foo: foo`. **The absence of
  whitespace between colon and term is mandatory.**

### M1 (pattern matching) (+)

#### Shorthands for `%let` (+)

- `<name term>←<value term>` expands to `(%let <name term> <value term>)`. **The absence of
  whitespace on both sides of the arrow is mandatory.**

#### Shorthands for `%keypool` (+)

- `{% <term list>}` expands to `(%keypool <term list>)`.

#### Shorthands for `%item` and `%item°` (+)

- `⟨<term list>⟩` expands to `(%item <term list>)`.
- `⟨<term list>⟩°` expands to `(%item° <term list>)`.
- `⟨<term list> ¦ <pairspattern>⟩` expands to `(%all (%item <term list>) <pairspattern>)`.
- `⟨<term list> ¦ <pairspattern>⟩°` expands to `(%all (%item° <term list>) <pairspattern>)`.

#### Shorthands for `%split` and `%split°` (+)

- `⟨<left term list> … <right term list>⟩` expands to `(%split _ <first left term> (%all (<rest of left terms> _*) (%split _ <first right term> (<rest of right terms> _*))))`.
- `⟨<> … <>⟩°` uses `%split°` instead of `%split`.
- `⟨<> … <> ¦ <pairspattern>⟩` expands to `(%all (%split ...) <pairspattern>)`.
- `⟨<> … <> ¦ <pairspattern>⟩°` expands to `(%all (%split° ...) <pairspattern>)`.

#### Shorthands for itemspart `%partition` (+)

- `[<term list>]` expands to `(%partition (<term list>) _)`.

#### Shorthands for pairspart `%partition` (+)

- `{¦ <pairspattern>}` expands to `<pairspattern>`.
- `{…<term>¦ <pairspattern>}` expands to `(%partition (%let <term> _) <pairspattern>)`.
   **Absence of whitespace between … and the term is mandatory.**
- `{+¦ <term list>}` expands to `(%layer _ {<each term from term list>: true})`
- `{-¦ <term list>}` expands to `(%layer _ {<each term from term list>: false})`

#### Pairspattern (+)

*Pairspatterns* are a group of syntactic shorthands for matching dictionary pairspart.
Pairspatterns are associated with the character `¦`, called the "pairspart pipe" in
WwML and Wirewright-related contexts. If you see the pairspart pipe, then the shorthand
you're looking at has something to do with the pairspart.

Pairspatterns expand to `%layer` in general.

A dictionary with existing pairs cannot contain a pairspattern. In other words, either a dictionary
has pairs; or the pairspattern does.

A pairspart can be empty, as in `(_* ¦)`. If interpreted as an M1 pattern, it would match
an itemsonly dictionary; this is because its expansion is `(%partition (_*) (%layer () ()))`.

Pairspatterns optionally begin with a so-called *pairspattern residue* term, which
corresponds to the first argument of `%layer`. If it is absent, it defaults to `()`.

- `_` expands to `(%layer _ {})`
- `x_` expands to `(%layer x_ {})`
- `_ x: 100 y: 200` expands to `(%layer _ {x: 100, y: 200})`
- `() x: 100 y: 200` expands to `(%layer () {x: 100, y: 200})`
- `x: 100 y: 200` expands to `(%layer () {x: 100, y: 200})`
- `{% a b c} x: 100 y: 200` expands to `(%layer (%keypool a b c) {x: 100, y: 200})`

The residue term is optional.

> [!WARNING]
> Since there is no way WwML can tell whether e.g. the pairspattern `x_` means `(%layer x_ {})`
> or `(%layer () {x: x_})` -- as in `(+ a_ b_ ¦ x_)` -- this is a case where you would
> have to write the residue `()` explicitly despite its stated optionality and reasonability
> of leaving it out. The ambiguity is resolved automatically in favor of the first expansion.
> In other words, keep `x_` to get the first expansion; and use `() x_` to get
> the second expansion. Thus `(+ a_ b_ ¦ x_)` in M1 will capture the pairspart under `x`; and
> `(+ a_ b_ ¦ () x_)` will capture the value of `x` under `x`; and ensure the residual
> pairspart is empty.

The pairspattern generally consists of key value pairs: `<key>: <value>`. A variety
of other shorthands is available.

##### Key-value pair shorthands (+)

- `-<name>` expands to `<name>: (%- _)`.
- `-<name>_<type>` expands to `<name>: (%- <type> <name>)`.
- `-<key>: <name>` expands to `<key>: (%- _ <name>)`.
- `<name>⁺` expands to `<name>: (%let <name> true)`
- `<name>⁻` expands to `<name>: (%let <name> false)`
- `<name>_<type>` expands to `<name>: <name>_<type>`.
- `<name>` expands to `<name>: _`.
- `<name>_: (%optional <default> <value>)` expands to `<name>: (%optional <default> (%let <name> <value>))`.
- `<name>_<type>: <value>` expands to `<name>: (%let <name> (%all _<type> <value>))`.
- `<name>_: <value>` expands to `<name>: (%let <name> <value>)`.
- `<name>⋮ <value>` expands to `<name>: (%optional <value> <name>_<type of value>)`.
- `<name>_<type>⋮ <value>` expands to `<name>: (%optional <value> <name>_<type>)`.
- `⋮<name>` expands to `<name>: (%- (%never) <name>)`.

#### Misc (+)

**Absence of whitespace between prefix and term is mandatory**.

- `<BACKTICK><term>` expands to `(%slot <term>)`.
- `≡<term>` expands to `(%nonself <term>)`.
- `%'<term>` expands to `(%literal <term>)`.

### Dictionary set and multiset (+)

- `{+ x y z}` expands to `{x: true, y: true, z: true}`. Elements are arbitrary terms.
- `{- x y z}` expands to `{x: false, y: false, z: false}`. Elements are arbitrary terms.
- `{# a a b c}` expands to `{a: 2, b: 1, c: 1}`. Elements are arbitrary terms.
- `{# 100×a 5×b c}` expands to `{a: 100, b: 5, c: 1}`. Elements are arbitrary terms. **Absence
  of whitespace between the amount, `×`, and the term is necessary.**

### Alloy (+)

- `^<term>` expands to `(^ <term>)`.
- `(<term list> ^… <arg>)` expands to `(^extend (<term list>) <arg>)`.

### Rulesets (+)

- `<left term> => <right term>` expands to `(rule <left term> <right term>)`.
- `<left term> <> <right term>` expands to `(backmap <left term> <right term>)`.

### Backmaps and rewrite circuits (+)

- `→<term>` expands to `($my <term>)`
- `↑<term>` expands to `($up <term>)`
- `↓<term>` expands to `($down <term>)`
- `$<term>` expands to `($ <term>)`
- `$'<term>` expands to `($once <term>)`

### Nitrene (+)

- `'<term>` expands to `(leaf <term>)`

### Delta7 (+)

- `@<term>` expands to `(edge <term>)`

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
trouble. They are rarely seen in chains; let-chains are used sometimes, but the default
associativity is exactly what is needed in that case. Therefore, WwML does not provide a way
to change the default associativity (e.g. parentheses). Instead, if you for some reason need
to change associativity, remember you can always lower to an operator's expanded form.

