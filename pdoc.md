## Matching anything

If you need to match any term whatsoever, you should use the pass operator. The pass operator
is invoked whenever you use a *nameless AND typeless blank*: `_`. The pass operator is
also implicit in named but typeless blanks: `x_` is understood as `x←_` which is a shorthand
for `(%let x _)`, which means "capture anything and call that x".

A pretty common occurrence is the use of `_` to represent "everything else" in rules:

```wwml
(square a_number) => (ok (* a a))
(square _) => (err "Oops. Cannot square it")

(square 4)     ;; => (ok 16)
(square "Qux") ;; => (err "Oops. Cannot square it")
```

## Matching a term exactly

If you need to match a specific term and no other term, you can use the `%literal` operator.

Number literals such as `100`, string literals (e.g. `"hello world"`), boolean literals
(`true`, `false`), and symbol literals (e.g. `qux`) are already shorthands for `(%literal 100)`,
`(%literal "hello world")`, `(%literal true)`, and so on, respectively; you are not required to
surround them by `%literal` explicitly.

```wwml
(xor false false) => false
(xor false true) => true
(xor true false) => true
(xor true true) => false
```

Dictionary literals that consist *only* of the aforementioned literals or of other dictionary
literals, and contain no `%`-symbols, are inferred to be `%literal`s. You do not need to surround
by `%literal` e.g. the dictionaries `(+ 1 2)`, `(person name: "John Doe" age: 42)`, or `(% 1 2)`.
However, you will have to use `%literal` for `(%qux 100)`, `(+ a_ b_)`, etc.

`%literal`s (shorthand or full-form) are often used to "hard-code" values in rules, possibly
as base cases for recursion or other kinds of rule interaction:

```wwml
(salary (person name: "John Doe" role: "Leader")) => "$1,000,000"
(salary _) => "$1,000"
```

If you want to be absolutely sure that a dictionary is given literal treatment (such as when you
are not aware of what the dictionary will contain ahead-of-time), then use `%literal`. Here in
`(match-lit (%new x_) x_)`, we do not know what the user will pass ahead-of-time and whether `x_`
will contain a pattern or not. So if we wanted to guarantee literal treatment, we would use
`(match-lit (%new (%literal x_)) x_)`. Note that this pattern is just an example (and a pretty
meaningless one since it is essentially `(match-lit x_ x_)`). Maybe you are creating patterns
on the fly, from some kind of template; then `%literal` could be used as a kind of "interpolation barrier".

There is a shorthand syntax for `%literal`: `%'`. For example, `%'(+ x_ y_)` is the same as `(%literal (+ x_ y_))`.

### Matching several alternative terms

If you need to match exactly one term from a set of alternatives, you can use the `%any` operator.

```wwml
(available? (%any Monday Wednesday Friday)) => true
(available? _) => false

(available? Monday) ;; => true
(available? Friday) ;; => true
(available? Sunday) ;; => false
```

## Matching numbers

You can match numbers in several different ways, with more or less discretion.

### Matching any number

If you only care whether something is a number or not, use `_number`; `_number` is a type check
and nothing more. Similarly, `x_number` is understood as `x←_number` and so on. `_number` here
means "any number".

An alternative and a "gateway" to more sophistication is `(%number _)`. It also matches any
number whatsoever. In fact, `_number` is a shorthand for `(%number _)`.

### `%number`

`%number` lets you match numbers more precisely. The underscore `_` stands for "it", the term
under question, presumably a number.

`(whole _)` means that "it" -- the number -- must be whole. So if you only want to match whole
numbers and nothing else, use `(%number (whole _))`.

```wwml
(square a←(%number (whole _))) => (ok (* a a))
(square _number) => (err "Bad at math, sorry :(")
(square _) => (err "Oops. Cannot square it")

(square 4)   ;; => (ok 16)
(square 1/3) ;; => (err "Bad at math, sorry :(")
```

To check whether a number is within a certain range there are two general ways.

The first one is useful if you need to check whether a number is below (`_ < T`, `_ <= T`) or above
(`_ > T`, `_ >= T`) a certain threshold T: `(%number _ < 100)`, `(%number _ > 1/3)`. The following
pattern will match all natural numbers: `(%number (whole _) >= 0)`.

The second one is useful if the number is bounded on both sides. In such cases you can
use one of `B < _ < E` (both ends excluded), `B <= _ < E` (end excluded), `B < _ <= E`
(begin excluded), or `B <= _ <= E` (both ends included). The following pattern will match
all 8-bit integers: `(%number 0 <= (whole _) <= 255)`.

```wwml
(category (%number 0 <= (whole _) <= 12)) => (ok "child")
(category (%number 13 <= (whole _) <= 19)) => (ok "teenager")
(category (%number 20 <= (whole _) <= 64)) => (ok "adult")
(category (%number (whole _) >= 65)) => (ok "senior")
(category _) => (err "invalid age")

(category 10)  ;; => (ok "child")
(category 25)  ;; => (ok "adult")
(category 13)  ;; => (ok "teenager")
(category 75)  ;; => (ok "senior")
(category -10) ;; => (err "invalid age")
(category 1/3) ;; => (err "invalid age")
;; etc...
```

Speaking of 8-bit integers, there is also a series of shorthands for matching the frequently
appearing fixed-width numeric types. The previous 8-bit number pattern is the expanded form
of `(%number u8)`. There is also `(%number u16)`, u32, u64, and u128 for unsigned; and similarly
`(%number i8)`, i16, i32, i64, and i128 for signed fixed-width types. If for some reason you only
want to match the positive or negative ranges of the signed types, you can prepend a sign `+` or
`-` to the type: `(%number -i8)` will only match the negative end and `(%number +i8)` will match
the positive end **and zero** of the range for the signed type `i8`. Similarly for i16, i32, and
so on:

```wwml
(rgb? ((%number u8) (%number u8) (%number u8))) => true
(rgb? _) => false

(rgb? (0x33 0xfa 0xfa)) => ;; true
(rgb? (1000 0xfa 0xfa)) => ;; false
;; etc...

(side (%number -i8)) => left
(side (%number +i8)) => right

(side -100)  ;; => left
(side 0)     ;; => right
(side 10)    ;; => right
```

## Matching symbols

Symbols specifically can be matched by `_symbol` or similarly `x_symbol` (understood as `x←_symbol`
and so on). `_symbol` here stands for "any symbol".

```wwml
(lamp on)      ;; => (ok "turned the lamp on")
(lamp off)     ;; => (ok "turned the lamp off")
(lamp _symbol) ;; => (err "what?")
```

### Matching nonblank symbols

It is sometimes useful to match nonblank symbols, especially when doing higher-order patterns
(patterns that match patterns). To address this Wirewright has `(%symbol nonblank)`:

```wwml
(literal-symbol? (%symbol nonblank)) => true
(literal-symbol? _symbol) => false

(literal-symbol? qux)        ;; => true
(literal-symbol? _)          ;; => false
(literal-symbol? _number)    ;; => false
(literal-symbol? qux_)       ;; => false
(literal-symbol? qux_number) ;; => false
;; etc...
```

### Matching blank symbols

Likewise, for matching blanks, Wirewright has `(%symbol blank <name> <type>)`. This operator
matches a *named* blank symbol. It splits it into two parts: name and type. Type is an unnamed,
typed blank, similar to the one emitted by `(%pipe type)`.

```wwml
(type-matches (%pipe type T_) (%symbol blank name_ T_)) => (T name)

(type-matches 100 x_number)       ;; => (_number 100)
(type-matches "hello" qux_string) ;; => (_string "hello")

(type-matches 100 foo_symbol) ;; => (type-matches 100 foo_symbol) [mismatch]
;; etc...
```

Note that there is no subtyping behavior: `T`s are compared using equality, not subtype,
so `(type-matches 100 foo_)` will mismatch in the example above: `foo_`s type, matched by
`(%symbol blank)`, is `_`; whereas `100`'s, matched by `(%pipe type)` is `_number`. The simplest
way to avoid this is to introduce a separate rule that handles `_`, simulating subtyping behavior:

```wwml
(type-matches (%pipe type T_) (%symbol blank name_ T_)) => (T name)
(type-matches _ (%symbol blank name_ %'_)) => (_ name)
```

Another way is to use a toplevel `%any°`, in case you cannot define multiple rules:

```wwml
(%any° (type-matches (%pipe type T_) (%symbol blank name_ T_))
       (type-matches _ (%symbol blank name_ T←%'_)))
  => (T name)
```

## Matching strings

Strings specifically can be matched by `_string` or similarly `x_string`.

The companion operator `%string` (like we have `%number`) is being developed. With `%string`, you
will be able to pattern match inside strings (like regex, but tightly integrated into the pattern
matching/backmap process).

```wwml
(known? "John Doe") => true
(known? "Sarah Doe") => true
(known? _string) => false

;; Not implemented:
;;
;; (date (%string "d<4>:year '- d<2>:month '- d<2>:day")) => (ok (date year month day))
;; (date _string) => (err "invalid date")
```

We have decided that in Wirewright, string patterns are going to be on the "outside"
and literal content will have to be escaped. This is in opposition to e.g. PCRE, where
literal content is on the outside and patterns have to be escaped.

This simplifies working with literal content somewhat. For instance, the algorithm to
convert an arbitrary string into a pattern is to surround it with `[]`, breaking at `[`
and `]` inside the string and escaping them using the `'` single-character escape as in `'[`.

Putting patterns on the "outside" also makes them a tiny bit more readable (although not by much).

## Matching booleans

To match any boolean, use `_boolean` or similarly `x_boolean`.

```wwml
(bool? _boolean) => true
(bool? _) => false
```

## Matching dictionaries

To match any dictionary, use `_dict` or similarly `x_dict`.

There are many `%`-operators that match dictionaries. In fact, almost all `%`-operators
match dictionaries. They are going to be described separately. In this section we will only
describe non-`%`ones.

```wwml
(reaction _dict) => "It's a dict!"
(reaction _) => "It's not a dict!"
```

### Matching an itemsonly dictionary

The idiomatic way to match an itemsonly dictionary is `(_*)`. If you want to capture it
use e.g. `xs←(_*)`. You can use `(xs_*)` too if you are OK with the difference in meaning
(the former is capturing the "outside" and the latter is capturing the "inside"). This
difference is important in backmaps `xs←(_*) <> {xs: (1 2 3)}` will result in `(1 2 3)`
and `xs←(_*) <> {(xs): (1 2 3)}` will result in `1 2 3` if there is a surrounding dict.
On the other hand, `(xs_*) <> {xs: (1 2 3)}` will result in `((1 2 3))` and `(xs_*) <> {(xs): (1 2 3)}`
will result in `(1 2 3)`.

A nonempty itemsonly dictionary can be matched similarly using `(_+)`.

```wwml
(reaction (_*)) => "It's an itemsonly dict!"
(reaction (_+)) => "It's a nonempty itemsonly dict!"
(reaction _dict) => "It's a dict!"
(reaction _) => "It's not a dict!"

(reaction ())
;; => "It's an itemsonly dict!"
(reaction (1 2 3))
;; => "It's a nonempty itemsonly dict!"
(reaction (name: "John"))
;; => "It's a dict!"
(reaction (greet name: "John"))
;; => "It's a dict!"
(reaction qux)
;; => "It's not a dict!"
```

### Matching a pairsonly dictionary

The idiomatic way to match a pairsonly dictionary is `(¦ _)`, which is a shorthand for
`(%partition () _)`. Capturing the pairspart would be `(¦ pp_)` and so on.

### Matching an edge

Edges are frequent in µsoma. As a consequence, in patterns, `(edge <typed blank>)` is treated
specially for brevity and historical reasons. It matches an edge "inside-out", treating it as
an "atomic" (indivisible) unit despite the way the pattern is written. It is an unfortunate but
very useful exception from the strictness of `%`-prefixing terms that are specially treated by
the pattern matching engine.

The special treatment is in the fact that `(edge x_)` will be matched as `x←(edge _)`. The only
valid types for the blank are `_number`, `_string`, and `_symbol`. `_` (pass, untyped blank) is
treated as `(%any° _number _string _symbol)`.

`(edge  ...)` has a shorthand syntax of `@...` in WwML: `@x` is the same as `(edge x)` and
`@x_` is the same as `(edge x_)` (which will be matched as `x←(edge _)`). `@x_number` is
a shorthand for `(edge x_number)`, which, as expected, receives special treatment:
`x←(edge _number)`.

However, `(edge (+ 1 2))` is invalid and will be treated "one level of meaning down",
in other words, as a dictionary pattern rather than an edge. Writing `@(+ 1 2)` is an
outright syntax error to prevent confusion. Similarly, `(edge x_dict)` is invalid and
will be interpreted as a normal dictionary pattern.

```wwml
(appender (_* `target) @edge_) <> {target: →edge, (edge): ()}

(appender () @foo)     ;; => (appender (@foo))
(appender (@foo) @bar) ;; => (appender (@foo @bar))
```

If you want to treat edges as "divisible" (that is, if you want them to receive normal,
dict treatment), then you can either enclose `edge` in a `%literal`: `((%literal edge) x_)`,
or use `%nonself` on the argument: `(edge (%nonself x_))`. `%nonself` has the shorthand
prefix `≡` so the latter may be rewritten as `(edge ≡x_)`.

```wwml
(appender (_* `target) (edge ≡edge_)) <> {target: →edge, (edge): ()}

(appender () @foo) ;; => (appender (foo) (edge))
```

### Matching pairs

Wirewright supports several pattern matching constructs for matching dictionary pairs. Most of
them have neat, composable shorthands, all grouped under the *dict pairspart partition* syntax
(see e.g. `%partition`).

#### Required pairs

The simplest construct and one that is implicit is the *required pair* construct: for example,
in `(point x: x_ y: y_)`, `x` and `y` are both *required pairs*: both keys must exist within
the matchee dict and both values must match their corresponding pattern (in this case, `x_` and `y_`).

```wwml
(age (user name: name_string age: age←(%number 1 <= (whole _) <= 24)))
  => age

(age (user name: "Alice" age: 21)) ;; => 21
```

Required pairs whose intended capture name is the same as the key have a shorthand form in the dict
pairspart partition: `(point ¦ x: x_ y: y_)`, which is the same as the previously mentioned pattern,
can be rewritten more succinctly as `(point ¦ () x_ y_)`. Note the use of `()` to "plug" the hole for
`%layer` (see `%layer` docs to learn more).

Explained differently, in the dict pairspart partition, `x_` is the same as `x: x_` which is a shorthand
for `x: x←_` (expanded to `x: (%let x _)`.

In this shorthand syntax, it is not necessary for the right-hand side to be a blank; it can be any pattern.
So `x_: (%number (whole _))`, for example, expands to `x: x←(%number (whole _))` and so on.

```wwml
(age (user ¦ () name_string age_: (%number 1 <= (whole _) <= 24)))
  => age

(age (user name: "Alice" age: 21)) ;; => 21
```

There is a similar typed blank shorthand: `x_number` in the pairspart pattern expands into `x: x_number`
and so on. Although it makes little sense, `x_number: ...` (e.g. `x_number: (%number (whole _))`) is expanded
into `x: (%all x_number (%number (whole _)))` and so on.

```wwml
(translated (point ¦ () x_number y_number) n_number) =>
  (point x: (+ x n) y: (+ y n))

(translated (point x: 100 y: 200) 5) ;; => (point x: 105 y: 205)
```

#### Optional pairs

If you want to allow a pair to be absent or its value pattern to mismatch, you can use
the `%optional` construct, e.g.: `(point x: (%optional 0 x_number) y: (%optional 0 y_number))`.
In this case, if `x`, `y`, or both are absent, `0` will be passed to the value pattern; similarly,
if `x`, `y`, or both are present but their value does not match the value pattern, `0` will
be passed to the value pattern. In effect, `0` works as a fallback term in case something
goes wrong with the user-provided value: `(%optional <fallback term> <value pattern>)`.

Obviously, if the fallback term too does not match the value pattern, then this is a mismatch of
the dictionary pattern overall.

There is a shorthand for `%optional` in the pairspart partition: `<key>⋮ <fallback>` expands
into `<key>: (%optional <fallback term> <key>_<type of fallback term>)`. So e.g. `x⋮ 0` will expand
into `x: (%optional 0 x_number)` and `y⋮ 0` into `y: (%optional 0 y_number)`.

If you want to disable type inference for fallback term, you can use the `<key>_⋮ <fallback>` shorthand,
which expands into `<key>: (%optional <fallback term> <key>_)`. For example, `x_⋮ 0` will expand into
`x: (%optional 0 x_)`, matching not only number term values, but values of any type (and content).

If your pattern is more complex than a typed or untyped blank, you will have to use the full
form of `%optional`: `(point ¦ x: (%optional 0 (%number 0 <= (whole _) <= 100)))`.

There are two common pitfalls with `%optional`:

- Captures should be made *inside*, not outside of `%optional`: it's `x: (%optional 0 qux←...)`, NOT
  `x: qux←(%optional 0 ...)`. The latter is invalid in this context; and will be treated one layer of
  meaning below by the pattern matching engine: as a dictionary pattern whose first item is `%optional`,
  second item is `0`, and so on; all captured under `qux`.

- Remember that `%optional` is not only about the absence of the value in the matchee, but also
  about the presence of a value that does not match the value pattern.

#### Matching the absence of a pair with a known key

Also known as *negative pairs* or *pair negation*.

The absence of a pair with a certain value can be matched using the `(%- <pattern for the value whose absence is expected>)`.
For example, `x: (%- _number)` means "key x with a number value must be absent from the matchee dict".
Said positively, this means "key x must be absent, or its value must not be a number".

The absence of a pair in general can be matched using the `(%- _)` construct. Note how `(%- _)` reads
as "key x with any value must be absent from the matchee dict". In other words, e.g. `x: (%- _)` means
"key x must be absent in the matchee dict".

#### Matching the absence of a pair with a known key, with keypath

The same as the above, except a second argument is appended to `%-`: `(%- <pattern for value whose absence is expected> <keypath capture>)`.

Has a shorthand in the dict pairspart partition: e.g., `(point ¦ -x_ -y_)` expands to `(point ¦ x: (%- _ x) y: (%- _ y))`,
and `-<key>_<type>` expands similarly to `(%- _<type> <key>)`, e.g. `-qux_number` in the dict pairspart
will expand to `qux: (%- _number qux)`, which reads as "match the absence of a numeric value of `qux`".

The intended use of this construct is in backmap patterns. Indeed, this is a very common
construct in "enhancement"-based rule systems. Different rules match "absences" in the matchee
and "enhance" it via the backspec. For instance, some rules may know how to compute the size
of a text:

```wwml
;; Rule Alice> I know how to compute the size of a text if it's missing both width and height.
;;             I'll ignore the rest of pairs.
(text caption_string ¦ _ -w_ -h_)
  <> {w: (measure-width →caption), h: (measure-height →caption)}

;; Rule Bob> I know how to compute the size of a text if it has a set width but is
;;           missing the height! I'll ignore the rest of pairs.
(text caption_string ¦ _ w_number -h_)
  <> {h: (measure-wrapped-height →caption →w)}
```

Thus they can "enhance" text nodes with size information, which can trigger a "chain reaction"
of further transformations (e.g. sizing of a content-sized parent).

## Captures and `%let`

Captures are a way to save the matched term (most often it is called *matchee*) in a *capture
environment*, which is more commonly referred to as *match environment* or simply *match env*
or *env*. The "matched term" here means the term matched by (corresponding to, "underneath")
the capture itself, rather than the term matched by the entire pattern.

Why would one capture?

1. To access and work with the captured terms after the pattern matching process.
2. To refer to the captured term within the pattern itself, but in other places. This is sometimes
   seen as "learning", since some operators (e.g. `%value`) do not know how to create a capture
   and only know how to read it (thus someone in front or ahead of them must write the capture first).
3. To use backmaps: backmaps allow one to modify terms underneath captures. In a sense, captures
   are "getters" and backmaps can turn them into "setters", for those familiar with the terms.

Captures are primarily made using `%let`: `(%let n (%number (whole _)))` captures a whole number
as `n`. There is a shorthand syntax for `%let`, `←`: in this case that would be `n←(%number (whole _))`.
Blanks `name_of-blank_` are a shorthand for `%let` with a type: `x_number` is the same as `x←_number`
which is the same as `(%let x _number)`.

Other operators can make captures as well. But only `%let` is of interest to us in this section.

The first argument of `%let` is the name of the capture. Any term will suffice: a number, a symbol,
a dictionary, etc:

- For `(x_ y_)` (and therefore `(x←_ y←_)`, which is the same as `((%let x _) (%let y _))`) and
  `(1 2)` as the top matchee one will have the match env `{x: 1, y: 2}`.
- For `(qux (%let 0 _) (%let 1 _))` and `(qux 1 2)` as the top matchee one will have
  the match env `(1 2)`.

Captures that have the same name are "equated" across the entire pattern:

```wwml
(equal? x_ x_) => true
(equal? _ _) => false

(equal 100 100) ;; => true
(equal 100 200) ;; => false
```

This feature may lead one to neat patterns such as:

```wwml
(value (%value k v_) k_) => (ok v)
(value _ _) => (err "key not found")

(value {x: 100, y: 200} x) ;; => (ok 100)
(value {x: 100, y: 200} y) ;; => (ok 200)
(value {x: 100, y: 200} z) ;; => (err "key not found")
```

As we have said, other operators make captures as well, so the "equatability" of
captures applies to them too. This can be seen in the example above and in this one:

```wwml
(first-common ⟨x_⟩ ⟨x_⟩) => (ok x)
(first-common _ _) => (err "no common elements")

(first-common (1 2 3) (a b 1 c 2)) ;; => (ok 1)
(first-common (1 2 3) (4 5 6))     ;; => (err "no common elements")
```

## `%partition`

All dictionaries in Wirewright can be divided into an *itemspart* and a *pairspart*.

An item is an entry with the key `0` or an entry with a natural number key that
has a predecessor item. E.g. `1` in `{0: foo, 1: bar}` is an item because it is
preceded by `0`, an item, and is a natural number.

A pair is any other entry. For example, in the dictionary `{0: foo, 1: bar, 3: baz}`,
the entries `0` and `1` are items; whereas `3` is a pair, since despite having a natural
number key, it does not have a predecessor item.

`%partition` lets you match the itemspart and the pairspart separately.

The itemspart is normally used for list/array functionality and the pairspart is used
for storing key-value pairs (e.g. configuration). However, they are only a way of looking
at a dictionary. Looking at a dictionary differently, you would simply get a hash map
with entries.

`%partition` has the shorthand syntax `(<item patterns...> ¦ <pairspart pattern>)`.
The `¦` symbol is known as the "partition pipe". WwML uses such Unicode symbols to leave
ASCII ones to you, and also, for simplicity. It is reasonably straightforward to set up
one's system to be able to enter them (e.g. using the Compose key).

```wwml
(split (items_* ¦ pairs_)) => (items pairs)

(split ()) ;; => (() ())
(split (100 200 300)) ;; => ((100 200 300) ())
(split (x: 100, y: 200, z: 300)) ;; => (() (x: 100 y: 200 z: 300))
(split (100 200 300 x: 100 y: 200 z: 300)) ;; => ((100 200 300) (x: 100 y: 200 z: 300))
```

## `%any°`

`%any°` is a general-purpose alternation operator. It is much like `%any`, but allows to
specify alternative *patterns* rather than *literals*. `%any°` outputs the results from all
successful branches exhaustively (hence it is a *source*, as the source mark `°` indicates).
In other words, `%any°` is not "find first pattern that matches", but rather, "find all patterns
that match, allowing others to fail".

Branches of `%any°` do not "see" each other. As a consequence, for example, equality constraints
do not work across an `%any°` boundary. Instead of trying to "equate", the matches that `%any°`
finds are simply concatenated into one big stream of matches.

```wwml
(find (%any° ⟨(even x_)⟩° ⟨(odd x_)⟩°)) =>° x

(find (even 2) (even 4) (odd 1) (odd 3) (qux 10) (qyx 20))
;; => 2
;;    4
;;    1
;;    3
```

## `%all`

`%all` allows to apply multiple patterns to the same term. All patterns must match. All
branches of `%all` are visible to each other.

```wwml
(names (%all (people _*) ⟨(person name_string)⟩°)) =>° name

(names
  (people (person "John Doe")
          (person "Samantha Doe")
          (dog "Bobby")))

;; => "John Doe"
;;    "Samantha Doe"

;; If we omit "people", required in the first branch of %all, we'll miss
;; the rule:

(names ((person "John Doe")
        (person "Samantha Doe")))

;; => (names ((person "John Doe")
;;            (person "Samantha Doe")))
```

## `%keypool`

`%keypool` matches dictionaries whose set of keys is a subset of the keys given
to `%keypool`.

For example, `(%keypool a b c)` matches:

- an empty dictionary {}
- a dictionary with only key a, b, or c: {a: ...}, {b: ...}, {c: ...}
- a dictionary with keys a and b: {a: ..., b: ...}
- a dictionary with keys a and c: {a: ..., c: ...}
- a dictionary with keys b and c: {b: ..., c: ...}
- a dictionary with all three keys a, b, and c: {a: ..., b: ..., c: ...}.

In other words, `%keypool` lets you allow a set of keys to be present in the dict
while denying the presence of all other keys.

```wwml
(user? (%keypool username email age)) => true
(user? _) => false

(user? {})
(user? {username: "alice", email: "alice@example.com"})
(user? {username: "alice", age: 25})
(user? {username: "alice", email: "alice@example.com", age: 25})
;; etc...
;; => true

(user? {username: "bob", password: "passw0rd"})
;; => false, `password` is not in the set of allowed keys.
```

## `%not`

`%not` allows you to prevent select terms from matching. The terms are treated literally.
`%not` accepts one or more term: `(%not a)`, `(%not a b c)`, etc.

```wwwl
(grant-access? _) => true
(grant-access? (%not "admin" "owner")) => false

(grant-access? "admin")    ;; => true
(grant-access? "owner")    ;; => true
(grant-access? "john_doe") ;; => false
```

## `%pipe`

`%pipe` lets you "pipe" the matchee through a chain of transformations. The following
transformations are supported:

- `span`: transforms a string matchee into the number of characters in it: e.g., `(%pipe span 1)`
  matches strings that contain just one character.
- `tally`: transforms a dictionary matchee into the number of entries in it: e.g., `(%pipe tally 3)`
  matches dictionaries with three entries (items, pairs, or both).
- `type`: transforms any term into an unnamed, typed blank: e.g. `(%pipe type T_)` will match
  `42` with `{T: _number}`, `"hello"` with `{T: _string}`, etc.
- `(+ n_number)`: adds `n` to a number matchee: e.g. `(%pipe (+ 100) x_)`.
- `(- n_number)`: subtracts `n` from a number matchee: e.g. `(%pipe (- 100) x_)`.
- `(* n_number)`: multiplies a number matchee by `n`: e.g. `(%pipe (* 2) double_)`.
- `(/ n_number)`: divides a number matchee by `n`: e.g. `(%pipe (/ 2) n_)`. Turns into
  a nevermatch if `n` is zero: `(%pipe (/ 0) this_will_never_match_)`.
- `(div n_number)`: integer division of a number matchee by `n`: e.g. `(%pipe (div 10) x_)`
  will match `42` with `{x: 40}`. Like `/`, it is a nevermatch if `n=0`
- `(mod n_number)`: remainder after integer division of a number matchee by `n`:
  e.g. `(%pipe (mod 10) x_)` will match `42` with `{x: 2}`. Like `/`, it is a nevermatch
  if `n=0`.
- `(** n_number)`: raises a number matchee to the power `n`: e.g. `(%pipe (** 2) n_)` will
  match `4` with `{x: 16}`. It is a nevermatch if the matchee is zero and `n` is negative.
- `(map mapper_dict)`: retrieves the value (if any) associated with a matchee term
  in `mapper`.

You can chain `%pipe` transformations: e.g., `(%pipe (+ 1) (** 3) (mod 2) (map (10 20)) x_)`
matches `42` with `{x: 20}` because `(42 + 1)**3 mod 2 = 1`, which, according to the mapper
dict `{0: 10, 1: 20}` is equal `20`, and is subsequently captured by `x_`.

```wwml
(parity (%pipe (mod 2) (map (even odd)) p_)) => p

(parity 0)  ;; => even
(parity 2)  ;; => even
(parity 1)  ;; => odd
(parity -5) ;; => odd
```

## `%new`

The idea with the `%new` operator is to be able to create new patterns at match-
time, through a process of "learn and replace". `%new` is opaque at pattern compile-
time. `%new` is a fairly advanced operator; most likely, you do not and will not
need it.

`%new` comes with some caveats and footguns, like anything advanced. Its simplest
version will work most of the time:

```wwml
(< (%new (%number _ < m_)) m_number) => true
(< _ _) => false

(< 5 5)   ;; => false
(< 100 5) ;; => false
(< 5 100) ;; => true
```

... but not always! The moment you want it to cooperate with e.g. `%many` so that
an equality constraint is established, the simple version fails. The following example
neatly illustrates (and slightly exaggerates) this: here, `%many` clearly cannot know
ahead-of-time the captures `%new` is going to make; and thus will think `x_` here
is confined to the interior of `%many`, establishing no equality constraint with
the outside:

```wwml
(qux (%many xs x_) (%new v_) v_) => ok
(qux _*) => not-ok

(qux a b c d x_)
;; {v: x_, x: d, xs: ({x: a}, {x: b}, {x: c})}
;; => ok ?! :(
```

Wirewright does not currently have a general "fix" for this issue; operators such as `%many`,
`%items`, and `%leaves` simply *must* know their interior vs. exterior captures ahead-of-
time, period. As a temporary solution, if *you* know ahead-of-time what captures are going
to be made in `%new`, you can use the *extended form* of `%new`.

The extended form accepts a list of captures that `%new` can make, called *exports*, followed
by the pattern template. Capture inference is still done on the template; you do not need to
list `v` as one of the captures.

```wwml
(qux (%many xs x_) (%new (x) v_) v_) => ok
(qux _*) => not-ok

(qux a b c d x_)
;; => not-ok :)

(qux d d d d x_)
;; => {v: x_, x: d, xs: ({} {} {})}
;; => ok :)
```

`%new` can do amazing things, but at the cost of pattern compilation at match-time. Wirewright
does not and will probably not optimize pattern compilation, so heavy use of dynamic patterns
is slow and will probably remain slow. How "slow" depends on the pattern, of course, but in
general, pattern compilation can take from dozens of microseconds to milliseconds. Note that
when a pattern is compiled with `%new`, it is cached into a global, `%new`-only LRU cache.
Thus re-use of the same inputs to `%new` will hit the cache, which improves performance.

Here is a cool example of what is possible with `%new`:

```wwml
((%plural/min pre)
 (%group l (%gap/max (%new (%number _ <= n_))))
 (| n_)
 (%group r (%gap/max (%new (%number _ <= n_))))
 (%plural/min post)) <> {(l): (), (r): ()}
```

In essence what you see here is the cursor "radiating" gaps in both directions up to some
"distance" n or lower. The "nudging" with `/min` and `/max` variants of `%gap` and `%plural`
is to make sure the engine gives way to the cursor (`|`).

The backspec says that `l` and `r` groups should be emptied. If we apply the backmap to the
dictionary `(a b c (| 1) d e f)`, we will get the dictionary `(a b (| 1) e f)` in response.
We can apply the backmap again to see `(a (| 1) f)`; the cursor is "eating" terms around it
up to a certain distance. If we make the sides unequal, as in `(a b (| 1) c d e f)`, we will
see the cursor eat terms while it can to the left, and then only eat terms to the right:

```wwml
Step 1 | (a b (| 1) c d e f)
Step 2 | (a (| 1) d e f)
Step 3 | ((| 1) e f)
Step 4 | ((| 1) f)
Step 5 | ((| 1))
```
