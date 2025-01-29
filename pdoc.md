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

A symbol's only feature is its identity. Other than that there is nothing in it. No operators other
than `_symbol` work with symbols specifically.

```wwml
(lamp on) => (ok "turned the lamp on")
(lamp off) => (ok "turned the lamp off")
(lamp _symbol) => (err "what?")
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
