## Matching anything

If you need to match any term at all, you should use the pass operator. The pass operator
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

## Matching numbers

You can match numbers in several different ways, with more or less discretion.

### Matching any number

If you only care that something is a number and that's all, use `_number`; it is a type check
and nothing more. Similarly, `x_number` is understood as `x←_number` and so on.

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
want to match the positive or negative ranges of the signed types, you can prepend as sign `+` or
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
