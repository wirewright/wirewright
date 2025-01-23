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
