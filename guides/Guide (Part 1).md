Open `seed.wwml` in your favorite text editor and write the following:

```wwml
(cell @x (1 2 3))
(cell @y ())
(feed (@x front) (@y back))
```

Save the file and run it with `./irack --single-step seed.wwml`:

```bash session
$ ./irack --single-step seed.wwml
(cell @x (1 2 3))
(cell @y ())
(feed (@x front) (@y back))
<Press Enter>

(cell @x (2 3))
(cell @y (1))
(feed (@x front) (@y back))
<Press Enter>

(cell @x (3))
(cell @y (1 2))
(feed (@x front) (@y back))
<Press Enter>

(cell @x ())
(cell @y (1 2 3))
(feed (@x front) (@y back))

$
```

With this example, we have set up a flow of numbers from the *dictionary* at `@x`, into the *dictionary* at `@y`.

What is a *dictionary*? A dictionary is a *term*, just like a *number*, a *string*, and a *symbol*. In fact, I think we are now well set to look at all six types of terms in Wirewright:

- *Number*: a data type for storing integers (`123`, `-123`, `0`; arbitrarily large), rationals (`1/3`, `0.5`; arbitrarily large), and floating-point values (`≈12.34`, `≈+Infinity`, `≈NaN`; uses a 32-bit floating point value under the hood).
- *String*: a data type for storing sequences of Unicode characters encoded with UTF-8.
- *Symbol*: a data type used to name things. You cannot easily create or manipulate symbols dynamically — the vast majority of them are created during "the Big Bang", when `seed.wwml` is parsed. Symbols are extremely cheap to hash and compare. `cell` is a symbol,  `feed` is a symbol, as are `front` and `back`.
- *Boolean*: a data type for storing logical `true` and `false`.
- *Blob*: a data type for storing arbitrary binary data, optionally with an associated  media type (MIME type).

And finally, there are *dictionaries* (*dict* for short). A dictionary can have many forms. It can be:

* A list of terms, called the *items* of the dictionary. `(1 2 3)` is a list of *number* terms, and `("Hello" "World" "Kaixo" "mundua")` is a list of *string* terms. `(+ 1 2)` is a list containing the symbol `+`, followed by numbers `1` and `2`. In `(+ (* 2 3) 4)`, the symbol `+` is followed by another dictionary (remember that dictionaries are terms, too!), and then by the number `4`.
* A list of key-value pairs, called the *pairs* of the dictionary. For example, in `(x: 100 y: 200)`, we define a dictionary containing two pairs. One is `x: 100`, whose *key* is the symbol `x`, and whose value is the number `100`. Similarly there's `y: 200`. Both the key and the value of a pair are terms. So you can do `("name": "John" "age": 25)`, where keys are strings; or something even weirder, a *dictionary* key: `((+ 1 2): 3 (+ 3 4): 7)`. It is customary in WwML to use the curly brace `{}` syntax for dictionaries that only contain pairs. So our examples become, correspondingly `{x: 100, y: 200}`, `{"name": "John", "age": 25}`, `{(+ 1 2): 3, (+ 3 4): 7)}`. The commas are optional, but in this curly brace syntax, they are recommended to aid readability. More generally, WwML treats commas `,` in source code (but not in strings etc!) as whitespace. So you can put them anywhere where whitespace works.
* A mix of both items and pairs, which are collectively known as *entries*. An item is an *entry* of a dictionary, and a *pair* is an entry too. Consider `(/ 1 2 precision: 3)`. Here, the symbol `/` and numbers `1` and `2` are the items of the dictionary, and `precision: 3` is the pair. Or take `(point x: 100 y: 200)`. Here, the dictionary has only one item, the symbol `point`; but there are two pairs, `x: 100` and `y: 200`.
* A dictionary can also be *empty*: `()` if you *intend* it to be a dictionary containing items or items and pairs; or `{}` if you intend it to only contain pairs. The choice of whether to use `()` or `{}` is yours, and is of a purely aesthetic value. WwML does not distinguish between the two; both *mean* the same thing, an empty dictionary.

It is oftentimes useful to *partition* a dictionary, which means to split its items from its pairs. The result of partitioning a dictionary is two new dictionaries: the first is called the *itemspart*, and the second one is the *pairspart* of the original dictionary. For example, partitioning the dictionary `(/ 1 2 precision: 3)` gives us the itemspart dict `(/ 1 2)` and the pairspart dict `{precision: 3}`.

Now, let's return to our circuit:

```wwml
(cell @x (1 2 3))
(cell @y ())
(feed (@x front) (@y back))
```

As you have probably recognized already, almost everything here is a dictionary: the `cell` nodes are, as is the `feed`. The `(@x front)` and `(@y back)` are dictionaries, too. Moreover, `@x` and `@y` are secretly dictionaries too! `@x` is a shorthand for `(edge x)` (and `@y` is the same as writing `(edge y)`). Of course, our two lists are dictionaries as well: `(1 2 3)` and the empty dict `()`.

Just about everything uses dictionaries in Wirewright, and more generally, *terms*. Some things communicate using terms. Other things *are* terms. Other things yet are *about* terms: they convert data from the outside world into terms, or do the opposite conversion. There are components which turn terms into commands to the OS; and there are those that turn the responses of the OS back into terms. UIs are represented with terms, as are circuits; *patterns* that match terms are terms, descriptions of term transformations are terms, rewrite rules are terms, entire rule systems are terms; as are many, many other things. Terms are truly fundamental in Wirewright, and the most fundamental kind of them is the dictionary.

Now that you know what a dictionary term is, let's try to see what our circuit is doing. The `(1 2 3)` inside `@x` is a list of values, a dict. The `()` in `@y` is an empty dict (which we intend to be a list of items, so we write it like `()` rather than `{}`). The `feed` node allows us to refer to the *front* and *back* of a dict. The exact meaning of *front* and *back* depends on their position in the feed (i.e., on whether it is part of the feed's *source* or *destination*):

- When you use `(@edge front)` in the source, `feed` reads this as: "first item of the dictionary at `@edge`". So in our case, `(@x front)` means "*the first item* of the dictionary at `@x`".
- When you use `(@edge back)` in the source, `feed` reads this as: "*the last item* of the dictionary at `@edge`".
- When you use `(@edge front)` in the destination, `feed` reads this as: "*before the first item (if any)* of the dictionary at `@edge`".
- When you use `(@edge back)` in the destination, `feed` reads this as: "*after the last item (if any)* of the dictionary at `@edge`".

So what we are doing with the feed in our circuit is we are moving the first item of the dictionary `@x` after the last item (if any) of the dictionary at `@y`.

Let's say we start with:

```wwml
(cell @x (1 2 3))
(cell @y ())
(feed (@x front) (@y back))
```

This is our *complete previous frame*. When Rack is asked to compute the next frame, it forwards the question to `feed`. In effect, it asks the `feed`: given what you see in the *complete previous frame*, what should happen?

To this question, `feed` readily responds. There is a dictionary in `@x` which has at least one item (and so it has a `front`). There is a list in `@y` (and so it has a `back`; any dictionary has a `back`, whether it has items or not, and even if it has pairs, it also has a `back` — remember mixed item and pair dictionaries.) The front item in our case is `1`. After reasoning through all this, `feed` concludes: what should happen is `1` should be removed from the beginning of the dict at `@x`, and appended to the dict at `@y`.

The mic is then back to Rack, which does some more thinking-and-asking. The `cell`s are asked the same question, for example; but they generally stay quiet, for they are passive or *inert* nodes. Finally, the `feed`'s proposed changes are accepted:

```wwml
(cell @x (2 3))
(cell @y (1))
(feed (@x front) (@y back))
```

Neat! Now rinse and repeat:

```wwml
(cell @x (3))
(cell @y (1 2))
(feed (@x front) (@y back))
```

And again:

```wwml
(cell @x ())
(cell @y (1 2 3))
(feed (@x front) (@y back))
```

And again... But this time, things are different.  The dict at `@x` does not have a front item! `feed` notices that, and refuses to change anything: one of its preconditions, that `@x` contains at least one item, is broken, so it chooses to stay quiet. The result is a frame that looks exactly like the previous one:

```wwml
(cell @x ())
(cell @y (1 2 3))
(feed (@x front) (@y back))
```

Remember *quiescence* from Guide 0? Rack notices there was no change, notices there are no background tasks, so nothing can possibly trigger a change from the outside. The circuit has reached quiescence, so Rack wraps things up and quits.

I think it is time now that we encounter our first *conflict*. A conflict occurs when two or more nodes try to modify the same third node (or each other!) in an incompatible way. What *incompatible* means evolved to be a concept rather hard to explain. It is best understood intuitively. Do note, by the way, that conflicts are rather rare in practical circuits; still, there is a point in understanding them.

Open `seed.wwml` and write:

```wwml
(cell @x (1 2 3))
(cell @y ())
(cell @z ())
(feed (@x front) (@y back))
(feed (@y front) (@z back))
```

Run it and observe what happens:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @x (1 2 3))
(cell @y ())
(cell @z ())
(feed (@x front) (@y back))
(feed (@y front) (@z back))
<Press Enter>

# Frame 1
(cell @x (2 3))
(cell @y (1))
(cell @z ())
(feed (@x front) (@y back))
(feed (@y front) (@z back))
<Press Enter>

# Frame 2
(cell @x (3))
(cell @y (1 2))
(cell @z ())
(feed (@x front) (@y back))
(feed (@y front) (@z back))
<Press Enter>

# Frame 3
(cell @x ())
(cell @y (1 2 3))
(cell @z ())
(feed (@x front) (@y back))
(feed (@y front) (@z back))
<Press Enter>

# Frame 4
(cell @x ())
(cell @y (2 3))
(cell @z (1))
(feed (@x front) (@y back))
(feed (@y front) (@z back))
<Press Enter>

# Frame 5
(cell @x ())
(cell @y (3))
(cell @z (1 2))
(feed (@x front) (@y back))
(feed (@y front) (@z back))
<Press Enter>

# Frame 6
(cell @x ())
(cell @y ())
(cell @z (1 2 3))
(feed (@x front) (@y back))
(feed (@y front) (@z back))
<Press Enter>

$
```

So where is the conflict? Well, perhaps you expected the `1` in Frame 2 to be moved down from `@y` into `@z`. Why was it not? All preconditions of both `feed`s are — apparently — met.

They *are* met, *and that's the problem*. Let's take the point of view of the first feed when Rack asks it to compute its contribution to Frame 2. What the feed sees at that point, remember, is the complete previous frame, Frame 1:

```wwml
(cell @x (2 3))
(cell @y (1))
(cell @z ())
(feed (@x front) (@y back)) ;; < We are this feed
(feed (@y front) (@z back))
```

In our mind, let's advance time by one time-step, ignoring the other feed:

```wwml
(cell @x (3))
(cell @y (1 2))
(cell @z ())
(feed (@x front) (@y back)) ;; < We are this feed
(feed (@y front) (@z back))
```

Everything looks fine, doesn't it? But now let's take the point of view of the *second* feed. It, too, is asked by Rack to compute its share of Frame 2. Again, what it sees is the entirety of Frame 1:

```wwml
(cell @x (2 3))
(cell @y (1))
(cell @z ())
(feed (@x front) (@y back))
(feed (@y front) (@z back)) ;; < We are this feed now!
```

Its preconditions are satisfied, so it computes:

```wwml
(cell @x (2 3))
(cell @y ())
(cell @z (1))
(feed (@x front) (@y back))
(feed (@y front) (@z back)) ;; < We are this feed now!
```

Everything looks alright for the second feed, too. But look at what happens from *Rack*'s point of view when it tries to reconcile the two changes, called *patches*:

| Frame 1           | Patches by<br>`(feed (@x front) (@y back))` | Patches by<br>`(feed (@y front) (@z back))` |
| ----------------- | ------------------------------------------- | ------------------------------------------- |
| `(cell @x (2 3))` | `(cell @x (3))`                             | *Not changed*                               |
| `(cell @y (1))`   | `(cell @y (1 2))`                           | `(cell @y ())`                              |
| `(cell @z ())`    | *Not changed*                               | `(cell @z (1))`                             |

The `@x` and `@z` cells are fine. There is no conflict. But look at `@y`. From Rack's point of view, they're entirely different dictionaries. It has no way to "merge" them. So it rejects *all* patches from one of the `feed`s. Which one is deterministic but undefined (if you want to know the exact mechanism, here it is: on conflict, we sort nodes lexicographically, and the first one is the winner. That's why rearranging nodes won't help affect the winner; Rack fully commits to node order-freedom here.)

When you want complex transformations like the one we attempted to do with our `feed`, you need something more "surgical". Feeds aren't it because they treat the dictionary as if it was opaque, indivisible. They have no notion of "reconciling an append with a dequeue", which would otherwise be perfectly possible.

Similarly, there is a conflict in the circuit:

```wwml
(cell @x (1 2 3))
(cell @y ())
(cell @z ())
(feed (@x front) (@y back))
(feed (@x back) (@z back))
```

If you run it, it just moves all values from `@x` to `@y` and then terminates. The reason is, the `feed`s clash over `@x`; and Rack decides the first `feed` should win. 

Your intuitive expectation might have been that values are "peeled off" of both sides of `@x` into `@y` and `@z`. But this can't be done with just a pair of `feed` nodes, because they will fight with each other over who gets to modify the dictionary in `@x`. Only one of them will win — as described above, the lexicographically first node will.

As a sneak peek into how you'd "peel" values off `@x` this way, try this circuit:

```wwml
(cell @x (1 2 3 4))
(cell @y ())
(cell @z ())
(backsys
  {¦ x: (a_ _*) y: (_* `>a)} <> {(a): (), >a: ^a}
  {¦ x: (_* a_) z: (_* `>a)} <> {(a): (), >a: ^a})
```

Run it, and you'd get:

```bash session
$ ./irack --single-step seed.wwml
(cell @x (1 2 3 4))
(cell @y ())
(cell @z ())
(backsys
  {¦ x: (a_ _*) y: (_* `>a)} <> {(a): (), >a: ^a}
  {¦ x: (_* a_) z: (_* `>a)} <> {(a): (), >a: ^a})
<Press Enter>

(cell @x (2 3))
(cell @y (1))
(cell @z (4))
(backsys
  {¦ x: (a_ _*) y: (_* `>a)} <> {(a): (), >a: ^a}
  {¦ x: (_* a_) z: (_* `>a)} <> {(a): (), >a: ^a})
<Press Enter>

(cell @x ())
(cell @y (1 2))
(cell @z (4 3))
(backsys
  {¦ x: (a_ _*) y: (_* `>a)} <> {(a): (), >a: ^a}
  {¦ x: (_* a_) z: (_* `>a)} <> {(a): (), >a: ^a})
<Press Enter>

$
```

`backsys` (short for *backsystem*) is a node you will meet later in this guide. Think of it as the Swiss-army knife of term (and node) transformation. An important aspect is that a `backsys`tem is a system of *simultaneous* rewrite rules. Backsystems work a bit like Rack works: all *backmaps* `... <> ...` in a backsystem are given a view  of the complete previous state to match on (in our case, the most interesting bit of that state being the dict at `@x`).

Feel free to modify the circuit above by adding more numbers to `@x`. An especially interesting thing to try out is to have an odd count of numbers.

So, as you can see, it *is* possible to peel off the values here, but you need a more "surgical" tool rather than the blunt `feed`. We will stick with `feed` for now anyway, though, due to its simplicity.

The variant of `feed` we have been working with is called the *transfer* variant. It *transfers* (moves) a value from one place (cell) to another (1:1, one-to-one).  There are other variants of `feed`, and it is time for us to take a look at them.

In `seed.wwml`, write the following:

```wwml
(cell @x 100)
(cell @y 200)
(cell @z)
(feed (@x @y) @z)
```

Run it and observe what happens:

```bash session
$ ./irack --single-trace seed.wwml
(cell @x 100)
(cell @y 200)
(cell @z)
(feed (@x @y) @z)
<Press Enter>

(cell @x)
(cell @y)
(cell @z (100 200))
(feed (@x @y) @z)
<Press Enter>

$
```

What you see right here is the *aggregate* variant of `feed` in action. The aggregate variant takes terms from one or more source places (with the assumption that all of them are occupied!), wraps them in a dictionary, and then places the dictionary at the destination.

The order of edges in `(@x @y)` in `feed` is important. It determines the order of values in the dictionary at the destination. For example, if you flip the order of `(@x @y)` to `(@y @x)`, you'll get:

```bash session
$ ./irack --single-trace seed.wwml
(cell @x 100)
(cell @y 200)
(cell @z)
(feed (@y @x) @z)
<Press Enter>

(cell @x)
(cell @y)
(cell @z (200 100))
(feed (@y @x) @z)
<Press Enter>

$
```

Notice how the values in the aggregate dict are now flipped, too.

The aggregate variant is the M:1 (many-to-one) variant of `feed`. There is its inverse, too, called the *distribute* variant. In `seed.wwml`, write:

```wwml
(cell @x)
(cell @y)
(cell @z (100 200))
(feed (@z items) (@x @y))
```

Running this with `irack`, we get:

```bash session
$ ./irack --single-trace seed.wwml
(cell @x)
(cell @y)
(cell @z (100 200))
(feed (@z items) (@x @y))
<Press Enter>

(cell @x 100)
(cell @y 200)
(cell @z)
(feed (@z items) (@x @y))
<Press Enter>

$
```

The broadcast `feed` "unpacked" the dictionary at `@z` into the corresponding cells `@x` and `@y`. Just like in the aggregate variant, the order of edges `(@x @y)` matters here, and affects which cells receive which corresponding values.

Now that we know the aggregate and the broadcast variants of `feed`, we can build an oscillator using both of them! In `seed.wwml`, type:

```wwml
(cell @x 100)
(cell @y 200)
(cell @z)
(feed (@x @y) @z)
(feed (@z items) (@x @y))
```

The idea is for the first `feed` to "pack" `@x` and `@y` into a dictionary and put it at `@z`, and then on the next frame the second feed would do the reverse ­— "unpack" `@z` into `@x` and `@y`. Thus the cycle closes, and the circuit oscillates indefinitely between the two states:

```bash session
$ ./irack --single-trace seed.wwml
(cell @x 100)
(cell @y 200)
(cell @z)
(feed (@x @y) @z)
(feed (@z items) (@x @y))
<Press Enter>

(cell @x)
(cell @y)
(cell @z (100 200))
(feed (@x @y) @z)
(feed (@z items) (@x @y))
<Press Enter>

(cell @x 100)
(cell @y 200)
(cell @z)
(feed (@x @y) @z)
(feed (@z items) (@x @y))
<Press Enter>

(cell @x)
(cell @y)
(cell @z (100 200))
(feed (@x @y) @z)
(feed (@z items) (@x @y))
<Ctrl-C>

$
```

There is another variant of `feed`, called the *parallel transfer* variant. It takes values from a list of one or more places, and moves them into a corresponding number of other places (M:M, many-to-many). In `seed.wwml`, write:

```wwml
(cell @x 100)
(cell @y 200)
(cell @a)
(cell @b)
(feed (@x @y) (@a @b))
```

Running the circuit, we see:

```bash session
$ ./irack --single-trace seed.wwml
(cell @x 100)
(cell @y 200)
(cell @a)
(cell @b)
(feed (@x @y) (@a @b))
<Press Enter>

(cell @x)
(cell @y)
(cell @a 100)
(cell @b 200)
(feed (@x @y) (@a @b))
<Press Enter>

$
```

The `feed` moved `@x` into `@a` (i.e., the corresponding destination), and `@y` into `@b`.

The last variant of `feed` we will cover here is the *broadcast* variant. It broadcasts the same value to multiple destinations, while removing it at the source. In `seed.wwml`, type:

```
(cell @x 100)
(cell @y)
(cell @z)
(feed @x (@y @z))
```

Then run the circuit with `irack`:

```bash session
$ ./irack --single-trace seed.wwml
(cell @x 100)
(cell @y)
(cell @z)
(feed @x (@y @z))
<Press Enter>

(cell @x)
(cell @y 100)
(cell @z 100)
(feed @x (@y @z))
<Press Enter>

$
```

As you can see, `100` was moved to both `@y` and `@z` (the destination places of the `feed`), and `@x` (the source place) was cleared.

`feed` allows you to do some extra things, but it is not necessary to know about them to proceed with this guide. If you are interested and want to experiment with `feed` a little bit more, feel free to check out the docs for `feed` in the doctool.

To do so, open the doctool and navigate to `rack`, then to `feed`. What you should see is a page listing the *overloads* of `feed`, and describing each of them in detail. *Overloads* is `doctool` vocabulary; do not confuse it with *variants*, which is vocabulary coming from Rack.

All variants of `feed` can be *inhibited*: you can attach a list of one or more edges that must *all* be empty (or not have a cell at them) for the `feed` to fire:

```wwml
(cell @x 100)
(cell @y)
(feed (not @inhibitor) @x @y)

(cell @inhibitor)
```

Running this, we get:

```bash session
$ ./irack --single-trace seed.wwml
(cell @x 100)
(cell @y)
(feed (not @inhibitor) @x @y)
(cell @inhibitor)
<Press Enter>

(cell @x)
(cell @y 100)
(feed (not @inhibitor) @x @y)
(cell @inhibitor)
<Press Enter>

$
```

The `100` was moved to `@y` just fine. But in the circuit below, the `@inhibitor` cell is nonempty:

```wwml
(cell @x 100)
(cell @y)
(feed (not @inhibitor) @x @y)
(cell @inhibitor "A value")
```

It can contain any value at all. The important condition is its *presence*, not what value it is. Running this circuit, we get:

```bash session
$ ./irack --single-trace seed.wwml
(cell @x 100)
(cell @y)
(feed (not @inhibitor) @x @y)
(cell @inhibitor "A value")
<Press Enter>

$
```

Rack exited immediately due to *quiescence*, which you are already familiar with: the circuit simply did not change, because no node in it willed to change. `cell`s are inert, and `feed` is inhibited by the nonempty `@inhibitor`.

In all `feed` variants, you can use `front` and `back` (which we have encountered in the beginning) instead of simple edges:

```wwml
(cell @xs (1 2 3))
(cell @ys (100 200 300))
(cell @z)
(feed ((@xs front) (@ys back)) @z)
```

Here is how this circuit evolves:

```bash session
$ ./irack --single-trace seed.wwml
(cell @xs (1 2 3))
(cell @ys (100 200 300))
(cell @z)
(feed ((@xs front) (@ys back)) @z)
<Press Enter>

(cell @xs (2 3))
(cell @ys (100 200))
(cell @z (1 300))
(feed ((@xs front) (@ys back)) @z)
<Press Enter>

$
```

Rack exits after the second frame because `@z` is now occupied, so `feed` has nothing to do, and all other cells are inert so they say nothing.

> [!NOTE]
> It is not possible to use `front` and `back` in the inhibitor list (yet?) It is also not possible to use the feed chain shorthand with variants other than transfer. That is, something along the lines of `(feed (@x @y) @z (@x @y))` is *invalid* (for now?)

## Experiments

**Experiment 1.** Modify the following circuit to make the `feed` node move items from the *back* of `@x` to the *front* of `@y`. Observe what happens. 

```wwml
(cell @x (1 2 3))
(cell @y ())
(feed (@x front) (@y back))
```

**Experiment 2**. Reverse a list using the original circuit from Experiment 1. As you observe its evolution, explain what it does. Why does it reverse the list?

**Experiment 3.** Find another way to reverse a list, as in Experiment 2. Hint: there are only two such ways and they are symmetrical.

**Experiment 4.** How many numbers at a time would *move* in this circuit? Try to reason through it, then run using `irack` to observe:
`
```wwml
(cell @xs (1 2 3 4 5))
(cell @y)
(feed (not @z) (@xs front) @y)
(feed @y @z)
(cell @z)
(feed @z (@zs back))
(cell @zs ())
```

**Experiment 5**. What would the behavior of the following circuit be? Try to simulate it in your head, then observe using `irack`:

```wwml
(cell @x (1 2 3 4 5))
(cell @y ())
(feed (not @b) (@x front) (@y back))

(cell @a .)
(cell @b)
(feed @a @b @a)
```

> [!NOTE]
> The `.` in `(cell @a .)` is simply a symbol term. It can be replaced by any other term. You can do that if you want. E.g., you can replace it with `foo` or `"hello"` or `100` and verify that nothing changes.

**Experiment 6.** How would this circuit behave? Try to simulate it in your head, then observe using `irack`:

```wwml
(cell @x (0 1 2 3 4))
(cell @y (5 6 7 8 9))
(cell @z ())

(feed (not @b) (@x front) (@z back))
(feed (not @a) (@y front) (@z back))

(cell @a .)
(cell @b)
(feed @a @b @a)
```