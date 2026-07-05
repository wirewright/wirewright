# Tutorial, Part 1

There are two things I'd like to say first.

First, It's easier to learn what Wirewright attempts to do by example, hands-on. So I won't write on syntax. Just play with things and make inferences.

Second, I'd recommend you to start with `irack` which is now available in the newest *randomized* releases ([go here to download](https://github.com/wirewright/wirewright/releases/latest)).

I'm linking `irack` statically so it should hopefully run on any x86-64 Linux, without the necessary "bloat" of MuSoma; the latter trying to "approximate" an entire multimedia stack.

It's easier to run the examples below with `irack`. `irack` is a pure (in the sense of lacking side effects), extremely lightweight way to try out the "meat" of Wirewright instead of struggling through MuSoma's editor, modes, etc. MuSoma is the way to interact with Wirewright in general; but right now, in this tutorial, let's stick with `irack` to avoid getting too distracted.

To get started, create a file, say, `/tmp/seed.wwml`. Open it with your favorite editor and write:

```wwml
(cell @x 0)
(cell @y)
(feed @x @y)
```

Notice something important: Wirewright (conceptually) doesn't distinguish between source code, compile-time, and runtime. It has *seeds* and *evolution*s of those seeds. This is a bit similar to seeds in Minecraft or in the Game of Life: you have an initial configuration of things, and you let that configuration evolve under some rules.

Now that you have the file, use the `irack` binary from the [latest release](https://github.com/wirewright/wirewright/releases/latest) to run it. Run it in single-step mode: `irack --single-step /tmp/seed.wwml`.

Observe what happens when you hit Enter. The zero moved into the empty cell:

```wwml
(cell @x)
(cell @y 0)
(feed @x @y)
```

... and on the next step, the program reached *quiescence*, which caused `irack` to exit.

An important thing to understand now is that `cell`s represent *places* or *locations* in the most abstract sense of the word. A cell can either be occupied (e.g. `(cell @x 0)`) or not (e.g. `(cell @x)`).

Now, `feed` creates a "ramp" between two cells, such that the *source cell* is above the *destination cell*. If there's a *term* in the source cell, it will "roll down" the ramp and fall into the destination cell. If the destination cell is occupied, the term won't have anywhere to fall to, and so the whole thing will be stable. For example, try filling `@y` before running the program:

```wwml
(cell @x 0)
(cell @y "I'm full")
(feed @x @y)
```

If you run this, you'll see that nothing happens. `irack` simply exits because the configuration is already stable.

`feed` can construct "ramps" between multiple cells.

```wwml
(cell @x 0)
(cell @y)
(cell @z)
(feed @x @y @z)
```

If you run this program with `irack`, you'll see how the `0` "rolls" through `@y` down to `@z`.

Here it's useful to note that the order of *nodes* (we call `(cell ...)`, `(feed ...)` etc. *nodes*) does not matter in the vast majority of cases in Wirewright. For example, the above is the same as writing:

```wwml
(feed @x @y @z)
(cell @x 0)
(cell @y)
(cell @z)
```

The exceptions to this are highly explicit and aren't reachable from `irack` (but they are reachable & used in MuSoma). I won't talk about them here.

Now to the most exciting part and the most favorite part of mine: *feedback loops*! What happens if you write a `feed` so that it loops?

```wwml
(cell @x 0)
(cell @y)
(feed @x @y @x)
```

What happens is you arrive at the "official" "Hello World" for Wirewright. It's obviously not a `printf("Hello World")`. In fact, there's no "Hello World" here at all. Yet the above serves the same *purpose*; it demonstrates a lot of what you'll have to take for granted when working with Wirewright. In a sense, the above is the "Hello World" of Wirewright just like the glider is arguably the "Hello World" of the Game of Life. It's an *oscillator*. If you run it, what you'll see is an oscillation between two states:

```wwml
(cell @x)
(cell @y 0)
(feed @x @y @x)
```

... and then back to:

```wwml
(cell @x 0)
(cell @y)
(feed @x @y @x)
```

... and so on, forever. If you extend the example with more cells, everything will work as expected: the zero will travel through more cells before looping back, that's it:

```wwml
(cell @x 0)
(cell @y)
(cell @z)
(feed @x @y @z @x)
```

... which evolves like:

```wwml
;; THIS IS A COMMENT: Frame 1
(cell @x)
(cell @y 0)
(cell @z)
(feed @x @y @z @x)

;; Frame 2
(cell @x)
(cell @y)
(cell @z 0)
(feed @x @y @z @x)

;; Frame 3 (looping back)
(cell @x 0)
(cell @y)
(cell @z)
(feed @x @y @z @x)
```

Now, let's look at an example which demonstrates *synchronous rewriting*.  Let's see  what happens when there are several values in the cells.

```wwml
(cell @u 100)
(cell @v 200)
(cell @x)
(cell @y)
(cell @z)
(feed @u @v @x @y @z @u)
```

This creates a "waterfall" with one cell in between:

```wwml
;; Frame 1
(cell @u 100)
(cell @v)
(cell @x 200)
(cell @y)
(cell @z)
(feed @u @v @x @y @z @u)

;; Frame 2
(cell @u)
(cell @v 100)
(cell @x)
(cell @y 200)
(cell @z)
(feed @u @v @x @y @z @u)

;; Frame 3
(cell @u)
(cell @v)
(cell @x 100)
(cell @y)
(cell @z 200)
(feed @u @v @x @y @z @u)

;; Frame 4
(cell @u 200)
(cell @v)
(cell @x)
(cell @y 100)
(cell @z)
(feed @u @v @x @y @z @u)

;; Frame 5
(cell @u)
(cell @v 200)
(cell @x)
(cell @y)
(cell @z 100)
(feed @u @v @x @y @z @u)

;; ... and so on.
```

Why is the gap there? This is because `feed`, as well as all other nodes, looks at the *complete previous frame* when it is evaluated. In this sense, the process is the same as in cellular automata. So while building Frame 1, `feed` looks at the seed, and sees that while the `@v-@x` "roll-down" is possible, `@u-@v` is not. Whereas while building Frame 2, `200` is at `@x`, so `@v` is empty. Therefore, the `@u-@v` "roll-down" becomes possible, just like `@x-@y`. Both "roll-downs" occur.

Now I want you to get acquainted with M1, Alloy, and Nitrene, simultaneously. M1 is Wirewright's *pattern matching* subsystem. Alloy is Wirewright's *structural templating language*. Nitrene is Wirewright's *expression language*.

So let's use the `transfer` node instead of `feed`. `transfer` works like `feed` but lets you match and transform the term "in passing", using Alloy and Nitrene.

```wwml
(cell @x 0)
(cell @y)
(transfer (@x ±n @y) ("Result is:" ^(+ n 1)))
```

If you run this, you'll see:

```wwml
(cell @x)
(cell @y ("Result is:" 1))
(transfer (@x ±n @y) ("Result is:" ^(+ n 1)))
```

So where's M1? Alloy? Nitrene? They're the different parts of the `transfer` node:

```text
          src    dst    Alloy *template*
           v      v  -----------------------
(transfer (@x ±n @y) ("Result is:" ^(+ n 1)))
              -- 
              M1 *pattern*
```

Nested inside the Alloy template, we have Nitrene:

```text
("Result is:" ^(+ n 1))
                -----
                Nitrene *expression*
```

`transfer` works thanks to cooperation between M1, Alloy, and Nitrene.

The M1 pattern in the above, `±n`, means "match a number" (`±`) and capture it under the name `n` (if you're familiar with regular expressions, this is like regular expression captures). The captures are then made available to the *Alloy template*. If you're familiar with web dev, Alloy templates are a bit like JSX. Inside Alloy templates, you are likely to find Nitrene expressions, often wrapped in `^(...)`. Now that we've likened Alloy to JSX, we can similarly think of Nitrene as being similar to JavaScript embedded in JSX. One notable thing though is that Nitrene is deliberately designed for being embedded this way (that is, many design decisions around how Nitrene behaves are driven by the fact that it's going to be embedded in Alloy in practice).

The Alloy template makes a *dictionary* with two *items*, the *string* `"Result is:"` followed by the result of the Nitrene expression `(+ n 1)`.

For the next example, let's build upon the previous one, but we'll create multiple `transfer` nodes and arrange them into a feedback loop.

```wwml
(cell @x 0)
(cell @y)
(transfer (@x ±n @y) ("Result is:" ^(+ n 1)))
(transfer (@y ("Result is:" ±r) @x) ^r)
```

Observe what happens:

```wwml
;; Frame 1
(cell @x)
(cell @y ("Result is:" 1))
(transfer (@x ±n @y) ("Result is:" ^(+ n 1)))
(transfer (@y ("Result is:" ±r) @x) ^r)

;; Frame 2
(cell @x 1)
(cell @y)
(transfer (@x ±n @y) ("Result is:" ^(+ n 1)))
(transfer (@y ("Result is:" ±r) @x) ^r)

;; Frame 3
(cell @x)
(cell @y ("Result is:" 2))
(transfer (@x ±n @y) ("Result is:" ^(+ n 1)))
(transfer (@y ("Result is:" ±r) @x) ^r)

;; Frame 4
(cell @x 2)
(cell @y)
(transfer (@x ±n @y) ("Result is:" ^(+ n 1)))
(transfer (@y ("Result is:" ±r) @x) ^r)

;; ... and so on.
```

The second transform here "destructures" the result of the first one, and puts it back into `@x`, which drives the feedback loop.

Now let's look at a slightly more complex example, which demonstrates how programs in Wirewright can observe and react to the evolution of their parts. We'll use the basic oscillator example we've looked at above:

```wwml
(circuit @subcircuit
  (cell @x 0)
  (cell @y)
  (feed @x @y @x))
```

If you run this, nothing changes except we now have the surrounding `circuit`:

```wwml
;; Frame 1
(circuit @subcircuit
  (cell @x)
  (cell @y 0)
  (feed @x @y @x))

;; Frame 2
(circuit @subcircuit
  (cell @x 0)
  (cell @y)
  (feed @x @y @x))

;; ...
```

`circuit` works like a `cell`; the difference is that it treats whatever is inside it as *nodes*, therefore, allowing them to evolve. Nodes inside `circuit` are completely isolated from the outside. Imagine `circuit` as a kind of "one-way glass dome" around the nodes it contains. You can see through the glass but the nodes inside it cannot; nor can they affect anything outside, from the inside. Importantly, anything on the outside can poke and change things in the "glass dome".

Let's use the `view` node, which maintains at the destination cell a projection of the term at the source cell. Its shape is similar to `transfer`, but it doesn't consume anything; it's just looking and making sure everything is in sync.

```wwml
(circuit @subcircuit
  (cell @x 0)
  (cell @y)
  (feed @x @y @x))

(view (@subcircuit (_ (cell _ ±n) _) @value)
  ^n)

(cell @value)
```

Let's see what the M1 pattern says first, though: `(_ (cell _ ±n) _)`. This pattern can be constructed using the technique of "progressive refinement", which you should imagine to be a bit like a "symbolic microscope".

Let's start with `_`. Most things with underscores in M1 patterns are so-called *blanks*; this particular treatment of underscores is heavily inspired by Wolfram Language, although in M1, it's somewhat more restricted. `_` is known as a nameless, typeless blank. It means "match absolutely anything". You can also read it simply as, "I don't care", or "irrelevant".

That's good, but we want to be a bit more specific: we want to match a *dictionary* that has three items. So let's "zoom in": `(_ _ _)`. Now, that's still too loose; we want the second item to be an occupied cell! Let's zoom into that similarly: `(_ (cell _ _) _)`. Since we want to capture the cell's value, let's give it a name, turning our blank from a nameless, typeless blank to a named, typeless blank: `(_ (cell _ n_) _)`. But what if *n* isn't a number? We aren't interested in such *n*s. So let's be even more specific, and say *n* must be a number: `±n` instead of simply `n_`. The resulting pattern is: `(_ (cell _ ±n) _)`, which is the pattern you see in the `view`.

If you run the program above, you'll see:

```wwml
;; Frame 1
(circuit @subcircuit
  (cell @x)
  (cell @y 0)
  (feed @x @y @x))
(view (@subcircuit (_ (cell _ ±n) _) @value)
  ^n)
(cell @value)

;; Frame 2
(circuit @subcircuit
  (cell @x 0)
  (cell @y)
  (feed @x @y @x))
(view (@subcircuit (_ (cell _ ±n) _) @value)
  ^n)
(cell @value 0)

;; Frame 3
(circuit @subcircuit
  (cell @x)
  (cell @y 0)
  (feed @x @y @x))
(view (@subcircuit (_ (cell _ ±n) _) @value)
  ^n)
(cell @value)

;; ... and so on.
```

Notice something important. `circuit` is inert when `view` looks at it. In the seed, `view` looks at the `circuit`, notices the second cell is empty, and erases its destination cell `@value`. Then, *in the same frame*, time is advanced for nodes inside `circuit`, which causes `0` to "fall" into `@y`. This produces the first frame; `irack` prints it in the terminal.

Next, `view` wakes up, sees the second cell contains a number, captures it under `n`, expands the template, and populates the destination cell. Then, in the same frame, time is advanced for nodes inside `circuit`, which makes `0` go back to `@x`. This produces the second frame; `irack` prints it in the terminal.

That's why you see a "lag" of one frame: why our observation lags behind the actual `circuit`. This is intended, although here it may seem slightly unintuitive. The reason is that Wirewright's D7 (roughly speaking, the thing responsible for evolving the circuit) evolves circuits *top-down*. Within a single frame, it lets the outer circuit *observe* and *perturb* subcircuits; and then, in breadth-first manner, proceeds to evolve those subcircuits. When D7 reaches the leaves, the frame ends. As an aside, the philosophical reasoning behind this choice of top-down (outside-in) over bottom-up (inside-out) evolution is a bit more complex, however. I'm not sure it makes sense to write about it here, and, to be honest, I barely remember why I made this particular choice. I just know it's necessary.

This lag is just one thing to keep in mind. In a sense, it's a fundamental truth of observation: you're always behind what you're observing, unless you can predict it (and indeed, this is the way to solve such "bugs", although here I won't attempt to do that since this is just a basic tutorial).

This shows *observation*, but what about *perturbation*? That is, how can an observer modify the content of the subcircuit?

Let's use the simpler `@x-@y` feed from above as our subcircuit.

```wwml
(circuit @subcircuit
  (cell @x 0)
  (cell @y)
  (feed @x @y))
```

This just causes `0` to fall:

```wwml
;; Frame 1
(circuit @subcircuit
  (cell @x)
  (cell @y 0)
  (feed @x @y))
```

Now, let's add a *backsystem*. But before that, let's see what a backsystem is.

A backsystem is a concept from M1, the pattern matching subsystem which we met a few examples above. A backsystem is a way to make a particular style of rewriting simpler. Consider the following:

```wwml
(swap a_ b_) => (swap ^b ^a)
```

This defines a *rewrite rule*. On the left you see the M1 pattern `(swap a_ b_)`, and on the right, the Alloy template `(swap ^b ^a)`. This rule *fires* when a term matching `(swap a_ b_)` is found.

Let's use the refinement technique from above to understand how this pattern is constructed. We start from `_`. That's too loose, we want to match a *dictionary* with three *items*: `(_ _ _)`. The first of them must be the *symbol* `swap`: `(swap _ _)`. We want to capture the remaining items to refer to them in the template, so let's give them names: `(swap a_ b_)`.

The template simply swaps the items, producing a new dictionary.

For example, if we have the dictionary `(swap 1 2)`, it matches the pattern and produces two captures: `{a: 1, b: 2}`. They are used in the template, producing `(swap 2 1)`. So far so good. Let's make our rule a bit more complex. Let's ignore what we currently call `b` and instead "zoom into" `a`, and swap things there.

```wwml
(swap (a_ b_) _) => ;; ... But wait!
```

As you can see, we can't just do that. We have to go through the tedious labor of capturing everything and then reconstructing the original term:

```wwml
(swap (a_ b_) c_) => (swap (^b ^a) ^c)
```

This is starting to look tedious. And it is! I'm giving very contrived examples here. In practice, some patterns -- most patterns! -- are extremely hard to reconstruct this way. Let's see how a *backmap* rule for this `swap` would look like to get an intuitive idea of what backmaps do:

```wwml
(swap (a_ b_) _) <> {a: ^b, b: ^a}
```

Now, consider the pattern `⟨±n⟩°`. It might look cryptic to you due to the weird brackets and the degree sign. You're familiar with `±n` already, though; it means "match a number and call that *n*". What does `⟨_⟩°` mean, then?

The short answer is, the brackets `⟨⟩` mean *find item*. `°` is a *postfix* which reads as *source*. The whole thing, `⟨⟩°`, reads as *find item source*, that is, *(find item) source*. The whole pattern turns into a *source* driven by *find item*. *Sources*, or *source patterns*, produce zero or more matches, eagerly and exhaustively, rather than just zero or one match.

Summing up the above, `⟨±n⟩` reads as *find first number item and capture under n*. For example, if you match `(1 2 3 4)`, you'll get `{n: 1}`; and in `(a b 100 200 c 300)`, you'll get `{n: 100}`.

Then, `⟨±n⟩°` makes the pattern into a source of such matches. For `(1 2 3 4)` , you'll get four matches: `{n: 1} {n: 2} {n: 3} {n: 4}`. For `(a b c 100 200 c 300)`, you'll get `{n: 100} {n: 200} {n: 300}`.

Now that you know what `⟨±n⟩°` means, let's consider a rewrite rule which features it. For example, let's say you want to increment all number items this way, and then wrap the resulting dictionary in `(done _)`.

```wwml
(increment ⟨±n⟩°) => ;; ???
```

It's quite hard to do this. In fact, we are forced to abandon the idea of using `⟨⟩°` altogether:

```wwml
(increment ns_dict)
  => (done
       (increment
         (^each (ns as ±n)
           ^(+ n 1))))
```

In some sense, the common theme with these patterns and our struggling with them is that we're forced to rebuild a house, from scratch, manually, when all we wanted is just to hang a painting in one of its rooms.

Now to backmaps. I came up with backmaps to combat this verbosity.  Backmaps cooperate very tightly with M1 pattern matching; so much so they are part of M1. Therefore, they know where all *n*s are (in our example). So the same rewrite can be expressed using a backmap:

```wwml
toplevel←(increment ⟨±n⟩°)
  <> {toplevel: (done ^(up toplevel)), n: ^(+ n 1)}
```

It's useful to think of backmaps as a way to do in-place replacement. However, it is extremely important to understand that backmaps are not *mutating* anything. A backmap is, conceptually, a shorthand for rewrites `=>` like the one above. They do the "tearing down" and "reassembly" of immutable structure for you, which, as I demonstrated above, is the boring & tedious part. Everything stays immutable throughout the whole process; the rewritten term is a completely new term (minus structural sharing).

If we simplify our task a little bit and say we just want to increment all numbers in a list, a backmap would do that in just about twenty characters: `⟨±n⟩° <> {n: ^(+ n 1)}`. This rule reads as, for each number item in a dictionary, increment it. If we rewrite `(a b 100 200 c 300)` with this, we get: `(a b 101 201 c 301)`.

The left-hand side of a backmap is the backmap's M1 *pattern*. The right-hand side is called a *backspec* (short for *backmap specification*). It specifies which captures must be replaced, and provides Alloy templates for what they should be replaced with. In each replacement template, all captures are available.

Moreover, in each replacement template, in Nitrene expressions, you can use `(up capture_)`  and `(dn capture_)`. The former, e.g. `(up foobar)`, you can think of as standing for *up-to-date*, but what it really means is, "look from above", i.e., the point of view is above the capture that is being replaced -- *up*, from that capture's point of view. Symmetrically for `dn`. The understanding of `up` and `dn` is not really necessary in the first tutorial, though, so let's leave them for later. You should be able to recognize them now, however, when you see them.

Now, I promised no theory, but all of this is theory, right? Not really. I know I was talking about *backsystems*; I haven't lost track of that. So let's define a *backsystem*, which is a *system of backmaps*, now that you're acquainted with backmaps; the distinctive feature of the latter being the `_ <> _` syntax. Backsystems can be defined in the circuit using the `backsys` node.

```wwml
(cell @x (a b 100 200 c 300))
(backsys @x
  ⟨±n⟩° <> {n: ^(+ n 1)})
```

If you run this, you'll see:

```wwml
;; Frame 1
(cell @x (a b 101 201 c 301))
(backsys @x
  ⟨±n⟩° <> {n: ^(+ n 1)})

;; Frame 2
(cell @x (a b 102 202 c 302))
(backsys @x
  ⟨±n⟩° <> {n: ^(+ n 1)})

;; Frame 3
(cell @x (a b 103 203 c 303))
(backsys @x
  ⟨±n⟩° <> {n: ^(+ n 1)})

;; ... and so on.
```

As you can see, the thing actually works. Feel free to plug in the other backmap(s) and see how the behavior of the circuit changes. Play with the value in the cell, too.

What's the point of the word *system* in *backsystem*, though? Well, consider:

```wwml
(cell @x (100 200))
(backsys @x
  (±n _) <> {n: ^(+ n 1)}
  (_ ±n) <> {n: ^(+ n 1)})
```

Here, two backmaps cooperate to increment the numbers. In this particular case they do not interfere with each other's progress; and I don't want to make up complex examples where they do; I don't want this to get even more confusing. Nevertheless, the two backmaps here form what I call a *rule system*; whose "systemicity", or "unity", is clearly visible if we imagine ourselves "living" inside `@x`. For us there, the numbers would just increment in concert. The observed effect (behavior) is indivisible even though it is produced by a collection of entities; I don't know about you, but for me, that surely looks very similar to a *system* -- the entities being rules (backmaps). So, *backsystem*.

```wwml
;; Frame 1
(cell @x (101 201))
(backsys @x
  (±n _) <> {n: ^(+ n 1)}
  (_ ±n) <> {n: ^(+ n 1)})

;; Frame 2
(cell @x (102 202))
(backsys @x
  (±n _) <> {n: ^(+ n 1)}
  (_ ±n) <> {n: ^(+ n 1)})

;; ... and so on.
```

Now, let's unwind all the way back to the observer example. Now that you've seen a `backsys` modify a `cell`, why shouldn't it be a `circuit` instead of `cell`, really?

```wwml
(circuit @subcircuit
  (cell @x 0)
  (cell @y)
  (feed @x @y))

(backsys @subcircuit
  ((cell _ `dst) (cell _ ±n) _)
    <> {dst: ^(+ n 1), (n): ()})
```

In the pattern here, `` `dst `` identifies a spot where to "plug something in" (an insertion point, so to speak).  For example, ``(foo `x) <> {x: 100}`` will match `(foo)` (and **not** e.g. `(foo abc)`); and  rewrite it, *inserting* `100` at the designated spot, so it becomes `(foo 100)`.

`(n): ()` in the backspec means *delete* *n*. So for example, the backmap `(foo x_) <> {(x): ()}` rewrites `(foo "hello")` to `(foo)` -- it deletes *x*.

If you run this, you'll see the following (I'll omit `backsys` because it stays the same all throughout):

```wwml
;; Frame 1
(circuit @subcircuit
  (cell @x)
  (cell @y 1)
  (feed @x @y))

;; Frame 2
(circuit @subcircuit
  (cell @x)
  (cell @y 2)
  (feed @x @y))

;; Frame 3
(circuit @subcircuit
  (cell @x)
  (cell @y 3)
  (feed @x @y))

;; ... and so on.
```

What's going on here? Why don't we see the "fall"?

First, you have to keep in mind the "observation lag" from above. I think it'd be easier to see what happens here and why we don't see the "fall" if I split frames into *subframes* so that the `circuit` is seen evolving separately from its container.
You can do the same using the `--subframes` flag in `irack`; run the example above like so: `irack --single-step --subframes /tmp/seed.wwml`.

```wwml
;; Subframe 1
(circuit @subcircuit
  (cell @x)
  (cell @y 0)
  (feed @x @y))

;; FRAME 1 complete. The above is printed.
;; ----------------

;; Backsystem notices the second cell is 0. In one swift move, it grabs
;; the zero and puts it into `@x`, incrementing it. This produces
;; Subframe 3.

;; Subframe 2
(circuit @subcircuit
  (cell @x 1)
  (cell @y)
  (feed @x @y))

;; Subframe 3
(circuit @subcircuit
  (cell @x)
  (cell @y 1)
  (feed @x @y))

;; FRAME 2 complete. The above is printed.
;; ----------------

;; Again, it's the backsystem's turn now.

;; Subframe 4.
(circuit @subcircuit
  (cell @x 2)
  (cell @y)
  (feed @x @y))
  
;; Subframe 5.
(circuit @subcircuit
  (cell @x)
  (cell @y 2)
  (feed @x @y))
  
;; FRAME 3 complete. The above is printed.
;; ----------------

;; ... and so on.
```

Since time runs outside-in, we simply can't observe the incremented value fall. We (humans) are at the outermost level; `backsys` is one level down, but it, too, can't catch the `circuit` in the interim, because it is evaluated already when it's the backsystem's move.

It might seem I'm only showing examples where this top-down order seems highly unintuitive. Fair point. However, consider:

```wwml
(circuit @subcircuit
  (cell @x 0)
  (cell @y)
  (feed @x @y @x))

(backsys @subcircuit
  (_ (cell _ ±n) _) <> {n: ^(+ n 1)})
```

If you run this (again, I'm omitting `backsys` for brevity):

```wwml
;; Frame 1
(circuit @subcircuit
  (cell @x)
  (cell @y 0)
  (feed @x @y @x))

;; Frame 2
(circuit @subcircuit
  (cell @x 1) ;; `backsys` incremented, then `feed` moved!
  (cell @y)
  (feed @x @y @x))

;; Frame 3
(circuit @subcircuit
  (cell @x)
  (cell @y 1)
  (feed @x @y @x))

;; Frame 4
(circuit @subcircuit
  (cell @x 2) ;; ditto
  (cell @y)
  (feed @x @y @x))

;; ... and so on.
```

Here, the choice of outside-in for time makes sense; we allow the container to observe & modify the contained, and then, give reigns to the contained; it is its turn to "relax" now, as in, reach or search for a relaxed state, given the perturbations by its container (if any).

Outside-in is therefore important, because relaxation is naturally scheduled when a subcircuit is (or could be) damaged; and observation always happens when a subcircuit is relaxed, or was at least given a chance to relax. The container is preferred and given reign over the contained; the contained responds to the container, and not the other way.
