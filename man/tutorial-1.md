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

Why is the gap there? This is because `feed`, as well as all other nodes, look at the *complete previous frame* when they are evaluated. In this sense, the process is the same as in cellular automata. So in Frame 1, `feed` looks at the seed, and sees that while the `@v-@x` "roll-down" is possible, `@u-@v` is not. In Frame 2, `200` is at `@x`, so `@v` is empty. Therefore, the `@u-@v` "roll-down" becomes possible, just like `@x-@y`. Both "roll-downs" occur.

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

Let's see what the M1 pattern says first, though: `(_ (cell _ ±n) _)`. This pattern can be constructed using the technique of "progressive refinement", which you should imagine to be like a "symbolic microscope".

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
