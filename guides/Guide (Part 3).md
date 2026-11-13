In this part of the guide we will cover some nodes that let you perform basic computations.

## `transfer`

The first such node is `transfer`. In `seed.wwml`, write the following:

```wwml
(cell @in 0)
(transfer (@in n_ @out) ^n)
(cell @out)
```

Running this produces:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in 0)
(transfer (@in n_ @out) ^n)
(cell @out)
<Press Enter>

# Frame 1
(cell @in)
(transfer (@in n_ @out) ^n)
(cell @out 0)
<Press Enter>

$
```

The `transfer` node, like the `feed` node, moves a term from one place (here, the cell at `@in`) to another (here, the cell at `@out`). At that point the likeness ends, though. The transfer node consists of several "pieces":

```text
                        template
                         v
(transfer (@in n_ @out) ^n)
          -------------
               spec
```

The transfer node transfers terms according to its spec. The spec provides a source place and a destination place (here, `@in` and `@out`, correspondingly). The spec also provides a *pattern*.

```text
               pattern
               --
(transfer (@in n_ @out) ^n)
           ---    ----
          input  output
```

Patterns in Wirewright are just terms. As you might remember, everything in Wirewright is terms, so that should come off with no surprise. In Wirewright, patterns are interpreted by a pattern matching engine called M1. In our case, the `transfer` node is responsible for showing the pattern to M1. (M1 is short for match one. If you're wondering, yes, there's M0 -- match zero -- which is used to implement M1. Believe it or not, M1 must also match terms -- *pattern* terms -- to do its job properly; hence there's the need for M0.)

Now, the pattern in our example is `n_`. It's a symbol term. Its distinctive feature is that it ends with an underscore. The underscore tells M1 that the symbol is a *blank*. A blank like the one we have here, `n_`, matches any term. It then *captures* the term under the specified name. In our case, the name is `n`. It can be any other name: `name_`, `foo_`, `not-a-very-long-name_`, etc. Capturing is somewhat similar to capturing you might be familiar with from regular expressions. Other languages call it *binding* (think binding variables during destructuring) .

So what we have in our example is a pattern that matches any term, then captures it under the name `n`. The captures made by the pattern are then passed to the `transfer`'s *template*.

Our template is `^n`. The template is, too, a term. Here it is, too, a symbol term. Except this time, the `transfer` node shows it to a different component of Wirewright, a *structural templating engine* called Alloy. "Structural templating" may sound overly fancy. But in practice we see it with, say, JSX, or Lisp `quote`/`unquote`/`quasiquote`. It's basically interpolation for trees. The other end of the "spectrum" of templating is string templating, which we see in PHP, Handlebars, and similar. String templating is basically string interpolation.

Templates are normally treated as literals. If you wrote `(+ 1 2)` instead of `^n` in the template section of `transfer`, it'd just output `(+ 1 2)` as-is:

```wwml
;; Frame 0 (seed)
(cell @in 0)
(transfer (@in n_ @out) (+ 1 2))
(cell @out)

;; Frame 1
(cell @in)
(transfer (@in n_ @out) (+ 1 2))
(cell @out (+ 1 2))
```

The peculiar hat symbol, `^`, also known as the caret, tells Alloy to instead "plug" the value of `n`. `^` was picked for this purpose because of its mnemonic effect: it almost screams, "put *that* here". For instance, with `^n`, you can imagine the hat telling, put `n` here. If you had `(double ^n)`, see how the hat "points" where `n` should be placed? That's the idea:

```wwml
;; Frame 0 (seed)
(cell @in 0)
(transfer (@in n_ @out) (double ^n))
(cell @out)

;; Frame 1
(cell @in)
(transfer (@in n_ @out) (double ^n))
(cell @out (double 0))
```

To summarize, the transfer node uses M1, a pattern matching engine, to determine what values are captured. It then uses Alloy, a structural templating engine, which determines how those values are incorporated into the output term.

The `transfer` node in our original example is essentially the identity function. Let's make it a bit more interesting. In `seed.wwml`, write the following:

```wwml
(cell @in 4)
(transfer (@in n_number @out) ^(* n n))
(cell @out)
```

The result looks like this:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in 4)
(transfer (@in n_number @out) ^(* n n))
(cell @out)
<Press Enter>

# Frame 1
(cell @in)
(transfer (@in n_number @out) ^(* n n))
(cell @out 16)
<Press Enter>

$
```

While `^n` tells Alloy to plug the value of *n*, as captured by the pattern, `^(...)` delegates the enclosed expression to Nitrene for evaluation. 

You're probably tired of the wonky names already, but bear with me. Nitrene is an *expression language*. Excel's formulas are an expression language. So now you roughly understand what's the idea behind Nitrene. It's a "calculator" kind of thing: a purely functional language that can do the sums, products, mins, and maxes for you, and quite a lot of other things — but currently we're only interested in the sums. It's a language in the Lisp sense of the word: it sits on top of the Lisp syntax (here, though, it's WwML syntax, which at times deviates quite a bit from Lisp/S-expressions).

So what goes inside `^(...)` is a Nitrene expression. In our case the expression is `(* n n)`, which raises the `n` we've captured to the power of 2. There's a more idiomatic way to express this in Nitrene, using the power function `**`: `(** n 2)`. Try this yourself if you want to, maybe with different powers.

So we have a... neat situation here. There's the transfer node. The transfer node has a pattern. The pattern looks at the transfer's source cell. If there's a match, the pattern makes captures. The captures go to the Alloy template. Our Alloy template is basically nonexistent: `^(* n n)`. We immediately jump into Nitrene: `(* n n)`. Nitrene evaluates the expression (by the way, Nitrene, too, has access to all captures made by the pattern — just as Alloy does). The result is plugged back into the template, which again is nonexistent in our case. Finally, the source cell is cleared, and the final term — the *instance* of the `transfer`'s template — is sent to the destination cell.

The pattern has changed a little from the first example we've looked at. Previously we had `n_` and now we have `n_number`. It is possible to put the *type*  of the term you want the blank to match after the trailing underscore. Remember the six types of terms? Here they are: `number`, `string`, `symbol`, `boolean`, `blob`, `dict`. So you can write these types after the underscore. For example: `name_string`, `age_number`, `valid_boolean`, `info_dict`, `payload_blob`.

The two types of blanks we've looked at are called *typed* and *untyped* blanks. A blank like `n_` is an untyped blank. A blank like `age_number` is a typed blank.

There's also the distinction between *named* and *anonymous* blanks. A blank like `_` is anonymous and untyped. A blank like `_number` is also anonymous, but now it is typed. A blank like `n_` is named but untyped. A blank like `valid_boolean` is both named and typed.

Anonymous blanks are useful when you want to skip a term conceptually: when you basically want to say, "I don't care", with different degrees of precision (as in, is there perhaps a type to your "I don't care", or do you completely not care?).

For example, in the pattern `(person _ age_number)` (assume the first blank denotes the person's name), we're making sure the first term — the first item — is the symbol `person`; then we're skipping the second term (we really don't care what it is); and finally we're capturing the person's age with a named, typed blank.

Anonymous typed blanks are useful when you want to skip a term, but must make sure it's of the correct type: `(person _string age_number)`. In other words, it's a bit more precise kind of "I don't care".

Let's test a similar pattern using `irack`. In `seed.wwml`, write:

```wwml
(cell @in (person "Jane" 26))
(transfer (@in (person name_string age_number) @out)
  ^(~ "Hi, " name ". You are " age " years old"))
(cell @out)
```

Here's what happens:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in (person "Jane" 26))
(transfer (@in (person name_string age_number) @out)
  ^(~ "Hi, " name ". You are " age " years old"))
(cell @out)
<Press Enter>

# Frame 1
(cell @in)
(transfer (@in (person name_string age_number) @out)
  ^(~ "Hi, " name ". You are " age " years old"))
(cell @out "Hi, Jane. You are 26 years old")
<Press Enter>

$
```

Neat! Though the string concatenation expression, `~`, is quite clumsy. Thankfully, WwML provides a useful shorthand syntax for exactly this kind of thing — the string interpolation syntax. Using this syntax, our example can be rewritten like so:

```wwml
(cell @in (person "Jane" 26))
(transfer (@in (person name_string age_number) @out)
  ^"Hi, ⸢name⸣. You are ⸢age⸣ years old")
(cell @out)
```

Now it's far easier to read. Don't be scared by the brackets. It's possible to type them effortlessly using the Compose key (if you're on Linux). I'm the living proof of that: I don't even think about these things anymore, the `⸢`s and `⟨`s and `⟪`s. The compactness is a big win for me, far bigger than the hurdle of learning to type these bizarre APL-like or math characters. See `guides/wwml.md` for the mapping / `.XCompose` config.

The interpolation syntax above is a shorthand. The WwML source  `"Hi, ⸢name⸣. You are ⸢age⸣ years old"` *expands* into `(~ "Hi, " name ". You are " age " years old")`. We then prefix the whole thing with the caret to get, effectively, `^(~ "Hi, " name ". You are " age " years old")` — the expression we've started with!

What happens when the pattern doesn't match? *Nothing*. Let's see. In `seed.wwml`, write:

```wwml
(cell @in not-a-person)
(transfer (@in (person name_string age_number) @out)
  ^"Hi, ⸢name⸣. You are ⸢age⸣ years old")
(cell @out)
```

Executing this gives us:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in not-a-person)
(transfer (@in (person name_string age_number) @out)
  ^(~ "Hi, " name ". You are " age " years old"))
(cell @out)
<Press Enter>

$
```

Immediately, we get quiescence. There's nothing for `transfer` to do, and there's nothing else in the circuit.

Now let's have a couple of transfers looking at the same cell:

```wwml
(cell @in 4)
(transfer (@in x_number @out) ^(+ x 1))
(transfer (@in x_string @out) ^"Hello, ⸢x⸣")
(cell @out)
```

The name doesn't have to be the same (I'm talking about `x` here). I'm using the same generic `x` just for the aesthetics.

This runs like so:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in 4)
(transfer (@in x_number @out) ^(+ x 1))
(transfer (@in x_string @out) ^"Hello, ⸢x⸣")
(cell @out)
<Press Enter>

# Frame 1
(cell @in)
(transfer (@in x_number @out) ^(+ x 1))
(transfer (@in x_string @out) ^"Hello, ⸢x⸣")
(cell @out 5)
<Press Enter>

$
```

As an exercise, try putting a string in `@in` instead, and see what the thing ends up doing. The important point is that the transfer that matches the source cell gets to move it to the destination cell.

In our example, all transfers feed into the same destination cell. We can instead try a different example, where each transfer has its own, unique destination cell:

```wwml
(cell @in 4)
(transfer (@in x_number @out-number) ^(+ x 1))
(transfer (@in x_string @out-string) ^"Hello, ⸢x⸣")
(cell @out-number)
(cell @out-string)
```

The output is as follows:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in 4)
(transfer (@in x_number @out-number) ^(+ x 1))
(transfer (@in x_string @out-string) ^"Hello, ⸢x⸣")
(cell @out-number)
(cell @out-string)
<Press Enter>

# Frame 1
(cell @in)
(transfer (@in x_number @out-number) ^(+ x 1))
(transfer (@in x_string @out-string) ^"Hello, ⸢x⸣")
(cell @out-number 5)
(cell @out-string)
<Press Enter>

$
```

Again, as an exercise, try to replace `4` with any string you like, and observe what happens.

What happens when two transfers are looking at the same source cell, and have overlapping patterns (meaning both of them match whatever is in the source cell)? Things get interesting, that's what happens.

When two or more transfers have overlapping patterns, they will at least agree that the source cell should be empty. So there's no conflict there. However, a conflict may arise on the destination cell, depending on its *merge policy* (which I've briefly covered before). Importantly, the conflict *won't* arise if both transfers end up producing the same thing, and writing it to the destination cell. But if they attempt to write two different things to the same destination cell, it's up to the destination cell to handle that.

Normally, and in our examples, the destination cell has the *exclusive* merge policy. This means it will reject writes to itself when there are more than one of them. This in turn rejects the transactions of both transfers, involving the clearing of the source cell, and the setting of the destination cell. If the cell has a different merge policy, though, things will be merged (or not merged!) according to that policy.

So if both transfers end up producing the same thing, there's no conflict:

```wwml
(cell @in 2)
(transfer (@in x_number @out) ^(* x 2))
(transfer (@in x_number @out) ^(* x x))
(cell @out)
```

Stepping the circuit produces:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in 2)
(transfer (@in x_number @out) ^(* x 2))
(transfer (@in x_number @out) ^(* x x))
(cell @out)
<Press Enter>

# Frame 1
(cell @in)
(transfer (@in x_number @out) ^(* x 2))
(transfer (@in x_number @out) ^(* x x))
(cell @out 4)
<Press Enter>

$
```

If they were to produce different things (say, by replacing `2` with`4`), we'd get a conflict, and so, immediate quiescence:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in 4)
(transfer (@in x_number @out) ^(* x 2)) ;; this proposes 8
(transfer (@in x_number @out) ^(* x x)) ;; this proposes 16
(cell @out)
<Press Enter>

$
```

With a different merge policy, let's say, `seq`, which picks the minimum among competing terms:

```wwml
(cell @in 4)
(transfer (@in x_number @out) ^(* x 2))
(transfer (@in x_number @out) ^(* x x))
(cell (@out seq))
```

We get this:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in 4)
(transfer (@in x_number @out) ^(* x 2)) ;; this proposes 8
(transfer (@in x_number @out) ^(* x x)) ;; this proposes 16
(cell (@out seq))
<Press Enter>

# Frame 1
(cell @in)
(transfer (@in x_number @out) ^(* x 2))
(transfer (@in x_number @out) ^(* x x))
(cell (@out seq) 8)
<Press Enter>

$
```

In the above, `8` is the minimum proposed term. The patch it is part of (i.e., the patch that was proposed by the first `transfer` node) is accepted. The other patch is rejected.

`transfer` can accept two or more sources. In `seed.wwml`, write:

```wwml
(cell @a 2)
(cell @b 3)
(transfer ((@a @b) (a_number b_number) @sum) ^(+ a b))
(cell @sum)
```

Note that there's a handy shorthand for a named, number-typed blank, e.g., `a_number` — `±a`. I'm going to use it from now on since it's in my muscle memory. We can rewrite `seed.wwml` like so:

```wwml
(cell @a 2)
(cell @b 3)
(transfer ((@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum)
```

The circuit evolves as follows:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @a 2)
(cell @b 3)
(transfer ((@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum)
<Press Enter>

# Frame 1
(cell @a)
(cell @b)
(transfer ((@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum 5)
<Press Enter>

$
```

Like `feed`, `transfer` can accept one or more *inhibitors* as well:

```wwml
(cell @busy true)
(cell @a 2)
(cell @b 3)
(transfer ((not @busy) (@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum)
```

Here, the nonempty `@busy` cell acts as an inhibitor for `transfer`, causing it to be inactive — and thus, the entire circuit reaches quiescence.

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @busy true)
(cell @a 2)
(cell @b 3)
(transfer ((not @busy) (@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum)
<Press Enter>

$
```

Let's empty `@busy` automatically using the `discard` node, which I've shown in the previous part of the guide.

```wwml
(cell @busy true)
(discard @busy)
(cell @a 2)
(cell @b 3)
(transfer ((not @busy) (@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum)
```

This runs like so:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @busy true)
(discard @busy)
(cell @a 2)
(cell @b 3)
(transfer ((not @busy) (@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum)
<Press Enter>

# Frame 1
(cell @busy)
(discard @busy)
(cell @a 2)
(cell @b 3)
(transfer ((not @busy) (@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum)
<Press Enter>

# Frame 2
(cell @busy)
(discard @busy)
(cell @a)
(cell @b)
(transfer ((not @busy) (@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum 5)
<Press Enter>

$
```

In Frame 1, the `discard` node emptied the `@busy` cell, so in Frame 2, `transfer` was able to run and calculate the `@sum`.

Let's finish our review of the `transfer` node (for the time being) on a *fun* note. Let's create a chain of transfers that operates in a feedback loop:

```wwml
(cell @record (person "Jane" 26))
(transfer (@record (person name_string _number) @name) ^name)
(transfer (@record (person _string ±age) @age) ^age)

(cell @name)
(cell @age)
(cell @next-age)
(transfer (@age ±age @next-age)
  ^(+ age 1))

(transfer ((@name @next-age) (name_string ±age) @record)
  (person ^name ^age))
```

As an exercise, run this with `irack --single-step` and see what happens.

## `view`

The `view` node is basically the same as `transfer`, except it doesn't consume the term in the source cell, and it doesn't require the destination cell to be empty.

Whereas `transfer` moves a term from one cell to another, `view` maintains a view — though with the necessary lag of one frame — of the source cell, in the destination cell.

In `seed.wwml`, write:

```wwml
(cell @in 100)
(view (@in ±n @out) ^(+ n 1))
(cell @out)
```

Stepping the circuit produces:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in 100)
(view (@in ±n @out) ^(+ n 1))
(cell @out)
<Press Enter>

# Frame 1
(cell @in 100)
(view (@in ±n @out) ^(+ n 1))
(cell @out 101)
<Press Enter>

$
```

Notice how the original input stays unchanged. The `@out` cell could have contained anything at all. The view node would simply write over that.

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @in 100)
(view (@in ±n @out) ^(+ n 1))
(cell @out "Hello World")
<Press Enter>

# Frame 1
(cell @in 100)
(view (@in ±n @out) ^(+ n 1))
(cell @out 101)
<Press Enter>

$
```

In the session above, I put the string `"Helo World"` in the output cell. Notice how in Frame 1, `view` wrote `101` over the string.

Unlike `transfer`, which requires its source and destination cells to be different, `view` allows you to specify the same cell as both the source and the destination.

```wwml
(cell @count 0)
(view (@count ±n @count) ^(+ n 1))
```

Running the example above, we get:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @count 0)
(view (@count ±n @count) ^(+ n 1))
<Press Enter>

# Frame 1
(cell @count 1)
(view (@count ±n @count) ^(+ n 1))
<Press Enter>

# Frame 2
(cell @count 2)
(view (@count ±n @count) ^(+ n 1))
<Ctrl-C>

$
```

The counter will increment indefinitely, until we interrupt `irack` with Ctrl-C.

To demonstrate that `view` is indeed a live view, let's make a "view" of our counter.  In `seed.wwml`, write:

```wwml
(cell @count 0)
(view (@count ±n @count) ^(+ n 1))
(view (@count ±n @view) ^"Count is: ⸢n⸣")
(cell @view)
```

Here's the resulting execution:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @count 0)
(view (@count ±n @count) ^(+ n 1))
(view (@count ±n @view) ^"Count is: ⸢n⸣")
(cell @view)
<Press Enter>

# Frame 1
(cell @count 1)
(view (@count ±n @count) ^(+ n 1))
(view (@count ±n @view) ^"Count is: ⸢n⸣")
(cell @view "Count is: 0")
<Press Enter>

# Frame 2
(cell @count 2)
(view (@count ±n @count) ^(+ n 1))
(view (@count ±n @view) ^"Count is: ⸢n⸣")
(cell @view "Count is: 1")
<Press Enter>

# Frame 3
(cell @count 3)
(view (@count ±n @count) ^(+ n 1))
(view (@count ±n @view) ^"Count is: ⸢n⸣")
(cell @view "Count is: 2")
<Ctrl-C>

$
```

Note the curious thing: `view` lags by one frame. Conceptually, it "costs" one frame to calculate the view. So if the thing you're making a view of — the source cell's content — changes *as fast*, that is, once per frame, then there's basically no way for `view` to catch up. That's just what is happening in the circuit above: the counter always outruns the view by one frame, and the view is perpetually lagging behind by one frame. If the counter changed less frequently, say, every couple of frames, then the view would show an in-sync value most of the time.

Just like `transfer`, `view` can take two or more source edges:

```wwml
(cell @a 2)
(cell @b 3)
(view ((@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum)
```

Evolving this, we get:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @a 2)
(cell @b 3)
(view ((@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum)
<Press Enter>

# Frame 1
(cell @a 2)
(cell @b 3)
(view ((@a @b) (±a ±b) @sum) ^(+ a b))
(cell @sum 5)
<Press Enter>

$
```

## More variants of `feed`

There's a couple more variants of feed that are useful to know. The first one is the `copy` variant of feed. It *copies* the value in the source cell instead of *moving* it to the destination.

In `seed.wwml`, write:

```wwml
(cell @x 100)
(feed (copy @x) @y)
(cell @y)
```

Let's see what happens:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @x 100)
(feed (copy @x) @y)
(cell @y)
<Press Enter>

# Frame 1
(cell @x 100)
(feed (copy @x) @y)
(cell @y 100)
<Press Enter>

$
```

There's really nothing to explain here, assuming you've read prior parts of the guide.

There's also `over` on the destination side. Normally, `feed` requires the source to be nonempty and the destination to be empty. But with an `over` destination, it will write *over* the value there:

```wwml
(cell @x 100)
(feed @x (over @y))
(cell @y)
```

Running this produces:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @x 100)
(feed @x (over @y))
(cell @y)
<Press Enter>

# Frame 1
(cell @x)
(feed @x (over @y))
(cell @y 100)
<Press Enter>

$
```

Now if we put some value in the destination cell, `@y`:

```wwml
(cell @x 100)
(feed @x (over @y))
(cell @y "Hello World")
```

We will get:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @x 100)
(feed @x (over @y))
(cell @y "Hello World")
<Press Enter>

# Frame 1
(cell @x)
(feed @x (over @y))
(cell @y 100)
<Press Enter>

$
```

`copy` and `over` can be combined:

```wwml
(cell @x 100)
(feed (copy @x) (over @y))
(cell @y "Hello World")
```

Observe the resulting trace:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @x 100)
(feed (copy @x) (over @y))
(cell @y "Hello World")
<Press Enter>

# Frame 1
(cell @x 100)
(feed (copy @x) (over @y))
(cell @y 100)
<Press Enter>

$
```