In the `ww-dist` directory:

```bash session
$ ls
doctool  examples  guides  irack  musoma-x86_64.AppImage  mxrack-x86_64.AppImage  README  runtime
```

... there's an AppImage called `mxrack-x86_64.AppImage`.

MxRack is short for **M**edia E**x**tensions for **Rack**. MxRack lets you build interactive applications with Wirewright. For this purpose it introduces several interactive nodes, mainly `window`, `mouse`, `keyboard`, `input`. It also lets you include *visuals* (think visual markup) right in the circuit.

We'll use MxRack in this part of the guide. When you run it it shows its help message:

```bash session
$ ./mxrack-x86_64.AppImage
<MxRack help message>
```

The AppImage works much like `irack`, except it doesn't support `--single-step` yet.

You're expected to give it a file. Optionally, you can provide `--quiet` to suppress printing the circuit at the start of each frame. Additionally, you can provide `--stat` to show frame statistics, though this is more for internal use.

Let's write a basic "Hello World" app using `mxrack`. In `seed.wwml`, write:

```wwml
(window (p "Hello World"))
```

Next, run it with:

```bash session
$ ./mxrack-x86_64.AppImage seed.wwml
```

By the way, feel free to check out the experimental live reloading flag, `--live`. It *mostly* works and will provide a much smoother experience.

```bash session
$ ./mxrack-x86_64.AppImage --live seed.wwml
```

In Wirewright, you very quickly get used to everything being live and responsive (more in the logical sense than in the design sense). And MxRack is not an exception.

You should see a window open, containing a text that says "Hello World".

![Figure 0](<slides/Guide 4/figure0.png>)

> [!NOTE]
> In case you want to close the window, you have to Ctrl-C `mxrack-x86_64.AppImage` in the shell. This is because window closure is something that the circuit has to handle. So MxRack doesn't terminate on window closure. In this part of the guide we won't bother handling window closure properly.

Here, `window` represents an operating-system window. Now, it is a literal symbolic representation of the window. For instance, if you specify a width and a height like so:

```wwml
(window width: 500 height: 400
  (p "Hello World"))
```

And then start resizing the window, you'll see the values change, live.

![Figure 1](<slides/Guide 4/figure1.png>)

Similarly, as an exercise, try adding `form: floating` on the window alongside `width` and `height`. Then try maximizing and minimizing the window, expanding it, and finally closing it; after each interaction, see how `form` on the window evolves to represent the current window state.

`p` stood for paragraph at some point in the past, like in HTML. However, it's got so ubiquitous in practical code, and so *un-semantic*, that it has lost its meaning. The elephant in the room being, too, that it can include many paragraphs, not necessarily one — so *really*, it's not a paragraph node at all.

So at this point the best meaning for `p` is either no meaning — feel free to treat it as a generic text node with a peculiar name; or the meaning of *print*. Pick whichever one you like most if that matters to you. Before you ask, though, `text` is already taken by a lower-level subsystem, and isn't exactly the same thing as `p`. Suffice it to say that `p` "compiles down" to `text`.

Now, obviously, the text is quite hard to see. What we'd like is for the text to be in the center. Its font size should probably be larger. Its color should be a bit brighter as well.

Let's use *Microfold*. Microfold is basically CSS in the "alternate reality" of Wirewright. Except Wirewright embraces fully the approach pioneered by Tailwind: style locality, small, short utilities for specific purposes, and extensibility, and so forth. (Unlike Tailwind, Wirewright lets you construct and change *themes* live; we'll see how this works later on.)

Microfold looks for `style: _` pairs on nodes in your circuit. Nodes that have them participate in the visuals of your app. Some nodes are an exception, that is, they do not require `style: _` to be treated visually. Examples of such nodes include `window` and `p`.

In `seed.wwml`, type:

```wwml
(window
  (p style: "center"
    "Hello World"))
```

Now, with MxRack, what we get is this:

![Figure 2](<slides/Guide 4/figure2.png>)

The `center` *utility* centers the visual node it's on in the parent container.

To make the text larger, we can use the `text-` sizing utilities. Microfold copies many "fundamental" utilities from Tailwind. So you get things like `text-lg`, `text-lg/8`, `text-3xl`, and so on. Similarly, there are `text-` color utilities such as `text-neutral-500` (the palette is, again, copied from Tailwind). 

We can make our text prettier using `font-light` as well. Light fonts tend to look better with very large sizes — but then remember I'm by no means a designer :^)

```wwml
(window
  (p style: "center text-3xl text-neutral-50 font-light"
    "Hello World"))
```

Running this with MxRack, we get:

![Figure 3](<slides/Guide 4/figure3.png>)

Very nice. Next, let's try changing the background color of the window. Perhaps we want a light theme. Let's change the `window`'s background to some kind of light gray, and let's make the text near-black.

The immediate caveat is `window`s are special, in that they want a *backdrop* color rather than a background color. The utility difference is tiny: just use e.g. `backdrop-neutral-950` on a window instead of `bg-neutral-950`.

<details>
<summary>Why does the difference exist?</summary>
The reason the difference exists is quite boring. Windows are resized by the OS without Wirewright in the loop. Simultaneously, the OS wants the window to repaint itself on resize (roughly speaking), so that everything stays smooth. As Wirewright isn't in the loop, this is impossible. The OS won't quite wait for Wirewright — and ultimately your circuit — to do its job of catching up.

So there's the need for the window to *autonomously* repaint itself to the best of its ability, until Wirewright catches up and sends a properly sized frame. The backdrop  color is retained on the window side and is used to fill the window as you're resizing, immediately. Backgrounds, on the other hand, are managed by Microfold and the rest of the graphics stack, so you'd have to wait until the graphics stack gives you a resized frame — leading to ugly black or white or "transparent" strips around window content.

As an exercise, try changing `backdrop` to `bg`. Use some eye-wrecking color. Then resize really quickly. Then go back to backdrop and do the same. Then add both a `backdrop` and a `bg` color to the window. Make them different, and both eye-wrecking. Observe what happens as you resize the window really quickly.
</details>

```wwml
(window style: "backdrop-neutral-50"
  (p style: "center text-3xl text-neutral-950 font-light"
    "Hello World"))
```

Things look more or less as we expect:

![Figure 4](<slides/Guide 4/figure4.png>)

Now let's go back to dark colors so as to not hurt our eyes. Also, let's forget about centering stuff for a moment.

```wwml
(window style: "backdrop-neutral-950"
  (p style: "text-3xl text-neutral-50 font-light"
    "Hello World"))
```

The window should now look like this:

![Figure 5](<slides/Guide 4/figure5.png>)

Much better. Let's say you want more than one text. Maybe you want a list of some sort.

```wwml
(window style: "backdrop-neutral-950"
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Go home"))
```

We're immediately hit with this horrible mess:

![Figure 6](<slides/Guide 4/figure6.png>)

This is because Wirewright defaults to Z-flow on `window`, meaning things stack on top of each other. With Z-flow, you are then expected to apply deltas (here expressed in *spacing units*, a concept also borrowed from Tailwind):

```wwml
(window style: "backdrop-neutral-950"
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "dt-5 text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "dt-10 text-3xl text-neutral-50 font-light"
    "Go home"))
```

Z-flow has the associated utility, `flow-none`. `window` implicitly has this utility. So you should imagine the whole thing to look like:

```text
(window style: "flow-none backdrop-neutral-950"
  ...)
```

![Figure 7](<slides/Guide 4/figure7.png>)

Deltas are a horrible way to go about things in our particular circumstance, so let's get rid of them, and see which other `flow`s exist. One last note about Z-flow (`flow-none`) is that it is normally used for stacking things, centering things on top of other things, and so on. You can also use it for "vector art" (though you should probably use `scenery.svg` instead; see the doctool).

So what other `flow`s exist, besides `flow-none`? Currently, there are four: `flow-row`, `flow-col`, `flow-wrap`, and `flow-y-wrap`. The latter is quite exotic, so I'm not going to cover it. Feel free to experiment with it yourself.

`flow-row` arranges things in a row:

```wwml
(window style: "flow-row backdrop-neutral-950"
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Go home"))
```

![Figure 8](<slides/Guide 4/figure8.png>)

`flow-col` arranges things in a column:

```wwml
(window style: "flow-col backdrop-neutral-950"
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Go home"))
```

![Figure 9](<slides/Guide 4/figure9.png>)

`flow-wrap` wraps things on overflow (`flow-row` does not; experiment by resizing the window in each case).

```wwml
(window style: "flow-wrap backdrop-neutral-950"
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Go home"))
```

![Figure 10](<slides/Guide 4/figure10.png>)

What we're interested in is `flow-col`. We can add a gap between items using the `gap` utility (expressed in spacing units):

```wwml
(window style: "flow-col gap-10 backdrop-neutral-950"
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "text-3xl text-neutral-50 font-light"
    "Go home"))
```

![Figure 11](<slides/Guide 4/figure11.png>)

We can also make items take a fraction of the window's (or more generally, the container's) size, using the `fr` utility. Perhaps it'd be easier to see what's going on if we also have differently-colored backgrounds  on `p`:

```wwml
(window style: "flow-col backdrop-neutral-950"
  (p style: "fr-1 bg-neutral-800 text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "fr-1 bg-neutral-700 text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "fr-1 bg-neutral-600 text-3xl text-neutral-50 font-light"
    "Go home"))
```

![Figure 12](<slides/Guide 4/figure12.png>)

Let's then perhaps change one of those `fr` numbers, one of the `1`s, to see what effect this has. I'm going to make the middle text `fr-2`:

```wwml
(window style: "flow-col backdrop-neutral-950"
  (p style: "fr-1 bg-neutral-800 text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "fr-2 bg-neutral-700 text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "fr-1 bg-neutral-600 text-3xl text-neutral-50 font-light"
    "Go home"))
```

![Figure 13](<slides/Guide 4/figure13.png>)

The result is quite distorted due to a peculiarity in how items claim space. Basically, what they claim may seem excessive. In order to "fix" that, without much explanation, let's make each item `clip` itself on overflow, which "neutralizes" excessive claims:

```wwml
(window style: "flow-col backdrop-neutral-950"
  (p style: "fr-1 clip bg-neutral-800 text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "fr-2 clip bg-neutral-700 text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "fr-1 clip bg-neutral-600 text-3xl text-neutral-50 font-light"
    "Go home"))
```

Note that clipping doesn't stop wrapping and so on (try resizing the window). It just tells the parent container (which handles `fr`), "Hey, I handle overflow myself, no need to reserve extra space to account for my possible overflow."

![Figure 14](<slides/Guide 4/figure14.png>)

The picture is much clearer now: `fr-2` makes the item (or rather, the item's share of space) twice as large as `fr-1`. `fr-3` will make it three times as big as `fr-1`, and so on.

As a sidenote, Microfold allows you to specify styles for immediate children on the parent, using the `item-` prefix. So instead of, say, repeating `clip` three times on each child, we could've said `item-clip` on the parent:

```wwml
(window style: "flow-col item-clip backdrop-neutral-950"
  (p style: "fr-1 bg-neutral-800 text-3xl text-neutral-50 font-light"
    "Buy apples")
  (p style: "fr-2 bg-neutral-700 text-3xl text-neutral-50 font-light"
    "Buy pineapples")
  (p style: "fr-1 bg-neutral-600 text-3xl text-neutral-50 font-light"
    "Go home"))
```

`item-` works for any utility. You can say, for instance, `item-bg-neutral-300`, `item-font-light`, and so forth (though the latter cascades down to texts anyway, without the `item-` prefix — all text utilities do).

Text and font utilities participate in the downward cascade, so we can put them all on the parent as well:

```wwml
(window style: "flow-col backdrop-neutral-950 item-clip text-3xl \
                text-neutral-50 font-light"
  (p "Buy apples" style: "fr-1 bg-neutral-800")
  (p "Buy pineapples" style: "fr-2 bg-neutral-700")
  (p "Go home" style: "fr-1 bg-neutral-600"))
```

Now to grouping. Instead of taking the semantic approach, which is preferred in modern HTML, Wirewright takes the old-school *un-semantic* approach — though instead of `div`s everywhere, Wirewright uses `group`.

A `group` is a construct from Rack. It can be used to group nodes, and is not a visual node on its own. So if you write in your `seed.wwml`:

```wwml
(group
  (cell @x 0)
  (cell @y)
  (feed @x @y))
```

Running this produces:

```bash session
$ ./irack --single-step seed.wwml
(group
  (cell @x 0)
  (cell @y)
  (feed @x @y))
<Press Enter>

(group
  (cell @x)
  (cell @y 0)
  (feed @x @y))
<Press Enter>

$
```

`group` does not behave like a scope, so:

```wwml
(cell @x 0)
(group
  (cell @y)
  (feed @x @y))
```

... evolves like so:

```bash session
$ ./irack --single-step seed.wwml
(cell @x 0)
(group
  (cell @y)
  (feed @x @y))
<Press Enter>

(cell @x)
(group
  (cell @y 0)
  (feed @x @y))
<Press Enter>

$
```

Let's say we have a list of items:

```wwml
(window style: "flow-col text-3xl text-neutral-50 font-light"
  (p "Buy apples")
  (p "Buy pineapples")
  (p "Go home"))
```

If we just `group` items, nothing will chage: `style`-less groups are transparent for Microfold, it just walks through them to find visual nodes.

```wwml
(window style: "flow-col text-3xl text-neutral-50 font-light"
  (group
    (p "Buy apples")
    (p "Buy pineapples"))
  (p "Go home"))
```

![Figure 15](<slides/Guide 4/figure15.png>)

We can add a style to the group. Maybe it should have a background color, some padding and margin (both expressed in spacing units), and finally rounded borders:

```wwml
(window style: "flow-col text-3xl text-neutral-50 font-light"
  (group style: "bg-neutral-800 p-5 m-5 rounded-lg"
    (p "Buy apples")
    (p "Buy pineapples"))
  (p "Go home"))
```

Notice how we didn't need to specify the `flow` of the group. Unlike `window`s, whose default flow is `flow-none`, the default flow of `group` is `flow-col`.

![Figure 16](<slides/Guide 4/figure16.png>)

Maybe we want to center something. To center the group itself, we can use `center`:

```wwml
(window style: "flow-col text-3xl text-neutral-50 font-light"
  (group style: "center bg-neutral-800 p-5 m-5 rounded-lg"
    (p "Buy apples")
    (p "Buy pineapples"))
  (p "Go home"))
```

![Figure 17](<slides/Guide 4/figure17.png>)

To center the content of the group, we can use `content-center`.

```wwml
(window style: "flow-col text-3xl text-neutral-50 font-light"
  (group style: "content-center bg-neutral-800 p-5 m-5 rounded-lg"
    (p "Buy apples")
    (p "Buy pineapples"))
  (p "Go home"))
```

![Figure 18](<slides/Guide 4/figure18.png>)

There's also the option to center only on the horizontal or vertical axis: `center-x`, `center-y`; and `content-center-x`, `content-center-y`, correspondingly.

Let's say we want something interactive now. How about a button?

```wwml
(window
  ($Button (@action inc) "Increment"))
```

I'd recommend running MxRack like so:

```bash session
$ ./mxrack-x86_64.AppImage --quiet seed.wwml
```

Or with the `--live` flag if you're running live:

```bash session
$ ./mxrack-x86_64.AppImage --live --quiet seed.wwml
```

This way, it won't flood your terminal with circuit printouts.

![Figure 19](<slides/Guide 4/figure19.png>) 

As you can see, the button is inactive; and as the mouse cursor indicates, you can't press it.

The reason is because the button lacks its action cell. Let's add it:

```wwml
(window
  (cell @action)
  ($Button (@action inc) "Increment"))
```

Now that there's a cell, the button can be pressed — but only once! It then remains in this pressed state.

![Figure 20.0|300](<slides/Guide 4/figure20_0.png>) ![Figure 20.1|300](<slides/Guide 4/figure20_1.png>) 

The reason is because the `@action` cell is now occupied. To test this, let's fill the `@action` cell ourselves:

```wwml
(window
  (cell @action inc)
  ($Button (@action inc) "Increment"))
```

If we run this circuit, we see that the button *starts* in the pressed state:

![Figure 21](<slides/Guide 4/figure21.png>)

Then there's the `p` node. As we already saw, the `p` node supports strings:

```wwml
(window (p "Hello World"))
```

It can also display numbers, booleans, and symbols:

```wwml
(window style: "flow-col"
  (p 100)
  (p true)
  (p qux))
```

![Figure 22](<slides/Guide 4/figure22.png>)

But what we're interested in is its ability to display the content of a cell:

```wwml
(window
  (cell @count 0)
  (p @count style: "text-3xl text-white"))
```

![Figure 23](<slides/Guide 4/figure23.png>)

The `p` node also allows you to use a Nitrene expression, through `^(...)`, in which you can refer to edges:

```wwml
(window
  (cell @count 0)
  (p ^(~ "Count is: " @count) style: "text-3xl text-white"))
```

This example can be rewritten with the string interpolation syntax described in the previous part of the guide:

```wwml
(window
  (cell @count 0)
  (p ^"Count is: ⸢@count⸣" style: "text-3xl text-white"))
```

![Figure 24](<slides/Guide 4/figure24.png>)

So in our button example, we can use the `node` node to "observe" the action cell, like so:

```wwml
(window
  (node @action-cell
    (cell @action))
  ($Button (@action inc) "Increment"))
```

The `node` node lets you refer to a live Rack/MxRack node through an edge. Now, let's display the `@action-cell` using the `p` node:

```wwml
(window style: "flow-col"
  (node @action-cell
    (cell @action))
  (p @action-cell style: "font-mono text-neutral-100")
  ($Button (@action inc) "Increment"))
```

![Figure 25.0|300](<slides/Guide 4/figure25_0.png>) ![Figure 25.1|300](<slides/Guide 4/figure25_1.png>) 

As you can see, just after I pressed the button, the `@action` cell (here printed quite crudely, since `p` uses a very simple subset of WwML by default)  was filled with `inc`. As long as `inc` remains there, the button will keep its pressed state, and you won't be able to press any buttons on the same cell.

Speaking of buttons on the same cell, let's add another button for decrement:

```wwml
(window style: "flow-col"
  (node @action-cell
    (cell @action))
  (p @action-cell style: "font-mono text-neutral-100")
  ($Button (@action inc) "Increment")
  ($Button (@action dec) "Decrement"))
```

Now if you click either button, the action cell is going to become occupied with the corresponding action, and the button you pressed will remain in the pressed state.

![Figure 26.0|300](<slides/Guide 4/figure26_0.png>) ![Figure 26.1|300](<slides/Guide 4/figure26_1.png>)

As an experiment, instead of the `@action` cell, let's have a `queue` of actions. We'll keep our `node` so that we can observe what's going on inside our app:

```wwml
(window style: "flow-col"
  (node @queue
    (queue (@action @actions) ()))
  (p @queue style: "font-mono text-neutral-100")
  ($Button (@actions inc) "Increment")
  ($Button (@actions dec) "Decrement"))
```

After pressing the buttons a couple of times, this is what we get:

![Figure 27](<slides/Guide 4/figure27.png>)

As you can see, the buttons now behave like normal buttons, instead of keeping themselves in the pressed state. This is because the `queue`'s back edge is always empty, as it always "absorbs" writes and enqueues them.

Finally, let's try implementing a simple counter app using the nodes we already know.

```wwml
(window style: "flow-col"
  (cell @count 0)
  (cell @action)
  (discard @action)
  (view ((@action @count) (inc ±n) (over @count)) ^(+ n 1))
  (view ((@action @count) (dec ±n) (over @count)) ^(- n 1))
  
  (p @count style: "text-neutral-50")
  ($Button (@action inc) "Increment")
  ($Button (@action dec) "Decrement"))
```

![Figure 28](<slides/Guide 4/figure28.png>)

We can style the whole thing up a little bit:

```wwml
(window style: "backdrop-neutral-950"
  (cell @count 0)
  (cell @action)
  (discard @action)
  (view ((@action @count) (inc ±n) (over @count)) ^(+ n 1))
  (view ((@action @count) (dec ±n) (over @count)) ^(- n 1))
  
  (group style: "center flow-col bg-neutral-900 p-5 rounded-lg"
    (p @count style: "center-x text-neutral-50 text-3xl font-light")
    (group style: "flow-row gap-3"
      ($Button (@action inc) "Increment")
      ($Button (@action dec) "Decrement"))))
```

![Figure 29](<slides/Guide 4/figure29.png>)

This stills looks terrible because we're using the Tailwind color palette — while the buttons are using the built-in MxRack `ui` palette. (The `ui` palette exists to support "themable" default styles for components such as `Button`; and is more "semantic"  than "chromatic". The definition of the `ui` palette can be found in `runtime/codices/mxrack.codex.wwml`.)

Let's use colors from the `ui` palette instead of the Tailwind palette to get a more coherent look. Let's also use `OutlinedButton` instead of `Button`; it looks better in this particular circumstance.

```wwml
(window style: "backdrop-ui-base"
  (cell @count 0)
  (cell @action)
  (discard @action)
  (view ((@action @count) (inc ±n) (over @count)) ^(+ n 1))
  (view ((@action @count) (dec ±n) (over @count)) ^(- n 1))
  
  (group style: "center flow-col bg-ui-surface p-5 rounded-lg"
    (p @count style: "center-x text-ui-content text-3xl font-light")
    (group style: "flow-row gap-3"
      ($OutlinedButton (@action inc) "Increment")
      ($OutlinedButton (@action dec) "Decrement"))))
```

Note that there's nothing wrong with either palette. It's just that stock button visuals (and visuals of  other components) use the `ui` palette. If you were to implement your own visuals — and you probably would to have custom buttons and so on — then you could just use the Tailwind palette everywhere.

![Figure 30](<slides/Guide 4/figure30.png>)

As an exercise, try reordering the buttons, and adding a functioning Reset button.