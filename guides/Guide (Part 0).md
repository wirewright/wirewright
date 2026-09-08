# Wirewright: The Guide (Part 0)

Thank you for being interested in Wirewright. It'll be a wild ride, I promise :) We'll start from the very basics, and slowly build our way up to more practical programs.

> [!NOTE]
> Wirewright is heavily experimental and is not even versioned properly yet. Some parts of the system are being built while they are documented. Even though *these* guides are expected to cover some of the most stable parts of the system, do not expect Wirewright to be stabile in general (yet). I am working on it.

To get started, download the [latest release](https://github.com/wirewright/wirewright/releases/latest) of Wirewright, an archive, and extract it. Inside the archive is a directory named `ww-dist`. `cd` there once you have it. From the command line, the whole process could look like this:

```bash session
$ wget -qO- https://github.com/wirewright/wirewright/releases/latest/download/ww-dist.tar.gz | gunzip | tar xvf -
$ cd ww-dist
$ ls
doctool  examples  irack  musoma-x86_64.AppImage  README  runtime
```

We will use **MuSoma**, an interactive graphical environment for Rack, in later guides. You can still check it out by running the examples:

```bash session
$ chmod +x musoma-x86_64.AppImage # If necessary

$ ./musoma-x86_64.AppImage examples/calculator.musoma.wwml

    ############
   ##############
   ##############
   #####     ####
   #####     ####    Wirewright µsoma
    #############    https://github.com/wirewright/wirewright [iota]
     ###########
   #   ######   #
   ###        ###
   ###############
    ###############

 LOG  2026-09-06 01:48:06  Initializing refs
 LOG  2026-09-06 01:48:06  Initializing workspace
 LOG  2026-09-06 01:48:06  Initializing and booting agents
 LOG  2026-09-06 01:48:06  Running
 LOG  2026-09-06 01:48:06  Reload MuSoma codex
 LOG  2026-09-06 01:48:06  Reload editR
 LOG  2026-09-06 01:48:06  Reload library
 LOG  2026-09-06 01:48:06  Reload seed
<Ctrl-C>
 
$
```

Use Ctrl-C to close MuSoma. Hitting "X" is not enough yet.

`irack` will be enough to learn the basics. Verify that it works by running it without arguments. It should output a fairly long help message:

```bash session
$ ./irack
<...a fairly long help message...>
```

## Cells and feeds

Create a file named `seed.wwml`, and open it in your favorite text editor:

```bash session
$ touch seed.wwml

$ hx seed.wwml # or Vim, Emacs, VSCode, Sublime Text, whatever.
```

In the file, write:

```wwml
(cell @x 0)
(cell @y)
(feed @x @y)
```

Save it and go back to the shell. To run the file, use `./irack --single-step seed.wwml` like so:

```bash session
$ ./irack --single-step seed.wwml
(cell @x 0)
(cell @y)
(feed @x @y)
<Press Enter>

(cell @x)
(cell @y 0)
(feed @x @y)
<Press Enter>

$
```

Now, what *in the world* did you just do and witness?

To start, the content of `seed.wwml` is a frozen snapshot of a world — a *symbolic world*. For historical reasons, we call symbolic worlds *circuits* in Wirewright. I will use the words *world* and *circuit* interchangeably — in the context of Wirewright, they mean the same thing.

So `seed.wwml` is a frozen snapshot of a Wirewright circuit. The circuit contains three *nodes*. Think of them as *entities* or *things* inhabiting the world. The first node is a `cell`.

A cell designates (fences off) a place in the world to store a *term*.

For now, think of terms as values. Examples of a term include *numbers*, *strings*, and *booleans* — you are probably familiar with them from other languages.

In our case, the first cell starts *nonempty*: it contains the number `0` in its fenced-off area.

The second node is an *empty cell*: its fenced-off area contains no value.

The third node is `feed`. For us here, it is enough to say `feed` moves a term from one place to another; more specifically, from one cell to another. Generally, though, a feed is one of the most basic *relations* between *places*.

An important condition for `feed` to work is that its *source place* is nonempty, and its *destination place* is empty. This is because you cannot move something from place A to place B if place B is already occupied!

Now, what `irack` did when you ran it from the terminal is it "unfroze" or "unpaused" the snapshot of the world in `seed.wwml`.

When you pressed Enter, `irack` computed the next *frame* of the world — in other words, it *evolved* the world into the future by one time-step:

```wwml
(cell @x)
(cell @y 0)
(feed @x @y)
```

Notice how `0` "fell" into the second cell — the `@y` cell. The `feed` node was responsible for this "fall". Its preconditions were met: a nonempty cell at `@x` , an empty cell at `@y`; so it moved the `0` from the `@x` cell to the `@y` cell.

`@`-things are called *edges* in Rack, and they are *very* important. `@x` is read simply as "at x", `@foobar` as "at foobar", etc. 

The quickest way to understand edges is to imagine them as groups which nodes can participate in. Only through participation in the same group can a node refer to another node.

A Rack edge is, in fact, a *hyperedge*. In our group analogy, this simply means more than two nodes can participate in the same group.

In Rack, when a node wants to refer to another node, it can only do so if both nodes are linked by an edge. In other words, they must *share* an edge; have an edge in common; they must participate in the same group.

A node can participate in zero or more edges. Where exactly the edges go syntactically and what they are used for by their participants depends on the participants themselves. Information about this can be found in the documentation for Rack, which I will cover in a dedicated section below.

To be a little more technical, edges specify which nodes are *adjacent* to one another — which nodes are next to each other, and which ones are far apart. Nodes and edges form a *hypergraph*. Rack rewrites the hypergraph on every time-step according to its rules, which in turn act based on what exists in the hypergraph.

> [!NOTE]
> The order of nodes in the circuit does not matter.

What the `feed` node does can be described as follows. It looks at the first edge `@x` and asks whether there are nonempty cells there. It then looks at the second edge `@y` and asks whether there is an empty cell there. If the answer is yes to both, and if `feed` is not confused (for example by many nonempty cells at `@x` or many empty cells at `@y`), it *proposes* a *patch* to the circuit. The patch describes how to move the value from the cell at `@x` to the cell at `@y`. Rack later accepts this patch if there are no conflicts.

Let's go back to our circuit, `seed.wwml`:

```bash session
$ ./irack --single-step seed.wwml
(cell @x 0)
(cell @y)
(feed @x @y)
<Press Enter>
(cell @x)
(cell @y 0)
(feed @x @y)
<Press Enter>

$
```

You can see that when you pressed Enter the second time, `irack` exited. *But why did it*?

What happened was Rack computed the next frame, but then noticed the frame is exactly the same as the previous frame, namely:

```wwml
(cell @x)
(cell @y 0)
(feed @x @y)
```

From Rack's point of view, it looks like the circuit stopped evolving. It reached something akin to an equilibrium, a point of stability, when nothing changes unless there is an external *perturbation*. This state called *quiescence* in Wirewright.

So, Rack noticed the circuit reached *quiescence*. Since there are no background tasks that can disturb it (our circuit is *very* simple), and therefore possibly wake it up from quiescence, `irack` decided to wrap things up and quit.

Let's modify the circuit in `seed.wwml` like so:

```wwml
(cell @x 0)
(cell @y)
(feed @x @y @x)
```

Notice how I have added an `@x` to the `feed`. Intuitively, this forms a loop. Let's run the circuit and see what happens:

```bash session
$ ./irack --single-step seed.wwml
(cell @x 0)
(cell @y)
(feed @x @y)
<Press Enter>
(cell @x)
(cell @y 0)
(feed @x @y)
<Press Enter>
(cell @x 0)
(cell @y)
(feed @x @y)
<Press Enter>
(cell @x)
(cell @y 0)
(feed @x @y)
<Ctrl-C>

$
```

The `0` bounces back and forth between the cells! A loop indeed. The `0` will do so forever. This is called an *oscillator*, because the circuit oscillates — it switches back and forth between a finite number of states in exactly the same way.

Do note that if you omit the `--single-step` flag, `irack` will flood your terminal very quickly.

The `feed` node in `seed.wwml` can be rewritten like so:

```wwml
(feed @x @y)
(feed @y @x)
```

In fact, `(feed @edge0 @edge1 @edge2 ... @edgeN)` is a shorthand for:

```wwml
(feed @edge0 @edge1)
(feed @edge1 @edge2)
...
(feed @edgeN-1 @edgeN)
```

## Experiments

Try to do the following experiments yourself. Use the following as a starting point *for each experiment*, unless the experiment states otherwise:

```wwml
(cell @x 0)
(cell @y)
(feed @x @y @x)
```

**Experiment 1.** Change the number from `0` to `100`, and see what happens.

**Experiment 2.** Make the `@y` cell full. For example, put a `"Hello World"` *string* in it. Single-step through the circuit's evolution and explain what happened and why.

**Experiment 3.** Rewrite the `feed` node using two `feed` nodes. Hint: `@x -> @y -> @x` is the same as `@x -> @y`, `@y -> @x`.

**Experiment 4**. Insert two empty cells at edges `@u` and `@v` before the `feed`. Modify the `feed` node so it passes the `0` through all cells, in sequence `@x -> @y -> @u -> @v`, without looping back. Observe what happens when you single-step through the circuit's evolution. Hint: `feed` accepts two or more edges and feeds the previous one into the next one.

**Experiment 5**. Play with the order of nodes from Experiment 4, and see whether anything changes when you run the circuit.

**Experiment 6.** Modify the circuit from Experiment 4 so that the `feed` loops back to `@x`, and observe how `0` flows through the cells as you single-step through the circuit.

**Experiment 7.** Modify the circuit from Experiment 4 so that two cells have values: keep the `0` and put e.g. a `1` or any other value you like in any other cell. Single-step through the circuit's evolution and try to describe what happens in each frame.

> [!NOTE]
> There is no evaluation order in Rack. In fact, there is no evaluation. There are only *frames*, which are discrete snapshots of the evolution of a circuit (world). Nodes in the *next* frame face the entire *previous* frame and decide what to do based on that.
> 
> This is somewhat similar to *synchronous cellular automata*, where rules look at the complete previous frame to decide what the next frame should look like. See also: [Asynchronous cellular automaton — Wikipedia](https://en.wikipedia.org/wiki/Asynchronous_cellular_automaton)

**Experiment 8.** Try moving the `1` from Experiment 7 into different cells. For example, if you had it in `@y`, move it to `@u`. Observe what happens as you step through the circuit's evolution in each case.

**Experiment 9.** Try to fill more and more cells in the circuit from Experiment 8, until you fill all cells. Step through each time you fill another cell, and see how the circuit behaves. Try to explain your observations intuitively.

**Experiment 10**. Based on Experiment 4, try adding a few more cells (say, `@z`, `@w`), and make `feed` pass the `0` through them, too. Make the `0` loop back if you haven't already. 

**Experiment 11.** Break the feed in the circuit from Experiment 5! Make a typo: instead of `@u`, use `@foo` — an edge which no cell participates in. Try to explain your observations intuitively.

**Experiment 12**. Modify the starting circuit to have another pair of cells at the bottom, `@u` and `@v`; and a `feed` below them, feeding `@u` to `@v`. Single-step through the circuit's evolution and observe what happens. Does the circuit reach *quiescence*? Why or why not?

## Documentation

Use the `doctool` binary that comes with each release. If you run it you should see something like this:

```bash session
$ ./doctool

 ##############
 ##############
 #####    #####    Wirewright
 ####      ####    https://github.com/wirewright/wirewright [iota]
 #####    #####
  ############
    ########

 LOG  2026-09-06 01:07:07  Wirewright doctool
 LOG  2026-09-06 01:07:07  Processed 452 composition(s).
 NOTE  2026-09-06 01:07:07  Serving HTTP on 127.0.0.1:9811... Hit Ctrl-C to exit.
```

Open `127.0.0.1:9811` in your favorite web browser. You are interested in the link pointing to `rack`. Click around and read things even if you do not instantly understand them. Exposure is far more important right now than deep comprehension.