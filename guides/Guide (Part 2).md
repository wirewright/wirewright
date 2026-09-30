While experimenting with Rack, one particularly useful node is the `delay` node. It allows you to delay the evolution of its *child node* for a few ticks. In `seed.wwml`, write:

```wwml
(cell @x 100)
(cell @y)
(delay 3 (feed @x @y))
```

Run this with `irack` and step through the evolution:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @x 100)
(cell @y)
(delay 3 (feed @x @y))
<Press Enter>

# Frame 1
(cell @x 100)
(cell @y)
(delay 2 (feed @x @y))
<Press Enter>

# Frame 2
(cell @x 100)
(cell @y)
(delay 1 (feed @x @y))
<Press Enter>

# Frame 3
(cell @x 100)
(cell @y)
(feed @x @y)
<Press Enter>

# Frame 4
(cell @x)
(cell @y 100)
(feed @x @y)
<Press Enter>

$
```

What we can see in the first few frames is `delay` counting down to `0`. In Frame 3, the countdown reaches `0`, causing the `delay` node to dissolve itself — which in turn leaves its child "out in the open", unwrapped.

Prior to Frame 3, `delay` acted as a protective shell for its child. In Frame 3, `feed` — the child — is not yet active, but `delay` cannot be seen either. This is the frame where `delay` dissolved itself. For all Rack knows, `delay` just destroyed itself in favor of a bunch of odd-looking terms; so `feed` is not yet active. It is only when computing the next frame, Frame 4, that Rack recognizes the `feed` node, and lets it do its thing.

So before Frame 3, the child was just inert data sitting inside `delay`, much like the value of a `cell`. Rack had no way to reach and transform it. After the `delay` node dissolved itself in Frame 3, the child `feed` became subject to the laws of physics of Rack; so in the very next frame, Frame 4, we see them in action: `feed` moving `100` from `@x` to `@y`.

Another node that is useful to know is `discard`. It is a very simple node: its purpose is to burn terms stored in a cell. You can literally imagine it as a funnel going from a cell to a trash can. Whatever gets into the cell, gets destroyed on the next frame. In other words, `discard` clears cells; and more generally, it clears places. In `seed.wwml`, type:

```wwml
(cell @x 100)
(discard @x)
```

Running the circuit, we get:

```bash session
$ ./irack --single-step seed.wwml
# Frame 0 (seed)
(cell @x 100)
(discard @x)
<Press Enter>

# Frame 1
(cell @x)
(discard @x)
<Press Enter>

$
```

As you can see, the `100` disappeared from `@x` in Frame 1. That is `discard`'s doing.

`discard` works like any other node in Rack. To compute Frame 1 from Frame 0, Rack asks each Frame 0 node for patches, given the complete previous frame (here, Frame 0). `cell`s are inert, but `discard` is not. It looks at its edge (here, `@x`), looks at whether there are nonempty cells there, and if there are, it clears them all. Provided there are no conflicts, Rack merges the patch into Frame 1; in which we indeed see an empty (clear) `@x`.

Notice how I said *nonempty cells*, plural. `discard` is one of the very few Rack nodes that are *not* confused by many cells at the same edge:

```wwml
(cell @x 100)
(cell @x 200)
(cell @x 300)
(discard @x)
```

If you save the above in `seed.wwml` and run it:

```bash session
$ ./irack --single-step seed.wwml
(cell @x 100)
(cell @x 200)
(cell @x 300)
(discard @x)
<Press Enter>

(cell @x)
(cell @x)
(cell @x)
(discard @x)
<Press Enter>

$
```

You are already familiar with four nodes: `cell`, `feed` (and its variants), `delay`, and `discard`. Instead of introducing even more nodes, let's make a brief pause and look at the kinds of interesting behaviors the aforementioned nodes alone can demonstrate.

Consider the following circuit:

```wwml
(cell @xs (1 2 3 4 5))
(cell @a)
(cell @b)
(cell @c)
(cell @y)
(feed (@xs front) @a)
(feed @a @b @c @y)
(delay 10 (discard @y))
```

Its main purpose is to demonstrate how the notion of *backpressure* emerges from the most fundamental operational principles of Rack. Save the circuit in `seed.wwml`, and run it as usual with `./irack --single-step seed.wwml`.

> [!NOTE]
> Instead of showing long frame traces, which are basically impossible to interpret, I'm going to attach videos. The videos are distributed along with the guides. Feel free to play, pause, and rewind to understand what the circuit is doing.

![Video of the circuit](<./slides/Guide 2/backpressure.mp4>)

When all cells become full (0:07 in the video), the `feed`s have nothing left to do. The entire circuit then waits until the `delay` node dissolves (by "waits", I do not mean anything of intelligence is going on here; the circuit "waits" simply because the only computation that is making progress is `delay`, and all others have stalled; the dissolution of `delay` just happens to be what the circuit needs to progress in this example).

So when the delay dissolves (0:10), `discard` starts burning at `@y` (0:11). This creates some empty space, so the second `feed` can move `@c` to `@y`, which creates further empty space above and so on (0:12-0:14). At 0:15, the first `feed` finally has a chance to move `5` from `@xs` to `@a`, since `@a` is now empty. Values remaining in the cells "fall down" and get burned by `discard`. After all values were burned, the circuit reaches quiescence.

What I like about this example is how it basically looks like physics and not some bizarre rewrite environment. These sorts of examples are why I decided to call Wirewright a *symbolic physics environment*; and in general they are the origin of the notion of *symbolic physics*. It just looks too much like physics to me. Try scrubbing through the video back and forth; I don't know about you, but I just can't unsee "the physics" in it.

Let's get back to introducing new nodes! The next one of interest for us in this guide is `queue`. Whereas nodes like `feed`,  `discard`, and `delay` are not unlike training wheels, `queue` is something you will actually see in practice occasionally. In `seed.wwml`, write:

```wwml
(queue (@front @back) (1 2 3 4 5))
(feed @front @back)
```

Run it with `irack` like so:

```bash session
$ ./irack --single-step seed.wwml
(queue (@front @back) (1 2 3 4 5))
(feed @front @back)
<Press Enter>

(queue (@front @back) (2 3 4 5 1))
(feed @front @back)
<Press Enter>

(queue (@front @back) (3 4 5 1 2))
(feed @front @back)
<Press Enter>

(queue (@front @back) (4 5 1 2 3))
(feed @front @back)
<Press Enter>

(queue (@front @back) (5 1 2 3 4))
(feed @front @back)
<Press Enter>

(queue (@front @back) (1 2 3 4 5))
(feed @front @back)
<Ctrl-C>

$
```

As you can see, the circuit is basically rotating the items in the queue. The first thing is moved to the back of the queue ad infinitum.

One important point here is that you **cannot** do something like this with `feed` and a plain `cell`. Indeed, this is a very nice opportunity to acquaint ourselves with so-called *merge policies*. 

A plain cell uses the *exclusive* merge policy. This means the cell will reject changes to itself in all cases except if exactly one value was proposed by the rest of the circuit:

```wwml
(cell @xs (1 2 3 4 5))
(feed (@xs front) (@xs back))
```

If you run this, you would get just:

```bash session
$ ./irack --single-step seed.wwml
(cell @xs (1 2 3 4 5))
(feed (@xs front) (@xs back))
<Press Enter>

$
```

The reason is that basically, `feed` enters into a conflict with *itself*: one "half" of it wants to remove the front of the list `@xs`, and the other one wants to append to the back of `@xs`. That's *two* proposed values for `@xs`. As `@xs` is an *exclusive* cell (its merge policy is *exclusive*), it "gladly" rejects all the values; which makes `feed` unhappy (by failing its transaction(s) — its larger efforts to modify the circuit). So `feed` retracts *all* its changes to the circuit. The result is a circuit in which nothing happens, so `irack` exits due to  quiescence.

You can use the *arena* merge policy to make the circuit above work:

```wwml
(cell (arena @xs) (1 2 3 4 5))
(feed (@xs front) (@xs back))
```

... but, funnily enough, that is more or less beside the point. We *have* acquainted ourselves with merge policies, have we not? There are some more of them. If you are interested for some reason, visit the `rack.[merge-policy]` group in the doctool. It gives more detailed explanations for *what* merge policies are and *which* merge policies exist.

Going back to the `queue` node — which is supposed to be far more interesting to us at the moment ­— it consists of two parts:

```text
(queue (@front @back) (1 2 3 4 5))
       --------------  -----------
	       header        buffer
```

The `queue`'s *header* specifies the edge which you want to use to refer to the *front* of the queue. Similarly, the header requires you to name an edge for the *back* of the queue. The header can also configure other aspects of the queue's behavior; we will cover that later.

The `queue`'s *buffer* is a dictionary term that is used to store items currently in the queue. The buffer can be any dictionary, but `queue` only cares about its *itemspart*. The following will work:

```wwml
(queue (@front @back) (1 2 3 x: 100 y: 200))
```

... but is *hopelessly* meaningless. Here, the buffer has two additional pairs `x: 100` and `y: 200`, which are simply ignored by `queue`.

The front of a queue behaves like a *nonempty* cell with respect to the rest of the circuit. This virtual front cell appears only when the queue's buffer is nonempty. 

The back of a queue, similarly, beahves like an *always-empty* cell.

The queue does not exist from the circuit's point of view. Only the front cell and the back cell do. The `queue` populates `@front` with the first item (if any) of the buffer *at the start of a frame*; and then automatically moves terms from `@back` to the back of the buffer *at the end of the frame*. If `@front` is cleared or updated during a frame, the queue removes or updates the first item in *buffer*, correspondingly.

One useful convention in the naming of `@front` and `@back` is to use *singular* for a front edge and *plural* for a back edge. For example, if you have a queue of numbers, you might name things like so:

```wwml
(queue (@number @numbers) (1 2 3))
(discard @number)
```

Running this circuit, we get:

```bash session
$ ./irack --single-step seed.wwml
(queue (@number @numbers) (1 2 3))
(discard @number)
<Press Enter>

(queue (@number @numbers) (2 3))
(discard @number)
<Press Enter>

(queue (@number @numbers) (3))
(discard @number)
<Press Enter>

(queue (@number @numbers) ())
(discard @number)
<Press Enter>

$
```

Or if you have, say, a queue of clients, you would write:

```wwml
(queue (@client @clients) ())
```

This `@clients` example is useful in another respect: it shows how to initialize an *empty* queue.

You can constrain the capacity of a queue. This can be done by specifying a `min` capacity and a `max` capacity in the queue's header.

The `min` capacity determines the amount of items in the buffer the queue requires to start *draining*. For example, if you say `min: 3`, this means the queue should start draining when its buffer has three or more items. By default, `min: 1`. `min: 0` (or negative, or fractional, or non-numeric, etc.) makes no sense and will render the queue inert.

The `max` capacity determines the greatest amount of items the buffer can accept. A queue with a set `max` capacity cannot accept more than that number of items. By default, `max: ∞`, meaning the queue can accept an arbitrary number of items.

A queue with a set `max` is another opportunity to demonstrate backpressure:

```wwml
(queue (@x @xs) (1 2 3 4 5))
(queue (@y @ys max: 2) ())
(cell @z)
(feed x @ys @y @z)
(discard @z)
```

Running this, we get:

![Queue backpressure](<slides/Guide 2/queues.mp4>)
At the end of this part of the guide, I think we are well equipped to encounter our first pair of IO nodes: `fs` and `db`. They are very simple nodes, not unlike the most basic variant of `feed`: think something like `(feed @a @b)`.

The `fs` node allows you to make *requests* to the file system. There is a number of possible requests (see `rack.fs` in the doctool), but we can start with `(read "/path/to/file")`.

In `seed.wwml`, write:

```wwml
(cell @request (read text file "/tmp/hello.txt"))
(cell @response)
(fs @request @response)
```

Now create the file `/tmp/hello.txt`:

```bash session
$ echo "Kaixo mundua" > /tmp/hello.txt
```

Then run the circuit:

```bash session
$ ./irack --single-step seed.wwml
(cell @request (read text file "/tmp/hello.txt"))
(cell @response)
(fs @request @response)
<Press Enter>

(cell @request)
(cell @response (ok (read "/tmp/hello.txt" "Kaixo mundua\n")))
(fs @request @response)
<Press Enter>

$
```

This is one of the ways you can read a file; here, a text file. This way is not the most idiomatic one, though; just look at how imperative it is! Do get a sneak peek at what is possible with a more... *structural* approach, we can use the `path...reading` node. In `seed.wwml`, write:

```wwml
(path ("/tmp/hello.txt" reading))
```

Then run this with `irack`. Importantly, what we're going to do for the first time in this series of guides, is we're going to omit the `--single-step` flag.

```bash session
$ ./irack seed.wwml
(path ("/tmp/hello.txt" reading))

(path ("/tmp/hello.txt" reading)
  (present "Kaixo mundua\n"))
```

Rack is just waiting now. Why? For what? Well, the circuit did reach quiescence, but there's a node in it that can be *perturbed* by the outside world — `path`. So let us perturb it. In another terminal session, write:

```bash session
$ echo "Hello!" > /tmp/hello.txt
```

If you now look at the `irack` session, you will see how the circuit was perturbed by the write and evolved further:

```bash session
$ ./irack seed.wwml
(path ("/tmp/hello.txt" reading))

(path ("/tmp/hello.txt" reading)
  (present "Kaixo mundua\n"))

(path ("/tmp/hello.txt" reading)
  (present "Hello!\n"))
```

Rack is again waiting for things to happen in the outside world. Let's remove the file:

```bash session
$ rm /tmp/hello.txt
```

Rack notices the file is now absent and evolves the circuit to:

```bash session
$ ./irack seed.wwml
(path ("/tmp/hello.txt" reading))

(path ("/tmp/hello.txt" reading)
  (present "Kaixo mundua\n"))
  
(path ("/tmp/hello.txt" reading)
  (present "Hello!\n"))

(path
  ("/tmp/hello.txt" reading)
  (absent
    "Error opening file with mode 'rb': '/tmp/hello.txt': No such file or directory"))
```

You can play with the node for as long as you want but eventually you'd probably want to Ctrl-C out of `irack`.

There is also the `path...report` node, by the way:

```wwml
(path ("/tmp/hello" report))
```

It works like the `path...reading` node but shows a report instead:

```bash session
$ ./irack seed.wwml
(path ("/tmp/hello" report))

(path ("/tmp/hello" report)
  (absent "path does not exist"))
```

Let's create a file at `/tmp/hello`:

```bash session
$ echo "Hi from /tmp/hello" > /tmp/hello
```

The circuit reacts to this and evolves like so:

```bash session
$ ./irack seed.wwml
(path ("/tmp/hello" report))

(path ("/tmp/hello" report)
  (absent "path does not exist"))

(path ("/tmp/hello" report)
  (file
    size: {bytes: 19, human: "19B"}
    timestamp: "2026-09-29 23:39:49 UTC"))
```

Now let's  remove the `/tmp/hello` file and make it a directory instead. Let's also put two files in the directory, `a` and `b`; and another directory `c` which we'll keep empty:

```bash session
$ rm /tmp/hello
$ mkdir /tmp/hello
$ touch /tmp/hello/a /tmp/hello/b
$ mkdir /tmp/hello/c
```

Now look at how the circuit had been evolving as we were making the changes:

```bash session
$ ./irack seed.wwml
(path ("/tmp/hello" report))

(path ("/tmp/hello" report)
  (absent "path does not exist"))

(path ("/tmp/hello" report)
  (file
    size: {bytes: 19, human: "19B"}
    timestamp: "2026-09-29 23:39:49 UTC"))

# After `rm /tmp/hello`  
(path ("/tmp/hello" report)
  (absent "path does not exist"))

# After `mkdir /tmp/hello`
(path ("/tmp/hello" report)
  (dir timestamp: "2026-09-29 23:42:45 UTC"))

# After `touch /tmp/hello/a /tmp/hello/b`
(path ("/tmp/hello" report)
  (dir timestamp: "2026-09-29 23:42:53 UTC"
    (file "a")
    (file "b")))

# After `mkdir /tmp/hello/c`
(path ("/tmp/hello" report)
  (dir timestamp: "2026-09-29 23:43:00 UTC"
    (file "a")
    (file "b")
    (dir "c")))
```

The most important takeaway from these examples with the `path` node is, in my opinion, the fact that you can have persistent structures in the symbolic world evolve according to actions happening in the real world. (Well, sort of; in the real world as it is defined by the OS.)

Also, when you look at the `path` node, be it `reading` or `report`, try to see it as a literal *symbolic UI*  for a file viewer (`reading`) or a file explorer `(report`). In fact, MuSoma (the GUI for Rack) does this, with both. If you open MuSoma with the following `seed.wwml`:

```wwml
(path ("/tmp/hello" report))
```

... assuming you haven't changed the `/tmp/hello` directory structure:

```bash session
$ ./musoma-x86_64.AppImage seed.wwml
```

... you'll see the following:

![Path report node displays like a file explorer in MuSoma](<slides/Guide 2/path-report.png>)
<center><i>Path report node displays like a file explorer in MuSoma</i></center>

Right now MuSoma doesn't support this, but one could imagine clicking on `a` or `b` etc., which would correspond to rewriting the path correspondingly (as if you manually changed e.g. `"/tmp/hello"` to `"/tmp/hello/a"` in the node).

Ctrl-C MuSoma if you opened it:

```bash session
$ ./musoma-x86_64.AppImage seed.wwml
# ...
<Ctrl-C>

$
```

Now the last node I said we will cover in this part of the guide is the `db` node. The `db` node is not unlike the `fs` node, although it has a slightly different syntax. Right now the only database we support is [SQLite](https://www.sqlite.org/). We will add support for more databases in the future.

In `seed.wwml`, write:

```wwml
(queue (@query @queries)
  ((exec "create table if not exists employees (id integer primary key, name text not null)")
   (exec "insert into employees (name) values (?), (?), (?)" "Jane" "Barbara" "John")
   (query "select * from employees")))

(queue (@reply @replies)
  ())

(db (@query -> "sqlite3:///tmp/employees.sqlite3" -> @replies))
```

Run this with `irack` and single-step:

```bash session
$ irack --single-step seed.wwml
```

You should see something along the lines of:

![The database node in action](<slides/Guide 2/db.mp4>)
On the first few frames we see the database node initializing (notice how it goes from nothing to `pending` to `up`). Then the database node goes to work, sending SQL queries to the database and waiting for replies. When the replies arrive it enqueues them into the `@replies` queue.

I know that is quite an artificial example, but you can *sort of* see the idea in action,  how the whole thing is supposed to work. Both `fs` and `db` are a particular type (or species, if you will) of symbolic object: they are both *symbolic machines*. They take some input, they do whatever they do, and they give you an output. The things are a black box to the circuit. Something like `path` is an entirely different species of symbolic objects; as is `feed`, `cell`, and so on. I have not given them distinctive names yet; but `fs`, `db`, and similar look very much like *symbolic machines* to me.

What I was saying is that the example above is rather artificial. But in a real app, something will be producing SQL statements (assuming you want to use  SQL); handing them off to the `db` node, and then handling the reply once it arrives. See, for instance, the todo app `examples/todo-list.musoma.wwml`. If you run it with MuSoma and move back and forth in time after you create todos and so forth, you will see that this is exactly how the thing works: eventually, the app places SQL statements in the query cell, then the `db` node does its talking with the database, and then finally it atomically exchanges the query with the reply.

The last point, about atomicity, is actually quite important. The `db` node (and the `fs` node, for that matter, and some other nodes) will never consume the query term before the corresponding reply arrives. The action of consuming a query and placing a reply is one atomic action: an atomic exchange.

An important picture for symbolic machines is literally of a machine, like the ones you encounter in the real world. A washing machine, perhaps, although its notions of input and output are rather abstract. The machines you might see in factories are perhaps a better example. Something with a clearly defined input tray or slot, and an output tray. "Human machines", such as bureaucracies, work in a distantly similar way. The important point is I want you to have a very *physical* image in mind. There is truly nothing abstract about `db` and `fs`. It is just that our programmer brains are... forgive me... damaged — or, to use less controversial terms, *warped* — by abstraction.

![DB machine illustration](<slides/Guide 2/db-machine.png>)
<center><i>Forgive me for my terrible illustration skills</i></center>

Do note that in the figure above, what I have *meant* to draw are two trays. You place your query at rest in the input tray, and it just sits there. The db node notices the query and begins its work. When it has results it takes the query from the input tray, "burns it", and puts the reply in the output tray. Then the reply just sits there until someone picks it up. The db node will also notice you picking the query up for some reason and carrying it away; in that case it will cancel whatever pending work there is pertaining to the query.

The circuit can have many `db` nodes. They will all reuse the same database connection (assuming they are referring to the same database, of course; in case of SQLite3, the same database file). In fact, every "Delete" or "Update" button in an app can *contain* a `db` node, if you want that for some reason. In my own experiments with Wirewright, though, I see a natural tendency of centralizing database access. But Wirewright is one of the first, if not the first, ways to actually see how a decentralized architecture would look like (on the application level, that is). My hope is that Wirewright would allow further experimentation with software architecture, because the current state of the art... Well, let me not swear here. *It works, it works, I know*. "Don't reinvent the wheel," say the square-wheel people... (A phrase inspired by a similar take by Casey Muratori.)
