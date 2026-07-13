<div align="center">

![Wirewright Logo](https://github.com/user-attachments/assets/3e5dc602-9c8d-412d-a7fa-9e1a0c3b466e)
</div>

# Wirewright

Wirewright is an experimental research project aiming to express computation as the evolution of symbolic structure inside an immutable
world. Wirewright tries to push this idea as far as possible: into IO (including UI, audio [TODO], and networking [TODO]),
symbolic AI, and beyond.

One of the goals of Wirewright is the realization of the idea of *a program as a physical thing*,
a kind of *symbolic mechanism*. What does one mean when one says, structure *is* computation, that is,
structure is *the same thing* as computation? Wirewright is an attempt to answer this question, among
many others.

In Wirewright,

- Buttons and inputs have insides, the same way a button in the real world is a box
  with a mechanism inside it. A Wirewright button is a tiny "organism" containing a
  symbolic model of the mouse. The button is capable of observing this model as well
  as a symbolic description of its own visuals, and reacting to the behavior of the user.
- Programs can look around and move!
- Editors inhabit the same world they edit.
- Sorting is understood as a law that reduces disorder.
- Some of the core distinctions of modern programming do not apply. The same *term* can
  act as data, state, code, and UI.
- Instead of *evaluation*, we have *simulation*.

Wirewright can also be seen through the lens of a game analogy. Imagine Wirewright as implementing
a game, but instead of entities you have data structures, and instead of graphics you have extended
S-expressions; and physics is not about boxes and vectors and collisions and such but about
making data structures interact with each other in various ways.

> [!NOTE]
> I'm not a native speaker of English. I apologize in advance for any mistakes I might make!

> [!NOTE]
> Wirewright is not a solution to any practical problem. Instead, it tries to see what else
> is there beyond & in the near neighborhood of OOP (in the Alan Key sense), pure FP, dataflow,
> discrete simulation, cellular automata, term rewriting, morphological computing, programmable matter,
> and the like. Whether this endeavor results in anything practically useful is currently unknown.
> However, see the examples. We're well past the "it can calculate factorial" point.

## Gallery

### Calculator

This is an example of an interactive calculator. The UI manipulates "AST" directly instead of working with strings. The app
is about ~200 lines of code, *including* comments, blank lines, etc. Starting from the middle of the video I show time-travel.

https://github.com/user-attachments/assets/5ec961a9-61f9-4761-91a3-428de10b709f

Reference: `examples/calculator.musoma.wwml`.

### SQLite

A very simple app that uses an SQLite database. The point is to show how databases can be represented in the symbolic world:
as a black box that takes queries and spits out responses after some time. Starting from the middle of the video I show time-travel
(which is pure; i.e., the database is not changed as I move through time).

https://github.com/user-attachments/assets/63624868-23b3-4990-9993-90dfd700d85e

Reference: `examples/sqlite.musoma.wwml`.

### Bounce

This is an example of a simple "moving" program. The `circuit` defines the boundaries of a "symbolic world". The backsystem
`backsys` defines some "laws". And the `module` inside the world implements a basic bouncing behavior. The module is
subjected to a mix of "laws" defined in the `backsys`, and the laws of Rack, which is the "ultimate" physics here,
responsible for animating `backsys`, `circuit`, etc. themselves.

https://github.com/user-attachments/assets/8c4d54ae-669a-49fd-b9c5-4ff2528b3c33

Reference: `examples/bounce.rack.wwml`.

### Merge sort

This example demonstrates how merge sort (or something very much like it...) can be imagined as a symbolic machine (or a group of symbolic machines).

https://github.com/user-attachments/assets/3213e84f-2696-49a0-a286-5908f77ecbcd

Reference: `examples/msort.rack.wwml`.

## Tutorials

I'm going to try writing tutorials for Wirewright. Please visit one of:

- [Tutorial, Part 1](man/tutorial-1.md)

Please note that Wirewright currently only runs on Linux. However, the tutorials above
use `irack`, which should run on WSL; at least I hope so.

## So what exactly *is* Wirewright?

Good question. As a software project (as opposed to a philosophical endeavor of mine), [my definition of Wirewright](https://youtu.be/rkWXB-3ReV0) is an ecosystem of components which together implement a particular "style" of symbolic computation -- one that is heavily inspired by physics. I call this "style" *symbolic physics*.

Now, if you want a short answer, Wirewright is not a single thing but an umbrella of multiple things, some of them described below, that are made to interact with each other in ways I find interesting.

If I am forced to define what Wirewright *is*, as opposed to what it consists of in practice (see below), I'll say it's an engine
featuring something akin to a "self-evolving abstract syntax tree". The tree is observed and rewritten in various ways by Wirewright
to implement UI, IO, state, and logic. This tree is also what I refer to as the *symbolic world*, although this phrase can be used
more generally.

### Data and notation

#### Terms (pure)

Terms are one of the core things in Wirewright. All terms are immutable. There are six types of terms: numbers, strings, booleans, symbols, dictionaries, and blobs (for binary data). Dictionaries are of most interest. Conceptually, a dictionary is a list of *entries*, where each entry is the pair `(key, value)`. Keys are unique. An entry can be an *item* (its key is 0 or is a number with a predecessor item in the dict) or a *pair* (all other entries). Items therefore form a chain called the *itemspart* (e.g., keys 0, 0->1, 0->1->2, etc.) The rest of the entries form the dictionary's *pairspart*.

#### Notation (pure)

**WwML** (most often abbreviated simply as ML) is a human-readable and writable notation for expressing terms. In other words, it is a way to express terms as text.

ML is based on S-expressions, extended with key-value pairs, e.g. `(/ 1 2 precision: 3)`. WwML features *a lot* of shorthands, so much so that sometimes it stops looking like S-expressions at all:

```wwml
(limit _ ⍊ up-w⫽h: (arg ±λ ⍊ -◇_) ±⟦min,max⟧-w⫽h)
  <> {λ: ^(⟦max,min⟧ ⟦min,max⟧-w⫽h λ), ◇: true}
```

### Term matching & transformation (pure)

**M1** a pattern matching & *backmapping* engine for terms. If Wirewright was an organism, M1 would be its sensory organ -- its eyes and ears.

**Alloy** is a structural templating language. Structural templating is like Lisp's `quote`, `unquote`, `quasiquote`. It can also be compared with something like Handlebars, except Alloy operates on terms rather than strings. Alloy looks like this:

```wwml
(^each (fragments as fragment_)
 (^match fragment
   (when (m-span text_string)
     (^each ((words text) as word_string)
       ^word))
   (when (m-key key_)
     (Key ^key))
   (when (m-key expects-mode_ key_)
     (^unless (= mode expects-mode)
       (Key "Esc"))
     (Key ^key))))
```

Here, things starting with `^` are related to Alloy.

**Nitrene** is an expression language. Nitrene, too, uses terms; in Wirewright, everything uses terms. Nitrene is meant to be embedded in Alloy, but you can embed Alloy in Nitrene as well. In the Alloy example above, expressions such as `(= mode expects-mode)`, `(words text)` and so on are Nitrene.

An *expression language* in the sense I am employing here can be likened to Excel formulas, in terms of its scope & the kinds of computations it allows you to do; that is, raw computation at the "leaves" (e.g. `(+ 2 2)`, `(max 1 2 3)`).

**Rulesets** let you define rules where the left-hand side is an M1 pattern and the right-hand side is an Alloy template. They also allow you to write *backmaps*: the left-hand side is also a pattern, but the right-hand side is now a list of replacements defined relative to each other. For example, `(swap a_ b_) <> {a: ^b, b: ^a}`, under some modes of evaluation, results in an oscillator which swaps *a* and *b* forever: `(swap 1 2)` is rewritten to `(swap 2 1)` and so on forever. Rulesets are one of the "hubs" in Wirewright: they bring together M1 (the pattern `(swap a_ b_)`), Alloy (the templates `^a`, `^b`), Nitrene (the expressions immediately inside the templates: `a`, `b`), and then M1 backmaps `_ <> _`. Rulesets, too, are terms; they are simply dictionaries representing a list of rules.

**Rho** is a collection of composable rewriters. Rho lets you write things like `(ascR (rulesetR))` (notice again how everything is a term). Running this rewriter on a term, Rho will perform an *ascending rewrite*, applying `(rulesetR)` bottom-up. `rulesetR` in turn connects to rulesets I wrote about above.

### Graphics

#### Styling (pure)

**Microfold** is part of the UI stack. It implements Tailwind-like styling among other things (such as e.g. "cue flow", a bidirectional flow of "cues" which lets you do things like group hover).  Below is an example of Microfold (`style: "..."`) interacting with Alloy (`^` things) and Nitrene (`vertical: ^(...)` etc.)

```wwml
(group style: "flow-row vertical:flow-col px-3 py-3/2 -@only:border-b-sm -@only:border-theme-overlay in-focused:bg-theme-surface"
       vertical: ^(= preferred-direction vertical)
   (group style: "flow-row gap-3/2 fr-1"
     (icon ^icon style: "center-y")
     (p ^path style: "fr-1 leading-none"))
   (p ^*center style: "flow-row gap-3/2 fr-2")
   (p ^*right style: "flow-row gap-3/2"))
```

#### UI (pure-ish)

**Scenery** is a vector graphics and layout engine. Microfold is lowered to Scenery, emitting descriptions of the UI containing things like `img`s, `svg`s, `text` nodes, `x-stack`s and `y-wrap`s.

Scenery, from the outside, is a black box which turns those descriptions into arrays of pixels (i.e., an image) and a symbolic visual description of the scene (a term which nodes can use to ask questions such as, "am I visible?", "is the mouse over me?", "how wide am I in pixels?", etc.)

I call it pure-ish because it is pure on the conceptual level (it is a function from the description of a scene to an image and a visual description); however, internally, it uses things like FreeType, schedules file reads and the like; in that sense it is impure.

### Symbolic physics

**D7** can be called a symbolic physics engine. It lets you define hypergraph rewrite rules; collections of such rules are called *rewrite regimes*. So D7 lets you define such regimes, and apply them to a term called the *circuit*. D7 recognizes *nodes* and edges in the circuit, builds a hypergraph, rewrites it, handles conflicts, manages caching, and so on. D7 does not implement general hypergraph rewriting, as that is too complicated computationally and is NP. It turns out most interesting stuff can be encoded using the hub and spokes topology, so D7 is more or less a matcher for that. D7 is an internal of the project at the time of writing, there's currently no user-facing interface for it.

**Rack** is a rewrite regime defined using D7. It introduces concepts like `cell`s, `backsys`tems (a system of backmaps) and so on. Rack is one of the "unifying" components, a hub which connects and interacts with many other components (I'd say most components if one excludes IO).

### MuSoma (impure)

**MuSoma** is the "grand unifier". It groups everything together into a single, coherent thing: Scenery & Microfold for UI, Rack and D7 for symbolic physics, something called **editR** (editor rewriter), implemented using Rho, for editing inside a symbolic "world".
In a sense, MuSoma is a "lab" which lets you watch and experiment with a symbolic world -- the circuit.

MuSoma introduces impurities into an otherwise pure and sealed system defined by the components I described above. MuSoma *perturbs* the symbolic world with OS-level events, and lets the symbolic world perturb the OS. MuSoma makes it possible for the symbolic world react to various OS-level events, talk to databases, spawn windows, track the mouse & the keyboard, watch files, and so on.

If you imagine the other components of Wirewright as a pure, sealed "guest world" and the OS as the "host world", then MuSoma would be the simulation layer which makes the guest world talk to the host world and vice versa.

At the highest level, MuSoma looks like a hybrid between an event loop and a game loop. At lower levels it implements the "glue" so that all the components of Wirewright can communicate with each other.

Somewhat orthogonally to the above, MuSoma is also an application, with a GUI, modes, key bindings, etc. It is responsible for producing projections of the circuit, including those ones used for UI. MuSoma connects nodes, through their projections, to symbolic feedback about how they look (which is one of the outputs of Scenery).

## References

### Inspiration

Wirewright is inspired by various ideas from these amazing people: Francisco Varela, Humberto Maturana,
Stephen Wolfram, Niklas Luhmann, Michael Levin, Bret Victor, ... (this list will grow, there are many
more of them, and I keep finding more!)

Since the project is in active development right now, it is very early to attribute things
precisely. Hopefully, this would be possible later.

### Misc

Wirewright's Microfold is heavily inspired by [Tailwind CSS](https://tailwindcss.com/)

Wirewright MuSoma includes colors from the following themes.

- [Rose Pine](https://rosepinetheme.com)

## Running Wirewright

You should hopefully be able to just [download the latest release of MuSoma](https://github.com/wirewright/wirewright/releases/latest).

It's an archive which you will have to extract. Inside the archive are an AppImage and some miscellaneous files, in particular
the examples. You should make the AppImage executable if it isn't already:

```console
chmod +x musoma-x86_64.AppImage
```

Then you should be able to run one of the examples.

```console
./musoma-x86_64.AppImage examples/calculator.musoma.wwml
```

If you want to start with an empty file, you'd probably want to have an editor in it. So create
the file and put the editor in it:

```console
echo '((I modify: true structural: true multiline: true compose: true) ("" * "") 0)' > /tmp/example.wwml
```

Then you can run MuSoma.

```console
./musoma-x86_64.AppImage /tmp/example.wwml
```

If you hit `Shift-R` in normal mode (the app starts in it), you'll be able to edit `/tmp/example.wwml`
and MuSoma will live-reload. Any running state will be lost on reload, however.

> [!NOTE]
> Wirewright doesn't yet quit when you close the window. You'll have to Ctrl-C by hand. Sorry.
> This isn't exactly a bug, more of a "design complication". MuSoma can be thought of as a "server"
> which "watches" the file you give it. When you close the window, well, you close the window... One
> action among many, that's it. If the program wanted to react to that, it should be able to, since
> the window, like many other things in Wirewright, is too a symbolic object, which the program
> can observe.

## [State of the] Docs

Hover over things to learn more about them in the MuSoma app (or, well, at least about things I've
bothered implementing tooltips for...)

The main things of interest right now are the examples. See the `examples/` directory.

If you want to see even more bizarre APLish / symbolic notation-ish incantations, feel free
to visit the `runtime/codices` directory. If you're interested in how those "incantations"
do (or relate to?) interactivity, go to `runtime/soma.lib.wwml`.

For the latter, you are advised to use MuSoma, by the way, so that everything renders nicely.
Consider opening MuSoma with the input example `examples/input.musoma.wwml`; then click the arrows &
pan around; this should get you acquainted with symbolic *paradise*! On the latter, now, look, I *love*
the notation, but from the outside, it probably looks like a bizarre mix of APL and Lisp. Wha te ver, huh?

Hit `?` in MuSoma in Normal mode to open the help pane. Right now it's pretty much empty, but
in the future, I hope to make it into a help center with access to the *doctool*.

The *doctool* is a "mythical" thing that some comments in the source code refer to. Right now,
it basically doesn't exist. However, I do write docs that the doctool will eventually be able
to find. The best way to find them right now is to be the doctool yourself; you can search for
`# |@ ` (for Crystal files) or `;; |@ ` (for WwML files) to learn more.

You probably won't be able to program much using MuSoma for now, so again, you can look at
the examples. I will try to write tutorials but I'm pretty bad at writing, and I'm not a native
speaker, so I'm not sure how that'll go. Note that the videos on the YouTube channel are highly
outdated at this point, and are only of project-historical relevance.

Note also that you check out the tests in `tests/` to get a feel of what the system is capable of
at the moment, and the approximate scope of the project. The names may all sound a little bit weird,
but that's not something I can control, unfortunately; the names work, they're short, and have nice
abbreviations; so I'm all for them. Referring to things with foo, bar, X, Y, or Greek letters is
more or less in the past at this point, although some components of the project are still named
this way.

## Hello World(s)

> [!NOTE]
> This is for MuSoma, which is a more "advanced" part of Wirewright. I recommend you to read
> and follow the tutorials first (see above).

If you're afraid of the MuSoma editor, and you probably should be given the amount of
shorthands it has accreted, just look into `runtime/codices/editR.codex.wwml`, well,
in that case you can use your favorite editor. To do so:

- Create a `.wwml` file anywhere and open it with your editor.
- Open MuSoma: `./musoma-x86_64.AppImage path/to/file.wwml`.
- Hit Shift-R to enable live-reload. It's disabled by default because it can lead to
  losing runtime state. In Wirewright, we have "seeds" and evolutions of those seeds.
  Think Minecraft or Game of Life. If you change the "seed" in Minecraft, all your buildings
  are gone. Doing live-reload is similar in MuSoma.
- Now you can edit the file and see the changes reflected in MuSoma. This is, by the way,
  how I wrote most of the complex examples. Unfortunately (for me...?) the built-in editor
  isn't as robust yet, nor is pretty printing.

Again, I must remind you to run the AppImage from terminal; because eventually, you'd
need to Ctrl-C it.

### Basic controls

- In the right pane (the circuit pane) you can pan around by pressing
  the Left button and dragging.
- You can also zoom in/out using the mouse wheel while over the circuit
  pane. Use the Middle button to reset zoom.
- In Normal mode, *you* control the pan. In Insert mode, *the editor* in
  the circuit controls the pan. Imagine the circuit pane as a "camera", which
  follows your instructions in Normal mode, but tracks/follows the editor
  in Insert mode. You can still pan/zoom in Insert mode, but the "camera"
  will force the editor to remain in focus, preventing you from zooming or
  panning it out of view.
- Use `+` and `-` in Normal mode to increase / decrease REM, which is the root
  font size on which most of the UI depends. Note, however, that certain designs/
  examples may start overflowing & get clipped as you increase the size,
  in particular because the window size in the left pane is defined in pixels...
  but, I digress, I know.

### Osc

If you're brave enough to experiment with Wirewright "from scratch", here are some Hello Worlds
that you can type.

Open an empty file like I described above. Hit l in Normal mode to type (the use of keys h-l
for left-right is from the Vim tradition, since it's in my muscle memory). You can hit Esc
to escape Normal mode.

Type the following (note that copy-paste doesn't yet work, probably for the better :^)

```wwml
(cell @x 0)
(cell @y)
(feed @x @y @x)
```

Hit space to escape from dictionaries: `I)` (`I` denoting the editor), hit space,
the editor will go to `)I`.

When you exit the feed node, assuming you've been writing stuff in the same order,
you should be able to see *oscillation*: `0` going back and forth.

Hit Esc to go to Normal mode. Hit Space to pause time. Hit Space to unpause time.
You can navigate history using `,` and `.` in Normal mode.

### Traditional

Type:

```wwml
(window
  (p "Kaixo, mundua!"))
```

You'll see a window appear to the left containing the text.

### Files

Type (replacing /any/path with some path, e.g. `/tmp`):

```wwml
(path-report "/any/path")
```

**PLEASE hit Esc to go to Normal mode if you have epilepsy at this point**,
the thing sometimes gets into a feedback loop that I'm yet to debug, related
to how MuSoma pans around to follow the editor automatically.

This "Hello World" shows a live file system view. You can try to add files/
directories and see MuSoma display the changes, live. If you go to Insert
mode (`l`) and navigate with the arrow keys (if you're following this letter-by-
letter that should be just Left), you'll see that the view is just a huge (or
small, depending on the directory you picked) symbolic object, which is updated
live by Wirewright.

You can use Ctrl-Backspace to remove the term before the editor in Insert mode.
For example: `a (path-report "/tmp" ...) I b`, hit C-backspace, `a I b`.

Similarly, you can try:

```wwml
(path-reading "/path/to/file")
```

Which is basically the closest Wirewright gets to something like Python's `open(_, "r")`.
It's live, too, so if you edit the file, you'll see the content change in MuSoma too.

## Building Wirewright

Wirewright currently only runs on Linux.

### Building with Docker

This is probably the easiest way to build Wirewright. Note that I'm not an expert
on Docker, so the Dockerfiles may not be the best ones on the planet.

You can use the `build-mu` shell script:

```console
sh build-mu.sh
```

It will eventually output musoma-dist.tar.gz. The archive contains the AppImage and
miscellaneous files.

> [!NOTE]
> AppImages built this way may produce a bunch of strange OpenSSL errors on
> some distros, which prevent networking from working in Wirewright. I'm not
> sure what the errors are caused by yet, but it feels like something is getting
> hard-coded somewhere at build-time, in the container, that is then incompatible
> with the distro the AppImage is run on. In my case, it's Manjaro vs. the standard
> Crystal Debian Docker image. The whole point of AppImages is destroyed by this,
> I guess; but that's the state of software in 2026; *obviously* we're mere steps
> from "artificial superintelligence", huh?

### Building without Docker

Wirewright can be built with Crystal 1.20.0 or later. If I forget to update the version number here, please
remember that Wirewright more than likely depends on the newest features and bug-fixes in Crystal. So you are
advised to build Wirewright with the latest version of Crystal.

### Dependencies

You will need to install [Crystal](https://github.com/crystal-lang/crystal) before building Wirewright.

Wirewright requires the following libraries. You should install them before building
the project. Most Linux distributions have these in their package registry.

- [SDL2](https://www.libsdl.org/) (but we are migrating to SDL3, so it should also be installed)
- [FriBidi](https://github.com/fribidi/fribidi)
- [FreeType](https://freetype.org/)
- [HarfBuzz](https://github.com/harfbuzz/harfbuzz)
- [GMP](https://gmplib.org/)
- [PCRE2](https://github.com/PCRE2Project/pcre2)
- [SQLite3](https://sqlite.org)

If you get a linker error, this probably means I forgot to include something in
the list above. Let me know if that's the case so that it can be made more accurate.

Wirewright vendors the following libraries (see the vendor/ directory):

- [PlutoVG](https://github.com/sammycage/plutovg)
- [PlutoSVG](https://github.com/sammycage/plutosvg)
- [Raqm](https://github.com/HOST-Oman/libraqm)
- [Unibreak](https://github.com/adah1972/libunibreak/)
- [XXhash](https://github.com/Cyan4973/xxHash)

> [!NOTE]
> Wirewright vendors `.a` files that I built on my machine. I didn't set anything
> while building them so they should run fine as long as you're on x86-64. However,
> this is really brittle unless you happen to have the same versions of dependencies
> as I have or later. So:

If the linker or something else explodes with weird errors, this probably
means `.a`s shipped with Wirewright are junk for your machine, for whatever reason --
modern tech is complicated enough, I suppose. So you may need to build them yourself.

Each package in vendor/ is structured reasonably well (... I guess?) to answer
any questions you might have, such as which version of the package to build. Some
packages have the patched code there (for transparency, I include a PATCHES file
as well). Others don't: you'll have to find their code and clone it yourself,
according to the VERSION file. Afterwards, simply replace the `.a`s shipped
with Wirewright with your ones.

An alternative route for you is to inspect Dockerfiles in build/ and see what you
have to install and do. One notable detail is that you can build with the `syslibs`
flag (`dev g <preset>; dev flag syslibs`) to use system libraries instead of
the vendored ones unconditionally, which may be helpful if you want most control.

### Installing shards

```console
shards install
```

### Building the dev tool

Wirewright uses a custom dev tool to manage the various subprojects inside the repo. You can build
the dev tool with:

```console
crystal build src/dev.cr --progress --release -Dpreview_mt -Dexecution_context 
```

### Using the dev tool

After running the dev tool build command you should be able to run the `dev` executable:

```console
./dev
```

There are several *presets* available. You can print them with:

```console
./dev g
```

Right now, the only interesting presets are `tests` and `musoma`. Switch using:

```console
./dev g tests # or musoma
```

And build with:

```console
./dev b
```

You should then have the `testtool` executable, which will run tests in the `tests/` directory.
Or `musoma`, which is, well, MuSoma.

## Want to learn more?

Visit the YouTube channel of Wirewright for videos about Wirewright: [Wirewright — YouTube](https://www.youtube.com/@wirewright).

