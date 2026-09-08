# Wirewright [![Discord](https://badgen.net/badge/icon/discord?icon=discord&label=Wirewright)](https://discord.com/invite/bkgmGU7FFQ)

<img width="300" alt="Wirewright MuSoma Logo" src="https://github.com/user-attachments/assets/0bb06a8f-8ddf-4e49-99fe-0c0b1f44b760" align="right" />

What if a program was not a sequence of instructions, but a world? A world you can populate with things, then watch how they evolve one step at a time — how they give rise to new things as old things disappear and change; a world you can poke *as it evolves*, introducing new things and removing old ones.

Wirewright is an experimental research project built around this idea.

Programs in most programming languages ultimately look like this:

```text
do A;
do B;
do C;
```

Even in cases where the surface appears declarative, there is still a machine down there and you're telling it how to carry out a task. The machine can be a CPU, or it can be a virtual machine — JVM, BEAM, or something else.

Wirewright is different. A Wirewright program is effectively a serialized snapshot of a world:

```wwml
(cell @x 100)
(cell @y)
(feed @x @y)
```

This snippet is not a piece of code in the conventional sense. It isn't a sequence of instructions and there is no machine that can "execute" it. Instead, it is a description a symbolic world, frozen in time. In this world, there are three *things*, called *nodes*: two `cell`s and one `feed`. One of the cells holds the number `100`.

The snippet above is what Wirewright calls *a seed*, borrowing from terminology used in the field of cellular automata. Cellular automata is a good reference point for understanding what Wirewright is. Roughly speaking, Wirewright is an automaton which uses *symbols*, *numbers*, *strings* etc. instead of *on/off* for state, and *trees* instead of grids for spatial arrangement. Importantly, trees can also be used for state.

Wirewright can also be seen through the lens of a game analogy. Imagine Wirewright as implementing a game, but instead of entities you have data structures, and instead of graphics you have extended S-expressions; and physics is not about boxes and vectors and collisions and other kinds of math, but about making data structures interact with each other in various ways through rewrite rules.

In Wirewright:

- Buttons and inputs have insides, the same way a button in the real world is a box with a mechanism inside it. A Wirewright button is a tiny "organism" containing a symbolic model of the mouse. The button is capable of observing this model as well as a symbolic description of its own visuals, and reacting to the behavior of the user.
- Programs can look around and move!
- Editors inhabit the same world they edit.
- Sorting is understood as a law that reduces disorder.
- Some of the core distinctions of modern programming do not apply. The same _term_ can act as data, state, code, and UI.
- Instead of *evaluation*, Wirewright has *evolution* and *simulation*.

## An overview of Wirewright

Wirewright is an *ecosystem*. The major components of this ecosystem are:
- **Data and syntax**: Terms (immutable values such as numbers, strings, dictionaries, etc.), WwML (a notation for expressing terms)
- **Pattern matching, transformation, and evaluation** of terms: Alloy (a structural templating language, a bit like JSX), Nitrene (an expression language, a bit like Excel formulas), M1 (a pattern matching engine).
- **Styling and graphics**: Microfold (a styling engine; imagine if CSS went the Tailwind way, that's roughly what Microfold is), Scenery (layout and graphics).
- **Symbolic physics and IO**: Rack (symbolic physics), Harmony (something remotely resembling Kubernetes but for IO).
- **Interface**: MuSoma (an interactive graphical environment for Rack), irack (a command-line interface to Rack).

## Gallery

### Calculator

This is an example of an interactive calculator. The UI manipulates "AST" directly instead of working with strings. The app is about ~200 lines of code, *including* comments, blank lines, etc. Starting from the middle of the video I show time-travel.

https://github.com/user-attachments/assets/5ec961a9-61f9-4761-91a3-428de10b709f

Reference: `examples/calculator.musoma.wwml`.

### SQLite

A very simple app that uses an SQLite database. The point is to show how databases can be represented in the symbolic world: as a black box that takes queries and spits out responses after some time. Starting from the middle of the video I show time-travel (which is pure; i.e., the database is not changed as I move through time).

https://github.com/user-attachments/assets/63624868-23b3-4990-9993-90dfd700d85e

Reference: `examples/sqlite.musoma.wwml`.

### Bounce

This is an example of a simple "moving" program. The `circuit` defines the boundaries of a "symbolic world". The backsystem `backsys` defines some "laws". And the `module` inside the world implements a basic bouncing behavior. The module is subjected to a mix of "laws" defined in the `backsys`, and the laws of Rack, which is the "ultimate" physics here, responsible for animating `backsys`, `circuit`, etc. themselves.

https://github.com/user-attachments/assets/8c4d54ae-669a-49fd-b9c5-4ff2528b3c33

Reference: `examples/bounce.rack.wwml`.

### Merge sort

This example demonstrates how merge sort (or something very much like it...) can be imagined as a symbolic machine (or a group of symbolic machines).

https://github.com/user-attachments/assets/3213e84f-2696-49a0-a286-5908f77ecbcd

Reference: `examples/msort.rack.wwml`.

### WebSocket server

In this example, I time-travel through a small "conversation". You can see how each connection is given its own `device`, and devices interact with each other using `sensor`s and `appearance`s. There is no centralized "chat" state inside the program; *the symbolic world itself* plays the role of centralized state, so to speak. 

https://github.com/user-attachments/assets/a3141c05-4242-4b40-8a05-f2c632a659f3

Reference: `examples/websocket-chat.rack.wwml`.
## Running Wirewright

You should hopefully be able to just [download the latest release](https://github.com/wirewright/wirewright/releases/latest).

It's an archive which you will have to extract. Inside the archive are:
- `musoma-x86_64.AppImage`, an AppImage for MuSoma, a GUI for Rack.
- `irack`, short for *interactive Rack*, a static binary which you can use to interact with Rack directly, if MuSoma doesn't work for you (e.g. you're on a server or using WSL), is too slow, or feels bloated.  `irack` is used in some of the basic tutorials.
- `doctool`, a static binary to serve Wirewright docs as `127.0.0.1:9811`.
- `examples/`, the examples directory (the same one as in the repo).

To run MuSoma, make the AppImage executable if it isn't already:

```console
chmod +x musoma-x86_64.AppImage
```

Then you should be able to run one of the examples.

```console
./musoma-x86_64.AppImage examples/calculator.musoma.wwml
```

If you want to start MuSoma with an empty file, you'd probably want to have an editor in it. You can create the file and put the editor in it like so:

```console
echo '((I modify: true structural: true multiline: true compose: true) ("" * "") 0)' > /tmp/example.wwml
```

The `((I ...) ...)` thing *is* the editor, and you've just placed it in the world.

Then you can run MuSoma:

```console
./musoma-x86_64.AppImage /tmp/example.wwml
```

If you hit `Shift-R` in normal mode (which MuSoma starts in), you'll be able to edit `/tmp/example.wwml` and MuSoma will live-reload. Do note that any running state will be lost on reload.

> [!NOTE]
> A notable inconvenience which is actually a feature for now is that Wirewright doesn't quit when you close the window (by pressing "X"); it only hides the window. You'll have to Ctrl-C by hand. 
> 
> This isn't exactly a bug, more of a "design complication". MuSoma can be thought of as a "server" which "watches" the file you give it. When you close the window, well, you close the window... One action among many, that's it. If the program wanted to react to that, it should be able to, since the window, like many other things in Wirewright, is too a symbolic object, which the program can observe.
## Docs

### In MuSoma

Hover over things to learn more about them in the MuSoma app (or, well, at least about things I've bothered implementing tooltips for...)

The main things of interest right now are the examples. See the `examples/` directory.

If you feel like exploring, look into the `runtime/` directory and the `*.wwml` files in it.

Consider opening MuSoma with the various examples and clicking the arrows and panning around. 

Hit `?` in MuSoma in Normal mode to open the help pane. Right now it's pretty much empty, but in the future, I hope to make it into a help center with access to the `doctool`.

#### Basic controls

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

### For Wirewright in general

Use the `doctool` binary that is shipped with the latest release. It serves docs, which are baked into the binary. It serves them at `127.0.0.1:9811`. Just run the doctool using something like `./doctool`. The doctool is a static binary which doesn't depend on anything else.

### Misc

Note also that you check out the tests in `tests/` to get a feel of what the system is capable of at the moment, and the approximate scope of the project. The names may all sound a little bit weird, but that's not something I can control, unfortunately; the names work, they're short, and have nice abbreviations; so I'm all for them. Referring to things with foo, bar, X, Y, or Greek letters is more or less in the past at this point, although some components of the project are still named this way.

## A deep dive

So what is Wirewright? Good question. As a software project (as opposed to a philosophical endeavor of mine), [my definition of Wirewright](https://youtu.be/rkWXB-3ReV0) is an ecosystem of components which together support a particular "style" of symbolic computation -- one that is heavily inspired by physics. I call this "style" *symbolic physics*.

At the foundation of Wirewright are **terms**. All terms are immutable. There are six types of terms: numbers, strings, booleans, symbols, dictionaries, and blobs (for binary data). Dictionaries are of most interest. Conceptually, a dictionary is a list of *entries*, where each entry is the pair `(key, value)`, and keys are unique. An entry can be an *item* (its key is 0 or a successor of an item key), or a *pair* (all other entries). Items therefore form a chain called the *itemspart* (e.g., keys 0, 1, 2, etc.) The rest of entries form the dictionary's *pairspart*.

A term is an abstract object, a kind of "Platonic" entity. The machine represents them with bits and bytes and trees in memory, but for a human, a *text* representation would be much more useful. Wirewright Main Language, WwML for short (and if you want it even shorter, ML), is such a representation.

**WwML** started as S-expressions extended with key-value pairs, e.g. `(/ 1 2 precision: 3)`. Over time, it evolved into a notation with *a lot* of shorthands, so much so that sometimes it stops looking like S-expressions at all:

```wwml
(limit _ ⍊ up-w⫽h: (arg ±λ ⍊ -◇_) ±⟦min,max⟧-w⫽h)
  <> {λ: ^(⟦max,min⟧ ⟦min,max⟧-w⫽h λ), ◇: true}
```

Much of WwML doesn't look like this, though; this is an exaggeration to prove my point about shorthands. It looks like a normal language, even if a little bit *weird*. See the examples below for how "normal" stuff looks like, as well as the `examples/` directory, `runtime/`, and `tests/`. These directories are full of WwML code so you can acquaint yourself with it "in its natural habitat".

Terms can be matched and transformed using M1, a pattern matching and *backmapping* engine. If Wirewright was an organism, M1 would be its sensory organ.

**Alloy** is a structural templating language. Structural templating is like Lisp's `quote`, `unquote`, `quasiquote`. It can also be compared with something like JSX. Alloy looks roughly like this:

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

Here, things starting with `^` pertain to Alloy.

**Nitrene** is more conventional: it treats a term as a conventional program, with function calls and such. It is called an *expression language*, and plays a role similar to the one played by formulas in Excel. In the Alloy example above, expressions such as `(= mode expects-mode)`, `(words text)` and so on are Nitrene.

**Microfold** is part of the UI stack. It implements Tailwind-like styling among other things (such as e.g. "cue flow", a bidirectional flow of "cues" which lets you do things like group hover).  Below is an example showing Microfold (`style: "..."`) , Alloy, and Nitrene simultaneously:

```wwml
(group style: "flow-row vertical:flow-col px-3 py-3/2 -@only:border-b-sm -@only:border-theme-overlay in-focused:bg-theme-surface"
       vertical: ^(= preferred-direction vertical)
   (group style: "flow-row gap-3/2 fr-1"
     (icon ^icon style: "center-y")
     (p ^path style: "fr-1 leading-none"))
   (p ^*center style: "flow-row gap-3/2 fr-2")
   (p ^*right style: "flow-row gap-3/2"))
```

**Scenery** is a vector graphics and layout engine. Microfold is lowered to Scenery, emitting descriptions of the UI containing things like `img`s, `svg`s, `text` nodes, `x-stack`s and `y-wrap`s. Scenery is then tasked with converting those descriptions to arrays of pixels.

**Rack** is a specimen of *symbolic physics* defined using Delta7, D7 for short. The latter is related to Rack in the same way a trigonometry or integration library is related to a physics engine. Rack introduces concepts like `cell`s, `backsys`tems (a system of backmaps) and so on. Rack is one of the central components of Wirewright. In fact, in many descriptions of Wirewright (such as the one in the very beginning of this readme), we are actually talking about Rack. 

**Harmony** is the thing managing servers, clients, and other kinds of IO for Rack — restarting them and so on

Finally, there's **MuSoma**. It is an interactive graphical environment for Rack. Like some systems divide themselves into a *graphical interface* and a *kernel*, Wirewright has the division between MuSoma and Rack.
## Tutorials and guides

See the `guides/` directory.

- [Wirewright: The Guide (Part 0)](<guides/Guide (Part 0).md>)
- [Wirewright: The Guide (Part 1)](<guides/Guide (Part 1).md>)

## Building Wirewright

> [!NOTE]
> Wirewright currently only runs on Linux.

### Building with Docker

This is probably the easiest way to build Wirewright. Note that I'm not an expert
on Docker, so the Dockerfiles may not be the best ones on the planet.

You can use the `build-mu` shell script:

```console
sh build-mu.sh
```

It will eventually output musoma-dist.tar.gz. The archive contains the AppImage and miscellaneous files.

> [!NOTE]
> AppImages built this way may produce OpenSSL errors on some distros, breaking Wirewright's networking.
> 
> The hacky way to fix them is to run MuSoma (or `irack`) with the SSL_CERT_DIR environment variable pointing to your distro's SSL certificates directory. See also: https://github.com/rustls/openssl-probe/blob/main/src/lib.rs
> 
> Hopefully I'll have time to fix this sometime. The idea is to do what the Rust library I've linked does, as far as I understand.

### Building without Docker

Wirewright can be built with Crystal 1.21.0 or later. If I forget to update the version number here, please remember that Wirewright more than likely depends on the newest features and bug-fixes in Crystal. So you are advised to build Wirewright with the latest version of Crystal.

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
means `.a`s shipped with Wirewright are junk for your machine, for whatever reason -- modern tech is complicated enough, I suppose. So you may need to build them yourself.

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
crystal build src/dev.cr --progress
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

To build the doctool, first, generate the articles.txt file:

```console
./dev g docdump && ./dev r
```

Then build the doctool:

```console
./dev g doctool && ./dev b
```
## References

I think Wirewright combines a lot of existing ideas into something coherent.

The relevant ideas come mainly from the field of *cybernetics*: see, for instance, Francisco Varela, Humberto Maturana,  Niklas Luhmann, Norbert Wiener.

Another bunch of ideas come from cellular automata, in particular from Stephen Wolfram's A New Kind of Science book and his Physics Project.

There is a lot of interesting ideas in the works of Michael Levin and Joscha Bach. 

Bret Victor demonstrated what interactivity means, and Wirewright tries to follow his advice.

*Obviously* I must reference Casey Muratori and Jonathan Blow. The ideas of Alan Key have proved useful as well.

The *term* side of the project and the project's focus on immutability would not have been possible without the work and talks of Rich Hickey.

Wirewright is influenced and inspired by biology, in particular cellular biology and neurobiology.

There are significant influences from philosophy as well, especially from the fields of philosophy of consciousness, phenomenology, and metaphysics.


Since the project is in active development, it is very early to attribute and link things precisely. Hopefully, that would be possible later.

The project did not start with a "research" phase but rather with improvisation, so it is hard to pinpoint exactly what influenced me and where it comes from. I am sure no part of Wirewright is truly new. A lot of things Wirewright touches are well-studied in academia: in automata theory, in term rewriting, in reactive systems, transition systems, and so on. I am not smart enough to explore these topics deeply, so all I know about them are the names and their very loose meanings. The *synthesis* of the aforementioned ideas, and the *practicality* of Wirewright is where my hopes on the project's usefulness are.
### Misc

Wirewright's Microfold is heavily inspired by (and in some places copies!)  [Tailwind CSS](https://tailwindcss.com/)

Wirewright MuSoma includes colors from the following themes.

- [Rose Pine](https://rosepinetheme.com)
## Want to learn more?

Visit the YouTube channel of Wirewright for videos about Wirewright: [Wirewright — YouTube](https://www.youtube.com/@wirewright).
## Feedback

I'd be happy to know what you think about Wirewright and answer any questions; a lot of documentation is in the *unwritten* state right now so I'm the only reliable source. You can use the GitHub Discussions pane or Discord, whichever one you like most. If you learn my email address, you can write there too.
