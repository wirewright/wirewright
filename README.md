<div align="center">

![Wirewright Logo](https://github.com/user-attachments/assets/3e5dc602-9c8d-412d-a7fa-9e1a0c3b466e)
</div>

# Wirewright

Wirewright is an experimental symbolic physics environment.

## Gallery

### MuSoma

I am excited to announce a new front-end for Wirewright (with the old name :^), MuSoma.

<img width="1200" height="986" alt="An image of the MuSoma app showing a calculator dividing by zero" src="https://github.com/user-attachments/assets/bd7f8462-b4da-4a79-b97c-437f84692b44" />


## Introduction

> [!WARNING]
> This is a multi-year research playground and is highly experimental! It can, and will break more
> than it will work! I don't know if there's any point in this! I don't know what this is "for",
> either. Sorry.
>
> BEWARE: Weird language ahead!

Most modern programming paradigms model computation as verbs (functions, procedures, processes, etc.) acting upon nouns (e.g. data structures). This isn't exactly a problem to solve, but it turns out you can reduce the number of verbs to just one, and the number of nouns to just one.  The only remaining noun is *the world*. The only remaining verb is *the physics*.

Wirewright attempts to model computation as a transformation over an immutable world: `physics(world) -> world'`. The function, `physics`, is not necessarily pure. In Wirewright, *Rack* implements it purely: `Rack(world) -> world'`. *MuSoma* extends Rack with impurities for I/O; so the function becomes, conceptually, `MuSoma(world, io) -> Rack(perturb(world, plan(world, io))) -> world'`. In practice, `musoma` is *a bit* more intricate than this, of course.

The internals of `physics` are irrelevant philosophically. In practice, Wirewright uses (hyper)graph rewriting and simple scans here and there. Conceptually, you can think of `physics` as scanning the world, finding interesting structure in it, and rewriting the world in response (either `world` or the real world, for that matter).

You do not need to write `physics`, `rack`, or `musoma`; Wirewright provides them for you, along with many other things, so all you care about is the world, and structure in it. Moreover, Wirewright encourages you to forget about the physics function altogether, and instead asks you to situate yourself *inside* the world. In this sense Wirewright is very similar to a computer game: all fun is out if all you're doing is thinking about how the game is implemented, what game engine it uses and so on. This "embedded" point-of-view is important for concepts such as *self-embodied programs*.

Since *world* is ultimately a *symbolic world*, `physics` is ultimately *symbolic physics*. Symbolic physics is, then, roughly, the intersection of graph rewriting, symbolic pattern matching, dataflow, logic programming, and constraint satisfaction.

The world is modeled as a hypergraph (with slight inspiration from Stephen Wolfram's physics project, although the way we end up using the hypergraph, I suppose, differs somewhat).

Self-embodied programs (SEPs) are an important concept which eventually led me to symbolic physics. A SEP is simultaneously an algorithm (because of how physics "animates" it) and a structure (because it exists as such in the immutable world). In a sense, a SEP is a program for physics, as if physics was a computer that one could target. The resulting "instructions" are structure (matter) itself.

Notably, self-embodiedness vanishes unless you situate yourself inside the world, so you only see structure. If you are outside, then you can see the physics function, and so, there's no magic and no self-embodiedness; the physics is an interpreter and structure corresponding to the SEP is its state.

Wirewright is primarily for me and is a playground for some of my ideas. Other than that, Wirewright is for anyone interested in symbolic physics, including its applications to deterministic, symbolic AI.

Currently, Wirewright lets you build very simple *circuits* using MuSoma, with its graphical *front-end*. You can look at some examples in the Gallery section below. See the `examples/` directory for more. We can do basic graphics and interactivity. Components other than `Button` remain future work as of now, although implementing them is more or less trivial based on my experience implementing the MuSoma app (which itself uses Wirewright, although in a slightly different way). The problem with buttons, input fields, and other UI widgets is that compared with the core of the project (which is more or less there), they take a very long time to make, while also being incredibly boring and unrewarding ("who cares if you made your own input field, we have input fields already").

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
./musoma-x86_64.AppImage examples/calculator.wwml
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
Consider opening MuSoma with the input example `examples/input.wwml`; then click the arrows &
pan around; this should get you acquainted with symbolic hell! On the latter, now, look, I *love*
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

### More of my ramblings

See the ramblings/ directory to read more of my ramblings. None of those are of publishing quality and most are probably going to read like pseudo-scientific nonsense. Sorry.

### Videos

Visit the YouTube channel of Wirewright for videos about Wirewright: [Wirewright — YouTube](https://www.youtube.com/@wirewright).

