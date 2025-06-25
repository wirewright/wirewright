<div align="center">

![Wirewright Logo](https://github.com/user-attachments/assets/3e5dc602-9c8d-412d-a7fa-9e1a0c3b466e)
</div>

# Wirewright

> Instead of asking: "What program should I write?"
>
> You ask: "What kind of universe can I design that causes the behaviors I want to see?"

Wirewright is a rewrite environment for self-embodied programs.

I am working hard to package the hundreds of thousands of lines of "all over the place" code I've written and about two years of ideas and exploration into something simple, usable, and practical. Please wait and wish me a lot of energy :)

## Gallery

### Frontend: soma6

https://github.com/user-attachments/assets/e86cb81d-67d7-45b8-8a68-7399e4fe367e

## Okay, but what *is* Wirewright?

The short answer is, I don't know. Rephrased slightly, *Wirewright is a rewrite environment for self-embodied programs*.

The long answer is, well, it's complicated. I'm trying to achieve *something* with this but I can barely tell what that something is.

In software, programs are usually seen as active agents. Programs are in control of what happens next. The operating system, the hardware, the network -- they are treated as resources that the program manipulates, makes API calls to, and so on. In other words, programs control the environment (or run under the illusion thereof; even "active environments in disguise" -- interpreters, for example -- are designed in such a way as to give reins to the program).

In Wirewright, programs are not active agents. They are passive structures. They don't run -- they exist. It's not necessary for them to be *programs* in the usual sense, even. They are immutable, persistent *shapes* made of universal building blocks called *terms*.

It is the environment that is active now. Behavior is no longer something authored by a program. It is something drawn out of a program by the rules of the world -- the environment, and its way of "looking" at things.

Wirewright implements a certain kind of *rewrite physics* -- so called *rule systems* that transform terms, inspired by the work of Stephen Wolfram, cellular automata, term rewriting systems, propagators, and more.

At the core of Wirewright is the environment's ability to change -- *rewrite* -- this passive structure made up of terms. Skipping ahead *just a tiny bit*, we get what I refer to as *indirect self-modification*. The structure does not mutate itself. It is transformed by a *world* that interprets its shape through pattern matching. Its embedded information, and sometimes even its symbolic self-description, then close the feedback loop -- causing the world to mutate the shape.

A traditional program can technically modify itself or reflect on itself (e.g. via quines, introspection, or metaprogramming); but in practice these are shallow, brittle mechanisms. Wirewright imagines something deeper -- programs whose structure (symbolic; i.e., a composition of identities) is meaningful to observers, where that structure includes hints about how it should be changed. The observer may then realize the change.

This is not self-modifying code; not in the usual sense. Self-modifying code attempts to rewrite itself from within -- often unsafely, and with little control. Self-modifying code is about an active program modifying itself. There are many paradoxes arising immediately when one considers self-controlled self-modification. It is easy to see why -- or, well, it is not! The paradoxes are usually related to unbounded recursion -- in one way or another. You can "bound" recursion, sure; but think about a surgeon operating on his own brain. Wouldn't *that* be dangerous and, well... unreliable, to say the least?

A structure in Wirewright cannot modify itself at all. But it can be *modified*, in complex, meaningful ways, *by an environment that understands what that structure is*. The structure, then, can be *shaped* by the programmer or an automated design process in a way that triggers self-modification *indirectly*, as described above.

In Wirewright, programming means building shapes that invite transformation -- shapes that can be *read*, *interpreted*, and *evolved by* the environment they are placed in. What I am writing here -- Wirewright -- is a *rewrite environment*. Your job, then, if you ever plan to interact with this... "alien artifact" sort of thing -- is to *discover* such shapes, to be rewritten by Wirewright with practically meaningful effect. I'm trying really hard to carry over common programming concepts into this somewhat "new world", so that shouldn't be as hard or *arbitrary* as, say, finding a new glider shape in Conway's Game of Life.

Wirewright is a little more complex, in that we provide an "overlay" of environments for the same structure of terms. Wirewright can be considered a layered, co-aware system of *interpreters* observing the same underlying passive terms. Some of the observers are written in Crystal (and thus native code); others are implemented as rule systems on top of that native code -- in a way, with Wirewright itself.

Defining a *self-embodied program* (SEP), all the above being said, becomes a possibility. It is such a passive structure, a shape made of *terms*, that executes an *algorithm* of some sort under a given environment (interleaving observers & systems of rules). In simple, "null" cases, a SEP persists forever under the laws of its environment, and its algorithm is "identity" (think of the identity function in functional programming; this is its equivalent in SEP-world). In very rich cases -- on the other end of this imaginary "spectrum" -- a SEP may contain a symbolic model of itself -- a kind of DNA -- and the world is capable of reading that model and building the next generation of the SEP from it. Such SEPs may persist and change like living organisms in an ecosystem.

Developing the biological analogy, you can think of Wirewright as a crude kind of *biological cell*. Given my limited understanding of biology, what I see cells do is they provide an active environment to the DNA. It is the DNA that is reacting to stimuli, but it does so passively. It does so *indirectly*, too, through activation and deactivation of certain genes by the environment (*stimulus*), or indirectly through gradient or concentration change in the cell and sensing thereof and so on; and transcription, translation, protein synthesis and interaction and so on to get *reaction*. Epigenetics and DNA mutation give way to the persistence of certain kinds of reactions. Note again how it is the environment that is active, not the DNA itself. The DNA is passively "consulted" with; it is the environment that is in charge of "consulting". The DNA is also a *structure*, in the loosest sense; one can cut it, recombine it, repair it, move it, and so on. Simultaneously, it remains a "program" for the cell's behavior, although only indirectly so.

In Wirewright, even something like a UI button -- which would normally be thought of as a visual element or an interactive widget -- is, at its core, just a term, the same kind of passive structure. It has no inherent interactivity or execution semantics. You can literally *type* buttons, by typing the text representation of the term that the button *is*. Through cooperation of subsystems such as DwUIR, UIR, and Microfold, -- a "composition of environments" is formed around the button. Through this composition, the button term acquires an image, becomes part of a layout hierarchy, and can be pressed with effect.

In one environment, a term is a button. For another one it is a destination for clicks. For a third, the same term is a self-embodied program, to be read and transformed. None of these environments know about each other. The button serves as a shared medium of anonymous interactions between them. The user clicks on the image of the button; the interaction environment "senses" that, and -- through a series of chain reactions -- modifies the button term; in other words, the environment tells the button -- encodes in the button's shape -- the fact that it was clicked just now. UIR, Microfold, and others on the imaging end do not react to user interaction; but rather, to the change of the underlying term -- thus, *indirectly*, mediated by the term. This lets the surrounding structure, too, "click" the button or do anything else with it -- if it needs to; with the same visual effect. In this sense, it is no different from a "user".

It is a difference of perspectives -- about the same underlying term; static, structural, and eternal-by-itself. In Wirewright, behavior arises not from composition of instructions or functions, but from *composition of observers* or *observing environments*. What they observe is a completely passive structure -- your "program". They are capable of modifying it as the result of their observation. And thus, capable of communicating with each other, indirectly, through modification of the shared structure and observation thereof -- exhibiting *co-awareness* (meaning observers modify the term, expecting another observer to see that); or absolutely not *co-aware*. *Co-awareness* in this sense is related to *stigmergy*, as in ants.

## Upcoming "selling points"

**Warning**: Wirewright is several months away from a working [prototype](https://youtu.be/eUkZNk90rbQ). These "selling points" are for the far future.

1. The core idea of Wirewright is to base the entirety of interaction and programming on indirect self-modification; to make self-modification intuitive, nondestructive, and easy to reason about.
2. Wirewright considers itself a logical continuation of Lisp (as an idea, I suppose). Lisp made code and data "translatable" into each other, yet in Lisp they (or rather, their "roles") are still distinct: some lists are code and others, data. Wirewright, on the other hand, attempts to rid of the idea of code altogether. In Wirewright, everything is data ­— and nothing is code. In a way, Wirewright is the "ultimate", “universal program”, to which anything is an input; similar to some solvers out there, but somewhat more general-purpose.
3. Wirewright is remotely related to the general programming approach named "functional core, imperative shell". As such an “imperative shell”, Wirewright integrates all communication and interaction with the outside world into a coherent whole that the self-embodied program can interact with. Communication over the network, access to database, storage, graphics, and more — all by interacting with Wirewright.
4. **µsoma** is a unified graphical user interface to Wirewright.
5. Wirewright acts simultaneously as an *observer* and an abstract kind of *physics* for the "functional core" ­— a self-embodied program. It reacts to changes in the latter and provides feedback through rewriting (but not necessarily; for instance, UI is simply a way to view the self-embodied program, like some weird "glasses" that show `(button "Increment" @actions)` as a rectangle with centered text).

## Running

There's an AppImage build in the releases section. No idea whether it'll work on your machine, I'm a complete noob when it comes to software distribution. The AppImage only contains SFML shared objects, so when you run it, you may get some dependency-related errors. Try to google them and install the corresponding dependencies, I guess. I think it is too early to bother about properly distributing the thing, but I still wanted a way for people to try out µsoma without compiling anything. The AppImage may succeed in this on your machine, or it maybe it won't :^)

Link so you don't have to scroll: https://github.com/wirewright/wirewright/releases/latest

## Building

Wirewright can be built with Crystal 1.16.0 or later. Due to some bugfixes that Wirewright depends on, it is recommended that you build on 1.16.2 or later.

0. You'd probably want to make `dev.sh` executable, if it's not already; something like `chmod +x dev.sh` should work.
1. Run `dev.sh init`. This will run `shards install`; and also point CrSFML to the header files of SFML 2.6.0, found in ext/.
2. Run `dev.sh soma --release` to *build* in release mode. Run `dev.sh soma` to *run* in debug mode.
3. `dev.sh soma --release` will **hopefully** produce an executable named `soma`. That's it.

## Want to learn more?

### More of my ramblings

See the ramblings/ directory to read more of my ramblings. None of those are of publishing quality and most are probably going to read like pseudo-scientific nonsense. Sorry.

### Videos

Visit the YouTube channel of Wirewright for videos about Wirewright: [Wirewright — YouTube](https://www.youtube.com/@wirewright).

