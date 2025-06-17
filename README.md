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

This is not only a metaphor. Wirewright implements a certain kind of *rewrite physics* -- so called *rule systems* that transform terms, inspired by the work of Stephen Wolfram, cellular automata, term rewriting systems, propagators, and more.

At the core of Wirewright is the environment's ability to change -- *rewrite* -- this passive structure made up of terms. Thus we get what I refer to as *indirect self-modification*. The structure does not mutate itself. It is transformed by a *world* that interprets its shape through pattern matching; its embedded information, and sometimes even its symbolic self-description.

To reiterate, this is not self-modifying code; not in the usual sense. Self-modifying code attempts to rewrite itself from within -- often unsafely, and with little control. It is easy to see why (or, well, it is not!) There are many paradoxes arising immediately when one considers self-controlled self-modification.

A structure in Wirewright cannot modify itself at all. But it can be *modified*, in complex, meaningful ways, *by an environment that understands what that structure is*.

In Wirewright, programming means building shapes that invite transformation -- shapes that can be *read*, *interpreted*, and *evolved by* the environment they are put in. What I am writing here -- Wirewright -- is a *rewrite environment*. Your job, then, if you ever plan to interact with this... "alien artifact" sort of thing -- is to *discover* such shapes, to be rewritten by Wirewright with practically meaningful effect.

Wirewright is a little more complex, in that we provide an "overlay" of environments for the same structure of terms. Wirewright can be considered a layered, co-aware system of *interpreters* observing the same underlying passive terms. Some of those observers are written in Crystal (and thus native code); others, are formatted as rule systems implemented with Wirewright itself.

A *self-embodied program*, thus, has a pretty precise meaning: it is such a passive structure, a shape made of terms, that executes an *algorithm* under a given environment (interleaving observers & systems of rules). SEPs persist and change like living organisms in an ecosystem. In simple, "null" cases, a SEP persists forever under the laws of its environment, and its algorithm is "identity". In richer cases, a SEP may contain a symbolic model of itself -- a kind of DNA -- and the world is capable of reading that model and building the next generation from it.

Developing the biological analogy, you can think of Wirewright as a crude kind of *biological cell*. Given my limited understanding of biology, what I see cells do is they provide an environment to the DNA. It is the DNA that is reacting to stimuli; but importantly enough, it does so *indirectly*, through activation and deactivation of certain genes by the environment (*stimulus*), directly or indirectly through gradient or concentration change in the cell and sensing thereof; and transcription, translation, protein synthesis, interactions of proteins and so on to get *reaction*. Epigenetics and DNA mutation gives way to the persistence of certain kinds of reactions.

In Wirewright, even something like a UI button -- which would normally be thought of as a visual element or interactive widget -- is, at its core, just a term, the same kind of passive structure. It has no inherent interactivity or execution semantics. You can literally *type* buttons, by typing the text representation of the term that the button *is*. Thanks to the fact that the environment includes "interpreters" like DwUIR, UIR, and Microfold, such terms can be rendered visually -- being part of a layout hierarchy, styled like Tailwind components, and -- in total -- presented as interactive UI elements.

So one interpreter can see the same term as a button. Another can see it as a self-embodied program, to be read and transformed. The user clicks on the visual part of a button; the environment "senses" that, and -- through a series of chain reactions -- modifies the term that caused the button to be drawn in the first place; in other words, the environment tells the button -- encodes in its own "shape" -- the fact that it was clicked just now. UIR, Microfold, and others do not to user interaction directly; but rather, to the change of the underlying term -- thus, *indirectly*, mediated by the term. This lets the surrounding structure, too, "click" the button -- if it needs to; with the same visual effect. In this sense, it is no different from a "user".

It is a difference of perspectives -- about the same underlying term; static, structural, and eternal-by-itself. In Wirewright, behavior arises not from composition of instructions or functions, but from *composition of observers*; observers of a shared, passive structure -- your "program"; observers that modify the "program"; and thus observers that can communicate with each other, indirectly, through this modification and sensing thereof -- *co-aware* or not.

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

### A rough analogy and some core design principles

Wirewright is designed to be the "body" that runs your programs. We implement "the eyes", "the ears", "the arms", and "the brains". And you implement "the mind".

Wirewright offers a universal "language", so-called *terms*; and complex machinery to map terms to and from various "modalities".

- The "protocol" or "format" for describing terms with text is called WwML.
- WwML parser thus turns strings into terms. WwML pretty printer, in turn, converts terms into strings.
- Meridium subsystem turns terms into network messages with *appearances*, and network messages back into terms using so-caled *sensors*.
- DwUIR turns terms into images. ??? turns images into terms (TODO).
- UIR rewriter turns layout describing terms into DwUIR.
- Microfold turns Tailwind-like style descriptions (also terms) into UIR.
- Delta7, Rhodium, Nitrene are various kinds of "physics" for terms. They're the "brain" of Wirewright.
- M1 is a pattern matching engine for terms.
- Rewrite circuits are a way to combine all of the above. Rewrite circuits are also terms, of course!

The Crystal part (i.e. the implementation) acts as an *observer*. It looks at terms, sees what changed, and reacts appropriately (and most likely, imperatively). In a sense, it is "outside" of the system; it is the magic -- a term structured just the right way is like a "spell", which "materializes" into the result, if any.

Most parts of Wirewright are what I would call *purely declarative*. Think HTML, but for general-purpose computation (not just page/text markup).

Wirewright, with its self-embodied programs, is also exploring the idea of a "holistic" approach to life; in that life should be interpreted as embedded and inseparable from its environment. In effect, what I am saying is that us placing a boundary between a living organism and the environment is actually a mistake; and that such a boundary in fact does not exist. The given organism and its behaviors must always be considered "in context", and that context often includes the surrounding environment. Otherwise, we see "complexity out of nowhere"; not because this is the case, but because we're looking too narrowly -- just at things within the boundary. It's like trying to understand what an ant does by studying its mandibles excessively, and being surprised by the complexity of behaviors "resultant from the structure of ant's mandibles". The extremal end of holism is to consider "the universe as a unit" -- as the only thing worth studying & designing (if one wants to produce interesting & extremely complex behavior). I am more and more turning into this extremal "school of thought". Wirewright is an experiment to design a "universe" (obviously in a very naive, simplified, and rather "practicalized" way); and see what kinds of behavior I can get out of that. Once the universe is defined, programs (agents, actors, patterns) don't "run" in a linear, imperative sense. They exist inside that universe, and behavior emerges as a result of the laws you (or most likely, I!) designed. Conway's Game of Life is the simplest example of approaching this extreme endpoint of holism from an engineering point of view. Wirewright attempts to generalize this, into a general-purpose, practical software platform.

### More of my ramblings

See the ramblings/ directory to read more of my ramblings. None of those are of publishing quality and most are probably going to read like pseudo-scientific nonsense. Sorry.

### Videos

Visit the YouTube channel of Wirewright for videos about Wirewright: [Wirewright — YouTube](https://www.youtube.com/@wirewright).

