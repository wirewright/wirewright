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

Wirewright, with its self-embodied programs, is also exploring the idea of a "holistic" approach to life; in that life should be interpreted as embedded and inseparable from its environment. In effect, what I am saying is that us placing a boundary between a living organism and the environment is actually a mistake; and that such a boundary in fact does not exist. The given organism and its behaviors must always be considered "in context", and that context often includes the surrounding environment. Otherwise, we see "complexity out of nowhere"; not because this is the case, but because we're looking too narrowly -- just at things within the boundary. It's like trying to understand what an ant does by studying its mandibles excessively, and being surprised by the complexity of behaviors "resultant from the structure of ant's mandibles". The extremal end of holism is to consider "the universe as a unit" -- as the only thing worth studying & designing (if one wants to produce interesting & extremely complex behavior). I am more and more turning into this extremal "school of thought". Wirewright is an experiment to design a "universe" (obviously in a very naive, simplified, and rather "practicalized" way); and see what kinds of behavior I can get out of that. Once the universe is defined, programs (agents, actors, patterns) don’t "run" in a linear, imperative sense. They exist inside that universe, and behavior emerges as a result of the laws you (or most likely, I!) designed. Conway’s Game of Life is the simplest example of approaching this extreme endpoint of holism from an engineering point of view. Wirewright attempts to generalize this, into a general-purpose, practical software platform.

### More of my ramblings

See the ramblings/ directory to read more of my ramblings. None of those are of publishing quality and most are probably going to read like pseudo-scientific nonsense. Sorry.

### Videos

Visit the YouTube channel of Wirewright for videos about Wirewright: [Wirewright — YouTube](https://www.youtube.com/@wirewright).

