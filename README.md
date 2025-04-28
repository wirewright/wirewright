# Wirewright

Wirewright is a rewrite environment for self-embodied programs.

I am working hard to package the hundreds of thousands of lines of "all over the place" code I've written and about two years of ideas and exploration into something simple, usable, and practical. Please wait and wish me a lot of energy :)

## Gallery

### Frontend: soma6

https://github.com/user-attachments/assets/e86cb81d-67d7-45b8-8a68-7399e4fe367e

### Frontend: pprint2_vis

A 45-minute video where I try to explain (and fail, I guess, given it's 45 minutes?) what some of this is.

[Wirewright as an alternative to compilation and interpretation: building a counter ­— YouTube](https://youtu.be/SQP96xtfLvc)

NOTE: stuff is much snappier in reality, GIFs compress a lot of that snappiness.

**Interactivity in µsoma**

![Interactive experience in µsoma](img/interactivity.gif)

**A jumping self-embodied program ("ping-pong")**

![A jumping program](img/jumping_program.gif)

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

The pprint2_vis frontend is scheduled for removal so you probably don't want to build it.

Wirewright can be built with Crystal 1.16.0 or later.

1. `shards install`.
2. `crystal build soma6.cr --release -Dpreview_mt -Dexecution_context -Dsoma6 -o soma`.

And do not forget to hope really really hard for success!

## Want to learn more?

### What is Wirewright?

I find the question “What is Wirewright?” surprisingly hard to answer.

One attempt could be to say that Wirewright is everything I’ve been working on and thinking about for roughly two years already, more or less full-time.

Wirewright is a bunch of ideas about ways to exchange messages in a truly distributed, decentralized, amorphous setting (that is, in a setting where no peer knows about “the whole” it’s part of; but rather, each peer is exploring the whole, “feeling it out”). In fact, that's the source of the project's name.

Wirewright is also a bunch of ideas about term rewriting, pattern matching, rule systems, rewriter circuits, and so on, taking inspiration from (among many other things) the work of Stephen Wolfram on physics and computation, and on his company’s Mathematica. Linked to that is my work in trying to bootstrap modern-looking UI using rule systems and a thin layer of native code for drawing. I’m still in the very beginning on this one. Naive me thought modern UIs are simple 😣 And in all honesty, I’m surprised it is running at all!

Wirewright is also an effort to prove that rule systems, which are, in my opinion, one level of abstraction above traditional programming (whatever that means), can run at least as fast as modern interpreters (e.g. Python). This performance effort — quite sadly! — is at zero percent progress right now. But hopefully there will be some progress on it in the future. I truly believe one can make immutability, persistence, rewriter circuits, and rule systems as fast as modern interpreters. Modern interpreters (pure interpreters, that is; no JIT) are obviously much worse (sometimes thousands of times worse) than native code. That’s still a very ambitious goal when talking about rule systems, though; seeing how they plough through hundreds if not thousands of complex patterns for even the smallest things, sometimes recursively, sometimes exhaustively, but most of the time, both — and more!

The second answer is, Wirewright is about self-embodied programs. That’s a fairly cryptic term, and, I must confess — one that I’m not able to define as clearly as they do in textbooks yet. But, more visually, I can define self-embodied programs as moving programs, or even go so far as to say self-embodied programs are living programs — programs that can modify themselves in reaction to stimuli. In other words, self-embodied programs are about *embodied response to stimuli*. In such cases response perceived by an observer is only an indirect effect of what is in fact self-modification.

The latter is particularly weird. Imagine an observer looking at Bob. Bob is moving his arm, perhaps in a greeting gesture. There are two ways to look at the matter. One, is that Bob is "moving his arm". One may consider in such case, that there is a certain algorithm with an input in the Bob's brain, and so on; nerves carrying carefully arranged signals down the spinal cord and to Bob's arm of choice, muscle cells responding. A chain reaction. Another, is to look at Bob wholly; to consider that Bob is undergoing self-modification on a potentially whole-body level that manifests itself, externally as Bob's arm moving so and so.

Wirewright is an attempt to craft an environment for such programs — which means, for example, the “laws of physics” that these programs will follow. In this regard Wirewright takes inspiration from Conway’s Game of Life and from cellular automata in general.

Wirewright’s approach to programming is, let’s create a world. Some laws of physics here, some “basic building blocks” there, then let’s build a program out of those building blocks (a self-embodied program); and then sit back, relax, and enjoy the show. Obviously not the most safety-critical kind of programming; you won’t program an autopilot or an MRI machine this way (please don’t!) Nor would Wirewright make you a billion dollars.

Instead, Wirewright is attempting to be a novel, “alien” way to program; importing ideas from lesser known parts of computer science and hopefully introducing some novel ones as well; and “mixing” them until just the right consistency is achieved. Wirewright is intended mainly as an inspiration; as a source of ideas, and a proof that they work.

### More of my ramblings

See the ramblings/ directory to read more of my ramblings. None of those are of publishing quality and most are probably going to read like pseudo-scientific nonsense. Sorry.

### Videos

I've recorded a few proto-prototypes of Wirewright some time ago. Note that I do not know whether they will reflect what Wirewright will become in reality. I do have a rough idea of where I'm going and fairly detailed plans of getting there, but still  — a plan stops working the moment you start following it.

- [Wirewright µsoma unitary interpreter demo 3 — YouTube](https://youtu.be/P48VAbvai2w)
- [Wirewright µsoma code can edit itself — Wirewright µsoma reflection demo — YouTube](https://youtu.be/MFME6DtHtKo)
