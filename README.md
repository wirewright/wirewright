# Wirewright

Wirewright is a rewrite environment for self-embodied programs.

I am working hard to package the hundreds of thousands of lines of "all over the place" code I've written and about two years of ideas and exploration into something simple, usable, and practical. Please wait and wish me a lot of energy :)

## Upcoming "selling points"

**Warning**: Wirewright is several months away from a working [prototype](https://youtu.be/eUkZNk90rbQ). These "selling points" are for the far future.

1. The core idea of Wirewright is to base the entirety of interaction and programming on indirect self-modification; to make self-modification intuitive, nondestructive, and easy to reason about.
2. Wirewright considers itself a logical continuation of Lisp (as an idea, I suppose). Lisp made code and data "translatable" into each other, yet in Lisp they (or rather, their "roles") are still distinct: some lists are code and others, data. Wirewright, on the other hand, attempts to rid of the idea of code altogether. In Wirewright, everything is data ­— and nothing is code. In a way, Wirewright is the "ultimate", “universal program”, to which anything is an input; similar to some solvers out there, but somewhat more general-purpose.
3. Wirewright is remotely related to the general programming approach named "functional core, imperative shell". As such an “imperative shell”, Wirewright integrates all communication and interaction with the outside world into a coherent whole that the self-embodied program can interact with. Communication over the network, access to database, storage, graphics, and more — all by interacting with Wirewright.
4. **µsoma** is a unified graphical user interface to Wirewright.
5. Wirewright acts simultaneously as an *observer* and an abstract kind of *physics* for the "functional core" ­— a self-embodied program. It reacts to changes in the latter and provides feedback through rewriting (but not necessarily; for instance, UI is simply a way to view the self-embodied program, like some weird "glasses" that show `(button "Increment" @actions)` as a rectangle with centered text).

## Want to learn more?

### More of my ramblings

See the ramblings/ directory to read more of my ramblings. None of those are of publishing quality and most are probably going to read like pseudo-scientific nonsense. Sorry.

### Videos

I've recorded a few proto-prototypes of Wirewright some time ago. Note that I do not know whether they will reflect what Wirewright will become in reality. I do have a rough idea of where I'm going and fairly detailed plans of getting there, but still  — a plan stops working the moment you start following it.

- [Wirewright µsoma unitary interpreter demo 3 — YouTube](https://youtu.be/P48VAbvai2w)
- [Wirewright µsoma code can edit itself — Wirewright µsoma reflection demo — YouTube](https://youtu.be/MFME6DtHtKo)
