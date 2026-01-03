<div align="center">

![Wirewright Logo](https://github.com/user-attachments/assets/3e5dc602-9c8d-412d-a7fa-9e1a0c3b466e)
</div>

> [!WARNING]
> You're looking at the development branch of Wirewright, `iota`. I've made `iota`
> the main branch because the only alternative is to keep `kappa`, and `kappa` is
> *very* outdated and won't receive updates anymore.
>
> Wirewright is currently lacking a *frontend*. This means that aside from tests
> and custom code (i.e., using Wirewright as a library), there's no way for a user
> (*you*) to interact with the system.
>
> This is *very* unfortunate, and a little bit funny, because what I'm saying here
> is, basically, "here's my project but you can't use it". *Shrugs.*
>
> But I have other things to do first, and then there's only one of me. I'm already
> working full-time on Wirewright, and that's not enough, not even *remotely*, considering
> the scope of the project.
>
> Furthermore, this kind of thing -- Wirewright -- stems purely out of my curiosity.
> There is no other incentive: no money will ever be in this area, period. Until the thing
> works. Or maybe it doesn't, and I'm screwed :) On my end it's a big gamble, giving it
> so much effort. On your, it's probably something feels just a little bit crank. But then
> I run it, and it works, so who cares if it's crank!
>
> If you're interested, the best strategy for you right now is to read about ideas
> related to Wirewright over at the Wiki page; there I'll dump the pieces as they
> "come out of me" You can wait, too. I don't know whether it'd take months or years
> to get a usable frontend going (probably months). Some prototypes exist already that
> use all the new stuff, but they're too broken and slow to be usable in at the moment.
> Right now, major work is ongoing on the components of Wirewright most under load,
> to speed things up.
>
> That's a big warning about nothing in particular, isn't it?

# Wirewright

Wirewright explores the idea of having entire *worlds* as first-class objects. With Wirewright, you can create worlds, and pass them around. You can then build higher-order worlds from a bunch of smaller worlds, each acting as an agent. You can have worlds creating and populating worlds. You can have worlds interacting with other worlds through something Wirewright calls *entanglement*.

See [the wiki](https://github.com/wirewright/wirewright/wiki/First%E2%80%90class-worlds) to read more about Wirewright. I don't want to scare people off with a wall of text.

## Gallery

### Frontend: soma6

NOTE: this frontend is no longer supported on this branch. See kappa.

https://github.com/user-attachments/assets/e86cb81d-67d7-45b8-8a68-7399e4fe367e

### Frontend: Wirewright Rack

NOTE: this frontend is no longer supported on this branch. See kappa.

<img width="1804" height="961" alt="Screenshot showing Wirewright Rack terminal UI atop UIR tests" src="https://github.com/user-attachments/assets/975fd033-b2d0-4745-827e-cb30ffd5a6f3" />

## References

Wirewright implements the ideas inspired by or directly taken from these amazing people: Francisco Varela, Humberto Maturana, Stephen Wolfram, Niklas Luhmann, Michael Levin, Bret Victor, ... (this list will grow, as there are many more of them!)

Wirewright wouldn't be possible without these technologies:

- [Crystal](https://github.com/crystal-lang/crystal)
- [PlutoVG](https://github.com/sammycage/plutovg)
- [PlutoSVG](https://github.com/sammycage/plutosvg)
- [termbox2](https://github.com/termbox/termbox2)
- [SDL](https://www.libsdl.org/)
- [BLAKE3 hash function](https://github.com/BLAKE3-team/BLAKE3)

## Building

Wirewright can be built with Crystal 1.18.0 or later. If I forget to update the version number here, please
remember that Wirewright more than likely depends on the newest features or bug-fixes in Crystal. So you are
advised to build Wirewright with the latest version of Crystal.

### Building the dev tool

Wirewright uses a custom dev tool to manage the various subprojects inside the repo. You can build
the dev tool with:

```text
$ crystal build src/dev.cr --progress --release -Dpreview_mt -Dexecution_context 
```

### Using the dev tool

After running the dev tool build command you should be able to run the `dev` executable:

```text
$ ./dev
# Shows help for the dev tool ...
```

There are several *presets* available. You can print them with:

```text
$ ./dev g
# Prints available presets ...
```

Right now, the only interesting preset is `tests`. Switch to it using:

```text
$ ./dev g tests
```

And build it with:

```
$ ./dev b
```

You should then have the `testtool` executable, which will run tests in the `tests/` directory.

## Want to learn more?

### More of my ramblings

See the ramblings/ directory to read more of my ramblings. None of those are of publishing quality and most are probably going to read like pseudo-scientific nonsense. Sorry.

### Videos

Visit the YouTube channel of Wirewright for videos about Wirewright: [Wirewright — YouTube](https://www.youtube.com/@wirewright).

