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
> There is no other incentive: no money will ever be in this area, period; so curiosity
> is the only fuel we have here. Until the thing works. Or maybe it doesn't, and I'm screwed :)
> On my end it's a big gamble, giving it so much effort. On your, it's probably something
> that feels just a little bit crank. But then I run it, and it works, so who cares if
> it's crank or not!
>
> If you're interested, the best strategy for you right now is to read about ideas
> related to Wirewright over at the Wiki page. Do not expect beautiful prose there:
> those are dumps, unformatted, mostly unedited pieces that I publish raw. Rather
> than considering them in any way related to truth, consider them as progress dumps
> that are false the moment they're out, because the very fact of writing them changed
> me somehow (or maybe at least showed me how stup... undeveloped the ideas *really* are).
> The code is much better, believe me. At least I hope so.
>
> You can wait, too. I don't know whether it'd take months or years to get a usable
> frontend going (probably months). Some prototypes exist already that use all the new
> stuff, but they're too broken and slow to be usable at the moment.
>
> Right now, major work is ongoing on the components of Wirewright that receive the majority
> of load. I am trying my best to speed them up. As a sidenote, it's nice to have modern CPUs
> doing stuff worthy of their might, instead of sweating as they draw ads on the screen.
> That the result of such workload is a button (one button!) anyway is funny, of course. But
> if you open the hood of a browser, you'll see a mechanism; and if you open the hood of
> Wirewright as it's drawing your button, you'll see tiny people running around like crazy,
> moving pixels around. It's surprising such complexity can even run within semi-bearable time-budget!
>
> That's a big warning about nothing in particular, huh?

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

## Influenced by

Wirewright is influenced by various ideas from these amazing people: Francisco Varela, Humberto Maturana,
Stephen Wolfram, Niklas Luhmann, Michael Levin, Bret Victor, ... (this list will grow, as there
are many more of them!)

Since the project is in active development right now, it is very early to pinpoint exactly
which ideas were inspired by (or taken from!) whom. Hopefully, this would be possible later.

## Building

Wirewright currently only runs on Linux.

Wirewright can be built with Crystal 1.20.0 or later. If I forget to update the version number here, please
remember that Wirewright more than likely depends on the newest features and bug-fixes in Crystal. So you are
advised to build Wirewright with the latest version of Crystal.

### Dependencies

You will need to install [Crystal](https://github.com/crystal-lang/crystal) before building Wirewright.

Wirewright requires the following libraries. You should install them before building
the project. Most Linux distributions have these in their package registry.

- [SDL2](https://www.libsdl.org/)
- [Unibreak](https://github.com/adah1972/libunibreak/)
- [Raqm](https://github.com/HOST-Oman/libraqm)
- [XXhash](https://github.com/Cyan4973/xxHash)
- `libmagic` (version 5.46; 5.47 seems to have broken `magic_buffer` so it mis-identifies things)
- [GMP](https://gmplib.org/)
- [PCRE2](https://github.com/PCRE2Project/pcre2)

If you get a linker error, this probably means I forgot to include something in
the list above. Let me know if that's the case so that it can be made more accurate.

Wirewright vendors the following libraries (see the vendor/ directory):

- [PlutoVG](https://github.com/sammycage/plutovg)
- [PlutoSVG](https://github.com/sammycage/plutosvg)

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

