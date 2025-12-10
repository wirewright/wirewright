<div align="center">

![Wirewright Logo](https://github.com/user-attachments/assets/3e5dc602-9c8d-412d-a7fa-9e1a0c3b466e)
</div>

> [!WARNING]
> You're looking at the development branch of Wirewright, `iota`. Most likely it won't
> even compile; that's why I'm keeping it separate from the main branch, `kappa`. Once
> it's ready, it's either going to be merged into kappa, or I'm going to make this branch
> the main one.

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

Wirewright runs on the ideas inspired by or directly taken from: Francisco Varela, Humberto Maturana, Stephen Wolfram, Niklas Luhmann, Michael Levin, Bret Victor, ... (this list will grow as I remember more of them)

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

