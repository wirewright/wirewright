<div align="center">

![Wirewright Logo](https://github.com/user-attachments/assets/3e5dc602-9c8d-412d-a7fa-9e1a0c3b466e)
</div>

# Wirewright

Wirewright explores the idea of having entire *worlds* as first-class objects. With Wirewright, you can create worlds, and pass them around. You can then build higher-order worlds from a bunch of smaller worlds, each acting as an agent. You can have worlds creating and populating worlds. You can have worlds interacting with other worlds through something Wirewright calls *entanglement*.

See [the wiki](https://github.com/wirewright/wirewright/wiki/First%E2%80%90class-worlds) to read more about Wirewright. I don't want to scare people off with a wall of text.

## Gallery

## MuSoma

I am excited to announce a new front-end for Wirewright (with the old name :^), MuSoma.

<img width="1200" height="932" alt="musoma-shot" src="https://github.com/user-attachments/assets/ae2701f5-284c-474c-b8b4-95d5e4857dec" />

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

Hover over things to learn more about them (or, well, about things I've bothered implementing tooltips for).
The main things of interest are the examples. You probably won't be able to program much in MuSoma
unless you're an exploratory type of person; there are barely any docs, and I have more interesting things
to do than writing them, uhmm, sorry, I guess. The whole thing is so exciting I don't want to look back
and trace my steps.

> [!NOTE]
> Wirewright doesn't yet quit when you close the window. You'll have to Ctrl-C it by hand. Sorry.

## Building Wirewright

Wirewright currently only runs on Linux.

### Building with Docker

This is probably the easiest way to build Wirewright. Note that I'm not an expert
on Docker, so the Dockerfiles may not be the best ones on the planet.

Go to the build directory:

```console
cd build
```

Build the Wirewright environment image:

```console
docker build -f wirewright-env.Dockerfile -t wirewright-env:latest .
```

Build the Wirewright base image (which you can run tests on or do general Wirewright work):

```console
docker build -f wirewright.Dockerfile -t wirewright-base:latest .
```

Build the MuSoma AppImage:

```console
docker build -f musoma.Dockerfile --output type=local,dest=. .
```

It will eventually dump musoma-dist.tar.gz into the build/ dir. The archive contains
the AppImage and miscellaneous files.

> [!NOTE]
> The Dockerfiles are very poorly written right now and are not suitable for development.
> You currently have to rebuild too much (`wirewright-base`). This makes fast iteration
> nearly impossible. Improving the Dockerfiles remains future work.

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
have to install and do.

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

Right now, the only interesting preset is `tests`. Switch to it using:

```console
./dev g tests
```

And build it with:

```console
./dev b
```

You should then have the `testtool` executable, which will run tests in the `tests/` directory.

## Want to learn more?

### More of my ramblings

See the ramblings/ directory to read more of my ramblings. None of those are of publishing quality and most are probably going to read like pseudo-scientific nonsense. Sorry.

### Videos

Visit the YouTube channel of Wirewright for videos about Wirewright: [Wirewright — YouTube](https://www.youtube.com/@wirewright).

