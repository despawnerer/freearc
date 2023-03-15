# FreeArc

Archive format developed originally by @Bulat-Ziganshin.

This project aims to take the latest released sources (0.67) and modernize them to be buildable with a modern toolchain, and on a Mac.

## Status

- [x] Compiling C++ parts
- [x] Compiling Haskell parts
- [ ] Linking
- [ ] Running

### Still needs to be done

- At the moment, some 7zip code parts are missing, so linking will fail.
- As consequence, actually running hasn't been attempted yet either.
- I have not looked at `Unarc` at all.

### Removed

- GUIs
- Windows support
- Little-endian support
- FAR plugin
- Shell extensions
- Installers etc.
- Lua support

## Prerequisites

- GNU make
- Cabal
- C++ compiler (clang works)
- GHC (9+ definitely works)
- Curl headers
- Ncurses headers

## Building

1. Compile C++ parts
   ```bash
   ./compile
   ```

2. Build

   ```bash
   cabal build
   ```
