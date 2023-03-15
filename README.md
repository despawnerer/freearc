# FreeArc

Archive format developed originally by @Bulat-Ziganshin.

This project aims to take the latest released sources (0.67) and modernize them to be buildable with a modern toolchain, and on a Mac.

## Status

- [x] Compiling C++ parts
- [x] Compiling Haskell parts
- [x] Linking
- [x] Running
- [x] Creating new FreeArc archives and extracting them
- [ ] Extracting existing FreeArc archives

### Still needs to be done

- I have not looked at `Unarc` at all.
- Would be great to get my hands on on some actual FreeArc archives that were created by the original program

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

## Building

1. Compile C++ parts
   ```bash
   ./compile
   ```

2. Build

   ```bash
   cabal build
   ```

## Running

```bash
cabal run
```
