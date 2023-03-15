# FreeArc

Archive format developed originally by @Bulat-Ziganshin.

This project aims to take the latest released sources (0.67) and modernize them to be buildable with a modern toolchain, and on a Mac.

## Status

- [x] Compiling C++ parts
- [x] Compiling Haskell parts
- [x] Linking
- [x] Running
- [ ] Compressing or decompressing actual FreeArc archives

### Still needs to be done

- Checking that compressing or decompressing something actually works
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
