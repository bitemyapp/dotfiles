# ide-haskell-cabal package

The `ide-haskell-cabal` package provides a build backend for `ide-haskell`
package based on `cabal`.

It supports easy switching between multiple versions of GHC by having a set of configuration settings for each version of GHC, plus a drop-down box to pick a GHC version. For each GHC version you can specify:

* The path (either adding to your system path or replacing it completely)
* The sandbox file (cabal `CABAL_SANDBOX_CONFIG` environment variable)
* The build directory (cabal `--builddir` parameter). This defaults to `dist/`.

It also provides support for `ide-haskell`'s build target selection by reading and parsing the `.cabal` file and extracting the available targets (it uses a thin `ghcjs`-compiled wrapper around the `Cabal` library to read the .`cabal` file).

## Keybindings

Ide-Haskell-Cabal comes with little pre-specified keybindings, so you will need to specify your own, if you want those.

You can edit Atom keybindings by opening 'Edit → Open Your Keymap'. Here is a template for all commands, provided by ide-haskell-cabal:

```cson
'atom-workspace':
  '': 'ide-haskell-cabal:build'
  '': 'ide-haskell-cabal:set-build-target'
  '': 'ide-haskell-cabal:clean'
  '': 'ide-haskell-cabal:test'
```
