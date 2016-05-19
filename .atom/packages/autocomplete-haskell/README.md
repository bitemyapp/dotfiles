# autocomplete-haskell atom package

Autocomplete-haskell provides autocompletion facilities for your Haskell
hacking.
It relies on scope names provided by [language-haskell][1] and `haskell-completion-backend` service, provided by [haskell-ghc-mod][2]

You can show auto-completions for hole `_`. This will try to find replacements
based on type. It's no magic though, so if hole has some crazy type, it won't
find anything. You can also refine hole completions based on name by using named holes, e.g. `_from`

Current autocompletion scopes:

* Import module name
* Import module symbols
* Language pragmas
* OPTIONS_GHC pragma
* Type name
* Class name
* Symbol name

Sadly, it does not pick up types and/or other symbols defined in current file
(ghc-mod seems to be incapable of this feat), so for this you have to rely on
default autocomplete-plus SymbolProvider.

## Dependencies

Atom packages:

* [language-haskell][1]
* [haskell-ghc-mod][2]

[1]: https://atom.io/packages/language-haskell
[2]: https://atom.io/packages/haskell-ghc-mod

Autocompletion:

![autocomplete](https://cloud.githubusercontent.com/assets/7275622/9704861/e4474ec4-54bc-11e5-92f4-84a3995e45cb.gif)

Import autocompletion:

![import](https://cloud.githubusercontent.com/assets/7275622/9704865/ff39f79a-54bc-11e5-9912-5fb2884b749b.gif)

Hole autocompletion:

![hole](https://cloud.githubusercontent.com/assets/7275622/9704890/5581ccae-54bd-11e5-8ec6-8aa289e5a099.gif)
