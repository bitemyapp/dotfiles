## 1.7.17
* Prelude typeclasses
* Merge branch 'master' of github.com:atom-haskell/language-haskell
* Grab prelude definitions from Prelude

## 1.7.16
* Fix #76

## 1.7.15
* Another stab at Windows postinstall

## 1.7.14
* Update postinstall hook to hopefully make it work in windows
* Spec update
* Various tweaks to word anchoring, symbol provider

## 1.7.13
* Fix runaway module name

## 1.7.12
* Spec update
* Capture identifiers before type ctors

## 1.7.11
* Fix CPP # confusion
* Updated contributors

## 1.7.10
* added liquidhaskell comment block (PR #69 by @ranjitjhala)

## 1.7.9
* Add 'type instance' keyword

## 1.7.8
* Initial support for type families

## 1.7.7
* Regression tests for #65
* Tweak ctor regex
* Spec update

## 1.7.6
* More preprocessor pragmas

## 1.7.5
* Add [] () to inline type signature characters

## 1.7.4
* More accurate inline type signature match
* Add meta.type-signature.haskell to in-line sigs

## 1.7.3
* Fix message highlighting
* Add coffeelint

## 1.7.2
* Merge pull request #63 from robrix/options-ghc-pragma
* Add OPTIONS_GHC to the recognized pragmas.

## 1.7.1
* Fix string tokenization (+tests)
* In-line type signatures

## 1.7.0
* Haskell message hint grammar

## 1.6.0
* Haskell Type Hint grammar

## 1.5.2
* Don't confuse timecop

## 1.5.1
* Support multiline module export ctors (#60)

## 1.5.0
* Haskell Autocompletion Hint grammar

## 1.4.12
* Fix #58 (escaped quotes in multiline strings)

## 1.4.11
* Fix where anchoring in typeDecl and GADT

## 1.4.10
* Fix catastrophic backtracking in GADTs (#55)

## 1.4.9
* Hack to trigger activation hook after all packages loaded

## 1.4.8
* Amend quote escapes fix

## 1.4.7
* Quote escapes fix (#53)

## 1.4.6
* Word anchoring and tick (') handling (#52)

## 1.4.5
* Fix double quote escape in strings (#51)

## 1.4.4
* Fix gadt and record syntax conflict (#50)

## 1.4.3
* Add character patterns to in-line quoted string (#47)

## 1.4.2
* Quasi-fix for #47

## 1.4.1
* Pipe escapes for LHS (#45)

## 1.4.0
* Match scripts with `runhaskell` shebang as Haskell files
* Add highlighting rule for shebang

## 1.3.1
* Add `benchmark, flag, source-repository, test-suite` sections highlighting for .cabal files (by @ianbollinger)

## 1.3.0
* GADTs where-syntax support (#43)

## 1.2.1
* Add CHANGELOG
* Fix indent block problems with data, newtype and type (#42)

## 1.2.0
* Initial support for c2hs and hsc2hs
* Better C preprocessor support (referencing source.c)

## 1.1.11
* README: Add info on autoindent customization.
* Fix increaseIndentPattern

## 1.1.10
* Remove `decreaseIndentPattern` for now

## 1.1.9
* Add support for `<` (reverse bird tracks)

## 1.1.8
* Build grammars in postinstall hook
* Add support for spec env in lit. hs.

## 1.1.7
* Fix single-line comment line anchoring

## 1.1.6
* Quasi-quotes support

## 1.1.5
* Better lhs grammar

## 1.1.4
* Update grammar
* Type ctor word-anchoring
* Fix for module_name

## 1.1.3
* Fixes word-anchoring and priority for qualified/as/hiding

## 1.1.2
* Allow module name to end with dot (for autocompletion)
* Haddock snippets for all comments
* Create LICENSE.md
* Update README.md

## 1.1.1
* Foreign import/export
* Haddock comments

## 1.1.0

## 1.0.2
* Merge pull request #31 from atom-haskell/master
* Remove deriving from type alias
* Simplify and data/newtype/record syntax
* Extend ctor args, empty line doesn't count as ind. block end
* Data declaration comments fix, added comments to export list
* Grammar update
* Minor fixes
* Allow qualified fn and cn
* Fix ident block end regex
* Fix bugs with type_signature looping
* Fix meta.declaration.type.data.record.haskell gobble
* Snippets updates and fixes
* deriving fix, some semantics for type/newtype/data
* type/newtype/data declarations
* More semantic naming
* Record field declaration syntax
* Haskell grammar fixes and cleanup
* Added snippets from ide-haskell
* Added UnicodeSyntax snippets
* Snippets cleanup
* Atom API 1.0 deprecation fixes
* Merge pull request #11 from MichaelRawson/master (Jared Roesch)
* fixed "module" keyword word-anchoring (Michael Rawson)

## 1.0.0
* Merge pull request #9 from mdgriffith/master (Jared Roesch)
* Merge pull request #8 from RossOgilvie/patch-1 (Jared Roesch)
* Cabal Comment Toggling (Matthew Griffith)
* Unnecessary word (Matthew Griffith)
* Added support for Cabal files (Matthew Griffith)
* Added support for the UnicodeSyntax extention. (Ross Ogilvie)

## 0.4.0
* Merge pull request #6 from samuela/fix-lambda-snippet (Jared Roesch)
* Merge pull request #5 from samuela/remove-type-sequence (Jared Roesch)
* Fix "Lambda Expression" snippet (samuela)
* Remove broken "-" snippet (samuela)

## 0.3.0
* Merge pull request #3 from mdgriffith/patch-1 (Jared Roesch)
* Comment symbol to allow toggling comments (mdgriffith)

## 0.2.0
* remove extraneous the (Jared Roesch)

## 0.1.1
* Merge pull request #1 from jc00ke/patch-1 (Jared Roesch)
* Haskell, right? (Jesse Cooke)

## 0.1.0
* update README.md with useful info (Jared Roesch)
* Initial commit converted from https://github.com/textmate/haskell.tmbundle (Jared Roesch)
