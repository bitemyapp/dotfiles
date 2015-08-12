# Haskell support in Atom

Adds syntax highlighting and snippets to Haskell files in Atom.

Grammars:

* Haskell (\*.hs)
* Literate Haskell (\*.lhs)
* Cabal (\*.cabal)

![image](https://cloud.githubusercontent.com/assets/7275622/8120540/f16d7ee6-10a8-11e5-9b9d-223ff05a54c6.png)

Based on [Haskell TextMate bundle](https://github.com/textmate/haskell.tmbundle).

# Auto-indent

If you don't like current auto-indentation settings, you can define your own regexp in `config.cson` (Edit -> Open Your Config), or disable it altogether, e.g.

To disable auto-indent:

```cson
".haskell.source":
  editor:
    increaseIndentPattern: ''
```

Note that regexp expression is using oniguruma for parsing, and it needs to be a string, not a javascript regexp. You'll also have to escape `\`.

By default, `increaseIndentPattern` has the following value:

```cson
".haskell.source":
  editor:
    increaseIndentPattern: '(((=|\\bdo|\\bwhere|\\bthen|\\belse|\\bof)\\s*)|(\\bif(?!.*\\bthen\\b.*\\belse\\b.*).*))$'
```
# License

Copyright © 2015 Atom-Haskell

Contributors:
* Ian D. Bollinger
* Jared Roesch
* Jesse Cooke
* Matthew Griffith
* mdgriffith
* Michael Rawson
* Nikolay Yakimov
* Ross Ogilvie
* samuela

See the [LICENSE.md][LICENSE] for details.

[LICENSE]: https://github.com/atom-haskell/language-haskell/blob/master/LICENSE.md
