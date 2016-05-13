module.exports =
  activate: ->
    setImmediate ->
      lps = atom.workspace.getTextEditors()
        .map((ed) -> ed.getGrammar().packageName)
      if 'language-haskell' in lps
        atom.packages.triggerActivationHook? 'language-haskell:grammar-used'
  deactivate: ->
