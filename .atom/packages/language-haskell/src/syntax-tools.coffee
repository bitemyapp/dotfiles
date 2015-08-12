"""
$ curl https://raw.githubusercontent.com/twilson63/cakefile-template/master/Cakefile > ../Cakefile

$ cd .. && coffee -c -o lib src/main.coffee
$ cd .. && npm version minor
$ cd .. && git comm
$ cd .. && cake build
"""

# Transforms an easy grammar specification object into a tmLanguage grammar
# specification object.
class GrammarCreator
  constructor: (@grammar, @print = false) ->

  process: ->
    grammar = @grammar
    print = @print
    G = {}

    for n in [ "comment", "fileTypes", "firstLineMatch", "keyEquivalent", "name", "scopeName", "injectionSelector" ]
      G[n] = grammar[n] if grammar[n]?

    {@autoAppendScopeName, @macros} = grammar

    @autoAppendScopeName = true if typeof @autoAppendScopeName is "undefined"
    @macros = {} if typeof @macros is "undefined"
    @grammarScopeName = G.scopeName.replace /.*\./, ''

    @hasGrammarScopeName = new RegExp "\\.#{@grammarScopeName}$"

    macros = @macros

    # make regexes to strings
    for k,v of macros
      if v instanceof RegExp
        macros[k] = v.source

    # resolve macros
    for k,v of macros
      macros[k] = @resolveMacros(v)

    loop
      all_done = true
      for k,v of macros
        macros[k] = @resolveMacros(v)

        if /\{[a-zA-Z_]\w*\}/.test(macros[k])
          all_done = false
          if v == macros[k]
            all_done = true
            # throw "unresolved macro in #{v}"

      if all_done
        break

    name = grammar['name']
    for k,v of @makePattern(grammar)
      G[k] = v

    G['name'] = name

    if grammar.repository?
      G.repository = {}
      for k,v of grammar.repository
        pats = @makePattern(v, macros)
        if pats.begin? or pats.match?
          pats = { "patterns": [ pats ] }
        else if pats instanceof Array
          pats = { "patterns": pats }

        G.repository[k] = pats

    if print
      if print.match /\.cson$/
        CSON = require "season"
        fs   = require "fs"

        fs.writeFileSync print, CSON.stringify(G)

      else if print.match /\.json$/
        fs.writeFileSync print, JSON.stringify(G, null, "    ")

      else if print == "CSON"
        CSON = require "season"
        process.stdout.write CSON.stringify(G)

      else
        process.stdout.write JSON.stringify(G, null, "    ")

    G

  resolveMacros: (regex) ->
    if regex instanceof RegExp
      regex = regex.source

    macros = @macros

    regex.replace /// \{\w+\} ///g, (mob) ->
      s = mob[1...-1]

      if typeof macros[s] isnt "undefined"
        macros[s]
      else
        mob

  makeScopeName: (name) ->
    name = @resolveMacros(name)
    if @autoAppendScopeName
      unless @hasGrammarScopeName.test(name)
        return "#{name}.#{@grammarScopeName}"

    name

  # Transforms an easy grammar specification object into a tmLanguage grammar
  # specification object.
  #
  # n -> name
  # N -> contentName
  # p -> patterns
  # i -> include
  # m -> match
  # b -> begin
  # e -> end
  # c -> captures/beginCaptures
  # C -> endCaptures
  # L -> applyEndPatternLast
  #
  makePattern: (pattern) ->
    pat = pattern
    P   = {}

    if typeof pattern == "string"
      P.include = pattern
      return P

    if pattern instanceof Array
      return (@makePattern(p) for p in pattern)

    for k,v of pat
      switch k
        when "N", "contentName"
          P.contentName = @makeScopeName(v)
        when "i", "include"
          P.include = v
        when "n", "name"
          P.name  = @makeScopeName(v)
        when "m", "match"
          P.match = @resolveMacros(v)
        when "b", "begin"
          P.begin = @resolveMacros(v)
        when "e", "end"
          P.end   = @resolveMacros(v)

        when "c", "captures", "beginCaptures"
          if P.begin?
            P.beginCaptures = c = {}
          else
            P.captures = c = {}

          if typeof v == "string"
            c[0] = { name: @makeScopeName(v) }
          else
            for ck,cv of v
              if typeof cv isnt "string"
                c[ck] = @makePattern(cv)
              else
                c[ck] = { name: @makeScopeName(cv) }

        when "C", "endCaptures"
          P.endCaptures = c = {}
          if typeof v == "string"
            c[0] = { name: @makeScopeName(v) }
          else
            for ck,cv of v
              if typeof cv isnt "string"
                c[ck] = @makePattern(cv)
              else
                c[ck] = { name: @makeScopeName(cv) }

        when "p", "patterns"
          unless v instanceof Array
            v = [ v ]
          P.patterns = (@makePattern(p) for p in v)

        when "L", "applyEndPatternLast"
          P.applyEndPatternLast = v

        else
          P[k] = v

    P

makeGrammar = (grammar, print = false) ->
  (new GrammarCreator grammar, print).process()

module.exports = makeGrammar
