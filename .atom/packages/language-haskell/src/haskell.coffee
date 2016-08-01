makeGrammar = require './syntax-tools'
prelude = require './prelude'

toString = (rx) ->
  if rx instanceof RegExp
    rx.source
  else
    rx

list = (item, s, sep) ->
  #recursive regexp, caution advised
  "(?<#{item}>(?:#{toString s})(?:\\s*(?:#{toString sep})\\s*\\g<#{item}>)?)"

listMaybe = (item, s, sep) ->
  #recursive regexp, caution advised
  "(?<#{item}>(?:#{toString s})(?:\\s*(?:#{toString sep})\\s*\\g<#{item}>)?)?"

concat = (list...) ->
  r = ''.concat (list.map (i) -> "(?:#{toString i})")...
  "(?:#{r})"

haskellGrammar =
  name: 'Haskell'
  fileTypes: [ 'hs' ]
  firstLineMatch: '^\\#\\!.*\\brunhaskell\\b'
  scopeName: 'source.haskell'

  macros:
    identStartCharClass: /[\p{Ll}_\p{Lu}\p{Lt}]/
    identCharClass: /[\p{Ll}_\p{Lu}\p{Lt}\p{Nd}']/
    functionNameOne: /[\p{Ll}_]{identCharClass}*/
    classNameOne: /[\p{Lu}\p{Lt}]{identCharClass}*/
    functionName: /(?:{className}\.)?{functionNameOne}/
    className: /{classNameOne}(?:\.{classNameOne})*/
    operatorChar: /[\p{S}\p{P}&&[^(),;\[\]`{}_"']]/
    ###
    In case this regex seems overly general, note that Haskell
    permits the definition of new operators which can be nearly any string
    of punctuation characters, such as $%^&*.
    ###
    operator: /{operatorChar}+/
    operatorFun: ///
      (?:
        \(
          (?!--+\)) # An operator cannot be composed entirely of `-` characters
          {operator}
        \)
      )
      ///
    basicChar: /[\ -\[\]-~]/
    escapeChar: ///
      \\(?:NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE
        |DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS
        |US|SP|DEL|[abfnrtv\\\"'\&])    # Escapes
      ///
    octalChar: /(?:\\o[0-7]+)/
    hexChar: /(?:\\x[0-9A-Fa-f]+)/
    controlChar: /(?:\^[A-Z@\[\]\\\^_])/
    character: '(?:{basicChar}|{escapeChar}|{octalChar}|{hexChar}|{controlChar})'
    classConstraint: concat /({className})\s+/,
      list('classConstraint', /{className}|{functionName}/, /\s+/)
    functionTypeDeclaration:
      concat list('functionTypeDeclaration', /{functionName}|{operatorFun}/, /,/),
        /\s*(::|∷)/
    ctorTypeDeclaration:
      concat list('ctorTypeDeclaration', /{className}|{operatorFun}/, /,/),
        /\s*(::|∷)/
    ctorArgs: ///
      (?!deriving)
      (?:
      {className}     #proper type
      |{functionName} #type variable
      |(?:(?!deriving)(?:[\w()'→⇒\[\],]|->|=>)+\s*)+ #anything goes!
      )
      ///
    ctor: concat /{lb}({className}){rb}/,
      listMaybe('ctorArgs', /\s+{ctorArgs}/, '')
    typeDeclOne: /(?:(?!{lb}where{rb})(?:{className}|{functionName}))/
    typeDecl: '(?>(?:{typeDeclOne})(?:\\s+{typeDeclOne})*)'
    indentChar: /[ \t]/
    indentBlockEnd: /^(?!\1{indentChar}|{indentChar}*$)/
    maybeBirdTrack: /^/
    lb: '(?:(?={identStartCharClass})(?<!{identStartCharClass}))'
    rb: '(?:(?<={identCharClass})(?!{identCharClass}))'
    b: '(?:{lb}|{rb})'

  patterns: [
      name: 'block.liquidhaskell'
      contentName: 'block.liquidhaskell.annotation'
      begin: '\\{-@(?!#)'
      end: '@-\\}'
      patterns: [
          include: '$self'
      ]
    ,
      name: 'comment.line.shebang.haskell'
      match: '^\\#\\!.*\\brunhaskell\\b.*$'
    ,
      name: 'keyword.operator.function.infix.haskell'
      match: /(`){functionName}(`)/
      captures:
        1: name: 'punctuation.definition.entity.haskell'
        2: name: 'punctuation.definition.entity.haskell'
      ###
      In case this regex seems unusual for an infix operator, note
      that Haskell allows any ordinary function application (elem 4 [1..10])
      to be rewritten as an infix expression (4 `elem` [1..10]).
      ###
    ,
      name: 'constant.language.unit.haskell'
      match: /\(\)/
    ,
      name: 'constant.language.empty-list.haskell'
      match: /\[\]/
    ,
      begin: /(\[)({functionNameOne})(\|)/
      end: /(\|)(\])/
      beginCaptures:
        1: name: 'punctuation.definition.quasiquotes.begin.haskell'
        2: name: 'entity.name.tag.haskell'
        3: name: 'string.quoted.quasiquotes.haskell'
      endCaptures:
        1: name: 'string.quoted.quasiquotes.haskell'
        2: name: 'punctuation.definition.quasiquotes.end.haskell'
      contentName: 'string.quoted.quasiquotes.haskell'
    ,
      name: 'meta.declaration.module.haskell'
      begin: /{lb}(module){rb}/
      end: /{lb}(where){rb}/
      beginCaptures:
        1: name: 'keyword.other.haskell'
      endCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          include: '#comments'
        ,
          include: '#module_name'
        ,
          include: '#module_exports'
        ,
          name: 'invalid'
          match: /[a-z]+/
      ]
    ,
      name: 'meta.declaration.class.haskell'
      begin: /{lb}(class){rb}/
      end: /{lb}(where){rb}|$/
      beginCaptures:
        1: name: 'storage.type.class.haskell'
      endCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          name: 'support.class.prelude.haskell'
          match: "{lb}(#{prelude.classes.join('|')}){rb}"
        ,
          include: '#type_name'
        ,
          include: '#generic_type'
      ]
    ,
      name: 'meta.declaration.instance.haskell'
      begin: /{lb}(instance){rb}/
      end: /{lb}(where){rb}|$/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        1: name: 'keyword.other.haskell'
      endCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          include: '#type_signature'
      ]
    ,
      name: 'meta.foreign.haskell'
      begin: /{maybeBirdTrack}(\s*)(foreign)\s+(import|export){rb}/
      end: /{indentBlockEnd}/
      beginCaptures:
        2: name: 'keyword.other.haskell'
        3: name: 'keyword.other.haskell'
      patterns:[
          match: /(?:un)?safe/
          captures:
            0: name: 'keyword.other.haskell'
        ,
          include: '$self'
      ]
    ,
      name: 'meta.import.haskell'
      begin: /{lb}(import){rb}/
      end: /($|;|(?=--))/
      beginCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          include: '#module_name'
        ,
          include: '#module_exports'
        ,
          match: /{lb}(qualified|as|hiding){rb}/
          captures:
            1: name: 'keyword.other.haskell'
      ]
    ,
      name: 'meta.declaration.type.GADT.haskell'
      begin: /{maybeBirdTrack}(\s*)(data|newtype)\s+({typeDecl})(?=\s+where{rb})/
      end: /{indentBlockEnd}/
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3:
          name: 'meta.type-signature.haskell'
          patterns: [include: '#type_signature']
      patterns: [
          include: '#comments'
        ,
          include: '#deriving'
        ,
          match: /{ctor}/
          captures:
            1: patterns: [include: '#type_ctor']
            2:
              name: 'meta.type-signature.haskell'
              patterns: [include: '#type_signature']
        ,
          name: 'keyword.other.haskell'
          match: /{lb}where{rb}/
        ,
          include: '#type_name'
        ,
          include: '#ctor_type_declaration'
      ]
    ,
      name: 'meta.declaration.type.data.haskell'
      begin: /{maybeBirdTrack}(\s*)(data|newtype)\s+({typeDecl})/
      end: /{indentBlockEnd}/
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3:
          name: 'meta.type-signature.haskell'
          patterns: [include: '#type_signature']
      patterns: [
          include: '#comments'
        ,
          include: '#deriving'
        ,
          match: /=/
          captures:
            0: name: 'keyword.operator.assignment.haskell'
        ,
          match: /{ctor}/
          captures:
            1: patterns: [include: '#type_ctor']
            2:
              name: 'meta.type-signature.haskell'
              patterns: [include: '#type_signature']
        ,
          match: /\|/
          captures:
            0: name: 'punctuation.separator.pipe.haskell'
        ,
          name: 'meta.declaration.type.data.record.block.haskell'
          begin: /\{/
          beginCaptures:
            0: name: 'keyword.operator.record.begin.haskell'
          end: /\}/
          endCaptures:
            0: name: 'keyword.operator.record.end.haskell'
          patterns: [
              name: 'punctuation.separator.comma.haskell'
              match: /,/
            ,
              include: '#record_field_declaration'
          ]
      ]
    ,
      name: 'meta.declaration.type.type.haskell'
      begin: /{maybeBirdTrack}(\s*)(type(?:\s+(?:family|instance))?)\s+({typeDecl})/
      end: /{indentBlockEnd}|(?={lb}where{rb})/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        2: name: 'storage.type.data.haskell'
        3:
          name: 'meta.type-signature.haskell'
          patterns: [include: '#type_signature']
      patterns: [
          include: '#comments'
        ,
          match: /=/
          captures:
            0: name: 'keyword.operator.assignment.haskell'
        ,
          include: '#type_signature'
      ]
    ,
      name: 'keyword.other.haskell'
      match: /{lb}(deriving|where|data|type|newtype){rb}/
    ,
      name: 'storage.type.haskell'
      match: /{lb}(data|type|newtype){rb}/
    ,
      name: 'keyword.operator.haskell'
      match: /{lb}infix[lr]?{rb}/
    ,
      name: 'keyword.control.haskell'
      match: /{lb}(do|if|then|else|case|of|let|in|default){rb}/
    ,
      name: 'meta.preprocessor.c'
      begin: /{maybeBirdTrack}(?=#)/
      end: '(?<!\\\\)(?=\\n)'
      patterns: [
        include: 'source.c'
      ]
      ###
      In addition to Haskell's "native" syntax, GHC permits the C
      preprocessor to be run on a source file.
      ###
    ,
      include: '#pragma'
    ,
      name: 'string.quoted.double.haskell'
      begin: /"/
      end: /"/
      beginCaptures:
        0: name: 'punctuation.definition.string.begin.haskell'
      endCaptures:
        0: name: 'punctuation.definition.string.end.haskell'
      patterns: [
          include: '#characters'
        ,
          begin: /\\\s/
          end: /\\/
          beginCaptures:
            0: name: 'markup.other.escape.newline.begin.haskell'
          endCaptures:
            0: name: 'markup.other.escape.newline.end.haskell'
          patterns: [
              match: /\S+/
              name: 'invalid.illegal.character-not-allowed-here.haskell'
          ]
      ]
    ,
      name: 'markup.other.escape.newline.haskell'
      match: /\\$/
    ,
      name: 'string.quoted.single.haskell'
      match: /(')({character})(')/
      captures:
        1: name: 'punctuation.definition.string.begin.haskell'
        2:
          patterns:[
            include: '#characters'
          ]
        3: name: 'punctuation.definition.string.end.haskell'
    ,
      include: '#function_type_declaration'
    ,
      match: '\\((?<paren>(?:[^()]|\\(\\g<paren>\\))*)(::|∷)(?<paren2>(?:[^()]|\\(\\g<paren2>\\))*)\\)'
      captures:
        1: patterns: [include: 'source.haskell']
        2: name: 'keyword.other.double-colon.haskell'
        3: {name: 'meta.type-signature.haskell', patterns: [include: '#type_signature']}
    ,
      # match: '(::|∷)((?:(?:{className}|{functionName}|->|=>|[→⇒()\\[\\]]|\\s)(?!:<-|=))*)'
      match: '(::|∷)((?:{className}|{functionName}|\\->|=>|[→⇒()\\[\\]]|\\s)*)'
      captures:
        1: name: 'keyword.other.double-colon.haskell'
        2: {name: 'meta.type-signature.haskell', patterns: [include: '#type_signature']}
    ,
      name: 'support.tag.haskell'
      match: "{lb}(#{prelude.constr.join('|')}){rb}"
    ,
      include: '#comments'
    ,
      name: 'support.function.prelude.haskell'
      match: "{lb}(#{prelude.funct.join('|')}){rb}"
    ,
      include: '#infix_op'
    ,
      name: 'punctuation.separator.comma.haskell'
      match: /,/
    ,
      name: 'constant.numeric.hexadecimal.haskell'
      match: '(?<!{identCharClass})0[xX][0-9a-fA-F]+'
    ,
      name: 'constant.numeric.octal.haskell'
      match: '(?<!{identCharClass})0[oO][0-7]+'
    ,
      name: 'constant.numeric.float.haskell'
      match: '(?<!{identCharClass})[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?'
    ,
      name: 'constant.numeric.float.haskell'
      match: '(?<!{identCharClass})[0-9]+[eE][+-]?[0-9]+'
      # Floats are always decimal
    ,
      name: 'constant.numeric.decimal.haskell'
      match: '(?<!{identCharClass})[0-9]+'
    ,
      name: 'keyword.operator.haskell'
      match: /{operator}/
    ,
      include: '#type_ctor'
    ,
      match: '{lb}{functionName}{rb}'
      name: 'identifier.haskell'
      captures:
        0:
          patterns: [
            name: 'support.other.module.haskell'
            match: /^(?:{className}\.)*{className}\.?/
          ]
  ]
  repository:
    block_comment:
      patterns: [
          name: 'comment.block.haddock.haskell'
          begin: /\{-\s*[|^]/
          end: /-\}/
          applyEndPatternLast: 1
          beginCaptures:
            0: name: 'punctuation.definition.comment.haddock.haskell'
          endCaptures:
            0: name: 'punctuation.definition.comment.haddock.haskell'
          patterns: [
              include: '#block_comment'
          ]
        ,
          name: 'comment.block.haskell'
          begin: /\{-(?!#)/
          end: /-\}/
          applyEndPatternLast: 1
          beginCaptures:
            0: name: 'punctuation.definition.comment.haskell'
          patterns: [
              include: '#block_comment'
          ]
      ]
    comments:
      patterns: [
          begin: /({maybeBirdTrack}[ \t]+)?(?=--+\s+[|^])/
          end: /(?!\G)/
          beginCaptures:
            1: name: 'punctuation.whitespace.comment.leading.haskell'
          patterns: [
              name: 'comment.line.double-dash.haddock.haskell'
              begin: /(--+)\s+([|^])/
              end: /\n/
              beginCaptures:
                1: name: 'punctuation.definition.comment.haskell'
                2: name: 'punctuation.definition.comment.haddock.haskell'
          ]
        ,
          ###
          Operators may begin with -- as long as they are not
          entirely composed of - characters. This means comments can't be
          immediately followed by an allowable operator character.
          ###
          begin: /({maybeBirdTrack}[ \t]+)?(?=--+(?!{operatorChar}))/
          end: /(?!\G)/
          beginCaptures:
            1: name: 'punctuation.whitespace.comment.leading.haskell'
          patterns: [
              name: 'comment.line.double-dash.haskell'
              begin: /--/
              end: /\n/
              beginCaptures:
                0: name: 'punctuation.definition.comment.haskell'
          ]
        ,
          include: '#block_comment'
      ]
    characters:
      patterns: [
          {match: '{escapeChar}', name: 'constant.character.escape.haskell'}
          {match: '{octalChar}', name: 'constant.character.escape.octal.haskell'}
          {match: '{hexChar}', name: 'constant.character.escape.hexadecimal.haskell'}
          {match: 'controlChar', name: 'constant.character.escape.control.haskell'}
        ]
    infix_op:
      name: 'entity.name.function.infix.haskell'
      match: /{operatorFun}/
    module_exports:
      name: 'meta.declaration.exports.haskell'
      begin: /\(/
      end: /\)/
      patterns: [
          include: '#comments'
        ,
          name: 'entity.name.function.haskell'
          match: /{lb}{functionName}{rb}/
        ,
          include: '#type_name'
        ,
          name: 'punctuation.separator.comma.haskell'
          match: /,/
        ,
          include: '#infix_op'
        ,
          name: 'meta.other.constructor-list.haskell'
          begin: /\(/
          end: /\)/
          patterns: [
            include: '#type_ctor'
          ]
      ]
    module_name:
      name: 'support.other.module.haskell'
      match: /(?:{className}\.)*{className}\.?/
    pragma:
      name: 'meta.preprocessor.haskell'
      begin: /\{-#/
      end: /#-\}/
      patterns: [
          match: /{lb}(LANGUAGE|OPTIONS_GHC|INCLUDE|WARNING|DEPRECATED|INLINE|NOINLINE|ANN|LINE|RULES|SPECIALIZE|UNPACK|SOURCE){rb}/
          name: 'keyword.other.preprocessor.haskell'
      ]
    function_type_declaration:
      name: 'meta.function.type-declaration.haskell'
      begin: concat /{maybeBirdTrack}(\s*)/, /{functionTypeDeclaration}/
      end: /{indentBlockEnd}/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        2:
          patterns: [
              name: 'entity.name.function.haskell'
              match: /{functionName}/
            ,
              include: '#infix_op'
          ]
        3: name: 'keyword.other.double-colon.haskell'
      patterns: [
          include: '#type_signature'
      ]
    ctor_type_declaration:
      name: 'meta.ctor.type-declaration.haskell'
      begin: concat /{maybeBirdTrack}(\s*)/, /{ctorTypeDeclaration}/
      end: /{indentBlockEnd}/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        2:
          patterns: [
              name: 'entity.name.type.haskell'
              match: /{className}/
            ,
              include: '#infix_op'
          ]
        3: name: 'keyword.other.double-colon.haskell'
      patterns: [
          include: '#type_signature'
      ]
    record_field_declaration:
      name: 'meta.record-field.type-declaration.haskell'
      begin: /{lb}{functionTypeDeclaration}/
      end: /(?={functionTypeDeclaration}|})/
      contentName: 'meta.type-signature.haskell'
      beginCaptures:
        1:
          patterns: [
              name: 'entity.other.attribute-name.haskell'
              match: /{lb}{functionName}{rb}/
            ,
              include: '#infix_op'
          ]
        2: name: 'keyword.other.double-colon.haskell'
      patterns: [
          include: '#type_signature'
      ]
    type_signature:
      patterns: [
          name: 'meta.class-constraints.haskell'
          match: concat /\(/,
            list('classConstraints', /{classConstraint}/, /,/),
            /\)/, /\s*(=>|⇒)/
          captures:
            1: patterns: [{include: '#class_constraint'}]
            #2,3 are from classConstraint
            4: name: 'keyword.other.big-arrow.haskell'
        ,
          name: 'meta.class-constraints.haskell'
          match: /({classConstraint})\s*(=>|⇒)/
          captures:
            1: patterns: [{include: '#class_constraint'}]
            #2,3 are from classConstraint
            4: name: 'keyword.other.big-arrow.haskell'
        ,
          include: '#pragma'
        ,
          name: 'keyword.other.arrow.haskell'
          match: /->|→/
        ,
          name: 'keyword.other.big-arrow.haskell'
          match: /=>|⇒/
        ,
          name: 'support.class.prelude.haskell'
          match: "{lb}(#{prelude.types.join('|')}){rb}"
        ,
          include: '#generic_type'
        ,
          include: '#type_name'
        ,
          include: '#unit'
        ,
          include: '#comments'
      ]
    type_name:
      name: 'entity.name.type.haskell'
      match: /{lb}{className}{rb}/
    type_ctor:
      name: 'entity.name.tag.haskell'
      match: /{lb}{className}{rb}/
    unit:
      name: 'constant.language.unit.haskell'
      match: /\(\)/
    generic_type:
      name: 'variable.other.generic-type.haskell'
      match: /{lb}{functionName}{rb}/
    class_constraint:
      name: 'meta.class-constraint.haskell'
      match: /{classConstraint}/
      captures:
        1: patterns: [
          name: 'entity.other.inherited-class.haskell'
          match: /{lb}{className}{rb}/
        ]
        2: patterns: [
            include: '#type_name'
          ,
            include: '#generic_type'
        ]
    deriving:
      patterns: [
          include: '#deriving_list'
        ,
          include: '#deriving_simple'
        ,
          include: '#deriving_keyword'
      ]
    deriving_keyword:
      name: 'meta.deriving.haskell'
      match: /{lb}(deriving){rb}/
      captures:
        1: name: 'keyword.other.haskell'
    deriving_list:
      name: 'meta.deriving.haskell'
      begin: /{lb}(deriving)\s*\(/
      end: /\)/
      beginCaptures:
        1: name: 'keyword.other.haskell'
      patterns: [
          match: /{lb}({className}){rb}/
          captures:
            1: name: 'entity.other.inherited-class.haskell'
      ]
    deriving_simple:
      name: 'meta.deriving.haskell'
      match: /{lb}(deriving)\s*({className}){rb}/
      captures:
        1: name: 'keyword.other.haskell'
        2: name: 'entity.other.inherited-class.haskell'

makeGrammar haskellGrammar, "grammars/haskell.cson"

completionHintGrammar =
  name: 'Haskell Autocompletion Hint'
  fileTypes: []
  scopeName: 'hint.haskell'

  macros: haskellGrammar.macros
  patterns: [
      include: '#function_type_declaration'
    ,
      include: '#ctor_type_declaration'
  ]
  repository: haskellGrammar.repository

makeGrammar completionHintGrammar, "grammars/haskell autocompletion hint.cson"

typeHintGrammar =
  name: 'Haskell Type Hint'
  fileTypes: []
  scopeName: 'hint.type.haskell'

  macros: haskellGrammar.macros
  patterns: [
      include: '#type_signature'
  ]
  repository: haskellGrammar.repository

makeGrammar typeHintGrammar, "grammars/haskell type hint.cson"

messageHintGrammar =
  name: 'Haskell Message Hint'
  fileTypes: []
  scopeName: 'hint.message.haskell'

  macros: haskellGrammar.macros
  patterns: [
      match: /^[^:]*:(.+)$/
      captures:
        1:
          patterns: [
            include: 'source.haskell'
          ]
    ,
      begin: /^[^:]*:$/
      end: /^(?=\S)/
      patterns: [
        include: 'source.haskell'
      ]
    ,
      begin: /‘/
      end: /’/
      patterns: [
        include: 'source.haskell'
      ]
  ]
  repository: haskellGrammar.repository

makeGrammar messageHintGrammar, "grammars/haskell message hint.cson"

literateHaskellGrammar =
  name: 'Literate Haskell'
  fileTypes: [ 'lhs' ]
  scopeName: 'text.tex.latex.haskell'

  macros: haskellGrammar.macros
  patterns: [
      begin: /^((\\)begin)({)(code|spec)(})(\s*\n)?/
      beginCaptures:
        1:
          name: 'support.function.be.latex'
        2:
          name: 'punctuation.definition.function.latex'
        3:
          name: 'punctuation.definition.arguments.begin.latex'
        5:
          name: 'punctuation.definition.arguments.end.latex'
      end: /^((\\)end)({)\4(})/
      endCaptures:
        1:
          name: 'support.function.be.latex'
        2:
          name: 'punctuation.definition.function.latex'
        3:
          name: 'punctuation.definition.arguments.begin.latex'
        4:
          name: 'punctuation.definition.arguments.end.latex'
      contentName: 'source.haskell.embedded.latex'
      name: 'meta.embedded.block.haskell.latex'
      patterns: [
          include: 'source.haskell'
      ]
    ,
      begin: /^(?=[><] )/
      end: /^(?![><] )/
      name: 'meta.embedded.haskell'
      patterns: haskellGrammar.patterns.concat
        match: /^> /
        name: 'punctuation.definition.bird-track.haskell'
    ,
      begin: '(?<!\\\\verb)\\|'
      end: /\|/
      name: 'meta.embedded.text.haskell.latex'
      patterns: haskellGrammar.patterns
    ,
      include: 'text.tex.latex'
  ]
  repository: haskellGrammar.repository

literateHaskellGrammar.macros.maybeBirdTrack = /^(?:>|<) /
literateHaskellGrammar.macros.indentBlockEnd =
  /^(?!(?:>|<) \1{indentChar}|(?:>|<) {indentChar}*$)|^(?!(?:>|<) )/
literateHaskellGrammar.macros.operatorChar = /[\p{S}\p{P}&&[^(),;\[\]`{}_"'\|]]/

makeGrammar literateHaskellGrammar, "grammars/literate haskell.cson"
