describe "Language-Haskell", ->
  grammar = null

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.haskell")

  it "parses the grammar", ->
    expect(grammar).toBeTruthy()
    expect(grammar.scopeName).toBe "source.haskell"

  describe "chars", ->
    it 'tokenizes general chars', ->
      chars = ['a', '0', '9', 'z', '@', '0', '"']

      for scope, char of chars
        {tokens} = grammar.tokenizeLine("'" + char + "'")
        expect(tokens).toEqual [
          {value:"'", scopes: ["source.haskell", 'string.quoted.single.haskell', "punctuation.definition.string.begin.haskell"]}
          {value: char, scopes: ["source.haskell", 'string.quoted.single.haskell']}
          {value:"'", scopes: ["source.haskell", 'string.quoted.single.haskell', "punctuation.definition.string.end.haskell"]}
        ]

    it 'tokenizes escape chars', ->
      escapeChars = ['\\t', '\\n', '\\\'']
      for scope, char of escapeChars
        {tokens} = grammar.tokenizeLine("'" + char + "'")
        expect(tokens).toEqual [
          {value:"'", scopes: ["source.haskell", 'string.quoted.single.haskell', "punctuation.definition.string.begin.haskell"]}
          {value: char, scopes: ["source.haskell", 'string.quoted.single.haskell', 'constant.character.escape.haskell']}
          {value:"'", scopes: ["source.haskell", 'string.quoted.single.haskell', "punctuation.definition.string.end.haskell"]}
        ]

  describe "strings", ->
    it "tokenizes single-line strings", ->
      {tokens} = grammar.tokenizeLine '"abcde\\n\\EOT\\EOL"'
      expect(tokens).toEqual  [
        { value : '"', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'punctuation.definition.string.begin.haskell' ] }
        { value : 'abcde', scopes : [ 'source.haskell', 'string.quoted.double.haskell' ] }
        { value : '\\n', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'constant.character.escape.haskell' ] }
        { value : '\\EOT', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'constant.character.escape.haskell' ] }
        { value : '\\EOL', scopes : [ 'source.haskell', 'string.quoted.double.haskell' ] }
        { value : '"', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'punctuation.definition.string.end.haskell' ] }
      ]


  describe "backtick function call", ->
    it "finds backtick function names", ->
      {tokens} = grammar.tokenizeLine("\`func\`")
      expect(tokens[0]).toEqual value: '`', scopes: ['source.haskell', 'keyword.operator.function.infix.haskell','punctuation.definition.entity.haskell']
      expect(tokens[1]).toEqual value: 'func', scopes: ['source.haskell', 'keyword.operator.function.infix.haskell']
      expect(tokens[2]).toEqual value: '`', scopes: ['source.haskell', 'keyword.operator.function.infix.haskell','punctuation.definition.entity.haskell']

  describe "keywords", ->
    controlKeywords = ['case', 'of', 'in', 'where', 'if', 'then', 'else']

    for scope, keyword of controlKeywords
      it "tokenizes #{keyword} as a keyword", ->
        {tokens} = grammar.tokenizeLine(keyword)
        expect(tokens[0]).toEqual value: keyword, scopes: ['source.haskell', 'keyword.control.haskell']

  describe "operators", ->
    it "tokenizes the / arithmetic operator when separated by newlines", ->
      lines = grammar.tokenizeLines """
        1
        / 2
      """
      expect(lines).toEqual  [
          [
            { value : '1', scopes : [ 'source.haskell', 'constant.numeric.decimal.haskell' ] }
          ],
          [
            { value : '/', scopes : [ 'source.haskell', 'keyword.operator.haskell' ] }
            { value : ' ', scopes : [ 'source.haskell' ] }
            { value : '2', scopes : [ 'source.haskell', 'constant.numeric.decimal.haskell' ] }
          ]
        ]

  it "tokenizes {-  -} comments", ->
    {tokens} = grammar.tokenizeLine('{--}')

    expect(tokens).toEqual [
        { value : '{-', scopes : [ 'source.haskell', 'comment.block.haskell', 'punctuation.definition.comment.haskell' ] }
        { value : '-}', scopes : [ 'source.haskell', 'comment.block.haskell' ] }
      ]

    {tokens} = grammar.tokenizeLine('{- foo -}')
    expect(tokens).toEqual  [
        { value : '{-', scopes : [ 'source.haskell', 'comment.block.haskell', 'punctuation.definition.comment.haskell' ] }
        { value : ' foo ', scopes : [ 'source.haskell', 'comment.block.haskell' ] }
        { value : '-}', scopes : [ 'source.haskell', 'comment.block.haskell' ] }
      ]

  describe "ids", ->
    it 'handles type_ids', ->
      typeIds = ['Char', 'Data', 'List', 'Int', 'Integral', 'Float', 'Date']

      for scope, id of typeIds
        {tokens} = grammar.tokenizeLine(id)
        expect(tokens[0]).toEqual value: id, scopes: ['source.haskell', 'entity.name.tag.haskell']

  describe ':: declarations', ->
    it 'parses newline declarations', ->
      data = 'function :: Type -> OtherType'
      {tokens} = grammar.tokenizeLine(data)
      expect(tokens).toEqual [
          { value : 'function', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'entity.name.function.haskell' ] }
          { value : ' ', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell' ] }
          { value : '::', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'keyword.other.double-colon.haskell' ] }
          { value : ' ', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell' ] }
          { value : 'Type', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell' ] }
          { value : ' ', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell' ] }
          { value : '->', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell', 'keyword.other.arrow.haskell' ] }
          { value : ' ', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell' ] }
          { value : 'OtherType', scopes : [ 'source.haskell', 'meta.function.type-declaration.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell' ] }
        ]

    it 'parses in-line parenthesised declarations', ->
      data = 'main = (putStrLn :: String -> IO ()) ("Hello World" :: String)'
      {tokens} = grammar.tokenizeLine(data)
      expect(tokens).toEqual [
        { value : 'main', scopes : [ 'source.haskell', 'identifier.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '=', scopes : [ 'source.haskell', 'keyword.operator.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '(', scopes : [ 'source.haskell' ] }
        { value : 'putStrLn', scopes : [ 'source.haskell', 'support.function.prelude.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '::', scopes : [ 'source.haskell', 'keyword.other.double-colon.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : 'String', scopes : [ 'source.haskell', 'meta.type-signature.haskell', 'support.class.prelude.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : '->', scopes : [ 'source.haskell', 'meta.type-signature.haskell', 'keyword.other.arrow.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : 'IO', scopes : [ 'source.haskell', 'meta.type-signature.haskell', 'support.class.prelude.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : '()', scopes : [ 'source.haskell', 'meta.type-signature.haskell', 'constant.language.unit.haskell' ] }
        { value : ')', scopes : [ 'source.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '(', scopes : [ 'source.haskell' ] }
        { value : '"', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'punctuation.definition.string.begin.haskell' ] }
        { value : 'Hello World', scopes : [ 'source.haskell', 'string.quoted.double.haskell' ] }
        { value : '"', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'punctuation.definition.string.end.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '::', scopes : [ 'source.haskell', 'keyword.other.double-colon.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : 'String', scopes : [ 'source.haskell', 'meta.type-signature.haskell', 'support.class.prelude.haskell' ] }
        { value : ')', scopes : [ 'source.haskell' ] }
      ]


    it 'parses in-line non-parenthesised declarations', ->
      data = 'main = putStrLn "Hello World" :: IO ()'
      {tokens} = grammar.tokenizeLine(data)
      expect(tokens).toEqual [
        { value : 'main', scopes : [ 'source.haskell', 'identifier.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '=', scopes : [ 'source.haskell', 'keyword.operator.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : 'putStrLn', scopes : [ 'source.haskell', 'support.function.prelude.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '"', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'punctuation.definition.string.begin.haskell' ] }
        { value : 'Hello World', scopes : [ 'source.haskell', 'string.quoted.double.haskell' ] }
        { value : '"', scopes : [ 'source.haskell', 'string.quoted.double.haskell', 'punctuation.definition.string.end.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell' ] }
        { value : '::', scopes : [ 'source.haskell', 'keyword.other.double-colon.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : 'IO', scopes : [ 'source.haskell', 'meta.type-signature.haskell', 'support.class.prelude.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.type-signature.haskell' ] }
        { value : '()', scopes : [ 'source.haskell', 'meta.type-signature.haskell', 'constant.language.unit.haskell' ] }
      ]

  describe 'regression test for 65', ->
    it 'works with space', ->
      data = 'data Foo = Foo {bar :: Bar}'
      {tokens} = grammar.tokenizeLine(data)
      expect(tokens).toEqual [
        { value : 'data', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'storage.type.data.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell' ] }
        { value : 'Foo', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell' ] }
        { value : '=', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'keyword.operator.assignment.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell' ] }
        { value : 'Foo', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'entity.name.tag.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell' ] }
        { value : '{', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'keyword.operator.record.begin.haskell' ] }
        { value : 'bar', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'entity.other.attribute-name.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell' ] }
        { value : '::', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'keyword.other.double-colon.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'meta.type-signature.haskell' ] }
        { value : 'Bar', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell' ] }
        { value : '}', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'keyword.operator.record.end.haskell' ] }
      ]

    it 'works without space', ->
      data = 'data Foo = Foo{bar :: Bar}'
      {tokens} = grammar.tokenizeLine(data)
      expect(tokens).toEqual [
        { value : 'data', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'storage.type.data.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell' ] }
        { value : 'Foo', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell' ] }
        { value : '=', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'keyword.operator.assignment.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell' ] }
        { value : 'Foo', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'entity.name.tag.haskell' ] }
        { value : '{', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'keyword.operator.record.begin.haskell' ] }
        { value : 'bar', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'entity.other.attribute-name.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell' ] }
        { value : '::', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'keyword.other.double-colon.haskell' ] }
        { value : ' ', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'meta.type-signature.haskell' ] }
        { value : 'Bar', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'meta.record-field.type-declaration.haskell', 'meta.type-signature.haskell', 'entity.name.type.haskell' ] }
        { value : '}', scopes : [ 'source.haskell', 'meta.declaration.type.data.haskell', 'meta.declaration.type.data.record.block.haskell', 'keyword.operator.record.end.haskell' ] }
      ]
