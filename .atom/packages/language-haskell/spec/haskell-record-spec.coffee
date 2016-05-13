describe 'Record', ->
  grammar = null

  zip = () ->
    lengthArray = (arr.length for arr in arguments)
    length = Math.max(lengthArray...)
    for i in [0...length]
      arr[i] for arr in arguments

  check = (line, exp) ->
    for t,i in zip(line,exp)
      t[0] ?= {}
      t[1] ?= {}
      t[0].index=i
      t[1].index=i
      expect(t[0]).toEqual(t[1])

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage("language-haskell")

    runs ->
      grammar = atom.grammars.grammarForScopeName("source.haskell")

  it 'understands record syntax', ->
    string = """
      data Car = Car {
          company :: String,
          model :: String,
          year :: Int
        } deriving (Show)
      """
    lines = grammar.tokenizeLines(string)
    # console.log JSON.stringify(lines, undefined, 2)
    exp = [
      [
        {
          "value": "data",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "storage.type.data.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell"
          ]
        },
        {
          "value": "Car",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.type-signature.haskell",
            "entity.name.type.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell"
          ]
        },
        {
          "value": "=",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "keyword.operator.assignment.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell"
          ]
        },
        {
          "value": "Car",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "entity.name.tag.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell"
          ]
        },
        {
          "value": "{",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "keyword.operator.record.begin.haskell"
          ]
        }
      ],
      [
        {
          "value": "    ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell"
          ]
        },
        {
          "value": "company",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "entity.other.attribute-name.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell"
          ]
        },
        {
          "value": "::",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "keyword.other.double-colon.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell"
          ]
        },
        {
          "value": "String",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell",
            "support.class.prelude.haskell"
          ]
        },
        {
          "value": ",",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell"
          ]
        }
      ],
      [
        {
          "value": "    ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell"
          ]
        },
        {
          "value": "model",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "entity.other.attribute-name.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell"
          ]
        },
        {
          "value": "::",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "keyword.other.double-colon.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell"
          ]
        },
        {
          "value": "String",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell",
            "support.class.prelude.haskell"
          ]
        },
        {
          "value": ",",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell"
          ]
        }
      ],
      [
        {
          "value": "    ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell"
          ]
        },
        {
          "value": "year",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "entity.other.attribute-name.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell"
          ]
        },
        {
          "value": "::",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "keyword.other.double-colon.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell"
          ]
        },
        {
          "value": "Int",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell",
            "support.class.prelude.haskell"
          ]
        }
      ],
      [
        {
          "value": "  ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "meta.record-field.type-declaration.haskell",
            "meta.type-signature.haskell"
          ]
        },
        {
          "value": "}",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.declaration.type.data.record.block.haskell",
            "keyword.operator.record.end.haskell"
          ]
        },
        {
          "value": " ",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell"
          ]
        },
        {
          "value": "deriving",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.deriving.haskell",
            "keyword.other.haskell"
          ]
        },
        {
          "value": " (",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.deriving.haskell"
          ]
        },
        {
          "value": "Show",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.deriving.haskell",
            "entity.other.inherited-class.haskell"
          ]
        },
        {
          "value": ")",
          "scopes": [
            "source.haskell",
            "meta.declaration.type.data.haskell",
            "meta.deriving.haskell"
          ]
        }
      ]
    ]
    for l in zip(lines, exp)
      check l[0], l[1]
