Mark = require '../lib/mark'
EditorState = require './editor-state'
{keydown, getEditorElement} = require './spec-helper'

describe 'Emacs', ->
  [workspaceElement, editor, editorElement] = []

  beforeEach ->
    workspaceElement = atom.views.getView(atom.workspace)
    jasmine.attachToDOM(workspaceElement)

    runs ->
      getEditorElement (element) ->
        editorElement = element
        editor = editorElement.getModel()


  describe 'activate', ->
    it "puts the editor in emacs-plus class", ->
      expect(editorElement.classList.contains('emacs-plus')).toBe(true)

  # describe 'deactivate', ->
  #   beforeEach ->
  #     atom.packages.deactivatePackage('emacs-plus')
  #
  #   it "removes the emacs-plus classe from the editor", ->
  #     expect(editor.isAlive()).toBe(true)
  #     expect(editorElement.classList.contains('emacs-plus')).toBe(false)

  describe 'emacs-plus:transpose-words', ->
    it 'transposes the current word with the one after it', ->
      EditorState.set(editor, "aaa b[0]bb .\tccc ddd")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-words')
      expect(EditorState.get(editor)).toEqual("aaa ccc .\tbbb[0] ddd")

    it 'transposes the previous and next words if at the end of a word', ->
      EditorState.set(editor, "aaa bbb[0] .\tccc ddd")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-words')
      expect(EditorState.get(editor)).toEqual("aaa ccc .\tbbb[0] ddd")

    it 'transposes the previous and next words if at the beginning of a word', ->
      EditorState.set(editor, "aaa bbb .\t[0]ccc ddd")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-words')
      expect(EditorState.get(editor)).toEqual("aaa ccc .\tbbb[0] ddd")

    it "transposes the previous and next words if in between words", ->
      EditorState.set(editor, "aaa bbb .[0]\tccc ddd")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-words')
      expect(EditorState.get(editor)).toEqual("aaa ccc .\tbbb[0] ddd")

    it "moves to the start of the last word if in the last word", ->
      # Emacs leaves point at the start of the word, but that seems unintuitive.
      EditorState.set(editor, "aaa bbb .\tcc[0]c ")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-words')
      expect(EditorState.get(editor)).toEqual("aaa bbb .\tccc[0] ")

    it "transposes the last two words if at the start of the last word", ->
      EditorState.set(editor, "aaa bbb .\t[0]ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-words')
      expect(EditorState.get(editor)).toEqual("aaa ccc .\tbbb[0]")

    it "transposes the first two words if at the start of the buffer", ->
      EditorState.set(editor, "[0]aaa .\tbbb ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-words')
      expect(EditorState.get(editor)).toEqual("bbb .\taaa[0] ccc")

    it "moves to the start of the word if it's the only word in the buffer", ->
      # Emacs leaves point at the start of the word, but that seems unintuitive.
      EditorState.set(editor, " \taaa [0]\t")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-words')
      expect(EditorState.get(editor)).toEqual(" \taaa[0] \t")

  describe "emacs-plus:transpose-lines", ->
    it "transposes this line with the previous one, and moves to the next line", ->
      EditorState.set(editor, "aaa\nb[0]bb\nccc\n")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-lines')
      expect(EditorState.get(editor)).toEqual("bbb\naaa\n[0]ccc\n")

    it "pretends it's on the second line if it's on the first", ->
      EditorState.set(editor, "a[0]aa\nbbb\nccc\n")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-lines')
      expect(EditorState.get(editor)).toEqual("bbb\naaa\n[0]ccc\n")

    it "creates a newline at end of file if necessary", ->
      EditorState.set(editor, "aaa\nb[0]bb")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-lines')
      expect(EditorState.get(editor)).toEqual("bbb\naaa\n[0]")

    it "still transposes if at the end of the buffer after a trailing newline", ->
      EditorState.set(editor, "aaa\nbbb\n[0]")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-lines')
      expect(EditorState.get(editor)).toEqual("aaa\n\nbbb\n[0]")

    it "inserts a blank line at the top if there's only one line with a trailing newline", ->
      EditorState.set(editor, "a[0]aa\n")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-lines')
      expect(EditorState.get(editor)).toEqual("\naaa\n[0]")

    it "inserts a blank line at the top if there's only one line with no trailing newline", ->
      EditorState.set(editor, "a[0]aa")
      atom.commands.dispatch(editorElement, 'emacs-plus:transpose-lines')
      expect(EditorState.get(editor)).toEqual("\naaa\n[0]")

  describe "emacs-plus:delete-horizontal-space", ->
    it "deletes all horizontal space around each cursor", ->
      EditorState.set(editor, "a [0]\tb c [1]\td")
      atom.commands.dispatch(editorElement, 'emacs-plus:delete-horizontal-space')
      expect(EditorState.get(editor)).toEqual("a[0]b c[1]d")

    it "deletes all horizontal space to the beginning of the buffer if in leading space", ->
      EditorState.set(editor, " [0]\ta")
      atom.commands.dispatch(editorElement, 'emacs-plus:delete-horizontal-space')
      expect(EditorState.get(editor)).toEqual("[0]a")

    it "deletes all horizontal space to the end of the buffer if in trailing space", ->
      EditorState.set(editor, "a [0]\t")
      atom.commands.dispatch(editorElement, 'emacs-plus:delete-horizontal-space')
      expect(EditorState.get(editor)).toEqual("a[0]")

    it "deletes all text if the buffer only contains horizontal spaces", ->
      EditorState.set(editor, " [0]\t")
      atom.commands.dispatch(editorElement, 'emacs-plus:delete-horizontal-space')
      expect(EditorState.get(editor)).toEqual("[0]")

    it "does not modify the buffer if there is no horizontal space around the cursor", ->
      EditorState.set(editor, "a[0]b")
      atom.commands.dispatch(editorElement, 'emacs-plus:delete-horizontal-space')
      expect(EditorState.get(editor)).toEqual("a[0]b")

  describe "emacs-plus:kill-word", ->
    it "deletes from the cursor to the end of the word if inside a word", ->
      EditorState.set(editor, "aaa b[0]bb ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      expect(EditorState.get(editor)).toEqual("aaa b[0] ccc")

    it "deletes the word in front of the cursor if at the beginning of a word", ->
      EditorState.set(editor, "aaa [0]bbb ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      expect(EditorState.get(editor)).toEqual("aaa [0] ccc")

    it "deletes the next word if at the end of a word", ->
      EditorState.set(editor, "aaa[0] bbb ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      expect(EditorState.get(editor)).toEqual("aaa[0] ccc")

    it "deletes the next word if between words", ->
      EditorState.set(editor, "aaa [0] bbb ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      expect(EditorState.get(editor)).toEqual("aaa [0] ccc")

    it "does nothing if at the end of the buffer", ->
      EditorState.set(editor, "aaa bbb ccc[0]")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      expect(EditorState.get(editor)).toEqual("aaa bbb ccc[0]")

    it "deletes any selected text", ->
      EditorState.set(editor, "aaa b(0)b[0]b ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      expect(EditorState.get(editor)).toEqual("aaa b[0]b ccc")

    it "operates on multiple cursors", ->
      EditorState.set(editor, "aaa b[0]bb c[1]cc ddd")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      expect(EditorState.get(editor)).toEqual("aaa b[0] c[1] ddd")

    it 'appending kills', ->
      EditorState.set(editor, 'aaa [0] bbb ccc')
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      expect(atom.clipboard.read()).toBe ' bbb ccc'
      expect(EditorState.get(editor)).toEqual('aaa [0]')

  describe "emacs-plus:backward-kill-word", ->
    it "deletes from the cursor to the beginning of the word if inside a word", ->
      EditorState.set(editor, "aaa bb[0]b ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(EditorState.get(editor)).toEqual("aaa [0]b ccc")

    it "deletes the word behind the cursor if at the end of a word", ->
      EditorState.set(editor, "aaa bbb[0] ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(EditorState.get(editor)).toEqual("aaa [0] ccc")

    it "deletes the previous word if at the beginning of a word", ->
      EditorState.set(editor, "aaa bbb [0]ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(EditorState.get(editor)).toEqual("aaa [0]ccc")

    it "deletes the previous word if between words", ->
      EditorState.set(editor, "aaa bbb [0] ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(EditorState.get(editor)).toEqual("aaa [0] ccc")

    it "does nothing if at the beginning of the buffer", ->
      EditorState.set(editor, "[0]aaa bbb ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(EditorState.get(editor)).toEqual("[0]aaa bbb ccc")

    it "deletes the leading space behind the cursor if at the beginning of the buffer", ->
      EditorState.set(editor, " [0] aaa bbb ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(EditorState.get(editor)).toEqual("[0] aaa bbb ccc")

    it "deletes any selected text", ->
      EditorState.set(editor, "aaa b(0)b[0]b ccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(EditorState.get(editor)).toEqual("aaa b[0]b ccc")

    it "operates on multiple cursors", ->
      EditorState.set(editor, "aaa bb[0]b cc[1]c ddd")
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(EditorState.get(editor)).toEqual("aaa [0]b [1]c ddd")

    it 'appending kills', ->

      EditorState.set(editor, 'aaa bbb ccc[0]')
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(atom.clipboard.read()).toBe 'bbb ccc'
      expect(EditorState.get(editor)).toEqual('aaa [0]')

    it 'appending kills on multiple cursors', ->
      EditorState.set(editor, "111 aaa bb[0]b 222 cc[1]c ddd")
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      atom.commands.dispatch(editorElement, 'emacs-plus:backward-kill-word')
      expect(atom.clipboard.read()).toBe "aaa bb\n222 cc"
      EditorState.set(editor, "111 [0]b [1]c ddd")

  describe "emacs-plus:just-one-space", ->
    it "replaces all horizontal space around each cursor with one space", ->
      EditorState.set(editor, "a [0]\tb c [1]\td")
      atom.commands.dispatch(editorElement, 'emacs-plus:just-one-space')
      expect(EditorState.get(editor)).toEqual("a [0]b c [1]d")

    it "replaces all horizontal space at the beginning of the buffer with one space if in leading space", ->
      EditorState.set(editor, " [0]\ta")
      atom.commands.dispatch(editorElement, 'emacs-plus:just-one-space')
      expect(EditorState.get(editor)).toEqual(" [0]a")

    it "replaces all horizontal space at the end of the buffer with one space if in trailing space", ->
      EditorState.set(editor, "a [0]\t")
      atom.commands.dispatch(editorElement, 'emacs-plus:just-one-space')
      expect(EditorState.get(editor)).toEqual("a [0]")

    it "replaces all text with one space if the buffer only contains horizontal spaces", ->
      EditorState.set(editor, " [0]\t")
      atom.commands.dispatch(editorElement, 'emacs-plus:just-one-space')
      expect(EditorState.get(editor)).toEqual(" [0]")

    it "does not modify the buffer if there is already exactly one space at around the cursor", ->
      EditorState.set(editor, "a[0]b")
      atom.commands.dispatch(editorElement, 'emacs-plus:just-one-space')
      expect(EditorState.get(editor)).toEqual("a [0]b")

  describe "emacs-plus:set-mark", ->
    it "sets and activates the mark of all cursors", ->
      EditorState.set(editor, "[0].[1]")
      [cursor0, cursor1] = editor.getCursors()
      atom.commands.dispatch(editorElement, 'emacs-plus:set-mark')

      expect(Mark.for(editor).isActive()).toBe(true)
      point = cursor0.getBufferPosition()
      expect([point.row, point.column]).toEqual([0, 0])

      expect(Mark.for(editor).isActive()).toBe(true)
      point = cursor1.getBufferPosition()
      expect([point.row, point.column]).toEqual([0, 1])

  describe "emacs-plus:keyboard-quit", ->
    it "deactivates all marks", ->
      EditorState.set(editor, "[0].[1]")
      mark = Mark.for(editor)
      mark.activate()
      atom.commands.dispatch(editorElement, 'core:cancel')
      expect(mark.isActive()).toBe(false)

  describe "emacs-plus:exchange-point-and-mark", ->
    it "exchanges all cursors with their marks", ->
      mark = Mark.for(editor)
      EditorState.set(editor, "[0]..[1].")
      mark.activate(true)
      keydown('f', ctrl: true)
      atom.commands.dispatch(editorElement, 'emacs-plus:exchange-point-and-mark')
      expect(EditorState.get(editor)).toEqual("[0].(0).[1].(1)")

  describe 'emacs-plus:kill-whole-line', ->
    it 'kill an entire line at once', ->
      EditorState.set(editor, "aa\nb[0]b\ncc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-whole-line')
      expect(EditorState.get(editor)).toEqual("aa\n[0]cc")

    it 'ignore the selection', ->
      EditorState.set(editor, "a(0)aa[0]\nbbb\nccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-whole-line')
      expect(EditorState.get(editor)).toEqual("[0]bbb\nccc")

      EditorState.set(editor, "a(0)aa\nbb[0]b\nccc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-whole-line')
      expect(EditorState.get(editor)).toEqual("aaa\n[0]ccc")

    it 'appending kills', ->
      EditorState.set(editor, "aa\nb[0]b\ncc\ndd")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-whole-line')
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-whole-line')
      expect(atom.clipboard.read()).toBe "bb\ncc\n"
      expect(EditorState.get(editor)).toEqual("aa\n[0]dd")

  describe 'emacs-plus:append-next-kill', ->
    it 'appending kills', ->
      EditorState.set(editor, '[0]aaa bbb ccc ddd')
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      atom.commands.dispatch(editorElement, 'editor:move-to-end-of-word')
      atom.commands.dispatch(editorElement, 'emacs-plus:append-next-kill')
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-word')
      expect(atom.clipboard.read()).toBe 'aaa ccc'
      expect(EditorState.get(editor)).toEqual(' bbb[0] ddd')

  describe 'emacs-plus:capitalize-word', ->
    it 'capitalize the current word', ->
      EditorState.set(editor, 'aaa b[0]bb ccc')
      atom.commands.dispatch(editorElement, 'emacs-plus:capitalize-word')
      expect(EditorState.get(editor)).toEqual('aaa B[0]bb ccc')

    it 'capitalize the current selection', ->
      EditorState.set(editor, 'aaa b(0)bb[0] ccc')
      atom.commands.dispatch(editorElement, 'emacs-plus:capitalize-word')
      expect(EditorState.get(editor)).toEqual('aaa b(0)Bb[0] ccc')

  describe 'emacs-plus:delete-indentation', ->
    it "joins the current line with the previous one if at the start of the line", ->
      EditorState.set(editor, "aa \n[0] bb\ncc")
      atom.commands.dispatch(editorElement, 'emacs-plus:delete-indentation')
      expect(EditorState.get(editor)).toEqual("aa[0] bb\ncc")

    it "does exactly the same thing if at the end of the line", ->
      EditorState.set(editor, "aa \n bb[0]\ncc")
      atom.commands.dispatch(editorElement, 'emacs-plus:delete-indentation')
      expect(EditorState.get(editor)).toEqual("aa[0] bb\ncc")

    it "joins the two empty lines if they're both blank", ->
      EditorState.set(editor, "aa\n\n[0]\nbb")
      atom.commands.dispatch(editorElement, 'emacs-plus:delete-indentation')
      expect(EditorState.get(editor)).toEqual("aa\n[0]\nbb")

  describe 'emacs-plus:kill-line', ->
    it 'inside a line', ->
      EditorState.set(editor, "aa\nb[0]b\ncc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-line')
      expect(EditorState.get(editor)).toEqual("aa\nb[0]\ncc")

    it 'end of line', ->
      EditorState.set(editor, "aa\nbb[0]\ncc")
      atom.commands.dispatch(editorElement, 'emacs-plus:kill-line')
      expect(EditorState.get(editor)).toEqual("aa\nbb[0]cc")
      atom.commands.dispatch(editorElement, 'core:paste')
      expect(EditorState.get(editor)).toEqual("aa\nbb\n[0]cc")

    it 'appending kills', ->
      EditorState.set(editor, "aa\n[0]bb\ncc\ndd")

      for n in [0...4]
        atom.commands.dispatch(editorElement, 'emacs-plus:kill-line')

      {text, metadata} = atom.clipboard.readWithMetadata()
      expect(text).toBe "bb\ncc\n"
      expect(metadata.fullLine).toBe false
      expect(metadata.indentBasis).toBe 0
