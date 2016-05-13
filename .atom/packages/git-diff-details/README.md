# git-diff-details package

View git diffs directly in atom.

## Keybindings
  * `alt-g alt-d` to toggle the diff view
  * `escape` to close the diff view
  * `alt-u` for undo
  * `alt-c` for copy

## Syntax highlighting
You can choose whether the diff should be highlighted or not:

![git-diff-details](https://github.com/samu/git-diff-details/blob/master/flat.png?raw=true)

![git-diff-details](https://github.com/samu/git-diff-details/blob/master/highlighted.png?raw=true)

## Styling
You can style the diffs to your liking. Here's an example:

```less
atom-text-editor::shadow .line {
  &.git-diff-details-new-highlighted {
    background-color: rgba(162, 232, 120, 0.4) !important;
  }

  &.git-diff-details-old-highlighted {
    background-color: rgba(232, 120, 120, 0.4) !important;
  }

  &.git-diff-details-new-flat {
    background-color: rgba(162, 232, 120, 0.7) !important;
  }

  &.git-diff-details-old-flat {
    background-color: rgba(232, 120, 120, 0.7) !important;
  }
}
```
