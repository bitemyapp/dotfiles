# Advanced Open File

[![TravisCI Build Status](https://travis-ci.org/Osmose/advanced-open-file.svg)](https://travis-ci.org/Osmose/advanced-open-file)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/cwyb7f46dd1bbuxh/branch/master?svg=true)](https://ci.appveyor.com/project/Osmose/advanced-open-file/branch/master)



Advanced Open File is a package for helping Atom users to open files and folders
easily. It can also create new files and folders if they don't exist.

![Screenshot of plugin](http://osmose.github.io/advanced-open-file/screenshot.png)

Advanced Open File is fork of
[Advanced New File](https://github.com/Trudko/advanced-new-file), itself a fork
of [Fancy New File](https://github.com/rev087/fancy-new-file). Thanks to both
rev087 and Trudko for their work.

## Usage

Hit `Cmd-Alt-O`/`Ctrl-Alt-O` to open the file list to the directory of the
current file. As you edit the path the file list will automatically show
matching files and directories. Hit `Tab` to autocomplete the path.

Relative paths are considered relative to the current project's first root
folder.

Hit `Enter` to open the file at the given path. If the file doesn't exist, a tab
will be opened that will save to that file. Any directories in the path that
don't exist will be created immediately upon hitting `Enter`.

You can also click on any entry in the file list to add it to the current path
(in the case of a directory) or to open it immediately (in the case of a file).
You can also use the `Up` and `Down` arrow keys to scroll through the list, and
`Enter` to select the currently-highlighted item.

If a directory has a plus symbol on the right side of its entry, clicking the
symbol will add it as a project directory. You can also add a highlighted
directory as a project directory using `Shift-Cmd-O`/`Ctrl-Alt-O`.

`Cmd-Z`/`Ctrl-Z` will undo changes made to the current path, such as
autocompletion or directory shortcuts.

You can use the keybindings for splitting panes (`Cmd-k <Arrow Key>` by default)
to open the selected path in a new split pane in the desired direction.

## Keybindings

Available commands for binding:

<dl>
  <dt><code>advanced-open-file:toggle</code></dt>
  <dd>Toggles the Advanced Open File dialog.</dd>

  <dt><code>core:confirm</code></dt>
  <dd>
    If a path has been selected with the cursor, open it. If no path is
    selected, open the current path in the input.
  </dd>

  <dt><code>advanced-open-file:confirm-selected-or-first</code></dt>
  <dd>
    Similar to <code>core:confirm</code>. If nothing is selected, select the
    first item in the list.
  </dd>

  <dt><code>core:cancel</code></dt>
  <dd>Close the Advanced Open File dialog.</dd>

  <dt>
    <code>pane:split-left</code>, <code>pane:split-right</code>,
    <code>pane:split-up</code>, and <code>pane:split-down</code>
  </dt>
  <dd>
    Open the selected path (or current path in input) in a new pane split from
    the current pane.
  </dd>

  <dt><code>application:add-project-folder</code></dt>
  <dd>
    If a folder path has been selected with the cursor, add it as a project
    directory.
  </dd>

  <dt><code>advanced-open-file:autocomplete</code></dt>
  <dd>Attempts to autocomplete the current input.</dd>

  <dt><code>advanced-open-file:undo</code></dt>
  <dd>Undo changes to the current path.</dd>

  <dt><code>advanced-open-file:move-cursor-up</code></dt>
  <dd>Move the cursor/highlight for the currently selected file up.</dd>

  <dt><code>advanced-open-file:move-cursor-down</code></dt>
  <dd>Move the cursor/highlight for the currently selected file down.</dd>

  <dt><code>advanced-open-file:delete-path-component</code></dt>
  <dd>
    A more powerful version of <code>alt-backspace</code> that erases the a
    directory component in the miniEditor including the slash ('/').
  </dd>
</dl>

The following extra keybindings are included by default:

Action                                     | Extra Keys
------------------------------------------ | ------------------
`advanced-open-file:move-cursor-up`        | `Ctrl-p`, `Ctrl-i`
`advanced-open-file:move-cursor-down`      | `Ctrl-n`, `Ctrl-k`
`advanced-open-file:delete-path-component` | `Ctrl-l`

You can of course remap the keys however you wish. For example, add the
following to your keymap to map `Ctrl-x Ctrl-f` to toggle the dialog and
`Ctrl-j` to move the cursor down:

```cson
'atom-workspace':
  'ctrl-x ctrl-f': 'advanced-open-file:toggle'

'.advanced-open-file atom-text-editor':
  'ctrl-j': 'advanced-open-file:move-cursor-down'
```

## Settings

<dl>
  <dt>Create directories</dt>
  <dd>
    Normally, when you attempt to open a path pointing to a directory that
    doesn't exist, an error beep sounds and nothing happens. When this setting
    is enabled, opening a non-existent directory will create it and show a
    notification confirming the creation.
  </dd>

  <dt>Create files instantly</dt>
  <dd>
    If checked, files are created immediately instead of on save if they don't
    exist.
  </dd>

  <dt>Default input value</dt>
  <dd>
    Determines what the default value in the path input is when the dialog is
    opened. Possible choices are nothing, the current project's root directory,
    or the directory of the currently-active file.
  </dd>

  <dt>Use fuzzy matching for matching filenames</dt>
  <dd>
    Changes the method for matching filenames while typing to use a fuzzy match
    algorithm instead of a prefix match one. When enabled, matches are sorted by
    their match weight instead of by name and type, and autocomplete
    automatically chooses the closest result instead of the common prefix among
    matching filenames.
  </dd>

  <dt>Shortcuts for fast directory switching</dt>
  <dd>
    <p>
      When enabled, allows for quick directory switching when appending certain
      strings to a path that ends in a slash:
    </p>
    <ul>
      <li>
        Adding an extra slash (e.g. <code>/</code>) will switch to the
        filesystem root.
      </li>
      <li>
        Adding a tilde and a slash (e.g. <code>~/</code>) will switch to the
        current user's home directory.
      </li>
      <li>
        Adding a colon and a slash (e.g. <code>:/</code>) will switch to the
        current project's root directory.
      </li>
    </ul>
  </dd>
</dl>

## Event Service

Other packages can subscribe to events to get notified when certain actions
happen in advanced-open-file. To do so, you'll need to consume the
`advanced-open-file-events` service:

### `package.json`

```json
"consumedServices": {
  "advanced-open-file-events": {
    "versions": {
      "0.1.0": "consumeEventService"
    }
  }
}
```

### Main Module

```coffeescript
{Disposable} = require 'atom'


module.exports =
  consumeEventService: (service) ->
    openDisposable = service.onDidOpenPath (path) ->
      console.log "Open: #{path}"

    createDisposable = service.onDidCreatePath (path) ->
      console.log "Create: #{path}"

    return new Disposable ->
      openDisposable.dispose()
      createDisposable.dispose()
```

### `onDidOpenPath`

Triggered when a file is opened via advanced-open-file.

```coffeescript
service.onDidOpenPath (path) ->
  console.log "Open: #{path}"
```

### `onDidCreatePath`

Triggered when a file is created via advanced-open-file. Note that this is only
triggered when the "Create files instantly" preference is enabled. It does not
trigger when the preference is disabled and a new file is opened and then
subsequently saved.

```coffeescript
service.onDidCreatePath (path) ->
  console.log "Create: #{path}"
```

## Contributing

First, if you're interested in contributing, thank you! It's awesome that you
want to help!

The easiest way to contribute is to [file an issue][] with the bug you've found
or the new feature you want added. If you're interested in implementing the fix
for your request yourself, or fixing an issue submitted by someone else, read
on.

[file an issue]: https://github.com/Osmose/advanced-open-file/issues/new

### Developer Setup

Setting up a development install is easy with [apm][]:

```sh
$ apm develop advanced-open-file /path/to/checkout
```

The command above will use Git to clone Advanced Open File to the
`/path/to/checkout` directory, download the dependencies, and create a symlink
in your `.atom` profile for the package.

Now, if you launch Atom with the `-d` flag, Atom will load the development
checkout of Advanced Open File (instead of the released version, if you have it
installed). Any changes you make to the code will be reflected if you use the
`Window: Reload` command in the [Command Palette][] to reload the editor.

That should be all you need to get started. Create a branch, write your changes,
and submit the branch as a pull request, and you should hear back shortly!

[apm]: https://github.com/atom/apm
[Command Palette]: https://atom.io/docs/latest/getting-started-atom-basics#command-palette

## License

Licensed under the MIT License. See `LICENSE` for details.
