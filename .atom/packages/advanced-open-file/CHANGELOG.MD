0.16.0 (April 18, 2016)
=======================
* #103, #104: Revert 0.15.0 due to conflicts with other packages using
  react-for-atom.


0.15.0 (April 17, 2016)
=======================
* #86: Improve performance when listing large directories by caching file reads
  and only rendering visible DOM nodes in the directory listing. This involved
  a rewrite of the view layer using React.


0.14.3 (March 6, 2016)
======================
* #93: Do not close dialog when adding project folders.


0.14.2 (February 6, 2016)
=========================
* #89: Add error handling when creating files or directories fails.


0.14.1 (December 29, 2015)
==========================

* Fix issue where opening a path immediately after autocompleting it opened the
  pre-autocomplete path instead of the full path.


0.14.0 (December 26, 2015)
==========================

* #82: Add shortcuts for opening files in new split panes.
* #57: Add setting for fuzzy filename matching.
* #84: Cursor no longer resets to the end of the path when typing in the middle
  of the path.
* #81: Fix project root shortcut when path input is otherwise empty.
* #80: Fall back to project root as initial path when the active editor doesn't
  have a path, such as the settings pane.
* #79: Fix gap below cursor in the textbox and cut-off shadows on the sides.


0.13.0 (November 29, 2015)
==========================

* Ignore all errors when running stat on files and treat them as if they don't
  exist. Previously only ENOENT errors were ignored.
* Fix bug in path shortcuts where they were being applied even if a path
  separator wasn't before the shortcut itself.
* #63: Implement event handler API as a versioned service for other packages.
* #60: Treat "~/" at the start of a path as the user's home directory if fast
  directory switching is turned off.
* #70: Add setting for enabling directory creation from the open dialog.
* #61: Add keyboard shortcut for adding project directories.
* #72: Determine modal max height programmatically for themes that place modals
  at odd positions.


0.12.3 (November 14, 2015)
==========================

* #69: Fix bug with autocomplete where we calculated the wrong bounds while
  finding the common prefix between two paths.
* #71: Re-add scrolling the file list when moving the cursor to an item that
  isn't currently visible.


0.12.2 (November 9, 2015)
=========================

* #67: Absolutify relative paths before attempting to create them.
* #66: Make autocomplete case-insensitive unless there is a capital letter in
  the path fragment. This makes it consistent with how the file listing behaves.


0.12.1 (November 9, 2015)
=========================

* #68: Removed Babel polyfill because it breaks other packages due to it's
  global polyfilling.


0.12.0 (November 6, 2015)
=========================

* Rewrote package using ES2015 via Atom's built-in Babel support. Replaced use
  of space-pen and jQuery with standard DOM functions for noticeably faster
  performance. Added specs for testing package behavior and added a TravisCI
  config to run them automatically.
* As part of rewrite, changed path separator behavior to detect which slash to
  use based on the existing path, e.g. `/foo/bar` uses `/`, while `\baz\biff`
  uses `\`.
* Removed the `Case-sensitive auto-completion` preference. Now auto-completion
  is always case-sensitive, and matching for the file listing is
  case-insensitive unless the current fragment contains a capital letter, in
  which case it becomes case-sensitive.


0.11.0 (October 5, 2015)
========================

* Add `advanced-open-file:delete-path-component` command for easily navigating
  directory hierarchies. (artagnon)
* Add `advanced-open-file:confirm-selected-or-first` command for easier opening
  of files; selects first item when nothing is selected. (artagnon)


0.10.0 (September 19, 2015)
===========================

* Disabled soft-word-wrap on the path input to avoid scrolling/wrapping issues
  on long paths.
* Improve directory detection for adding the project folder button. Fixes an
  issue with Windows directories starting with a drive letter missing the
  button.
* Rename Helm-style shortcuts to fast directory-switch shortcuts and add a new
  shortcut `:/` for switching to the project root directory.
* Only show the parent directory in the listing if the file list is not being
  filtered.
* Show "Add as project folder" button for all folders that are not project
  folders. Previously all folders within project folders were missing the
  button.
* Scroll cursor in input to the end of the text when triggering autocomplete.


0.9.2 (September 7, 2015)
=========================

* Improved dialog layout. (JesseLeite)
* Added event emitter, `onDidOpenFile`, and `onDidCreateFile` for other packages
  to detect when files are opened.


0.9.1 (September 4 2015)
========================

* Update description and README and publish to update the registry.


0.9.0 (September 4 2015)
========================

* Fix handling of Windows paths by replace all forward-slash instances
  with path.sep, and detecting the root directory for a path better.
* Removed support for creating multiple paths at once, as it doesn't
  really fit with what advanced-open-file is about and made the code
  harder to understand.
* Add `Ctrl-p`/`Ctrl-n` and `Ctrl-i`/`Ctrl-k` as alternative keybindings
  for moving through the file list, and use commands for all keybindings
  to make customization easier.
* Fix bug where clearing the input and entering "~/" with Helm-style
  shortcuts enabled did not automatically fill in the user's home
  directory.
* Added option for the default input value on toggle.
* When scrolling with the keyboard, the list of matching files now
  scrolls to show the selected file when it is out of view.
* Paths entered in the input that are not absolute are now considered
  relative to the first project root. When relative paths are being used, the
  directory listing will not show a parent entry past the project root.


0.8.2 (August 3 2015)
=====================

* Make icons in dropdown compatible with the file-icons package. SUCH FANCY
* Add icon to directories entries that, when clicked, adds them as project
  directories.


0.8.1 (July 14 2015)
====================

* Scroll to the end of the search box on attach in case the path is super long.


0.8.0 (July 11 2015)
====================

* Add option for Helm-style fast directory switching.
* Add ability to undo changes to the autocomplete field using Ctrl/Cmd-Z.
* Fix bug where clicking the icon or path in a list item would beep and
  fail instead of selecting the clicked list item.
* Fix bug where entering a directory named ~ was impossible with fast directory
  switching enabled.


0.7.2 (June 17 2015)
====================

* Default to project root when showing dialog while no tab is active.


0.7.1 (May 30 2015)
===================

* Add a GIF to the README (and publish a new release so atom.io picks it
  up).


0.7.0 (May 30 2015)
===================

* Add ability to hover and click on entries in the file/folder listing.
  Directories are added to the current path, while files are immediately
  opened.
* Add scrollbar when file/folder listing is taller than the window.
* Add ability to move through the file/folder listing with the keyboard.
  Up and down arrow keys will select list items and Enter will select
  them in the same way a mouse click does.
* Add entry at the top of the file/folder listing for the parent
  directory.
* Fix bug with appending a path separator to the filesystem root.


0.6.0 (May 24 2015)
===================

* Remove `suggestCurrentFilePath` setting, making it default behavior.
* Remove `addTextFromSelection` setting.
* Fix issue with dialog not being closed properly.
* Correctly focus currently-active editor when dialog is closed.


0.5.0 (May 24 2015)
===================

* Forked from Trudko/advanced-new-file to Osmose/advanced-open-file.
* Removed `removeWholeFolder` and `showFilesInAutocomplete` settings,
  making the latter the default behavior.
* Change shortcut to `ctrl-alt-o`.
* Change behavior to open existing files and create new files only for
  paths that don't exist.


0.4.3 (May 22 2015)
===================

* Removed deprecated API for calling listening for core:confirm and core:cancel


0.4.2 (May 15 2015)
===================

* Updated Context menu and use activationCommands intead of actiovationEvents(thanks to zimme)
* Removed deprecated configDefaults (thanks to olmokramer)


0.4.1 (Apr 27 2015)
==================
* Resolved problem with creating folder if create file instantly was set to true (thanks to phyllisstein)


0.4.0 (Apr 14 2015)
==================
* Added option for creating file immediately (thanks to silentvick)
* Fixed problem with overlay in Atom One theme (thanks to silentvick)


0.3.3 (Feb 15 2015)
===================

* Uncaught TypeError: undefined is not a function #9


0.3.2 (Feb 14 2015)
===================

* Focus editor on the start of the package


0.3.1 (Feb 14 2015)
===================

* Add text from selection only if there is active text editor


0.3.0 (Feb 14 2015)
===================

* Removed deprecation calls  #6
* New setting for inserting selection to the input field #7


0.2.2 (Feb 7 2015)
===================

* Opens new window with new file  #5
* Can't install v0.2.0  #4


0.2.1 (Feb 1 2015)
===================

* Added Remove Whole folder setting
* Resolved few issues brought by last release


0.2.0 (Jan 31 2015)
===================

* Simplify navigating up directory structure #2
* Remove deprecated code. #1


0.1.0 (Jan 18 2015)
===================

* Initial release
