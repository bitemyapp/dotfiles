## 1.16.0
* Change onMouseHoverShow default to 'Type and Info'
* More type/info tooltip options
* Add sig-fill command
* Fix caps display in error messages
* Kill ghc-modi on InteractiveActionTimeout
* Add special handler for interactive action timeout

## 1.15.3
* Make interactiveActionTimeout actually work

## 1.15.2
* Configurable timeouts

## 1.15.1
* Fix Promise.resolve

## 1.15.0
* Add .haskell-ghc-mod.json to readme
* Add message on .haskell-ghc-mod.json parse error
* Handle JSON.parse errors
* .haskell-ghc-mod.json

## 1.14.7
* Add contributors to README
* Merge pull request #162 from DeathByTape/master
* Set maxBuffer to 'Infinity'.

## 1.14.6
* Use absolute paths in go-to-declaration

## 1.14.5
* Add option for low-memory systems

## 1.14.4
* Avoid reporting Atom bugs

## 1.14.3
* Dir/Path cleanup in initBackend
* Use _.extend for extending process options
* Log options as object
* Remove unused child_process
* Cleanup getVersion, checkComp
* A bit more GhcModiProcessReal-related cleanup
* Clean up GhcModiProcessReal
* Log ghc-modi spawn options as object
* Stringify log messages
* Change debug text
* Refactor execFile into execPromise
* Don't use BufferedProcess because WEIRDNESS on Win
* Print stderr/stdout when checking ghc version
* Print stdout on stack path error
* getProcessOptions once in initBackend
* Always show stderr on stack path

## 1.14.2
* Use 'n' as EOL in Atom
* Merge branch 'upi-refactor'
* UPI refactor to sep. module

## 1.14.1
* Don't throw when checking for compiler
* Fix message on path

## 1.14.0
* Delayed process initialization; per-dir backend
* AHS bump
* Atom-haskell-utils bump

## 1.13.2
* Use install-root for stack sandbox detection

## 1.13.1
* Fixup for sporadic spaces in hlint parse error

## 1.13.0
* Check compiler version
* Fix pesky timeout problem
* Use promise in getProcessOptions
* Remove redundant condition
* Removed support for older ghc-mod versions

## 1.12.6
* Attempt at fixing stack sandbox resolution on Win

## 1.12.5
* Oops

## 1.12.4
* Fix #143

## 1.12.3
* More debug output with sandboxes
* Update type signature/parent separator

## 1.12.2
* Handle parse errors in parseHsModuleImports

## 1.12.1
* Make imports parsing asynchronous

## 1.12.0
* Option to disable ghc-mod for some projects

## 1.11.5
* Add timeout to `stack path` call

## 1.11.4
* Collect different capitalizations of PATH on win32

## 1.11.3
* Don't run commands on buffer without URI

## 1.11.2
* Fix #125

## 1.11.1
* Make case-split non-interactive
* Extend 'experimental' setting description
* Experimental features toggle
* Ghc-mod 5.6 initial Type constraints Browse parents
* Handle (..) imports
* True module import parsing

## 1.11.0
* Added case split
* Warning should be shown as warnings

## 1.10.3
* Filter out empty lines etc

## 1.10.2
* Show dummy messages as notifications

## 1.10.1
* Removed debug log

## 1.10.0
* User-supplied hlint options
* Typo

## 1.9.6
* Linter support docs
* Update supported versions

## 1.9.5
* Handle ghc-mod 4.1.2 version

## 1.9.4
* Fix an error where tabUnshift would fail

## 1.9.3
* Create new instance for tab[Un]Shift
* Improved insert-type
* Insert inline type signatures
* Filter empty type ranges out of ghc-mod output
* Properly insert type into lhs
* Fix insert-type for operators

## 1.9.2
* Handling no value in onShouldShowTooltip delegated to ide-haskell

## 1.9.1
* Fix atom-haskell/ide-haskell#145

## 1.9.0
* Show type on selection (disabled by default)

## 1.8.0
* Some black magic to work around tabs
* Somewhat better linter integration
* Do away with ::shadow selector

## 1.7.0
* Output message highlighter
* Tooltip highlighting
* Add go-to-declaration to keymap example (pull request #116 from @PetrGlad)

## 1.6.6
* Fix #106

## 1.6.5
* Partial fix for explicit type w/ctors import
* Fix ide-haskell/issues/136

## 1.6.4
* Fix getSymbolAtPoint handling of symbol at start of line (pull request #97 from @jacksonja)
* Use Util.isDirectory instead of FS.statSync

## 1.6.3
* atom-haskell-utils

## 1.6.2
* atom-haskell-utils version bump

## 1.6.1
* Try to fix #95
* Remove debug print

## 1.6.0
* Actually parse cabal.sanbox.config, stack bin-path
* Update README.md with link to stack info

## 1.5.9
* Fix #94

## 1.5.8
* Hopefully fix tokenization problems

## 1.5.7
* Replace anything resembling newline in int. proc.

## 1.5.6
* Remove empty arguments altogether

## 1.5.5
* Don't quote empty strings

## 1.5.4
* Prefer relative paths with ghc-modi<5.5.0.0

## 1.5.3
* BUGFIX: atom-haskell-utils bump

## 1.5.2
* Fix isDirectory bug

## 1.5.1
* Update and enable quoteArgs

## 1.5.0
* Use library-supplied getRootDir/Fallback
* Lint lhs files
* Fix TypeError
* Add package keywords
* Organize source files
* Activate on language-haskell:grammar-used
* Async version
* Defer `require`s until needed

## 1.4.1
* FIX: Fat arrow
* Include ghc-mod version into error report

## 1.4.0
* ghc-mod error reporting
* Don't enable quoteArgs yet

## 1.3.1
* Better error reporting

## 1.3.0
* Go-to-decl context menu item
* Go-to-decl initial
* Move EOT to Util
* Cleaner debug vers,caps

## 1.2.8
* Fix spawn leak
* Fix error reporting

## 1.2.7
* Better error reporting on failing to run ghc-mod

## 1.2.6
* Destroy moduleInfo on process destroy (#75)

## 1.2.5
* Fix InteractiveProcess::onExit

## 1.2.4
* Buffer ghc-modi warnings
* Show ghc-mod warnings in console
* Check ghc-mod version; capability resolver
* Set encoding in getProcessOptions()
* Move ghc-modi interaction to InteractiveProcess
* Handle ghc-mod errors in one place
* Refactor GhcModiProcess::run into base class

## 1.2.3
* Use interactive mode for 'find'
* Insert imports when no other imports present (#68)
* Fix insert type with operators (#73)

## 1.2.2
* Clear interactive command timeout on exit
* Less verbose interactive debug messages
* Fix more of ghc-mod capitalization

## 1.2.1
* Interactive commands buffering and timeout
* `ghc-mod` capitalization
* Allow for parallel check/lint
* backend-idle signal parallel to result

## 1.2.0
* Fix backend-idle notification
* Configurable max browse processes
* Fix ghc-modi reply parsing bug

## 1.1.9
* Fix ghc-modi reply parsing bug

## 1.1.8
* Use promise-queues for more robust command queues
* Bounded parallel auto-completion initialization
* Fix backend stop command

## 1.1.7
* Queue interactive commands

## 1.1.6
* Merge branch 'master' of github.com:atom-haskell/haskell-ghc-mod
* Work around ghc-mod stack root bug
* Merge pull request #69 from Roughsketch/master
* Fixed typo

## 1.1.5
* More clear description of additionalPathDirs opt.

## 1.1.4
* Check for existence of sandbox bindir

## 1.1.3
* Break endless cycle in getRootDir on undefined dir
* Use getRootDir with fallback

## 1.1.2
* More careful ghc-modi error handling

## 1.1.1
* Fix #61

## 1.1.0
* Simplify check/lint subscription code
* doCheckAndLint
* On-change check/lint
* Use promises for check/lint
* Use promises for sequential ghc-modi communication

## 1.0.0
* Migration to ide-haskell UPI interface

## 0.9.14
* Don't check/lint empty buffers

## 0.9.13
* Change end-of-input sequence from `\EOT\n` to `\n\EOT\n`

## 0.9.12
* Update Linter styles

## 0.9.11
* Fix occasional EACCESS

## 0.9.10
* Only pause ghc-modi stdout on command, and resume after

## 0.9.9
* Use platform-specific pathsep

## 0.9.8
* Use platform-specific EOL whenever possible

## 0.9.7
* Use `ghc-mod legacy-interactive` with ghc-mod-5.4.0.0 and up

## 0.9.6
* Configurable sync launch timeout
* Fixed confusing instructions about GHC Modi disabling (@wolftune)

## 0.9.5
* Fix detail view on spawn error
* Update supported versions

## 0.9.4
* Remove redundant debug output

## 0.9.3
* Use buffer for ghc-modi responses (#51)

## 0.9.2
* Fix ghc-mod revision detection
* Cache buffer-rootDir mappings

## 0.9.1
* Initial support for ghc-mod 5.4.0.0 (run ghc-mod in project root dir as reported by `ghc-mod root`)

## 0.9.0
* Allow for hole completion refinement

## 0.8.14
* Revert tab tweaks

## 0.8.13
* Check point validity in pointWithTabs functions

## 0.8.12
* Fix pointWithTabs

## 0.8.11
* Tweak ranges for tabs
* Removed redirect-map ghc-mod version notification

## 0.8.10
* Fix deactivation error

## 0.8.9
* Fix #41 (undefined path in `new Directory` error on Windows)

## 0.8.8
* Fix ghc-mod args in error reporting

## 0.8.7
* Hotfix #37 (res can be undefined)

## 0.8.6
* Attempt at fixing #31 (Atom returns `atom://config` as project directory)

## 0.8.5
* Attempt at fixing #36 (rely on garbage collector more in `getBufferInfo`)

## 0.8.4
* atom.project.getDirectories() can return file in some cases (#32)

## 0.8.3
* Cleaner destruction of ModuleInfo/BufferInfo. Can help with heisenbugs.

## 0.8.2
* More verbose debugging output

## 0.8.1
* Update redirect commands to correspond with upstream updates

## 0.8.0
* Optional support of AtomLinter for displaying messages (doesn't need ide-haskell installed)
* Cache backends
* Drop support for older provider versions
* Provider versions bumped to 1.0.0

## 0.7.12
* Honor additional PATH in runLang and runFlag

## 0.7.11
* Add fallback child_process.execFile for ghc-mod commands

## 0.7.10
* Initial support for literate Haskell

## 0.7.9
* Fix some problems with standalone files (#29)

## 0.7.8
* Wasn't possible to disable startup warning on ide-haskell not installed.

## 0.7.7
* Separate completion queues for different tasks. Should result in better responsiveness on start.

## 0.7.6
* Cleanup
* More debugging output

## 0.7.5
* Rough fix for #24

## 0.7.4
* Try to fix path capitalization issues on Windows

## 0.7.3
* Relativize path on check in case ghc-mod returns full path for some reason

## 0.7.2
* Avoid two consecutive separators in PATH

## 0.7.1
* `additionalPathDirectories` configuration option

## 0.7.0
* Fixed typo in completion-backend.coffee (@crazymykl)
* Initial support for input file redirection (WIP on ghc-mod master)
* Display non-fatal ghc-mod errors in outputView (was: print to console)
* Don't pass buffer text as tempfile if it's saved
* Always get abs. URI for check results
* General cleanup, which hopefully helps with #20, #21
* Removed frontend

## 0.6.6
* Fix bug in getCompletionsForSymbolInModule

## 0.6.5
* Fix typo (`rootDi` instead of `rootDir`)
* After 60 minutes of inactivity, kill ghc-modi

## 0.6.4
* Return at least something from getRootDir (attempt at fixing #17, #18)

## 0.6.3
* haskell-ide-backend 0.1.2 - adds getModulesExportingSymbolAt function
* Add `'` to word search regular expressions
* Properly dispose of emitters
* Completion-backend internal revamp

## 0.6.2
* Only search for symbol in current line
* Strip newlines from ghc-modi commands
* Fat arrow requied in ghc-modi proc.onExit (#14)

## 0.6.1
  * haskell-ide-backend 0.1.1

    Returns {type/info}=undefined if no type/info found

## 0.6.0
  * Backend services docs and fixes
  * Near-final version of API
  * Frontend removal notice

## 0.5.1
  * Deprecation fix

## 0.5.0
  * Ide-haskell compatibility (disable editor control etc)
  * Queue commands
  * Use BufferedProcess
  * Haskell-ghc-mod service deprecated, new services implemented

## 0.4.3
  * code cleanup
  * add filename and path to doCheck callback
  * Filter current file in doCheck
  * Add tempfile path as getInfo callback parameter
  * Replace tempfile path with actual path in getInfo
  * Fix newlines in ghc-mod check
  * README update

## 0.4.2
  * BUGFIX: Fat arrow in main module
  * Force ghc-mod for file check
  * Remove checkOnEdit option
  * Don't check file on open

## 0.4.1
  * Fix getRootPath deprecation

## 0.4.0
  * Migrate to new json-based service provider
  * Bump atom version

## 0.3.8
  * Check on open only if ghc-modi enabled

## 0.3.7
  * Fix windows path error

## 0.3.6
  * Fix gutter warning tooltips

## 0.3.5
  * Add option to disable ghc-modi (turns out there are a couple unresolved bugs in it)

## 0.3.4
  * Bugfixes

## 0.3.3
  * Preliminary support for cabal sandboxes

## 0.3.2
  * Better error reporting in case of ghc-modi failure

## 0.3.1
  * Fixed some deprecations

## 0.3.0
  * Service-hub API
  * Fix more obscure deprecations
  * Don't set globals
  * Persistent gutter tootlips

## 0.2.1
  * Use theme colors for decorations
  * Use different colors for warnings and errors

## 0.2.0
  * Use temp-files to feed buffer to ghc-mod directly
  * Add option to check file while editing (disabled by default)

## 0.1.5
  * Add options for check on save and ghc-mod path

## 0.1.4
  * BUGFIX: Sometimes, inserting type destroyed main cursor. Avoid that.

## 0.1.3
  * Experimental feature: insert type into editor
  * Highlight expression, type of which is showing

## 0.1.2
  * Use observeTextEditors instead of eachEditor

## 0.1.1
  * Stop ghc-modi if no Haskell files are open

## 0.1.0 - First Release
  * Basic functionality
