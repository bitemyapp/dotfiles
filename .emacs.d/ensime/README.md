# ENSIME
the ENhanced Scala Interaction Mode for Emacs

# Links
- [ Downloads ](https://github.com/aemoncannon/ensime/downloads)
- [ Manual ](http://aemoncannon.github.com/ensime/index.html)
- [ Manual as PDF ](http://aemoncannon.github.com/ensime/manual.pdf)
- [ Discussion Group ](http://groups.google.com/group/ensime?hl=en)


## Features

- Highlight errors and warnings in your code buffers.
- Inspect the type of any expression.
- Browse packages
- Completion for variables, methods, constructors, etc.
- Incrementally search through classpath symbols
- Find all references to a symbol
- Jump to symbol definitions.
- Semantic Highlighting
- Automated Refactorings (rename, organize imports, extract method)
- Source Formatting
- AST-based selection
- Supports sbt7,10,11
- Embedded sbt shell
- REPL
- Debug support


## Demo Videos

- [Overview (a bit out of date)](http://www.youtube.com/watch?v=A2Lai8IjLoY)
- [Searching](http://www.youtube.com/watch?v=fcgnAJz98QE)
- [Debugger Support](http://www.youtube.com/watch?v=v7-G6vD42z8)
- [Import Suggestions](http://www.youtube.com/watch?v=Ynp8Df7-paw&hd=1)



## System Requirements

- Emacs 22 or later.
- Linux, Mac OSX, Windows
- Java Runtime
- A Scala 2.8.x or 2.9.x project


## Documentation

- [The ENSIME User Manual](http://aemoncannon.github.com/ensime/index.html)


## Getting Started

__1) Install scala-mode__

Although it's not required, ENSIME is designed to compliment scala-mode. scala-mode can be found in the Scala distribution under ./misc/scala-tool-support/emacs/.


__2) Install the ENSIME Server__

Download the ENSIME distribution from the github [downloads page](http://github.com/aemoncannon/ensime/downloads). Unpack the ENSIME distribution into a directory of your choosing. 

Add the following lines to your .emacs file:

    ;; load the ensime lisp code...
    (add-to-list 'load-path "ENSIME_ROOT/elisp/")
    (require 'ensime)

    ;; This step causes the ensime-mode to be started whenever
    ;; scala-mode is started for a buffer. You may have to customize this step
    ;; if you're not using the standard scala mode.
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


__3) If you are using sbt, install the ENSIME Sbt Plugin (otherwise, see the [manual](http://aemoncannon.github.com/ensime/index.html#tth_sEc3.1.2))__

Add the following lines to your project/plugins.sbt file:

    addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "VERSION")

Replace VERSION with the latest version of the plugin, available on [the plugin page](https://github.com/aemoncannon/ensime-sbt-cmd).  Then, from an sbt shell, generate your ENSIME project:
    
    ensime generate

Note: Currently, it may be necessary to first delete your project/target directories before running 'ensime generate'.

You should now have a .ensime file in the root of your project. There's no need to edit this file manually as you can now specify ENSIME settings directly from your sbt build file. Check the [manual](http://aemoncannon.github.com/ensime/index.html#tth_sEc3.1.1) for details.


__5) Start ENSIME__

From inside Emacs, execute M-x ensime


## Developer Quick Start
Note: This section is for people who want to hack on ENSIME itself.

After cloning, and before you can run ENSIME, you must create the distribution directory structure. The sbt task 'stage' will create the directory 'dist' underneath the root clone directory. Then, follow the install instructions in section 2.2 above, substituting CLONE_DIR/dist as the root of your ENSIME distribution.


The work-flow I use when hacking ENSIME:

- Edit source files
- 'sbt stage'
- Stop existing ENSIME server by killing *inferior-ensime-server* buffer
- Restart ENSIME with M-x ensime
