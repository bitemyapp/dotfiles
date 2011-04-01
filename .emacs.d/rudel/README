* INTRODUCTION

  Rudel is a collaborative editing environment for GNU Emacs. Its
  purpose is to share buffers with other users in order to edit the
  contents of those buffers collaboratively. Rudel supports multiple
  backends to enable communication with other collaborative editors
  using different protocols, though currently Obby (for use with the
  Gobby editor) is the only fully-functional one.

  Since Rudel is not an application, but an extension to Emacs, it is
  not started and used like most applications (not even Emacs
  applications like Gnus). Rudel mostly works in the background to
  change the behavior of the set of Emacs buffers for which it has
  been activated.

  The user interface consists of a set of key bindings, a menu entry
  and some visual status indicators, which are added to the text,
  header line and/or mode line of buffers for which Rudel has been
  activated.

* GETTING STARTED

  Assuming Rudel has already been installed (see file:INSTALL) and
  auto loading has been set up, a global Rudel mode can be enabled as
  follows:

  : M-x global-rudel-minor-mode

  This will enable key bindings to list available backends, join, and
  host Rudel sessions (see below). To enable this mode permanently,
  the following fragment can be added to the Emacs initialization
  file (usually file:~/.emacs):

  #+BEGIN_SRC emacs-lisp
  (global-rudel-minor-mode 1)
  #+END_SRC

** LISTING BACKENDS

   : M-x rudel-backend-dump 

   This command is available through the Rudel drop down menu.
   However, typically this is only used for debugging, since backends
   are chosen automatically when joining or hosting a session.

** JOINING A SESSION

   : M-x rudel-join-session [ C-c c j ]

   Depending on the installed Rudel backends, system environment and
   configuration, a number of questions will be asked, followed by an
   attempt to join session described by your answers.

   A typical example of the questions asked when joining a session may
   look like this:

   #+BEGIN_EXAMPLE
   Server: localhost RET
   Port (default 6522): RET
   Username: jan RET
   Color: light sky blue RET
   Use Encryption (y or n): n RET
   Global Password: RET
   User Password: RET
   #+END_EXAMPLE

   *IMPORTANT*: For sessions using the obby backend (like in the
   example above), the following restriction has to be taken into
   account:

   + When the server is Rudel inside an Emacs process:
     Encryption cannot be used currently in this case. Consequently
     the answer to the `Use Encryption (y or n):' prompt above has to
     be `n RET'.
   + When the server is a Gobby process:
     Gobby only supports encrypted connections. So the answer has to
     be `y RET' is this case.

   It is possible to configure frequently used sessions using the
   customization options `rudel-configured-sessions'. When one or more
   sessions are configured, `rudel-join-session' will provide choices
   like "my-configured-session", ... and "ask-protocol". Selecting
   "ask-protocol" invokes the behavior described above. Selecting one
   of the configured sessions connects to that session without asking
   for all the data.

*** SAVING AND REUSING CONFIGURED SESSIONS

   Each session is described as a plist (a list of keys and values
   see Info node `(elisp)Property Lists'). Keys are specified using
   keywords and look like this: :host, :username, :color. Values are
   mostly strings, but symbols and numbers are possible as well.

   The following keys are required for any session:

   * :name              (string)
   * :transport-backend (string or symbol)
   * :protocol-backend  (string or symbol)

   Other keys are optional and depend on the selected
   backend. Required keys for which no value is specified will be
   prompted for when selecting the session. The values of the :name
   properties have to be distinct for all configured sessions.

   Additional keys required by most backends:

   * :host     (string)
   * :port     (number)
   * :username (string)
   * :color    (string)

   Here is a complete example of customized values for the obby
   backend:

   * :name              "sonian"
   * :transport-backend tcp
   * :protocol-backend  obby
   * :host              "sobby"
   * :port              6522
   * :encryption        t
   * :username          "phil"
   * :color             "white"
   * :global-password   ""         (this means "no password")
   * :user-password     ""

   The programmatic equivalent looks like this:

   #+BEGIN_SRC emacs-lisp
   (add-to-list
    'rudel-configured-sessions
    (list :name              "myserver"
          :protocol-backend  'tcp
          :transport-backend 'obby
          :host              "my.sobby-server.net"
          :username          user-login-name
          ;; Use M-x list-colors-display to see color choices.
          :color             "white"
          :encryption        t
          :port              6522
          ;; empty string means no password
          :global-password   ""
          :user-password     ""))
   #+END_SRC

** WORKING WITHIN A CONNECTED SESSION

*** SHARING BUFFERS

    : M-x rudel-publish-buffer [ C-c c p ]

    Make a buffer available for subscription to peers in a
    collaborative editing session.

    : M-x rudel-subscribe [ C-c c s ]

    Subscribe to a document offered by a peer in a collaborative
    editing session. Invoking this command will prompt for the name
    of the document to subscribe to; TAB will show a list of
    available documents.

    : M-x rudel-unsubscribe [ C-c c u ]

    Detaches the current buffer from the collaborative editing
    session. The most recent version of the content will remain in
    the buffer but not be affected by future changes from other
    peers.

*** CHANGING COLORS

    : M-x rudel-change-color [ C-c c c ]

    Change the color associated with your edits.

*** LEAVING A SESSION

    : M-x rudel-end-session [ C-c c e ]

    Exit the current collaborative editing session.

** HOSTING A SESSION

   : M-x rudel-host-session [ C-c c h ]

   Note that the session starts out without any participating users
   (This is sometimes referred to as being a dedicated server). If you
   want to participate in the session you host, you have to join it as
   described above.

* EXTRA MINOR MODES

** [Global] Header Subscriptions minor mode

   : rudel-header-subscriptions-minor-mode
   
   Displays subscribed users and information concerning their
   respective status in the header line of a buffer.

   : global-rudel-header-subscriptions-mode

   Enables or disables rudel-header-subscriptions-minor-mode
   automatically for all buffers.

** [Global] Mode line publish state minor mode

   : rudel-mode-line-publish-state-minor-mode

   Displays an indicator of the publication status of a buffer in its
   mode line. This indicator is similar to the read-only
   vs. read-write and the remote vs. local indicators. Publication is
   indicated by a the letter P. Buffers that are not published have
   an - indicator.

   : global-rudel-mode-line-publish-state-mode

   Enables or disables rudel-mode-line-publish-state-minor-mode
   automatically for all buffers.

* REPORTING BUGS AND GETTING HELP

** EmacsWiki

   The EmacsWiki page about Rudel has
   [[http://www.emacswiki.org/emacs/Rudel#toc8][a section for feedback and questions]].
   Feel free leave feedback or ask questions there. If a reply does
   not occur promptly, try writing to the mailing list of visiting the
   IRC channel.

** Issue Tracker

   Bugs can be reported using the issue tracker on the sourceforge
   project page: http://sourceforge.net/tracker/?group_id=249139

** Mailing List

   Another possibility for getting help and reporting problems is
   writing to the Rudel mailing list:
   http://sourceforge.net/mail/?group_id=249139

** IRC Channel

   Rudel users and developers can also be reached on the #rudel IRC
   channel on the freenode network: irc:/irc.freenode.net:6667/#rudel

* KNOWN ISSUES

  + Publishing eshell buffers will cause your session to be
    disconnected since eshell disables the hooks that Rudel uses to
    catch changes to the buffer. As a workaround, you can use M-x
    ansi-term or another terminal emulator.

* LICENSE

  This file is part of Rudel.

  Rudel is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Rudel is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
  License for more details.

  You should have received a copy of the GNU General Public License
  along with Rudel.  If not, see <http://www.gnu.org/licenses/>.

#+TITLE:   Rudel README
#+AUTHOR:  Jan Moringen
#+STARTUP: showeverything

# Local variables:
# mode: org
# end:
