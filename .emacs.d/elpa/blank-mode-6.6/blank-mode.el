;;; blank-mode.el --- Minor mode to visualise blanks (SPACE, HARD SPACE and TAB).

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Vinicius Jose Latorre

;; Time-stamp: <2007/03/09 13:13:40 vinicius>
;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: data, wp
;; Version: 6.6
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package is a minor mode to visualise blanks (SPACE, HARD SPACE and
;; TAB).
;;
;; blank-mode uses two ways to visualise blanks: faces and display table.
;;
;; Faces are used to highlight the background with a color.  blank-mode uses
;; font-lock to highlight blank characters.
;;
;; Display table change the way a character is displayed, that is, it provides
;; a visual mark for characters, for example, at the end of line (?\xB6), at
;; spaces (?\xB7) and at tabs (?\xBB).
;;
;; The `blank-style' and `blank-chars' variables are used to select which way
;; should be used to visualise blanks.
;;
;; To use blank-mode, insert in your ~/.emacs:
;;
;;    (require 'blank-mode)
;;
;; Or:
;;
;;    (autoload 'blank-mode-on         "blank-mode"
;;      "Turn on blank visualisation."         t)
;;    (autoload 'blank-mode-off        "blank-mode"
;;      "Turn off blank visualisation."        t)
;;    (autoload 'blank-mode            "blank-mode"
;;      "Toggle blank visualisation."          t)
;;    (autoload 'blank-global-mode-on  "blank-mode"
;;      "Turn on blank mode in every buffer."  t)
;;    (autoload 'blank-global-mode-off "blank-mode"
;;      "Turn off blank mode in every buffer." t)
;;    (autoload 'blank-global-mode     "blank-mode"
;;      "Toggle blank mode in every buffer."   t)
;;    (autoload 'blank-mode-customize  "blank-mode"
;;      "Customize blank visualisation."       t)
;;
;; For good performance, be sure to byte-compile blank-mode.el, e.g.
;;
;;    M-x byte-compile-file <give the path to blank-mode.el when prompted>
;;
;; This will generate blank-mode.elc, which will be loaded instead of
;; blank-mode.el.
;;
;; blank-mode was tested with GNU Emacs 20.6.1, 21 and 22.
;;
;;
;; Using blank-mode
;; ----------------
;;
;; There is no problem if you mix local and global minor mode usage.
;;
;; * To customize blank-mode, type:
;;
;;    M-x blank-mode-customize RET
;;
;; * LOCAL blank-mode:
;;    + To activate blank-mode locally, type:
;;
;;         M-x blank-mode-on RET
;;
;;      Or:
;;
;;         C-u 1 M-x blank-mode RET
;;
;;    + To deactivate blank-mode locally, type:
;;
;;         M-x blank-mode-off RET
;;
;;      Or:
;;
;;         C-u 0 M-x blank-mode RET
;;
;;    + To toggle blank-mode locally, type:
;;
;;         M-x blank-mode RET
;;
;; * GLOBAL blank-mode:
;;    + To activate blank-mode globally, type:
;;
;;         M-x blank-global-mode-on RET
;;
;;      Or:
;;
;;         C-u 1 M-x blank-global-mode RET
;;
;;    + To deactivate blank-mode globally, type:
;;
;;         M-x blank-global-mode-off RET
;;
;;      Or:
;;
;;         C-u 0 M-x blank-global-mode RET
;;
;;    + To toggle blank-mode globally, type:
;;
;;         M-x blank-global-mode RET
;;
;; You can also bind `blank-mode', `blank-mode-on', `blank-mode-off',
;; `blank-global-mode', `blank-global-mode-on', `blank-global-mode-off' and
;; `blank-mode-customize' to some key, like:
;;
;;    (global-set-key "\C-c\C-a" 'blank-mode-on)
;;    (global-set-key "\C-c\C-b" 'blank-mode-off)
;;    (global-set-key "\C-c\C-l" 'blank-mode)
;;    (global-set-key "\C-c\C-d" 'blank-global-mode-on)
;;    (global-set-key "\C-c\C-e" 'blank-global-mode-off)
;;    (global-set-key "\C-c\C-g" 'blank-global-mode)
;;    (global-set-key "\C-c\C-c" 'blank-mode-customize)
;;
;;
;; Hooks
;; -----
;;
;; blank-mode has the following hook variables:
;;
;; `blank-mode-hook'
;;    It is evaluated always when blank-mode is turned on locally.
;;
;; `blank-global-mode-hook'
;;    It is evaluated always when blank-mode is turned on globally.
;;
;; `blank-load-hook'
;;    It is evaluated after blank-mode package is loaded.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of blank-mode options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `blank-verbose'		Non-nil means generate messages.
;;
;; `blank-style'		Specify the visualisation style.
;;
;; `blank-chars'		Specify which kind of blank is visualised.
;;
;; `blank-space-face'		Face used to visualise SPACE.
;;
;; `blank-hspace-face'		Face used to visualise HARD SPACE.
;;
;; `blank-tab-face'		Face used to visualise TAB.
;;
;; `blank-map-face'		Face used to visualise NEWLINE char mapping.
;;
;; `blank-trailing-face'	Face used to visualise trailing blanks.
;;
;; `blank-line-face'		Face used to visualise "long" lines.
;;
;; `blank-space-before-tab-face'	Face used to visualise space before tab.
;;
;; `blank-space-regexp'		Specify space characters regexp.
;;
;; `blank-hspace-regexp'	Specify hard space characters regexp.
;;
;; `blank-tab-regexp'		Specify tab characters regexp.
;;
;; `blank-trailing-regexp'	Specify trailing characters regexp.
;;
;; `blank-space-before-tab-regexp'	Specify space before tab regexp.
;;
;; `blank-line-length'		Specify length beyond which the line is
;;				highlighted.
;;
;; `blank-display-mappings'	Specify an alist of mappings for displaying
;;				characters.
;;
;; `blank-global-modes'		Modes for which global `blank-mode' is
;;				automagically turned on.
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;	 (setq blank-space-face 'underline)
;;
;;    This way always keep your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET blank-space-face RET underline RET
;;
;;    This way keep your settings only during the current Emacs session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *Data* group,
;;	 expand *Blank* group
;;	 and then customize blank-mode options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v blank-space-face RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; e) or invoke:
;;
;;	 M-x blank-mode-customize RET
;;
;;    and then customize blank-mode options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Antti Kaihola <antti.kaihola@linux-aktivaattori.org> for helping
;; to fix `find-file-hooks' reference.
;;
;; Thanks to Andreas Roehler <andreas.roehler@easy-emacs.de> for indicating
;; defface byte-compilation warnings.
;;
;; Thanks to TimOCallaghan (EmacsWiki) for the idea about highlight "long"
;; lines. See EightyColumnRule (EmacsWiki).
;;
;; Thanks to Yanghui Bian <yanghuibian@gmail.com> for indicating a new
;; newline character mapping.
;;
;; Thanks to Pete Forman <pete.forman@westgeo.com> for indicating
;; whitespace-mode on XEmacs.
;;
;; Thanks to:
;;    Aurelien Tisne <aurelien.tisne@free.fr>	show-whitespace-mode.el
;;    Lawrence Mitchell <wence@gmx.li>		whitespace-mode.el
;;    Miles Bader <miles@gnu.org>		visws.el
;; And to all people who contributed with them.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Variables:


;;; Interface to the command system


(defgroup blank nil
  "Visualise blanks (SPACE, HARD SPACE and TAB)."
  :link '(emacs-library-link :tag "Source Lisp File" "blank-mode.el")
  :version "20.6"
  :group 'wp
  :group 'data)


(defcustom blank-verbose t
  "*Non-nil means generate messages."
  :type 'boolean
  :version "20.6"
  :group 'blank)


(defcustom blank-style '(mark color)
  "*Specify the visualisation style.

It's a list which element value can be:

   'mark	display mappings are visualised.

   'color	faces are visualised.

Any other value is ignored.

If it's nil, don't visualise TABs, SPACEs and HARD SPACEs.

See also `blank-display-mappings' for documentation."
  :type '(repeat :tag "Style of Blank"
		 (choice :tag "Style of Blank"
			 (const :tag "Display Table" mark)
			 (const :tag "Faces" color)))
  :version "20.6"
  :group 'blank)


(defcustom blank-chars '(tabs spaces trailing lines space-before-tab)
  "*Specify which kind of blank is visualised.

It's a list which element value can be:

   'trailing		trailing blanks are visualised.

   'tabs		TABs are visualised.

   'spaces		SPACEs and HARD SPACEs are visualised.

   'lines		lines whose length is greater than `blank-line-length'
			are highlighted.

   'space-before-tab	spaces before tabs are visualised.

Any other element value is ignored.

If it's nil, don't visualise TABs, SPACEs and HARD SPACEs.

Used when `blank-style' has 'color as an element."
  :type '(repeat :tag "Kind of Blank"
		 (choice :tag "Kind of Blank"
			 (const :tag "Trailing TABs, SPACEs and HARD SPACEs"
				trailing)
			 (const :tag "SPACEs and HARD SPACEs" spaces)
			 (const :tag "TABs" tabs)
			 (const :tag "Lines" lines)
			 (const :tag "SPACEs before TABs" space-before-tab)))
  :version "20.6"
  :group 'blank)


(defcustom blank-space-face 'blank-space-face
  "*Symbol face used to visualise SPACE.

Used when `blank-style' has 'color as an element."
  :type 'face
  :version "20.6"
  :group 'blank)


(defface blank-space-face
  '((((class color) (background dark))
     :background "SteelBlue4"  :foreground "Aquamarine3")
    (((class color) (background light))
     :background "LightYellow" :foreground "Aquamarine3")
    (t (:inverse-video t)))
  "Face used to visualise SPACE."
  :version "20.6"
  :group 'blank)


(defcustom blank-hspace-face 'blank-hspace-face ; 'nobreak-space
  "*Symbol face used to visualise HARD SPACE.

Used when `blank-style' has 'color as an element."
  :type 'face
  :version "20.6"
  :group 'blank)


(defface blank-hspace-face
  '((((class color) (background dark))
     :background "CadetBlue5"    :foreground "Aquamarine3")
    (((class color) (background light))
     :background "LemonChiffon3" :foreground "Aquamarine3")
    (t (:inverse-video t)))
  "Face used to visualise HARD SPACE."
  :version "20.6"
  :group 'blank)


(defcustom blank-tab-face 'blank-tab-face
  "*Symbol face used to visualise TAB.

Used when `blank-style' has 'color as an element."
  :type 'face
  :version "20.6"
  :group 'blank)


(defface blank-tab-face
  '((((class color) (background dark))
     :background "SkyBlue4" :foreground "Aquamarine3")
    (((class color) (background light))
     :background "Beige"    :foreground "Aquamarine3")
    (t (:inverse-video t)))
  "Face used to visualise TAB."
  :version "20.6"
  :group 'blank)


(defcustom blank-map-face 'blank-map-face
  "*Symbol face used to visualise NEWLINE char mapping.  See `blank-display-mappings'.

Used when `blank-style' has 'mark as an element."
  :type 'face
  :version "20.6"
  :group 'blank)


(defface blank-map-face
  '((((class color) (background dark))
     :background "CadetBlue5" :foreground "Aquamarine3" :bold t)
    (((class color) (background light))
     :background "Linen"      :foreground "Aquamarine3" :bold t)
    (t (:bold t :underline t)))
  "Face used to visualise NEWLINE char mapping.  See `blank-display-mappings'."
  :version "20.6"
  :group 'blank)


(defcustom blank-trailing-face 'blank-trailing-face ; 'trailing-whitespace
  "*Symbol face used to visualise traling blanks.

Used when `blank-style' has 'color as an element."
  :type 'face
  :version "20.6"
  :group 'blank)


(defface blank-trailing-face
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "red1" :foreground "yellow" :bold t)))
  "Face used to visualise trailing blanks."
  :version "20.6"
  :group 'blank)


(defcustom blank-line-face 'blank-line-face
  "*Symbol face used to visualise \"long\" lines.  See `blank-line-legnth'.

Used when `blank-style' has 'color as an element."
  :type 'face
  :version "20.6"
  :group 'blank)


(defface blank-line-face
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "gray20" :foreground "violet")))
  "Face used to visualise \"long\" lines.  See `blank-line-length'."
  :version "20.6"
  :group 'blank)


(defcustom blank-space-before-tab-face 'blank-space-before-tab-face
  "*Symbol face used to visualise space before tab.

Used when `blank-style' has 'color as an element."
  :type 'face
  :version "22"
  :group 'blank)


(defface blank-space-before-tab-face
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "DarkOrange" :foreground "firebrick")))
  "Face used to visualise space before tab."
  :version "22"
  :group 'blank)


(defcustom blank-hspace-regexp "\\(\xA0+\\)"
  "*Specify hard space characters regexp.

If you're using `mule' package, it may exist other characters besides \"\\xA0\"
that it should be considered hard space.

Here are some examples:

   \"\\\\(^\\xA0+\\\\)\"		visualise only leading hard spaces.
   \"\\\\(\\xA0+$\\\\)\"		visualise only trailing hard spaces.
   \"\\\\(^\\xA0+\\\\|\\xA0+$\\\\)\"	visualise leading and/or trailing hard spaces.
   \"\\t\\\\(\\xA0+\\\\)\\t\"		visualise only hard spaces between tabs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `blank-style' has 'color as an element, and `blank-chars' has
'spaces as an element."
  :type '(regexp :tag "Hard Space Chars")
  :version "20.6"
  :group 'blank)


(defcustom blank-space-regexp "\\( +\\)"
  "*Specify space characters regexp.

If you're using `mule' package, it may exist other characters besides \" \"
that it should be considered space.

Here are some examples:

   \"\\\\(^ +\\\\)\"		visualise only leading spaces.
   \"\\\\( +$\\\\)\"		visualise only trailing spaces.
   \"\\\\(^ +\\\\| +$\\\\)\"	visualise leading and/or trailing spaces.
   \"\\t\\\\( +\\\\)\\t\"	visualise only spaces between tabs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `blank-style' has 'color as an element, and `blank-chars' has
'spaces as an element."
  :type '(regexp :tag "Space Chars")
  :version "20.6"
  :group 'blank)


(defcustom blank-tab-regexp "\\(\t+\\)"
  "*Specify tab characters regexp.

If you're using `mule' package, it may exist other characters besides \"\\t\"
that it should be considered tab.

Here are some examples:

   \"\\\\(^\\t+\\\\)\"		visualise only leading tabs.
   \"\\\\(\\t+$\\\\)\"		visualise only trailing tabs.
   \"\\\\(^\\t+\\\\|\\t+$\\\\)\"	visualise leading and/or trailing tabs.
   \" \\\\(\\t+\\\\) \"	visualise only tabs between spaces.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `blank-style' has 'color as an element, and `blank-chars' has 'tabs
as an element."
  :type '(regexp :tag "Tab Chars")
  :version "20.6"
  :group 'blank)


(defcustom blank-trailing-regexp "\t\\| \\|\xA0"
  "*Specify trailing characters regexp.

If you're using `mule' package, it may exist other characters besides \" \",
\"\\t\" or \"\\xA0\" that it should be considered blank.

NOTE: DOES NOT enclose by \\\\( and \\\\) the elements to highlight.
      `blank-mode' surrounds this regexp by \"\\\\(\\\\(\" and
      \"\\\\)+\\\\)$\".

Used when `blank-style' has 'color as an element, and `blank-chars' has
'trailing as an element."
  :type '(regexp :tag "Trailing Chars")
  :version "20.6"
  :group 'blank)


(defcustom blank-space-before-tab-regexp "\\( +\\)\t"
  "*Specify spaces before tabs regexp.

If you're using `mule' package, it may exist other characters besides \" \",
\"\\t\" or \"\\xA0\" that it should be considered blank.

Used when `blank-style' has 'color as an element, and `blank-chars' has
'space-before-tab as an element."
  :type '(regexp :tag "Space Before Tab")
  :version "22"
  :group 'blank)


(defcustom blank-line-length 80
  "*Specify length beyond which the line is highlighted.

Used when `blank-style' has 'color as an element, and `blank-chars' has 'lines
as an element."
  :type '(integer :tag "Line Length")
  :version "20.6"
  :group 'blank)


;; Hacked from `visible-whitespace-mappings' in visws.el
(defcustom blank-display-mappings
  '((?\    [?\xB7]     [?.])		; space
    (?\xA0 [?\xA4]     [?_])		; hard space
    (?\n   [?\xB6 ?\n] [?$ ?\n])	; end-of-line
    ;; WARNING: the mapping below has a problem.
    ;; When a tab occupies exactly one column, it will display the character
    ;; ?\xBB at that column followed by a tab which goes to the next tab
    ;; column.
    ;; If this is a problem for you, please, comment the line below.
    (?\t   [?\xBB ?\t] [?\\ ?\t])	; tab
    )
  "*Specify an alist of mappings for displaying characters.

Each element has the following form:

   (CHAR VECTOR...)

Where:

CHAR	is the character to be mapped.

VECTOR	is a vector of characters to be displayed in place of CHAR.
	The first display vector that can be displayed is used; if no display
	vector for a mapping can be displayed, then that character is
	displayed unmodified.

The NEWLINE character is displayed using the face given by `blank-map-face'
variable.

Used when `blank-style' has 'mark as an element."
  :type '(repeat
	  (list :tag "Character Mapping"
		(character :tag "Char")
		(repeat :inline t :tag "Vector List"
			(vector :tag ""
				(repeat :inline t :tag "Vector Characters"
					(character :tag "Char"))))))
  :version "20.6"
  :group 'blank)


(defcustom blank-global-modes t
  "*Modes for which global `blank-mode' is automagically turned on.

Global `blank-mode' is controlled by the command `blank-global-mode'.

If nil, means no modes have `blank-mode' automatically turned on.
If t, all modes that support `blank-mode' have it automatically turned on.
If a list, it should be a list of `major-mode' symbol names for which
`blank-mode' should be automatically turned on.  The sense of the list is
negated if it begins with `not'.  For example:

   (c-mode c++-mode)

means that `blank-mode' is turned on for buffers in C and C++ modes only."
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (set :menu-tag "mode specific" :tag "modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t
			      (symbol :tag "mode"))))
  :version "20.6"
  :group 'blank)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros


(defmacro blank-message (&rest body)
  `(and blank-verbose (interactive-p)
	(message ,@body)))


(defmacro blank-minor-mode (arg mode on off)
  `(progn
     (if (if arg
	     (> (prefix-numeric-value arg) 0)
	   (not ,mode))
	 (,on)
       (,off))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User commands - Customization


;;;###autoload
(defun blank-mode-customize ()
  "Customize blank-mode options."
  (interactive)
  (customize-group 'blank))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User commands - Local mode


(defvar blank-mode nil
  "Non-nil means blank-mode local minor mode is enabled (bl on modeline).")
(make-variable-buffer-local 'blank-mode)


;;;###autoload
(defun blank-mode (&optional arg)
  "Toggle blank minor mode visualisation (bl on modeline).

If ARG is null, toggle blank visualisation.
If ARG is a number and is greater than zero, turn on visualisation; otherwise,
turn off visualisation."
  (interactive "P")
  (blank-minor-mode arg blank-mode
		    blank-mode-on blank-mode-off)
  (blank-message "Blank Mode is now %s." (if blank-mode "on" "off")))


;;;###autoload
(defun blank-mode-on ()
  "Turn on blank minor mode visualisation (bl on modeline)."
  (interactive)
  (or (and (boundp 'blank-mode) blank-mode)
      (when blank-style
	(setq blank-mode t)
	(blank-turn-on)
	(run-hooks 'blank-mode-hook)
	(blank-message "Blank Mode is now on."))))


;;;###autoload
(defun blank-mode-off ()
  "Turn off blank minor mode visualisation (bl on modeline)."
  (interactive)
  (and (boundp 'blank-mode) blank-mode
       (progn
	 (setq blank-mode nil)
	 (blank-turn-off)
	 (blank-message "Blank Mode is now off."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User commands - Global mode


(defvar blank-global-mode nil
  "Non-nil means blank-mode global minor mode is enabled (BL on modeline).")


;;;###autoload
(defun blank-global-mode (&optional arg)
  "Toggle blank global minor mode visualisation (BL on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system."
  (interactive "P")
  (blank-minor-mode arg blank-global-mode
		    blank-global-mode-on blank-global-mode-off)
  (blank-message "Blank Global Mode is %s" (if blank-global-mode "on" "off")))


;;;###autoload
(defun blank-global-mode-on ()
  "Turn on blank global minor mode visualisation (BL on modeline)."
  (interactive)
  (or (and (boundp 'blank-global-mode) blank-global-mode)
      (progn
	(save-excursion
	  (let ((buffers (buffer-list)))
	    (setq blank-global-mode t)
	    (if (boundp 'find-file-hook)
		(add-hook 'find-file-hook 'blank-turn-on-if-enabled t)
	      (add-hook 'find-file-hooks 'blank-turn-on-if-enabled t))
	    (while buffers		; adjust all local mode
	      (set-buffer (car buffers))
	      (unless blank-mode
		(blank-turn-on-if-enabled))
	      (setq buffers (cdr buffers)))))
	(run-hooks 'blank-global-mode-hook)
	(blank-message "Blank Global Mode is on"))))


;;;###autoload
(defun blank-global-mode-off ()
  "Turn off blank global minor mode visualisation (BL on modeline)."
  (interactive)
  (and (boundp 'blank-global-mode) blank-global-mode
       (progn
	 (save-excursion
	   (let ((buffers (buffer-list)))
	     (setq blank-global-mode nil)
	    (if (boundp 'find-file-hook)
		(remove-hook 'find-file-hook 'blank-turn-on-if-enabled)
	      (remove-hook 'find-file-hooks 'blank-turn-on-if-enabled))
	     (while buffers		; adjust all local mode
	       (set-buffer (car buffers))
	       (unless blank-mode
		 (blank-turn-off))
	       (setq buffers (cdr buffers)))))
	 (blank-message "Blank Global Mode is off"))))


(defun blank-turn-on-if-enabled ()
  (when (cond
	 ((eq blank-global-modes t))
	 ((listp blank-global-modes)
	  (if (eq (car-safe blank-global-modes) 'not)
	      (not (memq major-mode (cdr blank-global-modes)))
	    (memq major-mode blank-global-modes)))
	 (t nil))
    (let (inhibit-quit)
      ;; Don't turn on blank mode if...
      (or
       ;; ...we don't have a display (we're running a batch job)
       noninteractive
       ;; ...or if the buffer is invisible (the name starts with a space)
       (eq (aref (buffer-name) 0) ?\ )
       ;; ...or if the buffer is temporary (the name starts with *)
       (and (eq (aref (buffer-name) 0) ?*)
	    ;; except the scratch buffer.
	    (not (string= (buffer-name) "*scratch*")))
       ;; Otherwise, turn on blank mode.
       (blank-turn-on)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions


(defvar blank-font-lock nil
  "Used to remember whether a buffer initially had font lock on or not.")
(make-variable-buffer-local 'blank-font-lock)

(defvar blank-font-lock-keywords nil
  "Used to save locally `font-lock-keywords' value.")
(make-variable-buffer-local 'blank-font-lock-keywords)

(defvar blank-active-chars nil
  "Used to save locally `blank-chars' value.")
(make-variable-buffer-local 'blank-active-chars)

(defvar blank-active-style nil
  "Used to save locally `blank-style' value.")
(make-variable-buffer-local 'blank-active-style)


(defun blank-turn-on ()
  "Turn on blank visualisation."
  (setq blank-active-style blank-style)
  (and (memq 'color blank-active-style)
       (blank-color-on))
  (and (memq 'mark  blank-active-style)
       (blank-display-char-on)))


(defun blank-turn-off ()
  "Turn off blank visualisation."
  (and (memq 'color blank-active-style)
       (blank-color-off))
  (and (memq 'mark  blank-active-style)
       (blank-display-char-off)))


(defun blank-color-on ()
  "Turn on color visualisation."
  (setq blank-active-chars blank-chars)
  (when blank-active-chars
    (unless blank-font-lock
      (setq blank-font-lock t
	    blank-font-lock-keywords (copy-sequence font-lock-keywords)))
    (and (memq 'spaces blank-active-chars)
	 (font-lock-add-keywords
	  nil
	  (list
	   ;; Show spaces
	   (list blank-space-regexp  1 blank-space-face  t)
	   ;; Show hard spaces
	   (list blank-hspace-regexp 1 blank-hspace-face t))
	  t))
    (and (memq 'tabs blank-active-chars)
	 (font-lock-add-keywords
	  nil
	  (list
	   ;; Show tabs
	   (list blank-tab-regexp 1 blank-tab-face t))
	  t))
    (and (memq 'trailing blank-active-chars)
	 (font-lock-add-keywords
	  nil
	  (list
	   ;; Show trailing blanks
	   (list (concat "\\(\\(" blank-trailing-regexp "\\)+\\)$")
		 1 blank-trailing-face t))
	  t))
    (and (memq 'lines blank-active-chars)
	 (font-lock-add-keywords
	  nil
	  (list
	   ;; Show "long" lines
	   (list
	    (concat "^\\(.\\{" (int-to-string blank-line-length) ",\\}\\)$")
	    1 blank-line-face t))
	  t))
    (and (memq 'space-before-tab blank-active-chars)
	 (font-lock-add-keywords
	  nil
	  (list
	   ;; Show spaces before tabs
	   (list blank-space-before-tab-regexp 1
		 blank-space-before-tab-face t))
	  t))
  (font-lock-fontify-buffer)))


(defun blank-color-off ()
  "Turn off color visualisation."
  (when blank-active-chars
    (when blank-font-lock
      (setq blank-font-lock nil
	    font-lock-keywords blank-font-lock-keywords))
    (font-lock-fontify-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hacked from visws.el


(defvar blank-display-table nil
  "Used to save a local display table.")
(make-variable-buffer-local 'blank-display-table)

(defvar blank-display-table-was-local nil
  "Used to remember whether a buffer initially had a local display table or not.")
(make-variable-buffer-local 'blank-display-table-was-local)


(defun blank-legal-display-vector-p (vec)
  "Return true if every character in the display vector VEC can be displayed."
  (let ((i (length vec)))
    (when (> i 0)
      ;; This check should be improved!!!
      (while (and (>= (setq i (1- i)) 0)
		  (or (< (aref vec i) 256)
		      (char-valid-p (aref vec i)))))
      (< i 0))))


(defun blank-display-char-on ()
  "Turn on character display mapping."
  (and blank-display-mappings
       (let ((face-bits (ash (face-id blank-map-face) 19))
	     (map-list blank-display-mappings)
	     entry vecs len vec i)
	 ;; Remember whether a buffer has a local display table.
	 (unless blank-display-table-was-local
	   (setq blank-display-table-was-local t
		 blank-display-table (copy-sequence buffer-display-table)))
	 (or buffer-display-table
	     (setq buffer-display-table (make-display-table)))
	 (while map-list
	   (setq entry    (car map-list)
		 vecs     (cdr entry)
		 map-list (cdr map-list))
	   ;; Get a displayable mapping.
	   (while (and vecs (not (blank-legal-display-vector-p (car vecs))))
	     (setq vecs (cdr vecs)))
	   ;; Display a valid mapping.
	   (when vecs
	     (setq vec (copy-sequence (car vecs)))
	     ;; Only insert face bits on NEWLINE char mapping to avoid
	     ;; obstruction of other faces like TABs and (HARD) SPACEs faces,
	     ;; font-lock faces, etc.
	     (when (eq (car entry) ?\n)
	       (setq len (length (car vecs))
		     i -1)
	       (while (< (setq i (1+ i)) len)
		 (or (eq (aref vec i) ?\n)
		     (aset vec i (logior (aref vec i) face-bits)))))
	     (aset buffer-display-table (car entry) vec))))))


(defun blank-display-char-off ()
  "Turn off character display mapping."
  (and blank-display-mappings
       blank-display-table-was-local
       (setq blank-display-table-was-local nil
	     buffer-display-table          blank-display-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'minor-mode-alist '(blank-mode        " bl"))
(add-to-list 'minor-mode-alist '(blank-global-mode " BL"))


(provide 'blank-mode)


(run-hooks 'blank-load-hook)


;;; blank-mode.el ends here