;;; j-mode.el --- Major mode for editing J programs

;; Copyright (C) 2003, 2004 Alexander Schmolck

;; Author:        Alexander Schmolck
;; Created:       2003-12-15
;; Last modified: $Date: 2004/12/06 02:47:09 $
;; Keywords:      J, languages
;; URL:           <http://www.sourceforge.net/j-mode/>
;; License:       GPL (see the file COPYING that came with your emacs)

(defconst j-mode-version "$Revision: 1.7 $"
  "`j-mode' version number.")

;;; Commentary:
;;    __: http://www.gnu.org/software/emacs/manual/html_node/Init-File.html
;; 
;; ======
;; j-mode
;; ======
;; 
;; :author: Alexander Schmolck
;; :date: 2004-12-06
;; 
;; .. contents::
;; 
;; Purpose
;; -------
;; 
;; This mode provides editing and interactive evaluation support for the J
;; programming language (see <http://www.jsoftware.com>) under emacs.
;; 
;; .. image:: j-mode-screenshot.png
;;    :align: center
;; 
;; Features
;; --------
;; 
;; - syntax-highlighting 
;; - syntax-aware code indentation
;; - execution of code regions (current function, line etc.) in an
;;   interactive j-console
;; - help browsing: look up vocabulary item under cursor with a single keypress
;;   (``F1``)
;; 
;; Misfeatures
;; -----------
;; 
;; - Under windows running the J console in emacs is reported not to work
;;   properly -- however the problem seems to lie with J itself, so there's
;;   little that j-mode can do about it (**Addendum**: this appears to be fixed
;;   in ``j5.04``).
;; 
;; - Font locking might not also produce 100% correct results -- it seems
;;   practically impossible to completely resolve all since emacs's
;;   font-highlighting essentially gives suboptimal results for everything that
;;   steers away too far syntaxwise from C or lisp (**Addendum**: recently
;;   improved).
;; 
;; - I'm very busy and j-mode currently is very low priority -- so don't expect
;;   any substantial improvements in the foreseeable future. Having said that,
;;   I've been using j-mode under linux/emacs21 for some time and I think it
;;   should work reasonably well (better at least than the provided IDE under
;;   Linux, provided you're used to emacs).
;; 
;; Download
;; --------
;; 
;; <http://sourceforge.net/projects/j-mode/>
;; 
;; Installation
;; ------------
;; 
;; .. note:: If you are an emacs newbie you might first want to have a look here
;;           `(if you use windows)`_ and here `(regardless of OS)`_.
;;           (I might add that you don't need to restart emacs: you can just mark
;;           the code below and then execute it with ``M-x eval-region`` ).
;; 
;; .. _(if you use windows):
;;      http://www.gnu.org/software/emacs/windows/faq3.html#what-startup
;; .. _(regardless of OS):
;;       http://www.gnu.org/software/emacs/manual/html_node/Init-File.html
;; 
;; 
;; 1. Copy ``j-mode.el`` to a directory in your emacs `load-path'.
;; 
;; 2. Add the following to your ``.emacs`` file or ``site-init.el``::
;; 
;;     (autoload 'j-mode "j-mode.el"  "Major mode for J." t)
;;     (autoload 'j-shell "j-mode.el" "Run J from emacs." t)
;;     (setq auto-mode-alist
;;           (cons '("\\.ij[rstp]" . j-mode) auto-mode-alist))
;; 
;; 3. Set `j-path' to the right value, e.g. by adding something like the
;;    following to ``.emacs``::
;; 
;;     (setq j-path "/home/foo/j504/")
;; 
;; Other things you might find useful:
;; 
;; - ``imenu`` lets you jump between definition, you could add the following to
;;   your ``.emacs``::
;; 
;;    (add-hook 'j-mode-hook)
;;    (which-func-mode 1) ; shows the current function in statusbar
;; 
;; - the variable `browse-url-browser-function' determines which browser is
;;   used to browse the J help
;; 
;; - There are various options you can customize; do ``M-x customize-group``,
;;   then enter ``j``
;; 
;; 
;; Usage
;; -----
;; 
;; Open an .ijs file a press ``C-hm`` for more info or have a look at the
;; ``J`` menu item.
;; 
;; .. image:: http://sourceforge.net/sflogo.php?group_id=124293&amp;type=5
;;    :alt: sourceforge-logo
;;    :target: http://sourceforge.net/projects/j-mode/
;; 
;; TODO:
;; -----
;;
;; - smart ')' (auto-inserting '(' at beginning of expression when required).
;; - should explicit definitions really be treated specially for M-C-a etc?
;;   Maybe we should do this as python-mode's def/class destinction with
;;   prefix arg.
;; - should add proper prefix-arg handling to
;;   `j-beginning-of-explicit-definition' and `j-end-of-explicit-definition'
;;   (but this would seem to require different solutions for emacs and xemacs
;;   causing further compatibility cruft, sigh).
;; - should fix parsing of lone ")" (end of explicit definition should not be
;;   treated as paren) and, less importantly "'foo''bar'" (should be one sexp
;;   not two). Both seem quite difficult, first attempts with
;;   `font-lock-syntactic-keywords' where not blessed with success.
;; - highlighting for (foo)=: and `foo bar'=: ?
;; - better interaction with jconsole (should presumably also switch from
;;   `process-send-string' to `comint-send-string')
;; - full debugging support
;; - imenu support should ideally use `imenu-create-index-function'
;;   (`imenu-generic-expression' can't deal with ``'foo bar'=:...``), but I
;;   guess that's more a cosmetic concern
;; - should also add fume support for Xemacs
;; - add some templating/wizard type functionality
;; - proper support for labs/projects etc.
;; - support for align
;;
;; History:
;; ========
;;
;; 2004-12-06: - improved font-lock, help, documentation and fixed
;;               `j-execute-buffer' bug
;;
;;; Code:

(require 'browse-url)
(require 'cl)
(require 'comint)
(require 'custom)
(require 'easymenu)
(require 'imenu)

;;;_. Variables

;;;_ : Customizable Variables

(defgroup j nil
  "J language mode (see <http://www.jsoftware.com>)"
  :group 'languages
  :prefix "j-")

(defcustom j-path (expand-file-name "~/j503a/")
  "*The path under which J is installed."
  :type '(directory)
  :group 'j)

(defcustom j-command "jconsole"
  "*Command used to invoke J."
  :type '(string)
  :group 'j)


(defcustom j-command-args '()
  "*Command args J is invoked with."
  :type '(repeat string)
  :group 'j)

(defcustom j-dictionary-url
  "http://www.jsoftware.com/books/help/dictionary/"
  "URL of the j-dictionary (replace this)"
;;   (concat "file://"
;;           (expand-file-name
;;            "system/extras/help/dictionary/xmain.htm"
;;            j-path))
  :type '(string)
  :group 'j)

(defcustom j-foreign-help-url 
  "URL pointing to help for the foreign ('!:') conjunction."
  (concat  j-dictionary-url "xmain.htm")
  :type '(string)
  :group 'j)

(defcustom j-vocabulary-help-url 
  (concat  j-dictionary-url "vocabul.htm")
  "URL pointing to J vocubulary overview."
  :type '(string)
  :group 'j)

(defcustom j-indent-offset 2
"*Amount of offset per level of indentation."
  :type '(int)
  :group 'j)

;;;_ : defvars and defconsts

(defvar j-mode-map nil
  "Keymap used in J mode buffers")

(defvar j-mode-syntax-table nil
  "J mode syntax table")

;;COMPAT
(defvar j-no-generic-defun-functions 
  (not (boundp 'beginning-of-defun-function))
  "Xemacs compatibility crap.")

;;;_  , font-locking

(defconst j-keywords
  '(;; statements
    "assert."
    "if." "else." "elseif." 
    "select." "case." "fcase."
    "try." "except." "throw." "catch." "catcht."
    "do." "end."
    "return."
    "while." "whilst." 
    "break." "continue."
    "for." "for_" 
    "label_" 
    "goto_"
;;     ;; XXX maybe we should make 2 faces for these: 
;;     ;; left right args
;;     "["  "]"
;;     "x." "y."
;;     "u." "v."
;;     "m." "n."
    )
  "J Keywords."
  )

(defconst j-builtins
  `(;; modules
    "require" "load" "loadd" "script" "scriptd" 
    "jpath" "jcwdpath" "jhostpath" "jsystemdefs"
    ;; the evil foreign conjunction
    "[0-9]+!:[0-9]+"
    ;; OO
    "coclass" "cocreate" "cocurrent" "codestroy" "coerase"
    "coextend" "cofullname" "coinsert" "coname" "conames" "conew"
    "conl" "copath" "coreset"
    ;; environment
    "type" "names" "nameclass" "nc" "namelist" "nl" "erase" 
    ;; system
    "assert"
    "getenv" "setenv" "exit" "stdin" "stdout" "stderr"
    ;; :   :0 
    "def" "define" )
    "Some of the verbs from the z locale."
    )
(defconst j-constants
  '(
    ;; char codes
    "CR" "CRLF" "LF" "TAB" "a\\."
    ;; ace
    "a:"
    ;; grammar codes
    ;;0     1          2            3      3       4
    "noun" "adverb"  "conjunction" "verb" "monad" "dyad"
    )
    "Constants in the z locale.")

(defconst j-font-lock-keywords 
   (list
    ;; comments
    ;;FIXME hack to get comment to light up reliably -- this however a bit
    ;;*too* reliable -- it also works within strings (but that should normally
    ;;not cause to much trouble)
    '("\\(?:^\\|[^A-Za-z0-9_]\\)\\(NB\\..*$\\)" 1 font-lock-comment-face t) 
;;     '("NB\\..*$" . font-lock-comment-face)
;;;    ;;; multi-assignments (XXX should maybe get rid of this)
;;;    ;; global
;;;    (list 
;;;     "'\\(\\([A-Za-z]+[A-Za-z0-9_]*[ \t]*\\)+\\)'[ \t]*=:" 
;;;     1
;;;     'font-lock-function-name-face)
;;;    ;; local
;;;    (list 
;;;     "'\\(\\([A-Za-z]+[A-Za-z0-9_]*[ \t]*\\)+\\)'[ \t]*=\\." 
;;;     1
;;;     'font-lock-variable-name-face)
    ;;XXX which face should we use for these?
    '("[^A-Za-z0-9_]\\([mntuvwxy]\\.\\)" 1 font-lock-type-face t) 
    (list 
     "^[):]\n"
     0
     'font-lock-keyword-face)
    ;; global assignments (XXX should presumably split this into explicit and
    ;; tacit)
    (list 
     "\\([A-Za-z]+[A-Za-z0-9_]*\\)[ \t]*=:" 
     1 
     'font-lock-function-name-face)
    ;; local assignments
    (list 
     "\\([A-Za-z]+[A-Za-z0-9_]*\\)[ \t]*=\\." 
     1 
     'font-lock-variable-name-face)
    ;; for loop variables
    (list 
     "\\<for_\\([A-Za-z]+[A-Za-z0-9_]*\\)"
     1 
     'font-lock-variable-name-face)
    ;; Keywords
    (list 
     (concat 
      "\\<"
      "\\("
      (mapconcat #'regexp-quote
                 j-keywords
                 "\\|")
      "\\)"
	   )
     1
     'font-lock-keyword-face)
    ;; Builtins 
    (list 
     (concat
      "\\<"
      "\\("
      (mapconcat 'identity
                 j-builtins
                 "\\|")
      "\\)"
      "\\>"
      )
     1 
     'font-lock-builtin-face)
    (list 
     (concat
      "\\<"; XXX start delim
      "\\("
      (mapconcat 'identity
                 j-constants
                 "\\|")
      "\\)"
      "\\>" ;XXX end delim ;FIXME a: a.
      )
     1 
     'font-lock-constant-face)
   )
   "Expressions to highlight in J buffers.")

;;;_. Initializiations
;(put 'j-mode 'font-lock-defaults '(j-font-lock-keywords))
;;      '(j-font-lock-keywords             ;KEYWORDS
;;        nil                              ;KEYWORDS-ONLY
;;        nil                              ;CASE-FOLD
;;        '("^)$" 0 '((?\) . ".")))        ;SYNTAX-ALIST
;;        #'beginning-of-line              ;SYNTAX-BEGIN
;;        ))


;;;_ : Syntax table
;; initialize syntax table, if required
(unless j-mode-syntax-table
  (setq j-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\{  "."     j-mode-syntax-table) ; punct, not bracket
  (modify-syntax-entry ?\}  "."     j-mode-syntax-table) ; "                "
  (modify-syntax-entry ?\[  "."     j-mode-syntax-table) ; "                "
  (modify-syntax-entry ?\]  "."     j-mode-syntax-table) ; "                "
  (modify-syntax-entry ?\"  "."     j-mode-syntax-table) ; punct, not string
  (modify-syntax-entry ?\.  "w"     j-mode-syntax-table) ; word, not punct
  (modify-syntax-entry ?\:  "w"     j-mode-syntax-table) ; word, not punct
  (modify-syntax-entry ?\(  "(\)"   j-mode-syntax-table) ; open paren
  (modify-syntax-entry ?\)  ")\("   j-mode-syntax-table) ; close paren
  ;; XXX this means 'foo''bar' is treated as *2* sexps, unfortunately this
  ;; is not easily fixed
  (modify-syntax-entry ?\'  "\""     j-mode-syntax-table)
  (modify-syntax-entry ?\\  "."     j-mode-syntax-table) ; punct, not escape
  ;(modify-syntax-entry ?\'  "\\"     j-mode-syntax-table)
  )

;;;_ : Mode map
;; initialize key-map, if required
(unless j-mode-map
  (setq j-mode-map (make-sparse-keymap))
  ;; COMPAT: Xemacs doesn't seem to know about `beginning-of-defun-function'
  ;; etc.
  (if j-no-generic-defun-functions
      (progn
        (define-key j-mode-map "\e\C-a"   'j-beginning-of-explicit-definition)
        (define-key j-mode-map "\e\C-e"   'j-end-of-explicit-definition)
        (defun j-mark-defun ()
          "`mark-defun' a la J (for Xemacs compatiblity)."
          (interactive)
          (push-mark (point))
          (j-end-of-explicit-definition)
          (push-mark (point) nil t)
          (j-beginning-of-explicit-definition)
          (re-search-backward "^\n" (- (point) 1) t))
        ;; COMPAT "\e\C-h" has different meanings for Emacs/Xemacs
        (define-key j-mode-map [(control meta h)]   'j-mark-defun))
      (progn
        (setf (symbol-function 'j-mark-defun) #'mark-defun)
        (setf (symbol-function 'j-beginning-of-explicit-definition) 
              #'beginning-of-defun)
        (setf (symbol-function 'j-end-of-explicit-definition) #'end-of-defun)))
  ;;XXX should maybe use a different set of bindings
  (define-key j-mode-map "\C-c!"    'j-shell)
  (define-key j-mode-map "\C-c\C-c" 'j-execute-buffer)
  (define-key j-mode-map "\e\C-x"   'j-execute-explicit-definition)
  (define-key j-mode-map "\C-c\C-r" 'j-execute-region)
  (define-key j-mode-map "\C-c\C-l" 'j-execute-line)
  (define-key j-mode-map [f1] 'j-describe-voc)
  )

;;;_ : Menubar
(defvar j-menu nil
  "Menu for J mode.
Will get automatically created by `easymenu'.")
(easy-menu-define j-menu j-mode-map "J Mode menu"
  '("J"
    ["Comment Out Region"   comment-region  (mark)]
    ["Uncomment Region"     (comment-region (point) (mark)) (mark)]
    "-"
    ["Mark explicit definition" j-mark-defun t]
    "-"
    ["Start J"              j-shell t]
    ["Execute buffer"       j-execute-buffer t]
    ["Execute region"       j-execute-region (mark)]
    ["Execute explicit definition" j-execute-explicit-definition t]
    ["Execute line"         j-execute-line t]
    "-"
    ["Help on J-mode"       describe-mode t]
    ["Lookup Vocabulary Item (under cursor)"   j-describe-voc t]
    ["Help on !:"           j-browse-help-foreign t]
    ["Help on Vocabulary (all)"  j-browse-help-vocabulary t]
    "-"
;;     ["( ) Suspension Mode"  j-toggle-suspension-mode 
;;      [:selected (and (get-process "J") (j-supension-mode-p))
;;       :help "Toggles whether errors will trigger the J debugger"]]
    ))
;;;_ : Imenu support
;;;_. Functions
;;;_ : Helper functions
;;;_  , j-ensure-j-process
(defun j-ensure-j-process ()
  (or (get-process "J")
      (progn
        (apply 'make-comint
               "J"
               (expand-file-name j-command 
                                 j-path)
               nil
               j-command-args)
        (make-local-variable 'comint-prompt-regexp)
        (set-syntax-table j-mode-syntax-table)
        ;; (make-local-variable 'font-lock-defaults)
        ;; (setq font-lock-defaults (append j-font-lock-keywords 
        ;;                  '("^|.*?error:" . font-lock-warning-face)))
        (setq comint-prompt-regexp "^    +") ;XXX
                                        ;(run-hooks 'j-shell-hook)
        (get-process "J"))))
;;;_ : Movement and marking
;;;_  , j-end-of-explicit-definition


;; FIXME should have something like j-comment-line-p
(defsubst j-looking-at-comment-p ()
  "Return t if `looking-at' a J comment. Modifies match data!"
  ;; XXX technically we should check \< or some such, but this should do
  (looking-at "NB\\..*"))


;; 
;; see <URL:http://www.jsoftware.com/books/help/learning/12.htm>
(defsubst j-which-explict-definition ()
  "Return nil, 'one-liner or 'multi-liner depending on what kind of explicit
definition we are `looking-at'. Modifies match-data!"
  ;; XXX we could dump the check for NB. if we prepending '^' to the others
  (cond ((j-thing-outside-string "\\<define[ \t]*$") :multi-liner)
        ((j-thing-outside-string "\\(<\\def\\| :\\)[ \t]+")
         (case (char-after (match-end 0))
           ((nil) (error "XXX Illegal definition?"))
           ;; XXX one liners could also be constructed differently, but I
           ;; don't think that's parsable without evaluation
           ((?\') :one-liner)
           (otherwise :multi-liner)))
        (t nil)))


;;;_  , j-end-of-explicit-definition
(defun j-end-of-explicit-definition ()
  "Goto the end of the next explicit definition below point."
  (interactive)
  (let ((old-point (point)))
    (j-beginning-of-explicit-definition)
    (j-end-of-explicit-definition-raw)
    (if (<= (point) old-point)
        (j-end-of-explicit-definition-raw))))

;;;_  , j-end-of-explicit-definition-raw
(defun j-end-of-explicit-definition-raw ()
  (if (not (= (point) (point-at-eol)))
      (beginning-of-line)
      (forward-line 1))
  (beginning-of-line)
  (save-match-data
    (let ((type (loop for type = (j-which-explict-definition)
                      until (or type (= (point-at-eol) (point-max)))
                      do (forward-line 1)
                      finally return type)))
      (ecase type
        ((nil) nil)
        (:one-liner (progn (beginning-of-line 2) t))
        (:multi-liner (progn (search-forward-regexp "^)\n") t))))))
;;;_  , j-beginning-of-explicit-definition
(defun* j-beginning-of-explicit-definition ()
  "Got the start of the next explicit definition above point."
  (interactive)
  (if (not (= (point) (point-at-bol)))
      (beginning-of-line)
      (forward-line -1))
  (save-match-data
    ;; XXX this use of loop might be dodgy
    (loop until (or (j-which-explict-definition))
          do    (forward-line -1)
          when  (= (point-at-bol) (point-min))
            do (return-from j-beginning-of-explicit-definition nil)
      )
    t))


;;;_ : Indentation

(defconst j-indenting-keywords-regexp
  (concat "\\<\\("
          (mapconcat 'identity '(;;"do\\."
                                 "if\\." "else\\." "elseif\\." 
                                 "select\\." "case\\." "fcase\\."
                                 "throw\\." 
                                 "try\\." "except\\." "catch\\." "catcht\\."
                                 "while\\." "whilst\\." 
                                 "for\\." "for_" 
                                 "label_") "\\|") "\\)"))
(defconst j-dedenting-keywords-regexp
  (concat "\\<\\("
          (mapconcat 'identity   '("end\\."
                                   "else\\." "elseif\\."
                                   "case\\." "fcase\\."
                                   "catch\\." "catcht\\." "except\\."
                                   ) "\\|") "\\)"))

;;FIXME untested, stupid name
(defun j-thing-outside-string (thing-regexp)
  "Look for REGEXP from `point' til `point-at-eol' outside strings and
comments. Match-data is set for THING-REGEXP. Returns nil if no match was
found, else beginning and end of the match."
  (save-excursion
    (if (not (search-forward-regexp thing-regexp (point-at-eol) t))
        nil
        (let* ((thing-begin (match-beginning 0))
               (thing-end (match-end 0))
               (parse (save-excursion
                        (parse-partial-sexp (point-at-bol) thing-end))))
          (if (or (nth 3 parse) (nth 4 parse))
              nil
              (list thing-begin thing-end))))))

;; FIXME this doesn't just look at first keywords currently, same above
(defun* j-compute-indentation ()
  "Return what indentation should be in effect, disregarding contents of
current line."
  (save-excursion
    ;; skip empty/comment lines, if that leaves us in the first line, return 0
    (loop do (forward-line -1)
          while (looking-at "^[ \t]*\\(?:NB\\..*\\)?$")
          when (= (point-at-bol) (point-min))
            do (return-from j-compute-indentation 0))
    (save-match-data
      (back-to-indentation)
      (if (and (looking-at j-indenting-keywords-regexp)
               (progn 
                 (goto-char (match-end 0))
                 (not (j-thing-outside-string "\\<end\\."))))
          (+ (current-indentation) j-indent-offset)
          (current-indentation)))))

(defun j-indent-line ()
  "Indent current line correctly."
  (interactive)
  (let ((old-point (point)))
    (save-match-data
      (back-to-indentation)
      (let* ((tentative-indent (j-compute-indentation))
             ;;FIXME doesn't handle comments correctly
             (indent (if (looking-at j-dedenting-keywords-regexp)
                         (- tentative-indent j-indent-offset)
                         tentative-indent))
             (delta (- indent (current-indentation))))
;;         (message "###DEBUGi:%d t:%d" indent tentative-indent)
        (indent-line-to indent)
        (back-to-indentation)
        (goto-char (max (point) (+ old-point delta))))
      )))

;;;_ : mode activation

;;;###autoload
(defun j-mode ()
  "Major mode for editing J files. This mode offers: 

  1. syntax highlighting

  2. execution of J code:
     - \\[j-shell] to start J
     - \\[j-execute-buffer] to execute the whole buffer
     - \\[j-execute-explicit-definition] to execute the current explicit definition
     - \\[j-execute-region] to execute the active region
     - \\[j-execute-line] to execute the current line

  3. auto-indenting:
     - \\[newline-and-indent] to 
     - \\[j-indent-line]
     - \\[indent-region]

  4. Jumping to definitions (via `imenu'), marking (\\[j-mark-defun])
     and moving up (\\[j-beginning-of-explicit-definition]) and down (\\[j-end-of-explicit-definition]) 
     explicit definitions.

  5. Access to online help (\\[j-describe-voc\\], `j-browse-help-foreign', 
     `j-browse-help-vocabulary')

Summary of keybindings:
\\{j-mode-map}
"
  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'font-lock-defaults)
  ;(make-local-variable 'font-lock-syntactic-keywords)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  ;;(make-variable-buffer-local 'j-in-suspension-mode)
  (setq major-mode         'j-mode
        mode-name          "J"
        font-lock-defaults (list 
                            '(j-font-lock-keywords)           ;KEYWORDS
                             nil                              ;KEYWORDS-ONLY
                             nil                              ;CASE-FOLD
                             ;; could tune this for further optimization
                             nil                              ;SYNTAX-ALIST
                             ;; a mere speed-up
                             #'beginning-of-line              ;SYNTAX-BEGIN
                             )
        ;;FIXME `font-lock-syntactic-keywords' is a hack to fix syntax-table
        ;; limitations; we use it to make lone ')' behave as punctuation (not
        ;; bracket)
        comment-start      "NB. "
        comment-start-skip "NB\\. *"
        comment-end        ""
	comment-column     40
        indent-tabs-mode   nil ;; tabs are evil
        
        indent-line-function          #'j-indent-line
        ;;FIXME nonsense
        ;;j-in-suspension-mode nil
        )
  ;;COMPAT
  (unless j-no-generic-defun-functions
      (make-local-variable 'beginning-of-defun-function)
      (make-local-variable 'end-of-defun-function)
      (setq 
       beginning-of-defun-function   #'j-beginning-of-explicit-definition
       end-of-defun-function         #'j-end-of-explicit-definition))
;;   (setq font-lock-syntactic-keywords 
;;         ;;FIXME doesn't work for some reason (same with $)
;;         '(("^\\()\\)\n" (1 "."))
;; ;;         ;;FIXME 'foo''bar' strings are equally broken
;; ;;           ("'[^']+\\('\\)\\('\\)[^']*" 
;; ;;            (1 ".")
;; ;;            (1 "."))
;;           ))

  (setq imenu-generic-expression
        (list
         '(nil "\\<\\([A-Za-z]+[A-Za-z0-9_]*\\)[ \t]*=:" 1)
         ;;FIXME
         '("Classes" "\\<coclass[ \t]+'\\([A-Za-z]+[A-Za-z0-9_]*\\)'" 1)
         ))
  (use-local-map j-mode-map)
  (set-syntax-table j-mode-syntax-table)
  (easy-menu-add j-menu)
  (imenu-add-to-menubar (format "%s-%s" "IM" mode-name))
  (run-hooks 'j-mode-hook))

;;;_ : JConsole interaction
;;;_  , j-shell
;;;###autoload
(defun j-shell ()
  "Start the J interpreter."
  (interactive)
  (switch-to-buffer-other-window (process-buffer (j-ensure-j-process))))

;;;_  , j-execute-buffer
(defun j-execute-buffer (arg)
  "Send the contents of the buffer to a running J session.
  
  Possible values for ARG:

    nil:            same as 0
    0  (i.e. #b00): abort-on-error     silent   (default)
    1  (i.e. #b01): abort-on-error     display
    2  (i.e. #b10): continue-on-error  silent
    3  (i.e. #b10): continue-on-error  display

  (cf. 0!:0XX)
  "
  (interactive "P")
  (setq arg (or arg 0))
  (when (or (> arg 3) (< arg 0))
    (error "illegal prefix argument (must be 0-3)"))
  (save-some-buffers)
    (let ((proc (j-ensure-j-process))
          (buf-name (buffer-name))
          (buf-file-name (buffer-file-name))
    ;;                   run file     continue-on-error? display?
          (cmd (format "0!:0%d%d" (if (> arg 1) 1 0) (if (oddp arg) 1 0))))
;;;      (message "##DEBUG arg:%s cmd:%s odd:%s" arg cmd (oddp arg))
      (pop-to-buffer (process-buffer proc))
      (funcall (process-filter proc) proc 
               (format "NB. emacs: executing %s...\n" buf-name))
      (process-send-string "J" (concat
                                cmd 
                                " <'" buf-file-name "'"
                                "\n10 {a.\n"))))
           
;;;_  , j-execute-region
(defun j-execute-region (start end)
  "Send the region between START and END to the running J interpreter."
  (interactive "r")
  (let* ((proc (j-ensure-j-process))
         (start (min (point) (mark)))
         (end (max (point) (mark)))
         (region (buffer-substring-no-properties start end)))
    (when (= start end)
        (error "Region is empty"))
    (pop-to-buffer (process-buffer proc))
    (funcall (process-filter proc) proc 
              (if (and (= start (point-min)) (= end (point-max)))
                  "NB. sending whole buffer...\n"
                "NB. sending region...\n"))
    (process-send-string "J" (concat region "\n10 {a.\n"))
    ))

;;;_  , j-execute-line
(defun j-execute-line ()
  "Send the current line to a running J session."
  (interactive)
  (let ((proc (j-ensure-j-process))
        (line (concat (buffer-substring-no-properties 
                       (point-at-bol) (point-at-eol))
                      "\n")))
    (pop-to-buffer (process-buffer proc))
 ;;;(message "DEBUG: line:<<%s>>" line) ;DEBUG
    (funcall (process-filter proc) proc line)
    (process-send-string "J" line )))

;;;_  , j-execute-explicit-definition

;;XXX should maybe check that we are indeed in an explicit def, unlike elisp
;;counterpart
(defun j-execute-explicit-definition ()
  "Send the current explicit definition to a running J session."
  (interactive)
  (save-excursion
    (mark-defun)
    (let ((start (point))
          (end (mark)))
      (j-execute-region start end))))

;;;_ : Help



;; Extracted from <http://www.jsoftware.com/books/help/dictionary/vocabul.htm>
;; 
;; like so (plus minor hand-editing) 
;;
;; perl -ne 'print "(\"$2\" . \"$1\")\n" while 
;;    m|a\s+href="(.*?htm)"\s*><tt>(.*)?\s</tt>.*?|g;' vocabul.htm | \
;; perl -pe 'BEGIN{%r=("\\"=>"\\\\","&lt;"=>"<","\"\""=>"\"\\\"")}
;;    s/(\\|&lt;|"")/$r{$1}/e'

(defconst j-voc-to-doc-alist
  `(("=" . "d000.htm")
    ("=." . "d001.htm")
    ("=:" . "d001.htm")
    ("<" . "d010.htm")
    ("<." . "d011.htm")
    ("<:" . "d012.htm")
    (">" . "d020.htm")
    (">." . "d021.htm")
    (">:" . "d022.htm")
    ("_" . "d030.htm")
    ("_." . "d031.htm")
    ("_:" . "d032.htm")
    ("+" . "d100.htm")
    ("+." . "d101.htm")
    ("+:" . "d102.htm")
    ("*" . "d110.htm")
    ("*." . "d111.htm")
    ("*:" . "d112.htm")
    ("-" . "d120.htm")
    ("-." . "d121.htm")
    ("-:" . "d122.htm")
    ("%" . "d130.htm")
    ("%." . "d131.htm")
    ("%:" . "d132.htm")
    ("^" . "d200.htm")
    ("^." . "d201.htm")
    ("^:" . "d202n.htm")
    ("$" . "d210.htm")
    ("$." . "d211.htm")
    ("$:" . "d212.htm")
    ("~" . "d220v.htm")
    ("~." . "d221.htm")
    ("~:" . "d222.htm")
    ("|" . "d230.htm")
    ("|." . "d231.htm")
    ("|:" . "d232.htm")
    ("." . "d300.htm")
    (".." . "d301.htm")
    (".:" . "d301.htm")
    (":" . "d310n.htm")
    (":." . "d311.htm")
    ("::" . "d312.htm")
    ("," . "d320.htm")
    (",." . "d321.htm")
    (",:" . "d322.htm")
    (";" . "d330.htm")
    (";." . "d331.htm")
    (";:" . "d332.htm")
    ("#" . "d400.htm")
    ("#." . "d401.htm")
    ("#:" . "d402.htm")
    ("!" . "d410.htm")
    ("!." . "d411.htm")
    ("!:" . "d412.htm")
    ("/" . "d420.htm")
    ("/." . "d421.htm")
    ("/:" . "d422.htm")
    ("\\" . "d430.htm")
    ("\\." . "d431.htm")
    ("\\:" . "d432.htm")
    ("[" . "d500.htm")
    ("[:" . "d502.htm")
    ("]" . "d500.htm")
    ("{" . "d520.htm")
    ("{." . "d521.htm")
    ("{:" . "d522.htm") 
    ("{::". "d523.htm")
    ("}" . "d530n.htm")
    ("}." . "d531.htm")
    ("}:" . "d532.htm")
    ("\"" . "d600n.htm")
    ("\"." . "d601.htm")
    ("\":" . "d602.htm")
    ("`" . "d610.htm")
    ("`:" . "d612.htm")
    ("@" . "d620.htm")
    ("@." . "d621.htm")
    ("@:" . "d622.htm")
    ("&" . "d630n.htm")
    ("&." . "d631c.htm")
    ("&.:" . "d631.htm")
    ("&:" . "d632.htm")
    ("?" . "d640.htm")
    ("?." . "d640.htm")
    ("a." . "dadot.htm")
    ("a:" . "dadot.htm")
    ("A." . "dacapdot.htm")
    ("b." . "dbdotn.htm")
    ("c." . "dcdot.htm")
    ("C." . "dccapdot.htm")
    ("d." . "dddot.htm")
    ("D." . "ddcapdot.htm")
    ("D:" . "ddcapco.htm")
    ("e." . "dedot.htm")
    ("E." . "decapdot.htm")
    ("f." . "dfdot.htm")
    ("H." . "dhcapdot.htm")
    ("i." . "didot.htm")
    ("i:" . "dico.htm")
    ("j." . "djdot.htm")
    ("L." . "dlcapdot.htm")
    ("L:" . "dlcapco.htm")
    ("m." . "dmdot.htm")
    ("n." . "dmdot.htm")
    ("NB." . "dnb.htm")
    ("o." . "dodot.htm")
    ("p." . "dpdot.htm")
    ("p.." . "dpdotdot.htm")
    ("p:" . "dpco.htm")
    ("q:" . "dqco.htm")
    ("r." . "drdot.htm")
    ("s:" . "dsco.htm")
    ("S:" . "dscapco.htm")
    ("t." . "dtdotm.htm")
    ("t:" . "dtco.htm")
    ("T." . "dtcapdot.htm")
    ("u." . "dudot.htm")
    ("v." . "dudot.htm")
    ("u:" . "duco.htm")
    ("x."  . "dxdot.htm")
    ("y." . "dxdot.htm")
    ("x:" . "dxco.htm")
    ,@(mapcar (lambda (control) (cons control "ctrl.htm")) j-keywords)
    )
  "Maps J vocubulary items to html help page.")

(defconst j-voc 
  (sort (mapcar #'car j-voc-to-doc-alist)
        #'(lambda (v w) (> (length v) (length w))))
  "J (non-control, non-foreign) vocabulary items.")

(defconst j-foreign-regexp
  "[0-9]+!:[0-9]+"
  )

(defconst j-voc-regexp
  (concat "\\(" j-foreign-regexp "\\|"
          (mapconcat #'regexp-quote (append j-voc j-keywords) "\\|") 
          "\\)"))
(defun j-foreign-to-doc (foreign)
  "Find html page for a foreign conjunction"
  (format "dx%03d.htm" (string-to-int
                         (substring foreign 0 (position ?\! foreign)))))
(defvar j-describe-function-history nil
  "History of J vocabulary items look up in the docu.")

(defun j-hack-google-url (thing)
    (flet ((urlencode (term)
             ;; XXX this is rippped off from `browse-url-file-url', which
             ;; doesn't seem to it very well
             (let ((s 0))
               (while (setq s (string-match "%" term s))
                 (setq term (replace-match "%25" t t term)
                       s (1+ s))))
             (while (string-match "[*\"()',=;? ]" term)
               (let ((enc (format "%%%x" (aref term (match-beginning 0)))))
                 (setq term (replace-match enc t t term))))
             term))
      (format "http://www.google.com/search?q=site%%3Ajsoftware.com%%20%s&btnI=I'm+Feeling+Lucky" (urlencode thing))))



(defun* j-describe-voc (voc)
  "Look-up J vocabulary item at point on-line in the official J-doc -- prompt
  if there isn't any recognized vocabulary item under point."
  (interactive (list (let ((voc-at-point 
                            (save-match-data
                              ;;FIXME should really use something smarter here
                              ;;to correctly parse the symbol under point --
                              ;;at least this will reliably work if point is
                              ;;at beginning of symbol
                              (and (or (looking-at j-voc-regexp)
                                       (save-excursion
                                         (while (looking-at "[A-Za-z0-9_]")
                                           (backward-char))
                                         (looking-at j-voc-regexp)))
                                   (match-string-no-properties 0)))))
                       (if voc-at-point
                           voc-at-point
                           (completing-read
                            "Look up a vocabulary item in the J manual: "
                            j-voc-to-doc-alist nil
                            nil  ;; don't require match -- foreign!
                            voc-at-point
                            'j-describe-function-history)))))
  (let ((page
         (cond ((string-match j-foreign-regexp voc)
                (j-foreign-to-doc voc))
               ((assoc voc j-voc-to-doc-alist)
                (cdr (assoc voc j-voc-to-doc-alist)))
               (t (message "Don't know about %s, using google!!!" voc)
                  (browse-url (j-hack-google-url voc))
                  (return-from j-describe-voc)
                ;; (error "Don't know where help for %s is" voc)
                ))))
    (message "voc: %s page: %s" voc page)
    (browse-url (concat j-dictionary-url page))))

;("_9: </tt>to<tt> 9:" . "dconsf.htm"))


(defun j-browse-help-foreign ()
  "Browse help for foreign ('!:')."
  (interactive)
  (browse-url j-foreign-help-url))

(defun j-browse-help-vocabulary ()
  "Browse help on vocubulary."
  (interactive)
  (browse-url j-vocabulary-help-url))
 
;;;_ : Debugging support
;; Just first fragments for now

;; (defvar j-in-suspension-mode nil)

;; (defun j-suspension-mode-p ()
;;   (and (get-process "J") j-in-suspension-mode))

;; (defun j-toggle-suspension (arg)
;;   "Toggle whether J is in suspension mode. In suspension errors trigger the
;; debugger. Prefix arg enables suspension, zero prefix arg disables it."
;;   (interactive "P")
;;   (let ((should-suspend (if arg 
;;                             (zerop arg)
;;                           (not (j-suspension-mode-p)))))
;;     (j-execute-line (format "13!0 %d" (if j-in-suspension-mode 1 0)))))

;; ;;FIXME debugging stuff in general
;; (defmacro j-debug-command (name cmd)
;;   `(defun ,(intern (concat "j-debug-" name)) ()
;;     (interactive)
;;      (unless (j-suspension-mode-p)
;;        (error "Not in suspension"))
;;   (j-execute-line ,cmd)))

;; (j-debug-command "display-stack" "13!:1 ''")
;; (j-debug-command "display-full-stack" "13!:13 ''")
;; (j-debug-command "query-stops"   "13!:2 ''")
  
;;;_. PROVIDE
(provide 'j-mode)
;;; j-mode.el ends here

