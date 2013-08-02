;;; lout-mode.el --- an Emacs major-mode for editing Lout source.
;;;
;;; Author: Eric Marsden <emarsden@mail.dotcom.fr>
;;; Keywords: wp lout

;;   Copyright (C) 1997-1999 Eric Marsden
;;  
;;   Lout-mode is free software; you may redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published
;;   by the Free Software Foundation; either version 2, or (at your
;;   option) any later version.
;;  
;;   Lout-mode is distributed in the hope that it will be useful, but
;;   WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;   General Public License for more details.
;;
;;; Commentary: (see the README)
;;
;; The latest version of this file should be available from
;;     <URL:http://www.chez.com/emarsden/lout/>
;;
;; Known problems:
;; * the list of Lout keywords is incomplete
;; * the error message parser doesn't distinguish between true errors
;;   and simple warnings. It may also be fragile if the error messages
;;   produced by the Lout interpreter change with the version.
;;
;; TODO
;; * if variable 'lout-multiple-compiles' is t then keep compiling until
;;   you stop getting 'unknown symbol' warnings
;; * partial compilation à la AucTeX (for compile-region)
;;   could try narrowing to region (or using outline/folding mode) to
;;   select those regions we want compiled/printed
;;
;; ChangeLog:
;; * much improved syntax table
;; * use define-skeleton instead of my hacked insertion macro
;; * printer command may now be a function, as well as a string
;; * reporter interface for reporting bugs
;; * more complete symbol list (thanks to Alexandre Devaure)
;; * bug fixes (thanks to Alexandre Devaure <adevaure@mail.dotcom.fr>,
;;   James Ramsey <jjramsey_6x9eq42@yahoo.com> and <cgvadim@swansea.ac.uk>). 


;;; Code:

(eval-when-compile
  (require 'easymenu)
  (require 'compile)
  (require 'cl))

(defconst lout-version "0.3")           ; of this major-mode

(defvar lout-view-command "gv -safer"
  "*The name of the Postscript previewer on your system.
This program is called from \\[lout-view]. You can include command-line
arguments such as \"-dSAFER\" to protect against dangerous Postscript
commands.")

(defvar lout-print-command "lpx -Fps"
  "*How to print Postscript files on your system.
If this is a string it will be sent to a subshell, after having the
name of the Postscript file appended to it. If it is a function it
will be called with the name of the Postscript file as a parameter,
and should return a string which when passed to a shell, will print the
file.")

(defvar lout-quiet-save-flag nil
  "*If non-nil, ask before saving modified buffers before compiling.
Otherwise the buffer is saved automatically before calling \\[lout-file].")

(defvar lout-run-command "lout"
  "*The name of the Lout interpreter.")

(defvar lout-run-safely-flag t
  "*If non-nil, run Lout in \"safe\" mode.
Whether we should pass the -S switch to the Lout interpreter to
protect against malicious embedded system calls.")

;; unimplemented !!
(defvar lout-multiple-compiles nil
  "*If non-nil, run lout multiple times per compile.
Whether we should run the Lout interpreter repeatedly until it stops
emitting warnings about undefined symbols. Unimplemented.")

(defvar lout-user-guide "/usr/doc/lout/user.ps"
  "*Path to the Lout user guide")


;; Incomplete, and should probably be broken up into more sublists,
;; for graphics commands, font commands, important and less important
;; commands, abbreviations, etc. This is left as an exercise for the
;; reader :-)
(defconst lout-environment-list
  '("@Abstract" "@AL" "@AlignedDisplay" "@AlphaList" "@And" "@Angle"
    "@Appendix" "@Arc" "@Arrow" "@B" "@BaseOf" "@Begin"
    "@BeginAlignedDisplays" "@BeginAppendices" "@BeginSubSections"
    "@BeginSubSubAppendices" "@BeginSubSubSections" "@BI" "@BL" "@Book"
    "@BookLayout" "@BoundaryMarks" "@Box" "@Break" "@BulletList" "@CC"
    "@CD" "@CDot" "@Center" "@CenteredDisplay" "@CenteredList"
    "@ChapCite" "@ChapRef" "@ChapRefListTitle" 
    "@Chapter" "@Char" "@Circle" "@Cite" "@CL" "@Claim" "@CNP" "@Color"
    "@Colour" "@ColumnGap" "@ColumnNumber" "ContentsGoesHere"
    "@ContentsGap" "@ContentsGapAbove" "@ContentsGapBelow"
    "@ContentsLeader" "@ContentsLeaderGap" "@ContentsRightWidth"
    "@CoverSheet" "@CP" "@CPrint" 
    "@Corollary" "@CurveBox" "@D" "@DashList" "@Data" "@Database"
    "@Date" "@DefaultIndent" "@Definition" "@Diamond" "@Display"
    "@DisplayGap" "@DisplayIndent" "@Distance"
    "@DL" "@DLI" "@Doc" "@Document" "@DocumentLayout" "@DotJoin"
    "@DotSep" "@DP" "@DropListItem" 
    "@DropTagItem" "@DTI" "@El" "@Ellipse" "@End" "@EndList"
    "@EndAlignedDisplays"
    "@EndAppendicies" "@EndChapters" "@EndNote" "@EndOverheads"
    "@EndProof" "@EndSections" "@EndSubAppendices" "@EndSubSections"
    "@EndSubSubAppendices" "@EndSubSubSections" "@Eq"
    "@EvenLeftMargin" "@EvenRightMargin" "@Example"
    "@Fig" "@Figure" "@FigureCaptionPos" "@FigureLocation" "@FigureNumbers"
    "@FigurePageNumber" "@FirstPageNumber" "@Fmta" "@Font"
    "@FootMargin" "@FootAboveGap" "@FootGap" "@FootLen" "@FootNote"
    "@FootNoteBreak" "@FootNoteFont" "@FootNoteLocation"
    "@FootNoteNumbers" "@FootNoteThrough" "@Frame" "@FullWidth" 
    "@Graph" "@GraphCircle" "@GraphCross" "@GraphDashed" "@GraphDiamond"
    "@GraphDotted" "@GraphFilledCircle" "@GraphFilledDiamond"
    "@GraphFilledSquare" "@GraphFilledTriangle" "@GraphNoLine"
    "@GraphPlus" "@GraphSolid" "@GraphSquare" "@GraphTriangle"
    "@HArrow" "@Heading" "@HeadingFont" "@HLine"
    "@I" "@ID" "@If" "@IL" "@Illustration"
    "@Include" "@IncludeGraphic" "@IndentedDisplay" "@IndentedList"
    "@IndexBlanks" "@IndexBreak" "@IndexColumnGap" "@IndexColumnNumber"
    "@IndexFont" "@InitialBreak" "@InitialFont" "@InitialLanguage"
    "@InitialSpace" "@IntroFirstPageNumber" "@IntroPageNumbers"
    "@Introduction" "@JoinFigures" "@L" "@Label"
    "@Language" "@LD" "@Lecture" "@LeftDisplay" "@LeftList" "@LeftNote"
    "@Lemma" "@LI" "@Line" "@List" "@ListItem" "@LL" "@LLP" "@Location"
    "@LP" "@MajorNum" "@MajorTitle" "@MakeContents" "@MakeIndex"
    "@MakeReferences" "@MarkOf" "@MarkRow" "@MinorNum" "@MinorTitle"
    "@Minute" "@Multiply" "@NL" "@NoChapCite" "@NoChapRef" "@NoCite"
    "@NoRef" "@Not" "@NP" "@Null" "@NumberedDisplay"
    "@NumberedList" "@NumberedOf" "@OddLeftMargin" "@OddRightMargin"
    "@Or" "@OrdinaryLayout" "@OuterNote" "@Over" "@Overhead"
    "@OverheadLayout" "@OverheadTransparencies" "@OverStrike"
    "@PageBackground" "@PageBoxType" "PageHeaders" "@PageHeight"
    "@PageNum" "@PageNumbers" "@PageOrientation" "@PageType" "@PageWidth"
    "@PageMark" "@PageOf" "@PAL" "@ParaGap" "@ParaIndent"
    "@ParenAlphaList" "@ParenNumberedList" "@ParenRomanList"
    "@ParenUCAlphaList" "@ParenUCRomanList" "@ParSym"
    "@ParNumber" "@ParText" "@ParTitle" "@Pas" "@Place" "@PNL"
    "@Polygon" "@PP" "@Preface"
    "@Prev" "@PRL" "@Proof" "@Proposition" "@PUCAL" "@PUCRL" "@QD" "@QL"
    "@QuotedDisplay" "@QuotedList"
    "@R" "@RawEndList" "@RawList" "@Ref" "@RefPrint" "@RefStyle"
    "@RefCiteLabels" "@RefCiteStyle" "@Reference"
    "@ReferencesBeforeAppendices" "@RefListBreak" "@RefListFont"
    "@RefListFormat" "@RefListGap" "@RefListIndent" "@RefListLabels"
    "@RefListLabelWidth" "@RefListRightIndent" "@RefListSortKey"
    "@RefListTitle" "@RefNumbers" "@Register" "@Report" "@ReportLayout"
    "@Right" "@RightDisplay" "@RightNote" "@RL" "@RomanList" "@Rotate"
    "@Rowa" "@RR" "@RunningTitle" "@S" "@Scale" "@Second" "@Section"
    "@SectSym" "@SeparateIntoNumbering" "@SetColour"
    "@ShadowBox" "@ShowLabels" "@SL""@Square" "@Star" "@StarList"
    "@SubAppendix"
    "@SubSection" "@SubSubAppendix" "@SubSubSection" "@Sym"
    "@SysDatabase" "@SysInclude"
    "@Tab" "@Table" "@TableCaptionPos"
    "@TableLocation" "@TableNumbers" "@Tag" "@TaggedList"
    "@TagItem" "@Theorem"
    "@TI" "@Time" "@Title" "@TitlePageFont" "@TL" "@TopMargin"
    "@True" "@Type" "@UCAL"
    "@UCAlphaList" "@UCRL" "@UCRomanList"
    "@Underline" "@Use" "@VArrow" "@Verbatim" "@VeryWideTaggedList"
    "@VLine" "@VShift"
    "@VWTL" "@Wide"
    "@WideTaggedList" "@WTL"
    "@XDistance" "@YDistance")
  "Standard Lout block names.")

(defconst lout-symbols-list
  '("@Bullet" "@ParSym" "@Dagger" "@CDot" "@Yen" "@Degree"
    "@Second" "@Multiply" "@CopyRight" "@TradeMark" "@Start"
    "@SectSym" "@DaggerDbl" "@Sterling" "@Florin" "@Minute"
    "@Lozenge" "@Divide" "@Register"))   

(defconst lout-font-lock-keywords
  (list
   '("@\\(Chapter\\|Section\\|SubSection\\|Abstract\\|BeginSections\\|EndSections\\|BeginSubSections\\|EndSubSections\\|Appendix\\|BeginSubAppendices\\|EndSubAppendices\\|SubAppendix\\|Preface\\)"
     1 font-lock-function-name-face)
   '("@\\(Begin\\)[ ]+\\([a-zA-Z]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
   '("@\\([a-zA-Z]+\\)" 1 font-lock-keyword-face)
   '("{||?\\|//?}{[0-9]+[cipmfsvx]}?" . font-lock-type-face)
   '("@B[ ]+{\\([^}]*\\)}" 1 'bold keep)
   '("@B[ ]+\\(\\w+\\)" 1 'bold keep)
   '("@I[ ]+{\\([^}]*\\)" 1 'italic keep)
   '("@I[ ]+\\(\\w+\\)" 1 'italic keep)
   '("@BI[ ]+{\\([^}]*\\)}" 1 'bold-italic keep)
   '("@BI[ ]+\\(\\w+\\)" 1 'bold-italic keep)
   '("@Title +{\\([^}]*\\)}" 1 'bold keep)
   '("def\||macro" . font-lock-function-name-face))
  "Additional expressions to highlight in Lout mode.")


;; each of these will have ^.*@ prepended
(defvar lout-outline-level1
  "\\(Abstract\\|Chapter\\|Section\\)\\s-+\\(@Title\\)?\\s-*{")
(defvar lout-outline-level2 "SubSection\\s-+\\(@Title\\)?\\s-*{")
(defvar lout-outline-level3 "SubSubSection\\s-+\\(@Title\\)?\\s-*{")

(defvar lout-imenu-generic-expression
  '(("*Chapters*" "^\\s-*@Chapter\\s-+@Title\\s-*{\\s-*\\([^}]+\\)\\s-*}" 1)
    ("*Sections*" "^\\s-*@Section\\s-+@Title\\s-*{\\s-*\\([^}]+\\)\\s-*}" 1)
    ("*Subsections*"
     "^\\s-*@SubSection\\s-+@Title\\s-*{\\s-*\\([^}]+\\)\\s-*}" 1))
  "Imenu regexps, for identifying parts of an Lout document we may wish
   to jump to.")

(defvar lout-face-alist
  '((bold         . "@B {")
    (italic       . "@I {")
    (bold-italic  . "@BI {")
    (default      . "@R {"))
  "Alist of face and Lout font name for facemenu.")

(defvar lout-font-alist
  '((?\C-b "@B {" "}")
    (?\C-i "@I {" "}")
    (?\C-l "@BI {" "}")                 ; {bold} intersect {italic} = l
    (?\C-r "@R {" "}")                  ; (Boris Goldowsky)
    (?\C-f "@F {" "}")
    (?\C-s "@S {" "}")))



;;; End of configuration variables ------------------------------------------

(defmacro GNU (&rest x)
  (list 'if (string-match "GNU" (version)) (cons 'progn x)))
(defmacro XEmacs (&rest x)
  (list 'if (string-match "XEmacs" (version)) (cons 'progn x)))

;; build an alist from all the types of Lout symbols we know of
(defvar lout-completions-alist
  (mapcar (lambda (x) (cons x ""))
          (append lout-environment-list
                  lout-symbols-list)))

(defvar lout-mode-syntax-table nil
  "Syntax table used while in Lout mode.")

(defvar lout-view-process nil)
(defvar lout-print-process nil)
(defvar lout-mode-map nil "Keymap for Lout mode.")

(if lout-mode-map
    nil
  (setq lout-mode-map (make-sparse-keymap))
  (define-key lout-mode-map "\e\t"          'lout-complete-symbol)
  (define-key lout-mode-map "\C-c\C-b"      'lout-file)
  (define-key lout-mode-map "\C-c\C-v"      'lout-view)
  (define-key lout-mode-map "\C-c\C-p"      'lout-print)
  (define-key lout-mode-map "\C-c\C-kc"     'lout-kill-compilation)
  (define-key lout-mode-map "\C-c\C-kv"     'lout-view-kill)
  (define-key lout-mode-map "\C-c\C-kp"     'lout-print-kill)

  (define-key lout-mode-map "\C-c\C-s"      'lout-insert-section)
  (define-key lout-mode-map "\C-c\C-c"      'lout-insert-chapter)
  (define-key lout-mode-map "\C-c\C-t"      'lout-insert-table)
  (define-key lout-mode-map "\C-c\C-g"      'lout-insert-figure)
  (define-key lout-mode-map "\C-c\C-u"      'lout-insert-subsection)
  (define-key lout-mode-map "\C-ca"         'lout-insert-item); NOT IMPLEMENTED!!
  
  (define-key lout-mode-map "\C-cC-#"       'lout-comment-paragraph)
  
  (define-key lout-mode-map "\C-c\C-f\C-i"  'lout-italic-font)
  (define-key lout-mode-map "\C-c\C-f\C-b"  'lout-bold-font)
  (define-key lout-mode-map "\C-c\C-f\C-o"  'lout-bitalic-font)
  (define-key lout-mode-map "\C-c\C-f\C-r"  'lout-roman-font)
  (define-key lout-mode-map "\C-c\C-f\C-t"  'lout-typew-font)
  (define-key lout-mode-map "\C-c\C-f\C-c"  'lout-scap-font)
  (define-key lout-mode-map "\C-c\C-a"      'lout-insert-abstract))

;; The "Insert symbol" menu is built dynamically from the contents of
;; the variable lout-symbols-list. Unfortunately this causes a
;; noticeable delay the first time the menu is "opened".
(defun lout-symbols-menu ()
  (append (list "Symbol")
          (mapcar
           (lambda (x) (vector x (list 'lout-insert-generic x) t)) ; was 't
           lout-symbols-list)))


(require 'easymenu)
(easy-menu-define lout-mode-menu
                  lout-mode-map
                  "Menu used in Lout mode."
  (list "Lout"
        ["Complete" lout-complete-symbol t]
        (list "Insert"
              ["Abstract" lout-insert-abstract t]
              ["Chapter" lout-insert-chapter t]
              ["Section" lout-insert-section t]
              ["SubSection" lout-insert-subsection t]
              ["Figure" lout-insert-figure t]
              ["Graphics" (lout-insert-command "@Graphics") t]
              ["Table" lout-insert-table t]
              (list "Display"
                    ["Standard" (lout-insert-command "@Display") t]
                    ["Left"     (lout-insert-command "@LeftDisplay") t]
                    ["Indented" (lout-insert-command "@IndentedDisplay") t]
                    ["Quoted"   (lout-insert-command "@QuotedDisplay") t]
                    ["Centered" (lout-insert-command "@CenteredDisplay") t]
                    ["Right"    (lout-insert-command "@RightDisplay") t])
              (list "List"
                    ["Plain"  (lout-insert-list "@List") t]
                    ["1."     (lout-insert-list "@NumberedList") t]
                    ["i."     (lout-insert-list "@RomanList") t]
                    ["I."     (lout-insert-list "@UCRomanList") t]
                    ["a."     (lout-insert-list "@AlphaList") t]
                    ["A."     (lout-insert-list "@UCAlphaList") t]
                    ["Bullet" (lout-insert-list "@BulletList") t]
                    ["*"      (lout-insert-list "@StarList") t]
                    ;; ! without the space we get a horizontal rule
                    [" -"     (lout-insert-list "@DashList") t]
                    ["(1)"    (lout-insert-list "@ParenNumberedList") t]
                    ["(i)"    (lout-insert-list "@ParenRomanList") t]
                    ["(I)"    (lout-insert-list "@ParenUCRomanList") t]
                    ["(a)"    (lout-insert-list "@ParenAlphaList") t]
                    ["(A)"    (lout-insert-list "@ParenUCAlphaList") t])
              (lout-symbols-menu)
              (list "Note"
                    ["Footnote"     (lout-insert-command "@FootNote") t]
                    ["Left margin"  (lout-insert-command "@LeftNote") t]
                    ["Right margin" (lout-insert-command "@RightNote") t]
                    ["Inner margin" (lout-insert-command "@InnerNote") t]
                    ["Outer margin" (lout-insert-command "@OuterNote") t]))
        (list "Font"
              ["Bold"       lout-bold-font    :keys "C-c C-f C-b"]
              ["Italic"     lout-italic-font  :keys "C-c C-f C-i"]
              ["BoldItalic" lout-bitalic-font :keys "C-c C-f C-o"]
              ["Roman"      lout-roman-font   :keys "C-c C-f C-r"]
              ["Typewriter" lout-typew-font   :keys "C-c C-f C-t"]
              ["Small Caps" lout-scap-font    :keys "C-c C-f C-c"])
        ;; should add change font commands C-u C-c C-f C-<whatever> which
        ;; operate on current region, different @Break styles
        "--------------------------------------------------------"
        ["Loutify" lout-file t]
        ["Preview" lout-view t]
        ["Print" lout-print t]
        ["Help" lout-view-doc t]
        ["Next Error" next-error (get-buffer "*compilation*")]
        ["Kill lout job" lout-kill-compilation (get-buffer "*compilation*")]
        ["Kill previewer" lout-view-kill lout-view-process]
        ["Kill print job" lout-print-kill lout-print-process]
        ["Imenu" imenu (fboundp 'imenu)]
        (list "Miscellaneous"
              ["Comment region" comment-region (lout-mark-active)]
              ["Uncomment region" lout-uncomment-region (lout-mark-active)]
              ["Fill region" fill-region (lout-mark-active)])))


;;;###autoload
(defun lout-mode ()
  "Major mode for editing Lout files.

Use \\[lout-file] to run the Lout interpreter on the current file.
You can step through the errors picked up by the compiler with
\\[next-error].
\\[lout-view] previews the Postscript file produced by the compilation.
\\[lout-print] prints the Postscript file.

Special commands:
\\{lout-mode-map}

Mode variables:
lout-run-command
    Command string used by \\[lout-file]
lout-view-command:
    Command string used by \\[lout-view] to run the Postscript previewer.
lout-print-command:
    Command string used by \\[lout-print] to print the Postscript output.

Entering Lout-mode runs the hook `text-mode-hook' then `lout-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map lout-mode-map)
  (setq major-mode 'lout-mode)
  (setq mode-name "Lout")
  (setq local-abbrev-table text-mode-abbrev-table)
  (unless lout-mode-syntax-table
    (setq lout-mode-syntax-table (make-syntax-table))
    (set-syntax-table lout-mode-syntax-table)
    (loop for char from   0 to  31 do (modify-syntax-entry char "."))
    (loop for char from 128 to 255 do (modify-syntax-entry char "."))
    (modify-syntax-entry ? " ")         ; whitespace
    (modify-syntax-entry ?~ " ")        ; whitespace
    (modify-syntax-entry ?\t " ")       ; whitespace
    (modify-syntax-entry ?\n ">")       ; comment end
    (modify-syntax-entry ?\f ">")       ; comment end
    (modify-syntax-entry ?#  "<")       ; comment start
    (modify-syntax-entry ?@  "_")       ; symbol constituent
    (modify-syntax-entry ?_  "_")
    (modify-syntax-entry ?\" "\"")
    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){")
    (modify-syntax-entry ?«  "(»")      ; french cedilla
    (modify-syntax-entry ?»  ")«")
    (modify-syntax-entry ?^  ".")
    (modify-syntax-entry ?'  "w")
    (modify-syntax-entry ?/ "'")        ; expression prefix
    (modify-syntax-entry ?| "'")        ; expression prefix    
    (modify-syntax-entry ?& "'")        ; expression prefix
    (modify-syntax-entry ?$ "'"))       ; expression prefix
  (set-syntax-table lout-mode-syntax-table)  
  (easy-menu-add lout-mode-menu)

  ;; OK, I'm assuming you separate paragraphs with blank lines.
  ;; Change this to include '@LP', '@PP' if you feel the need.
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)

  ;; outline-minor-mode support
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-heading-end-regexp)
  (make-local-variable 'outline-level)
  (setq outline-regexp (concat "^[^@]*@\\("
                               lout-outline-level1
                               "\\)\\|\\("
                               lout-outline-level2
                               "\\)\\|\\("
                               lout-outline-level3
                               "\\)"))
  (setq outline-heading-end-regexp "}")
  (setq outline-level
    (function
     (lambda ()
       (cond ((looking-at lout-outline-level1) 1)
             ((looking-at lout-outline-level2) 2)
             ((looking-at lout-outline-level3) 3)))))
  
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (setq comment-start "# ")
  (setq comment-start-skip "#+[ \t]*")
  (setq comment-end "")
  (setq comment-column 32)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(lout-font-lock-keywords
          nil ; keywords-only
          nil ; case-fold: if not nil ignore case when fontifying
          nil ; syntax table
          nil ; syntax begin: function enclosing a syntactic block
          (font-lock-mark-block-function . mark-paragraph)))
  
  ;; facemenu stuff
  (make-local-variable 'facemenu-add-face-function)
  (make-local-variable 'facemenu-end-add-face)
  (make-local-variable 'facemenu-remove-face-function)
  (setq facemenu-add-face-function
	(lambda (face end)
	  (let ((face-text (cdr (assq face lout-face-alist))))
	    (if face-text
		face-text
	      (error "Face %s not configured for %s mode" face mode-name))))
	facemenu-end-add-face "}"
	facemenu-remove-face-function t)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression lout-imenu-generic-expression)
  (make-local-variable 'lout-face-alist)
  (setq lout-face-alist lout-face-alist)                       

  (run-hooks 'text-mode-hook 'lout-mode-hook))



;; ================================== SUBPROCESS HANDLING ============
;; A neighbor came to Nasrudin, asking to borrow his donkey. "It is
;; out on loan," the teacher replied. At that moment, the donkey
;; brayed loudly inside the stable. "But I can hear it bray, over
;; there." "Whom do you believe," asked Nasrudin, "me or a donkey?"

(defun lout-compilation-parse-errors (limit-search find-at-least)
  "Parse the current buffer as error messages.
We take line-number, column-number and store the text location in
compilation-error-list. The function `next-error', assigned to
\\[next-error], takes the next error off the list and visits its
location."
  (let ((lout-compilation-error-list nil)
        (lout-filename "dummy.ld"))
    (message "Parsing Lout error messages...")
    ;; Skip to beginning of error messages. We look for a line of the form
    ;; lout file "lout-filename" (from ...):
    ;; If lout-filename is a Lout database file we ignore all the errors
    ;; concerning it, and skip to the source file.
    (goto-char (point-min))
    (while (string-match ".ld$" lout-filename)
      (re-search-forward "lout file +\"\\([^\"]+\\)" limit-search t)
      (setq lout-filename (match-string 1)))
  
    (while (re-search-forward "\\([0-9]+\\),\\([0-9]+\\):" limit-search t)
      (let ((line-number (string-to-number (match-string 1)))
            (column-number (string-to-number (match-string 2)))
            (error-marker (point-marker))
            (text-marker nil)
            (text-buffer (find-file-noselect lout-filename)))
        ;; look in the lout buffer for corresponding text position
        ;; (beginning-of-line)
        (if text-buffer
            (save-excursion
              (set-buffer text-buffer)
              (goto-line line-number)
              (move-to-column column-number)
              (setq text-marker (point-marker))
              (setq lout-compilation-error-list
                    (cons (cons error-marker text-marker)
                          lout-compilation-error-list))))))
    (setq compilation-parsing-end (point-max))
    (message "Parsing Lout error messages...done")
    (setq compilation-error-list (nreverse lout-compilation-error-list))))

;; this should work even with MS operating systems ...
(defun lout-output-filename (infile-name)
  "Calculate a name for lout's output.
Given the name of the Lout source file, produce an absolute output
file name ending with .ps"
  (convert-standard-filename
   (concat default-directory
           (file-name-sans-extension (file-name-nondirectory infile-name))
           ".ps")))

(defun lout-kill-compilation ()
  "Interrupt the Lout interpreter."
  (interactive)
  (kill-compilation))

(defun lout-file ()
  "Run lout on the current file, to produce a Postscript file.
`lout-run-command' needs to be set to the name of the Lout interpreter."
  (interactive)
  (if (and (buffer-modified-p)
           (or lout-quiet-save-flag
               (yes-or-no-p "Save buffer before compiling? ")))
      (save-buffer))
  (if (null lout-run-command)
      (error "You need to set `lout-run-command'"))
  (make-local-variable 'compile-command)
  (setq compile-command (concat lout-run-command
                                (if lout-run-safely-flag " -S " " ")
                                buffer-file-name
                                " > "
                                (lout-output-filename buffer-file-name)))
  (message compile-command)
  (make-local-variable 'compilation-parse-errors-function)
  (setq compilation-parse-errors-function 'lout-compilation-parse-errors)
  (compile compile-command))

;; split the string S into a list of (whitespace-separated) words
(defun lout-tokenize (s)
  (let ((accum '()))
    (while (string-match "^[ \t]*\\([^ \t]+\\)\\(.*\\)$" s)
      (setq accum (append accum (list (match-string 1 s))))
      (setq s (match-string 2 s)))
    accum))

(defun lout-view ()
  "Preview the Postscript file produced by lout.
You need to set the variable `lout-view-command' to the name of your
previewer."
  (interactive)
  (let ((lout-outfile ""))
    (if (memq lout-view-process (process-list))
        (error "The previewer is already running. Ask it to reload the file"))
    (or lout-view-command
        (error "You must set `lout-view-command'"))
    (setq lout-outfile (lout-output-filename buffer-file-name))
    (if (not (file-exists-p lout-outfile))
        (error "No appropriate Postscript file could be found"))
    (setq lout-view-process
          (apply 'start-process
                 "lout-view"
                 nil
                 (append (lout-tokenize lout-view-command)
                         (list lout-outfile))))))

(defun lout-view-kill ()
  "Kill the Postscript preview program."
  (interactive)
  (if (not (memq lout-view-process (process-list)))
      (error "The previewer isn't running!"))
  (kill-process lout-view-process)
  (setq lout-view-process nil))

(defun lout-view-doc ()
  "View the Lout user guide."
  (interactive)
  (apply 'start-process
         "lout-view-doc"
         nil
         (append (lout-tokenize lout-view-command)
                 (list lout-user-guide))))

;; note that spawning a new print job means you can no longer cancel
;; the previous print job.
(defun lout-print ()
  "Print the Postscript output file produced by lout.
You need to set the variable `lout-print-command'."
  (interactive)
  (let* ((outfile (lout-output-filename buffer-file-name))
         (command (cond ((functionp lout-print-command)
                         (funcall lout-print-command outfile))
                        ((stringp lout-print-command)
                         (concat lout-print-command " " outfile))
                        (t
                         (error "lout-print-command must be a string or a function")))))
    (setq lout-print-process
          (apply 'start-process
                 "lout-print"
                 nil
                 (lout-tokenize command)))))

(defun lout-print-kill ()
  (interactive)
  (if (not (memq lout-print-process (process-list)))
      (error "There are no print jobs running!"))
  (kill-process lout-print-process)
  (setq lout-print-process nil))

(defun lout-font (string)
  (interactive "*")
  (let ((entry (assoc string lout-font-alist))
        (pos (point)))        
    (if (null entry)
        (error "lout-font: no such font entry!"))
    (insert (nth 1 entry))
    (setq pos (point))
    (insert (nth 2 entry))
    (goto-char pos)))

(defun lout-bold-font     () (interactive) (lout-font ?\C-b))
(defun lout-italic-font   () (interactive) (lout-font ?\C-i))
(defun lout-bitalic-font  () (interactive) (lout-font ?\C-l))
(defun lout-roman-font    () (interactive) (lout-font ?\C-r))
(defun lout-typew-font    () (interactive) (lout-font ?\C-f))
(defun lout-scap-font     () (interactive) (lout-font ?\C-s))


(defun lout-complete-symbol ()
  "Perform completion on word preceding point.
If the word starts with '@' it is assumed to be an Lout keyword,
and it is compared with a list of standard Lout keywords. Otherwise
it is assumed to be an ordinary word and is passed to
`ispell-complete-word'."
  (interactive "*")
  (let* ((end (point))
         (beg (unwind-protect
                  (save-excursion
                    (forward-word -1)
                    (forward-char -1)
                    (unless (eq ?@ (char-after)) (forward-char 1))
                    (point))))
         (word (buffer-substring beg end))
         (completion ""))
    (if (not (equal ?@ (string-to-char word)))
        (ispell-complete-word word)    
      (setq completion (try-completion word lout-completions-alist))
      (cond ((eq completion t)            ; exact match
             nil) 
            ((null completion)            ; no match
             (message "Can't find completion for \"%s\"" word)
             (ding))
            ((not (string= word completion))
             (delete-region beg end)
             (insert completion))         ; true completion
            (t                            ; partial completion
             (message "Making completion list...")
             (let ((completions (all-completions word lout-completions-alist)))
               (with-output-to-temp-buffer "*Completions*"
                 (display-completion-list completions)))
             (message "Making completion list...done"))))))


;; ========================================== INSERTION COMMANDS ==============

;; Insert our arguments into the buffer. A '_' in the input is a
;; request to return the cursor to this point after the insertion.
;; I've mostly replaced this by skeleton-insertion commands which do
;; the job much better, and are user-configurable.
(defmacro lout-macro-insert (first &rest body)
  (let ((pos (make-symbol "point-position"))
        (args (make-symbol "args")))
    `(progn
       (setq args '(,@body))
       (insert ,first)
       (while (not (null args))
         (if (eq '_ (car args))
             (setq pos (point))
           (insert (car args)))
         (setq args (cdr args)))
       (goto-char pos))))

(defun lout-insert-generic (thing)
  (interactive "*")
  (insert thing))

(defun lout-insert-command (thing)
  (interactive "*")
  (lout-macro-insert
   thing " { " _ " } "))

;; type is a string such as @NumberedList
(defun lout-insert-list  (type)
  (interactive "*")
  (lout-macro-insert
   type "\n"
   "   @ListItem { " _ " }\n"
   "   @ListItem { }\n"
   "@EndList\n"))


(autoload 'skeleton-read "skeleton")

(define-skeleton lout-insert-abstract "Insert the @Abstract" nil
  (if (bolp) nil ?\n)
  '(setq v1 (skeleton-read "Abstract title: "))
   "@Abstract @Title{ " v1 " }\n"
   "@Begin\n"
   "@LP " _ \n \n
   "@End @Abstract\n")

(define-skeleton lout-insert-chapter "Insert a new @Chapter" nil
  (if (bolp) nil ?\n)
  '(setq v1 (skeleton-read "Chapter title: "))
  "@Chapter @Title {" v1 "}\n"
  "@Begin\n\n"
  "@LP " _ \n \n
  "@End @Chapter\n\n")

(define-skeleton lout-insert-section "Insert a new @Section" nil
  (if (bolp) nil ?\n)
  '(setq v1 (skeleton-read "Section title: "))
  "@Section @Title {" v1 "}" \n
  "@Begin" \n
  "@LP" _ \n \n
  "@End @Section" \n \n)

(define-skeleton lout-insert-subsection "Insert a new @SubSection" nil
  (if (bolp) nil ?\n)
  '(setq v1 (skeleton-read "Subsection title: "))
  "@SubSection @Title { " v1 " }\n"
  "@Begin\n"
  "@LP " _ \n \n
  "@End @SubSection\n")

(define-skeleton lout-insert-figure "Insert a @Figure" nil
  (if (bolp) nil ?\n)
  '(setq v1 (skeleton-read "Caption: "))
  "@Figure @Caption { " v1 " }\n"
  "{ " _ "}\n")

(define-skeleton lout-insert-item  "\n   @ListItem { " _ " }")

(define-skeleton lout-insert-table "Insert a table" nil
  '(if (bolp) nil ?\n)
  '(setq v1 (skeleton-read "Table caption: "))
   "@Tab @Caption { " v1 "}\n"
   "   @Fmta {@Col A ! @Col B}\n"
   "{\n"
   "   @Rowa A { " _ " } B { }\n"
   "   @Rowa A { } B { }\n"
   "}\n")

(defun lout-comment-paragraph ()
  "Comment out the current paragraph."
  (interactive "*")
  (save-excursion
    (mark-paragraph)
    (comment-region (region-beginning) (region-end))))

;; can't call lambda expressions from easymenu!
(defun lout-uncomment-region (beg end)
  (interactive "*r")
  (comment-region beg end -1))

(defun lout-submit-bug-report ()
  "Submit a bug report on lout-mode via email."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   "emarsden@mail.dotcom.fr"
   (concat "lout-mode.el " lout-version)
   ))

;; grrr
(GNU    (defun lout-mark-active () mark-active))
(XEmacs (defun lout-mark-active () (mark)))

;; snarfed from old auc-tex.el
(defun lout-insert-mode-line ()
  "Insert `# -*- lout -*-' at the top of your file if not present.
Could be used in your lout-mode-hook"
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (if (not (re-search-forward "-\\*-.*-\\*-" 100 t))
        (insert-string (concat "# -*- "
                               (substring (symbol-name major-mode) 0 -5)
                               " -*-\n")))))


(run-hooks 'lout-mode-load-hook)
(provide 'lout-mode)

;;; lout-mode.el ends here
