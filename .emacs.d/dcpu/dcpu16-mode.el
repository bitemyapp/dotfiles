;; Copyright 2012 Erik Swanson

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Written using the tutorial at:
;; renormalist.net/Renormmalist/EmacsLanguageModeCreationTutorial

;; Allow users to run their own code when dcpu16-mode is run
(defvar dcpu16-mode-hook nil)

;; Allow definition of keymaps
(defvar dcpu16-mode-map
  (let ((dcpu16-mode-map (make-keymap)))
    (define-key dcpu16-mode-map "\C-j" 'newline-and-indent)
    dcpu16-mode-map)
  "Keymap for DCPU16 major mode")

;; Syntax highlighting using keywords
;;
;; the font-lock-keywords variable is a list of keywords to highlight

;; The pattern used is to define highlighting rules is:
;;         (matcher . facename)
;; where matcher is a pattern and facename is the name that will be used
;; for actual highlighting

;; These regexps were not hand optimized, they were generated with:
;; (regexp-opt '("SET" "ADD" "SUB" "MUL" "DIV" "MOD" "SHL" "SHR" 
;; "AND" "BOR" "XOR" "IFE" "IFN" "IFG" "IFB" "JSR"))
;; (regexp-opt '("A" "B" "C" "X" "Y" "Z" "I" "J" "PC" "O" "POP" "PEEK" "PUSH"))
;;
;; They are then surrounded by \< and \> to make sure that they only
;; match when surrounded by a space or EOF

(defconst dcpu16-font-lock-keywords-1
  (list
   '("\\<A\\(?:[DN]D\\)\\|BOR\\|DIV\\|IF[BEGN]\\|JSR\\|M\\(?:OD\\|UL\\)\\|S\\(?:ET\\|H[LR]\\|UB\\)\\|XOR\\>" . font-lock-builtin-face)
   '("\\<P\\(?:C\\|EEK\\|OP\\|USH\\)\\|[ABCIJOXYZ]\\>" . font-lock-variable-name-face))
  "Highlighting expressions for DCPU16 Mode")

;; A second level of highlighting that we append to the first
(defconst dcpu16-font-lock-keywords-2
  (append dcpu16-font-lock-keywords-1
	  (list
	   '(":[a-z\\|A-Z\\|_]*" . font-lock-function-name-face)))
  "Highlight labels in DCPU16 Mode")


;; A third level of highlighting that we append to the second
(defconst dcpu16-font-lock-keywords-3
  (append dcpu16-font-lock-keywords-2
	  (list
	   '(";.*" . font-lock-comment-face)))
  "Highlight comments in DCPU16 mode")

;; Define the default level of highlighting
(defvar dcpu16-font-lock-keywords dcpu16-font-lock-keywords-3
  "Default highlighting expressions for DCPU16 mode")

;; Indentation Amount
;;
;; We want users of DCPU16 Mode to be able to customize the number
;; of spaces used for indentation.
(defvar indentation-spaces 8
  "*The number of spaces that an instruction will be indented.")

;; Indentation for DCPU16 code. We base indentation level off of 
;; the following rules:
;;
;; 1) If it is the first line, do not indent
;; 2) If the line is/starts with a lable, or comment, do not indent
;; 3) If the line is blank, do not indent
;; 4) Otherwise, indent 12 spaces
(defun dcpu16-indent-line ()
  "Indent current line as DCPU16 code"
  (interactive)
  (beginning-of-line)                ; set the point to the begining of the line
  (if (bobp)                         ; check for rule 1 (beginning of buffer)
      (indent-line-to 0)
    (if (looking-at "^[:;]")         ; check for rule 2
	(indent-line-to 0)
      (if (looking-at "^\\s-*\n")    ; check for rule 3
	  (indent-line-to 0)
	(if (looking-at "^[ \t]*\.") ; check for rule 4
	    (indent-line-to indentation-spaces))
    ))))


;; The Syntax Table
;;
;; We don't make many modifications to the syntax here, but we could.
;; The only change we do make is to make underscore characters a part of words.
(defvar dcpu16-mode-syntax-table
  (let ((dcpu16-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" dcpu16-mode-syntax-table)
    dcpu16-mode-syntax-table)
  "Syntax table for dcpu16-mode")

;; The Entry Function
;; This is the function that is called when we enter the major mode
;; It sets the syntax-table, mode-map, and keymap
;;
;; We also register our indent function so that emacs calls it whenever
;; it wants to indent.
;;
;; Finally, we set the major-mode variable, set the mode-name, and run hooks
(defun dcpu16-mode ()
  "Major mode for editing DCPU-16 Assembler files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dcpu16-mode-syntax-table)
  (use-local-map dcpu16-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(dcpu16-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'dcpu16-indent-line)
  (setq major-mode 'dcpu16-mode)
  (setq mode-name "DCPU-16")
  (run-hooks 'dcpu16-mode-hook))


;; The most important line:
(provide 'dcpu16-mode)