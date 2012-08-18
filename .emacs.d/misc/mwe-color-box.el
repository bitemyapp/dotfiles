;;; mwe-color-box.el --- display color boxes for each nesting level

;; Copyright (C) 2004, 2007  Free Software Foundation, Inc.

;; Author: Michael Weber <michaelw@foldr.org>
;; Keywords: faces, games
;; Initial-Version: <2004-11-07 22:05:07 michaelw>
;; Time-stamp: <2007-03-17 11:48:06 michaelw>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Color-boxify current buffer with M-x mwe:color-box-buffer
;; Sit back.  Enjoy. :)

;; Notes: 
;;   * Buffer is made read-only, so that editing is not possible
;;
;; Inspired by http://www.32768.com/bill/weblog/000660.shtml#000660

;;
;; As an example, uncomment FACTORIAL, mark it, and
;; use M-x `mwe:color-box-region/miscbill'.
;;
;; To get the same colors as on the webpage, eval:
;; (custom-set-faces
;;  '(mwe:nesting-face-0 ((((class color)) (:background "#90b0f0"))))
;;  '(mwe:nesting-face-1 ((((class color)) (:background "#b090f0"))))
;;  '(mwe:nesting-face-2 ((((class color)) (:background "#f0b090"))))
;;  '(mwe:nesting-face-3 ((((class color)) (:background "#90b0f0"))))
;;  '(mwe:nesting-face-4 ((((class color)) (:background "#90f0b0"))))
;;  '(mwe:nesting-face-5 ((((class color)) (:background "#b0f090"))))
;;  '(mwe:nesting-face-6 ((((class color)) (:background "#b090f0"))))
;;  '(mwe:nesting-face-7 ((((class color)) (:background "#90b0f0"))))
;;  '(mwe:nesting-face-8 ((((class color)) (:background "#b0f090")))))

;; (DEFUN FACTORIAL
;;   (X)
;;   (COND
;;    (
;;     (EQ X 1)
;;     1)
;;    (T
;;      (* X
;;         (FACTORIAL
;;          (- X 1)
;;          )
;;         )
;;      )
;;    )
;;   )

;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'rect)
  (require 'thingatpt))

(defvar *mwe:color-box-face-list*
  '(mwe:nesting-face-0 mwe:nesting-face-1
    mwe:nesting-face-2 mwe:nesting-face-3
    mwe:nesting-face-4 mwe:nesting-face-5
    mwe:nesting-face-6 mwe:nesting-face-7
    mwe:nesting-face-8 mwe:nesting-face-9)
  "Faces used for color boxes.
See `mwe:color-box-region'.")

;;; color boxes
(defun mwe:color-box-color (depth)
  "Determines color of color box at nesting depth DEPTH."
  (nth (mod depth (length *mwe:color-box-face-list*))
       *mwe:color-box-face-list*))

;;;###autoload
(defun mwe:color-box-region (beg end &optional rmargin reg-tok-fn)
  "Create nested color boxes for region BEG to END.
If positive number, RMARGIN sets right margin of color boxes to column RMARGIN.
If Non-nil, REG-TOK-FN sets the tokenizer.  If nil, uses `mwe:tokenize-region'.

Calls `mwe:color-box-color' with argument DEPTH to pick color."
  (interactive "*r")
  (let ((buf (current-buffer)))
    (with-output-to-temp-buffer (buffer-name
                                 (get-buffer-create "*Color Boxified*"))
      (with-current-buffer standard-output
        (lisp-mode)
        (insert-buffer-substring buf beg end)
        (setq reg-tok-fn (or reg-tok-fn #'mwe:tokenize-region))
        (let ((indent-tabs-mode nil))
          (save-match-data
            (loop
             for (type depth ov . ignore) in (funcall reg-tok-fn beg end)
             for beg = (overlay-start ov)
             for end = (overlay-end ov)
             for maxcol = (if (natnump rmargin) rmargin
                            (mwe:region-max-column beg end))

             do (case type
                  ((sexp)
                   (mwe:rectangle-put-properties (overlay-start ov)
                                                 (overlay-end ov)
                                                 (if rmargin (- maxcol depth) maxcol)
                                                 'face (mwe:color-box-color depth))))
             do (delete-overlay ov))
            (toggle-read-only 1)))))))

;;;###autoload
(defun mwe:color-box-buffer (&optional buf)
  "Create nested color boxes for buffer.
See also `mwe:color-box-region'."
  (interactive "*")
  (with-current-buffer (or buf (current-buffer))
    (let ((*mwe:region-tokenizer* #'mwe:slist-tokenizer))
      (mwe:color-box-region (point-min) (point-max)))))

;;; property helpers

(defvar *mwe:color-box-overlays* ()
  "List of active color box overlays.
See `mwe:color-box-region'.")
(make-variable-buffer-local '*mwe:color-box-overlays*)

(defun mwe:line-put-properties (startcol endcol &rest props)
  "Sets properties PROPS for current line.
Start and end columns are given by STARTCOL and ENDCOL.
If ENDCOL exceeds current line length, whitespace is added up to ENDCOL."
  (move-to-column startcol t)
  (let ((ov (make-overlay (point)
			  (progn (move-to-column endcol t) (point))
			  (current-buffer)
			  t nil)))
    (push ov *mwe:color-box-overlays*)
    (apply 'overlay-put ov props)))

(defun mwe:rectangle-put-properties (beg end maxcol &rest props)
  "Sets properties PROPS for rectangle.
Rectangle is given by points BEG and END.  Right margin is at column MAXCOL."
  (save-excursion
    (let* ((end (if (> maxcol 0)
		    (progn (goto-char end)
			   (move-to-column maxcol t)
			   (point))
		    end)))
      (apply 'apply-on-rectangle 'mwe:line-put-properties beg end props))))

(defun mwe:region-max-column (beg end &optional trailing-space-p)
  "Returns maximum line width of region BEG to END.
If TRAILING-SPACE-P is nil (default), ignore whitespace at end of lines."
  (interactive "r")
  (save-excursion
    (save-restriction
      (goto-char beg)
      (narrow-to-region (line-beginning-position) end)
      (let ((maxcol 0))
	(while (not (eobp))
	  (end-of-line)
	  (unless trailing-space-p
	    (skip-chars-backward "\t "))
	  (when (< maxcol (current-column))
	    (setq maxcol (current-column)))
	  (forward-line))
	(when (interactive-p)
	  (message "maximum column in region: %d" maxcol))
	maxcol))))

;;; tokenizing

(defvar *mwe:region-tokenizer* #'mwe:slist-tokenizer
  "Refers to function used for tokenizing.
Should be bound locally before using function `mwe:tokenize-region'.")

(defun mwe:tokenize-region (beg end)
  "Tokenize region BEG to END.
First prepares region, then calls function referred to in
variable `*mwe:region-tokenizer*'."
  (save-restriction
    (goto-char beg)
    (narrow-to-region (line-beginning-position) end)
    (let ((maxcol (mwe:region-max-column beg end)))
      (apply-on-rectangle (lambda (scol ecol maxcol)
			    (move-to-column maxcol t))
			  (point-min)
			  (point-max)
			  maxcol))
    (untabify (point-min) (point-max))
    (funcall *mwe:region-tokenizer*)))

;;; s-expression tokenizer

(defun mwe:skip-whitespace ()
  "Skip over whitespace and comments."
  (interactive)
  (while (forward-comment +1)))

(defun mwe:make-sexp-token (beg end type depth &optional slist)
  "Make sexp tokens.
Arguments are:
BEG    point where token start
END    point where token ends
TYPE   SEXP or ATOM
DEPTH  nesting depth
SLIST  list of sub-token where current sexp is built from
       (optional, depending on token type)"
  (flet ((mwe:make-overlay (beg end)
           (make-overlay beg end (current-buffer) nil t)))
    (cons (list type depth (mwe:make-overlay beg end)) slist)))

(defadvice mwe:make-sexp-token (before mwe:make-sexp-token-hide-parens
				       (beg end type depth &optional slist)
				       activate compile)
  (when (and slist mwe:color-box-hide-parens)
    (when (eq ?\( (char-after beg))
      (put-text-property beg (1+ beg) 'invisible 'color-box-mode))
    (when (eq ?\) (char-before end))
      (put-text-property (1- end) end 'invisible 'color-box-mode))))

(defun mwe:sexp-tokenizer (&optional depth)
  "S-expression tokenizer.
DEPTH is current nesting level.
Returns list of tokens.

Tokens are built via calls to `mwe:make-sexp-token'.  Arguments are:
1. BEG    point where token start
2. END    point where token ends
3. TYPE   SEXP or ATOM
4. DEPTH  nesting depth
5. SLIST  list of sub-token where current sexp is built from
          (may be nil)
"
  (setq depth (or depth 0))
  (mwe:skip-whitespace)
  (cond ((looking-at "(")
	 (let* ((point (prog1 (point) (forward-char)))
		(toks (mwe:slist-tokenizer (1+ depth)))
		(epoint (progn (end-of-sexp) (point))))
	   (mwe:make-sexp-token point epoint 'sexp depth toks)))

	((looking-at "['`]")
	 (let* ((point (prog1 (point) (forward-char)))
		(toks (mwe:sexp-tokenizer (1+ depth)))
		(epoint (point)))
	   (mwe:make-sexp-token point epoint 'sexp depth toks)))

	(t (let* ((point (point))
		  (epoint (progn (end-of-sexp) (point))))
	     (when (< point epoint)
	       (mwe:make-sexp-token point epoint 'atom depth))))))

(defun mwe:slist-tokenizer (&optional depth)
  "S-list tokenizer.
DEPTH is current nesting level.

See also `mwe:sexp-tokenizer'."
  (setq depth (or depth 0))
  (mwe:skip-whitespace)
  (loop until (or (looking-at ")") (eobp))
	nconc (prog1 (mwe:sexp-tokenizer depth)
		(mwe:skip-whitespace))))


;;; Miscellaneous Bill's rendering theme

;;;###autoload
(defun mwe:color-box-region/miscbill (beg end &optional rmargin)
  (interactive "*r")
  (let ((*mwe:color-box-colors/miscbill* (copy-list *mwe:color-box-face-list*)))
    (nconc *mwe:color-box-colors/miscbill* *mwe:color-box-colors/miscbill*)
    (flet ((mwe:color-box-color (depth)
              (pop *mwe:color-box-colors/miscbill*)))
      (mwe:color-box-region beg end (case rmargin
					((0) nil)
					((nil) 30)
					(t rmargin))))))

;;; faces

(defgroup mwe:color-box '((mwe:nesting-faces custom-group))
  "Color boxes."
  :group 'editing)

(defcustom mwe:color-box-hide-parens t
  "Hide parentheses when activating color boxes."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil))
  :group 'mwe:color-box)

(defgroup mwe:nesting-faces ()
  "Nesting level faces."
  :group 'faces)

(defface mwe:nesting-face-0
    '((((class color))
       (:background "gray2")))
  "Face for displaying nesting-level 0."
  :group 'mwe:nesting-faces)

(defface mwe:nesting-face-1
    '((((class color))
       (:background "gray10")))
  "Face for displaying nesting-level 1."
  :group 'mwe:nesting-faces)

(defface mwe:nesting-face-2
    '((((class color))
       (:background "gray17")))
  "Face for displaying nesting-level 2."
  :group 'mwe:nesting-faces)

(defface mwe:nesting-face-3
    '((((class color))
       (:background "gray25")))
  "Face for displaying nesting-level 3."
  :group 'mwe:nesting-faces)

(defface mwe:nesting-face-4
    '((((class color))
       (:background "gray32")))
  "Face for displaying nesting-level 4."
  :group 'mwe:nesting-faces)

(defface mwe:nesting-face-5
    '((((class color))
       (:background "gray40")))
  "Face for displaying nesting-level 5."
  :group 'mwe:nesting-faces)

(defface mwe:nesting-face-6
    '((((class color))
       (:background "gray47")))
  "Face for displaying nesting-level 6."
  :group 'mwe:nesting-faces)

(defface mwe:nesting-face-7
    '((((class color))
       (:background "gray52")))
  "Face for displaying nesting-level 7."
  :group 'mwe:nesting-faces)

(defface mwe:nesting-face-8
    '((((class color))
       (:background "gray60")))
  "Face for displaying nesting-level 8."
  :group 'mwe:nesting-faces)

(defface mwe:nesting-face-9
    '((((class color))
       (:background "gray67")))
  "Face for displaying nesting-level 9."
  :group 'mwe:nesting-faces)

(provide 'mwe-color-box)
;;; mwe-color-box.el ends here
