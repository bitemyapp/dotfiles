;;; w3m-om.el --- Mule 2 specific functions for emacs-w3m

;; Copyright (C) 2001, 2002, 2003, 2005
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Katsumi Yamaoka    <yamaoka@jpl.org>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains the stuffs to use emacs-w3m on Mule-2.  For more
;; detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/

;;; Code:

(eval-when-compile (require 'cl))

(require 'poe)
(require 'poem)
(require 'pcustom)
(require 'pccl)

(eval-when-compile
  (unless (fboundp 'custom-declare-variable)
    (defconst w3m-icon-directory nil
      "This definition is used to avoid byte-compile warnings.")))

;; Variables which will be defined in the other module.
(eval-when-compile
  (defvar w3m-mode-map)
  (defvar w3m-menubar)
  (defvar w3m-default-coding-system))

(eval-and-compile
  (if (locate-library "bitmap")
      (require 'w3m-bitmap)
    ;; Dummy functions.
    (defalias 'w3m-create-image 'ignore)
    (defalias 'w3m-create-resized-image 'ignore)
    (defalias 'w3m-insert-image 'ignore)
    (defalias 'w3m-remove-image 'ignore)
    (defalias 'w3m-image-type-available-p 'ignore)
    (defalias 'w3m-display-graphic-p 'ignore)
    (defalias 'w3m-display-inline-images-p 'ignore)))

;; Dummy functions.
(defalias 'w3m-setup-toolbar 'ignore)
(defalias 'w3m-update-toolbar 'ignore)
(defalias 'w3m-mule-unicode-p 'ignore)

;; Required for old Emacsen.  See the file README for details.
(eval-and-compile
  (autoload 'regexp-opt "regexp-opt"))

(defvar w3m-om-coding-system-alist
  '((alternativnyj	. *alternativnyj*)
    (big5		. *big5*)
    (binary		. *noconv*);; `binary' will be provided by APEL.
    (cn-gb-2312		. *euc-china*)
    (ctext		. *ctext*)
    (emacs-mule		. *internal*)
    (euc-china		. *euc-china*)
    (euc-japan		. *euc-japan*)
    (euc-korea		. *euc-korea*)
    (euc-kr		. *euc-kr*)
    (iso-2022-7bit	. *iso-2022-jp*)
    (iso-2022-7bit-ss2	. *iso-2022-ss2-7*)
    (iso-2022-jp	. *iso-2022-jp*)
    (iso-8859-1		. *iso-8859-1*)
    (iso-8859-2		. *iso-8859-2*)
    (iso-8859-3		. *iso-8859-3*)
    (iso-8859-4		. *iso-8859-4*)
    (iso-8859-5		. *iso-8859-5*)
    (iso-8859-7		. *iso-8859-7*)
    (iso-8859-8		. *iso-8859-8*)
    (iso-8859-9		. *iso-8859-9*)
    (junet		. *junet*)
    (koi8		. *koi8*)
    (shift_jis		. *sjis*)
    (tis-620		. *tis620*)
    (undecided		. *autoconv*)
    (viscii		. *viscii*)
    (vscii		. *vscii*))
  "*Alist of a modern coding-system and a traditional coding-system.
The car of each element will be provided as a new coding-system by
copying of the cdr of an element.  No need to contain the eol-type
variants in this alist.")

(eval-and-compile
  (defvar w3m-om-character-set-alist
    '((ascii			. lc-ascii)
      (latin-iso8859-1		. lc-ltn1)
      (latin-iso8859-2		. lc-ltn2)
      (latin-iso8859-3		. lc-ltn3)
      (latin-iso8859-4		. lc-ltn4)
      (thai-tis620		. lc-thai)
      (greek-iso8859-7		. lc-grk)
      (arabic-iso8859-6		. lc-arb)
      (hebrew-iso8859-8		. lc-hbw)
      (katakana-jisx0201	. lc-kana)
      (latin-jisx0201		. lc-roman)
      (cyrillic-iso8859-5	. lc-crl)
      (latin-iso8859-9		. lc-ltn5)
      (japanese-jisx0208-1978	. lc-jpold)
      (chinese-gb2312		. lc-cn)
      (japanese-jisx0208	. lc-jp)
      (korean-ksc5601		. lc-kr)
      (japanese-jisx0212	. lc-jp2)
      (chinese-cns11643-1	. lc-cns1)
      (chinese-cns11643-2	. lc-cns2)
      (chinese-big5-1		. lc-big5-1)
      (chinese-big5-2		. lc-big5-2))
    "*Alist of a modern character set and a symbol holding its
identification number."))

(defvar w3m-om-coding-category-alist
  '((alternativnyj	. *coding-category-iso-8-1*)
    (big5		. *coding-category-big5*)
    (binary		. *coding-category-bin*)
    (cn-gb-2312		. *coding-category-iso-8-2*)
    (ctext		. *coding-category-iso-8-1*)
    (emacs-mule		. *coding-category-internal*)
    (euc-china		. *coding-category-iso-8-2*)
    (euc-japan		. *coding-category-iso-8-2*)
    (euc-korea		. *coding-category-iso-8-2*)
    (euc-kr		. *coding-category-iso-8-2*)
    (iso-2022-7bit	. *coding-category-iso-7*)
    (iso-2022-7bit-ss2	. *coding-category-iso-else*)
    (iso-2022-jp	. *coding-category-iso-7*)
    (iso-8859-1		. *coding-category-iso-8-1*)
    (iso-8859-2		. *coding-category-iso-8-1*)
    (iso-8859-3		. *coding-category-iso-8-1*)
    (iso-8859-4		. *coding-category-iso-8-1*)
    (iso-8859-5		. *coding-category-iso-8-1*)
    (iso-8859-7		. *coding-category-iso-8-1*)
    (iso-8859-8		. *coding-category-iso-8-1*)
    (iso-8859-9		. *coding-category-iso-8-1*)
    (junet		. *coding-category-iso-7*)
    (koi8		. *coding-category-iso-8-1*)
    (shift_jis		. *coding-category-sjis*)
    (tis-620		. *coding-category-iso-8-1*)
    (undecided		. *coding-category-bin*)
    (viscii		. *coding-category-bin*)
    (vscii		. *coding-category-bin*))
  "*Alist of a modern coding-system and a traditional coding-category.")

(let ((id "(generated automatically by emacs-w3m)")
      to from info-vector document i)
  (dolist (elem w3m-om-coding-system-alist)
    (setq to (car elem)
	  from (cdr elem))
    (when (and (not (coding-system-p to))
	       (setq info-vector (copy-sequence (get-code from))))
      (setq document (aref info-vector 2))
      (aset info-vector 2 (if (and (stringp document)
				   (> (length document) 0))
			      (concat document "\n  " id)
			    id))
      (set to to)
      (put to 'coding-system info-vector)
      (put to 'post-read-conversion (get from 'post-read-conversion))
      (put to 'pre-write-conversion (get from 'pre-write-conversion))
      (when (vectorp (get from 'eol-type))
	(setq i 0)
	(dolist (variant (append
			  (put to 'eol-type
			       (vector (intern (format "%s-unix" to))
				       (intern (format "%s-dos" to))
				       (intern (format "%s-mac" to))))
			  nil))
	  (set variant variant)
	  (put variant 'coding-system to)
	  (put variant 'eol-type (setq i (1+ i))))))))


;; Functions to handle coding-system.
(unless (fboundp 'coding-system-list)
  (defalias 'coding-system-list
    (lambda (&optional base-only)
      "Return a list of all existing non-subsidiary coding systems.
The optional argument is ignored."
      (let ((codings nil))
	(mapatoms
	 (lambda (arg)
	   (if (and (or (vectorp (get arg 'coding-system))
			(vectorp (get arg 'eol-type)))
		    (null (get arg 'pre-write-conversion))
		    (null (get arg 'post-read-conversion)))
	       (setq codings (cons arg codings)))))
	codings))))

(defsubst w3m-find-coding-system (obj)
  "Return OBJ if it is a coding-system."
  (if (coding-system-p obj) obj))

(defalias 'w3m-make-ccl-coding-system 'make-ccl-coding-system)

(defun w3m-om-modernize-coding-system (coding-system)
  "Return a modern coding-system name of CODING-SYSTEM if it is available."
  (let ((base (and (coding-system-p coding-system)
		   (get-base-code coding-system)))
	name eol name-eol)
    (if base
	(if (setq name (car (rassq base w3m-om-coding-system-alist)))
	    (if (and (setq eol (get coding-system 'eol-type))
		     (integerp eol)
		     (coding-system-p
		      (setq name-eol
			    (intern (format "%s-%s"
					    name
					    (plist-get '(1 unix 2 dos 3 mac)
						       eol))))))
		name-eol
	      name)
	  coding-system)
      'binary)))

(defun w3m-detect-coding-region (start end &optional priority-list)
  "Detect coding system of the text in the region between START and END.
Return the first possible coding system.

PRIORITY-LIST is a list of coding systems ordered by priority."
  (let (category categories opriority x)
    (dolist (codesys priority-list)
      (setq category (cdr (assq codesys w3m-om-coding-category-alist)))
      (when (and category
		 (not (memq category categories)))
	(push category categories)))
    (when categories
      (setq opriority (sort (list '*coding-category-internal*
				  '*coding-category-sjis*
				  '*coding-category-iso-7*
				  '*coding-category-iso-8-1*
				  '*coding-category-iso-8-2*
				  '*coding-category-iso-else*
				  '*coding-category-big5*
				  '*coding-category-bin*)
			    'coding-priority<))
      (set-coding-priority (nreverse categories)))
    (prog2
	(setq x (code-detect-region start end))
	(w3m-om-modernize-coding-system (if (consp x)
					    (car x)
					  x))
      (when opriority
	(set-coding-priority opriority)))))

(eval-and-compile
  (unless (fboundp 'charset-id)
    (defalias 'charset-id
      (lambda (charset)
	"Return charset identification number of CHARSET."
	(symbol-value (cdr (assq charset w3m-om-character-set-alist)))))))

;; charset-id() is required in w3m-ccl.el.
(require 'w3m-ccl)


;;; Generic functions.
(defun w3m-expand-path-name (name &optional base)
  "Convert path string NAME to the canonicalized one."
  (with-temp-buffer
    (insert (if base
		(if (eq ?/ (aref base (1- (length base))))
		    base (concat base "/"))
	      "")
	    name)
    (let (p q path)
      (goto-char (point-min))
      (save-match-data
	(while (search-forward "/" nil t)
	  (setq p (match-beginning 0)
		q (match-end 0))
	  (if (search-forward "/" nil t)
	      (goto-char (match-beginning 0))
	    (goto-char (point-max)))
	  (setq path (buffer-substring q (point)))
	  (cond
	   ((string= path ".")
	    (delete-region q (if (eobp) (point) (match-end 0))))
	   ((string= path "..")
	    (setq q (point))
	    (when (search-backward "/" nil t)
	      (search-backward "/" nil t)
	      (delete-region (match-end 0) q)))
	   ((eq (length path) 0)
	    (unless (eobp) (delete-region p (point))))))
	(setq path (buffer-string)))
      (if (eq (length path) 0) "/" path))))


(eval-and-compile
  (unless (fboundp 'read-passwd)
    ;; This code is imported from subr.el of Emacs-20.7 and slightly modified.
    (defalias 'read-passwd
      (function
       (lambda (prompt &optional confirm default)
	 "Read a password, prompting with PROMPT.  Echo `.' for each character
typed.  End with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills
line.  Optional argument CONFIRM, if non-nil, then read it twice to
make sure. Optional DEFAULT is a default password to use instead of
empty input."
	 (if confirm
	     (let (success)
	       (while (not success)
		 (let ((first (read-passwd prompt nil default))
		       (second (read-passwd "Confirm password: " nil default)))
		   (if (equal first second)
		       (setq success first)
		     (message
		      "Password not repeated accurately; please start over")
		     (sit-for 1))))
	       success)
	   (let ((pass nil)
		 (c 0)
		 (echo-keystrokes 0)
		 (cursor-in-echo-area t)
		 (inhibit-input-event-recording t))
	     (while (progn (message "%s%s"
				    prompt
				    (make-string (length pass) ?.))
			   (setq c (read-char-exclusive))
			   (and (/= c ?\r) (/= c ?\n) (/= c ?\e)))
	       (if (= c ?\C-u)
		   (setq pass "")
		 (if (and (/= c ?\b) (/= c ?\177))
		     (setq pass (concat pass (char-to-string c)))
		   (if (> (length pass) 0)
		       (setq pass (substring pass 0 -1))))))
	     (message nil)
	     (or pass default ""))))))))

(let (current-load-list)
  (eval
   (` (defadvice read-string (after allow-4th-arg (prompt &optional
							  initial-input
							  history
							  default-value)
				    activate)
	"Advised by emacs-w3m.
Allow the optional fourth argument DEFAULT-VALUE which will be used as
the value to return if the user enters the empty string."
	(, (if (and (= emacs-major-version 19) (>= emacs-minor-version 29))
	       '(if (zerop (length ad-return-value))
		    (if (stringp default-value)
			(progn
			  (if history
			      (set history
				   (cons
				    default-value
				    (delete default-value
					    (delete ad-return-value
						    (symbol-value history))))))
			  (setq ad-return-value default-value))
		      (if history
			  (set history
			       (delete ad-return-value
				       (symbol-value history)))))
		  (if history
		      (set history
			   (cons ad-return-value
				 (delete ad-return-value
					 (symbol-value history))))))
	     '(if (and (zerop (length ad-return-value))
		       (stringp default-value))
		  (setq ad-return-value default-value))))))))

(unless (fboundp 'multibyte-string-p)
  (defalias 'multibyte-string-p 'stringp))

(eval-when-compile
  ;; Pickup `move-to-column-strictly'.
  (require 'rect))

(defun move-to-column-force (column)
  "Move point to column COLUMN rigidly in the current line.
If COLUMN is within a multi-column character, replace it by
spaces and tab."
  (inline (move-to-column-strictly column t)))

(unless (fboundp 'compose-mail)
  (defalias 'compose-mail
    (lambda (&optional to subject other-headers continue
		       switch-function yank-action send-actions)
      "Start composing a mail message to send."
      (interactive (list nil nil nil current-prefix-arg))
      (let ((function (get mail-user-agent 'composefunc)))
	(funcall function to subject other-headers continue
		 switch-function yank-action send-actions)))))

;;; Faces:
(defvar w3m-om-use-overstrike-to-make-face-bold 'w3m
  "*If non-nil, use `set-face-bold-p' to make faces bold by overstriking.
If it is the symbol `w3m', only 'w3m-' prefixed faces will be affected.")

(defadvice custom-declare-face (around set-face-bold-with-overstrike activate)
  "Advised by emacs-w3m.
Use `set-face-bold-p' to make faces bold by overstriking.  See also the
documentation for `w3m-om-use-overstrike-to-make-face-bold'."
  (if (if (eq w3m-om-use-overstrike-to-make-face-bold 'w3m)
	  (string-match "\\`w3m-" (symbol-name (ad-get-arg 0)))
	w3m-om-use-overstrike-to-make-face-bold)
      (let ((si:custom-set-face-bold (symbol-function 'custom-set-face-bold))
	    (si:custom-face-bold (symbol-function 'custom-face-bold)))
	(defalias 'custom-set-face-bold 'set-face-bold-p)
	(defalias 'custom-face-bold (lambda (face &rest args)
				      (face-bold-p face)))
	(unwind-protect
	    ad-do-it
	  (fset 'custom-set-face-bold si:custom-set-face-bold)
	  (fset 'custom-face-bold si:custom-face-bold)))
    ad-do-it))

;;; Widget:
(defun w3m-om-define-missing-widgets ()
  "Define some missing widget(s)."
  (unless (get 'other 'widget-type)
    ;; The following definition is imported from wid-edit.el of Emacs 20.7.
    (define-widget 'other 'sexp
      "Matches any value, but doesn't let the user edit the value.
This is useful as last item in a `choice' widget.
You should use this widget type with a default value,
as in (other DEFAULT) or (other :tag \"NAME\" DEFAULT).
If the user selects this alternative, that specifies DEFAULT
as the value."
      :tag "Other"
      :format "%t%n"
      :value 'other)))

(eval-after-load "wid-edit" '(w3m-om-define-missing-widgets))

(provide 'w3m-om)

;;; w3m-om.el ends here
