;;; w3m-bitmap.el --- Display bitmap image functions for w3m

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Taiki SUGAWARA  <taiki.s@cityfujisawa.ne.jp>
;;          Katsumi Yamaoka <yamaoka@jpl.org>
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module requires BITMAP-MULE package.  It can be downloaded from:
;;
;;    ftp://ftp.jpl.org/pub/elisp/bitmap/

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m-util)
(require 'w3m-proc)
(require 'w3m-image)
(require 'bitmap)

;;; Check for the broken facility:
(w3m-static-when (or (= emacs-major-version 19)
		     (and (= emacs-major-version 20)
			  (<= emacs-minor-version 2)))

  (defconst w3m-bitmap-emacs-broken-p
    ;; We use `eval' in case Emacs brokenness differs from compile-time
    ;; to load-time.
    (eval
     '(let* ((default-enable-multibyte-characters t)
	     (default-mc-flag t)
	     (buffer (get-buffer-create " *check-for-broken-facility*")))
	(prog1
	    (save-excursion
	      (set-buffer buffer)
	      (erase-buffer)
	      (insert (bitmap-compose "38c038c038084492926c00f808f008f0"))
	      (put-text-property (point-min) (point-max)
				 'check-for-broken-facility t)
	      (/= (if (fboundp 'ad-Orig-current-column)
		      (ad-Orig-current-column)
		    (current-column))
		  1))
	  (kill-buffer buffer))))
    "T means this Emacs has a bug on managing column numbers on bitmap
characters if there are any overlays or text properties.  If you are
using Mule 2.3 based on Emacs 19.34 and the value for this constant
is t, we strongly recommend you fix the bug and rebuild Mule.  See
manuals in the emacs-w3m distribution.")

  (when w3m-bitmap-emacs-broken-p
    ;; Not to get the byte-code for `current-column' inlined in case
    ;; when compiling manually.
    (put 'current-column 'byte-compile nil)

    (if noninteractive
	(message "%s%s%s"
		 "BROKEN FACILITY DETECTED: "
		 (if (boundp 'MULE)
		     "Mule"
		   "Emacs")
		 " won't manage columns on bitmap chars with props.")
      (let ((buffer (get-buffer-create "*notification@emacs-w3m*")))
	(unwind-protect
	    (save-window-excursion
	      (delete-other-windows)
	      (switch-to-buffer buffer)
	      (setq buffer-read-only nil)
	      (erase-buffer)
	      (insert "BROKEN FACILITY DETECTED:\n\nThis "
		      (if (boundp 'MULE)
			  "Mule"
			"Emacs")
		      "\
 has a bug on managing column numbers on bitmap characters if
there are any overlays or text properties."
		      (if (boundp 'MULE)
			  "  The emacs-w3m development
team strongly recommend you apply the patch and rebuild Mule.  The
patch named mule-2.3@19.34.patch is included in patches/ directory in
the emacs-w3m distribution."
			"")
		      "\n\nPress any key to continue: ")
	      (set-buffer-modified-p nil)
	      (read-char))
	  (with-current-buffer buffer
	    (beginning-of-line 0)
	    (delete-region (point) (point-max))
	    (set-buffer-modified-p nil))
	  (bury-buffer buffer)))))

  (defun w3m-bitmap-current-column ()
    "Like `current-column', except that works with byte-indexed bitmap
chars as well."
    (let ((home (point))
	  (cols 0))
      (while (not (bolp))
	(forward-char -1)
	(setq cols (+ cols (char-width (following-char)))))
      (goto-char home)
      cols))

  (defun w3m-bitmap-move-to-column (column &optional force strictly)
    "Like `move-to-column', except that works with byte-indexed bitmap
chars as well."
    (beginning-of-line)
    (let ((cols 0)
	  width)
      (if (wholenump column)
	  (progn
	    (while (and (not (eolp))
			(< cols column))
	      (setq width (char-width (following-char))
		    cols (+ cols width))
	      (forward-char 1))
	    (if force
		(cond ((> cols column)
		       (if strictly
			   (progn
			     (delete-backward-char 1)
			     (insert-char ?\  width)
			     (forward-char (- column cols))
			     column)
			 cols))
		      ((< cols column)
		       (insert-char ?\  (- column cols))
		       column)
		      (t
		       column))
	      cols))
	(signal 'wrong-type-argument (list 'wholenump column)))))

  (defun w3m-bitmap-move-to-column-force (column)
    "Like `move-to-column-force', except that works with byte-indexed
bitmap chars as well."
    (inline (w3m-bitmap-move-to-column column t t)))

  (defun w3m-bitmap-next-line (arg)
    "Simple emulation to `next-line' to work with byte-indexed bitmap
chars."
    (interactive "p")
    (unless (memq last-command '(next-line previous-line))
      (setq temporary-goal-column (w3m-bitmap-current-column)))
    (forward-line arg)
    (w3m-bitmap-move-to-column temporary-goal-column))

  (defun w3m-bitmap-previous-line (arg)
    "Simple emulation to `previous-line' to work with byte-indexed bitmap
chars."
    (interactive "p")
    (w3m-bitmap-next-line (- arg)))

  (require 'advice)

  (defmacro w3m-bitmap-defadvice-if-broken (function &optional linewise)
    "Perform `defadvice' to FUNCTION to attempt to make it work correctly
even if there is a bug in Emacs on managing column numbers on bitmap
characters.  If FUNCTION will work within a line, set LINEWISE to t."
    (let ((replacement (intern (format "w3m-bitmap-%s" function)))
	  arglist)
      (when (and (fboundp function)
		 (fboundp replacement))
	(setq arglist (ad-arglist (symbol-function replacement)))
	`(let (current-load-list)
	   (defadvice ,function
	     ,(if arglist
		  (list 'around 'make-it-work-with-bitmap-chars arglist
			'activate)
		'(around make-it-work-with-bitmap-chars activate))
	     "Advised by emacs-w3m.
Attempt to make the function work correctly even if Emacs has a bug on
managing column numbers on bitmap characters."
	     (if ,(if linewise
		      '(let ((bol (line-beginning-position))
			     (eol (line-end-position)))
			 (or (overlays-in bol eol)
			     (text-properties-at (point))
			     (next-property-change (point) nil eol)
			     (previous-property-change (point) nil bol)))
		    '(or (overlays-in (point-min) (point-max))
			 (next-property-change (point))
			 (previous-property-change (point))))
		 (setq ad-return-value
		       ,(cons replacement
			      (delq '&optional
				    (delq '&rest
					  (copy-sequence arglist)))))
	       ad-do-it))))))

  (when w3m-bitmap-emacs-broken-p
    (w3m-bitmap-defadvice-if-broken current-column t)
    (w3m-bitmap-defadvice-if-broken move-to-column t)
    (w3m-bitmap-defadvice-if-broken move-to-column-force t)
    (w3m-bitmap-defadvice-if-broken next-line)
    (w3m-bitmap-defadvice-if-broken previous-line)))

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-display-inline-images)
  (defvar w3m-mode-map)
  (defvar w3m-work-buffer-list)
  (defvar w3m-work-buffer-name)
  (autoload 'w3m-retrieve "w3m")
  (autoload 'move-to-column-force "w3m-om"))

(defface w3m-bitmap-image-face
  '((((background light))
     (:foreground "Black"))
    (t
     (:background "White") (:foreground "Black")))
  "Face used to highlight bitmap images.  Note that it won't be used if
`w3m-bitmap-image-face-inherit' is non-nil."
  :group 'w3m-face)

(defcustom w3m-bitmap-convert-arguments nil
  "*List of the additional arguments passed to the convert program to
convert from any image to xbm."
  :group 'w3m
  :type '(repeat (string :format "Argument: %v\n" :size 0)))

(defcustom w3m-bitmap-image-face-inherit t
  "*If non-nil, inherit image face from the original face.
It means that the face `w3m-bitmap-image-face' won't be used to
highlight bitmap images."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-bitmap-cache-image-hook nil
  "*Hook run with use cached image."
  :group 'w3m
  :type 'hook)

;;; Bitmap image functions.
(defvar w3m-bitmap-image-cache-alist nil)
(defvar w3m-bitmap-image-use-cache t
  "*If non-nil, bitmap-image is cached to this alist")

(defun w3m-bitmap-image-buffer (buffer)
  "Create bitmap-image from BUFFER."
  (let* ((cmp (bitmap-decode-xbm (bitmap-read-xbm-buffer buffer)))
	 (len (length cmp))
	 (i 0)
	 list)
    (while (< i len)
      (push (bitmap-compose (aref cmp i)) list)
      (setq i (1+ i)))
    (nreverse list)))

(defun w3m-bitmap-image-get-overlay (pos)
  "Return an overlay in and around POS, which has the `bitmap-image'
property ."
  (let ((home (point))
	ovrs ovr)
    (goto-char pos)
    (setq ovrs (overlays-in (line-beginning-position) (line-end-position)))
    (while (and (not ovr)
		ovrs)
      (if (overlay-get (car ovrs) 'w3m-bitmap-image-line)
	  (setq ovr (car ovrs))
	(setq ovrs (cdr ovrs))))
    (goto-char home)
    ovr))

(defun w3m-bitmap-image-insert (pos image &optional props ovr)
  "Insert IMAGE to POS.  IMAGE should be a list of bitmap image lines or
a non-list text.  PROPS specifies properties for bitmap images.  OVR
is an overlay which covers the area for bitmap images.  If OVR is nil,
a new overlay will be created and returned."
  (save-excursion
    (when (markerp pos)
      (setq pos (marker-position pos)))
    (goto-char pos)
    (let ((ovrbeg (line-beginning-position))
	  (col (current-column))
	  indent-tabs-mode end-col)
      (unless ovr
	(setq ovr (make-overlay ovrbeg ovrbeg))
	(overlay-put ovr 'w3m-bitmap-image-line t)
	(overlay-put ovr 'w3m-bitmap-image-count 0)
	(w3m-static-when (= emacs-major-version 20)
	  ;; Make the overlay transparent to the face text property.
	  (overlay-put ovr 'face '((:background) (:foreground)))))
      (if (consp image)
	  (progn
	    (insert (car image))
	    (setq image (cdr image))
	    (overlay-put ovr 'w3m-bitmap-image-count
			 (1+ (overlay-get ovr 'w3m-bitmap-image-count))))
	(insert image)
	(setq image nil))
      (when props
	(w3m-add-text-properties pos (point) props))
      (setq end-col (current-column))
      (forward-line)
      (while (or image (< (point) (overlay-end ovr)))
	(when (>= (point) (overlay-end ovr))
	  (beginning-of-line)
	  (insert "\n")
	  (forward-line -1))
	(move-to-column-force col)
	(if image
	    (progn
	      (setq pos (point))
	      (insert (car image))
	      (when props
		(w3m-add-text-properties pos (point) props)))
	  (indent-to-column end-col))
	(setq image (cdr image))
	(forward-line))
      (move-overlay ovr (min ovrbeg (overlay-start ovr))
		    (1- (point)))
      (overlay-put ovr 'evaporate t)
      ovr)))

(defun w3m-bitmap-image-delete-internal (pos ovr &optional width)
  (save-excursion
    (goto-char pos)
    (let (col eol)
      (if ovr
	  (progn
	    (overlay-put ovr 'evaporate nil)
	    (setq col (current-column))
	    (while (< (point) (overlay-end ovr))
	      (setq eol (line-end-position))
	      (move-to-column-force col)
	      (delete-region (point)
			     (if width
				 (min (+ (point) width) eol)
			       (or (text-property-not-all (point) eol
							  'w3m-bitmap-image t)
				   eol)))
	      (if (and (bolp)
		       (eolp)
		       (> (point) pos))
		  (delete-char 1)
		(forward-line))))
	(setq eol (line-end-position))
	(delete-region pos (if width
			       (min (+ pos width) eol)
			     (or (text-property-not-all pos eol
							'w3m-bitmap-image t)
				 eol)))))))

(defun w3m-bitmap-image-delete (pos ovr)
  "Delete bitmap-image on POS."
  (when ovr
    (let ((cnt (1- (overlay-get ovr 'w3m-bitmap-image-count))))
      (overlay-put ovr 'w3m-bitmap-image-count cnt)
      (w3m-bitmap-image-delete-internal pos ovr)
      (when (zerop cnt)
	(save-excursion
	  (goto-char (min (point) (overlay-start ovr)))
	  (forward-line)
	  (when (< (point) (overlay-end ovr))
	    (delete-region (point) (1+ (overlay-end ovr)))))))))

;;; Handle images:

;; Function which returns non-nil when the current display device can
;; show images inline.
(defun w3m-display-graphic-p ()
  window-system)

(defun w3m-display-inline-images-p ()
  "Returns non-nil when images can be displayed under the present
circumstances."
  (and w3m-display-inline-images (w3m-display-graphic-p)))

(defun w3m-create-image (url &optional no-cache referer size handler)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used.
If second optional argument REFERER is non-nil, it is used as Referer: field."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-create-image url no-cache referer handler))
    (if (and w3m-bitmap-image-use-cache
	     (assoc (if (and w3m-resize-images
			     (and (consp size)(car size)(cdr size)))
			(list url size)
		      url)
		    w3m-bitmap-image-cache-alist))
	(prog1
	    (cdr (assoc (if (and w3m-resize-images
				 (consp size)(car size)(cdr size))
			    (list url size)
			  url)
			w3m-bitmap-image-cache-alist))
	  (run-hook-with-args 'w3m-bitmap-cache-image-hook url))
      (w3m-process-do-with-temp-buffer
	  (type (w3m-retrieve url nil no-cache nil referer))
	(ignore-errors
	  (when (and (stringp type) (string-match "^image/" type))
	    (setq type (replace-match "" nil nil type))
	    (lexical-let ((url url)
			  (size size)
			  (data (buffer-string))
			  set-size)
	      (if (and w3m-resize-images
		       (consp size)(car size)(cdr size))
		  (setq set-size t))
	      (w3m-process-do-with-temp-buffer
		  (success (progn
			     (unless (boundp 'MULE)
			       (set-buffer-multibyte nil))
			     (insert data)
			     (apply 'w3m-imagick-start-convert-buffer
				    handler type "xbm"
				    (append
				     (if set-size
					 (list "-geometry"
					       (concat (number-to-string
							(car size))
						       "x"
						       (number-to-string
							(cdr size)) "!")))
				     w3m-bitmap-convert-arguments))))
		(when success
		  (let ((image (w3m-bitmap-image-buffer (current-buffer))))
		    (push (cons (if set-size (list url size) url)
				image) w3m-bitmap-image-cache-alist)
		    image))))))))))

(defun w3m-create-resized-image (url rate &optional referer size handler)
  "Resize an cached image object.
URL is the image file's url.
RATE is resize percentage.
If REFERER is non-nil, it is used as Referer: field.
If SIZE is non-nil, its car element is used as width
and its cdr element is used as height."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-create-image url nil referer size handler))
    (lexical-let ((url url)
		  (rate rate)
		  fmt data)
      (w3m-process-do-with-temp-buffer
	  (type (w3m-retrieve url 'raw nil nil referer handler))
	(when (and (stringp type) (string-match "^image/" type))
	  (setq fmt (replace-match "" nil nil type)
		data (buffer-string))
	  (w3m-process-do
	      (resized (w3m-resize-image-by-rate data rate handler))
	    (when resized
	      (w3m-process-do-with-temp-buffer
		  (success (progn
			     (w3m-static-if (boundp 'MULE)
				 (setq mc-flag nil)
			       (set-buffer-multibyte nil))
			     (insert resized)
			     (apply 'w3m-imagick-start-convert-buffer
				    handler fmt "xbm"
				    w3m-bitmap-convert-arguments)))
		(when success
		  (w3m-static-if (boundp 'MULE)
		      (setq mc-flag t)
		    (set-buffer-multibyte t))
		  (w3m-bitmap-image-buffer (current-buffer)))))))))))

(defun w3m-insert-image (beg end image url)
  "Display image on the current buffer.
Buffer string between BEG and END are replaced with IMAGE."
  (let* ((properties (text-properties-at beg))
	 (name (buffer-substring beg end))
	 (ovr (w3m-bitmap-image-get-overlay beg))
	 (face (and w3m-bitmap-image-face-inherit
		    (nth 1 (memq 'face properties)))))
    (when (equal (nth 1 (memq 'w3m-image properties)) url)
      (w3m-bitmap-image-delete-internal beg ovr (- end beg))
      (w3m-bitmap-image-insert beg image
			       (w3m-modify-plist properties
						 'w3m-image-status 'on
						 'w3m-bitmap-image t
						 'w3m-image-name name
						 'face
						 (or face 'w3m-bitmap-image-face))
			       ovr))))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END.
\(Note: END will be ignored in this version of `w3m-remove-image'.)"
  (let ((name (get-text-property beg 'w3m-image-name))
	(ovr (w3m-bitmap-image-get-overlay beg)))
    (when name
      (w3m-bitmap-image-delete beg ovr)
      (w3m-bitmap-image-insert beg name nil ovr)
      (+ beg (length name)))))

(defun w3m-image-type-available-p (image-type)
  w3m-imagick-convert-program)

(provide 'w3m-bitmap)

;; w3m-bitmap.el ends here
