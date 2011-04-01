;;; rudel-overlay.el --- Overlay functions for Rudel
;;
;; Copyright (C) 2008, 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, overlay
;; X-RCS: $Id:$
;;
;; This file is part of Rudel.
;;
;; Rudel is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Rudel is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'custom)

(eval-when-compile
  (require 'cl))

(require 'eieio)


;;; Rudel overlay faces
;;

(defcustom rudel-overlay-author-display t
  "Indicate authorship by setting text color to user color."
  :group   'rudel
  :type    'boolean
  :set     (lambda (symbol value)
	     (set-default symbol value)
	     (when (featurep 'rudel-overlay)
	       (rudel-overlay-options-changed))))

(put 'rudel-overlay-author-display 'safe-local-variable t)

(defface rudel-author-overlay-face
  '((default (:background "black")))
  "Face used to highlight contributions according to their authors.
Attributes involving color are not applied literally. Instead the
color is replaced with the color associated with the respective
author."
  :group 'rudel)


;;; General overlay functions
;;

(defun rudel-overlay-p (overlay)
  "Non-nil if OVERLAY is a Rudel overlay."
  (overlay-get overlay :rudel))

(defun rudel-overlay-length (overlay)
  "Distance between end and start of OVERLAY."
  (- (overlay-end overlay) (overlay-start overlay)))

(defun rudel-overlay-user (overlay)
  "User object associated to OVERLAY."
  (overlay-get overlay :user))

(defun rudel-overlays (&optional predicate)
  "Return a list of Rudel-related overlays or overlays satisfying PREDICATE.
If PREDICATE is non-nil returned overlays satisfy PREDICATES;
Otherwise all Rudel-related overlays are returned."
  (unless predicate
    (setq predicate #'rudel-overlay-p))

  (let* ((overlay-lists (overlay-lists))
	 (overlays      (append (car overlay-lists)
				(cdr overlay-lists))))
    (remove-if-not predicate overlays))
  )

(defun rudel-overlays-at (position &optional predicate)
  "Return a list of Rudel-related overlays at POSITION.
If PREDICATE is non-nil returned overlays satisfy PREDICATES;
Otherwise all Rudel-related overlays are returned."
  (unless predicate
    (setq predicate #'rudel-overlay-p))
  (remove-if-not predicate (overlays-at position)))

(defun rudel-overlays-in (start end &optional predicate)
  "Return a list of Rudel-related overlays in the range START to END.
If PREDICATE is non-nil returned overlays satisfy PREDICATES;
Otherwise all Rudel-related overlays are returned."
  (unless predicate
    (setq predicate #'rudel-overlay-p))
  (remove-if-not predicate (overlays-in start end)))

(defun rudel-overlays-remove-all ()
  "Remove all Rudel overlays from the current buffer."
  (mapc #'delete-overlay (rudel-overlays)))


;;; Author overlay
;;

(defun rudel-author-overlay-p (overlay)
  "Predicate for author overlays."
  (eq (overlay-get overlay :rudel) 'author))

(defun rudel-author-overlays ()
  "Return the list of author overlays in the current buffer."
  (rudel-overlays #'rudel-author-overlay-p))

(defun rudel-author-overlay-at (position &optional author)
  ""
  (let ((overlays (rudel-overlays-at
		   position #'rudel-author-overlay-p)))
    ;; There can only be one rudel overlay at any given position
    (when overlays
      (when (or (not author)
		(eq (rudel-overlay-user (car overlays)) author))
	(car overlays))))
  )

(defun rudel-author-overlays-in (start end &optional author)
  ""
  (rudel-overlays-in
   start end
   (lambda (overlay)
     (and (rudel-overlay-p overlay)
	  (or (not author)
	      (eq (rudel-overlay-user overlay) author)))))
  )

(defun rudel-make-author-overlay (buffer from to author)
  "Make and return an overlay for the range FROM to TO in BUFFER suitable for contributions by AUTHOR.
AUTHOR has to be an object of type rudel-user-child."
  (let ((overlay (make-overlay from to buffer t)))
    (rudel-overlay-author-set-properties overlay author)
    overlay))

(defun rudel-overlay-author-set-properties (overlay author)
  "Set properties of OVERLAY according to slots of AUTHOR.
AUTHOR has to be an object of type rudel-user-child."
  (with-slots ((name :object-name) color) author
    (overlay-put overlay :rudel     'author)
    (overlay-put overlay :user      author)
    (overlay-put overlay 'face      (when rudel-overlay-author-display
				      (rudel-overlay-make-face
				       (rudel-overlay-make-face-symbol
					'author name)
				       'rudel-author-overlay-face
				       color)))
    (overlay-put overlay 'help-echo (when rudel-overlay-author-display
				      (format "Written by %s" name))))
  )

(defun rudel-overlay-author-update (overlay)
  "Update properties of OVERLAY from its attached user object."
  (let ((author (rudel-overlay-user overlay)))
    (rudel-overlay-author-set-properties overlay author)))


;;; Update functions for author overlays
;;

(defun rudel-update-author-overlay-after-insert (buffer position length author)
  "Update author overlays in BUFFER to incorporate an insertion of length LENGTH at POSITION by AUTHOR.
POSITION refers to an Emacs buffer position.
AUTHOR has to be an object of type rudel-author-child."
  (when author
    (with-current-buffer buffer
      (let* ((end    (+ position length))
	     (before (when (> position 1)
		       (rudel-author-overlay-at (- position 1) author)))
	     (at     (rudel-author-overlay-at position))
	     (after  (when (< end (point-max))
		       (rudel-author-overlay-at (+ end 1) author))))
	(cond
	 ;; If there is an overlay, we have to split it unless the
	 ;; author is AUTHOR or we are on its boundary.
	 (at
	  (unless (eq (rudel-overlay-user at) author)
	    (let* ((on-start (= (overlay-start at) position))
		   (on-end   (= (- (overlay-end at) 1) position))
		   (before (unless on-start
			     (if on-end at (copy-overlay at))))
		   (after  (unless on-end at)))
	      (when before
		(move-overlay before (overlay-start before) position))
	      (when after
		(move-overlay after end (overlay-end after)))
	      (rudel-make-author-overlay buffer position end author))))
	 ;; There is no overlay under the insert, but there are
	 ;; overlays of the same author immediately before and after
	 ;; the insert. We merge these two into one large overlay
	 ;; including the insert.
	 ((and before after)
	  (let ((end (overlay-end after)))
	    (delete-overlay after)
	    (move-overlay before (overlay-start before) end)))
	 ;; If there is an overlay of the same author before the
	 ;; insert, we extend it.
	 (before
	  (move-overlay before (overlay-start before) end))
	 ;; If there is an overlay of the same author after the
	 ;; insert, we extend it.
	 (after
	  (move-overlay after position (overlay-end after)))
	 ;; If there are no overlays at all, we create a suitable one.
	 (t
	  (rudel-make-author-overlay buffer position end author))))))
  )

(defun rudel-update-author-overlay-after-delete (buffer position length author)
  "Update author overlays in BUFFER to incorporate a deletion of length LENGTH at POSITION by AUTHOR.
POSITION refers to an Emacs buffer position.
AUTHOR has to be an object of type rudel-author-child."
  (with-current-buffer buffer
    (mapc
     (lambda (overlay)
       (when (zerop (rudel-overlay-length overlay))
	 (delete-overlay overlay)))
     (rudel-author-overlays-in position position)))
  )


;;; Miscellaneous functions
;;

(defun rudel-overlay-make-face-symbol (category name)
  "Allocate a symbol for a face for CATEGORY and NAME."
  (intern (format "rudel-%s-overlay-%s-face"
		  (if (stringp category)
		      category
		    (symbol-name category))
		  name)))

(defun rudel-overlay-make-face (face template color)
  "Copy TEMPLATE to FACE and replace color attributes with COLOR.
TEMPLATE has to be a face. FACE can be nil or a face. In the
latter case, FACE is returned unmodified."
  (unless (facep face)
    (make-face face)
    (copy-face template face)
    (rudel-overlay-set-face-attributes face color))
  face)

(defun rudel-overlay-set-face-attributes (face color)
  "Set color-related attributes of FACE with respect to COLOR."
  (when (facep face)
    (dolist (property '(:foreground :background :underline :overline))
      (unless (eq (face-attribute face property) 'unspecified)
	(set-face-attribute face nil property color)))))

(defun rudel-overlay-options-changed ()
  "Update Rudel overlays after a change of customization options."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (mapc #'rudel-overlay-author-update (rudel-author-overlays)))))

(provide 'rudel-overlay)
;;; rudel-overlay.el ends here
