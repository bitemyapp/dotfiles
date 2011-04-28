;;; w3-menu.el --- Mouse specific functions for emacs-w3
;; Author: $Author: wmperry $
;; Created: $Date: 2000/11/15 13:58:28 $
;; Version: $Revision: 1.3 $
;; Keywords: mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

(defun w3-follow-mouse-other-frame (e)
  "Function suitable to being bound to a mouse key.  Follows the link under
the mouse click, opening it in another frame."
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link-other-frame))

(defun w3-follow-inlined-image-mouse (e)
  "Follow an inlined image from the mouse"
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-inlined-image))

(defun w3-follow-inlined-image ()
  "Follow an inlined image, regardless of whether it is a hyperlink or not."
  (interactive)
  (let ((widget (widget-at (point))))
    (and (not widget) (error "No inlined image at point."))
    (setq widget (widget-get widget :parent))
    
    (and (or (not widget)
	     (not (eq 'image (car widget))))
	 (error "No inlined image at point."))
    (and (widget-get widget :src)
	 (w3-fetch (widget-get widget :src)))))

(defvar w3-mouse-button1 (cond
			  ((featurep 'infodock) nil)
			  ((and w3-running-xemacs (featurep 'mouse)) 'button1)
			  (w3-running-xemacs nil)
			  (t 'down-mouse-1)))
(defvar w3-mouse-button2 (cond
			  ((featurep 'infodock) nil)
			  ((and w3-running-xemacs (featurep 'mouse)) 'button2)
			  (w3-running-xemacs nil)
			  (t 'down-mouse-2)))
(defvar w3-mouse-button3 (cond
			  ((featurep 'infodock) nil)
			  ((and w3-running-xemacs (featurep 'mouse)) 'button3)
			  (w3-running-xemacs nil) 
			  (t 'down-mouse-3)))

(if w3-mouse-button3
    (define-key w3-mode-map (vector w3-mouse-button3) 'w3-popup-menu))

(if w3-mouse-button1
    (define-key w3-netscape-emulation-minor-mode-map
      (vector w3-mouse-button1) 'w3-widget-button-click))
      
(if w3-mouse-button2
    (progn
      (define-key w3-mode-map (vector (list 'meta w3-mouse-button2))
	'w3-follow-mouse-other-frame)
      (define-key w3-netscape-emulation-minor-mode-map
	(vector w3-mouse-button2) 'w3-follow-mouse-other-frame)))

(if (not w3-running-xemacs)
    (progn
      (if (and w3-track-mouse (not (= 21 emacs-major-version)))
	  (define-key w3-mode-map [mouse-movement] 'w3-mouse-handler))
      (if w3-popup-menu-on-mouse-3
	  (define-key w3-mode-map [down-mouse-3] 'w3-popup-menu))))
  
(provide 'w3-mouse)
