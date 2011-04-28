;;; w3-keymap.el --- Keybindings for Emacs/W3
;; Author: $Author: fx $
;; Created: $Date: 2001/05/29 15:46:28 $
;; Version: $Revision: 1.2 $
;; Keywords: comm, help, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Keymap definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-mode-map (make-sparse-keymap) "Keymap to use in w3-mode.")
(suppress-keymap w3-mode-map)
(set-keymap-parent w3-mode-map widget-keymap)

(define-key w3-mode-map "h" (make-sparse-keymap))
(define-key w3-mode-map "H" (make-sparse-keymap))
(define-key w3-mode-map "a" (make-sparse-keymap))

(define-key w3-mode-map "ha"       'w3-hotlist-apropos)
(define-key w3-mode-map "hd"       'w3-hotlist-delete)
(define-key w3-mode-map "hi"       'w3-hotlist-add-document)
(define-key w3-mode-map "hv"       'w3-hotlist-view)
(define-key w3-mode-map "hr"       'w3-hotlist-rename-entry)
(define-key w3-mode-map "hu"       'w3-use-hotlist)
(define-key w3-mode-map "hA"       'w3-hotlist-append)
(define-key w3-mode-map "hI"       'w3-hotlist-add-document-at-point)
(define-key w3-mode-map "hR"       'w3-hotlist-refresh)

(define-key w3-mode-map "x" (make-sparse-keymap))
(define-key w3-mode-map "xa" 'w3-hotindex-add-key)
(define-key w3-mode-map "xd" 'w3-hotindex-rm-key)
(define-key w3-mode-map "xq" 'w3-hotindex-query)

(define-key w3-mode-map "HF"       'w3-history-forward)
(define-key w3-mode-map "HB"       'w3-history-backward)
(define-key w3-mode-map "Hv"       'w3-show-history-list)

(define-key w3-mode-map " "	   'w3-scroll-up)
(define-key w3-mode-map "<"        'beginning-of-buffer)
(define-key w3-mode-map ">"        'end-of-buffer)
(define-key w3-mode-map "?"        'w3-help)
(define-key w3-mode-map "B"        'w3-history-backward)
(define-key w3-mode-map "D"        'w3-download-url-at-point)
(define-key w3-mode-map "F"        'w3-history-forward)
(define-key w3-mode-map "G"        'w3-show-graphics)
(define-key w3-mode-map "I"        'w3-popup-info)
(define-key w3-mode-map "K"        'w3-save-this-url)
;; FIXME!
;;(define-key w3-mode-map "P"        'w3-print-url-under-point)
(define-key w3-mode-map "Q"        'w3-leave-buffer)
(define-key w3-mode-map "R"        'w3-refresh-buffer)
(define-key w3-mode-map "S"        'w3-source-document-at-point)
(define-key w3-mode-map "U"        'w3-use-links)
(define-key w3-mode-map "V"        'w3-view-this-url)
(define-key w3-mode-map "\C-?"     'scroll-down)
(define-key w3-mode-map [backspace] 'scroll-down)
(define-key w3-mode-map "\C-c\C-b" 'w3-show-history-list)
(define-key w3-mode-map "\C-c\C-v" 'w3-version)
(define-key w3-mode-map "\C-o"     'w3-fetch)
(define-key w3-mode-map "\M-M"     'w3-mail-document-under-point)
(define-key w3-mode-map "\M-m"	   'w3-mail-current-document)
(define-key w3-mode-map "\M-s"	   'w3-save-as)
(define-key w3-mode-map "\M-\r"    'w3-follow-inlined-image)
(define-key w3-mode-map "b"	   'w3-widget-backward)
(define-key w3-mode-map "c"        'w3-mail-document-author)
(define-key w3-mode-map "d"        'w3-download-this-url)
(define-key w3-mode-map "f"	   'w3-widget-forward)
(define-key w3-mode-map "g"        'w3-reload-document)
(define-key w3-mode-map "i"        'w3-document-information)
(define-key w3-mode-map "k"        'w3-save-url)
(define-key w3-mode-map "l"        'w3-goto-last-buffer)
(define-key w3-mode-map "m"        'w3-complete-link)
(define-key w3-mode-map "n"        'w3-widget-forward)
(define-key w3-mode-map "o"	   'w3-open-local)
(define-key w3-mode-map "p"        'w3-print-this-url)
(define-key w3-mode-map "q"	   'w3-quit)
(define-key w3-mode-map "r"        'w3-reload-document)
(define-key w3-mode-map "s"        'w3-source-document)
(define-key w3-mode-map "u"        'w3-leave-buffer)
(define-key w3-mode-map "v"	   'url-view-url)
(define-key w3-mode-map "w"        'w3-submit-bug)

;; These are duplicated here instead of just inherited from widget-keymap
;; due to some issues with Emacspeak.  FIXME.
(define-key w3-mode-map [tab] 'w3-widget-forward)
(define-key w3-mode-map [(shift tab)] 'w3-widget-backward)
(define-key w3-mode-map [(meta tab)] 'w3-widget-backward)
(define-key w3-mode-map [backtab] 'w3-widget-backward)

;; Emulate some netscape stuff by default
(define-key w3-mode-map [(control alt t)] 'url-list-processes)
(define-key w3-mode-map [(control meta t)] 'url-list-processes)

;; Have fun with document ordering
(define-key w3-mode-map [(meta space)] 'w3-next-document)
(define-key w3-mode-map [(meta delete)] 'w3-prev-document)

(provide 'w3-keymap)
