;;; guru-mode.el --- Become an Emacs guru

;; Copyright (C) 2012 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/guru-mode
;; Version: 0.1
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Guru mode teaches to use Emacs properly.
;;
;;; Code:

(defvar guru-mode-map (make-sparse-keymap)
  "Guru mode's keymap.")

(defvar affected-bindings-list '(("<left>" . "C-b")
                                 ("<right>" . "C-f")
                                 ("<up>" . "C-p")
                                 ("<down>" . "C-n")
                                 ("<C-left>" . "M-b")
                                 ("<C-right>" . "M-f")
                                 ("<C-up>" . "M-{")
                                 ("<C-down>" . "M-}")
                                 ("<M-left>" . "M-b")
                                 ("<M-right>" . "M-f")
                                 ("<M-up>" . "M-{")
                                 ("<M-down>" . "M-}")
                                 ("<delete>" . "C-d")
                                 ("<C-delete>" . "M-d")
                                 ("<M-delete>" . "M-d")
                                 ("<next>" . "C-v")
                                 ("<C-next>" . "M-x <")
                                 ("<prior>" . "M-v")
                                 ("<C-prior>" . "M-x >")
                                 ("<home>" . "C-a")
                                 ("<C-home>" . "M-<")
                                 ("<end>" . "C-e")
                                 ("<C-end>" . "M->")))

(defun guru-rebind (original-key alt-key)
  `(lambda ()
     (interactive)
     (message "%s keybinding is disabled! Use <%s> instead" ,original-key  ,alt-key)))

(dolist (cell affected-bindings-list)
  (define-key guru-mode-map
    (read-kbd-macro (car cell)) (guru-rebind (car cell) (cdr cell))))

(defun turn-on-guru-mode ()
  "Enable Guru mode."
  (guru-mode +1))

(defun turn-off-guru-mode ()
  "Disable Guru mode."
  (guru-mode -1))

;;;###autoload
(define-minor-mode guru-mode
  "A minor mode that teaches you to use Emacs effectively."
  :lighter " guru"
  :keymap guru-mode-map
  :group 'guru)

;; define global minor mode
;;;###autoload
(define-globalized-minor-mode guru-global-mode guru-mode turn-on-guru-mode)

(provide 'guru-mode)
;;; guru-mode.el ends here

