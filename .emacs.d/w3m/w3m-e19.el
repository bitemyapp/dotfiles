;;; w3m-e19.el --- Emacs 19 specific functions for emacs-w3m

;; Copyright (C) 2002, 2005 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Kevin Rodgers <kevin.rodgers@ihs.com>,
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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module provides Emacs 19 specific functions.  Visit
;; <URL:http://emacs-w3m.namazu.org/> for more details of emacs-w3m.
;;
;; Note that it requires APEL and the new CUSTOM, you can get them
;; from the following places:
;;
;; ftp://ftp.m17n.org/pub/mule/apel/apel-10.3.tar.gz
;; ftp://ftp.dina.kvl.dk/pub/Staff/Per.Abrahamsen/custom/custom-1.9962.tar.gz
;;
;; In addition, you need to install regexp-opt.elc in your load-path.
;; The source file is available in attic/ directory of the emacs-w3m
;; distribution.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'pcustom)
(require 'regexp-opt)

(unless (fboundp 'compose-mail)
  (defun compose-mail (&optional to subject other-headers continue
				 switch-function yank-action send-actions)
    "Start composing a mail message to send."
    (interactive
     (list nil nil nil current-prefix-arg))
    (let ((function (get mail-user-agent 'composefunc)))
      (funcall function to subject other-headers continue
	       switch-function yank-action send-actions))))

(defmacro define-ccl-program (&rest args))

(defun make-char (charset &optional c1 c2)
  (or c1 ?\000))

(defalias 'coding-system-list 'ignore)
(defalias 'multibyte-string-p 'ignore)
(defalias 'w3m-create-image 'ignore)
(defalias 'w3m-create-resized-image 'ignore)
(defalias 'w3m-detect-coding-region 'ignore)
(defalias 'w3m-display-graphic-p 'ignore)
(defalias 'w3m-display-inline-images-p 'ignore)
(defalias 'w3m-find-coding-system 'ignore)
(defalias 'w3m-image-type-available-p 'ignore)
(defalias 'w3m-insert-image 'ignore)
(defalias 'w3m-make-ccl-coding-system 'ignore)
(defalias 'w3m-remove-image 'ignore)
(defalias 'w3m-setup-toolbar 'ignore)
(defalias 'w3m-update-toolbar 'ignore)
(defalias 'w3m-mule-unicode-p 'ignore)

(autoload `ange-ftp-read-passwd "ange-ftp")
(defalias 'read-passwd 'ange-ftp-read-passwd)

(defadvice read-string (after
			default-value
			(prompt &optional initial-input history default-value)
			activate)
  "Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
for history commands, and as the value to return if the user enters
the empty string."
  (if (and (equal ad-return-value "")
	   default-value)
      (setq ad-return-value default-value)))

(provide 'w3m-e19)

;;; w3m-e19.el ends here
