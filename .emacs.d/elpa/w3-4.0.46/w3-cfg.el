;;; w3-cfg.el --- Configuration info from Emacs/W3
;; Author: $Author: wmperry $
;; Created: $Date: 1998/12/01 22:12:12 $
;; Version: $Revision: 1.1 $
;; Keywords: hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1998 Free Software Foundation, Inc.
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

(defconst w3-configuration-data
  '(
    (srcdir              . ".")
    (datadir             . "${prefix}/share")
    (prefix              . "/usr")
    (infodir             . "${prefix}/info")
    (lispdir             . "$(prefix)/share/xemacs/site-lisp")
    (EMACS               . "xemacs")
    (CUSTOM              . "@CUSTOM@")
    (EMACS_FLAVOR        . "xemacs")
    (EMACS_VERSION       . "21.4")
    (EMACS_PACKAGE_DIR   . "")
    (XEMACS              . "yes")
    )
  "Emacs/W3 configuration data.
This data is from the configuration step in building Emacs/W3, and
the data may not accurately reflect your current environment.")

(defun w3-configuration-data (option &optional default)
  (let ((info (or (cdr-safe (assq option w3-configuration-data)) default)))
    (if (not (stringp info))
	(setq info (eval info)))
    (while (string-match "\\$[({]\\([^{(]+\\)[})]" info)
      (setq info (concat (substring info 0 (match-beginning 0))
			 (w3-configuration-data (intern (match-string 1 info))
						"")
			 (substring info (match-end 0)))))
    info))
    
(provide 'w3-cfg)
