;;; rudel-obby-display.el --- Display functions for obby documents and users
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, user interface
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
;; This file contains specialized versions of the
;; `rudel-display-string' method for Rudel classes that are
;; specialized in the obby backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-display)
(require 'rudel-icons)

(require 'rudel-obby) ;; make sure `rudel-obby-user' is defined

(defmethod rudel-display-string ((this rudel-obby-user)
				 &optional use-images align)
  "Return a textual representation of THIS for user interface purposes."
  (with-slots (connected color) this
    (let ((encryption  (and (slot-boundp this :encryption) ;; TODO this is bad
			    (oref this :encryption)))
	  (name-string (call-next-method)))
      (concat
       ;; Name bit
       (cond
	((numberp align)
	 (format (format "%%-%ds" align) name-string))
	((eq align t)
	 (format "%-12s" name-string))
	(t
	 name-string))

       ;; A space in case the next part will be text.
       (unless use-images
	 " ")

       ;; Connection status bit
       (apply
	#'propertize
	(if connected "c" "-")
	'help-echo (format (if connected
			       "%s is connected"
			     "%s is not connected")
			   name-string)
	'face      (list :background color)
	(when use-images
	  (list 'display (if connected
			     rudel-icon-connected
			   rudel-icon-disconnected))))

       ;; Encryption bit
       (apply
	#'propertize
	(if encryption "e" "-")
	'help-echo (format (if encryption
			       "%s's connection is encrypted"
			     "%s's connection is not encrypted")
			   name-string)
	'face      (list :background color)
	(when use-images
	  (list 'display (if encryption
			     rudel-icon-encrypted
			   rudel-icon-plaintext)))))))
  )

(provide 'rudel-obby-display)
;;; rudel-obby-display.el ends here
