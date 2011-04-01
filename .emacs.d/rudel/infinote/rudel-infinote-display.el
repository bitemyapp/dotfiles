;;; rudel-infinote-display.el --- Display functions for infinote users
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, user interface
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
;; specialized in the infinote backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-icons)

(require 'rudel-infinote-user)

(defmethod rudel-display-string ((this rudel-infinote-document-user)
				 &optional use-images)
  "Return a textual representation of THIS for user interface purposes."
  (with-slots ((name :object-name) status) this
    (concat
     (call-next-method)

     (case status
       (active
	(propertize
	 "a"
	 'display   rudel-icon-connected
	 'help-echo (format "%s is connected"
			    name)))

       (inactive
	(propertize
	 "i"
	 'display   rudel-icon-connected
	 'help-echo (format "%s is connected, but inactive"
			    name)))

       (unavailable
	(propertize
	 "-"
	 'display   rudel-icon-disconnected
	 'help-ehco (format "%s is not connected"
			    name)))

       (t
	"?"))))
  )

(provide 'rudel-infinote-display)
;;; rudel-infinote-display.el ends here
