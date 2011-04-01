;;; rudel-speedbar.el --- Speedbar rendering of Rudel objects
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, collaboration, speedbar
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
;; This implements rendering of Rudel objects in speedbar.


;;;  History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'speedbar)
(require 'eieio-speedbar)


;;; Class rudel-user methods
;;

(defmethod eieio-speedbar-description ((this rudel-user))
  "Provide a speedbar description for OBJ."
  (format "User %s" (object-name-string this)))

(defmethod eieio-speedbar-object-buttonname ((this rudel-user))
  "Return a string to use as a speedbar button for OBJECT."
  (format "%s" (object-name-string this)))


;;; Class rudel-document methods
;;

(defmethod eieio-speedbar-description ((this rudel-document))
  "Construct a description for from the name of document object THIS."
  (format "Document %s" (object-name-string this)))

(defmethod eieio-speedbar-object-buttonname ((this rudel-document))
  "Return a string to use as a speedbar button for OBJECT."
  (rudel-unique-name this))


;;; Speedbar support mode
;;

(defun rudel-speedbar-make-map ()
  "Make the generic object based speedbar keymap."
  (speedbar-make-specialized-keymap))

(defvar rudel-speedbar-key-map
  (rudel-speedbar-make-map)
  "A generic object based speedbar display keymap.")

(defvar rudel-speedbar-menu
  '([ "Subscribe" #'ignore t])
  "Menu part in easymenu format used in speedbar while browsing objects.")

(defun rudel-speedbar-toplevel-buttons (dir)
  "Return a list of objects to display in speedbar.
Argument DIR is the directory from which to derive the list of objects."
  (when rudel-current-session
    (with-slots (users documents) rudel-current-session
      (append users documents))))

(eieio-speedbar-create 'rudel-speedbar-make-map
		       'rudel-speedbar-key-map
		       'rudel-speedbar-menu
		       "Collaboration Session"
		       'rudel-speedbar-toplevel-buttons)

;;;###autoload
(defun rudel-speedbar ()
  "Show connected users and available documents of Rudel session in speedbar."
  (interactive)
  (speedbar-frame-mode 1)
  (speedbar-change-initial-expansion-list "Collaboration Session")
  (speedbar-get-focus))

(provide 'rudel-speedbar)
;;; rudel-speedbar.el ends here
