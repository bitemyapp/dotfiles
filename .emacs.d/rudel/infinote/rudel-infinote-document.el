;;; rudel-infinote-document.el --- Infinote document class
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, document
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
;; This file contains the `rudel-infinote-document' class which is the
;; base class for different document classes used in the infinote
;; backend. (See rudel-infinote-node.el for an overview)


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-infinote-node)


;;; Class rudel-infinote-document
;;

(defclass rudel-infinote-document (rudel-infinote-node
				   rudel-document)
  ((self :initarg :self
	 :type    rudel-infinote-document-user
	 :reader  rudel-self
	 :documentation
	 "The user object belonging to the local side."))
  "Super class of infinote document classes.")

(defmethod rudel-add-user ((this rudel-infinote-document) user)
  "Add USER to THIS document.
The :session-user slot of user is set to the session user. The
session user is looked up and created if necessary."
  (with-slots ((name :object-name) color) user
    ;; First, find an existing session user or create a new one.
    (let ((session-user
	   (with-slots (session) this
	     (or
	      (rudel-find-user session name)
	      (let ((user (rudel-infinote-user
			   name
			   :color color)))
		(rudel-add-user session user)
		user)))))

      ;; Associate the user object of the session to USER.
      (unless (slot-boundp user :session-user)
	(oset user :session-user session-user))

      ;; This actually adds the user to THIS.
      (call-next-method this user) ;; TODO the next method should return the user
      user))
  )

(defmethod rudel-set-self ((this rudel-infinote-document) user)
  "Set USER as self user of THIS.
If the session associated to THIS does not have a self user, the
session user object corresponding to USER is set as self user of
the session."
  ;; Check whether the session has a self user. If not, find the user
  ;; associated to USER and set it as self user.
  (with-slots (session self) this
    (when (not (rudel-self session))
      (let ((session-self
	     (rudel-find-user session (object-name-string user))))
	(if (not session-self)
	    (error
	     "Could not find designated self user in session: `%s'"
	     (object-name-string user))
	  (rudel-set-self session session-self))))

    ;; Set self slot of this.
    (setq self user))
  )

(provide 'rudel-infinote-document)
;;; rudel-infinote-document.el ends here
