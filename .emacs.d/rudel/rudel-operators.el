;;; rudel-operators.el --- Sets of modification operators for Rudel objects
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, operators
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
;; Collections of operations on specific objects are collected into
;; classes. Currently there are
;;
;; + rudel-document-operators: perform operations on document objects
;;
;; + rudel-connection-operators: perform operations on connection
;;   objects
;;
;; + rudel-overlay-operators: perform operations by altering overlays
;;   of buffer objects
;;
;; + rudel-hook-operators: perform operations by calling hooks


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-overlay)


;;; Class rudel-document-operators
;;

(defclass rudel-document-operators ()
  ((document :initarg  :document
	     :type     rudel-document-child
	     :documentation
	     "Document to which modification operators are
applied."))
  "Provides operation methods which modify an associated document.")

(defmethod rudel-insert ((this rudel-document-operators) position data)
  "Insert DATA at POSITION into the document attached to THIS."
  (with-slots (document) this
    (rudel-insert document position data)))

(defmethod rudel-delete ((this rudel-document-operators) position length)
  "Delete a region of LENGTH characters at POSITION from the document attached to THIS."
  (with-slots (document) this
    (rudel-delete document position length)))


;;; Class rudel-connection-operators
;;

(defclass rudel-connection-operators ()
  ((connection :initarg :connection
	       :type    rudel-connection-child
	       :documentation
	       "Connection on which the operations are
performed.")
   (document   :initarg :document
	       :type    rudel-document-child
	       :documentation
	       "Document object to which operations refer."))
  "Provides operation methods which affect an associated
connection.")

(defmethod rudel-insert ((this rudel-connection-operators) position data)
  "Notify the connection associated to THIS of the insertion of DATA at POSITION."
  (with-slots (connection document) this
    (rudel-local-insert connection document position data)))

(defmethod rudel-delete ((this rudel-connection-operators) position length)
  "Notify the connection associated to THIS of a deletion of LENGTH at POSITION."
  (with-slots (connection document) this
    (rudel-local-delete connection document position length)))


;;; Class rudel-overlay-operators
;;

(defclass rudel-overlay-operators ()
  ((document :initarg  :document
	     :type     rudel-document-child
	     :documentation
	     "The document to the overlays of which the
operations are applied")
   (user     :initarg  :user
	     :type     rudel-user-child
	     :documentation
	     "The user object associated to operations."))
  "Provides operation methods which affect the overlays of a
buffer.")

(defmethod rudel-insert ((this rudel-overlay-operators) position data)
  "Update the overlays associated to THIS to incorporate an insertion of DATA at POSITION."
  (with-slots (document user) this
    (with-slots (buffer) document

      ;; Since we inserted something, (point-max) is at least the
      ;; length of the insertion + 1. So we can safely subtract the
      ;; length of the insertion and 1.
      (unless position
	(with-current-buffer buffer
	  (setq position (- (point-max) (length data) 1))))


      (rudel-update-author-overlay-after-insert
       buffer (+ position 1) (length data) user)))
  )

(defmethod rudel-delete ((this rudel-overlay-operators) position length)
  "Update the overlays associated to THIS to incorporate a deletion of LENGTH at POSITION."
  (with-slots (document user) this
    (with-slots (buffer) document
      (rudel-update-author-overlay-after-delete
       buffer (+ position 1) length user))))


;;; Class rudel-hook-operators
;;

(defclass rudel-hook-operators ()
  ((document :initarg  :document
	     :type     rudel-document-child
	     :documentation
	     "The document object to which operations refer.")
   (user     :initarg  :user
	     :type     rudel-user-child
	     :documentation
	     "The user object associated to operations."))
  "Provides operation methods which cause corresponding hooks to
be called.")

(defmethod rudel-insert ((this rudel-hook-operators) position data)
  "Call insert hook associated to THIS with POSITION and DATA."
  (with-slots (document user) this
    (with-slots (buffer) document
      (run-hook-with-args 'rudel-insert-hook buffer user position data))))

(defmethod rudel-delete ((this rudel-hook-operators) position length)
  "Call delete hook associated to THIS with POSITION and LENGTH."
  (with-slots (document user) this
    (with-slots (buffer) document
      (run-hook-with-args 'rudel-delete-hook buffer user position length))))

(provide 'rudel-operators)
;;; rudel-operators.el ends here
