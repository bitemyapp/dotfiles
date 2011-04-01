;;; jupiter.el --- An implementation of the Jupiter algorithm
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, jupiter, algorithm, distributed, integrity
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
;; This file contains an implementation of the jupiter algorithm,
;; which ensures the synchronization of data shared between multiple
;; peers despite differences in network latency.
;;
;; This implementation is partly based on the implementation used in
;; the obby library <http://gobby.0x539.de/trac/>. Note, however, that
;; the details of the implementations differ.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'jupiter-operation)
(require 'jupiter-insert)
(require 'jupiter-delete)
(require 'jupiter-compound)
(require 'jupiter-nop)


;;; Class jupiter-context
;;

(defclass jupiter-context ()
  ((local-revision  :initarg  :local-revision
		    :type     (integer 0)
		    :initform 0
		    :documentation
		    "Revision number of the local data.")
   (remote-revision :initarg  :remote-revision
		    :type     (integer 0)
		    :initform 0
		    :documentation
		    "Revision number of the remote data.")
   (local-log       :initarg  :local-log
		    :type     list
		    :initform nil
		    :documentation
		    "List of local operations, that have not been
acknowledged by the remote side."))
  "Objects of this class store the state of one side of a
concurrent modification activity, which is synchronized using the
jupiter algorithm.")

(defmethod jupiter-local-operation ((this jupiter-context) operation)
  "Store OPERATION in the operation log of THIS and increase local revision count."
  (with-slots (local-revision local-log) this
    (push (cons local-revision operation) local-log)
    (incf local-revision)))

(defmethod jupiter-remote-operation ((this jupiter-context)
				     local-revision remote-revision
				     operation)
  "Transform OPERATION with revisions LOCAL-REVISION and REMOTE-REVISION using the local operations stored in THIS.
LOCAL-REVISION is the local revision of THIS context, the remote
site is referring to."
  (let ((transformed-operation operation))
    (with-slots ((this-remote-revision :remote-revision)
		 local-log) this

      ;; Discard stored local operations which are older than the
      ;; local revision to which the remote site refers.
      (setq local-log (delete-if
		       (lambda (revision) (< revision local-revision))
		       local-log
		       :key 'car))

      ;; Transform the operation
      (mapc
       (lambda (log-operation)

	 ;; Transform the remote operation using the stored operation.
	 (setq transformed-operation
	       (jupiter-transform (cdr log-operation)
				  transformed-operation))

	 ;; Transform the stored operation using the already
	 ;; transformed remote operation.
	 (setf (cdr log-operation)
	       (jupiter-transform transformed-operation
				  (cdr log-operation))))
       (reverse local-log))

      ;; Increase remote revision
      (incf this-remote-revision))
    ;; The transformed operation is the result of the computation.
    transformed-operation)
  )

(defmethod object-print ((this jupiter-context) &rest strings)
  "Add revisions and log length to string representation of THIS."
  (with-slots (local-revision remote-revision local-log) this
    (call-next-method
     this
     (format " local %d" local-revision)
     (format " remote %d" remote-revision)
     (format " log-items %d" (length local-log)))))

(provide 'jupiter)
;;; jupiter.el ends here
