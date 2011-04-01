;;; jupiter-insert.el --- Jupiter insert operation
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: jupiter, operation, insert
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
;; Class `jupiter-insert' implements an insert operation for the
;; Jupiter algorithm.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-operations)
(require 'jupiter-operation)
(require 'jupiter-nop)


;;; Class jupiter-insert
;;

(defclass jupiter-insert (jupiter-operation
			  rudel-insert-op)
  ()
  "Objects of this class represent insertions into buffers.")

(defmethod jupiter-transform ((this jupiter-insert) other)
  "Transform OTHER using THIS."
  (cond

   ;;
   ;; Transform an insert operation
   ;;
   ((jupiter-insert-p other)
    (with-slots ((this-from   :from)
		 (this-to     :to)
		 (this-length :length)
		 (this-data   :data)) this
      (with-slots ((other-from   :from)
		   (other-to     :to)
		   (other-length :length)
		   (other-data   :data)) other
	(cond
	 ;;
	 ;; <other>
	 ;;         <this>
	 ;; No need to do anything in this case.
	 ;; ((< other-from this-from))

	 ;;
	 ;;        <other>
	 ;; <this>
	 ;;
	 ((> other-from this-from)
	  (incf other-from this-length))

	 ;;
	 ;; <other>
	 ;; <this>
	 ;; OTHER inserted at the same start position as we did. Since
	 ;; the situation is symmetric in the location properties of
	 ;; OTHER and THIS, we use the inserted data to construct an
	 ;; ordering.
	 ((= other-from this-from)
	  (when (string< this-data other-data)
	    (incf other-from this-length)))))))

   ;;
   ;; Transform a delete operation
   ;;
   ((jupiter-delete-p other)
    (with-slots ((this-from   :from)
		 (this-to     :to)
		 (this-length :length)) this
      (with-slots ((other-from   :from)
		   (other-to     :to)
		   (other-length :length)) other
	(cond

	 ;;
	 ;; <other>
	 ;;         <this>
	 ;; just keep OTHER

	 ;;
	 ;; <other> and   <other> and        <other>
	 ;; <this>      <this>        <this>
	 ((>= other-from this-from)
	  (incf other-from this-length)
	  (incf other-to   this-length))

	 ;;
	 ;; <  other  >
	 ;;   <this>
	 ;; OTHER deleted a region that includes the point at which THIS
	 ;; inserted in its interior. OTHER has to be split into one
	 ;; deletion before and one delete after the inserted data.
	 ((and (< other-from this-from) (> other-to this-to))
	  (setq other
		(jupiter-compound "compound"
	         :children (list (jupiter-delete "delete-left"
				  :from other-from
				  :to   this-from)
				 (jupiter-delete "delete-right"
				  :from this-to
				  :to   (+ other-to this-length))))))
	 ))))

   ;;
   ;; Transform a compound operation
   ;;
   ((jupiter-compound-p other) ;; TODO encapsulation violation
    (with-slots (children) other
      (dolist (child children)
	(setf child (jupiter-transform this child)))))

   ;;
   ;; Transform a nop operation
   ;;
   ((jupiter-nop-p other))

   ;; TODO this is for debugging
   (t (error "Cannot transform operation of type `%s'"
	     (object-class other))))
  other)

(defmethod object-print ((this jupiter-insert) &rest strings)
  "Add from, to, length and data to string representation of THIS."
  (with-slots (from to length data) this
    (call-next-method
     this
     (format " from %d" from)
     (format " to %d" to)
     (format " length %d" length)
     (format " data \"%s\"" data)))
  )

(provide 'jupiter-insert)
;;; jupiter-insert.el ends here
