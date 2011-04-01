;;; adopted-insert.el --- Adopted insert operation
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, adopted, algorithm, operation, insert
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
;; along with rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; Class `adopted-insert' implements an insert operation for the
;; Adopted algorithm.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-operations)
(require 'adopted-operation)
(require 'adopted-compound)
(require 'adopted-nop)


;;; Class adopted-insert
;;

(defclass adopted-insert (adopted-operation
			  rudel-insert-op)
  ()
  "Objects of this class represent insertions into buffers.")

(defmethod adopted-transform ((this adopted-insert) other)
  "Transform OTHER using THIS."
  (cond

   ;;
   ;; Transform an insert operation
   ;;
   ((adopted-insert-p other)
    (with-slots ((this-from :from) (this-to :to) (this-length :length) (this-data :data)) this
      (with-slots ((other-from :from) (other-to :to) (other-length :length) (other-data :data)) other
	(cond
	 ;;
	 ;; <other>
	 ;;         <this>
	 ;; No need to do anything in this case.
	 ((< other-from this-from)) ;; TODO remove this case; but not the comment

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
   ((adopted-delete-p other)
    (with-slots ((this-from :from) (this-to :to) (this-length :length)) this
      (with-slots ((other-from :from) (other-to :to) (other-length :length)) other
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
		(adopted-compound "compound"
	         :children (list (adopted-delete "delete-left"
				  :from other-from
				  :to   this-from)
				 (adopted-delete "delete-right"
				  :from this-to
				  :to   (+ other-to this-length))))))
	 ))))

   ;;
   ;; Transform a compound operation
   ;;
   ((adopted-compound-p other) ;; TODO encapsulation violation
    (with-slots (children) other
      (dolist (child children)
	(setf child (adopted-transform this child)))))

   ;;
   ;; Transform a nop operation
   ;;
   ((adopted-nop-p other))

   ;; TODO this is for debugging
   (t (error "Cannot transform operation of type `%s'"
	     (object-class other))))
  other)

(provide 'adopted-insert)
;;; adopted-insert.el ends here
