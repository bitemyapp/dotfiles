;;; adopted-compound.el --- Adopted compound operation
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, adopted, algorithm, operation, compound
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
;; Class `adopted-compound' implements a compound operation comprised
;; of a number of child operations.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'adopted-operation)


;;; Class adopted-compound
;;

(defclass adopted-compound (adopted-operation)
  ((children :initarg  :children
	     :type     list
	     :initform nil
	     :documentation
	     ""))
  "Objects of this class are operations, which are composed of a
number of child operation.")

;; TODO this has side effects. It can only be called once
(defmethod rudel-apply ((this adopted-compound) object)
  "Apply THIS to BUFFER by applying the child operation."
  (with-slots (children) this
    (let ((child (first children))
	  (rest  (rest  children)))
      ;; Apply all child operations
      (while child
	(rudel-apply child object)
	;; For each applied child operation, transform remaining
	;; operation with the applied operation.
	(dolist (next rest)
	  (setf next (adopted-transform child next)))
	;; Advance to next child operation.
	(setq child (first rest)
	      rest  (rest rest)))))
  )

(defmethod adopted-transform ((this adopted-compound) other)
  "Transform OTHER using the child operations of THIS."
  (with-slots (children) this
    (dolist (child children) ;; TODO reverse children?
      (setq other (adopted-transform child other)))
    other))

(provide 'adopted-compound)
;;; adopted-compound.el ends here
