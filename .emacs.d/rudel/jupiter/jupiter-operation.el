;;; jupiter-operation.el --- Operation base class for jupiter algorithm
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Jupiter, operation, base
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
;; The class jupiter-operation is a base class for Jupiter operation
;; classes.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'eieio)

(require 'rudel-operations)


;;; Class jupiter-operation
;;

(defclass jupiter-operation (rudel-operation)
  ()
  "Objects of derived classes represent operations, which change documents.
Objects can transform each other to produce sequences of
operations, which produce identical changes than permutations of
the same operations."
  :abstract t)

;; This one really could use multiple dispatch
(defgeneric jupiter-transform ((this jupiter-operation) other)
  "Transform OTHER such that the effect of applying it after THIS are equal to applying OTHER before THIS unmodified.
In general, OTHER is destructively modified or replaced.")

(provide 'jupiter-operation)
;;; jupiter-operation.el ends here
