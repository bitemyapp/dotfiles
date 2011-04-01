;;; adopted-nop.el --- Adopted no operation
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, adopted, algorithm, operation, nop
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
;; Class `adopted-nop' implements a no-operation for the Adopted
;; algorithm.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'adopted-operation)


;;; Class adopted-nop
;;

(defclass adopted-nop (adopted-operation)
  ()
  "Operation, which does not change anything.")

(defmethod rudel-apply ((this adopted-nop) object)
  "Applying THIS does not change OBJECT.")

(defmethod adopted-transform ((this adopted-nop) other)
  "Transforming OTHER with THIS simply returns OTHER."
  other)

(provide 'adopted-nop)
;;; adopted-nop.el ends here
