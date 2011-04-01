;;; rudel-telepathy.el --- A telepathy backend for Rudel
;;
;; Copyright (C) 2008, 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, telepathy, backend
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
;; This file contains a Rudel backend which realizes session
;; initiation and transport of Rudel data through freedesktop's
;; Telepathy framework (http://telepathy.freedesktop.org).


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-backend)
(require 'rudel-transport)


;;; Constants
;;

(defconst rudel-telepathy-version '(0 1)
  "Version of the telepathy backend for Rudel.")


;;;  Class rudel-telepathy-backend
;;

;;;###autoload
(defclass rudel-telepathy-backend (rudel-transport-backend)
  ((capabilities :initform '()))
  "Class rudel-telepathy-backend ")

(defmethod initialize-instance ((this rudel-telepathy-backend) slots)
  "Initialize slots of THIS according to SLOTS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-telepathy-version))


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'telepathy 'rudel-telepathy-backend)

(provide 'rudel-telepathy)
;;; rudel-telepathy.el ends here
