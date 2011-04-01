;;; rudel-errors.el --- Error data used in Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, errors, conditions
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
;; The following condition hierarchy is defined:
;;
;; error
;; + rudel-error
;;   + rudel-incomplete-info
;;   + rudel-join-error
;;   + rudel-host-error


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

;; rudel-incomplete-info

(intern "rudel-incomplete-info")

(put 'rudel-incomplete-info 'error-conditions
     '(error
       rudel-error rudel-incomplete-info))

(put 'rudel-incomplete-info 'error-message
     "Required properties missing in property list")

;; rudel-join-error

(intern "rudel-join-error")

(put 'rudel-join-error 'error-conditions
     '(error
       rudel-error rudel-join-error))

(put 'rudel-join-error 'error-message
     "Could not join session")

;; rudel-host-error

(intern "rudel-host-error")

(put 'rudel-host-error 'error-conditions
     '(error
       rudel-error rudel-host-error))

(put 'rudel-host-error 'error-message
     "Could not host session")

(provide 'rudel-errors)
;;; rudel-errors.el ends here
