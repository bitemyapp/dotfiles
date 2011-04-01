;;; rudel-infinote-errors.el --- Error data used in the infinote Rudel backend
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, infinote, errors
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
;; This file contains definitions of error conditions used in the
;; Rudel infinote backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-errors)


;;; Error conditions
;;

;; rudel-infinote-no-such-node

(intern "rudel-infinote-no-such-node")

(put 'rudel-infinote-no-such-node 'error-conditions
     '(error
       rudel-error rudel-infinote-no-such-node))

(put 'rudel-infinote-no-such-node 'error-message
     "No such node")

(provide 'rudel-infinote-errors)
;;; rudel-infinote-errors.el ends here
