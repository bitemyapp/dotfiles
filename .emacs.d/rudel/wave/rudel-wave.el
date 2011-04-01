;;; rudel-wave.el --- A Wave backend for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, Wave protocol, backend
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
;; This file contains a Rudel protocol backend, which implements the
;; Wave client protocol.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-backend)
(require 'rudel-protocol)


;;; Constants
;;

(defconst rudel-wave-version '(0 1)
  "Version of the wave backend for Rudel.")


;;; Class rudel-wave-backend
;;

;;;###autoload
(defclass rudel-wave-backend (rudel-protocol-backend)
  ((capabilities :initform '(join
			     chat
			     track-subscriptions)))
  "Main class of the Rudel Wave backend. Creates wave client
connections.")

(defmethod initialize-instance ((this rudel-wave-backend) slots)
  "Initialize slots of THIS with SLOTS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-wave-version))


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'protocol)
		   'wave 'rudel-wave-backend)

(provide 'rudel-wave)
;;; rudel-wave.el ends here
