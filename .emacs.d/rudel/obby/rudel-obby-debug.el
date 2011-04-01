;;; rudel-obby-debug.el --- Debugging functions for obby backend
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, obby, debugging
;; X-RCS: $Id:$
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; Debugging functions for the obby backend.


;;; History:
;;
;; 0.2 - New debug infrastructure
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-debug)

(require 'rudel-obby-client)
(require 'rudel-obby-server)


;;; Client connection debugging
;;

(defmethod rudel-debug-target ((this rudel-obby-connection))
  "Return debug target of the transport as debug target for THIS."
  (with-slots (transport) this
    (rudel-debug-target transport)))


;;; Server connection debugging
;;

(defmethod rudel-debug-target ((this rudel-obby-client))
  "Return debug target of the transport as debug target for THIS."
  (with-slots (transport) this
    (rudel-debug-target transport)))

(provide 'rudel-obby-debug)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; rudel-obby-debug.el ends here
