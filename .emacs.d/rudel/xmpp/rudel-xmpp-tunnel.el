;;; rudel-xmpp-tunnel.el --- XMPP tunnel transport backend for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, tunnel, transport, backend
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


;;; History:
;;
;; 0.1 - initial version


;;; Code:
;;

(require 'rudel-backend)
(require 'rudel-xml)

(require 'rudel-xmpp)


;;; Class rudel-xmpp-tunnel-transport
;;

(defclass rudel-xmpp-tunnel-transport (rudel-xmpp-transport)
  ()
  "Transport backend that tunnels any kind of data (not
necessarily XML) through an XMPP connection.")

(defmethod rudel-send ((this rudel-xmpp-tunnel-transport) data)
  ""
  (let ((encoded (base64-encode-string data)))
    (rudel-send this `(("data") ,encoded))))

(defmethod rudel-message ((this rudel-xmpp-tunnel-transport) xml)
  ""
  (with-tag-attrs (data) xml
    (let ((decoded (base64-decode-string data)))
      (rudel-accept this decoded))))


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'xmpp 'rudel-xmpp-tunnel-backend)

(provide 'rudel-xmpp-tunnel)
;;; rudel-xmpp-tunnel.el ends here
