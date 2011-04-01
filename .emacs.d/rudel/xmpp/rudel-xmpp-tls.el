;;; rudel-xmpp-tls.el --- TLS support for XMPP connections
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, tls, encryption
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
;; This file contains the implementation of TLS encryption for XMPP
;; connections.


;;; History:
;;
;; 0.1 - initial version


;;; Code:
;;

(require 'rudel-xmpp-state)


;;; Class rudel-xmpp-state-tls-start
;;

(defclass rudel-xmpp-state-tls-start (rudel-xmpp-state)
  ()
  "State used to enable TLS encryption for a connection.")

(defmethod rudel-enter ((this rudel-xmpp-state-tls-start))
  "Enable TLS encryption for the connection associated with THIS."
  (require 'rudel-tls)
  ;; something like this: (rudel-tls-start-tls transport)
  'authenticated)


;;; TLS state list
;;

(defvar rudel-xmpp-tls-states
  '((start-tls . rudel-xmpp-state-tls-start))
  "")

(dolist (state rudel-xmpp-tls-states)
  (add-to-list 'rudel-xmpp-states state))

(provide 'rudel-xmpp-tls)
;;; rudel-xmpp-tls.el ends here
