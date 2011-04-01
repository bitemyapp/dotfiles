;;; rudel-xmpp-debug.el --- Debugging functions for the Rudel XMPP backend
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, debug
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
;; Debugging functions for the Rudel XMPP transport backend.


;;; History:
;;
;; 0.2 - New debugging infrastructure
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-xml)
(require 'rudel-debug)

(require 'rudel-xmpp)
(require 'rudel-xmpp-util)
(require 'rudel-xmpp-state)
(require 'rudel-xmpp-sasl)


;;; All XMPP states
;;

(defmethod rudel-debug-target ((this rudel-xmpp-state))
  "Return debug target of the transport as debug target for THIS."
  (with-slots (transport) this
    (rudel-debug-target transport)))


;;; Handle base64 encoded data in SASL steps
;;

(defmethod rudel-send ((this rudel-xmpp-state-sasl-mechanism-step)
		       &rest args)
  "Delegate sending ARGS to the transport associated with THIS."
  ;; We need this primary method in order for the :after method below
  ;; to work as intended. Without this method, the :after method would
  ;; get called and `no-applicable-method' would not get called.
  (with-slots (transport) this
    (apply #'rudel-send transport args)))

(defmethod rudel-send :after
  ((this rudel-xmpp-state-sasl-mechanism-step) xml)
  "Show base64-decoded version of XML."
  (when (and (eq (xml-node-name xml) 'response)
	     (car-safe (xml-node-children xml)))
    (mapc
     (lambda (pair)
       (rudel-debug-write
	this
	:sent
	"RESPDATA"
	(if (find ?= pair)
	    (apply #'format "%-16s: %s" (split-string pair "="))
	  pair)))
     (split-string
      (base64-decode-string (car (xml-node-children xml)))
      ",")))
  )

(defmethod rudel-accept :before
  ((this rudel-xmpp-state-sasl-mechanism-step) xml)
  "Show base64-decoded version of XML."
  (when (and (eq (xml-node-name xml) 'challenge)
	     (stringp (car-safe (xml-node-children xml))))
    (mapc
     (lambda (pair)
       (rudel-debug-write
	this
	:received
	"CHALDATA"
	(if (find ?= pair)
	    (apply #'format "%-16s: %s" (split-string pair "="))
	  pair)))
     (split-string
      (base64-decode-string (car (xml-node-children xml)))
      ",")))
  )

(provide 'rudel-xmpp-debug)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; rudel-xmpp-debug.el ends here
