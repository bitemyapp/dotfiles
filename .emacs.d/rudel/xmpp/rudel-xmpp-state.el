;;; rudel-xmpp-state.el --- Base class for states used in XMPP connections
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, state machine
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
;; The class `rudel-xmpp-state' is the base class for all states used
;; in XMPP connections.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-util)
(require 'rudel-state-machine)

;; TODO
;; we also need rudel-xmpp.el for `rudel-xmpp-transport' but that
;; would create a circular dependency. It is OK not to require it,
;; since rudel-xmpp.el is always loaded first.


;;; Class rudel-xmpp-state
;;

(defclass rudel-xmpp-state (rudel-state
			    rudel-impersonator
			    rudel-delegator)
  ((impersonation-target-slot :initform 'transport)
   (delegation-target-slot    :initform 'transport)
   (transport                 :initarg  :transport
			      :type     rudel-transport ;; TODO rudel-xmpp-transport?
			      :documentation
			      "The transport class the state
machine of which uses the state object."))
  "Base class for XMPP state classes.")

(defmethod rudel-enter ((this rudel-xmpp-state) &rest args)
  "Default behavior is to stay in the newly entered state."
  nil)

(defmethod rudel-leave ((this rudel-xmpp-state))
  "Default behavior is to do nothing when leaving a state.")

;; TODO choose one
(defmethod rudel-accept ((this rudel-xmpp-state) xml)
  "Default behavior is to accept XML without taking action."
  nil)

(defmethod rudel-accept ((this rudel-xmpp-state) xml)
  ""
  (let ((name (xml-node-name xml)))
    (case name
     ;;
     ;; TODO example
     ;; <stream:error>
     ;; <not-authorized xmlns="urn:ietf:params:xml:ns:xmpp-streams"/>
     ;; </stream:error>
     ('stream:error ;; TODO is this qualified
      'they-finalize)

     ;; we do not accept unexpected messages.
     (t
      'we-finalize)))
  )

(provide 'rudel-xmpp-state)
;;; rudel-xmpp-state.el ends here
