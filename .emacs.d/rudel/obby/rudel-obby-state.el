;;; rudel-obby-state.el --- Base class for states used in the obby backend
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, obby, state machine
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
;; This file contains a base class for finite state machine states
;; used in the obby backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'warnings)

(require 'eieio)

(require 'rudel-util)
(require 'rudel-state-machine)

(require 'rudel-obby-util)


;;; Class rudel-obby-state
;;

(defclass rudel-obby-state (rudel-state
			    rudel-impersonator
			    rudel-delegator)
  ((impersonation-target-slot :initform 'connection)
   (delegation-target-slot    :initform 'connection)
   (connection                :initarg  :connection
			      :type     object
			      :documentation
			      "Connection object that uses the
state."))
  "Base class for state classes used in the obby backend."
  :abstract t)

(defmethod rudel-enter ((this rudel-obby-state))
  "Default behavior is doing nothing when entering a state."
  nil)

(defmethod rudel-leave ((this rudel-obby-state))
  "Default behavior is doing nothing when leaving a state.")

(defmethod rudel-accept ((this rudel-obby-state) message)
  "Dispatch to appropriate handler based on MESSAGE.
Display a warning if no such handler is found."
  ;; Try to dispatch to the correct message handler. If there is none,
  ;; warn.
  (let ((name      (car message))
	(arguments (cdr message)))
    (condition-case error
	;; Try to dispatch
	(rudel-dispatch this "rudel-obby/" name arguments)
      ;; Warn if we failed to locate or execute the method. Return nil
      ;; in this case, so we remain in the current state.
      (rudel-dispatch-error
       (progn
	 (display-warning
	  '(rudel obby)
	  (format "%s: no method (%s: %s): `%s/%s'; arguments: %s"
		  (object-print this) (car error) (cdr error)
		  "rudel-obby" name arguments)
	  :debug)
	 nil))))
  )


;;; Class rudel-obby-client-connection-state
;;

(defclass rudel-obby-client-connection-state (rudel-obby-state)
  ()
  "Base class for state classes used by obby client connections."
  :abstract t)

(defmethod rudel-obby/net6_ping ((this rudel-obby-client-connection-state))
  "Handle net6 'ping' message."
  (rudel-send this "net6_pong")
  nil)


;;; Class rudel-obby-server-connection-state
;;

(defclass rudel-obby-server-connection-state (rudel-obby-state)
  ()
  "Base class for server connection states."
  :abstract t)

(defmethod rudel-broadcast ((this rudel-obby-server-connection-state)
			    receivers name &rest arguments)
  "Broadcast message NAME with arguments ARGUMENTS to RECEIVERS."
  (with-slots (connection) this
    (apply #'rudel-broadcast connection receivers name arguments)))


;;; Class rudel-obby-document-handler
;;

(defclass rudel-obby-document-handler ()
  ((document-container-slot :type       symbol
			    :allocation :class
			    :documentation
			    "A symbol specifying the name of the
slot that holds an object on which `rudel-find-document' can be
called to retrieved document object by their ids."))
  "Mixin class that provides ability to process submessages of
obby 'document' messages."
  :abstract t)

(defmethod rudel-obby/obby_document
  ((this rudel-obby-document-handler) doc-id action &rest arguments)
  "Handle obby 'document' message family."
  ;; Try to dispatch to the correct message handler. If there is none,
  ;; warn.
  (with-parsed-arguments ((doc-id document-id))
    ;; Locate the document based on owner id and document id.
    (let* ((container (slot-value (oref this :connection)
				  (oref this document-container-slot)))
	   (document  (rudel-find-document container doc-id
					   #'equal #'rudel-both-ids)))
      (if document
	  (condition-case error
	      ;; Try to dispatch
	      (rudel-dispatch this "rudel-obby/obby_document/" action
			      (cons document arguments))
	    ;; Warn if we failed to locate or execute the
	    ;; method. Return nil in this case, so we remain in the
	    ;; current state.
	    (rudel-dispatch-error
	     (progn
	       (display-warning
		'(rudel obby)
		(format "%s: no method (%s: %s): `%s/%s'; arguments: %s"
			(object-print this) (car error) (cdr error)
			"rudel-obby/obby_document/" action arguments)
		:debug)
	       nil)))
	;; If we did not find the document, warn.
	(progn
	  (display-warning
	   '(rudel obby)
	   (format "Could not find document: `%s'" doc-id)
	   :debug)
	  nil))))
  )

(provide 'rudel-obby-state)
;;; rudel-obby-state.el ends here
