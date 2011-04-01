;;; rudel-xmpp-sasl.el --- SASL mechanism for the Rudel XMPP backend
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, sasl, authentication
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
;; This file defines additional XMPP connection states that implement
;; the SASL authentication mechanism using Emacs' sasl library.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'xml)
(require 'sasl)

(require 'eieio)

(require 'rudel-xmpp-state)


;;; Class rudel-xmpp-state-sasl-start
;;

(defclass rudel-xmpp-state-sasl-start (rudel-xmpp-state)
  ()
  "Start state of the SASL negotiation.")

(defmethod rudel-enter ((this rudel-xmpp-state-sasl-start)
			name server features)
  "Extract the list of supported mechanisms from FEATURES.
Then switch to the try one state to try them in order."
  ;; Find mechanism tags
  (let* ((mechanism-tags (remove* 'mechanisms features
				  :test-not #'eq
				  :key      #'xml-node-name))
	 ;; XML -> alist
	 (mechanisms
	  (apply #'append
		 (mapcar
		  (lambda (mechanisms)
		    (let ((schema (or (xml-get-attribute mechanisms 'xmlns)
				      "urn:ietf:params:xml:ns:xmpp-sasl")))
		      (mapcar
		       (lambda (mechanism)
			 (list schema
			       (car (xml-node-children mechanism))))
		       (xml-node-children mechanisms))))
		  mechanism-tags))))

    ;; Try the first mechanism
    (list 'sasl-try-one name server mechanisms))
  )


;;; Class rudel-xmpp-state-sasl-try-one
;;

(defclass rudel-xmpp-state-sasl-try-one (rudel-xmpp-state)
  ()
  "State that selects a mechanism and switches to the mechanism
start state for that mechanism.")

(defmethod rudel-enter ((this rudel-xmpp-state-sasl-try-one)
			name server mechanisms)
  "If Emacs support the first mechanism in MECHANISMS, try it, otherwise skip it.
Mechanism are tried by switching to the mechanism start state.
When no mechanisms are left, switch to the authentication failed state."
  ;; If there are mechanism on the list, try them, otherwise fail.
  (if mechanisms
      (destructuring-bind (schema mechanism-name) (car mechanisms)
	;; If Emacs supports the head of the mechanism list, try it,
	;; otherwise go with the tail.
	(let ((mechanism (sasl-find-mechanism (list mechanism-name))))
	  (if mechanism
	      (list 'sasl-mechanism-start
		    name server schema mechanism (cdr mechanisms))
	    (list 'sasl-try-one name server (cdr mechanisms)))))
    'authentication-failed)
  )


;;; Class rudel-xmpp-state-sasl-mechanism-start
;;

(defclass rudel-xmpp-state-sasl-mechanism-start (rudel-xmpp-state)
  ((schema    :initarg :schema
	      :type    string
	      :documentation
	      "")
   (mechanism :initarg :mechanism
	      :type    vector
	      :documentation
	      "")
   (rest      :initarg :rest
	      :type    list
	      :documentation
	      ""))
  "Start state of the negotiation for the selected mechanism.")

(defmethod rudel-enter ((this rudel-xmpp-state-sasl-mechanism-start)
			name1 server1 schema1 mechanism1 rest1)
  ""
  (with-slots (schema mechanism rest) this
    (setq schema    schema1
	  mechanism mechanism1
	  rest      rest1)

    (let* ((client (sasl-make-client mechanism name1 "xmpp" server1))
	   (step   (sasl-next-step client nil))
	   (name   (sasl-mechanism-name mechanism)))

      ;; Send initial 'auth' message, possibly containing initial
      ;; response data.
      (let* ((response-data-raw (sasl-step-data step))
	     (response-data     (when response-data-raw
				  (base64-encode-string
				   response-data-raw t))))
	(rudel-send this
		    `(auth
		      ((xmlns     . ,schema)
		       (mechanism . ,name))
		      ,@(when response-data
			  (list response-data)))))

      ;; Construct the initial SASL step for the mechanism and start
      ;; the challenge/response sequence.
      (list 'sasl-mechanism-step name1 server1 schema client step rest)))
  )


;;; Class rudel-xmpp-state-sasl-mechanism-step
;;

(defclass rudel-xmpp-state-sasl-mechanism-step (rudel-xmpp-state)
  ((name   :initarg :name
	   :type    string
	   :documentation
	   "Username used in SASL authentication mechanism.")
   (server :initarg :server
	   :type    string
	   :documentation
	   "Server name used in SASL authentication mechanism.")
   (schema :initarg :schema
	   :type    string
	   :documentation
	   "Schema URN identifying the SASL mechanism.")
   (client :initarg :client
	   :type    vector
	   :documentation
	   "SASL mechanism data.")
   (step   :initarg :step
	   :type    vector
	   :documentation
	   "SASL mechanism state data.")
   (rest   :initarg :rest
	   :type    list
	   :documentation
	   "List of remaining mechanisms to try."))
  "Intermediate step of the negotiation for the selected
mechanism.")

(defmethod rudel-enter ((this rudel-xmpp-state-sasl-mechanism-step)
			name1 server1 schema1 client1 step1 rest1)
  "Store SCHEMA1, CLIENT1, STEP1 and REST1 for later use."
  (with-slots (name server schema client step rest) this
    (setq name   name1
	  server server1
	  schema schema1
	  client client1
	  step   step1
	  rest   rest1))
  nil)

(defmethod rudel-accept ((this rudel-xmpp-state-sasl-mechanism-step) xml)
  "Interpret XML to decide how to proceed with the authentication mechanism."
  (case (xml-node-name xml)
    ;; Authentication mechanism failed.
    (failure
     (let ((child (car-safe (xml-node-children xml))))
       (case (xml-node-name child)

	;; The id chosen for identification was not accepted (example:
	;; incorrectly formatted user id).
	(invalid-authzid
	 (with-slots (name server rest) this
	   (list 'sasl-try-one name server rest))) ;; TODO how do we react?

	;; The not-authorized failure means that the credentials we
	;; provided were wrong.
	('not-authorized
	 (with-slots (name server rest) this
	   (list 'sasl-try-one name server rest))) ;; TODO how do we react?

	;; Default behavior is to try next mechanism.
	;;
	;; Not handled explicitly: <aborted/>, <incorrect-encoding/>,
	;; <invalid-mechanism/>, <mechanism-too-weak/>,
	;; <temporary-auth-failure/>
	(t
	 (with-slots (name server rest) this
	   (list 'sasl-try-one name server rest))))))

    ;; Authentication mechanism succeeded. Switch to authenticated
    ;; state.
    (success
     'authenticated)

    ;; Authentication mechanism requires a challenge-response
    ;; step. The Emacs SASL implementation does the heavy lifting for
    ;; us.
    (challenge
     ;; TODO is the challenge data always there?
     (with-slots (name server schema client step rest) this
       ;; TODO assert string= schema (xml-node-attr xml "xmlns")

       ;; Pass challenge data, if any, to current step.
       (when (stringp (car-safe (xml-node-children xml)))
	 (let ((challenge-data (base64-decode-string
				(car (xml-node-children xml)))))
	   (sasl-step-set-data step challenge-data)))

       ;; Proceed to next step and send response, possibly with
       ;; response data.
       (let* ((sasl-read-passphrase (lexical-let ((this1 this))
				      (lambda (prompt)
					(rudel-obtain-sasl-password
					 this1 prompt))))
	      (next                 (sasl-next-step client step)))
	 (if next
	     ;; If there is another step, send a 'response' element,
	     ;; possibly containing the response data.
	     (progn
	       (let* ((response-data-raw (sasl-step-data next))
		      (response-data     (when response-data-raw
					   (base64-encode-string
					    response-data-raw t))))
		 (rudel-send this
			     `(response
			       ,@(when schema
				   `(((xmlns . ,schema))))
			       ,@(when response-data
				   (list response-data)))))

	       (list 'sasl-mechanism-step
		     name server schema client next rest))
	   ;; If there is no next step, try the next mechanism.
	   (list 'sasl-try-one name server rest)))))

    ;; Unknown message.
    (t
     nil)) ;; TODO send error or call-next-method?
  )

(defmethod rudel-obtain-sasl-password
  ((this rudel-xmpp-state-sasl-mechanism-step) prompt)
  "Replaces prompt function of the sasl library.
This function adds all available context information to the
password request and passes it to `rudel-obtain-password'."
  (with-slots (name server schema) this
    (rudel-obtain-password
     'xmpp-sasl
     (list
      :host     server
      :port     5222 ;; TODO this one could be wrong
      :schema   schema
      :username name)
     prompt))
  )


;;; SASL state list
;;

(defvar rudel-xmpp-sasl-states
  '((sasl-start           . rudel-xmpp-state-sasl-start)
    (sasl-try-one         . rudel-xmpp-state-sasl-try-one)
    (sasl-mechanism-start . rudel-xmpp-state-sasl-mechanism-start)
    (sasl-mechanism-step  . rudel-xmpp-state-sasl-mechanism-step))
  "States used during SASL authentication.")

(eval-after-load "rudel-xmpp"
  '(dolist (state rudel-xmpp-sasl-states)
     (add-to-list 'rudel-xmpp-states state)))

(provide 'rudel-xmpp-sasl)
;;; rudel-xmpp-sasl.el ends here
