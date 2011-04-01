;;; rudel-xmpp.el --- XMPP transport backend for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, transport, backend
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
;; This file contains the XMPP transport backend class
;; `rudel-xmpp-backend', which implements transporting XML messages
;; through XMPP connections.


;;; History:
;;
;; 0.1 - initial version


;;; Code:
;;

(require 'rudel-state-machine)

(require 'rudel-backend)
(require 'rudel-transport)
(require 'rudel-transport-util) ;; For `rudel-transport-filter'
(require 'rudel-socket) ;; We instantiate the TCP transport

(require 'rudel-util)

(require 'rudel-xmpp-util)
(require 'rudel-xmpp-state)


;;; Constants
;;

(defconst rudel-xmpp-transport-version '(0 2)
  "Version of the XMPP transport backend for Rudel.")

(defconst rudel-xmpp-protocol-version '(1 0)
  "Version of the XMPP protocol supported by this implementation.")


;;; Class rudel-xmpp-backend
;;

;;;###autoload
(defclass rudel-xmpp-backend (rudel-transport-backend)
  ((capabilities :initform '(connect)))
  "Transport backend works by transporting XMPP messages through
XMPP connections.")

(defmethod initialize-instance ((this rudel-xmpp-backend) slots)
  "Initialize slots and set version of THIS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-xmpp-transport-version))

(defmethod rudel-ask-connect-info ((this rudel-xmpp-backend)
				   &optional info)
  "Augment INFO by reading a hostname and a port number."
  ;; Read server host and port.
  (let ((host (or (plist-get info :host)
		  (read-string "Server: ")))
	(port (or (plist-get info :port)
		  (read-number "Port: ")))
	(jid  (or (plist-get info :jid)
		  (read-string "Jabber ID (JID): "))))
    (append (list :host host
		  :port port
		  :jid  jid)
	    info)))

(defmethod rudel-make-connection ((this rudel-xmpp-backend)
				  info info-callback
				  &optional progress-callback)
  "Connect to an XMPP server using the information in INFO.
INFO has to be a property list containing at least the
keys
+ :host a server name
+ :port TCP port to connect to
+ :jid  jabber id that should be used
If non-nil, PROGRESS-CALLBACK has to be a function which is
called repeatedly to report progress."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:host :jid))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and connect.
  (let* ((host           (plist-get info :host))
	 (jid            (plist-get info :jid))
	 ;; Create the underlying transport.
	 ;; TODO handle errors
	 (tcp-transport  (rudel-make-connection
			  (cdr (rudel-backend-get 'transport 'tcp))
			  info info-callback progress-callback))
	 ;; Create a suitable stack of transport filters on top of the
	 ;; underlying transport.
	 (stack          (rudel-xmpp-make-transport-filter-stack
			  tcp-transport))
	 ;; Create the actual XMPP transport.
	 (xmpp-transport (rudel-xmpp-transport
			 (format "to %s as %s" host jid)
			 :transport stack
			 :start     (list 'new host jid))))

    ;; Now start receiving via the TCP connection and wait until the
    ;; connection has been established.
    (rudel-start tcp-transport)

    (rudel-state-wait xmpp-transport
		      '(established)
		      '(we-finalize they-finalize disconnected)
		      progress-callback)

    ;; Return the usable transport object.
    xmpp-transport))


;;; Class rudel-xmpp-state-new
;;

(defclass rudel-xmpp-state-new (rudel-xmpp-state)
  ()
  "Initial state of new XMPP connections.")

(defmethod rudel-enter ((this rudel-xmpp-state-new) to jid)
  "Switch to \"negotiate-stream\" state."
  (list 'negotiate-stream to jid (list 'sasl-start jid to)))


;;; Class rudel-xmpp-state-negotiate-stream
;;

(defclass rudel-xmpp-state-negotiate-stream (rudel-xmpp-state)
  ((success-state :initarg :success-state
		  :type    (or list symbol)
		  :documentation
		  "State to switch to in case of successful
negotiation."))
  "Stream negotiation state.")

(defmethod rudel-enter ((this rudel-xmpp-state-negotiate-stream)
			to jid success-state)
  "Send opening stream tag constructed with TO and JID."
  ;; Store the name of the successor state in case of successful
  ;; stream negotiation for later.
  (oset this :success-state success-state)

  ;; The first message we receive will be an incomplete XML document
  ;; with root <stream:stream ... >.
  (with-slots (transport) this
    (rudel-set-assembly-function transport #'rudel-xmpp-assemble-stream)
    (rudel-set-generate-function transport #'identity))

  ;; We cannot generate this message by serializing an XML infoset
  ;; since the document is incomplete. We construct it as a string
  ;; instead.
  (rudel-send
   this
   (format "<?xml version=\"1.0\" encoding=\"%s\"?> \
<stream:stream \
xmlns:stream=\"http://etherx.jabber.org/streams\" \
xmlns=\"jabber:client\" \
version=\"%s\" \
to=\"%s\" \
id=\"%s\">"
	   "UTF-8"
	   (mapconcat #'identity
		      (mapcar #'number-to-string
			      rudel-xmpp-protocol-version)
		      ".") ;; TODO rudel-version->string. hm, Emacs
			   ;; has version-to-list, maybe also
			   ;; version-list-to-string?
	   to
	   jid))
  nil)

(defmethod rudel-leave ((this rudel-xmpp-state-negotiate-stream))
  "Stop assembling based on opening stream tag."
  ;; One the stream is negotiated, assemble data based on complete XML
  ;; trees rather than the opening stream tag.
  (with-slots (transport) this
    (rudel-set-assembly-function transport #'rudel-xml-assemble-tags)
    (rudel-set-generate-function transport #'xml->string)))

(defmethod rudel-accept ((this rudel-xmpp-state-negotiate-stream) xml)
  ""
  (cond
   ;; Stream negotiation error.
   ;;((eq (xml-node-name xml) 'stream:stream)
   ;;nil) ;; TODO send error

   ;; Success
   (t
    ;; Extract features from received message and pass them to success
    ;; state.
    (with-slots (success-state) this
      (let ((features (xml-node-children
		       (car (xml-get-children xml 'stream:features)))))
	(if (listp success-state)
	    (append success-state (list features))
	  (list success-state features))))))
  )


;;; Class rudel-xmpp-state-authenticated
;;

;; TODO similar to new state; could use common base class
(defclass rudel-xmpp-state-authenticated (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-authenticated))
  ""
  ;; Switch to negotiate-stream telling it to switch to established in
  ;; case the negotiation succeeds.
  (list 'negotiate-stream "jabber.org" "scymtym" 'established))
;; TODO use real server- and username


;;; Class rudel-xmpp-state-authentication-failed
;;

(defclass rudel-xmpp-state-authentication-failed (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-authentication-failed))
  ""
  'we-finalize)


;;; Class rudel-xmpp-state-established
;;

(defclass rudel-xmpp-state-established (rudel-xmpp-state)
  ()
  "The XMPP connection enters this state when security
negotiation and the negotiation of the actual stream are
complete.")

(defmethod rudel-accept ((this rudel-xmpp-state-established) xml)
  "Store XML in buffer of THIS for later processing."
  (with-slots (shelve-buffer) this
    (push xml shelve-buffer))
  nil)


;;; Class rudel-xmpp-state-idle
;;

(defclass rudel-xmpp-state-idle (rudel-xmpp-state)
  ()
  "The XMPP connection enters this state when security
negotiation and the negotiation of the actual stream are
complete.")

(defmethod rudel-enter ((this rudel-xmpp-state-idle))
  "Process data previously shelved in (the transport owning) THIS."
  (with-slots (filter shelve-buffer) this
    (when filter
      (dolist (item shelve-buffer)
	(funcall filter item)))
    (setq shelve-buffer nil))
  nil)

(defmethod rudel-accept ((this rudel-xmpp-state-idle) xml)
  ""
  (with-slots (filter) this
    (when filter
      (funcall filter xml)))
  nil)


;;; Class rudel-xmpp-state-we-finalize
;;

(defclass rudel-xmpp-state-we-finalize (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-we-finalize))
  ""
  ;; We send the closing tag, </stream:stream>, of the stream
  ;; document. This has be processed as string, not XML.
  (with-slots (transport) this
    (rudel-set-generate-function transport #'identity))

  (rudel-send this "</stream:stream>")

  ;; TODO (rudel-close connection))?
  'disconnected)


;;; Class rudel-xmpp-state-they-finalize
;;

(defclass rudel-xmpp-state-they-finalize (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-they-finalize))
  ""
  (rudel-close this)
  nil)


;;; Class rudel-xmpp-state-disconnected
;;

(defclass rudel-xmpp-state-disconnected (rudel-xmpp-state)
  ()
  "")


;;; XMPP state list
;;

(defvar rudel-xmpp-states
  '(;; Basic XMPP states
    (new                   . rudel-xmpp-state-new)
    (negotiate-stream      . rudel-xmpp-state-negotiate-stream)
    (authenticated         . rudel-xmpp-state-authenticated)
    (authentication-failed . rudel-xmpp-state-authentication-failed)
    (established           . rudel-xmpp-state-established)
    (idle                  . rudel-xmpp-state-idle)
    (we-finalize           . rudel-xmpp-state-we-finalize)
    (they-finalize         . rudel-xmpp-state-they-finalize)
    (disconnected          . rudel-xmpp-state-disconnected))
  "Basic states used in an XMPP connection.
Authentication mechanisms can add more states to this list.")


;;; Class rudel-xmpp-transport
;;

(defclass rudel-xmpp-transport (rudel-state-machine
				rudel-transport-filter)
  ((shelve-buffer :initarg  :shelve-buffer
		  :type     list
		  :initform nil
		  :documentation
		  "Stores parsed data that cannot be processed in
 the current for processing in a successor state."))
  "")

(defmethod initialize-instance ((this rudel-xmpp-transport) slots)
  "Initialize THIS and register states."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states this rudel-xmpp-states)

  ;; Install a handler that passes received data to the user-provided
  ;; handler.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport
       (lambda (data)
	 (rudel-accept this1 data)))))
  )

(defmethod rudel-register-state ((this rudel-xmpp-transport)
				 symbol state)
  "Associate THIS to STATE before registering STATE."
  ;; Associate THIS connection to STATE.
  (oset state :transport this)

  ;; Register the modified STATE.
  (when (next-method-p)
    (call-next-method))
  )

(defmethod rudel-start ((this rudel-xmpp-transport))
  "Start processing by THIS.
Starting the transport can lead to immediate processing of
previously shelved data"
  (rudel-switch this 'idle))

(defmethod rudel-close ((this rudel-xmpp-transport))
  "Close the XMPP connection used by THIS."
  (unless (member (rudel-current-state this)
		  '(we-finalize they-finalize disconnected))
    (rudel-switch this 'we-finalize))

  (rudel-state-wait this '(disconnected))

  (when (next-method-p)
    (call-next-method)) ;; TODO does this call rudel-close again?
  )


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'xmpp 'rudel-xmpp-backend)

(provide 'rudel-xmpp)
;;; rudel-xmpp.el ends here
