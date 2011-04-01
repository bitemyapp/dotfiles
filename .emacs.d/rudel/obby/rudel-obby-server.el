;;; rudel-obby-server.el --- Server component of the Rudel obby backend
;;
;; Copyright (C) 2008, 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, obby, backend, server
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
;; This file contains the server part of the obby backend for Rudel.
;;
;; It is implemented using one state machine (class
;; `rudel-obby-client') for each client connection. These state
;; machines have the following states:
;;
;; + new                  `rudel-obby-server-state-new'
;; + encryption-negotiate `rudel-obby-server-state-encryption-negotiate'
;; + before-join          `rudel-obby-server-state-before-join'
;; + idle                 `rudel-obby-server-state-idle'


;;; History:
;;
;; 0.3 - Support for transports
;;
;; 0.2 - State machine
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'warnings)

(require 'eieio)

(require 'jupiter)

(require 'rudel-state-machine)

(require 'rudel-obby) ;; to silence the byte-compiler
(require 'rudel-obby-errors)
(require 'rudel-obby-util)
(require 'rudel-obby-state)
(require 'rudel-obby) ;; for `rudel-obby-user' and `rudel-obby-document'


;;; Class rudel-obby-server-state-new
;;

(defclass rudel-obby-server-state-new
  (rudel-obby-server-connection-state)
  ()
  "State in which new connections start out."
  :method-invocation-order :c3)

(defmethod rudel-enter ((this rudel-obby-server-state-new))
  "Sends welcome messages to the client and starts the session
timeout timer."
  ;; Send greeting sequence to the client.
  (rudel-send this
	      "obby_welcome"
	      (number-to-string rudel-obby-protocol-version))

  ;; Switch to encryption negotiation state.
  'encryption-negotiate)


;;; Class rudel-obby-server-state-encryption-negotiate
;;

(defclass rudel-obby-server-state-encryption-negotiate
  (rudel-obby-server-connection-state)
  ()
  "Encryption negotiation state."
  :method-invocation-order :c3)

(defmethod rudel-enter ((this rudel-obby-server-state-encryption-negotiate))
  "Send net6 'encryption' message requesting to not enable encryption."
  (rudel-send this "net6_encryption" "0")
  nil)

(defmethod rudel-obby/net6_encryption_ok
  ((this rudel-obby-server-state-encryption-negotiate))
  "Handle net6 'encryption_ok' message.
Even if the client requests an encrypted connection, we cancel
the negotiation."
  (rudel-send this "net6_encryption_failed")
  'before-join)

(defmethod rudel-obby/net6_encryption_failed
  ((this rudel-obby-server-state-encryption-negotiate))
  "Handle net6 'encryption_failed' message.
No action has to be taken, since the client simply proceeds after
failed encryption negotiation."
  'before-join)


;;; Class rudel-obby-server-state-before-join
;;

(defclass rudel-obby-server-state-before-join
  (rudel-obby-server-connection-state)
  ()
  "Waiting for client request joining the session."
  :method-invocation-order :c3)

(defmethod rudel-obby/net6_client_login
  ((this rudel-obby-server-state-before-join) username color
   &optional global-password user-password)
  "Handle net6 'client_login' message."
  (with-parsed-arguments ((color color))
    (with-slots (server
		 (client-id :id)
		 user
		 encryption) (oref this :connection)
      ;; Make sure USERNAME and COLOR are valid.
      (let ((error (rudel-check-username-and-color
		    server username color)))
	(if error
	    ;; If USERNAME or COLOR are invalid, send the error code
	    ;; to the client and stay in the current state.
	    (progn
	      (rudel-send this
			  "net6_login_failed"
			  (format "%x" error))
	      nil)

	  ;; Create a user object for this client and add it to the
	  ;; server.
	  (setq user (rudel-make-user
		      server
		      username client-id color encryption))
	  (rudel-add-user server user)

	  ;; Broadcast the join event to all clients (including the
	  ;; new one).
	  (with-slots ((name :object-name) color (user-id :user-id)) user
	    (rudel-broadcast this (list 'exclude (oref this :connection))
			     "net6_client_join"
			     (format "%x" client-id)
			     name
			     "0"
			     (format "%x" user-id)
			     (rudel-obby-format-color color)))

	  ;; Get the new client up to date:
	  ;; - transmit user list
	  ;;   - connected users
	  ;;   - disconnected users
	  ;; - transmit document list
	  (with-slots (users clients documents) server
	    ;; Send number of synchronization items: sum of numbers of
	    ;; offline users and documents.
	    (let ((number-of-items (+ (length users) (length documents))))
	      (rudel-send this
			  "obby_sync_init"
			  (format "%x" number-of-items)))

	    ;; Transmit list of connected users.
	    (dolist (client clients)
	      (with-slots ((client-id :id) user) client
		(when user
		  (with-slots ((name    :object-name)
			       color
			       (user-id :user-id)) user
		    (rudel-send this
				"net6_client_join"
				(format "%x" client-id)
				name
				"0"
				(format "%x" user-id)
				(rudel-obby-format-color color))))))

	    ;; Transmit list of disconnected users.
	    (let ((offline-users (remove-if #'rudel-connected users)))
	      (dolist (user offline-users)
		(with-slots ((name :object-name) user-id color) user
		  (rudel-send this
			      "obby_sync_usertable_user"
			      (format "%x" user-id)
			      name
			      (rudel-obby-format-color color)))))

	    ;; Transmit document list
	    (dolist (document documents)
	      (with-slots ((name      :object-name)
			   (doc-id    :id)
			   owner-id
			   suffix
			   subscribed) document
		(apply #'rudel-send
		       this
		       "obby_sync_doclist_document"
		       (format "%x" owner-id)
		       (format "%x" doc-id)
		       name
		       (format "%x" suffix)
		       "UTF-8"
		       (mapcar
			(lambda (user1) ;; TODO we could use `user' here, but there is a bug in cl
			  (format "%x" (rudel-id user1)))
			subscribed)))))

	  (rudel-send this "obby_sync_final")
	  'idle))))
  )


;;; Class rudel-obby-server-state-idle
;;

(defclass rudel-obby-server-state-idle
  (rudel-obby-server-connection-state
   rudel-obby-document-handler)
  ((document-container-slot :initform 'server))
  "Idle state of a server connection.

The connection enters this state when all setup work is finished,
the client has joined the session and no operation is in
progress. In this state, the connection waits for new messages
from the client that initiate operations. Simple (which means
stateless in this case) operations are performed without leaving
the idle state."
  :method-invocation-order :c3)

(defmethod rudel-obby/obby_user_colour
  ((this rudel-obby-server-state-idle) color-)
  "Handle obby 'user_colour' message.
This method is called when the connected user requests a change
of her color to COLOR."
  (with-parsed-arguments ((color- color))
    (with-slots (user) (oref this :connection)
      (with-slots (color (user-id :user-id)) user

	;; Set color slot value and notify the user object.
	(setq color color-)
	(rudel-change-notify user)

	;; Broadcast to other clients.
	(rudel-broadcast this (list 'exclude (oref this :connection))
			 "obby_user_colour"
			 (format "%x" user-id)
			 (rudel-obby-format-color color)))))
  nil)

(defmethod rudel-obby/obby_document_create
  ((this rudel-obby-server-state-idle)
   doc-id name encoding content)
  "Handle obby 'document_create' message."
  (with-parsed-arguments ((doc-id   number)
			  (encoding coding-system))
    (with-slots (user server) (oref this :connection)
      (with-slots ((user-id :user-id)) user
	;; Create a (hidden) buffer for the new document.
	(let* ((buffer         (get-buffer-create
				(generate-new-buffer-name
				 (concat " *"  name "*"))))
	       ;; Create the new document object
	       (document       (rudel-obby-document
				name
				:buffer     buffer
				:subscribed (list user)
				:id         doc-id
				:owner-id   user-id
				:suffix     1)))

	  ;; Initialize the buffer's content
	  (with-current-buffer buffer
	    (insert content))

	  (with-slots (suffix) document
	    ;; Determine an appropriate suffix to provide an unique
	    ;; name for the new document.
	    (while (rudel-find-document server
					(if (= suffix 1)
					    name
					  (format "%s<%d>" name suffix))
					#'string= #'rudel-unique-name)
	      (incf suffix))

	    ;; Add the document to the server's document list
	    (rudel-add-document server document)

	    ;; Maybe notify the creating client of the changed suffix.
	    (unless (= suffix 1)
	      (rudel-send this
			  "obby_document"
			  (format "%x %x" user-id doc-id)
			  "rename"
			  (format "%x" user-id)
			  name
			  (format "%x" suffix)))

	    ;; Notify other clients of the new document
	    (rudel-broadcast this (list 'exclude  (oref this :connection))
			     "obby_document_create"
			     (format "%x" user-id)
			     (format "%x" doc-id)
			     name
			     (format "%x" suffix)
			     (upcase (symbol-name encoding))))

	  ;; Add a jupiter context for (THIS DOCUMENT).
	  (rudel-add-context server (oref this :connection) document))))
    nil)
  )

(defmethod rudel-obby/obby_document/subscribe
  ((this rudel-obby-server-state-idle) document user-id)
  "Handle 'subscribe' submessage of obby 'document' message."
  (with-parsed-arguments ((user-id number))
    (let ((user (with-slots (server) (oref this :connection)
		  (rudel-find-user server user-id
				   #'= #'rudel-id))))
      (with-slots (owner-id (doc-id :id) subscribed buffer) document

	;; Track subscription, handle duplicate subscription requests.
	(when (memq user subscribed)
	  (error "User `%s' already subscribed to document `%s'"
		 (object-name user) (object-name document)))
	(rudel-add-user document user)

	;; Synchronize the buffer content to the client.
	(with-current-buffer buffer
	  ;; Send overall buffer size
	  (rudel-send this
		      "obby_document"
		      (format "%x %x" owner-id doc-id)
		      "sync_init"
		      (format "%x" (1- (position-bytes (point-max)))))

	  ;; Send buffer chunks with author ids
	  (dolist (chunk (rudel-chunks document))
	    (multiple-value-bind (from to author) chunk
	      (let ((string (buffer-substring (+ from 1) (+ to 1))))
		(rudel-send this
			    "obby_document"
			    (format "%x %x" owner-id doc-id)
			    "sync_chunk"
			    string
			    (format "%x"
				    (if author
					(oref author :user-id)
				      0)))))))

	;; Notify clients of the new subscription (including our own
	;; client, who requested the subscription).
	(with-slots ((user-id :user-id)) user
	  (rudel-broadcast this nil
			   "obby_document"
			   (format "%x %x" owner-id doc-id)
			   "subscribe"
			   (format "%x" user-id)))))

    ;; Add a jupiter context for (THIS document).
    (with-slots (server) (oref this :connection)
      (rudel-add-context server (oref this :connection) document))
    nil)
  )

(defmethod rudel-obby/obby_document/unsubscribe
  ((this rudel-obby-server-state-idle) document user-id)
  "Handle 'unsubscribe' submessage of 'obby_document' message."
  (with-parsed-arguments ((user-id number))
    (let ((user (with-slots (server) (oref this :connection)
		  (rudel-find-user server user-id
				   #'= #'rudel-id))))
      (with-slots (owner-id (doc-id :id) subscribed) document

	;; Track subscription, handle invalid unsubscribe requests
	(unless (memq user subscribed)
	  (error "User `%s' not subscribed to document `%s'"
		 (object-name user) (object-name document)))
	(rudel-remove-user document user)

	;; Notify clients of the canceled subscription (including our
	;; own client, who requested being unsubscribed).
	(with-slots ((user-id :user-id)) user
	  (rudel-broadcast this nil
			   "obby_document"
			   (format "%x %x" owner-id doc-id)
			   "unsubscribe"
			   (format "%x" user-id))))

      ;; Remove jupiter context for (THIS DOCUMENT).
      (with-slots (server) (oref this :connection)
	(rudel-remove-context server (oref this :connection) document)))
    nil)
  )

(defmethod rudel-obby/obby_document/record
  ((this rudel-obby-server-state-idle)
   document local-revision remote-revision action &rest arguments)
  "Handle 'record' submessages of 'obby_document' message."
  (with-parsed-arguments ((local-revision  number)
			  (remote-revision number))
    ;; Dispatch to specialized operation handlers.
    (rudel-dispatch
     this "rudel-obby/obby_document/record/" action
     (append (list document local-revision remote-revision)
	     arguments)))
  )

(defmethod rudel-obby/obby_document/record/ins
  ((this rudel-obby-server-state-idle)
   document local-revision remote-revision position data)
  "Handle 'ins' submessage of 'record' submessages of 'obby_document' message."
  (with-parsed-arguments ((position number))
    ;; Construct the operation object and process it.
    (rudel-remote-operation
     (oref this :connection) document
     remote-revision local-revision
     (jupiter-insert
      (format "insert-%d-%d"
	      remote-revision local-revision)
      :from position
      :data data))
    nil)
  )

(defmethod rudel-obby/obby_document/record/del
  ((this rudel-obby-server-state-idle)
   document local-revision remote-revision position length)
  "Handle 'del' submessage of 'record' submessages of 'obby_document' message."
  (with-parsed-arguments ((position number)
			  (length   number))
    ;; Construct the operation object and process it.
    (rudel-remote-operation
     (oref this :connection) document
     remote-revision local-revision
     (jupiter-delete
      (format "delete-%d-%d"
	      remote-revision local-revision)
      :from position
      :to   (+ position length)))
    nil)
  )


;;; Client connection states.
;;

(defvar rudel-obby-server-connection-states
  '((new                  . rudel-obby-server-state-new)
    (encryption-negotiate . rudel-obby-server-state-encryption-negotiate)
    (before-join          . rudel-obby-server-state-before-join)
    (idle                 . rudel-obby-server-state-idle))
  "Name symbols and classes of connection states.")


;;; Class rudel-obby-client
;;

(defclass rudel-obby-client (rudel-state-machine)
  ((transport  :initarg  :transport
	       :type     rudel-transport
	       :documentation
	       "The transport object through which this
connection sends and receives its data.")
   (server     :initarg  :server
	       :type     rudel-obby-server
	       :documentation
	       "")
   (id         :initarg  :id
	       :type     integer
	       :accessor rudel-id
	       :documentation
	       "")
   (user       :initarg  :user
	       :type     (or rudel-obby-user null)
	       :initform nil
	       :documentation
	       "")
   (encryption :initarg  :encryption
	       :type     boolean
	       :documentation
	       ""))
  "Each object of this class represents one client, that is
connected to the server. This object handles all direct
communication with the client, while broadcast messages are
handled by the server.")

(defmethod initialize-instance ((this rudel-obby-client) slots)
  "Initialize slots of THIS, register states and install filter."
  ;; Initialize slots of THIS
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states this rudel-obby-server-connection-states)

  ;; Set up the transport.
  (with-slots (transport) this

    ;; Build the following transport filter stack:
    ;; + `rudel-parsing-transport-filter'
    ;; + `rudel-assembling-transport-filter'
    ;; + TRANSPORT
    (setq transport (rudel-obby-make-transport-filter-stack transport))

    ;; Install process filter and sentinel.
    (lexical-let ((this1 this))
      ;; Install `rudel-accept' as filter to dispatch messages to the
      ;; current state machine state.
      (rudel-set-filter transport
			(lambda (data)
			  (rudel-accept this1 data)))

      ;; Install a sentinel that calls `rudel-close' on THIS upon
      ;; receiving a 'close' event.
      (rudel-set-sentinel transport
			  (lambda (event)
			    (case event
			      (close
			       (rudel-close this1)))))))
  )

(defmethod rudel-register-state ((this rudel-obby-client) symbol state)
  "Register SYMBOL and STATE and set connection slot of STATE."
  ;; Associate THIS connection to STATE.
  (oset state :connection this)

  ;; Register STATE.
  (call-next-method))

(defmethod rudel-end ((this rudel-obby-client))
  ""
  (rudel-disconnect this))

(defmethod rudel-close ((this rudel-obby-client))
  ""
  (with-slots (server) this
    (rudel-remove-client server this)))

(defmethod rudel-send ((this rudel-obby-client) &rest args)
  "Send ARGS through the transport of THIS."
  (with-slots (transport) this
    (rudel-send transport args)))

(defmethod rudel-broadcast ((this rudel-obby-client)
			    receivers name &rest args)
  "Broadcast message NAME with arguments ARGS to RECEIVERS."
  (with-slots (server) this
    (apply #'rudel-broadcast server receivers name args)))

(defmethod rudel-remote-operation ((this rudel-obby-client)
				   document
				   local-revision remote-revision
				   operation)
  "Execute and relay OPERATION on DOCUMENT."
  (with-slots (server user) this
    ;; Transform OPERATION and find clients that need to receive
    ;; notifications.
    (let* ((context     (rudel-find-context server this document))
	   (transformed (jupiter-remote-operation
			 context
			 local-revision remote-revision
			 operation))
	   (receivers   (rudel-subscribed-clients-not-self
			 this document)))

      ;; Relay change notification to other clients. We use
      ;; TRANSFORMED before the byte -> char conversion which is what
      ;; the receivers expect.
      (with-slots (user-id) user
	(with-slots (owner-id (doc-id :id)) document
	  ;; Construct and send messages to all receivers individually
	  ;; since the contents of the messages depends on the state
	  ;; of the jupiter context associated the respective
	  ;; receiver.
	  (dolist (receiver receivers)

	    ;; Find the jupiter context for RECEIVER and use its
	    ;; revision information.
	    (let ((context (rudel-find-context server receiver document)))
	      ;; Construct and send one message.
	      (with-slots (local-revision remote-revision) context
		(apply #'rudel-send
		       receiver
		       "obby_document"
		       (format "%x %x" owner-id doc-id)
		       "record"
		       (format "%x" user-id)
		       (format "%x" local-revision)
		       (format "%x" remote-revision)
		       (rudel-operation->message transformed)))

	      ;; Submit the operation to the jupiter context.
	      (jupiter-local-operation context transformed)))))

      ;; Incorporate change into DOCUMENT (the server-side
      ;; document). We have to convert bytes -> chars before we can do
      ;; this.
      (with-slots (buffer) document
	(rudel-obby-byte->char transformed buffer))

      (rudel-remote-operation document user transformed)))
  )

(defmethod rudel-subscribed-clients-not-self ((this rudel-obby-client)
					      document)
  "Return a list of clients subscribed to DOCUMENT excluding THIS."
  (with-slots (clients) (oref this :server)
    (with-slots (subscribed) document
      (remove-if
       (lambda (client)
	 (with-slots (user) client
	   (or (eq client this)
	       (not (memq user subscribed)))))
       clients)))
  )


;;; Class rudel-obby-server
;;

(defclass rudel-obby-server (rudel-server-session)
  ((listener       :initarg :listener
		   :type    rudel-listener
		   :documentation
		   "The listener object that dispatches incoming
connections to this server.")
   (clients        :initarg  :clients
		   :type     list
		   :initform nil
		   :documentation
		   "List of objects representing clients
connected to the server.")
   (next-client-id :initarg  :next-client-id
		   :type     (integer 1)
		   :initform 1
		   :documentation
		   "An id that will be assigned to the next
client that connects to the server.")
   (next-user-id   :initarg  :next-user-id
		   :type     (integer 1)
		   :initform 1
		   :documentation
		   "An id that will be assigned to the next user
that joins the associated session.")
   (contexts       :initarg  :contexts
		   :type     hash-table
		   :documentation
		   "A hash table associating documents to jupiter
transformation context objects."))
  "Class rudel-obby-server ")

(defmethod initialize-instance ((this rudel-obby-server) slots)
  "Initialize slots of THIS and install a dispatch function."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Create a hash-table to store the contexts.
  (with-slots (contexts) this
    (setq contexts (make-hash-table :test 'equal)))

  ;; Dispatch incoming connections to our `rudel-add-client' method.
  (with-slots (listener) this
    (lexical-let ((this1 this))
      (rudel-set-dispatcher
       listener
       (lambda (client-transport)
	 (rudel-add-client this1 client-transport)))))
  )

(defmethod rudel-end ((this rudel-obby-server))
  ""
  (rudel-disconnect this))

(defmethod rudel-broadcast ((this rudel-obby-server)
			    receivers name &rest arguments)
  "Send a message of type NAME with arguments ARGUMENTS to RECEIVERS.

RECEIVERS can be a object derived from rudel-obby-client, a list
of such objects or a list with car 'exclude and cdr a list of
such objects derived from rudel-obby-client."
  ;; Construct list of receivers.
  (let ((receiver-list
	 (cond
	  ;; If RECEIVERS is nil, the message should be broadcast to
	  ;; all clients.
	  ((null receivers) (oref this :clients))
	  ;; If RECEIVERS is a (non-empty) list of rudel-obby-client
	  ;; (or derived) objects, treat it as a list of receivers.
	  ((and (listp receivers)
		(rudel-obby-client-child-p (car receivers)))
	   receivers)
	  ;; If RECEIVERS is a (non-empty) list with cdr equal to
	  ;; 'exclude treat it as a list of receivers to exclude.
	  ((and (listp receivers)
		(eq (car receivers) 'exclude))
	   (with-slots (clients) this
	     (set-difference clients (cdr receivers)
			     :key #'rudel-id)))
	  ;; If RECEIVERS is a single rudel-obby-client (or derived)
	  ;; object, send the message to that client.
	  ((rudel-obby-client-child-p receivers)
	   (list receivers))
	  ;;
	  (t (signal 'wrong-type-argument (type-of receivers))))))

    ;; Send message to receivers.
    (dolist (receiver receiver-list)
      (apply #'rudel-send receiver name arguments)))
  )

(defmethod rudel-make-user ((this rudel-obby-server)
			    name client-id color encryption)
  ""
  (with-slots (next-user-id) this
    (let ((user (rudel-obby-user name
                 :color      color
		 :client-id  client-id
		 :user-id    next-user-id
		 :connected  t
		 :encryption encryption)))
      (incf next-user-id)
      user))
  )

(defmethod rudel-check-username-and-color ((this rudel-obby-server)
					   username color)
  "Check whether USERNAME and COLOR are valid.
USERNAME must not be empty and must not be used by another
user. COLOR has to be sufficiently different from used colors."
  (cond
   ;; The empty user name is not allowed
   ((string= username "")
    rudel-obby-error-username-invalid)

   ;; Make sure the user name is not already in use.
   ((rudel-find-user this username
		     #'string= #'object-name-string)
    rudel-obby-error-username-in-use)

   ;; Make sure COLOR is a valid color.
   ((not (color-values color))
    rudel-obby-error-color-invalid)

   ;; Make sure the color is sufficiently dissimilar to all used
   ;; colors.
   ((rudel-find-user this color
		     (lambda (left right)
		       (< (color-distance left right) 20000)) ;; TODO constant
		     #'rudel-color)
    rudel-obby-error-color-in-use))
  )

(defmethod rudel-add-client ((this rudel-obby-server)
			     client-transport)
  ""
  (with-slots (next-client-id clients) this
    (let ((client (rudel-obby-client
		   (object-name client-transport)
		   :server     this
		   :transport  client-transport
		   :id         next-client-id
		   :encryption nil)))
      (push client clients))
    (incf next-client-id))
  )

(defmethod rudel-remove-client ((this rudel-obby-server)
				client)
  ""
  (with-slots ((client-id :id) user) client
    ;; Broadcast the part event to all remaining clients.
    (rudel-broadcast this (list 'exclude client)
		     "net6_client_part"
		     (format "%x" client-id))

    ;; If the client has an associated user object, set the status of
    ;; the user object to offline.
    (when user
      ;; Set slot value.
      (with-slots (connected) user
	(setq connected nil))

      ;; Run change hook.
      (object-run-hook-with-args user 'change-hook)))

  (object-remove-from-list this :clients client)
  )

(defmethod rudel-find-context ((this rudel-obby-server) client document)
  "Return the jupiter context associated to (CLIENT DOCUMENT) in THIS."
  (with-slots (contexts) this
    (gethash (rudel-obby-context-key client document) contexts)))

(defmethod rudel-add-context ((this rudel-obby-server) client document)
  "Add a jupiter context for (CLIENT DOCUMENT) to THIS."
  (with-slots (contexts) this
    (with-slots ((client-id :id)) client
      (with-slots ((doc-name :object-name)) document
	(puthash
	 (rudel-obby-context-key client document)
	 (jupiter-context (format "%d-%s" client-id doc-name))
	 contexts))))
  )

(defmethod rudel-remove-context ((this rudel-obby-server) client document)
  "Remove the jupiter context associated to (CLIENT DOCUMENT) from THIS."
  (with-slots (contexts) this
    (remhash
     (rudel-obby-context-key client document)
     contexts)))

(defun rudel-obby-context-key (client document)
  "Generate hash key based on CLIENT and DOCUMENT."
  (with-slots ((client-id :id)) client
    (with-slots ((doc-id :id)) document
      (list client-id doc-id))))

(defmethod object-print ((this rudel-obby-server) &rest strings)
  "Print THIS with number of clients."
  (with-slots (clients) this
    (apply #'call-next-method
	   this
	   (format " clients: %d"
		   (length clients))
	   strings))
  )

(provide 'rudel-obby-server)
;;; rudel-obby-server.el ends here
