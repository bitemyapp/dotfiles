;;; rudel-obby.el --- An obby backend for Rudel
;;
;; Copyright (C) 2008, 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, obby, backend, implementation
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
;; obby protocol (used by the Gobby collaborative editor until version
;; 0.5).


;;; History:
;;
;; 0.3 - Support for transports in client code
;;
;; 0.2 - Refactored client and server to employ state machine
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'rudel)
(require 'rudel-backend)
(require 'rudel-transport)
(require 'rudel-protocol)
(require 'rudel-util)
(require 'rudel-icons)
(require 'rudel-compat) ;; for `read-color' replacement
(require 'rudel-interactive) ;; for read functions


;;; Constants
;;

(defconst rudel-obby-version '(0 3)
  "Version of the obby backend for Rudel.")

(defconst rudel-obby-protocol-version 8
  "Version of the obby protocol this library understands.")

(defconst rudel-obby-default-port 6522
  "Default port used by the obby protocol.")

(defvar rudel-obby-long-message-threshold 32768
  "Threshold for message size, above which messages are sent in
multiple chunks.")

(defvar rudel-obby-long-message-chunk-size 16384
  "Chunk size used, when chunking long messages.")


;;; Class rudel-obby-backend
;;

;;;###autoload
(defclass rudel-obby-backend (rudel-protocol-backend)
  ((capabilities :initform '(join host
			     change-color
			     track-subscriptions
			     encrypt)))
  "Main class of the Rudel obby backend. Creates obby client
connections and creates obby servers.")

(defmethod initialize-instance ((this rudel-obby-backend) slots)
  "Initialize slots of THIS with SLOTS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-obby-version))

(defmethod rudel-ask-connect-info ((this rudel-obby-backend)
				   &optional info)
  "Ask user for the information required to connect to an obby server."
  ;; Read server host and port.
  (let* (;; TODO encryption is also not handled here
	 (encryption        (if (member :encryption info)
				(plist-get info :encryption)
			      (y-or-n-p "Use encryption (Required by Gobby server, not supported by Rudel server)? ")))
	 ;; Read desired username and color
	 (username        (or (plist-get info :username)
			      (rudel-read-user-name)))
	 (color           (or (plist-get info :color)
			      (rudel-read-user-color)))
	 (global-password (if (member :global-password info)
			      (plist-get info :global-password)
			    (rudel-obtain-password
			     'global info "Global password: ")))
	 (user-password   (if (member :user-password info)
			      (plist-get info :user-password)
			    (rudel-obtain-password
			     'user info "User password: "))))
    (append (list :encryption      encryption
		  :username        username
		  :color           color
		  :global-password (unless (string= global-password "")
				     global-password)
		  :user-password   (unless (string= user-password "")
				     user-password))
	    info))
  )

(defmethod rudel-connect ((this rudel-obby-backend) transport
			  info info-callback
			  &optional progress-callback)
  "Connect to an obby server using the information INFO.
Return the connection object."
  ;; Before we start, load the client functionality.
  (require 'rudel-obby-client)

  ;; Create the network process
  (let* ((session    (plist-get info :session))
	 (host       (plist-get info :host)) ;; Just as name
	 (encryption (plist-get info :encryption))
	 (connection (rudel-obby-connection
		      host
		      :session   session
		      :transport transport
		      :info      info)))

    ;; Start the transport and wait until the basic session setup is
    ;; complete.
    (rudel-start transport)

    (rudel-state-wait connection
		      '(waiting-for-join-info)
		      '(we-finalized they-finalize disconnected)
		      progress-callback)

    ;; Wait until we join the session.
    (catch 'connect
      (let ((switch-to (list 'joining info)))
	(while t
	  ;; Request username and/or color when necessary.
	  (unless (and (plist-get info :username)
		       (plist-get info :color))
	    (setq info      (funcall info-callback this info)
		  switch-to (list 'joining info)))

	  ;; Switch connection to specified state to start the login
	  ;; procedure.
	  (when switch-to
	    (apply #'rudel-switch connection switch-to))

	  ;; Wait for the login procedure to succeed or fail.
	  (condition-case error-data
	      ;; When the connection enters state 'idle', the login
	      ;; succeeded; Break out of the while loop then.
	      (progn
		(rudel-state-wait
		 connection
		 '(idle)
		 '(join-failed
		   we-finalized they-finalized disconnected)
		 progress-callback)
		(throw 'connect t))

	    ;; Connection entered error state
	    (rudel-entered-error-state
	     (destructuring-bind (symbol . state) (cdr error-data)
	       (if (eq (rudel-find-state connection 'join-failed) state)

		   ;; For the join-failed state, we can extract
		   ;; details and react accordingly.
		   (case symbol
		     ;; Error state is 'join-failed'
		     (join-failed
		      (with-slots (error-symbol error-data) state
			(message "Login error: %s %s."
				 error-symbol error-data)
			(sleep-for 2)
			(case error-symbol
			  ;; Invalid username; reset it.
			  (rudel-obby-invalid-username
			   (setq info      (plist-put info :username nil)
				 switch-to (list 'joining info)))

			  ;; Username already in use; reset it.
			  (rudel-obby-username-in-use
			   (setq info      (plist-put info :username nil)
				 switch-to (list 'joining info)))

			  ;; Invalid color; reset it.
			  (rudel-obby-invalid-color
			   (setq info      (plist-put info :color nil)
				 switch-to (list 'joining info)))

			  ;; Color already in use; reset it.
			  (rudel-obby-color-in-use
			   (setq info      (plist-put info :color nil)
				 switch-to (list 'joining info)))

			  ;; Unknown error TODO should we signal?
			  (t nil))))

		     ;; Error state is one of {we, they}-finalize
		     ((we-finalized they-finalized)
		      (with-slots (reason) state
			(signal 'rudel-join-error (list reason)))))

		 ;; For all other error states, we just give up.
		 (signal 'rudel-join-error nil))))))))

    ;; The connection is now usable; return it.
    connection))

(defmethod rudel-ask-host-info ((this rudel-obby-backend)
				&optional info)
  "Ask user for information required to host an obby session."
  ;; Read address and port unless they are already specified in INFO.
  (let ((address (or (plist-get info :address)
		     "0.0.0.0"))
	(port    (or (plist-get info :port)
		     (read-number "Port: " rudel-obby-default-port))))
    (append (list
	     :address address
	     :port    port)
	    info))
  )

(defmethod rudel-host ((this rudel-obby-backend) listener info)
  "Host an obby session using the information INFO.
Return the created server."
  ;; Before we start, we load the server functionality.
  (require 'rudel-obby-server)

  ;; Construct and return the server object.
  (rudel-obby-server
   "obby-server"
   :listener listener))

(defmethod rudel-make-document ((this rudel-obby-backend)
				name session)
  "Make a new document in SESSION named NAME.
Return the new document."
  ;; Find an unused document id and create a document with that id.
  (let ((id (rudel-available-document-id this session)))
    (with-slots (user-id) (oref session :self)
      (rudel-obby-document name
			   :session  session
			   :id       id
			   :owner-id user-id
			   :suffix   1)))
  )

(defmethod rudel-available-document-id ((this rudel-obby-backend)
					session)
  "Return a document id, which is not in use in SESSION."
  ;; Look through some candidates until an unused id is hit.
  (let* ((used-ids (with-slots (documents) session
		     (mapcar 'rudel-id documents)))
	 (test-ids (number-sequence 0 (length used-ids))))
    (car (sort (set-difference test-ids used-ids) '<)))
  )


;;; Class rudel-obby-user
;;

(defclass rudel-obby-user (rudel-user)
  ((client-id  :initarg  :client-id
	       :type     (or null integer) ;; We allow nil instead of making
	       :accessor rudel-client-id   ;; the slot unbound, to be able to
	       :initform nil               ;; search with test `rudel-client-id
	       :documentation              ;; without headaches
	       "Id of the client connection, which the user used to log in.
The value is an integer, if the user is connected, and nil
otherwise.")
   (user-id    :initarg  :user-id
	       :type     integer
	       :accessor rudel-id
	       :documentation
	       "")
   (connected  :initarg  :connected
	       :type     boolean
	       :accessor rudel-connected
	       :documentation
	       "")
   (encryption :initarg  :encryption ;; TODO maybe we should use unbound when the user is not connected
	       :type     boolean
	       :documentation
	       ""))
  "Class rudel-obby-user ")

(defmethod eieio-speedbar-description ((this rudel-obby-user))
  "Provide a speedbar description for THIS."
  (let ((connected  (oref this :connected))
	(encryption (if (slot-boundp this :encryption)
			(oref this :encryption)
		      nil)))
    (format "User %s (%s, %s)" (object-name-string this)
	    (if connected  "Online" "Offline")
	    (if encryption "Encryption" "Plain")))
  )

(defmethod eieio-speedbar-object-buttonname ((this rudel-obby-user))
  "Return a string to use as a speedbar button for THIS."
  (rudel-display-string this))


;;; Class rudel-obby-document
;;

(defclass rudel-obby-document (rudel-document)
  ((id       :initarg  :id
	     :type     integer
	     :accessor rudel-id
	     :documentation
	     "The id of this document.
The id has to be unique only with respect to the other documents
owned by the owner.")
   (owner-id :initarg  :owner-id
	     :type     integer
	     :documentation
	     "")
   (suffix   :initarg  :suffix
	     :type     integer
	     :documentation
	     "A counter used to distinguish identically named
documents."))
  "Objects of the class rudel-obby-document represent shared
documents in obby sessions.")

(defmethod rudel-both-ids ((this rudel-obby-document))
  "Return a list consisting of document and owner id of THIS document."
  (with-slots ((doc-id :id) owner-id) this
    (list owner-id doc-id)))

(defmethod rudel-unique-name ((this rudel-obby-document))
  "Generate a unique name for THIS based on the name and the suffix."
  (with-slots (suffix) this
    (concat (when (next-method-p)
	      (call-next-method))
	    (when (> suffix 1)
	      (format "<%d>" suffix))))
  )

(defmethod eieio-speedbar-description ((this rudel-obby-document))
  "Construct a description for from the name of document object THIS."
  (format "Document %s" (object-name-string this)))

(defmethod eieio-speedbar-object-buttonname ((this rudel-obby-document))
  "Return a string to use as a speedbar button for OBJECT."
  (with-slots (subscribed) this
    (format "%-12s %s" (object-name-string this)
	    (if subscribed "s" "-")))
  )


;;; Obby message functions
;;

(defun rudel-obby-replace-in-string (string replacements)
  "Replace elements of REPLACEMENTS in STRING.
REPLACEMENTS is a list of conses whose car is the pattern and
whose cdr is the replacement for the pattern."
  (let ((result string))
    (dolist (replacement replacements)
      (let ((from (car replacement))
	    (to   (cdr replacement)))
	(setq result (replace-regexp-in-string
		      from to result nil t))))
    result)
  )

(defun rudel-obby-escape-string (string)
  "Replace meta characters in STRING with their escape sequences."
  (rudel-obby-replace-in-string
   string
   '(("\\\\" . "\\b") ("\n" . "\\n") (":" . "\\d")))
  )

(defun rudel-obby-unescape-string (string)
  "Replace escaped versions of obby meta characters in STRING with the actual meta characters."
  (rudel-obby-replace-in-string
   string
   '(("\\\\n" . "\n") ("\\\\d" . ":") ("\\\\b" . "\\")))
  )

(defun rudel-obby-parse-color (color)
  "Parse the obby color string COLOR into an Emacs color."
  (let* ((color-numeric (string-to-number color 16))
	 (color-string  (format "#%04X%04X%04X"
				(lsh (logand #xff0000 color-numeric) -08)
				(lsh (logand #x00ff00 color-numeric) -00)
				(lsh (logand #x0000ff color-numeric)  08))))
    color-string)
  )

(defun rudel-obby-format-color (color)
  "Format the Emacs color COLOR as obby color string."
  (multiple-value-bind (red green blue) (color-values color)
    (format "%02x%02x%02x" (lsh red -8) (lsh green -8) (lsh blue -8))))

(defun rudel-obby-assemble-message (name &rest arguments)
  ""
  (concat (mapconcat
	   (lambda (part)
	     (if (and (not (null part)) (stringp part))
		 (rudel-obby-escape-string part)
	       part))
	   (cons name arguments) ":")
	  "\n")
  )

(defun rudel-obby-parse-message (message)
  "Split MESSAGE at `:' and unescape resulting parts.

The terminating `\n' should be removed from MESSAGE before
calling this function."
  (mapcar #'rudel-obby-unescape-string (split-string message ":")))

(defun rudel-obby-generate-message (name-and-args)
  "Transform NAME and arguments into an obby protocol message.

The resulting message is a string that looks like this:
\"NAME:ARG1:ARG2:...:ARGN\\n\""
  (concat (mapconcat
	   (lambda (part)
	     (if (and (not (null part)) (stringp part)) ;; TODO temp
		 (rudel-obby-escape-string part)
	       part))
	   name-and-args ":")
	  "\n")
  )


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'protocol)
		   'obby 'rudel-obby-backend)

;;;###autoload
(eval-after-load 'rudel-zeroconf
  '(rudel-zeroconf-register-service "_lobby._tcp"
				    'start-tls 'obby))

(provide 'rudel-obby)

(require 'rudel-obby-display) ;; define `rudel-display-string'; this
			      ;; is a circular dependency

;;; rudel-obby.el ends here
