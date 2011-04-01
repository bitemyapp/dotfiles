;;; rudel-tcp.el --- socket transport backend for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, socket, transport, backend
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
;; Socket transport backend for Rudel.


;;; History:
;;
;; 0.2 - Use underlying socket directly
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl)) ;; for `lexical-let' and `every'

(require 'rudel-backend)
(require 'rudel-transport)


;;; Constants
;;

(defconst rudel-tcp-version '(0 2)
  "Version of the TCP transport for Rudel.")


;;; Class rudel-socket-transport
;;

(defclass rudel-socket-transport (rudel-transport)
  ((socket   :initarg  :socket
	     :type     process
	     :documentation
	     "The socket represented by this transport object.")
   (filter   :initarg  :filter
	     :type     (or null function)
	     :initform nil
	     :reader   rudel-filter
	     :writer   rudel-set-filter
	     :documentation
	     "The filter function. This function is not directly
installed into the underlying process and therefore has to be
stored separately.")
   (sentinel :initarg  :sentinel
	     :type     (or null function)
	     :initform nil
	     :reader   rudel-sentinel
	     :writer   rudel-set-sentinel
	     :documentation
	     "The sentinel function. This function is not
directly installed into the underlying process and therefore has
to be stored separately."))
  "Objects of this class use sockets to transport data.")

(defmethod initialize-instance :after ((this rudel-socket-transport) slots)
  "Install process filter and sentinel for THIS."
  (with-slots (socket) this
    (lexical-let ((this1 this))
      (set-process-filter
       socket (lambda (process data)
		(with-slots (filter) this1
		  (when filter
		    (funcall filter data)))))

      (set-process-sentinel
       socket (lambda (process message)
		(with-slots (sentinel) this1
		  (when sentinel
		    (case (process-status process)
		      ;; Nothing to do here.
		      (run
		       nil)

		      ;; Dispatch events which indicate the
		      ;; termination of the connection to the
		      ;; sentinel.
		      ((closed failed exit finished)
		       (funcall sentinel 'close)))))))))
  )

(defmethod rudel-send ((this rudel-socket-transport) data)
  "Send DATA through THIS."
  (with-slots (socket) this
    (process-send-string socket data)))

(defmethod rudel-close ((this rudel-socket-transport))
  "Close THIS."
  (with-slots (socket) this
    (delete-process socket)))

(defmethod rudel-start ((this rudel-socket-transport))
  "Start THIS after it has been suspended."
  (with-slots (socket) this
    (continue-process socket)))


;;; Class rudel-socket-listener
;;

(defclass rudel-socket-listener (rudel-listener)
  ((socket   :initarg  :socket
	     :type     (or null process)
	     :initform nil
	     :documentation
	     "The server socket represented by this listener
object.")
   (dispatch :initarg  :dispatch
	     :type     (or null function)
	     :writer   rudel-set-dispatcher
	     :documentation
	     "Function called for incoming connections.
The dispatch function has to accept a single argument which will
be a transport object representing the incoming connection."))
  "")

(defmethod rudel-close ((this rudel-socket-listener))
  "Make THIS stop listening for incoming connections."
  (with-slots (socket) this
    (delete-process socket)))

(defmethod rudel-handle-connect ((this rudel-socket-listener) socket)
  "Handle incoming connection SOCKET."
  (with-slots (dispatch) this
    (when dispatch
      ;; Wrap SOCKET in a transport object. Pass the constructed
      ;; object to the dispatch function.
      (let ((transport (rudel-socket-transport
			(format
			 "TCP from %s"
			 (format-network-address
			  (process-contact socket :remote)))
			:socket socket)))
	(funcall dispatch transport))))
  )


;;; Class rudel-tcp-backend
;;

;;;###autoload
(defclass rudel-tcp-backend (rudel-transport-backend)
  ((capabilities :initform (listen connect)))
  "TCP transport backend.
The transport backend is a factory for TCP transport objects.")

(defmethod initialize-instance ((this rudel-tcp-backend) slots)
  "Initialize slots and set version of THIS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-tcp-version))

(defvar rudel-tcp-ask-connect-info-host-history nil
  "History of hosts read by TCP backend's `rudel-ask-connect-info'.")

(defvar rudel-tcp-ask-connect-info-port-last nil
  "Last port read by TCP backend's `rudel-ask-connect-info'.")

(defmethod rudel-ask-connect-info ((this rudel-tcp-backend)
				   &optional info)
  "Augment INFO by read a hostname and a port number."
  ;; Read server host and port.
  (let ((host (or (plist-get info :host)
		  (read-string
		   (if (car rudel-tcp-ask-connect-info-host-history)
		       (format "Server (default %s): "
			       (car rudel-tcp-ask-connect-info-host-history))
		     "Server: ")
		   nil
		   'rudel-tcp-ask-connect-info-host-history
		   (car rudel-tcp-ask-connect-info-host-history))))
	(port (or (plist-get info :port)
		  (setq
		   rudel-tcp-ask-connect-info-port-last
		   (read-number
		    "Port: "
		    rudel-tcp-ask-connect-info-port-last))))) ;; TODO rudel-read-port PROMPT CATEGORY ?
    (append (list :host host
		  :port port)
	    info)))

(defmethod rudel-make-connection ((this rudel-tcp-backend)
				  info info-callback
				  &optional progress-callback)
  "Connect to a TCP server using the information in INFO.
INFO has to be a property list containing the keys :host
and :port."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:host :port))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and create the socket.
  (let* ((host   (plist-get info :host))
	 (port   (plist-get info :port))
	 ;; Create the network process
	 (socket (make-network-process
		  :name     (format "TCP to %s" host)
		  :host     host
		  :service  port
		  :stop     t
		  :noquery  t)))
    (rudel-socket-transport
     (format "to %s:%s" host port)
     :socket socket))
  )

(defmethod rudel-wait-for-connections ((this rudel-tcp-backend)
				       info info-callback)
  "Create TCP server according to INFO.
INFO has to be a property list containing the key :port."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:port))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and create the socket.
  (let* ((address  (plist-get info :address))
	 (port     (plist-get info :port))
	 ;; Create the listener object; without process for now.
	 (listener (rudel-socket-listener
		    (format "on %s:%s" (or address "*") port)))
	 ;; Create the network process.
	 (socket   (lexical-let ((listener1 listener))
		     (apply
		      #'make-network-process
		      :name     (format "TCP on %s" port)
		      :service  port
		      :server   t
		      :noquery  t
		      :filter   #'ignore
		      :sentinel #'ignore
		      :log
		      (lambda (server socket message)
			(rudel-handle-connect listener1 socket))
		      (when address
			(list :host address))))))
    ;; Return the listener.
    (oset listener :socket socket)
    listener))


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'tcp 'rudel-tcp-backend)

(provide 'rudel-socket)
;;; rudel-socket.el ends here
