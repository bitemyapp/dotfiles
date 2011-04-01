;;; rudel-zeroconf.el --- Zeroconf support for Rudel
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, service, discovery, advertising, zeroconf,
;;           rendezvous, avahi
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
;; Zeroconf session discovery and advertising for Rudel. The main
;; class `rudel-zeroconf-backend' implements discovery and advertising
;; for registered service types. Service types are registered by
;; adding an element of the form
;; (SERVICE TRANSPORT-BACKEND PROTOCOL-BACKEND)
;; to the `rudel-zeroconf-service-types'. TRANSPORT-BACKEND is the
;; symbol of the transport backend (see `rudel-transport-backend'),
;; PROTOCOL-BACKEND is the symbol of the protocol backend (see
;; `rudel-protocol-backend'), and SERVICE is the string used as
;; service type in the Zeroconf record (example: '("_lobby._tcp" tcp
;; obby)).


;;; History:
;;
;; 0.2 - Support for transport and protocol backends
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl)) ;; first, second, third

(require 'zeroconf)

(require 'rudel-backend)
(require 'rudel-session-initiation)


;;; Constants and global variables
;;

(defconst rudel-zeroconf-version '(0 2)
  "Version of the Zeroconf backend for Rudel.")

(defvar rudel-zeroconf-service-types nil
  "Service types used by Rudel backends.
Each element is of the form
 (SERVICE TRANSPORT-BACKEND PROTOCOL-BACKEND).")


;;; Accessors and manipulators for the service list
;;

(defalias 'rudel-zeroconf-service-type 'first
  "Return type of service.")

(defalias 'rudel-zeroconf-transport-backend 'second
  "Return transport backend associated with service type.")

(defalias 'rudel-zeroconf-protocol-backend 'third
  "Return protocol backend associated with service type.")

(defun rudel-zeroconf-service (key which)
  "Return the Zeroconf service type used by BACKEND."
  (find which rudel-zeroconf-service-types
	:key key :test (if (eq key 'rudel-zeroconf-service-type)
			   #'string= #'eq)))

;;;###autoload
(defun rudel-zeroconf-register-service
  (type transport-backend protocol-backend)
  "Add an entry for TYPE with TRANSPORT-BACKEND and PROTOCOL-BACKEND to the list of service types.
TRANSPORT-BACKEND is the name of the transport backend handling
the service type TYPE.
PROTOCOL-BACKEND is the name of the protocol backend handling the
service type TYPE."
  (add-to-list 'rudel-zeroconf-service-types
	       (list type transport-backend protocol-backend)))


;;; Initialization
;;

(message "Initializing Zeroconf ...")
(zeroconf-init)


;;; Class rudel-zeroconf-backend
;;

;;;###autoload
(defclass rudel-zeroconf-backend (rudel-session-initiation-backend)
  ((capabilities :initform (discover advertise))
   (priority     :initform primary))
  "")

(defmethod initialize-instance ((this rudel-zeroconf-backend) slots)
  "Initialize slots of THIS with SLOTS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-zeroconf-version))

(defmethod rudel-discover ((this rudel-zeroconf-backend))
  "Return a list of session information property lists for Zeroconf-advertised sessions."
  (mapcar
   #'rudel-zeroconf-service->plist
   (remove-if
    #'null
    (mapcar
     #'zeroconf-resolve-service
     (apply
      #'append
      (mapcar
       #'rudel-zeroconf-services
       rudel-zeroconf-service-types)))))
  )

(defmethod rudel-advertise ((this rudel-session-initiation-backend) info)
  "Use Zeroconf to advertise the session described by INFO to other users."
  (let ((name              (plist-get info :name))
	(transport-backend (plist-get info :transport-backend))
	(protocol-backend  (plist-get info :protocol-backend))
	(host              (plist-get info :host))
	(port              (plist-get info :port))
	(data              (plist-get info :data)))
    (when protocol-backend
      (apply #'rudel-zeroconf-publish
	     transport-backend protocol-backend name host port data)))
  t)

(defmethod rudel-withdraw ((this rudel-session-initiation-backend))
  "Withdraw Zeroconf record."
  (error "Not implemented, yet"))


;;; Zeroconf wrapper functions
;;

(defun rudel-zeroconf-services (service)
  "List Zeroconf services for BACKEND."
  (zeroconf-list-services
   (rudel-zeroconf-service-type service)))

(defun rudel-zeroconf-services-present-p (service)
  "Check whether there are Zeroconf services for BACKEND."
  (rudel-zeroconf-services service))

(defun rudel-zeroconf-publish (transport-backend protocol-backend
			       name host port &rest data)
  "Publish PROTOCOL-BACKEND over TRANSPORT-BACKEND service NAME for HOST and PORT."
  ;; Try to find the service entry for the protocol backend and
  ;; publish the service if an entry is found.
  (let ((service (rudel-zeroconf-service
		  #'rudel-zeroconf-protocol-backend protocol-backend)))
    (when service
      (zeroconf-publish-service
       name
       (rudel-zeroconf-service-type service)
       "local"
       (concat host ".local")
       port
       "" ; address
       (mapcar
	(lambda (item)
	  (concat (car item) "=" (cdr item)))
	data))))
  )

(defun rudel-zeroconf-withdraw (backend name)
  "Withdraw service NAME for BACKEND."
  (error "Not implemented, yet"))


;;; Miscellaneous functions
;;

(defun rudel-zeroconf-service->plist (service)
  "Convert a Zeroconf service record to an info property list."
  (let* ((type         (zeroconf-service-type service))
	 (data         (rudel-zeroconf-parse-txt-record
			(zeroconf-service-txt service)))
	 (service-type (rudel-zeroconf-service
			#'rudel-zeroconf-service-type type)))
    ;; Construct information property list.
    (list
     :name              (format "Zeroconf advertised session \"%s\""
				(zeroconf-service-name service))
     :transport-backend (rudel-backend-get
			 'transport
			 (rudel-zeroconf-transport-backend service-type))
     :protocol-backend  (rudel-backend-get
			 'protocol
			 (rudel-zeroconf-protocol-backend service-type))
     :host              (zeroconf-service-host service)
     :port              (zeroconf-service-port service)
     ;; Encryption defaults to yes to be compatible with Gobby.
     :encryption        (or (not (member :encryption data))
			    (string= (plist-get data :encryption)
				     "yes"))))
  )

(defun rudel-zeroconf-parse-txt-record (record)
  "Parse RECORD into a property list of keys and values."
  (apply #'append
	 (mapcar
	  (lambda (entry)
	    (multiple-value-bind (key value) (split-string entry "=")
	      (list (intern (concat ":" key))
		    value)))
	  record))
  )


;;; User interaction
;;

(defun rudel-zeroconf-read-service (backend)
  "Retrieve services for BACKEND and read one from user."
  ;; First, find all matching services for the backend.
  (let* ((services         (rudel-zeroconf-services backend))
	 ;; Read one of the names of these services.
	 (service-name     (completing-read
			    "Service: "
			    (mapcar #'zeroconf-service-name services)
			    nil t))
	 ;; Retrieve and resolve the selected service object.
	 (service          (find service-name services
				 :key  #'zeroconf-service-name
				 :test #'string=))
	 (service-resolved (zeroconf-resolve-service service)))
    ;; Return host and port
    (list (zeroconf-service-host service-resolved)
	  (zeroconf-service-port service-resolved)))
  )


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'session-initiation)
		   'zeroconf 'rudel-zeroconf-backend)

(provide 'rudel-zeroconf)
;;; rudel-zeroconf.el ends here
