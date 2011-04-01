;;; rudel-infinote.el --- Infinote backend for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, gobby, infinote, protocol
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
;; infinote protocol (used by the Gobby family of collaborative
;; editors starting with version 0.5).



;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-backend)
(require 'rudel-protocol)

(require 'rudel-interactive) ;; for read functions


;;; Constants
;;

(defconst rudel-infinote-version '(0 1)
  "Version of the infinote backend for Rudel.")


;;; Class rudel-infinote-backend
;;

;;;###autoload
(defclass rudel-infinote-backend (rudel-protocol-backend)
  ((capabilities :initform '(join
			     change-color
			     chat
			     track-subscriptions track-cursors
			     track-selections track-viewports
			     group-undo)))
  "")

(defmethod initialize-instance ((this rudel-infinote-backend) slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-infinote-version))

(defmethod rudel-ask-connect-info ((this rudel-infinote-backend)
				   &optional info)
  ""
  ;; Read desired username and color
  (let ((username (or (plist-get info :username)
		      (rudel-read-user-name)))
	(color    (or (plist-get info :color)
		      (rudel-read-user-color))))
    (append
     (list :username username
	   :color    color)
     info))
  )

(defmethod rudel-connect ((this rudel-infinote-backend) transport
			  info info-callback
			  &optional progress-callback)
  "Connect to an infinote server using the information INFO.
Return the connection object."
  ;; Before we start, load the client functionality.
  (require 'rudel-infinote-client)

  ;; Create the connection object
  (let* ((session    (plist-get info :session))
	 (host       (plist-get info :host)) ;; Just as name
	 (connection (rudel-infinote-client-connection
		      (format "to %s" (or host "unknown host"))
		      :session   session
		      :transport transport)))

    ;; Start the transport.
    (rudel-start transport)

    ;; Wait until the connection has done its session initiation on
    ;; the protocol level.
    (rudel-wait connection progress-callback)

    ;; The connection is now ready for action; Return it.
    connection)
  )

(defmethod rudel-make-document ((this rudel-infinote-backend)
				name encoding session)
  ""
  (rudel-infinote-text-document name
				:session session))

(defmethod rudel-make-node ((this rudel-infinote-backend)
			    type name id parent)
  "Create a node object according to TYPE, NAME, ID and PARENT.
The new node will be named NAME and have id ID. It will be a
child node of PARENT unless PARENT is nil in which case the new
node will be the root node."
  (cond
   ;; Text
   ((string= type "InfText")
    (rudel-infinote-text-document
     name
     :id     id
     :parent parent))

   ;; SubDirectory
   ((string= type "InfSubdirectory")
    (rudel-infinote-node-directory
     name
     :id     id
     :parent parent))

   ;; unknown type
   (t
    (error "No such node type: `%s'" type)))
  )

(defmethod rudel-make-group ((this rudel-infinote-backend)
			     type name method &optional node)
  "Create a new group according to TYPE, NAME and METHOD.
The optional argument NODE can specify the node (usually a
document) associated to the new group."
  (cond
   ;; Text document
   ((string= type "InfText")
    (rudel-infinote-group-text-document
     name
     :publisher "you"
     :method    method
     :document  node))

   ;; unknown type
   (t
    (error "No such node type: `%s'" type)))
  )


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'protocol)
		   'infinote 'rudel-infinote-backend)

;;;###autoload
(eval-after-load 'rudel-zeroconf
  '(rudel-zeroconf-register-service "_infinote._tcp"
				    'xmpp 'infinote))

(provide 'rudel-infinote)
;;; rudel-infinote.el ends here
