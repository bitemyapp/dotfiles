;;; rudel-infinote-client.el --- Client part of the infinote backend for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, client
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
;; This file contains the client part of the infinote backend for
;; Rudel.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'warnings)

(require 'rudel)
(require 'rudel-state-machine) ;; TODO necessary?
(require 'rudel-infinote-util)
(require 'rudel-infinote-errors)

(require 'rudel-infinote-state)

(require 'rudel-infinote-group) ;; TODO temp?
(require 'rudel-infinote-group-directory)
(require 'rudel-infinote-group-document)

(require 'rudel-infinote-node-directory) ;; TODO temp
(require 'rudel-infinote-text-document)

(require 'rudel-infinote-user)

(require 'adopted)


;;; Class rudel-infinote-client-connection
;;

;; TODO make this a base class for client and server
(defclass rudel-infinote-client-connection (rudel-connection)
  ((transport       :initarg  :transport
		    :type     rudel-transport
		    :documentation
		    "")
   (groups          :initarg  :groups
		    :type     hash-table
		    :documentation
		    "Association of group names and
objects. Groups are objects of subclasses of
`rudel-infinote-group'.")
   (nodes           :initarg  :nodes
		    :type     list
		    :initform nil
		    :documentation
		    "List of node objects in this
connection. Nodes are objects of subclasses of
`rudel-infinote-node'. Nodes usually have associated group
objects.")
   (sequence-number :initarg  :sequence-number ;; TODO this belongs in the group class?
		    :type     (integer 1)
		    :initform 1
		    :documentation
		    "")
   (plugins         :initarg  :plugins
		    :type     list
		    :initform nil
		    :documentation
		    "List of plugins advertised by the remote
side."))
  "TODO")

(defmethod initialize-instance ((this rudel-infinote-client-connection)
				slots)
  ""
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Create hash-table for groups.
  (with-slots (groups) this
    (setq groups (make-hash-table :test #'equal)))

  ;; Install handler.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter transport
			(lambda (xml)
			  (rudel-receive this1 xml)))))

  ;;
  (with-slots (session) this
    (let ((user (rudel-infinote-user
		 "scymtym"
		 :color  "red"
		 ;:status 'active
		 )))
      ;;(plist-get info ;; TODO

      (with-slots (self) session
	(setq self user))) ;; TODO temp

    ;; The special 'InfDirectory' group is there from the beginning.
    (let ((directory-group (rudel-infinote-group-directory
			    "InfDirectory"
			    :publisher "you"))) ;; TODO use correct publisher name
      (rudel-add-group this directory-group)

      (require 'rudel-infinote-node-directory)
      (rudel-add-node this
		      (rudel-infinote-node-directory
		       "root"
		       :id     0
		       :parent nil
		       :group  directory-group))

      ;; TODO install a hook that takes the sequence number and plugin
      ;; list from the directory group and stores it in THIS.
      ))
  )

(defmethod rudel-get-group ((this rudel-infinote-client-connection) name)
  "Return group named NAME or nil if there is no such group."
  (with-slots (groups) this
    (gethash name groups)))

(defmethod rudel-add-group ((this rudel-infinote-client-connection) group)
  ""
  (with-slots ((name :object-name) connection) group
    ;;
    (setq connection this) ;; TODO encapsulation violation?

    ;;
    (with-slots (groups) this
      (puthash name group groups)))
  )

(defmethod rudel-remove-group ((this rudel-infinote-client-connection)
			       group-or-name)
  "Remove GROUP-OR-NAME from the list of groups of THIS.
GROUP-OR-NAME is a `rudel-infinote-group' object or a string in
which case it is the name of a group."
  (with-slots (groups) this
    (let ((name (cond
		 ((rudel-infinote-group-child-p group-or-name)
		  (object-name group-or-name))

		 (t
		  group-or-name))))
      (remhash name groups))))

(defmethod rudel-make-and-add-group ((this rudel-infinote-client-connection)
				     type name method &optional node)
  "Create a group object and add it to THIS."
  ;; TODO the backend creates these
  (let ((group (rudel-infinote-group-text-document
		name
		:publisher "you" ;; TODO temp
		:method    method
		;;:id        id
		:document  node)))
    (rudel-add-group group)))

(defmethod rudel-find-node ((this rudel-infinote-client-connection)
			    which &optional test key)
  "Find node WHICH in the node list of THIS.
WHICH is compared to the result of KEY using TEST."
  (with-slots (nodes) this
    (find which nodes
	  :key  (or key #'rudel-id)
	  :test (or test #'=))))

(defmethod rudel-add-node ((this rudel-infinote-client-connection) node)
  "Add NODE to the list of nodes of THIS."
  (object-add-to-list this :nodes node))

(defmethod rudel-remove-node ((this rudel-infinote-client-connection) node)
  "Remove NODE from the list of nodes of THIS."
  (object-remove-from-list this :nodes node))

(defmethod rudel-make-and-add-node ((this rudel-infinote-client-connection)
				    id parent-id name type)
  ;; TODO the backend does the creation
  (with-slots (session) this
    (let ((parent (and parent-id
		       (rudel-find-node this parent-id))))
      ;; Signal an error if a parent was specified, but we cannot find
      ;; it.
      (unless (or (null parent-id) parent)
	(signal 'rudel-infinote-no-such-node (list parent-id)))

      ;; Create the new node. Distinguish document and directory nodes
      ;; based on TYPE.
      (destructuring-bind (node . is-document)
	  (cond
	   ;; This is a special kind of node. Nodes of this kind are
	   ;; inner nodes in the node tree.
	   ((string= type "InfSubdirectory")
	    (cons (rudel-infinote-node-directory
		   name
		   :id     id
		   :parent parent
		   :group  (rudel-get-group this "InfDirectory"))
		  nil))

	   ;; Other special kinds of nodes would go here

	   ;; Ordinary document nodes.
	   ;; TODO the backend should construct the appropriate document
	   ;; object based on TYPE
	   ((string= type "InfText")
	    (cons (rudel-infinote-text-document
		   name
		   :id     id
		   :parent parent)
		  t)))

	;; Integrate the document object into the hierarchy.
	(when parent
	  (rudel-add-child parent node))
	(rudel-add-node this node)
	(when is-document
	  (rudel-add-document session node)))))
  )

(defmethod rudel-send ((this rudel-infinote-client-connection) xml)
  ""
  (with-slots (transport) this
    (rudel-send transport xml)))

(defmethod rudel-receive ((this rudel-infinote-client-connection) xml)
  ""
  (case (xml-node-name xml)
    ;;
    (group
     (let* ((name  (xml-get-attribute xml 'name))
	    (xml   (xml-node-children xml))
	    (group (rudel-get-group this name)))
       (if group

	   ;; Dispatch to GROUP
	   (rudel-accept group (car xml))

	 ;; Display a warning and ignore the message.
	 (display-warning
	  '(rudel infinote)
	  (format "Could not find group: `%s'" name)
	  :warning))) ;; TODO pass list or single element?
     ;; Our own state does not change
     nil)

    ;;
    (t
     (when (next-method-p)
       (call-next-method)))) ;; TODO what is actually called here?
  )

(defmethod rudel-disconnect ((this rudel-infinote-client-connection)) ;; TODO maybe we could automatically delegate to the transport
  ""
  (with-slots (transport) this
    (rudel-disconnect transport)))

(defmethod rudel-wait ((this rudel-infinote-client-connection)
		       &optional progress-callback)
  "Block until THIS is done with the session setup."
  (let ((group (rudel-get-group this "InfDirectory")))
    (rudel-state-wait group '(idle) '() progress-callback)))

(defmethod rudel-publish ((this rudel-infinote-client-connection) document)
  ""
  ;; Create a new adopted context for DOCUMENT.
  ;(rudel-add-context this document)

  ;;<add-node
  ;;  parent="node_id"
  ;;  type="Type"
  ;;  name="Name"
  ;;  seq="seq_id">
  ;;    <sync-in />
  ;;    <subscribe />
  ;;</add-node>

  ;; Announce the new document to the server.
  (let ((group  (rudel-get-group this "InfDirectory"))
	(parent 0)
	(type   "InfText")
	(name   (object-name-string document)))
    (rudel-send group
		`((add-node
		   ((parent . ,(format "%d" parent))
		    (type   . ,type)
		    (name   . ,name))))))
    )
;; TODO should be a method of the directory group

(defmethod rudel-subscribe-to ((this rudel-infinote-client-connection)
			       document)
  ""
  ;; Create a new adopted context for DOCUMENT.
  ;; TODO (rudel-add-context this document)

  ;; Subscribe to DOCUMENT's group in the directory group and then
  ;; join the session group associated to DOCUMENT.

  ;; Announce the subscription to the server and wait until the
  ;; subscription is finished
  (let ((group (rudel-get-group this "InfDirectory"))) ;; TODO (with-group?
    (rudel-switch group 'subscribing (oref document :id))
    (rudel-state-wait group '(idle) nil))
  ;; TODO responsibility of the group?

  ;; Join the group of the document.
  (with-slots (group) document
    (rudel-switch group 'joining)
    (rudel-state-wait group '(idle) nil))
  ;; TODO responsibility of the document?

  ;; We receive a notification of our own subscription from the
  ;; server. TODO Or, do we? Consequently we do not add SELF to the
  ;; list of subscribed users of DOCUMENT.
  )

(defmethod rudel-unsubscribe-from ((this rudel-infinote-client-connection)
				   document)
  ""
  ;; Delete the jupiter context for DOCUMENT.
  ;; TODO (rudel-remove-context this document)

  ;; Announce the end of our subscription to the server.
  (with-slots (id group) document
    (rudel-send group
		`(session-unsubscribe
		  ((id . ,(format "%d" id))))))
  ;; TODO the group should handle this
  ;; TODO maybe there should be a separate state for this?

  ;; We receive a notification of the end of our own subscription from
  ;; the server. TODO do we? Consequently we do not remove SELF from
  ;; the list of subscribed users of DOCUMENT.
  )

(defmethod rudel-subscribe-session ((this rudel-infinote-client-connection)
				    name method id)
  ""
  ;; TODO this makes sense for document sessions only, but we want to
  ;; subscribe to directories, too
  (with-slots (session) this
    (require 'rudel-infinote-group-text-document) ;; TODO temp
    (let* ((document (rudel-find-document session id
					  #'eq #'rudel-id))
	   (group    (rudel-infinote-group-text-document ;; TODO class
		      name
		      :publisher "you" ;; TODO temp
		      :method    method
		      ;;:id        id
		      :document  document)))

      (rudel-add-group this group)
      (oset document :group group))) ;; TODO temp
  )

(defmethod rudel-local-insert ((this rudel-infinote-client-connection)
			       document position data)
  ""
  (rudel-local-operation
   this
   document
   (adopted-insert "insert"
		   :from position
		   :data data))
  )

(defmethod rudel-local-delete ((this rudel-infinote-client-connection)
			       document position length)
  ""
  (rudel-local-operation
   this
   document
   (adopted-delete "delete"
		   :from position
		   :to   (+ position length)))
  )

(defmethod rudel-local-operation ((this rudel-infinote-client-connection)
				  document operation)
  "Handle OPERATION performed on DOCUMENT by sending a message through THIS connection."
  ;; Find jupiter context for DOCUMENT.
  ;;(let ((context (rudel-find-context this document)))

  ;; Notify the server of the operation
  (let ((self (rudel-self (oref this :session))))
    (with-slots (id group) document
      (rudel-send group
		  (rudel-infinote-embed-in-request
		   self (rudel-operation->xml operation)))))

  ;; Submit the operation to the jupiter context.
  ;; (jupiter-local-operation context operation))
  )

(defmethod rudel-remote-operation ((this rudel-infinote-client-connection)
				   document user
				   remote-revision local-revision
				   operation)
  "Handle OPERATION received through THIS connection performed by USER on DOCUMENT."
  (let* (;; Find jupiter context for DOCUMENT.
	 ;;(context     (rudel-find-context this document))
	 ;; And transform the operation.
	 (transformed operation)) ;;(jupiter-remote-operation
		       ;;context
		       ;;remote-revision local-revision
		       ;;operation)))

    ;; Apply the transformed operation to the document.
    (rudel-remote-operation document user transformed))
  )

(provide 'rudel-infinote-client)
;;; rudel-infinote-client.el ends here
