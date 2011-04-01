;;; rudel-infinote-group-directory.el --- Infinote directory group
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, group, communication
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
;; Implementation of the infinote 'InfDirectory' communication
;; group. This special group is used to perform and announce changes
;; to the document tree and for session management.
;; See http://gobby.0x539.de/trac/wiki/Infinote/Protocol


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'warnings)

(require 'eieio)

(require 'xml)

(require 'rudel-xml)

(require 'rudel-infinote-group)


;;; Class rudel-infinote-directory-state-new
;;

;; TODO rename to rudel-infinote-group-directory-state-new
(defclass rudel-infinote-directory-state-new
  (rudel-infinote-group-state)
  ((sequence-number :initarg :sequence-number
		    :type    (integer 0)
		    :documentation
		    "The sequence number the server reserved for
this exchange.")
   (plugins         :initarg  :plugins
		    :type     list
		    :initform nil
		    :documentation
		    "The list of plugins is stored here
temporarily after receiving it from the server."))
  "New state of the directory group.
Initial state of the state machine of the infinote directory
group.")

(defmethod rudel-infinote/welcome
  ((this rudel-infinote-directory-state-new) xml)
  "Handle infinote welcome message."
  ;; Temporarily store list of plugins and sequence
  ;; number. Ultimately, the plugin list belongs in the :plugins slot
  ;; of the connection object and the sequence-number should be used
  ;; by all groups to identify server replies.
  (with-slots (sequence-number plugins) this
    (with-tag-attrs ((version  protocol-version number)
		     (sequence sequence-id      number)) xml
      (setq sequence-number sequence
	    plugins         (mapcar
			     (lambda (plugin)
			       (xml-get-attribute plugin 'type))
			     (xml-node-children
			      (car (xml-get-children
				    xml 'note-plugins)))))))

  ;; Switch to idle state.
  'idle)


;;; Class rudel-infinote-directory-state-idle
;;

(defclass rudel-infinote-directory-state-idle
  (rudel-infinote-group-state)
  ()
  "Idle state of the directory group.")

(defmethod rudel-infinote/add-node
  ((this rudel-infinote-directory-state-idle) xml)
  ""
  ;; TODO there can be a child:
  ;; <subscribe group="group_name" method="method_name" />
  ;; or can it? here in idle state?
  ;; Maybe this attribute can only be used for client -> server
  ;; communication.
  (with-slots (group) this
    ;; TODO how should/does this handle absent attributes?
    (with-tag-attrs ((id     id     number)
		     (parent parent number)
		     name
		     type) xml
      (rudel-add-node group id parent name type)))
  nil)

(defmethod rudel-infinote/remove-node
  ((this rudel-infinote-directory-state-idle) xml)
  ""
  (with-tag-attrs (id) xml ;; seq
    nil))

(defmethod rudel-infinote/sync-in
  ((this rudel-infinote-directory-state-idle) xml)
  ""
  ;; TODO can contain child <subscribe group="group_name" method="method_name" />
  (with-tag-attrs (id parent name type group method) xml ;; optional? seq
    nil)
  )


;;; Class rudel-infinote-directory-state-exploring
;;

(defclass rudel-infinote-directory-state-exploring
  (rudel-infinote-group-state)
  ((remaining-messages :initarg :remaining-messages
		       :type    (integer 0)
		       :documentation
		       ""))
  "Directory group state entered when the children of a node are
explored.")

(defmethod rudel-enter
  ((this rudel-infinote-directory-state-exploring) id)
  ""
  (rudel-send this
	      `(explore-node
		  ((id  . ,(format "%d" id)))))
  nil)

(defmethod rudel-infinote/explore-begin ;; TODO there should be another state
  ((this rudel-infinote-directory-state-exploring) xml)
  ""
  ;; <explore-begin total="13" seq="0"/>
  (with-slots (remaining-messages) this
    (with-tag-attrs ((total total number)) xml
      (setq remaining-messages total))) ;; TODO in hex?
  nil)

(defmethod rudel-infinote/add-node
  ((this rudel-infinote-directory-state-exploring) xml)
  ;; TODO identical to idle state
  ""
  (with-slots (group remaining-messages) this
    (with-tag-attrs ((id     id     number)
		     (parent parent number)
		     name
		     type) xml
	(rudel-add-node group id parent name type))
    (decf remaining-messages))
  nil)

(defmethod rudel-infinote/explore-end
  ((this rudel-infinote-directory-state-exploring) xml)
  ""
  (with-slots (remaining-messages) this
    (unless (zerop remaining-messages)
      (display-warning
       '(rudel infinote)
       (format
	"received 'explore-end' message when still expecting %d messages"
	remaining-messages)
       :warning)))
  'idle)


;;; Class rudel-infinote-directory-state-subscribing
;;

(defclass rudel-infinote-directory-state-subscribing
  (rudel-infinote-group-state)
  ((id :initarg :id
       :type    (or null (integer 0))
       :documentation
       "The id of the target node of the subscription."))
  "Directory group state entered when subscribing to a session.")

(defmethod rudel-enter
  ((this rudel-infinote-directory-state-subscribing) id)
  "Send 'subscribe-session' message and store ID in THIS for later."
  (with-slots ((id1 :id)) this
    (setq id1 id)
    (rudel-send this
		`(subscribe-session
		  ((id . ,(format "%d" id1))))))
  nil)

(defmethod rudel-infinote/subscribe-session
  ((this rudel-infinote-directory-state-subscribing) xml)
  ""
  (with-slots ((id1 :id)) this
    (with-tag-attrs ((name group)
		     method
		     (id   id    number)) xml ;; optional seq

      ;; Check received id against stored id.
      (unless (= id1 id)
	(setq id1 nil)
	(display-warning
	 '(rudel infinote)
	 (format "Did not request subscription to group %d (was expecting %d)"
		 id id1)
	 :warning))

      ;; Check method and subscribe.
      (let ((method-symbol (intern-soft method))) ;; TODO (make-symbol method)
	(unless (memq method-symbol '(central))
	  (error "Invalid method: `%s'" method))
	;; TODO proper error handling

	(rudel-subscribe-session this name method-symbol id1))))

  'idle)

;; TODO this message is used when the server requested the subscription?
(defmethod rudel-leave ((this rudel-infinote-directory-state-subscribing))
  "Acknowledge the subscription when leaving the state."
  (with-slots (id) this
    (when id
      (rudel-send this
		  `(subscribe-ack
		    ((id . ,(format "%d" id))))
		  t))) ;; no sequence number
  )


;;; Directory group states
;;

(defvar rudel-infinote-group-directory-states
  '((new         . rudel-infinote-directory-state-new)
    (idle        . rudel-infinote-directory-state-idle)
    (exploring   . rudel-infinote-directory-state-exploring)
    (subscribing . rudel-infinote-directory-state-subscribing)
    (closed      . rudel-infinote-group-state-closed))
  "States of the state machine used by the directory group.")


;;; Class rudel-infinote-group-directory
;;

(defclass rudel-infinote-group-directory (rudel-infinote-sequence-number-group)
  ((method :initform 'central))
  "Objects of this class represent infinote directory
communication groups.")

(defmethod initialize-instance ((this rudel-infinote-group-directory)
				slots)
  ""
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states
   this rudel-infinote-group-directory-states)
  )

(defmethod rudel-add-node ((this rudel-infinote-group-directory)
			   id parent name type)
  ""
  (with-slots (connection) this
    (rudel-make-and-add-node connection id parent name type)))

(defmethod rudel-remove-node ((this rudel-infinote-group-directory))
  ""
  (error "Removing nodes is not implemented"))

(defmethod rudel-subscribe-session ((this rudel-infinote-group-directory)
				    name method id)
  ""
  (with-slots (connection) this
    (rudel-subscribe-session connection name method id)))

(provide 'rudel-infinote-group-directory)
;;; rudel-infinote-group-directory.el ends here
