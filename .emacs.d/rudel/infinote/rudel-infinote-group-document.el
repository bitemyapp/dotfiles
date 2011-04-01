;;; rudel-infinote-group-document.el --- Infinote document group
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
;; This file contains the implementation of the infinote communication
;; group used by document sessions.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'warnings)

(require 'eieio)

(require 'rudel-color) ;; for HSV color functions
(require 'rudel-xml)

(require 'rudel-infinote-group)


;;; Class rudel-infinote-group-document-state-idle
;;

(defclass rudel-infinote-group-document-state-idle
  (rudel-infinote-group-state)
  ()
  "")

(defmethod rudel-infinote/sync-begin
  ((this rudel-infinote-group-document-state-idle) xml)
  "Handle 'sync-begin' message."
  (with-tag-attrs ((num-messages num-messages number)) xml
    ;; Switch to synchronizing state.
    (list 'synchronizing num-messages)))

(defmethod rudel-infinote/user-join
  ((this rudel-infinote-group-document-state-idle) xml)
  "Handle 'user-join' message."
  (with-tag-attrs ((id        id        number)
		   name
		   status
		   (caret     caret     number)
		   (selection selection number)
		   (hue       hue       number)) xml
    (if (rudel-find-user this id #'= #'rudel-id)
	;; If the user is already subscribed to the document,
	;; display a warning and ignore the request.
	(display-warning
	 '(rudel infinote)
	 (format
	  "User with id %d is already subscribed to document `%s'"
	  id (object-name-string (oref this :document)))
	 :warning)

      ;; Otherwise, construct the document user object and add it to
      ;; the document.
      (rudel-add-user
       this
       (rudel-infinote-document-user
	name
	:id     id
	:color  (rudel-hsv->string
		 hue 0.3 (rudel-color-background-value))
	:status (intern-soft status)))))

  ;; Stay in this state.
  nil)

(defmethod rudel-infinote/user-rejoin
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  (with-tag-attrs ((id        id        number)
		   name
		   status
		   (caret     caret     number)
		   (selection selection number)
		   (hue       hue       number)) xml
    (let ((user (rudel-find-user this id #'= #'rudel-id)))
      (if (not user)
	  ;; We did not find the user, display a warning and give up.
	  (display-warning
	   '(rudel infinote)
	   (format "Could not find user: %d" id)
	   :warning)

	;; If we found the user, update its slots.
	(rudel-set-color  user (rudel-hsv->string
				hue 0.3 (rudel-color-background-value)))
	(rudel-set-id     user id)
	(rudel-set-status user (intern-soft status))

	(rudel-change-notify user))))

  ;; Stay in this state.
  nil)

(defmethod rudel-infinote/user-status-change
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  (with-tag-attrs ((id  id  number)
		   status)          xml
    (let ((user (rudel-find-user this id #'= #'rudel-id)))
      (if (not user)
	  ;; We did not find the user, display a warning and give up.
	  (display-warning
	   '(rudel infinote)
	   (format "Could not find user: %d" id)
	   :warning)

	;; If we found the user, update its slots.
	(rudel-set-status user (intern-soft status)) ;; TODO add type symbol to with-tag-attr?
	(rudel-change-notify user))))

  ;; Stay in this state.
  nil)

(defmethod rudel-infinote/user-color-change
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  (with-tag-attrs ((id  id  number)
		   (hue hue number)) xml
    (let ((user (rudel-find-user this id #'= #'rudel-id)))
      (if (not user)
	  ;; We did not find the user, display a warning and give up.
	  (display-warning
	   '(rudel infinote)
	   (format "Could not find user: %d" id)
	   :warning)

	;; If we found the user, update its slots.
	(rudel-set-color user (rudel-hsv->string
			       hue 0.3 (rudel-color-background-value)))
	(rudel-change-notify user))))

  ;; Stay in this state.
  nil)

;; TODO does this belong here or in derived classes?
(defmethod rudel-infinote/request
  ((this rudel-infinote-group-document-state-idle) xml)
  ""
  (with-tag-attrs ((user-id user number)) xml
    (let* ((operation (car (xml-node-children xml))) ;; TODO are multiple operations possible?
	   (type      (xml-node-name operation))
	   (user      (rudel-find-user
		       this user-id #'= #'rudel-id)))
      (if (not user)
	  ;; Warn if we cannot find the user.
	  (display-warning
	   '(rudel infinote)
	   (format "Could not find user: %d'" user-id)
	   :warning)

	;; Dispatch to handler.
	(rudel-dispatch
	 this
	 "rudel-infinote/request/" (symbol-name type)
	 (list user operation)))))

  ;; Stay in this state.
  nil)

(defmethod rudel-infinote/session-close
  ((this rudel-infinote-group-document-state-idle) xml)
  "Handle 'session-close' message."
  ;; Switch to closed state.
  'closed)

;; we can receive
;; <request-failed domain="error_domain" code="error_code" seq="seq_id">
;;  <text>Human readable</text>
;; </request-failed>
;; or should the base class handle this?

;; TODO we can send
;; <user-join name="name" seq="seq_id" />
;; and
;; <session-unsubscribe />


;;; Class rudel-infinote-state-synchronizing
;;

(defclass rudel-infinote-group-document-state-synchronizing
  (rudel-infinote-group-state)
  ((all-items       :initarg :all-items
		    :type    (integer 0)
		    :documentation
		    "")
   (remaining-items :initarg :num-items
		    :type    (integer 0)
		    :documentation
		    ""))
  "")

(defmethod rudel-enter ((this rudel-infinote-group-document-state-synchronizing)
			num-items)
  ""
  (with-slots (document all-items remaining-items) this
    ;; Remove all subscribed users from the document. The
    ;; synchronization will add users anew.
    (rudel-clear-users document)

    (setq all-items       num-items
	  remaining-items num-items))
  nil)

(defmethod rudel-infinote/sync-user
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  "Create a user object and add it to the document."
  ;; TODO send sync-error if remaining-items is already zero
  (with-slots (remaining-items) this
    (with-tag-attrs ((id        id        number)
		     name
		     status
		     (caret     caret     number)
		     (selection selection number)
		     (hue       hue       number)) xml
      (let ((user (rudel-infinote-document-user
		   name
		   :color  (rudel-hsv->string
			    hue 0.3 (rudel-color-background-value))
		   :id     id
		   :status (intern-soft status))))

	;; Add user to the list of subscribed users of the document.
	(rudel-add-user document user)))

    ;; Expect one less synchronization item.
    (decf remaining-items))
  ;; Stay in this state.
  nil)

(defmethod rudel-infinote/sync-request
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  "Handle 'sync-request' message."
  (with-slots (remaining-items) this
    (with-tag-attrs (user time) xml
      ) ;; TODO

    ;; Expect one less synchronization item.
    (decf remaining-items))
  ;; Stay in this state.
  nil)

(defmethod rudel-infinote/sync-segment ;; TODO text documents only?
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  "Handle 'sync-segment' message."
  (with-slots (remaining-items) this
    (with-tag-attrs (author) xml
      ) ;; TODO

    ;; Expect one less synchronization item.
    (decf remaining-items))
  ;; Stay in this state.
  nil)

(defmethod rudel-infinote/sync-end
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  "Handle 'sync-end' message."
  (with-slots (all-items remaining-items) this
    (if (= remaining-items 0)
	;; Everything is fine, we received the expected number of
	;; items.
	(rudel-send this '(sync-ack))
      ;; We did not received the correct number of items. Send an
      ;; error message and display a warning.
      (rudel-send
       this
       `(sync-error
	 ((domain . "INF_SESSION_SYNCHRONIZATION_ERROR")
	  (code   . "0"))
	 ,(format
	   "Received less synchronization items (%d) than previously announced (%d)"
	   (- all-items remaining-items)
	   all-items)))

      (display-warning
       '(rudel infinote)
       (format
	"Received less synchronization items (%d) than previously announced (%d)"
	(- all-items remaining-items)
	:warning))))
  ;; Stay in this state.
  'idle)

(defmethod rudel-infinote/sync-cancel
  ((this rudel-infinote-group-document-state-synchronizing) xml)
  "Handle 'sync-cancel' message."
  ;; Stay in this state.
  'idle)

;; In this state, we can send
;;<sync-error domain="domain" code="code">
;; <text>Human readable</text>
;;</sync-error>



;;; Class rudel-infinote-group-document-state-joining
;;

(defclass rudel-infinote-group-document-state-joining
  (rudel-infinote-group-state)
  ()
  "This state indicates that we are currently joining the session
associated to a document. After sending a 'user-join' message, we
expect a 'user-join' or 'user-rejoin' message in response.")

(defmethod rudel-enter
  ((this rudel-infinote-group-document-state-joining))
  ""
  (let ((self (rudel-self (oref this :session))))
    (with-slots ((name :object-name)
		 color
		 status) self
      (let ((hue (car (apply #'rudel-rgb->hsv
			     (color-values color)))))
	(rudel-send this
		    `(user-join
		      ((name   . ,name)
		       (status . "active")
		       (time   . "")
		       (caret  . ,(format "%d" 0)) ;; selection
		       (hue    . ,(format "%f" hue))))))))
  ;; Remain in this state and wait for reply.
  nil)

(defmethod rudel-infinote/user-join
  ((this rudel-infinote-group-document-state-joining) xml)
  "Handle 'user-join' message."
  (with-tag-attrs ((id        id        number)
		   name
		   status
		   (caret     caret     number)
		   (selection selection number)
		   (hue       hue       number)) xml
    ;; In the joining state, the join message has to refer to our own
    ;; user. Therefore, we obtain the self user object from the
    ;; session, update its slots and add it to the document.
    (let ((self (rudel-self (oref this :session))))
      ;; When we did not find the self user display a warning.
      (when (not self)
	(display-warning
	 '(rudel infinote)
	 "No self user in session"
	 :warning))

      (let ((user (rudel-add-user
		   this
		   (apply
		    #'rudel-infinote-document-user
		    name
		    :color  (rudel-hsv->string
			     hue 0.3 (rudel-color-background-value))
		    :id     id
		    :status (intern-soft status)
		    (when self
		      (list :session-user self))))))
	(rudel-set-self this user))))

  ;; Since we expect the join or rejoin message for our own user, we
  ;; can leave the state and go to idle.
  'idle)

(defmethod rudel-infinote/user-rejoin
  ((this rudel-infinote-group-document-state-joining) xml)
  ""
  (with-tag-attrs ((id        id        number)
		   name
		   status
		   (caret     caret     number)
		   (selection selection number)
		   (hue       hue       number)) xml
    (let ((user (rudel-find-user this id #'= #'rudel-id)))
      ;; When we did not find the self user or the document user or
      ;; they are not the same object, display a warning.
      (if (not user)
	  (display-warning
	   '(rudel infinote)
	   (format "Could not find self user in document: %d" id)
	   :warning)

	;; Update slots of the user object
	(object-set-name-string user name)
	(rudel-set-color  user (rudel-hsv->string
				hue 0.3 (rudel-color-background-value)))
	(rudel-set-id     user id)
	(rudel-set-status user (intern-soft status))

	(rudel-set-self this user)

	(rudel-change-notify user))))

  ;; Since we expect the join or rejoin message for our own user, we
  ;; can leave the state and go to idle.
  'idle)


;;;
;;

(defvar rudel-infinote-group-document-states
  '((idle          . rudel-infinote-group-document-state-idle)
    (synchronizing . rudel-infinote-group-document-state-synchronizing)
    (joining       . rudel-infinote-group-document-state-joining)
    (closed        . rudel-infinote-group-state-closed))
  "TODO")


;;; Class rudel-infinote-group-document
;;

(defclass rudel-infinote-group-document (rudel-infinote-group
					 rudel-impersonator
					 rudel-delegator)
  ((document                  :initarg :document
			      :type    rudel-infinote-document-child
			      :documentation
			      "")
   (impersonation-target-slot :initform document)
   (delegation-target-slot    :initform document))
  "")

(defmethod initialize-instance ((this rudel-infinote-group-document)
				slots)
  ""
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states
   this rudel-infinote-group-document-states)
  )

(provide 'rudel-infinote-group-document)
;;; rudel-infinote-group-document.el ends here
