;;; rudel-infinote-group.el --- Common aspects of infinote communication groups
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
;; This file contains the following super class classes for
;; implementing infinote communication groups:
;;
;; + `rudel-infinote-group'
;;   + `rudel-infinote-sequence-number-group'
;;
;; Communication groups are modeled as named state machines. States
;; have to be implemented in sub classes.
;;
;; For implementing infinote group state, the super class
;; `rudel-infinote-group-state' which mixes in impersonation (via
;; `rudel-impersonator') and delegation (via `rudel-delegator') to the
;; containing group is provided.


;;; History:
;;
;; 0.2 - Automatic sequence number injection
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'warnings)

(require 'eieio)
(require 'eieio-base) ;; for `eieio-named'

(require 'rudel-util) ;; for `rudel-impersonator', `rudel-delegator'
(require 'rudel-state-machine)
(require 'rudel-infinote-state)


;;; Class rudel-infinote-group-state
;;

(defclass rudel-infinote-group-state (rudel-infinote-state
				      rudel-impersonator
				      rudel-delegator)
  ((impersonation-target-slot :initform 'group)
   (delegation-target-slot    :initform 'group)
   (group                     :initarg :group
			      :type    rudel-infinote-group-child
			      :documentation
			      ""))
  ""
  :abstract t)

(defmethod rudel-accept ((this rudel-infinote-group-state) xml)
  "Dispatch XML to appropriate handler method based on content."
  (let ((type (xml-node-name xml)))
    (case type
      ;; Handle request-failed messages.
      (request-failed
       ;; TODO handle the problem
       ;; TODO there can be a description:
       ;;      <request-failed><text>Bla</text></request-failed>
       (with-tag-attrs (domain
			(code            code number)
			(sequence-number seq  number)) xml
	 (display-warning
	  '(rudel infinote)
	  (format "request failed; sequence number: `%s', \
domain: `%s', code: `%s'"
		  sequence-number domain code)
	  :warning))
       'idle)

      ;; Dispatch all normal message to appropriate methods
      ;; automatically.
      (t
       (let ((name (symbol-name type)))
	 (condition-case error
	     ;; Try to dispatch on the message type.
	     (rudel-dispatch this
			     "rudel-infinote/" name
			     (list xml))
	   ;; Warn if we failed to locate or execute the method. Return
	   ;; nil in this case, so we remain in the current state.
	   (rudel-dispatch-error
	    (progn
	      (display-warning
	       '(rudel infinote)
	       (format "%s: no method (%s: %s): `%s/%s'; arguments: %s"
		       (object-print this) (car error) (cdr error)
		       "rudel-infinote" name arguments)
	       :warning)
	      nil)))))))
  )


;;; Class rudel-infinote-group-state-closed
;;

(defclass rudel-infinote-group-state-closed (rudel-infinote-group-state)
  ()
  "Groups enter this state when receiving a <session-close/>
message.")
;; TODO can all groups receive <session-close/> or just document groups?

(defmethod rudel-accept ((this rudel-infinote-group-state-closed) xml)
  "Simply ignore all further messages."
  nil)


;;; Class rudel-infinote-group
;;

(defclass rudel-infinote-group (eieio-named
				rudel-state-machine)
  ((connection :initarg :connection
	       ;:type    rudel-infinote-connection ;; TODO
	       :documentation
	       "The connection used by this group object to do
its communication.")
   (publisher  :initarg  :publisher
	       :type     string
	       :documentation
	       "")
   (method     :initarg  :method
	       :type     symbol
	       :documentation
	       "")
   (members    :initarg  :members ;; TODO currently unused
	       :type     list
	       :initform nil
	       :documentation
	       ""))
  "Super class for all communication groups used in infinote
sessions. Groups are basically modeled as named state
machines. Subclasses have to provide their own states."
  :abstract t)

(defmethod rudel-register-state ((this rudel-infinote-group) symbol state)
  "Set the :group slot of STATE to THIS."
  ;; Associate THIS connection to STATE.
  (oset state :group this)

  ;;
  (when (next-method-p)
    (call-next-method)))

(defmethod rudel-send ((this rudel-infinote-group) data)
  "Send DATA through the connection associated to THIS."
  (with-slots (connection) this
    (rudel-send connection
		(rudel-infinote-embed-in-group this data))))


;;; Class rudel-infinote-sequence-number-group
;;

(defclass rudel-infinote-sequence-number-group (rudel-infinote-group)
  ((next-sequence-number :initarg  :next-sequence-number
			 :type     (integer 0)
			 :initform 0
			 :documentation
			 "Sequence number used when sending
requests.")
   (remote-id            :initarg  :remote-id
			 :type     (integer 0)
			 :documentation
			 "Id assigned to us by the remote
side. This is used to identify messages directed at us."))
  "Objects of this class inject sequence number into messages
sent via `rudel-send'.")

(defmethod rudel-send ((this rudel-infinote-sequence-number-group)
		       data &optional no-sequence-number)
  "Add a sequence number to DATA and send it.
After sending, increment the sequence number counter.
If NO-SEQUENCE-NUMBER is non-nil, do not add a sequence number
and do not increment the sequence number counter."
  (if no-sequence-number
      (call-next-method this data)
    (with-slots ((seq-num :next-sequence-number)) this
      (let ((data       (xml-node-name data))
	    (attributes (xml-node-attributes data))
	    (children   (xml-node-children data)))
	(call-next-method
	 this
	 (append
	  (list
	   data
	   (cons `(seq . ,(number-to-string seq-num))
		 attributes))
	  children)))
      (incf seq-num)))
  )


;;; Miscellaneous functions
;;

(defmacro rudel-infinote-embed-in-group (group &rest forms) ;; TODO bad name
  "Construct a message out of FORMS by adding data from GROUP.
The returned message consists of an outer <group> element with
GROUP's properties in its attributes and FORMS as children."
  (declare (indent 1)
	   (debug (form &rest form)))
  (let ((group-var (make-symbol "group"))
	(name      (make-symbol "name"))
	(publisher (make-symbol "publisher")))
    `(let* ((,group-var ,group)
	    (,name      (object-name-string ,group-var))
	    (,publisher (oref ,group-var :publisher)))
       `(group
	 ((name      . ,,name)
	  (publisher . ,,publisher))
	 ,,@forms)))
  )

(provide 'rudel-infinote-group)
;;; rudel-infinote-group.el ends here
