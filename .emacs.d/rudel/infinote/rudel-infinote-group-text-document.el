;;; rudel-infinote-group-text-document.el --- Communication group used by text documents
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, communication, group, text, document
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
;; Text documents.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-xml)

(require 'rudel-operations)

(require 'rudel-infinote-group-document)
(require 'rudel-infinote-text-document)


;;; Class rudel-infinote-group-text-document-state-idle
;;

(defclass rudel-infinote-group-text-document-state-idle
  (rudel-infinote-group-document-state-idle)
  ()
  "")

(defmethod rudel-infinote/request/insert
  ((this rudel-infinote-group-text-document-state-idle)
   user xml)
  ""
  (with-tag-attrs ((position pos  number)
		   (text     text))       xml
    (rudel-remote-operation
     this user
     (rudel-insert-op
      "insert"
      :position position
      :data     (or text "\n")))) ;; TODO is this correct?
  nil)

(defmethod rudel-infinote/request/insert-caret
  ((this rudel-infinote-group-text-document-state-idle)
   user xml)
  ""
  (with-tag-attrs ((position pos  number)
		   (text     text))       xml
    ;; Perform the insert operation
    (rudel-remote-operation
     this user
     (rudel-insert-op
      "insert"
      :from position
      :data (or text "\n")))

    ;; Perform the cursor movement operation
    (rudel-remote-operation
     this user
     (rudel-move-cursor-op
      "move-cursor"
      :from position)))
  nil)

(defmethod rudel-infinote/request/delete
  ((this rudel-infinote-group-text-document-state-idle)
   user xml)
  ""
  (with-tag-attrs ((position pos number)
		   (length   len number)) xml
    (rudel-remote-operation
     this user
     (rudel-delete-op
      "delete"
      :from   position
      :length length)))
  nil)

(defmethod rudel-infinote/request/delete-caret
  ((this rudel-infinote-group-text-document-state-idle)
   user xml)
  ""
  (with-tag-attrs ((position pos number)
		   (length   len number)) xml
    ;; Perform the delete operation
    (rudel-remote-operation
     this user
     (rudel-delete-op
      "delete"
      :from   position
      :length length))

    ;; Perform the cursor movement operation
    (rudel-remote-operation
     this user
     (rudel-move-cursor-op
      "move-cursor"
      :from position)))
  nil)

(defmethod rudel-infinote/request/no-op
  ((this rudel-infinote-group-text-document-state-idle)
   user xml)
  ""
  nil)

(defmethod rudel-infinote/request/move
  ((this rudel-infinote-group-text-document-state-idle)
   user xml)
  ""
  (with-tag-attrs ((position caret     number)
		   (length   selection number)) xml
    ;; Perform the cursor movement operation
    (rudel-remote-operation
     this user
     (rudel-move-cursor-op
      "move-cursor"
      :from position))

    ;; Perform the selection movement operation
    (rudel-remote-operation
     this user
     (rudel-move-selection-op
      "move-selection"
      :from   position
      :length length)))
  nil)

(defmethod rudel-infinote/request/undo
  ((this rudel-infinote-group-text-document-state-idle) xml)
  ""
  nil)

(defmethod rudel-infinote/request/undo-caret
  ((this rudel-infinote-group-text-document-state-idle) xml)
  ""
  nil)

(defmethod rudel-infinote/request/redo
  ((this rudel-infinote-group-text-document-state-idle) xml)
  ""
  nil)

(defmethod rudel-infinote/request/redo-caret
  ((this rudel-infinote-group-text-document-state-idle) xml)
  ""
  nil)


;;; Class derived from synchronizing state
;;

(defclass rudel-infinote-group-text-document-state-synchronizing
  (rudel-infinote-group-document-state-synchronizing)
  ()
  "")

(defmethod rudel-infinote/sync-segment ;; TODO text documents only?
  ((this rudel-infinote-group-text-document-state-synchronizing) xml)
  ""
  (with-slots (remaining-items) this
    (with-tag-attrs ((author-id author number)
		     (text      text))         xml
      (let ((author (rudel-find-user
		     document author-id #'= #'rudel-id)))
	(if (not author)
	    ;; We did not find the user, display a warning and give
	    ;; up.
	    (display-warning
	     '(rudel infinote)
	     (format "Could not find user: %d" author-id)
	     :warning)
	  ;; Perform the insert operation
	  (rudel-remote-operation
	   this author
	   (rudel-insert-op
	    "insert-sync-segment"
	    :from nil
	    :data (or text "\n")))))

      ;; Expect one less synchronization item.
      (decf remaining-items)))
  nil)

(defmethod rudel-infinote/request/delete
  ((this rudel-infinote-group-text-document-state-synchronizing) xml)
  ""
;; <delete pos="pos"><segment author="user_id">text</segment>[...]</delete>
;;
;;     * pos, Integer: The character offset at which to start deleting
;;       characters.
;;     * segments: The text that was deleted, including author
;;       information, i.e. which user wrote what text.
;;
;; Deletes text from the buffer and specifies what text is
;; deleted. This operation is only used in <sync-request />
;; messages. The synchronization client cannot deduce what text was
;; actually deleted, but must be able to compute the reverse operation
;; in case someone undoes the request. In a normal <request />
;; message, other users can deduce what text was deleted by having a
;; look at the document and which transformations were required to
;; transform the request to the current state before the operation is
;; actually executed.
  (with-tag-attrs (position pos number) xml
    (do-tag-children (child xml)
	(with-tag-attrs (author author number) xml)
      ))
  nil)


;;; Text document group states
;;

(defvar rudel-infinote-group-text-document-states
  '((idle          . rudel-infinote-group-text-document-state-idle)
    (synchronizing . rudel-infinote-group-text-document-state-synchronizing)
    (joining       . rudel-infinote-group-document-state-joining) ;; TODO
    (closed        . rudel-infinote-group-state-closed))
  "TODO")


;;; Class rudel-infinote-group-text-document
;;

(defclass rudel-infinote-group-text-document (rudel-infinote-group-document)
  ((parent :type rudel-infinote-node-directory-child))
  "")

(defmethod initialize-instance
  ((this rudel-infinote-group-text-document) slots)
  ""
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; We have our own states, register them.
  (oset this :states nil)
  (rudel-register-states
   this rudel-infinote-group-text-document-states))

(defmethod rudel-remote-operation
  ((this rudel-infinote-group-text-document) user operation)
  ""
  (with-slots (document) this
    (rudel-remote-operation document user operation)))

(provide 'rudel-infinote-group-text-document)
;;; rudel-infinote-group-text-document.el ends here
