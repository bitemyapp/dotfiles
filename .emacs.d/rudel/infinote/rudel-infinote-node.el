;;; rudel-infinote-node.el --- Base class of infinote node classes
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, node
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
;; An Infinote session organizes its documents in a tree. Text
;; documents are the leafs of this tree, Inner nodes are called
;; directory nodes.
;;
;; Example:
;;
;; + root       (directory node)
;;   + dir1     (directory node)
;;     + doc1   (text node)
;;   + doc2     (text node)
;;
;; The corresponding class hierarchy in Rudel looks like this:
;;
;; + `rudel-infinote-node'
;;   + `rudel-infinote-node-directory'
;;   + `rudel-infinote-document'
;;     + `rudel-infinote-document-text'


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-infinote-group)


;;; Class rudel-infinote-document
;;

(defclass rudel-infinote-node ()
  ((id     :initarg :id
	   :type    (integer 0)
	   :reader  rudel-id
	   :documentation
	   "The unique id of this node.")
   (parent :initarg :parent
	   :type    (or null rudel-infinote-node-child)
	   :reader  rudel-parent
	   :documentation
	   "The parent node of this node or nil for the root
node.")
   (group  :initarg :group
	   :type    (or null rudel-infinote-group-child)
	   :documentation
	   "The communication group associated with this node."))
  "Objects of this class form a tree the leafs of which
correspond to text documents or other content containing
documents.")

(defmethod rudel-unique-name ((this rudel-infinote-node))
  "Return a unique name for THIS by forming a path from the root node."
  (with-slots (parent) this
    (concat
     (when parent
       (rudel-unique-name parent))
     "/"
     (object-name-string this)))
  )

(provide 'rudel-infinote-node)
;;; rudel-infinote-node.el ends here
