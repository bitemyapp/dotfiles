;;; rudel-xml.el --- XML processing functions used by Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xml
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
;; Conversion functions:
;; + `xml->string'
;; + `string->xml'
;;
;; XML Macros:
;; + `with-tag-attrs'
;; + `do-tag-children'
;;
;; Stream parsing functions:
;; + `rudel-xml-toplevel-tag-positions'
;; + `rudel-xml-toplevel-tags'


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'xml)


;;; Miscellaneous functions
;;

(defun xml->string (xml &optional pretty-print)
  "Convert infoset XML to string representation.
PRETTY-PRINT is currently ignored."
  (if pretty-print
    (with-temp-buffer
      (xml-print (list xml))
      (buffer-string))
    (rudel-xml-print-node xml)))

(defun string->xml (string)
  "Convert STRING to XML infoset."
  (with-temp-buffer
    (insert string)
    (car (xml-parse-region (point-min) (point-max)))))


;;; Additional XML macros
;;

(defun rudel-xml--node-component (node-var name &optional type)
  "Generate code for accessing the NAME component of NODE-VAR.
The optional argument TYPE is used when name is :child
or :children to specify the element name of the child."
  (case name
    ;; Retrieve child text node of NODE-VAR.
    (:text
     (list
      `(car (xml-node-children ,node-var))
      nil))

    ;; Retrieve a single child named TYPE of NODE-VAR.
    (:child
     (unless type
       (signal 'wrong-number-of-arguments
	       (list 'rudel-xml--node-component name 2)))
     (list
      `(car (xml-get-children ,node-var (quote,type)))
      t))

    ;; Retrieve a list of children, optionally filtering by NAME.
    (:children
     (if type
	 (list
	  `(xml-get-children ,node-var (quote ,type))
	  t)
       (list
	`(xml-node-children ,node-var)
	nil)))

    ;; Retrieve an attribute value.
    (t
     (list
      `(xml-get-attribute ,node-var (quote ,name))
      nil)))
  )

(defun rudel-xml--parse-value (value-var type)
  "Generate code to parse the value of VALUE-VAR as TYPE.
Currently, TYPE can be one of 'string and 'number."
  (case type
    ;; String; no conversion
    (string
     value-var)

    ;; Convert to number
    (number
     `(when ,value-var
	(string-to-number ,value-var)))

    ;; For other types, signal an error.
    (t
     (signal 'wrong-type-argument (list 'type type))))
  )

(defmacro with-tag-attrs (attrs tag &rest body)
  "Execute BODY with bindings of attribute values of TAG according to forms in ATTRS.
ATTRS is structured as follows:
ATTRS   ::= (BINDING*)
BINDING ::= VAR | (VAR ATTR) | (VAR ATTR TYPE)
VAR is a symbol. ATTR is a symbol whose symbol-name is used as
tag name. TYPE can be 'number."
  (declare (indent 2)
	   (debug (listp form &rest form)))
  (let* ((node-var (make-symbol "node-var"))
	 (bindings
	  (mapcar
	   (lambda (attr)
	     (cond

	      ;; Simple form
	      ((symbolp attr)
	       `(,attr ,(car (rudel-xml--node-component
			      node-var attr))))

	      ;; Variable name and attribute name
	      ((= (length attr) 2)
	       (destructuring-bind (attr-var name) attr
		 (let ((value (car (rudel-xml--node-component
				    node-var name))))
		   `(,attr-var ,value))))

	      ;; Variable name, attribute name and type
	      ((= (length attr) 3)
	       (destructuring-bind (attr-var name type) attr
		 (destructuring-bind (value type-consumed)
		     (rudel-xml--node-component
		      node-var name type)
		   (if type-consumed
		       `(,attr-var ,value)
		     (let ((string (make-symbol "value-string")))
		       `(,attr-var (let ((,string ,value))
				     ,(rudel-xml--parse-value
				       string type))))))))

	      ;; Invalid form
	      (t
	       ;; TODO define a proper condition or use signal?
	       (error "Invalid tag clause: %s" attr))))
	   attrs)))

    ;; Construct binding forms
    `(let ((,node-var ,tag))
       (let (,@bindings)
	 (progn
	   ,@body))))
  )

(defmacro do-tag-children (var-and-tag &rest body)
  "Bind a var to children of a tag, eval BODY for each binding.
VAR-AND-TAG has to be a list of the form (VAR TAG)."
  (declare (indent 1)
	   (debug ((symbolp form) &rest form)))
  (let ((var      (nth 0 var-and-tag))
	(tag      (nth 1 var-and-tag))
	(children (make-symbol "children")))
    `(let ((,children (xml-node-children ,tag)))
       (dolist (,var ,children)
	 ,@body)))
  )


;;; Stream-based parsing
;;

(defun rudel-xml-toplevel-tag-positions (string)
  "Return positions of top-level XML tags in STRING.
The return value is a list of cons cells. Each cell contains a
start position and an end position."
  (let ((depth       0)
	(tag-opening nil)
	(start)
	(tags        nil))
    (dolist (index (number-sequence 0 (- (length string) 1)))
      (cond
       ;; Opening element
       ((= (aref string index) ?<)
	(setq tag-opening (/= (aref string (+ index 1)) ?/))
	(when (and (= depth 0)
		   tag-opening)
	  (setq start index)))

       ;; Closing element
       ((= (aref string index) ?>)
	(unless (or (= (aref string (- index 1)) ?/)
		    (= (aref string (- index 1)) ??))
	  (if tag-opening
	      (incf depth)
	    (decf depth)))
	(when (= depth 0)
	  (push (cons start (+ index 1)) tags)))))

    ;; Return list of tag positions.
    (nreverse tags)))

(defun rudel-xml-toplevel-tags (string)
  "Parse STRING as partial XML document, return complete and partial tags."
  (let ((tags (rudel-xml-toplevel-tag-positions string)))
    (list

     ;; Map top-level tag ranges into substrings.
     (mapcar
      (lambda (tag-range)
	(substring string (car tag-range) (cdr tag-range)))
      tags)

     ;; Add rest of the string
     (if tags
	 (substring string (apply #'max (mapcar #'cdr tags)))
       string)))
  )

(defun rudel-xml-assemble-tags (data storage)
  "Assemble complete XML tags in DATA, return list of tags and a rest.
The returned value is a list of the following form
\(COMPLETE INCOMPLETE\)
where complete COMPLETE is a list of complete tags and INCOMPLETE
is a string containing not yet complete tags."
  (destructuring-bind (tags buffer)
      (rudel-xml-toplevel-tags (concat storage data))
    (list tags buffer)))


;;; Utility functions
;;

(defun rudel-xml-print-node (node)
  "Serialize XML infoset NODE."
  (cond
   ((stringp node)
    node)

   (t
    (let ((name       (symbol-name (xml-node-name node)))
	  (attributes (xml-node-attributes node))
	  (children   (xml-node-children node)))
      (concat
       "<" name
       (when attributes " ")
       (mapconcat #'rudel-xml-print-attr attributes " ")
       (if children ">" "/>")
       (mapconcat #'rudel-xml-print-node children "")
       (when children
	 (concat "</" name ">"))))))
  )

(defun rudel-xml-print-attr (attr)
  "Print XML attribute ATTR which is a cons cell."
  (concat (symbol-name (car attr))
	  "="
	  "\"" (xml-escape-string (cdr attr)) "\""))

(provide 'rudel-xml)
;;; rudel-xml.el ends here
