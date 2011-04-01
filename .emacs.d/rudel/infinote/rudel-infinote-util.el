;;; rudel-infinote-util.el --- Miscellaneous functions for infinote backend
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, miscellaneous, utility
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
;; This file contains miscellaneous functions used in the infinote
;; backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;
(eval-when-compile
  (require 'cl))

(require 'rudel-util)
(require 'rudel-xml)

(require 'adopted-insert)
(require 'adopted-delete)
(require 'adopted-compound)
(require 'adopted-nop)


;;; Message serialization
;;

(defgeneric rudel-operation->xml ((this adopted-operation))
  "Generate an XML infoset from THIS operation.")

(defmethod rudel-operation->xml ((this adopted-insert))
  "Serialize THIS insert operation."
  (with-slots (from data) this
    `(insert
      ((pos . ,(format "%d" from)))
      ,data)))

(defmethod rudel-operation->xml ((this adopted-delete))
  "Serialize THIS delete operation."
  (with-slots (from length) this
    `(delete
      ((pos . ,(format "%d" from))
       (len . ,(format "%d" length))))))

(defmethod rudel-operation->xml ((this adopted-compound))
  "Serialize THIS compound operation."
  (with-slots (children) this
    (apply #'append
	   '(split)
	   (mapcar #'rudel-operation->xml children))))

(defmethod rudel-operation->xml ((this adopted-nop))
  "Serialize THIS nop operation."
  `(nop))


;;; Miscellaneous functions
;;

(defun rudel-infinote-parse-sequence-number (value)
  "Parse the string VALUE into a cons cell.
If string is of the form \"A/B\" the returned cell is (A . B)
where A and B are then of type number."
  ;; Make sure VALUE is of the expected form.
  (unless (string-match-p
	   (rx (seq
		string-start
		(1+ (any "0-9"))) "/" (1+ (any "0-9"))
		string-end)
	   value)
    (signal 'wrong-type-argument nil))

  (let ((values (split-string value "/")))
    (cons (string-to-number (nth 0 values))
	  (string-to-number (nth 1 values)))))

(defun rudel-infinote-generate-sequence-number (values)
  "Generate string representation of cons cell VALUES.
For a cons cell (A . B), the generated string is of the form
\"A/B\"."
  (concat (number-to-string (car values))
	  "/"
	  (number-to-string (cdr values))))

(defmacro rudel-infinote-embed-in-request (user &rest forms) ;; TODO duplicate?
  ""
  (declare (indent 1)
	   (debug (form &rest form)))
  (let ((user-var (make-symbol "user"))
	(id-var   (make-symbol "id")))
    `(let ((,user-var ,user))
       (with-slots (,id-var) ,user-var
	 `(request
	   ((user . ,(format "%d" ,id-var)))
	   ,,@forms))))
  )

(provide 'rudel-infinote-util)
;;; rudel-infinote-util.el ends here
