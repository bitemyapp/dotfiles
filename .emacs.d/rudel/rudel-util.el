;;; rudel-util.el --- Miscellaneous functions for Rudel
;;
;; Copyright (C) 2008, 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, miscellaneous, util
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
;; This file contains miscellaneous classes and functions for Rudel.
;;
;; The following mixins are provided:
;;
;; + `rudel-hook-object'  - like regular hooks, but for objects
;; + `rudel-impersonator' - transparently access slots
;; + `rudel-delegator'    - transparently call methods


;;; History:
;;
;; 0.2 - Impersonation and delegation mixins
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'rudel-errors)


;;; Errors
;;

;; rudel-dispatch-error

(intern "rudel-dispatch-error")

(put 'rudel-dispatch-error 'error-conditions
     '(error
       rudel-error rudel-dispatch-error))

(put 'rudel-dispatch-error 'error-message
     "Could not dispatch message to handler")


;;; Class rudel-hook-object
;;

(defclass rudel-hook-object ()
  ()
  "Mixin for classes that offer one or more hooks for each of
their objects.

This idiom is usually called something like signal/slot or
event/subscription, but for Emacs, the notion of hooks seems more
appropriate."
  :abstract t)

(defmethod object-add-hook ((this rudel-hook-object)
			    hook function &optional append)
  "Add FUNCTION to HOOK for THIS.
If APPEND is non-nil FUNCTION becomes the last element in the
list of hooks."
  (let ((value (slot-value this hook)))
    (unless (member function value)
      (set-slot-value this hook
		      (if append (append value (list function))
			(cons function value)))))
  )

(defmethod object-remove-hook ((this rudel-hook-object)
			       hook function)
  "Remove FUNCTION from HOOK for THIS."
  (set-slot-value this hook
		  (remove function (slot-value this hook))))

(defmethod object-run-hook-with-args ((this rudel-hook-object)
				      hook &rest arguments)
  "Run HOOK of THIS with arguments ARGUMENTS."
  (let ((hook (slot-value this hook)))
    (apply #'run-hook-with-args 'hook this arguments)))


;;; Class rudel-impersonator
;;

(defclass rudel-impersonator ()
  ((impersonation-target-slot :type       symbol
			      :allocation :class
			      :documentation
			      "A symbol specifying the name of
the slot that holds the reference to the target object."))
  "A mixin that allows derived classes to transparently accesses
the slots of some other object as if they were their own slots."
  :abstract t)

(defmethod slot-missing ((this rudel-impersonator)
			 slot-name operation &optional new-value)
  "Look up SLOT-NAME in the state machine associated to THIS."
  (let ((target (slot-value this (oref this impersonation-target-slot))))
    (condition-case error
	(case operation
	  (oref
	   (slot-value target slot-name))

	  (oset
	   (set-slot-value target slot-name new-value)))
      (invalid-slot-name
       (if (next-method-p)
	   (call-next-method)
	 (apply #'signal error)))))
  )


;;; Class rudel-delegator
;;

(defclass rudel-delegator ()
  ((delegation-target-slot :type       symbol
			   :allocation :class
			   :documentation
			   "A symbol specifying the name of the
slot that holds the reference to the target object."))
  "A mixin that allows derived state classes to transparently
call methods of some other object as if they were their own
methods."
  :abstract t)

(defmethod no-applicable-method ((this rudel-delegator)
				 method &rest args)
  "Call METHOD on the target object instead of THIS."
  (let ((target (slot-value this (oref this delegation-target-slot))))
    (apply method target (rest args))))


;;; Fragmentation and assembling functions.
;;

(defmacro rudel-assemble-fragments (data storage function)
  "Return complete fragment in DATA, store excess data in STORAGE.
If the value of STORAGE is non-nil when calling, consider content
as leftover data from last call and concatenate with DATA before
processing.
FUNCTION is called to identify complete and partial fragments in
the data."
  (declare (debug (symbolp symbolp form)))
  (let ((complete (make-symbol "complete"))
	(partial  (make-symbol "partial")))
    ;; Ask FUNCTION to find complete and partial fragments in the
    ;; combined data DATA and STORAGE. Store the results in DATA
    ;; STORAGE.
    `(multiple-value-bind (,complete ,partial)
	 (funcall ,function ,data ,storage)
       (setq ,storage ,partial
	     ,data    ,complete)))
  )

(defun rudel-assemble-lines (data storage)
  "Split DATA at line breaks and return complete and incomplete lines.
DATA has to be a cons-cell which contains a string of new data in
its car and a list of old data strings in its cdr.
The returned value is a list of the following form
\(COMPLETE INCOMPLETE\)
where complete COMPLETE is a list of complete lines and
INCOMPLETE is a list of string fragments of not yet complete
lines."
  ;; Try to find a line break in data.
  (let ((index (position ?\n data :from-end t)))
    (list
     ;; Complete lines
     (when index
       (let ((lines (split-string (substring data 0 index) "\n")))
	 (setcar lines (concat
			(mapconcat #'identity (reverse storage) "")
			(car lines)))
	 lines))
     ;; Incomplete data
     (unless (and index (eq index (- (length data) 1)))
       (if index
	   (list (substring data (+ index 1)))
	 (cons data storage)))))
  )

(defmacro rudel-loop-fragments (data var &rest forms)
  "Execute FROMS with VAR subsequently bound to all fragments in DATA."
  (declare (indent 2)
	   (debug (form symbolp &rest form)))
  (let ((fragment (make-symbol "fragment")))
    `(dolist (,fragment ,data)
       (let ((,var ,fragment))
	 (progn ,@forms))))
  )

(defmacro rudel-loop-chunks (data var size &rest forms)
  "Execute FROMS in a loop with VAR bound to chunks of DATA of SIZE.
Unless (zerop (mod (length data) size) 0) the final chunk is
truncated. The expression SIZE is evaluated in each loop unless
it is a number."
  (declare (indent 3)
	   (debug (form symbolp numberp &rest form)))
  ;; If we got a constant number as SIZE, we can check right away.
  (when (and (numberp size) (<= size 0))
    (error "Size should be positive"))

  (let ((rest   (make-symbol "rest"))
	(amount (make-symbol "amount"))
	;; If SIZE has to be evaluated, we have to check at runtime.
	(check  (unless (numberp size)
		  `((when (<= ,size 0)
		      (error "Size should be positive"))))))
    `(progn
       ,@check
       (let ((,rest ,data)
	     (,var)
	     (,amount))
	 (while (not (string= ,rest ""))
	   (setq ,amount (min (length ,rest) ,size)
		 ,var    (substring ,rest 0 ,amount)
		 ,rest   (substring ,rest ,amount))
	   (progn ,@forms)))))
  )


;;; Miscellaneous functions
;;

(defun rudel-dispatch (object prefix name arguments)
  "Call method (concat PREFIX NAME) of OBJECT with ARGUMENTS.
If no such method can be found, the condition
rudel-dispatch-error is signaled."
  ;; Construct a matching symbol.
  (let* ((method (intern-soft (concat prefix name))))
    ;; If we found a suitable method, run it; Otherwise signal.
    (unless method
      (signal 'rudel-dispatch-error 'method-symbol-unbound))
    (condition-case error
	;; Try to call METHOD. This can still fail when METHOD is not
	;; defined for the class of OBJECT.
	(apply method object arguments)
      ;; Only handle a condition 'no-method-definition' that refers to
      ;; METHOD, otherwise continue unwinding.
      (no-method-definition
       (if (eq method (cadr error))
	   (signal 'rudel-dispatch-error 'no-method-for-object)
	 (signal (car error) (cdr error))))))
  )

(provide 'rudel-util)
;;; rudel-util.el ends here
