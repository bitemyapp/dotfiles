;;; rudel-state-machine.el --- A simple state machine for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, fsm
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
;; This is a simple implementation of a finite state machine
;; (FSM). The is modeled by rudel-state-machine class, objects of
;; which contain state objects of classes derived from
;; rudel-state. There are no explicit transition rules, since states
;; specify their successors.


;;; History:
;;
;; 0.2 - Hookable state machine
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'rudel-errors)
(require 'rudel-util) ;; for `rudel-hook-object'


;;; Errors related to the state machine
;;

;; rudel-state-error

(intern "rudel-state-error")

(put 'rudel-state-error 'error-conditions
     '(error
       rudel-error rudel-state-error))

(put 'rudel-state-error 'error-message
     "Invalid state or state transition")

;; rudel-invalid-successor-state

(intern "rudel-invalid-successor-state")

(put 'rudel-invalid-successor-state 'error-conditions
     '(error
       rudel-error rudel-state-error rudel-invalid-successor-state))

(put 'rudel-invalid-successor-state 'error-message
     "Invalid successor state in state transition")

;; rudel-entered-error-state

(intern "rudel-entered-error-state")

(put 'rudel-entered-error-state 'error-conditions
     '(error
       rudel-error rudel-state-error rudel-entered-error-state))

(put 'rudel-entered-error-state 'error-message
     "Transition to error state")

;; rudel-no-start-state

(intern "rudel-no-start-state")

(put 'rudel-no-start-state 'error-conditions
     '(error
       rudel-error rudel-state-error rudel-no-start-state))

(put 'rudel-no-start-state 'error-message
     "No start state specified for state machine")


;;; Class rudel-state
;;

(defclass rudel-state ()
  ()
  "A state that can be used in state machines."
  :abstract t)

(defgeneric rudel-accept ((this rudel-state) &rest arguments)
  "Executed when the machine receives an event while in state THIS.")

(defgeneric rudel-enter ((this rudel-state) &rest arguments)
  "Executed when the machine switches to state THIS.")

(defgeneric rudel-leave ((this rudel-state))
  "Executed when the machine leaves state THIS.")


;;; Class rudel-state-machine
;;

(defclass rudel-state-machine ()
  ((states :initarg  :states
	   :type     list ;; alist
	   :initform nil
	   :documentation
	   "A list (NAME . STATE) conses where NAME is a symbol
and STATE is an object of a class derived from rudel-state.")
   (state  :initarg  :state
	   :type     rudel-state-child
	   :writer   rudel-set-state
	   :documentation
	   "The current state of the machine."))
  "A finite state machine.")

(defmethod initialize-instance ((this rudel-state-machine) slots)
  "Initialize slots of THIS skipping :start initarg."
  ;; Call the next method, passing only non-virtual initargs.
  (when (next-method-p)
    (call-next-method
     this (rudel-state-machine-strip-initargs slots))))

(defmethod initialize-instance :after ((this rudel-state-machine) slots)
  "Set current state of THIS to a proper initial value.
If a start state is specified using the :start init argument to
the constructor, that state is used. If there is no such state,
the list of states is search for a state named 'start or 'new. If
that fails as well, the first state in the state list is used."
  (with-slots (states) this
    ;; Find a suitable start state and switch to it.
    (let* ((start-arg (plist-get slots :start))
	   (args      (when (listp start-arg)
			(cdr start-arg)))
	   (start     (or ;; First look for :start initarg.
		          (cond
			   ((rudel-state-child-p start-arg)
			    start-arg)
			   ((symbolp start-arg)
			    (rudel-find-state this start-arg))
			   ((listp start-arg)
			    (rudel-find-state this (car start-arg))))
			  ;; Then look for states named 'start or 'new.
			  (cdr (assoc 'start states))
			  (cdr (assoc 'new   states))
			  ;; Fallback to first state in state list.
			  (when states
			    (cdr (nth 0 states))))))
      (unless start
	(signal 'rudel-no-start-state nil))

      ;; Make start state the current state and call send an enter
      ;; message.
      (rudel-set-state this start)
      (rudel--switch-to-return-value
       this start (apply #'rudel-enter start args))))
  )

(defmethod rudel-find-state ((this rudel-state-machine) name)
  "Return state object for symbol NAME."
  (with-slots (states) this
    (cdr (assoc name states))))

(defmethod rudel-register-state ((this rudel-state-machine) name state)
  "Register STATE and its NAME with THIS state machine."
  (object-add-to-list this :states (cons name state) t))

(defmethod rudel-register-states ((this rudel-state-machine) states)
  "Register STATES with THIS state machine.
STATES is a list of cons cells whose car is a symbol - the name
of the state - and whose cdr is a class."
  (dolist (symbol-and-state states)
    (destructuring-bind (name . class) symbol-and-state
      (rudel-register-state
       this name (make-instance class (symbol-name name)))))
  )

(defmethod rudel-current-state ((this rudel-state-machine) &optional object)
  "Return name and, optionally, state object of the current state of THIS.
If OBJECT is non-nil, (NAME . OBJECT) is returned. Otherwise,
just NAME."
  (with-slots (states state) this
    (let ((state-symbol (car (find state states :key #'cdr :test #'eq))))
      (if object
	  (cons state-symbol state)
	state-symbol)))
  )

(defmethod rudel-accept ((this rudel-state-machine) &rest arguments)
  "Process an event described by ARGUMENTS."
  (with-slots (state) this
    ;; Let the current state decide which state is next.
    (let ((next (apply #'rudel-accept state arguments)))
      (cond
       ;; If NEXT is nil, a symbol or a state object, we switch states
       ;; without passing any data.
       ((or (null next) (symbolp next) (rudel-state-child-p next))
	(rudel-switch this next))

       ;; If NEXT is a list, it contains the symbol of the successor
       ;; state and additional data.
       ((listp next)
	(apply #'rudel-switch this next))

       ;; Other types cannot be processed.
       (t
	(signal 'wrong-type-argument (list (type-of next)))))))
  )

(defmethod rudel-switch ((this rudel-state-machine) next
			 &rest arguments)
  "Leave current state and switch to state NEXT.
ARGUMENTS are passed to the `rudel-enter' method of the successor
state."
  (with-slots (states state) this
    (cond
     ;; When NEXT is a state object, use it.
     ((rudel-state-child-p next))

     ;; When NEXT is nil, stay in the current state.
     ((null next)
      (setq next state))

     ;; When NEXT is a symbol (but not nil), look up the corresponding
     ;; state. Signal an error, if there is none.
     ((symbolp next)
      (let ((next-state (assoc next states)))
	(unless next-state
	  (signal 'rudel-invalid-successor-state
		  (list next '<- state)))
	(setq next (cdr next-state))))

     ;; Other types cannot be processed.
     (t
      (signal 'wrong-type-argument (list (type-of next)))))

    ;; Unless the successor state is equal to the current state, leave
    ;; the current state and switch to the successor.
    (if (and (eq state next)
	     (null arguments))
	;; Return state
	state

      ;; Notify (old) current state.
      (rudel-leave state)

      ;; Update current state.
      (rudel-set-state this next)

      ;; Notify (new) current state. Using the call's value as next
      ;; state is a bit dangerous since a long sequence of immediate
      ;; state switches could exhaust the stack.
      (rudel--switch-to-return-value
       this state (apply #'rudel-enter state arguments))))
  )

(defmethod rudel--switch-to-return-value ((this rudel-state-machine)
					  state next)
  "Switch from STATE to the next state indicated by NEXT.
STATE is the current state.
NEXT can nil, a list or a `rudel-state' object."
  (cond
   ;; Remain in current state.
   ((null next)
    state)
   ;; NEXT contains next state and arguments to pass to it when
   ;; switching.
   ((listp next)
    (apply #'rudel-switch this next))
   ;; Otherwise NEXT is a `rudel-state' object.
   (t
    (rudel-switch this next)))
  )

(defmethod object-print ((this rudel-state-machine) &rest strings)
  "Add current state to the string representation of THIS."
  (if (slot-boundp this 'state)
      (with-slots (state) this
	(apply #'call-next-method
	       this
	       (format " state: %s"
		       (object-name-string state))
	       strings))
    (call-next-method this " state: #start" strings))
  )


;;; Class rudel-hook-state-machine
;;

(defclass rudel-hook-state-machine (rudel-hook-object
				    rudel-state-machine)
  ((last-args   :initarg  :last-args
	        :type     list
	        :initform nil
	        :documentation
	        "In this slot `rudel-switch' stores the switch
arguments for processing in the `rudel-set-state' method.")
   ;; Hooks
   (accept-hook :initarg  :accept-hook
	        :type     list
	        :initform nil
		:documentation
		"This hook is run when the state machine accepts
input.")
   (switch-hook :initarg  :switch-hook
		:type     list
		:initform nil
		:documentation
		"This hook is run when the state machine switches
between states."))
  "State machine objects of this class run hooks when they accept
arguments and when they switch states.")

(defmethod rudel-accept :before ((this rudel-hook-state-machine)
				 &rest arguments)
  "This method runs 'accept-hook' before ARGUMENTS are processed."
  (apply #'object-run-hook-with-args this 'accept-hook arguments))

(defmethod rudel-switch :before ((this rudel-hook-state-machine) next
				 &rest arguments)
  "This method stores ARGUMENTS for later processing."
  (oset this :last-args arguments))

(defmethod rudel-set-state :before ((this rudel-hook-state-machine) next
				    &rest arguments)
  "This method runs 'switch-hook' when switching states."
  (with-slots (last-args) this
    (apply #'object-run-hook-with-args
	   this 'switch-hook next last-args)))


;;; Miscellaneous functions
;;

(defun rudel-state-machine-strip-initargs (slots)
  "Remove virtual initargs and their values from SLOTS."
  (let ((rest             slots)
	(replacement-args))
    ;; Remove :start initarg
    (while rest
      (unless (eq (car rest) :start)
	(push (first  rest) replacement-args)
	(push (second rest) replacement-args))
      (setq rest (cddr rest)))

    ;; Return remaining initargs.
    (reverse replacement-args))
  )

(defun rudel-state-wait (machine success-states
			 &optional error-states callback)
  "Repeatedly call CALLBACK until MACHINE is in a state in SUCCESS-STATES or ERROR-STATES.
MACHINE should be of type rudel-state-machine-child or at least
have a method `rudel-get-state'.

SUCCESS-STATES and ERROR-STATES are lists which contain the
names (as symbols) of success and error states respectively.
This function does not return when MACHINE enters states not in
SUCCESS-STATES or ERROR-STATES. As a result, a deadlock can occur
when MACHINE deadlocks or cycles through states not in either
list infinitely.

When non-nil, CALLBACK has to be a function that accepts one
argument of the form (SYMBOL . STATE) where SYMBOL is the name
symbol of the current state and STATE is the state object."
  ;; Wait until MACHINE enter a state in SUCCESS-STATES or
  ;; ERROR-STATES.
  (let ((result
	 (catch 'state-wait
	   (while t
	     ;; Retrieve current state.
	     (destructuring-bind (symbol . state)
		 (rudel-current-state machine t)

	       ;; Check against success and error states.
	       (when (memq symbol success-states)
		 (throw 'state-wait (cons 'success (cons symbol state))))
	       (when (memq symbol error-states)
		 (throw 'state-wait (cons 'error   (cons symbol state))))

	       ;; Run callback and sleep.
	       (when callback
		 (funcall callback (cons symbol state)))
	       (sleep-for 0.05))))))
    (when callback
      (funcall callback t))

    ;; If MACHINE ended up in an error state, signal
    (unless (eq (car result) 'success)
      (signal 'rudel-entered-error-state (cdr result)))
    ;; Return state
    (cdr result))
  )

(provide 'rudel-state-machine)
;;; rudel-state-machine.el ends here
