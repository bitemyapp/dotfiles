;;; rudel-transport-util.el --- Utility functions for Rudel transport functionality
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, backend, transport, utility, miscellaneous
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
;; This file contains utility functions for implementing Rudel
;; transport functionality. In particular, several transport filter
;; classes for common task are available:
;;
;; + `rudel-transport-filter'
;;   + `rudel-assembling-transport-filter'
;;   + `rudel-parsing-transport-filter'
;;   + `rudel-injecting-transport-filter'
;;   + `rudel-buffering-transport-filter'
;;   + `rudel-collecting-transport-filter'
;;   + `rudel-progress-reporting-transport-filter'


;;; History:
;;
;; 0.3 - Collecting transport filter
;;
;; 0.2 - Progress reporting transport filter
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-errors) ;; for `rudel-error'
(require 'rudel-transport)


;;; Error conditions
;;

;; rudel-invalid-state

(intern "rudel-invalid-state")

(put 'rudel-invalid-state 'error-conditions
     '(error
       rudel-error rudel-invalid-state))

(put 'rudel-invalid-state 'error-message
     "Operation invalid for current state.")


;;; Class rudel-transport-filter
;;

(defclass rudel-transport-filter (rudel-transport)
  ((transport :initarg  :transport
	      :type     rudel-transport
	      :documentation
	      "Transport object that performs the send and
receive operations this filter builds upon.")  ;; TODO should be read only
   (filter    :initarg  :filter
	      :type     (or null function)
	      :initform nil
	      :reader   rudel-filter
	      :writer   rudel-set-filter
	      :documentation
	      "Function that is called when data is received.")
   (sentinel  :initarg  :sentinel
	      :type     (or null function)
	      :initform nil
	      :reader   rudel-sentinel
	      :writer   rudel-set-sentinel
	      :documentation
	      "Function that is called when the status of the
transport changes."))
  "This class is a base class for transport filters that
transform a bidirectional data stream as it passes through them."
  :abstract t)

(defmethod slot-missing ((this rudel-transport-filter)
			 slot-name operation &optional new-value)
  "Make slots of underlying transport available as virtual slots of THIS."
  (cond
   ((and (or (eq slot-name :root-transport)
	     (eq slot-name 'root-transport))
	 (eq operation 'oref))
    (with-slots (transport) this
      (if (rudel-transport-filter-child-p transport)
	  (oref transport :root-transport)
	transport)))

   ((eq operation 'oref)
    (slot-value (oref this :transport) slot-name))

   ((eq operation 'oset)
    (set-slot-value (oref this :transport) slot-name new-value)))
  )

(defmethod no-applicable-method ((this rudel-transport-filter)
				 method &rest args)
  "Make methods of underlying transport callable as virtual methods of THIS."
  (apply method (oref this :transport) (rest args)))


;;; Class rudel-assembling-transport-filter
;;

(defclass rudel-assembling-transport-filter (rudel-transport-filter)
  ((buffer            :initarg  :buffer
		      :type     list
		      :initform nil
		      :documentation
		      "Stores message fragments until complete
messages can be assembled.")
   (assembly-function :initarg  :assembly-function
		      :type     function
		      :reader   rudel-assembly-function
		      :writer   rudel-set-assembly-function
		      :documentation
		      "Function that is called to assemble
message fragments into complete messages.")
   (fragment-function :initarg  :fragment-function
		      :type     (or null function)
		      :initform nil
		      :reader   rudel-fragment-function
		      :writer   rudel-set-fragment-function
		      :documentation
		      "Function that is called to fragment
complex messages into fragments. If the value is nil, messages
are sent unmodified."))
  "Objects of this class assemble received message fragments into
complete messages by calling an assembly function.")

(defmethod initialize-instance ((this rudel-assembling-transport-filter)
				slots)
  "Initialize THIS using SLOTS and install suitable handlers."
  ;; Initialize slots.
  (when (next-method-p)
    (call-next-method))

  (with-slots (transport) this
    (lexical-let ((this1 this))
      ;; Install a handler for received data that assembles messages
      ;; and passes them to the user-provided handler.
      (rudel-set-filter
       transport
       (lambda (data)

	 ;; Assemble complete fragments from stored fragments and
	 ;; possibly incomplete messages in DATA.
	 (with-slots (buffer assembly-function) this1
	   (rudel-assemble-fragments data buffer assembly-function))

	 ;; Process all complete messages.
	 (with-slots (filter) this1
	   (when filter
	     (mapc filter data)))))

      ;; Install a handler for sentinel events and pass them to the
      ;; user-provided handler.
      (rudel-set-sentinel transport (lambda (event)
				      (with-slots (sentinel) this1
					(when sentinel
					  (funcall sentinel event)))))))
  )

(defmethod rudel-send ((this rudel-assembling-transport-filter) data)
  "Send DATA using the transport of THIS."
  (with-slots (transport fragment-function) this
    (if fragment-function
	;; If there is a fragment function, fragment DATA and send the
	;; individual fragments.
	(dolist (fragment (funcall fragment-function data))
	  (rudel-send transport fragment))
      ;; If there is no fragment function, just send DATA.
      (rudel-send transport data))))


;;; Class rudel-parsing-transport-filter
;;

(defclass rudel-parsing-transport-filter (rudel-transport-filter)
  ((parse-function    :initarg  :parse-function
		      :type     function
		      :initform 'identity
		      :reader   rudel-parse-function
		      :writer   rudel-set-parse-function
		      :documentation
		      "Function that is called on each received
piece of data to transform it into a suitable representation.")
   (generate-function :initarg  :generate-function
		      :type     function
		      :initform 'identity
		      :reader   rudel-generate-function
		      :writer   rudel-set-generate-function
		      :documentation
		      "Function that is called on each outgoing
object to transform it into a string representation."))
  "Objects of this class convert sent and received data between
string representations and structured representations by calling
a pair of one parse and one generate function.")

(defmethod initialize-instance ((this rudel-parsing-transport-filter) slots)
  "Initialize THIS using SLOTS and install suitable handlers."
  ;; Initialize slots.
  (when (next-method-p)
    (call-next-method))

  (with-slots (transport) this
    (lexical-let ((this1 this))
      ;; Install a handler for received data that parses messages into
      ;; structured representations and passes those to the
      ;; user-provided handler.
      (rudel-set-filter
       transport
       (lambda (message-data)
	 ;; Parse and process all complete messages.
	 (with-slots (parse-function filter) this1
	   (when filter
	     (let ((message (funcall parse-function message-data)))
	       (funcall filter message))))))

      ;; Install a handler for sentinel events and pass them to the
      ;; user-provided handler.
      (rudel-set-sentinel transport (lambda (event)
				      (with-slots (sentinel) this1
					(when sentinel
					  (funcall sentinel event)))))))
  )

(defmethod rudel-send ((this rudel-parsing-transport-filter) message)
  "Apply generate function to MESSAGE, pass result to transport of THIS."
  (with-slots (transport generate-function) this
    (rudel-send transport (funcall generate-function message))))


;;; Class rudel-injecting-transport-filter
;;

(defclass rudel-injecting-transport-filter (rudel-transport-filter)
  ()
  "Objects of this class act as transport filters but do not
receive their data from underlying transports. Instead data is
injected by calling `rudel-inject'.")

(defmethod rudel-inject ((this rudel-injecting-transport-filter) data)
  "Inject DATA as if it was received from an underlying transport."
  (with-slots (filter) this
    (when filter
      (funcall filter data))))


;;; Class rudel-buffering-transport-filter
;;

(defclass rudel-buffering-transport-filter (rudel-transport-filter)
  ((queue-in  :initarg  :queue-in
	      :type     list
	      :initform nil
	      :documentation
	      "Queue of incoming data. Non-empty list only when
the filter is in stopped.")
   (queue-out :initarg  :queue-out
	      :type     list
	      :initform nil
	      :documentation
	      "Queue of outgoing data. Non-empty list only when
the filter is in stopped.")
   (stopped   :initarg  :stopped
	      :type     boolean
	      :initform nil
	      :documentation
	      "Flag describing whether the filter is currently
stopped."))
  "Objects of this class are transport filters that can queue
incoming and outgoing data and process it later.")

(defmethod initialize-instance ((this rudel-buffering-transport-filter)
				slots)
  "Initialize slots of THIS and install filter in underlying transport."
  ;; Initialize slots.
  (when (next-method-p)
    (call-next-method))

  (with-slots (transport) this
    (lexical-let ((this1 this))
      ;; Install `rudel-handle' as filter in underlying transport.
      (rudel-set-filter transport (lambda (data)
				    (rudel-handle this1 data)))

      ;; Install a handler for sentinel events and pass them to the
      ;; user-provided handler.
      (rudel-set-sentinel transport (lambda (event)
				      (with-slots (sentinel) this1
					(when sentinel
					  (funcall sentinel event)))))))
  )

(defmethod rudel-send ((this rudel-buffering-transport-filter) data)
  "Send DATA through THIS, queueing when necessary."
  (with-slots (transport stopped queue-out) this
    (if stopped
	;; If stopped, queue DATA.
	(push data queue-out)
      ;; Otherwise send DATA right away.
      (rudel-send transport data))))

(defmethod rudel-stop ((this rudel-buffering-transport-filter))
  "Stop THIS, queue incoming and out going data."
  (with-slots (stopped) this
    ;; The filter cannot be stopped if it already is stopped.
    (when stopped
      (signal 'rudel-invalid-state '(stopped)))

    ;; Set stopped flag.
    (setq stopped t))
  )

(defmethod rudel-start ((this rudel-buffering-transport-filter))
  "Start THIS, process queued incoming and outgoing data."
  (with-slots (stopped queue-in queue-out filter) this
    ;; Send queued outgoing data.
    (dolist (data (nreverse queue-out))
      (rudel-send this data))

    ;; Process queued incoming data.
    (when filter
      (mapc filter (nreverse queue-in)))

    ;; Clear incoming and outgoing queues, set stopped flag to nil.
    (setq queue-in  nil
	  queue-out nil
	  stopped   nil)))

(defmethod rudel-handle ((this rudel-buffering-transport-filter) data)
  "Handle DATA."
  (with-slots (stopped queue-in filter) this
    (if stopped
	;; If THIS is stopped, queue DATA.
	(push data queue-in)
      ;; Otherwise call FILTER with DATA right away.
      (when filter
	(funcall filter data))))
  )


;;; Class rudel-collecting-transport-filter
;;

(defclass rudel-collecting-transport-filter (rudel-transport-filter)
  ((queue        :initarg  :queue
		 :type     list
		 :initform nil
		 :documentation
		 "Data fragments queued for transmission in the
next chunk.")
   (queued-size  :initarg  :queued-size
		 :type     integer
		 :initform 0
		 :documentation
		 "The size of the queued data.")
   (flush-size   :initarg  :flush-size
		 :type     integer
		 :initform 1024
		 :documentation
		 "Amount of queued data leading to an immediate
transmission.")
   (timer        :initarg  :timer
		 :type     (or null timer)
		 :initform nil
		 :documentation
	         "A timer used to trigger the transmission of
queued data.")
   (delay        :initarg  :delay
		 :type     number
		 :initform 1
		 :documentation
		 "The maximum time to wait before transmitting
queued data even if it is smaller than a complete chunk."))
  "Object of this class are transport filters that queue data
sent through them until certain amounts of data are available for
transmission.")

(defmethod initialize-instance ((this rudel-collecting-transport-filter)
				slots)
  "Initialize slots of THIS and setup filter of underlying transport."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  (with-slots (transport) this
    (lexical-let ((this1 this))
      ;; Install a filter in the underlying transport.
      (rudel-set-filter transport (lambda (data)
				    (with-slots (filter) this1
				      (when filter
					(funcall filter data)))))

      ;; Install a handler for sentinel events and pass them to the
      ;; user-provided handler.
      (rudel-set-sentinel transport (lambda (event)
				      (with-slots (sentinel) this1
					(when sentinel
					  (funcall sentinel event)))))))
  )

(defmethod rudel-send ((this rudel-collecting-transport-filter) data)
  "Send or enqueue DATA."
  (with-slots (transport queue queued-size flush-size) this
    ;; Enqueue new data.
    (push data queue)
    (incf queued-size (length data))

    ;; Transmit data immediately if necessary, otherwise ensure the
    ;; timer is running.
    (if (< queued-size flush-size)
	(rudel-maybe-start-timer this)
      (rudel-maybe-cancel-timer this)
      (rudel-flush this)))
  )

(defmethod rudel-flush ((this rudel-collecting-transport-filter))
  "Transmit all data queued in THIS immediately."
  (with-slots (transport queue queued-size) this
    (rudel-send transport (mapconcat #'identity (nreverse queue) ""))
    (setq queue       nil
	  queued-size 0)))

(defmethod rudel-maybe-start-timer
  ((this rudel-collecting-transport-filter))
  "Start timer that runs `rudel-flush' when it expires."
  ;; If necessary, create a timer that runs `rudel-flush' when it
  ;; expires.
  (with-slots (timer delay) this
    (unless timer
      (lexical-let ((this1 this))
	(setq timer (run-at-time
		     delay nil ;; no repeat
		     (lambda ()
		       (rudel-flush this1)
		       (oset this1 :timer nil)))))))
  )

(defmethod rudel-maybe-cancel-timer
  ((this rudel-collecting-transport-filter))
  "Cancel the flush timer of this."
  (with-slots (timer) this
    (when timer
      (cancel-timer timer)
      (setq timer nil))))


;;; Class rudel-progress-reporting-transport-filter
;;

(defconst rudel-long-message-threshold 32768
  "Threshold for message size, above which messages are sent in
multiple chunks.")

(defconst rudel-long-message-chunk-size 16384
  "Chunk size used, when chunking long messages.")

;; TODO have a callback instead of the actual reporter
(defclass rudel-progress-reporting-transport-filter (rudel-transport-filter)
  ((reporter :initarg :reporter
	     :documentation
	     "TODO"))
  "TODO")

(defmethod initialize-instance
  ((this rudel-progress-reporting-transport-filter) slots)
  "TODO"
  (when (next-method-p)
    (call-next-method))

  (with-slots (reporter) this
    (setq reporter (make-progress-reporter "Sending data " 0.0 1.0)))

  ;; Install a handler as filter in underlying transport.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter transport (lambda (data)
				    (with-slots (filter) this1
				      (when filter
					(funcall filter data)))))))
  )

(defmethod rudel-send ((this rudel-progress-reporting-transport-filter)
		       data)
  "TODO"
  (with-slots (transport reporter) this
    (if (>= (length data) rudel-long-message-threshold)

	;; For huge messages, chunk the message data and transmit the
	;; chunks
	(let ((total   (/ (length data)
			  rudel-long-message-chunk-size))
	      (current 0))
	  (rudel-loop-chunks data chunk rudel-long-message-chunk-size
	    (progress-reporter-update reporter (/ (float current) total))
	    (rudel-send transport chunk)
	    (incf current))
	  (progress-reporter-done reporter))

      ;; Send small messages in one chunk
      (rudel-send transport data)))
  )


;;; Miscellaneous functions
;;

(defun rudel-transport-make-filter-stack (base specs)
  "Construct a filter stack on top of BASE according to SPECS.

SPECS is structured as follows:
SPECS ::= (SPEC*)
SPEC  ::= (CLASS KWARG*)
KWARG ::= KEYWORD VALUE
CLASS is the symbol of a class derived from
`rudel-transport-filter' KEYWORD is a keyword and VALUE is an
arbitrary expression and is used unevaluated.

The returned value is the \"top\" of the constructed stack (BASE
being the \"bottom\")."
  (let ((current base))
    (dolist (spec specs)
      (destructuring-bind (class &rest args) spec
	  (setq current (apply #'make-instance
			       class
			       :transport current
			       args))))
    current))

(provide 'rudel-transport-util)
;;; rudel-transport-util.el ends here
