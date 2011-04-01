;;; rudel-debug.el --- Debugging functions for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, debugging
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
;; along with rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This file contains debugging functions for Rudel. The most
;; important aspects are functions to data-debug central Rudel objects
;; and tracing support for basic Rudel objects.


;;; History:
;;
;; 0.2 - New tracing framework
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)
(require 'data-debug)
(require 'eieio-datadebug)

(require 'rudel-util)
(require 'rudel-state-machine)
(require 'rudel-transport-util)
(require 'rudel-socket)


;;; Customization
;;

(defgroup rudel-debug nil
  "Customization options related to Rudel's debugging functions."
  :group 'rudel)

(defface rudel-debug-sent-data-face
  '((default (:background "orange")))
  "Face used for sent data."
  :group 'rudel-debug)

(defface rudel-debug-received-data-face
  '((default (:background "light sky blue")))
  "Face used for received (but not yet processed) data."
  :group 'rudel-debug)

(defface rudel-debug-state-face
  '((default (:background "light gray")))
  "Face used when indicating state changes."
  :group 'rudel-debug)

(defface rudel-debug-special-face
  '((default (:background "light sea green")))
  "Face used for additional information."
  :group 'rudel-debug)

(defvar rudel-debug-tag-faces
  '((:sent     . (rudel-debug-sent-data-face     "<  "))
    (:received . (rudel-debug-received-data-face ">  "))
    (:state    . (rudel-debug-state-face         "|  "))
    (:special  . (rudel-debug-special-face       ";  ")))
  "Associate tag to faces and prefixes.")


;;; Data debug functions
;;

(defun rudel-adebug-discover ()
  "Analyze list of discoverable sessions in data debug buffer."
  (interactive)

  (with-current-buffer (data-debug-new-buffer "RUDEL-DISCOVERED-SESSIONS")
    (data-debug-insert-stuff-list (rudel-session-initiation-discover) "# ")))

(defun rudel-adebug-session ()
  "Analyze current session in data debug buffer."
  (interactive)

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (with-current-buffer (data-debug-new-buffer "RUDEL-SESSION")
    (data-debug-insert-thing rudel-current-session "# " "")))

(defun rudel-adebug-server (server)
  "Analyze server in data debug buffer."
  (interactive)

  (with-current-buffer (data-debug-new-buffer "RUDEL-SERVER")
    (data-debug-insert-thing server "# " "")))


;;; Advice stuff
;;

(defadvice rudel-join-session (after rudel-debug last activate)
  "Run data-debug inspection on newly created session objects."
  (require 'rudel-debug)
  (rudel-adebug-session))

(defadvice rudel-host-session (after rudel-debug last activate)
  "Run data-debug inspection on newly created server objects."
  (require 'rudel-debug)
  (rudel-adebug-server ad-return-value))


;;; Network functions
;;

(defun rudel-suspend-session-socket ()
  "Suspend the socket associated to the current session."
  (interactive)

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (with-slots (connection) rudel-current-session
    (with-slots (socket) connection
      (stop-process socket))))

(defun rudel-resume-session-socket ()
  "Resume the socket associated to the current session."
  (interactive)

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (with-slots (connection) rudel-current-session
    (with-slots (socket) connection
      (continue-process socket))))


;;; Reset functions
;;

(defun rudel-kill-processes ()
  "TODO"
  (interactive)
  (mapc #'delete-process (process-list)))

(defun rudel-reset ()
  "TODO"
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when rudel-buffer-document
	(setq rudel-buffer-document nil))))
  (rudel-kill-processes)
  (setq rudel-current-session nil))


;;; Utility functions
;;

(defgeneric rudel-debug-target (object)
  "Return debug stream name for OBJECT.")

(defmethod rudel-debug-target ((this eieio-default-superclass))
  "Default implementation simply uses the object name of THIS."
  (object-name-string this))

(defun rudel-debug-write-to-stream (stream tag label data
				    &optional object)
  "Insert DATA and maybe OBJECT into stream using TAG, LABEL as style."
  (let* ((buffer-name (format "*%s stream*" stream))
	 (buffer      (or (get-buffer buffer-name)
			  (data-debug-new-buffer buffer-name)))
	 (appearance  (cdr (assoc tag rudel-debug-tag-faces)))
	 (face        (when appearance
			(or (nth 0 appearance)
			    'default)))
	 (prefix      (or (nth 1 appearance)
			  ""))
	 (string      (cond
		       ((stringp data)
			data)
		       ((object-p data)
			(object-print data))
		       (t
			(prin1-to-string data)))))
    (save-excursion
      (set-buffer buffer)
      (goto-char 0)
      (insert prefix
	      (if label
		  (format
		   "%-8s"
		   (propertize label 'face 'font-lock-type-face))
		"        ")
	      " "
	      (propertize string 'face face)
	      (if (and (>= (length string) 1)
		       (string= (substring string -1) "\n"))
		  "" "\n"))
      (when object
	(data-debug-insert-thing
	 object
	 (concat prefix
		 (propertize "OBJECT   " 'face 'font-lock-type-face))
	 ""))))
    )

(defun rudel-debug-write (source tag label data &optional object)
  "Write DATA and OBJECT to debug stream associated to SOURCE.
TAG and LABEL determine the logging style."
  (rudel-debug-write-to-stream
   (rudel-debug-target source) tag label data object))


;;; State machine debugging
;;

(defvar rudel-debug-old-state nil
  "Saves state of state machines across one function call.")

(defmethod rudel-switch :before
  ((this rudel-state-machine) state &rest arguments)
  "Store name of STATE for later printing."
  (with-slots (state) this
    (setq rudel-debug-old-state
	  (if state (object-name-string state) "#start")))
  )

(defmethod rudel-switch :after
  ((this rudel-state-machine) state &rest arguments)
  "Log STATE and ARGUMENTS to debug stream."
  (with-slots (state) this
    (let ((old-state rudel-debug-old-state)
	  (new-state (object-name-string state)))
      (unless (string= old-state new-state)
	(rudel-debug-write
	 this
	 :special
	 "FSM"
	 (if arguments
	     (format "%s -> %s %s" old-state new-state arguments)
	   (format "%s -> %s" old-state new-state))))))
  )


;;; Debugging functions for `rudel-transport-filter'
;;

(defmethod rudel-debug-target ((this rudel-transport-filter))
  "Find target of filter THIS by looking at underlying transport."
  (with-slots (transport) this
    (rudel-debug-target transport)))


;;; Debugging functions for `rudel-assembling-transport-filter'
;;

(defmethod rudel-set-assembly-function :before
  ((this rudel-assembling-transport-filter) function)
  "Log change of assembly function to FUNCTION."
  (with-slots (socket assembly-function) this
    (rudel-debug-write
     this
     :special
     "ASSEMBLE"
     (format "%s -> %s"
	     (symbol-name assembly-function)
	     (symbol-name function))))
  )

(defmethod rudel-set-filter ((this rudel-assembling-transport-filter)
			     filter1)
  "Log DATA as it goes through THIS."
  (with-slots (filter) this
    (lexical-let ((this1   this)
		  (filter2 filter1))
      (setq filter (lambda (data)
		     (rudel-debug-write
		      this1
		      :received
		      "ASSEMBLE"
		      data)
		     (funcall filter2 data)))))
  )

(defmethod rudel-send :before
  ((this rudel-assembling-transport-filter) data)
  "Log DATA as it goes through THIS."
  (rudel-debug-write this :sent "RAW" data nil))


;;; Debugging function `rudel-parsing-transport-filter'
;;

(defmethod rudel-set-parse-function :before
  ((this rudel-parsing-transport-filter) function)
  "Log parse function change to FUNCTION."
  (with-slots (parse-function) this
    (rudel-debug-write
     this
     :special
     "PARSE"
     (format "%s -> %s"
	     (symbol-name parse-function)
	     (symbol-name function))))
  )

(defmethod rudel-set-generate-function :before
  ((this rudel-parsing-transport-filter) function)
  "Log generate function change to FUNCTION."
  (with-slots (generate-function) this
    (rudel-debug-write
     this
     :special
     "GENERATE"
     (format "%s -> %s"
	     (symbol-name generate-function)
	     (symbol-name function))))
  )

(defmethod rudel-set-filter ((this rudel-parsing-transport-filter)
			     filter1)
  "Log DATA as it goes through THIS."
  (with-slots (filter) this
    (lexical-let ((this1   this)
		  (filter2 filter1))
      (setq filter (lambda (data)
		     (rudel-debug-write
		      this1
		      :received
		      "PARSE"
		      (format "%s" data) data)
		     (funcall filter2 data)))))
  )

(defmethod rudel-send :before
  ((this rudel-parsing-transport-filter) string-or-data)
  "Log STRING-OR-DATA as it goes through THIS."
  (let ((formatted (cond
		    ((stringp string-or-data)
		     string-or-data)

		    ((object-p string-or-data)
		     (object-print string-or-data))

		    (t
		     (format "%s" string-or-data)))))
    (rudel-debug-write
     this
     :sent
     "GENERATE"
     formatted (unless (stringp string-or-data)
		 string-or-data)))
  )


;;; Socket transport debugging
;;

(defmethod rudel-set-filter ((this rudel-socket-transport)
			     filter)
  "Log DATA as it goes through THIS."
  (lexical-let ((this1   this)
		(filter1 filter))
    (oset
     this :filter
     (lambda (data)
       (rudel-debug-write this1 :received "SOCKET" data)
       (funcall filter1 data))))
  )

(defmethod rudel-send :before ((this rudel-socket-transport)
			       data)
  "Log DATA verbatim as it is sent through the socket of THIS."
  (rudel-debug-write this :sent "SOCKET" data nil))

(provide 'rudel-debug)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; rudel-debug.el ends here
