;;; rudel-hooks.el --- Hooks for Rudel events
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, hook
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
;; This file contains all global hooks (as opposed to hooks provided
;; by individual objects) provided by Rudel.


;;; History:
;;
;; 0.1 - Initial version.


;;; Code:
;;

(require 'eieio)

(require 'rudel-util) ;; for `object-add-hook', `object-remove-hook'


;;; Hook variables
;;

(defvar rudel-session-start-hook nil
  "This hook is run when a new session is started.
The only argument is the session object.")

(defvar rudel-session-end-hook nil
  "This hook is run when a session ends.
The only argument is the session object.")

(defvar rudel-session-add-user-hook nil
  "This hook is run when a user is added to a session.
The arguments are the session and the user.")

(defvar rudel-session-remove-user-hook nil
  "This hook is run when a user is removed from a session.
The arguments are the session and the user.")

(defvar rudel-session-add-document-hook nil
  "This hook is run when a document is added to a session.
The arguments are the session and the document.")

(defvar rudel-session-remove-document-hook nil
  "This hook is run when a document is removed from a session.
The arguments are the session and the document.")


(defvar rudel-user-change-hook nil
  "This hooks is run when a user object changes.
The only argument is the user object.")


(defvar rudel-document-attach-hook nil
  "This hook is run when a document is attached to a buffer.
The arguments are the document and the buffer.")

(defvar rudel-document-detach-hook nil
  "This hook is run when document is detached from its buffer.
The arguments are the document and the buffer.")


;;; Handlers
;;

(defun rudel-hooks--session-start (session)
  "Watch SESSION for added/removed users and documents."
  ;; Install handlers for the hooks of the session.
  (with-slots (users documents) session

    ;; Watch for session end.
    (object-add-hook session 'end-hook
		     #'rudel-hooks--session-end)

    ;; Watch all users in the session.
    (dolist (user users)
      (rudel-hooks--session-add-user session user))

    ;; Watch session for added/removed users.
    (object-add-hook
     session 'add-user-hook
     #'rudel-hooks--session-add-user)
    (object-add-hook
     session 'remove-user-hook
     #'rudel-hooks--session-remove-user)

    ;; Watch all documents in the session.
    (dolist (document documents)
      (rudel-hooks--session-add-document session document))

    ;; Watch session for added/removed documents.
    (object-add-hook
     session 'add-document-hook
     #'rudel-hooks--session-add-document)
    (object-add-hook
     session 'remove-document-hook
     #'rudel-hooks--session-remove-document))
  )

(defun rudel-hooks--session-end (session)
  "Stop watching SESSION for added/removed users and documents."
  ;; Remove handlers from the session.
  (with-slots (users documents) session

    ;; Stop watching for session end.
    (object-remove-hook session 'end-hook
			#'rudel-hooks--session-end)

    ;; Stop watching all users in the session.
    (dolist (user users)
      (rudel-hooks--session-remove-user session user))

    ;; Stop watching session for added/removed users.
    (object-remove-hook
     session 'add-user-hook
     #'rudel-hooks--session-add-user)
    (object-remove-hook
     session 'remove-user-hook
     #'rudel-hooks--session-remove-user)

    ;; Stop watching all documents in the session.
    (dolist (document documents)
      (rudel-hooks--session-remove-document session document))

    ;; Stop watching session for added/removed documents.
    (object-remove-hook
     session 'add-document-hook
     #'rudel-hooks--session-add-document)
    (object-remove-hook
     session 'remove-document-hook
     #'rudel-hooks--session-remove-document))

  ;; Run the hook.
  (run-hook-with-args 'rudel-session-end-hook session)
  )

(defun rudel-hooks--session-add-user (session user)
  "Watch USER for changes and run `rudel-session-add-user-hook'."
  ;; Watch USER.
  (object-add-hook user 'change-hook #'rudel-hooks--user-change)

  ;; Run the hook.
  (run-hook-with-args 'rudel-session-add-user-hook session user))

(defun rudel-hooks--session-remove-user (session user)
  "Stop watching USER and run `rudel-session-remove-user-hook'"
  ;; Stop watching USER.
  (object-remove-hook user 'change-hook #'rudel-hooks--user-change)

  ;; Run the hook.
  (run-hook-with-args 'rudel-session-remove-user-hook
		      session user))

(defun rudel-hooks--session-add-document (session document)
  "Watch DOCUMENT and run `rudel-session-add-document-hook'."
  ;; Watch document for attach/detach.
  (object-add-hook document 'attach-hook
		   #'rudel-hooks--document-attach)
  (object-add-hook document 'detach-hook
		   #'rudel-hooks--document-detach)

  ;; Run the hook.
  (run-hook-with-args 'rudel-session-add-document-hook
		      session document)
  )

(defun rudel-hooks--session-remove-document (session document)
  "Stop watching DOCUMENT and run `rudel-session-remove-document-hook'."
  ;; Stop watching DOCUMENT for attach/detach.
  (object-remove-hook
   document 'attach-hook #'rudel-hooks--document-attach)
  (object-remove-hook
   document 'detach-hook #'rudel-hooks--document-detach)

  ;; Run the hook.
  (run-hook-with-args 'rudel-session-remove-document-hook
		      session document)
  )


(defun rudel-hooks--user-change (user)
  "Run `rudel-user-change-hook' with argument USER."
  (run-hook-with-args 'rudel-user-change-hook user))


(defun rudel-hooks--document-attach (document buffer)
  "Run `rudel-document-attach-hook' with arguments DOCUMENT and BUFFER."
  (run-hook-with-args 'rudel-document-attach-hook
		      document buffer))

(defun rudel-hooks--document-detach (document buffer)
  "Run `rudel-document-detach-hook' with arguments DOCUMENT and BUFFER."
  (run-hook-with-args 'rudel-document-detach-hook
		      document buffer))


;;; Initialization
;;

(defun rudel-hooks--install-handlers ()
  "Install handlers for session start/end."
  ;; Install handlers for already started sessions.
  (when (boundp 'rudel-current-session)
    (mapc
     #'rudel-hooks--session-start
     (when rudel-current-session
       (list rudel-current-session))))

  ;; Watch for session start/end.
  (add-hook 'rudel-session-start-hook
	    #'rudel-hooks--session-start)
  )

(defun rudel-hooks--uninstall-handlers ()
  "Uninstall handlers for session start/end."
  ;; Stop watching session start/end.
  (remove-hook 'rudel-session-start-hook
	       #'rudel-hooks--session-start)

  ;; Uninstall handlers for already started sessions.
  (when (boundp 'rudel-current-session)
    (mapc
     #'rudel-hooks--session-end
     (when rudel-current-session
       (list rudel-current-session))))
  )

(rudel-hooks--install-handlers)

(provide 'rudel-hooks)
;;; rudel-hooks.el ends here
