;;; rudel-mode.el --- Global and buffer-local Rudel minor modes
;;
;; Copyright (C) 2008, 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, mode
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
;; This file contains the following global and buffer-local Rudel
;; minor modes:
;;
;; + `rudel-header-subscriptions-minor-mode': Display subscribed users
;;   and their respective status in the header line
;; + `rudel-mode-line-publish-state-minor-mode': Display publication
;;   state of buffers in their mode lines
;; + `global-rudel-minor-mode': Installs a keymap and a Rudel menu


;;; History:
;;
;; 0.4 - Display buffer publication state in mode line.
;;
;; 0.3 - Display subscriptions in header line.
;;
;; 0.2 - Use define-minor-mode.
;;
;; 0.1 - Initial version.


;;; Code:
;;

(require 'easy-mmode)
(require 'easymenu)

(require 'rudel)
(require 'rudel-util) ;; for `object-add-hook', `object-remove-hook'
(require 'rudel-hooks)
(require 'rudel-display)


;;; Customization Options
;;

(defcustom rudel-header-subscriptions-use-images t
  "Use images when displaying subscribed users in header-line."
  :group 'rudel
  :type  'boolean
  :set   (lambda (symbol value)
	   (set-default symbol value)
	   (when (featurep 'rudel-mode)
	     (rudel-header-subscriptions--options-changed))))

(defcustom rudel-header-subscriptions-separator " "
  "String used to separate indicator strings of subscribed users."
  :group 'rudel
  :type  'string
  :set   (lambda (symbol value)
	   (set-default symbol value)
	   (when (featurep 'rudel-mode)
	     (rudel-header-subscriptions--options-changed))))

(defcustom rudel-mode-line-publish-state-unpublished-string "-"
  "String used to indicate not published state in the mode line."
  :group 'rudel
  :type  'string
  :set   (lambda (symbol value)
	   (set-default symbol value)
	   (when (featurep 'rudel-mode)
	     (rudel-mode-line-publish-state--options-changed))))

(defcustom rudel-mode-line-publish-state-published-string "P"
  "String used to indicate published state in the mode line."
  :group 'rudel
  :type  'string
  :set   (lambda (symbol value)
	   (set-default symbol value)
	   (when (featurep 'rudel-mode)
	     (rudel-mode-line-publish-state--options-changed))))

(dolist (v '(rudel-header-subscriptions-use-images
             rudel-header-subscriptions-separator
             rudel-mode-line-publish-state-unpublished-string
             rudel-mode-line-publish-state-published-string))
  (put v 'save-local-variable t))


;;; Header line subscriptions helper functions
;;

(defun rudel-header-subscriptions--make-format (document)
  "Return a Lisp object usable as `header-line-format' generated from DOCUMENT."
  (with-slots (subscribed) document
    (mapconcat
     (lambda (user)
       (rudel-display-string
	user rudel-header-subscriptions-use-images))
     subscribed rudel-header-subscriptions-separator)))

(defun rudel-header-subscriptions--update-from-document (document)
  "Update header-line of the buffer attached to DOCUMENT."
  (with-slots (buffer) document
    (when buffer
      (with-current-buffer buffer
	(setq header-line-format
	      (rudel-header-subscriptions--make-format document))
	(force-mode-line-update)))))

(defun rudel-header-subscriptions--update-from-buffer ()
  "Update header-line of the current buffer from associated document."
  (setq header-line-format
	(when (rudel-buffer-document)
	  (rudel-header-subscriptions--make-format
	   (rudel-buffer-document))))
  (force-mode-line-update))

(defun rudel-header-subscriptions--options-changed ()
  "Update headers in buffers that have header subscriptions mode enabled."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when rudel-header-subscriptions-minor-mode
	(rudel-header-subscriptions--update-from-buffer)))))


;;; Header line indication of users' status and activities
;;

(defun rudel-header-subscriptions--user-change (document user)
  "Update header line after USER changed."
  ;; Update the header line to reflect the changes to USER.
  (rudel-header-subscriptions--update-from-document document))

(defun rudel-header-subscriptions--add-user (document user)
  "Start monitoring USER and update header line."
  ;; Monitor USER.
  (lexical-let ((document1 document))
    (object-add-hook user 'change-hook
		     (lambda (user)
		       (rudel-header-subscriptions--user-change
			document1 user))))

  ;; Update the header line once to get the user added.
  (rudel-header-subscriptions--update-from-document document)
  )

(defun rudel-header-subscriptions--remove-user (document user)
  "Stop monitoring USER and update header line."
  ;; TODO Stop monitoring USER.
  ;; (object-remove-hook user 'change-hook

  ;; Update the header line once to get the user removed.
  (rudel-header-subscriptions--update-from-document document)
  )

;;;###autoload
(define-minor-mode rudel-header-subscriptions-minor-mode
  "Toggle Rudel header subscriptions minor mode.

This mode displays users subscribed to the document associated
with the buffer in the header-line. Depending on the kind of
session, additional information like connection status,
encryption or activity indication may be displayed with each
user.

If ARG is null, toggle Rudel header subscriptions mode.
If ARG is a number greater than zero, turn on Rudel header
subscriptions mode; otherwise, turn it off."
  :init-value nil
  :group      'rudel
  (cond
   ;; Emacs session is not interactive
   (noninteractive
    (setq rudel-header-subscriptions-minor-mode nil))

   ;; Mode is being enabled and the buffer has an attached document.
   ((and rudel-header-subscriptions-minor-mode
	 (rudel-buffer-document))
    (let ((document (rudel-buffer-document)))

      ;; Monitor all users that already are subscribed to the
      ;; document.
      (with-slots (subscribed) document
	(dolist (user subscribed)
	  (rudel-header-subscriptions--add-user document user)))

      ;; Monitor future (un)subscribe events.
      (object-add-hook document 'subscribe-hook
		       #'rudel-header-subscriptions--add-user)
      (object-add-hook document 'unsubscribe-hook
		       #'rudel-header-subscriptions--remove-user))

    ;; Update header line.
    (rudel-header-subscriptions--update-from-buffer))

   ;; Mode is being disabled, but the buffer has an attached document.
   ((and (not rudel-header-subscriptions-minor-mode)
	 (rudel-buffer-document))
    (let ((document (rudel-buffer-document)))

      ;; Stop monitoring all users that are subscribed to the
      ;; document.
      (with-slots (subscribed) document
	(dolist (user subscribed)
	  (rudel-header-subscriptions--remove-user document user)))

      ;; Stop monitoring (un)subscribe events.
      (object-remove-hook document 'subscribe-hook
			  #'rudel-header-subscriptions--add-user)
      (object-remove-hook document 'unsubscribe-hook
			  #'rudel-header-subscriptions--remove-user))

    ;; Reset header line to default format.
    (setq header-line-format default-header-line-format)
    (force-mode-line-update)) ;; TODO remove all handlers

   ;; No buffer document
   (t
    ;; Ensure the mode is disabled.
    (setq rudel-header-subscriptions-minor-mode nil)

    ;; Reset header line to default format.
    (setq header-line-format default-header-line-format)
    (force-mode-line-update)))
  )


;;; Global header subscriptions mode
;;

;; Tracking stuff for the global mode

(defun rudel-header-subscriptions--attach (document buffer)
  "Activate header subscriptions mode for BUFFER."
  (with-current-buffer buffer
    (rudel-header-subscriptions-minor-mode 1)))

(defun rudel-header-subscriptions--detach (document buffer)
  "Deactivate header subscriptions mode for BUFFER."
  (with-current-buffer buffer
    (rudel-header-subscriptions-minor-mode 0)))

(defun rudel-header-subscriptions--add-document (session document)
  "Watch DOCUMENT for attach/detach events."
  ;; When document is attached to a buffer, turn the mode on.
  (with-slots (buffer) document
    (when buffer
      (rudel-header-subscriptions--attach document buffer)))

  ;; Watch document for attaching and detaching.
  (object-add-hook
   document 'attach-hook #'rudel-header-subscriptions--attach)
  (object-add-hook
   document 'detach-hook #'rudel-header-subscriptions--detach))

(defun rudel-header-subscriptions--remove-document (session document)
  "Stop watching DOCUMENT for attach/detach events."
  ;; When document is attached to a buffer, turn the mode off.
  (with-slots (buffer) document
    (when buffer
      (rudel-header-subscriptions--detach document buffer)))

  ;; Stop watching document for attaching and detaching.
  (object-remove-hook
   document 'attach-hook #'rudel-header-subscriptions--attach)
  (object-remove-hook
   document 'detach-hook #'rudel-header-subscriptions--detach))

(defun rudel-header-subscriptions--session-start (session)
  "Watch SESSION documents and watch for added/removed documents."
  ;; Watch all documents in the session.
  (with-slots (documents) session
    (dolist (document documents)
      (rudel-header-subscriptions--add-document session document)))

  ;; Watch session for added/removed documents.
  (object-add-hook
   session 'add-document-hook
   #'rudel-header-subscriptions--add-document)
  (object-add-hook
   session 'remove-document-hook
   #'rudel-header-subscriptions--remove-document)
  )

(defun rudel-header-subscriptions--session-end (session)
  "Stop watching SESSION for added/removed documents."
  ;; Stop watching all documents in the session.
  (with-slots (documents) session
    (dolist (document documents)
      (rudel-header-subscriptions--remove-document session document)))

  ;; Stop watching session for added/removed documents.
  (object-remove-hook
   session 'add-document-hook
   #'rudel-header-subscriptions--add-document)
  (object-remove-hook
   session 'remove-document-hook
   #'rudel-header-subscriptions--remove-document)
  )

;;;###autoload
(define-globalized-minor-mode global-rudel-header-subscriptions-mode
  rudel-header-subscriptions-minor-mode
  rudel-header-subscriptions-minor-mode
  :group 'rudel)

(defadvice global-rudel-header-subscriptions-mode
  (around track-subscriptions activate)
  "Start/stop tracking subscriptions when the mode is (de)activated."
  (let ((value ad-do-it))
    (if value

	;; Add handlers to session start and end hooks and run the
	;; start handler on already started sessions.
	(progn

	  ;; Go through all existing sessions.
	  (mapc #'rudel-header-subscriptions--session-start
		(when rudel-current-session
		  (list rudel-current-session)))

	  ;; Watch for new/ended sessions.
	  (add-hook 'rudel-session-start-hook
		    #'rudel-header-subscriptions--session-start)
	  (add-hook 'rudel-session-end-hook
		    #'rudel-header-subscriptions--session-end))

      ;; Remove handlers from session start and end hooks and run the
      ;; end handler on active sessions.
      (mapc #'rudel-header-subscriptions--session-end
	    (when rudel-current-session
	      (list rudel-current-session)))

      (remove-hook 'rudel-session-start-hook
		   #'rudel-header-subscriptions--session-start)
      (remove-hook 'rudel-session-end-hook
		   #'rudel-header-subscriptions--session-end)))
  )


;;; Mode line indication of buffer state
;;

(defvar rudel-mode-line-publish-state-string
  (propertize
   "-"
   'mouse-face 'mode-line-highlight
   'help-echo  "Buffer is not published")
  "Contains a mode line fragment indicating the publication state
of the buffer.")
(make-variable-buffer-local 'rudel-mode-line-publish-state-string)
(put 'rudel-mode-line-publish-state-string 'risky-local-variable t)

(defun rudel-mode-line-publish-state--add-indicator-to-mode-line ()
  "Add Rudel publish state indicator to mode line."
  (let* ((new-format      (copy-list mode-line-format))
         (format-rest     (nthcdr
                           (position 'mode-line-modified mode-line-format)
                           new-format))
         (format-rest-cdr (cdr format-rest)))
    (setcdr format-rest (cons 'rudel-mode-line-publish-state-string
			      format-rest-cdr))
    (setq mode-line-format new-format))
  (force-mode-line-update))

(defun rudel-mode-line-publish-state--remove-indicator-from-mode-line ()
  "Remove Rudel publish state indicator from mode line."
  (let ((format-rest (nthcdr
		      (position 'mode-line-remote mode-line-format)
		      mode-line-format)))
    ;; Only change the mode line if our indicator is present.
    (when (eq (second format-rest) 'rudel-mode-line-publish-state-string)
      (setcdr format-rest (cddr format-rest))
      (force-mode-line-update))))

(defun rudel-mode-line-publish-state--update-string ()
  "Update variable `rudel-mode-line-publish-state-string'."
  ;; Update `rudel-mode-line-publish-state-string' with appropriate
  ;; propertized indicator string.
  (setq rudel-mode-line-publish-state-string
	(cond
	 ((rudel-buffer-document)
	  (propertize
	   rudel-mode-line-publish-state-published-string
	   'mouse-face 'mode-line-highlight
	   'help-echo  "Buffer is published"))
	 (t
	  (propertize
	   rudel-mode-line-publish-state-unpublished-string
	   'mouse-face 'mode-line-highlight
	   'help-echo  "Buffer is not published"))))

  ;; Update the mode line.
  (force-mode-line-update)
  )

(defun rudel-mode-line-publish-state--document-attach (document buffer)
  "Handle attaching of DOCUMENT to BUFFER.
When `rudel-mode-line-publish-state-minor-mode' is enabled in
BUFFER, update the state string."
  ;; Only act when BUFFER has the minor mode enabled.
  (with-current-buffer buffer
    (when rudel-mode-line-publish-state-minor-mode
      ;; Update the mode line.
      (rudel-mode-line-publish-state--update-string)

      ;; Watch for detaching of DOCUMENT from BUFFER.
      (object-add-hook
       document 'detach-hook
       #'rudel-mode-line-publish-state--document-detach)))
  )

(defun rudel-mode-line-publish-state--document-detach (document buffer)
  "Handle detaching of DOCUMENT from BUFFER."
  ;; Update the mode line of BUFFER.
  (with-current-buffer buffer
    (rudel-mode-line-publish-state--update-string))

  ;; Stop watching for detaching of DOCUMENT from BUFFER. It (or a
  ;; different document) has to attach again first, before the next
  ;; detaching can occur.
  (object-remove-hook
   document 'detach-hook
   #'rudel-mode-line-publish-state--document-detach)
  )

(defun rudel-mode-line-publish-state--options-changed ()
  "Update mode lines in buffers that have mode line publish state mode enabled."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when rudel-mode-line-publish-state-minor-mode
	(rudel-mode-line-publish-state--update-string)))))

;;;###autoload
(define-minor-mode rudel-mode-line-publish-state-minor-mode
  "Toggle Rudel mode line publish state minor mode.

This mode displays an indicator of the buffer's state with
respect to an associated Rudel document in the mode line. If the
buffer has an attached document, the string \"P\" is displayed
after the remote file indicator. Otherwise, the string \"-\" is
displayed.

If ARG is null, toggle Rudel mode line publish state minor mode.
If ARG is a number greater than zero, turn on Rudel minor mode
line publish state mode; otherwise, turn it off."
  :init-value nil
  :group      'rudel
  (cond
   ;; Emacs session is not interactive
   (noninteractive
    (setq rudel-mode-line-publish-state-minor-mode nil))

   ;; Mode is enabled
   (rudel-mode-line-publish-state-minor-mode
    ;; Extend and update the mode line, no matter whether the buffer
    ;; has a document or not.
    (rudel-mode-line-publish-state--add-indicator-to-mode-line)
    (rudel-mode-line-publish-state--update-string)

    ;; Watch document, if available or attach events, when a document
    ;; is not available.
    (let ((document (rudel-buffer-document)))
      (if document
	  ;; Handle detaching of the document from the buffer.
	  (object-add-hook
	   document 'detach-hook
	   #'rudel-mode-line-publish-state--document-detach)
	;; Handle attaching of documents to buffers. We use the global
	;; hook here, installing the handler twice is prevented by
	;; `add-hook'.
	(add-hook 'rudel-document-attach-hook
		  #'rudel-mode-line-publish-state--document-attach))))

   ;; Mode is disabled
   (t
    ;; Maybe stop watching for the document detaching from the buffer.
    (let ((document (rudel-buffer-document)))
      (when document
	(object-remove-hook
	 document 'detach-hook
	 #'rudel-mode-line-publish-state--document-detach)))

    ;; Remove the indicator from the mode line.
    (rudel-mode-line-publish-state--remove-indicator-from-mode-line)))
  )


;;; Global mode line publish state mode
;;

;;;###autoload
(define-globalized-minor-mode global-rudel-mode-line-publish-state-mode
  rudel-mode-line-publish-state-minor-mode
  rudel-mode-line-publish-state-minor-mode
  :group 'rudel)


;;; Global Rudel mode, menu and keymap
;;

(defvar rudel-minor-keymap
  (let ((map     (make-sparse-keymap))
	(sub-map (make-sparse-keymap)))
    ;; Define sub keymap
    (define-key sub-map "j" #'rudel-join-session)
    (define-key sub-map "h" #'rudel-host-session)
    (define-key sub-map "e" #'rudel-end-session)

    (define-key sub-map "c" #'rudel-change-color)

    (define-key sub-map "p" #'rudel-publish-buffer)
    (define-key sub-map "u" #'rudel-unpublish-buffer)
    (define-key sub-map "s" #'rudel-subscribe)

    ;; Bind the sub keymap into map
    (define-key map "\C-cc" sub-map)
    map)
  "Keymap used in Rudel minor mode.")

(when rudel-minor-keymap
  (easy-menu-define
    rudel-minor-menu rudel-minor-keymap "Rudel Minor Mode Menu"
    '("Rudel"
      [ "Join Session"             rudel-join-session
	                           (not rudel-current-session) ]
      [ "Leave Session"            rudel-leave-session
	                           rudel-current-session ]
      "---"
      [ "Host a Session"           rudel-host-session
	                           t ]
      "---"
      [ "Change Color"             rudel-change-color
	                           (and rudel-current-session
					(rudel-capable-of-p
					 (oref rudel-current-session :backend)
					 'change-color)) ] ; TODO bulky
      "---"
      [ "Publish current Buffer"   rudel-publish-buffer
	                           (and rudel-current-session
					(not (rudel-buffer-has-document-p))) ]
      [ "Unsubscribe from current Buffer" rudel-unpublish-buffer
	                           (rudel-buffer-has-document-p) ]
      [ "Subscribe to Document"    rudel-subscribe
	                           rudel-current-session ]
      "---"
      [ "Rudel Overview"           rudel-speedbar
	                           t ]
      [ "Show Backends"            rudel-backend-dump
	                           t ]
      "---"
      ( "Options"
	[ "Highlight Contributions in Authors' Colors"
	  (lambda ()
	    (interactive)
	    (setq rudel-overlay-author-display
		  (not rudel-overlay-author-display))
	    (rudel-overlay-options-changed))
	  :style    toggle
	  :selected rudel-overlay-author-display ]
	( "Show subscribed Users"
	  [ "In this Buffer"
	    rudel-header-subscriptions-minor-mode
	    :style    toggle
	    :selected rudel-header-subscriptions-minor-mode ]
	  [ "Globally"
	    global-rudel-header-subscriptions-mode
	    :style    toggle
	    :selected global-rudel-header-subscriptions-mode ] )
	( "Show Status in mode line"
	  [ "In this Buffer"
	    rudel-mode-line-publish-state-minor-mode
	    :style    toggle
	    :selected rudel-mode-line-publish-state-minor-mode ]
	  [ "Globally"
	    global-rudel-mode-line-publish-state-mode
	    :style    toggle
	    :selected global-rudel-mode-line-publish-state-mode ] ) ) )
    )
  )

;;;###autoload
(define-minor-mode global-rudel-minor-mode
  "Toggle global Rudel minor mode (No modeline indicator).

If ARG is null, toggle global Rudel mode.
If ARG is a number greater than zero, turn on global Rudel mode;
otherwise, turn it off.

\\{rudel-minor-keymap}"
  :init-value nil
  :keymap     rudel-minor-keymap
  :global     t
  :group      'rudel
  (cond
   ;; Emacs session is not interactive
   (noninteractive
    (setq global-rudel-minor-mode nil))

   ;; Mode is enabled
   (global-rudel-minor-mode
    )

   ;; Mode is disabled
   (t
    ))
  )

(provide 'rudel-mode)
;;; rudel-mode.el ends here
