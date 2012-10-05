;;; ensime-debug.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-and-compile
  (require 'cl))

(defgroup ensime-db nil
  "Customization of ensime debugger support."
  :group 'ensime
  :prefix 'ensime-db)

(defface ensime-breakpoint-face
  '((((class color) (background dark)) (:background "DarkGreen"))
    (((class color) (background light)) (:background "LightGreen"))
    (t (:bold t)))
  "Face used for marking lines with breakpoints."
  :group 'ensime-ui)

(defface ensime-pending-breakpoint-face
  '((((class color) (background dark)) (:background "DarkGreen"))
    (((class color) (background light)) (:background "LightGreen"))
    (t (:bold t)))
  "Face used for marking lines with a pending breakpoints."
  :group 'ensime-ui)

(defface ensime-marker-face
  '((((class color) (background dark)) (:background "DarkGoldenrod4"))
    (((class color) (background light)) (:background "DarkGoldenrod2"))
    (t (:bold t)))
  "Face used for marking the current point of execution."
  :group 'ensime-ui)

(defface ensime-writable-value-face
  '((t (:bold t)))
  "Face used for marking editable values."
  :group 'ensime-ui)

(defvar ensime-db-default-main-args nil
  "History of arguments passed to main class.")

(defvar ensime-db-default-main-class nil
  "History of main class to debugger.")

(defvar ensime-db-default-hostname "localhost"
  "History of vm hostname.")

(defvar ensime-db-default-port "9999"
  "History of vm port.")

(defvar ensime-db-history nil
  "History of argument lists passed to jdb.")

(defvar ensime-db-buffer-name "*ensime-debug-session*")
(defvar ensime-db-value-buffer "*ensime-debug-value*")
(defvar ensime-db-backtrace-buffer "*ensime-db-backtrace-buffer*")
(defvar ensime-db-output-buffer "*ensime-db-output-buffer*")

(defvar ensime-db-active-thread-id nil
  "The unique id of the which is currently receiving debug
 commands.")

(defvar ensime-db-thread-suspended-hook nil
  "Hook called whenever the debugger suspends a thread.")


;; Helpers for building DebugLocation structures
(defun ensime-db-make-obj-ref-location (obj-id)
  `(:type reference :object-id ,obj-id))
(defun ensime-db-make-array-el-location (obj-id index)
  `(:type element :object-id ,obj-id :index ,index))
(defun ensime-db-make-obj-field-location (obj-id field-name)
  `(:type field :object-id ,obj-id :name ,field-name))
(defun ensime-db-make-stack-slot-location (thread-id frame offset)
  `(:type slot :thread-id ,thread-id :frame ,frame :offset ,offset))


;; Event Handling

(defun ensime-db-handle-event (evt)
  (case (plist-get evt :type)
    (output (ensime-db-handle-output evt))
    (start (ensime-db-handle-start evt))
    (step (ensime-db-handle-step evt))
    (breakpoint (ensime-db-handle-break-hit evt))
    (death (ensime-db-handle-shutdown evt))
    (disconnect (ensime-db-handle-shutdown evt))
    (exception (ensime-db-handle-exception evt))
    (threadStart)
    (threadDeath)
    (otherwise (ensime-db-handle-unknown-event evt))
    ))

(defun ensime-db-handle-unknown-event (evt)
  (message "Unknown event: %s" evt))

(defun ensime-db-handle-output (evt)
  (with-current-buffer (get-buffer-create
			ensime-db-output-buffer)
    (insert (plist-get evt :body))))

(defun ensime-db-handle-exception (evt)
  (setq ensime-db-active-thread-id
	(plist-get evt :thread-id))

  (message "Exception on thread %s..."
	   (plist-get evt :thread-id))

  (when (and (plist-get evt :file)
	     (plist-get evt :line))
    (ensime-db-set-debug-marker
     (plist-get evt :file)
     (plist-get evt :line)))

  (when-let (exc-val (ensime-rpc-debug-value
		      (ensime-db-make-obj-ref-location
		       (plist-get evt :exception))))
    (ensime-ui-show-nav-buffer
     ensime-db-value-buffer
     exc-val t))
  ;;  (run-hooks 'ensime-db-thread-suspended-hook)
  )

(defun ensime-db-handle-start (evt)
  (message "Debug VM started. Set breakpoints and then execute ensime-db-run."))

(defun ensime-db-handle-step (evt)
  (setq ensime-db-active-thread-id
	(plist-get evt :thread-id))
  (ensime-db-set-debug-marker
   (plist-get evt :file)
   (plist-get evt :line))
  (message "Thread '%s' suspended at %s : %s"
	   (plist-get evt :thread-name)
	   (file-name-nondirectory (plist-get evt :file))
	   (plist-get evt :line))
  (run-hooks 'ensime-db-thread-suspended-hook)
  )

(defun ensime-db-handle-break-hit (evt)
  (setq ensime-db-active-thread-id
	(plist-get evt :thread-id))
  (ensime-db-set-debug-marker
   (plist-get evt :file)
   (plist-get evt :line))
  (message "Thread '%s' hit breakpoint at %s : %s"
	   (plist-get evt :thread-name)
	   (file-name-nondirectory (plist-get evt :file))
	   (plist-get evt :line))
  (run-hooks 'ensime-db-thread-suspended-hook)
  )

(defun ensime-db-handle-shutdown (evt)
  (message "Debug VM Quit")
  (ensime-db-clear-marker-overlays)
  (setq ensime-db-active-thread-id nil))


;; UI

(defun ensime-db-set-debug-marker (file line)
  "Open location in a new window."
  (ensime-db-clear-marker-overlays)
  (when-let (ov (ensime-make-overlay-at
		 file line nil nil
		 "Debug Marker"
		 'ensime-marker-face))
    (push ov ensime-db-marker-overlays))

  (ensime-goto-source-location
   (list :file file :line line)
   'window)
  )


(defun ensime-db-create-breapoint-overlays (positions face)
  (dolist (pos positions)
    (let ((file (ensime-pos-file pos))
	  (line (ensime-pos-line pos)))
      (when (and (stringp file) (integerp line))
	(when-let (ov (ensime-make-overlay-at
		       file line nil nil
		       "Breakpoint"
		       face))
	  (push ov ensime-db-breakpoint-overlays))))))


(defun ensime-db-refresh-breakpoints ()
  "Refresh all breakpoints from server."
  (ensime-db-clear-breakpoint-overlays)
  (let* ((bps (ensime-rpc-debug-list-breakpoints))
	 (active (plist-get bps :active))
	 (pending (plist-get bps :pending)))
    (ensime-db-create-breapoint-overlays
     active 'ensime-breakpoint-face)
    (ensime-db-create-breapoint-overlays
     pending 'ensime-pending-breakpoint-face)))


(defvar ensime-db-breakpoint-overlays '())

(defun ensime-db-clear-breakpoint-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay ensime-db-breakpoint-overlays)
  (setq ensime-db-breakpoint-overlays '()))


(defvar ensime-db-marker-overlays '())

(defun ensime-db-clear-marker-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay ensime-db-marker-overlays)
  (setq ensime-db-marker-overlays '()))

(defmacro* ensime-db-with-active-thread ((tid-sym) &rest body)
  `(if ensime-db-active-thread-id
       (let ((,tid-sym ensime-db-active-thread-id))
	 ,@body)
     (message "No active debug thread.")))



(defun ensime-db-value-p (val)
  (not (null (plist-get val :val-type))))

(defun ensime-db-backtrace-p (val)
  (not (null (plist-get val :frames))))

(defun ensime-db-ui-insert-backtrace (val)
  (ensime-db-visit-backtrace
   val
   (list
    :header
    (lambda (thread-id thread-name)
      (ensime-insert-with-face
       (format "Thread: %s\n" thread-name)
       font-lock-comment-face))

    :frame
    (lambda (class-name method-name file line this-obj-id)
      (insert "\n\n")
      (let ((heading (format "%s at %s:%s"
			     method-name
			     (file-name-nondirectory file)
			     line)))
	(ensime-insert-with-face (make-string (length heading) ?\-)
				 font-lock-constant-face)
	(insert "\n")
	(ensime-insert-link heading
			    file nil font-lock-constant-face line)
	(insert "\n")
	(ensime-insert-with-face (make-string (length heading) ?\-)
				 font-lock-constant-face)
	(insert "\n")
	font-lock-constant-face)
      (ensime-db-ui-insert-object-link "this" this-obj-id)
      (insert "\n"))

    :local-var
    'ensime-db-ui-insert-stack-var

    )))


(defun ensime-db-obj-to-ref (val)
  (list :val-type 'ref :object-id
	(plist-get val :object-id)))

(defun ensime-db-commit-writable-values ()
  "For each dirty writable value overlay in buffer, commit that value
 to the database using the corresponding writer-func"
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (and (overlay-get ov 'ensime-debug-value)
	       (overlay-get ov 'is-dirty))
      (let ((writer-func (overlay-get ov 'writer-func))
	    (value (ensime-chomp (buffer-substring-no-properties
				  (overlay-start ov)
				  (overlay-end ov)))))
	(let ((status (funcall writer-func value)))
	  (when status
	    (overlay-put ov 'face 'ensime-writable-value-face))
	  )))))


(defun ensime-db-ui-handle-writable-value-modification
  (ov is-after beg end &optional pre-mod-length)
  "Invoked by emacs overlay system when editable value is modified. Handles
 updating of UI to represent edited state."
  (when is-after
    (overlay-put ov 'face 'ensime-errline-highlight)
    (overlay-put ov 'is-dirty t)
    (message "Type C-c C-c to commit all changes to the debugged JVM.")))


(defun ensime-db-ui-make-writable-value (start end writer-func)
  "Make a range of characters writable in the current buffer. Edits to the
 will result in ui change to indicate dirty state."
  (let ((ov (make-overlay start end (current-buffer) t t)))
    (overlay-put ov 'face 'ensime-writable-value-face)
    (overlay-put ov 'ensime-debug-value t)
    (overlay-put ov 'writer-func writer-func)
    (overlay-put ov 'local-map (let ((map (make-sparse-keymap)))
				 (define-key map (kbd "C-c C-c")
				   'ensime-db-commit-writable-values)
				 map
				 ))
    (let ((hooks (list 'ensime-db-ui-handle-writable-value-modification)))
      (overlay-put ov 'modification-hooks hooks)
      (overlay-put ov 'insert-in-front-hooks hooks)
      (overlay-put ov 'insert-behind-hooks hooks))))

(defun ensime-db-ui-insert-stack-var
  (thread-id frame-index index name summary type-name)
  "Inserts a backtrace entry for a single local variable.
 The value will be inserted as a writable value."
  (let ((inhibit-modification-hooks t))
    (ensime-insert-action-link
     name
     `(lambda (x)
	(let ((stack-val (ensime-rpc-debug-value
			  (ensime-db-make-stack-slot-location
			   ,thread-id ,frame-index ,index))))
	  (ensime-ui-show-nav-buffer
	   ensime-db-value-buffer
	   stack-val t nil)))
     font-lock-keyword-face)
    (insert ": ")
    (ensime-insert-with-face
     (ensime-last-name-component type-name)
     'font-lock-type-face)
    (insert " = ")
    (let ((val-start (point))
	  (val-end))
      (insert summary)
      (setq val-end (point))
      (insert "\n")

      ;; Make overlay after the fact so we don't tread
      ;; inside our overlays as we go.
      (ensime-db-ui-make-writable-value
       (- val-start 1) val-end
       `(lambda (value) (ensime-rpc-debug-set-value
			 (ensime-db-make-stack-slot-location
			  ,thread-id
			  ,frame-index
			  ,index)
			 value))))))


(defun ensime-db-ui-insert-object-link (text obj-id)
  (let ((ref (list :val-type 'ref :object-id obj-id)))
    (ensime-insert-action-link
     text
     `(lambda (x)
	(ensime-ui-show-nav-buffer
	 ensime-db-value-buffer
	 ',ref t nil)))))

(defun ensime-db-ui-indent (width)
  (insert (make-string width ?\ )))

(defun ensime-db-ui-insert-value (val expansion)
  (ensime-db-visit-value
   val expansion '()

   (list
    :primitive
    (lambda (val path)
      (ensime-db-ui-indent (length path))
      (insert (format "%s: %s\n"
		      (ensime-escape-control-chars
		       (plist-get val :summary))
		      (plist-get val :type-name))))


    :string
    (lambda (val path)
      (ensime-db-ui-indent (length path))
      (ensime-insert-with-face
       (format "%s\n"
	       (ensime-escape-control-chars
		(plist-get val :summary)))
       'font-lock-string-face))


    :object
    (lambda (val path)
      (ensime-db-ui-indent (length path))
      (insert (format "Instance of %s\n"
		      (plist-get val :type-name))))



    :object-field
    (lambda (val f path)
      (let* ((name (plist-get f :name))
	     (summary (plist-get f :summary))
	     (type-name (plist-get f :type-name)))

	(ensime-db-ui-indent (length path))
	(ensime-insert-action-link
	 name
	 `(lambda (x)
	    (let* ((new-expansion (ensime-db-grow-expansion
				   ensime-db-buffer-value-expansion
				   ',(append path (list name))))
		   (new-val (plist-put (copy-list ensime-db-buffer-root-value)
				       :expansion new-expansion)))
	      (ensime-ui-show-nav-buffer
	       ensime-db-value-buffer
	       new-val t nil t)))
	 font-lock-keyword-face)

	(insert ": ")
	(ensime-insert-with-face
	 (ensime-last-name-component (plist-get f :type-name))
	 'font-lock-type-face)
	(insert (format " = %s" summary))
	(insert "\n")))


    :array
    (lambda (val path)
      (ensime-db-ui-indent (* 2 (length path)))
      (insert (format "Array[%s] of length %s\n"
		      (plist-get val :element-type-name)
		      (plist-get val :length))))


    :array-el
    (lambda (val i path)
      (ensime-db-ui-indent (length path))
      (ensime-insert-action-link
       (format "[%s]" i)
       `(lambda (x)
	  (let* ((new-expansion (ensime-db-grow-expansion
				 ensime-db-buffer-value-expansion
				 ',(append path (list i))))
		 (new-val (plist-put (copy-list ensime-db-buffer-root-value)
				     :expansion new-expansion)))
	    (ensime-ui-show-nav-buffer
	     ensime-db-value-buffer
	     new-val t nil t)))
       font-lock-keyword-face)

      (insert "\n"))

    :null
    (lambda (val path)
      (ensime-db-ui-indent (length path))
      (insert "null: Null\n"))

    )))



(make-variable-buffer-local
 (defvar ensime-db-buffer-value-expansion '()
   "The value expansion associated with this buffer."))

(make-variable-buffer-local
 (defvar ensime-db-buffer-root-value nil
   "The value expansion associated with this buffer."))

(defvar ensime-db-ui-value-handler
  (list
   :init (lambda (info)
	   (setq ensime-db-buffer-root-value info)
	   (setq ensime-db-buffer-value-expansion (plist-get info :expansion))
	   (ensime-db-ui-insert-value
	    info ensime-db-buffer-value-expansion))
   :update (lambda (info))
   :help-text "Press q to quit, use n,s,o,c to control debugger."
   :keymap `(
	     (,(kbd "n") ,'ensime-db-next)
	     (,(kbd "s") ,'ensime-db-step)
	     (,(kbd "o") ,'ensime-db-step-out)
	     (,(kbd "c") ,'ensime-db-continue)
	     )
   ))


(defvar ensime-db-ui-backtrace-handler
  (list
   :init (lambda (info)
	   (ensime-db-ui-insert-backtrace
	    info))
   :update (lambda (info))
   :help-text "Press q to quit, use n,s,o,c to control debugger."
   :writable t
   :keymap `(
	     (,(kbd "n") ,'ensime-db-next)
	     (,(kbd "s") ,'ensime-db-step)
	     (,(kbd "o") ,'ensime-db-step-out)
	     (,(kbd "c") ,'ensime-db-continue)
	     (,(kbd "C-c C-c") ,'ensime-db-commit-writable-values)
	     )
   ))


(defun ensime-db-update-backtraces ()
  (when (get-buffer ensime-db-backtrace-buffer)
    (ensime-db-backtrace t)))


;; (message "%s" (ensime-db-grow-expansion '(nil ("a") ("b" ("c"))) '("b" "c" "d")))
;; (message "%s" (ensime-db-grow-expansion '(nil) '("b")))
;; (message "%s" (ensime-db-grow-expansion '(nil ("b")) '("b" "c")))
;; (message "%s" (ensime-db-grow-expansion '(nil ("b" (1) (2))) '("b" 2 "q")))
;; (message "%s" (ensime-db-grow-expansion nil '("a")))
;; (message "%s" (ensime-db-grow-expansion '(("dude")) '("a")))
(defun ensime-db-grow-expansion (expansion-in
				 path)
  (let ((expansion (copy-tree expansion-in)))

    (cond

     ((null path) expansion)

     ((assoc (car path) expansion)
      (let* ((sub (assoc (car path) expansion)))
	(setcdr sub
		(ensime-db-grow-expansion
		 (cdr sub) (cdr path)))
	expansion
	))

     (t (append expansion (list (list (car path))))))))


(defun ensime-db-sub-expansion (expansion index-name)
  (assoc index-name expansion))


(defun ensime-db-visit-obj-field (val
				  field
				  expansion
				  path
				  visitor)
  (let ((field-name (plist-get f :name)))
    (funcall (plist-get visitor :object-field) val f path)
    (when-let (sub-expansion (ensime-db-sub-expansion
			      expansion field-name))
      (let ((sub-val (ensime-rpc-debug-value
		      (ensime-db-make-obj-field-location
		       (plist-get val :object-id)
		       field-name)
		      )))
	(ensime-db-visit-value sub-val sub-expansion
			       (append path (list field-name))
			       visitor)
	))))



(defun ensime-db-visit-array-el (val
				 i
				 expansion
				 path
				 visitor)
  (funcall (plist-get visitor :array-el) val i path)
  (when-let (sub-expansion (ensime-db-sub-expansion
			    expansion i))
    (let ((sub-val (ensime-rpc-debug-value
		    (ensime-db-make-array-el-location
		     (plist-get val :object-id)
		     i))))
      (ensime-db-visit-value sub-val sub-expansion
			     (append path (list i))
			     visitor))))



(defun ensime-db-visit-value (val
			      expansion
			      path
			      visitor)

  (case (plist-get val :val-type)

    (ref (when-let (looked-up (ensime-rpc-debug-value
			       (ensime-db-make-obj-ref-location
				(plist-get val :object-id))))
	   (ensime-db-visit-value looked-up
				  expansion
				  path
				  visitor)))

    (prim (funcall (plist-get visitor :primitive) val path))

    (obj (progn
	   (funcall (plist-get visitor :object) val path)
	   (dolist (f (plist-get val :fields))
	     (ensime-db-visit-obj-field val f expansion path visitor))))

    (arr (progn
	   (funcall (plist-get visitor :array) val path)
	   (let ((i 0)
		 (limit (min (plist-get val :length) 10)))
	     (while (< i limit)
	       (ensime-db-visit-array-el val i expansion path visitor)
	       (incf i)))
	   ))

    (str (progn
	   (funcall (plist-get visitor :string) val path)
	   (dolist (f (plist-get val :fields))
	     (ensime-db-visit-obj-field val f expansion path visitor))))

    (null (funcall (plist-get visitor :null) val path))

    (otherwise (debug "What is this? %s" val))
    ))


(defun ensime-db-visit-backtrace (val
				  visitor)
  (funcall (plist-get visitor :header)
	   (plist-get val :thread-id)
	   (plist-get val :thread-name))
  (dolist (frame (plist-get val :frames))
    (funcall (plist-get visitor :frame)
	     (plist-get frame :class-name)
	     (plist-get frame :method-name)
	     (plist-get (plist-get frame :pc-location) :file)
	     (plist-get (plist-get frame :pc-location) :line)
	     (plist-get frame :this-object-id))
    (dolist (var (plist-get frame :locals))
      (funcall (plist-get visitor :local-var)
	       (plist-get val :thread-id)
	       (plist-get frame :index)
	       (plist-get var :index)
	       (plist-get var :name)
	       (plist-get var :summary)
	       (plist-get var :type-name)
	       )
      )
    )
  )



;; User Commands

(defun ensime-db-value-for-name-at-point (p)
  "Get the value of the symbol at point."
  (when ensime-db-active-thread-id
    (let* ((sym (ensime-sym-at-point p))
	   (name (or (plist-get sym :name)
		     "this")))
      (ensime-db-with-active-thread (tid)
				    (ensime-rpc-debug-value-for-name
				     tid name)
				    ))))

(defun ensime-db-inspect-value-at-point (p)
  "Get the value of the symbol at point."
  (interactive (list (point)))
  (let ((val (ensime-db-value-for-name-at-point (point))))
    (if val (ensime-ui-show-nav-buffer ensime-db-value-buffer val t)
      (message "Nothing to inspect."))))

(defun ensime-db-backtrace (&optional no-select)
  "Show the backtrace for the current suspended thread."
  (interactive)
  (ensime-rpc-async-debug-backtrace
   ensime-db-active-thread-id
   0 -1
   `(lambda (val)
      (if val (ensime-ui-show-nav-buffer ensime-db-backtrace-buffer
					 val (not ,no-select))
	(message "Backtrace unavailable.")))))

(defun ensime-db-next ()
  "Cause debugger to go to next line, without stepping into
 method invocations."
  (interactive)
  (ensime-db-with-active-thread
   (tid) (ensime-rpc-debug-next tid)))

(defun ensime-db-step ()
  "Cause debugger to go to next line, stepping into
 method invocations."
  (interactive)
  (ensime-db-with-active-thread
   (tid) (ensime-rpc-debug-step tid)))

(defun ensime-db-step-out ()
  "Cause debugger to go to next line, stepping out of
 method invocations."
  (interactive)
  (ensime-db-with-active-thread
   (tid) (ensime-rpc-debug-step-out tid)))

(defun ensime-db-continue ()
  "Continue stopped debugger."
  (interactive)
  (ensime-db-with-active-thread
   (tid) (ensime-rpc-debug-continue tid)))

(defun ensime-db-run ()
  "Start debugging the current program."
  (interactive)
  (if (ensime-rpc-debug-active-vm)
      (ensime-rpc-debug-run)
    (ensime-db-start)))

(defun ensime-db-set-break (f line)
  "Set a breakpoint in the current source file at point."
  (interactive (list buffer-file-name (line-number-at-pos (point))))
  (ensime-rpc-debug-set-break f line)
  (ensime-db-refresh-breakpoints))

(defun ensime-db-clear-break (f line)
  "Clear breakpoint."
  (interactive (list buffer-file-name (line-number-at-pos (point))))
  (ensime-rpc-debug-clear-break f line)
  (ensime-db-refresh-breakpoints))

(defun ensime-db-clear-all-breaks ()
  "Clear all breakpoints."
  (interactive)
  (ensime-rpc-debug-clear-all-breaks)
  (ensime-db-refresh-breakpoints))

(defun ensime-db-quit ()
  "Stop debugging the current program. Kills the debug buffer."
  (interactive)
  (ensime-db-clear-marker-overlays)
  (ensime-rpc-debug-stop))

(defun ensime-db-get-cmd-line ()
  "Get the command needed to launch a debugger, including all
the current project's dependencies. Returns list of form (cmd [arg]*)"
  (let* ((debug-class
	  (ensime-strip-dollar-signs
	   (ensime-completing-read-path
	    "Qualified name of class to debug: "
	    ensime-db-default-main-class)))
	 (debug-args (read-string
		      "Commandline arguments: "
		      ensime-db-default-main-args)))
    (setq ensime-db-default-main-class debug-class)
    (setq ensime-db-default-main-args debug-args)
    (concat debug-class " " debug-args)))

(defun ensime-db-get-hostname ()
  "Get the target hostname"
  (let* ((debug-hostname (read-string
			  "Hostname: "
			  ensime-db-default-hostname)))
    (setq ensime-db-default-hostname debug-hostname)
    debug-hostname))

(defun ensime-db-get-port ()
  "Get the target port"
  (let* ((debug-port (read-string
		      "Port: "
		      ensime-db-default-port)))
    (setq ensime-db-default-port debug-port)
    debug-port))

(defun ensime-db-connection-closed (conn)
  (ensime-db-clear-breakpoint-overlays)
  (ensime-db-clear-marker-overlays))


(defun ensime-db-start ()
  "Start a debug VM"
  (interactive)

  (ensime-with-conn-interactive
   conn
   (let ((root-path (or (ensime-configured-project-root) "."))
	 (cmd-line (ensime-db-get-cmd-line)))

     (let ((ret (ensime-rpc-debug-start cmd-line)))
       (if (string= (plist-get ret :status) "success")
	   (progn
	     (message "Starting debug VM...")
	     (add-hook 'ensime-db-thread-suspended-hook
		       'ensime-db-update-backtraces)

	     (add-hook 'ensime-net-process-close-hooks
		       'ensime-db-connection-closed))

	 (message (format "An error occured during starting debug VM: %s"
			  (plist-get ret :details))))
       ))))


(defun ensime-db-attach ()
  "Attach to a debug VM"
  (interactive)

  (ensime-with-conn-interactive
   conn
   (let ((hostname (ensime-db-get-hostname))
	 (port (ensime-db-get-port)))

     (let ((ret (ensime-rpc-debug-attach hostname port)))
       (if (string= (plist-get ret :status) "success")
	   (progn
	     (message "Attaching to target VM...")
	     (add-hook 'ensime-db-thread-suspended-hook
		       'ensime-db-update-backtraces)

	     (add-hook 'ensime-net-process-close-hooks
		       'ensime-db-connection-closed))

	 (message "An error occured during attaching to target VM: %s"
		  (plist-get ret :details)))))
   ))

(provide 'ensime-debug)
