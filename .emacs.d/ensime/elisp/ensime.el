;;; ensime.el --- ENhanced Scala Interaction Mode for Emacs
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This file includes code from slime.el of the SLIME project
;;     (also licensend under the GNU General Public License.) The
;;     following copyrights therefore apply:
;;
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
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
  (when (<= emacs-major-version 21)
    (error "Ensime requires an Emacs version of 21, or above")))

(eval-and-compile
  (require 'cl))

(require 'thingatpt)
(require 'comint)
(require 'timer)
(require 'tooltip)
(require 'pp)
(require 'hideshow)
(require 'font-lock)
(require 'auto-complete)
(require 'easymenu)
(require 'ensime-config)
(require 'ensime-auto-complete)
(require 'ensime-sbt)
(require 'ensime-inf)
(require 'ensime-debug)
(require 'ensime-builder)
(require 'ensime-refactor)
(require 'ensime-undo)
(require 'ensime-search)
(require 'ensime-scalex)
(require 'ensime-doc)
(require 'ensime-semantic-highlight)
(require 'ensime-ui)
(eval-when (compile)
  (require 'apropos)
  (require 'compile))

(defgroup ensime nil
  "Interaction with the ENhanced Scala Environment."
  :group 'tools)

(defgroup ensime-ui nil
  "Interaction with the ENhanced Scala Environment UI."
  :group 'ensime)

(defcustom ensime-truncate-lines t
  "Set `truncate-lines' in popup buffers.
  This applies to buffers that present lines as rows of data, such as
  debugger backtraces and apropos listings."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-kill-without-query-p t
  "If non-nil, kill ENSIME processes without query when quitting Emacs."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-tooltip-hints t
  "If non-nil, mouse tooltips are activated."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-tooltip-type-hints t
  "If non-nil, type-inspecting tooltips are activated."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-graphical-tooltips nil
  "If non-nil, show graphical bubbles for tooltips."
  :type 'boolean
  :group 'ensime-ui)

(defgroup ensime-server nil
  "Server configuration."
  :prefix "ensime-"
  :group 'ensime)

(defcustom ensime-connected-hook nil
  "List of functions to call when ENSIME connects to Lisp."
  :type 'hook
  :group 'ensime-server)

(defcustom ensime-default-server-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'ensime-server)

(defcustom ensime-default-port 9999
  "Port to use as the default for `ensime-connect'."
  :type 'integer
  :group 'ensime-server)

(defcustom ensime-default-server-cmd "bin/server"
  "Command to launch server process."
  :type 'string
  :group 'ensime-server)

(defcustom ensime-default-server-root
  (file-name-directory
   (file-name-directory
    (directory-file-name
     (file-name-directory
      (locate-library "ensime")))))
  "Location of ENSIME server library."
  :type 'string
  :group 'ensime-server)

(defcustom ensime-mode-key-prefix [?\C-c]
  "The prefix key for ensime-mode commands."
  :group 'ensime-mode
  :type 'sexp)

(defvar ensime-protocol-version "0.7")

(defvar ensime-prefer-noninteractive nil
  "State variable used for regression testing.")

(defvar ensime-server-buffer-name "*inferior-ensime-server*")

(defvar ensime-popup-in-other-frame nil)

(defvar ensime-ch-fix 1
  "Single character offset to convert between emacs and
 0-based character indexing.")

;;;;; ensime-mode

(defgroup ensime-mode nil
  "Settings for ensime-mode scala source buffers."
  :prefix "ensime-"
  :group 'ensime)

(defun ensime-source-file-p (&optional filename)
  "Return t if the given filename (or the currently visited file if no
argument is supplied) is a .scala or .java file."
  (let ((file (or filename buffer-file-name)))
    (when file
      (integerp (string-match "\\(?:\\.scala$\\|\\.java$\\)" file)))))

(defun ensime-java-file-p (f)
  (string-match "\\.java$" f))

(defun ensime-scala-file-p (f)
  (string-match "\\.scala$" f))

(defun ensime-visiting-java-file-p ()
  (ensime-java-file-p buffer-file-name))

(defun ensime-visiting-scala-file-p ()
  (ensime-scala-file-p buffer-file-name))

(defun ensime-scala-mode-hook ()
  "Conveniance hook function that just starts ensime-mode."
  (ensime-mode 1))

(defvar ensime-source-buffer-saved-hook nil
  "Hook called whenever an ensime source buffer is saved.")

(defvar ensime-source-buffer-loaded-hook nil
  "Hook called whenever an ensime source buffer is loaded.")

(defun ensime-run-after-save-hooks ()
  "Things to run whenever a source buffer is saved."
  (condition-case err-info
      (run-hooks 'ensime-source-buffer-saved-hook)
    (error
     (message
      "Error running ensime-source-buffer-saved-hook: %s"
      err-info))))

(defun ensime-run-find-file-hooks ()
  "Things to run whenever a source buffer is opened."
  (condition-case err-info
      (run-hooks 'ensime-source-buffer-loaded-hook)
    (error
     (message
      "Error running ensime-source-buffer-loaded-hook: %s"
      err-info))))

(defun ensime-save-buffer-no-hooks ()
  "Just save the buffer per usual, don't type-check!"
  (let ((after-save-hook nil)
        (before-save-hook nil))
    (save-buffer)))

(defun ensime-delete-buffer-and-file ()
  "Kill the current buffer and delete the corresponding file!"
  (interactive)
  (ensime-assert-buffer-saved-interactive
   (let ((f buffer-file-name))
     (ensime-rpc-remove-file f)
     (delete-file f)
     (kill-buffer nil)
     )))

(defun ensime-write-buffer (&optional filename clear-modtime set-unmodified)
  "Write the contents of buffer to its buffer-file-name.
Do not show 'Writing..' message."
  (let ((file (or filename buffer-file-name))
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (when clear-modtime
      (clear-visited-file-modtime))
    (write-region (point-min) (point-max) file nil 'nomessage)
    (when set-unmodified
      (set-buffer-modified-p nil))
    ))


(defun ensime-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))


(defvar ensime-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))

      (define-key prefix-map (kbd "C-v i") 'ensime-inspect-type-at-point)
      (define-key prefix-map (kbd "C-v 5 i")
	'ensime-inspect-type-at-point-other-frame)
      (define-key prefix-map (kbd "C-v p") 'ensime-inspect-package-at-point)
      (define-key prefix-map (kbd "C-v o") 'ensime-inspect-project-package)
      (define-key prefix-map (kbd "C-v c") 'ensime-typecheck-current-file)
      (define-key prefix-map (kbd "C-v a") 'ensime-typecheck-all)
      (define-key prefix-map (kbd "C-v e") 'ensime-show-all-errors-and-warnings)
      (define-key prefix-map (kbd "C-v r") 'ensime-show-uses-of-symbol-at-point)
      (define-key prefix-map (kbd "C-v s") 'ensime-sbt-switch)
      (define-key prefix-map (kbd "C-v z") 'ensime-inf-switch)
      (define-key prefix-map (kbd "C-v f") 'ensime-format-source)
      (define-key prefix-map (kbd "C-v u") 'ensime-undo-peek)
      (define-key prefix-map (kbd "C-v v") 'ensime-search)
      (define-key prefix-map (kbd "C-v x") 'ensime-scalex)
      (define-key prefix-map (kbd "C-v t") 'ensime-show-doc-for-symbol-at-point)
      (define-key prefix-map (kbd "C-v .") 'ensime-expand-selection-command)

      (define-key prefix-map (kbd "C-d d") 'ensime-db-start)
      (define-key prefix-map (kbd "C-d b") 'ensime-db-set-break)
      (define-key prefix-map (kbd "C-d u") 'ensime-db-clear-break)
      (define-key prefix-map (kbd "C-d s") 'ensime-db-step)
      (define-key prefix-map (kbd "C-d o") 'ensime-db-step-out)
      (define-key prefix-map (kbd "C-d n") 'ensime-db-next)
      (define-key prefix-map (kbd "C-d r") 'ensime-db-run)
      (define-key prefix-map (kbd "C-d c") 'ensime-db-continue)
      (define-key prefix-map (kbd "C-d q") 'ensime-db-quit)
      (define-key prefix-map (kbd "C-d i") 'ensime-db-inspect-value-at-point)
      (define-key prefix-map (kbd "C-d t") 'ensime-db-backtrace)
      (define-key prefix-map (kbd "C-d a") 'ensime-db-clear-all-breaks)

      (define-key prefix-map (kbd "C-b s") 'ensime-sbt-switch)
      (define-key prefix-map (kbd "C-b c") 'ensime-sbt-do-compile)
      (define-key prefix-map (kbd "C-b n") 'ensime-sbt-do-clean)
      (define-key prefix-map (kbd "C-b p") 'ensime-sbt-do-package)

      (define-key prefix-map (kbd "C-d u") 'ensime-db-clear-break)
      (define-key prefix-map (kbd "C-d s") 'ensime-db-step)
      (define-key prefix-map (kbd "C-d n") 'ensime-db-next)
      (define-key prefix-map (kbd "C-d r") 'ensime-db-run)
      (define-key prefix-map (kbd "C-d c") 'ensime-db-continue)
      (define-key prefix-map (kbd "C-d q") 'ensime-db-quit)
      (define-key prefix-map (kbd "C-d l") 'ensime-db-list-locals)

      (define-key prefix-map (kbd "C-r r") 'ensime-refactor-rename)
      (define-key prefix-map (kbd "C-r o") 'ensime-refactor-organize-imports)
      (define-key prefix-map (kbd "C-r l") 'ensime-refactor-extract-local)
      (define-key prefix-map (kbd "C-r m") 'ensime-refactor-extract-method)
      (define-key prefix-map (kbd "C-r i") 'ensime-refactor-inline-local)
      (define-key prefix-map (kbd "C-r t") 'ensime-import-type-at-point)

      (define-key prefix-map (kbd "C-b b") 'ensime-builder-build)
      (define-key prefix-map (kbd "C-b r") 'ensime-builder-rebuild)

      (define-key map ensime-mode-key-prefix prefix-map)

      ;; Prefix-less shortcuts bindings...
      (define-key map (kbd "M-.") 'ensime-edit-definition)
      (define-key map (kbd "M-,") 'ensime-pop-find-definition-stack)

      (define-key map (kbd "M-n") 'ensime-forward-note)
      (define-key map (kbd "M-p") 'ensime-backward-note)

      (define-key map [C-down-mouse-1] 'ignore)
      (define-key map [C-up-mouse-1] 'ignore)
      (define-key map [C-down-mouse-3] 'ignore)
      (define-key map [C-up-mouse-3] 'ignore)
      (define-key map [C-mouse-1] 'ensime-control-mouse-1-single-click)
      (define-key map [C-mouse-3] 'ensime-control-mouse-3-single-click)
      )

    map)
  "Keymap for ENSIME mode."
  )

(easy-menu-define ensime-mode-menu ensime-mode-map
  "Menu for ENSIME mode"
  '("ENSIME"
    ("Build"
     ["Build project" ensime-builder-build]
     ["Rebuild project" ensime-builder-rebuild])

    ("Test")

    ("Source"
     ["Format source" ensime-format-source]
     ["Find all references" ensime-show-uses-of-symbol-at-point]
     ["Inspect type" ensime-inspect-type-at-point]
     ["Inspect type in another frame" ensime-inspect-type-at-point-other-frame]
     ["Inspect enclosing package" ensime-inspect-package-at-point]
     ["Inspect project package" ensime-inspect-project-package]
     ["Typecheck file" ensime-typecheck-current-file]
     ["Typecheck project" ensime-typecheck-all]
     ["Show all errors and warnings" ensime-show-all-errors-and-warnings]
     ["Undo source change" ensime-undo-peek])

    ("Refactor"
     ["Organize imports" ensime-refactor-organize-imports]
     ["Import type at point" ensime-import-type-at-point]
     ["Rename" ensime-refactor-rename]
     ["Extract local val" ensime-refactor-extract-local]
     ["Extract method" ensime-refactor-extract-method]
     ["Inline local val" ensime-refactor-inline-local])

    ("Navigation"
     ["Lookup definition" ensime-edit-definition]
     ["Lookup definition in other window" ensime-edit-definition-other-window]
     ["Lookup definition in other frame" ensime-edit-definition-other-frame]
     ["Pop definition stack" ensime-pop-find-definition-stack]
     ["Backward compilation note" ensime-backward-note]
     ["Forward compilation note" ensime-forward-note]
     ["Expand selection" ensime-expand-selection-command]
     ["Search" ensime-search]
     ["Scalex-Search" ensime-scalex])

    ("Documentation"
     ["Browse documentation of symbol" ensime-show-doc-for-symbol-at-point])

    ("SBT"
     ["Start or switch to" ensime-sbt-switch]
     ["Compile" ensime-sbt-do-compile]
     ["Clean" ensime-sbt-do-clean]
     ["Package" ensime-sbt-do-package])

    ("Debugger"
     ["Start" ensime-db-start]
     ["Set break point" ensime-db-set-break]
     ["Clear breakpoint" ensime-db-clear-break]
     ["Clear all breakpoints" ensime-db-clear-all-breaks]
     ["Step" ensime-db-step]
     ["Next" ensime-db-next]
     ["Run" ensime-db-run]
     ["Continue" ensime-db-continue]
     ["Quit" ensime-db-quit]
     ["Show Backtrace" ensime-db-backtrace]
     ["Inspect value at point" ensime-db-inspect-value-at-point]
     )

    "---"
    ["Go to SBT console" ensime-sbt-switch]
    ["Go to Scala REPL" ensime-inf-switch]
    ["Shutdown ENSIME server" ensime-shutdown]
    ))

(define-minor-mode ensime-mode
  "ENSIME: The ENhanced Scala Interaction Mode for Emacs (minor-mode).
\\{ensime-mode-map}"
  nil
  nil
  ensime-mode-map

  (if ensime-mode
      (progn
        (ensime-ac-enable)
        (easy-menu-add ensime-mode-menu ensime-mode-map)

        (add-hook 'after-save-hook 'ensime-run-after-save-hooks nil t)

	(add-hook 'find-file-hook 'ensime-run-find-file-hooks nil t)

        (add-hook 'ensime-source-buffer-saved-hook
                  'ensime-typecheck-current-file)

        (add-hook 'ensime-source-buffer-saved-hook
                  'ensime-builder-track-changed-files t)

        (add-hook 'ensime-source-buffer-saved-hook
                  'ensime-sem-high-refresh-hook t)

        (add-hook 'ensime-source-buffer-loaded-hook
                  'ensime-sem-high-refresh-hook t)

        (add-hook 'ensime-source-buffer-loaded-hook
                  'ensime-typecheck-current-file)


        (when ensime-tooltip-hints
          (add-hook 'tooltip-functions 'ensime-tooltip-handler)
          (make-local-variable 'track-mouse)
          (setq track-mouse t)
          (make-local-variable 'tooltip-delay)
          (setq tooltip-delay 1.0)
          (define-key ensime-mode-map [mouse-movement] 'ensime-mouse-motion))

        (ensime-refresh-all-note-overlays))
    (progn
      (ensime-ac-disable)
      (remove-hook 'after-save-hook 'ensime-run-after-save-hooks t)

      (remove-hook 'find-file-hook 'ensime-run-find-file-hooks t)

      (remove-hook 'ensime-source-buffer-saved-hook
                   'ensime-typecheck-current-file)

      (remove-hook 'ensime-source-buffer-saved-hook
                   'ensime-builder-track-changed-files)

      (remove-hook 'ensime-source-buffer-saved-hook
                   'ensime-sem-high-refresh-hook)

      (remove-hook 'ensime-source-buffer-loaded-hook
                   'ensime-sem-high-refresh-hook)

      (remove-hook 'ensime-source-buffer-loaded-hook
                   'ensime-typecheck-current-file)

      (remove-hook 'tooltip-functions 'ensime-tooltip-handler)
      (make-local-variable 'track-mouse)
      (setq track-mouse nil))))

;;;;;; Mouse handlers

(defun ensime-control-mouse-1-single-click (event)
  "Command handler for control+clicks of mouse button 1.
   If control is held, jump to definition of symbol under
   point."
  (interactive "e")
  (mouse-set-point event)
  (ensime-edit-definition))

(defun ensime-control-mouse-3-single-click (event)
  "Command handler for double clicks of mouse button 1.
   If the user clicks on a package declaration or import,
   inspect that package. Otherwise, try to inspect the type
   of the thing at point."
  (interactive "e")
  (ensime-inspect-type-at-point))


(defun ensime-mouse-motion (event)
  "Command handler for mouse movement events in `ensime-mode-map'."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (setq tooltip-last-mouse-motion-event (copy-sequence event))
    (tooltip-start-delayed-tip)))


;;;;;; Tooltips


(defun ensime-tooltip-show-message (msg)
  "Display tooltip, respecting ensime tooltip options."
  (if ensime-graphical-tooltips
      (tooltip-show msg tooltip-use-echo-area)
    (message msg)))


(defun ensime-tooltip-handler (event)
  "Hook function to display a help tooltip. If an error
   or warning overlay exists at point, show the description
   of that error or warning. Otherwise try to inspect the
   type of the expression under the cursor."
  (when (and (eventp event)
             ensime-mode
             (ensime-current-connection)
             (posn-point (event-end event)))

    (let* ((point (posn-point (event-end event)))
           (ident (tooltip-identifier-from-point point))
           (note-overlays (ensime-overlays-at point))
	   (val-at-pt (ensime-db-value-for-name-at-point point)))


      (cond

       ;; If debugger is active and we can get the value of the symbol
       ;; at the point, show it in the tooltip.
       (val-at-pt (ensime-tooltip-show-message (ensime-db-value-short-name
						val-at-pt)) t)

       ;; If error or warning overlays exist,
       ;; show that message..
       (note-overlays (progn
                        (ensime-tooltip-show-message
                         (overlay-get (car note-overlays) 'help-echo))
                        t))


       ;; Otherwise show a type hint..
       ((and ident ensime-tooltip-type-hints)
        (progn
          (ensime-eval-async
           `(swank:type-at-point ,buffer-file-name ,point)
           #'(lambda (type)
               (when type
                 (let ((msg (ensime-type-full-name-with-args type)))
                   (ensime-tooltip-show-message msg)
                   ))))
          t
          )))
      )))




;;;;;; Modeline

;; Setup the custom ensime modeline handler
(add-to-list 'minor-mode-alist
             '(ensime-mode (:eval (ensime-modeline-string))))

(defun ensime-modeline-string ()
  "Return the string to display in the modeline.
  \"ENSIME\" only appears if we aren't connected.  If connected, include
  connection-name, and possibly some state
  information."
  (when ensime-mode
    (condition-case err
	(let ((conn (ensime-current-connection)))
	  (cond ((and ensime-mode (not conn))
		 (cond
		  ((ensime-probable-owning-connection-for-source-file
		    buffer-file-name)
		   " [ENSIME: Connected...]")
		  (t " [ENSIME: No Connection]")))

		((and ensime-mode (ensime-connected-p conn))
		 (concat " "
			 "[ENSIME: "
			 (or (plist-get (ensime-config conn) :project-name)
			     "Connected...")
			 (let ((status (ensime-modeline-state-string conn))
			       (unready (not (ensime-analyzer-ready conn))))
			   (cond (status (concat " (" status ")"))
				 (unready " (analyzing...)")
				 (t "")))
			 (concat (format " : %s/%s"
					 (ensime-num-errors conn)
					 (ensime-num-warnings conn)))
			 "]"))
		(ensime-mode " [ENSIME: Dead Connection]")
		))
      (error (progn
	       " [ENSIME: wtf]"
	       )))))




(defun ensime-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
	 (format "%s" (process-status conn)))
	((let ((pending (length (ensime-rex-continuations conn))))
	   (cond ((zerop pending) nil)
		 (t (format "%s" pending)))))))

;; Startup

(defun ensime ()
  "Read config file for settings. Then start an inferior
   ENSIME server and connect to its Swank server."
  (interactive)
  (when (and (ensime-source-file-p) (not ensime-mode))
    (ensime-mode 1))
  (let* ((config (ensime-config-find-and-load)))

    (when (not (null config))
      (let* ((cmd (or (plist-get config :server-cmd)
		      ensime-default-server-cmd))
	     (env (plist-get config :server-env))
	     (dir (or (plist-get config :server-root)
		      ensime-default-server-root))
	     (buffer ensime-server-buffer-name)
	     (args (list (ensime-swank-port-file))))

	(ensime-delete-swank-port-file 'quiet)
	(let ((server-proc (ensime-maybe-start-server cmd args env dir buffer)))
	  (ensime-inferior-connect config server-proc)))
      )))


(defun ensime-reload ()
  "Re-initialize the project with the current state of the config file.
Analyzer will be restarted. All source will be recompiled."
  (interactive)
  (ensime-assert-connected
   (let* ((conn (ensime-current-connection))
	  (current-conf (ensime-config conn))
	  (config (ensime-config-find-and-load
		   (plist-get current-conf :root-dir))))

     (when (not (null config))
       (ensime-set-config conn config)
       (ensime-init-project conn config)))))

(defun ensime-maybe-start-server (program program-args env directory buffer)
  "Return a new or existing inferior server process."
  (cond ((not (comint-check-proc buffer))
	 (ensime-start-server program program-args env directory buffer))
	((ensime-reinitialize-inferior-server-p program program-args env buffer)
	 (when-let (conn (find (get-buffer-process buffer) ensime-net-processes
			       :key #'ensime-server-process))
	   (ensime-net-close conn))
	 (get-buffer-process buffer))
	(t (ensime-start-server program program-args env directory
				(generate-new-buffer-name buffer)))))


(defun ensime-reinitialize-inferior-server-p (program program-args env buffer)
  (let ((args (ensime-inferior-server-args (get-buffer-process buffer))))
    (and (equal (plist-get args :program) program)
	 (equal (plist-get args :program-args) program-args)
	 (equal (plist-get args :env) env)
	 (not (y-or-n-p "Create an additional *inferior-server*? ")))))


(defvar ensime-server-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defun ensime-start-server (program program-args env directory buffer)
  "Does the same as `inferior-server' but less ugly.
   Return the created process."
  (with-current-buffer (get-buffer-create buffer)
    (when directory
      (cd (expand-file-name directory)))
    (comint-mode)
    (let ((process-environment (append env process-environment))
	  (process-connection-type nil))
      (set (make-local-variable 'comint-process-echoes) nil)
      (set (make-local-variable 'comint-use-prompt-regexp) nil)
      (comint-exec (current-buffer) ensime-server-buffer-name
		   program nil program-args))
    (let ((proc (get-buffer-process (current-buffer))))
      (ensime-set-query-on-exit-flag proc)
      (run-hooks 'ensime-server-process-start-hook)
      proc)))


(defun ensime-shutdown()
  "Request that the current ENSIME server kill itself."
  (interactive)
  (ensime-quit-connection (ensime-current-connection)))


(defvar ensime-inferior-server-args nil
  "A buffer local variable in the inferior proccess. See `ensime-start'.")

(defun ensime-inferior-server-args (process)
  "Return the initial process arguments.
   See `ensime-start'."
  (with-current-buffer (process-buffer process)
    ensime-inferior-server-args))

(defun ensime-inferior-connect (config server-proc)
  "Start a Swank server in the inferior Server and connect."
  (ensime-read-port-and-connect config server-proc nil))


(defun ensime-file-in-directory-p (file-name dir-name)
  "Determine if file named by file-name is contained in the
   directory named by dir-name."
  (let* ((dir (file-name-as-directory (expand-file-name dir-name)))
	 (file (expand-file-name file-name))
	 (d file))
    (catch 'return
      (while d
	(let ((d-original d))
	  (setq d (file-name-directory
		   (directory-file-name d)))
	  (when (equal dir d)
	    (throw 'return t))
	  (when (equal d d-original)
	    (throw 'return nil))
	  )))))

(defun ensime-escape-control-chars (s)
  "Return a copy of s with control characters
 escaped."
  ;; TODO:
  ;; Can't get this to work with single regexp - hence shitty
  ;; iterative version. Maybe Emacs bug.
  (let ((s s))
    (setq s (replace-regexp-in-string "\n" "\\n" s nil t))
    (setq s (replace-regexp-in-string "\t" "\\t" s nil t))
    (setq s (replace-regexp-in-string "\r" "\\r" s nil t))
    (setq s (replace-regexp-in-string "\b" "\\r" s nil t))
    s))


(defun ensime-configured-project-root ()
  "Return root path of the current project as defined in the
config file and stored in the current connection. Nil is returned
if there is no active connection, or if the project root was not
defined."
  (when (ensime-connected-p)
    (let ((config (ensime-config (ensime-current-connection))))
      (plist-get config :root-dir))))

(defmacro ensime-assert-connected (&rest body)
  "Surround body forms with a check to see if we're connected.
If not, message the user."
  `(if (ensime-connected-p)
       (progn ,@body)
     (message "This command requires a connection to an ENSIME server.")))

(defmacro ensime-with-conn-interactive (conn-sym &rest body)
  "Surround body forms with a check to see if we're connected.
If not, message the user."
  `(let* ((,conn-sym (or (ensime-current-connection)
			 (ensime-prompt-for-connection))))
     (if conn
	 (progn ,@body)
       (message
	"This command requires a connection to an ENSIME server."))))

(defun ensime-swank-port-file ()
  "Filename where the SWANK server writes its TCP port number."
  (ensime-temp-file-name (format "ensime_port.%S" (emacs-pid))))

(defun ensime-read-swank-port ()
  "Read the Swank server port number from the `ensime-swank-port-file'."
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (ensime-swank-port-file))
      (goto-char (point-min))
      (let ((port (read (current-buffer))))
	(assert (integerp port))
	port))))

(defun ensime-temp-file-name (name)
  "Return the path of a temp file with filename 'name'."
  (expand-file-name
   (concat (file-name-as-directory (ensime-temp-directory)) name)))

(defun ensime-temp-directory ()
  "Return the directory name of the system's temporary file dump."
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	(t "/tmp/")))

(defmacro* ensime-with-buffer-written-to-tmp ((file-sym) &rest body)
  "Write latest buffer state to a temp file, bind the temp filename
 to file-sym, and eval body. The idea is to not disturb the original
 file's state."
  `(let ((,file-sym (ensime-temp-file-name
		     (concat ".tmp_" (file-name-nondirectory
				      buffer-file-name)))))
     (ensime-write-buffer ,file-sym)
     ,@body))

(defmacro ensime-with-temp-file (name)
  "Return the path of a temp file with filename 'name'."
  (concat (file-name-as-directory (ensime-temp-directory))
	  name))

(defun ensime-delete-swank-port-file (&optional quiet)
  (condition-case data
      (delete-file (ensime-swank-port-file))
    (error
     (ecase quiet
       ((nil) (signal (car data) (cdr data)))
       (quiet)
       (message (message "Unable to delete swank port file %S"
			 (ensime-swank-port-file)))))))

(defun ensime-read-port-and-connect (config server-proc retries)
  (ensime-cancel-connect-retry-timer)
  (ensime-attempt-connection config server-proc retries 1))


(defun ensime-attempt-connection (config server-proc retries attempt)
  ;; A small one-state machine to attempt a connection with
  ;; timer-based retries.
  (let ((host (or (plist-get config :server-host) ensime-default-server-host))
	(port-file (ensime-swank-port-file)))
    (unless (active-minibuffer-window)
      (message "Polling %S.. (Abort with `M-x ensime-abort-connection'.)"
	       port-file))
    (cond ((and (file-exists-p port-file)
		(> (nth 7 (file-attributes port-file)) 0)) ; file size
	   (ensime-cancel-connect-retry-timer)
	   (let ((port (ensime-read-swank-port))
		 (args (ensime-inferior-server-args server-proc)))
	     (message "Read port %S from %S." port port-file)
	     (ensime-delete-swank-port-file 'message)
	     (let ((c (ensime-connect host port)))

	       ;; It may take a few secs to get the
	       ;; source roots back from the server,
	       ;; so we won't know immediately if currently
	       ;; visited source is part of the new
	       ;; project.
	       ;;
	       ;; Make an educated guess for the sake
	       ;; of UI snappiness (fast mode-line
	       ;; update).
	       (when (and (ensime-source-file-p)
			  (plist-get config :root-dir)
			  (ensime-file-in-directory-p
			   buffer-file-name
			   (plist-get config :root-dir))
			  (not (ensime-connected-p)))
		 (setq ensime-buffer-connection c))

	       (ensime-set-config c config)

	       (let ((ensime-dispatching-connection c))
		 (ensime-eval-async
		  '(swank:connection-info)
		  (ensime-curry #'ensime-handle-connection-info c)))

	       (ensime-set-server-process c server-proc)
	       ;; As a conveniance, we associate the client connection with
	       ;; the server buffer.
	       ;; This assumes that there's only one client connection
	       ;; per server. So far this is a safe assumption.
	       (when-let (server-buf (process-buffer server-proc))
		 (with-current-buffer server-buf
		   (setq ensime-buffer-connection c)))

	       )))
	  ((and retries (zerop retries))
	   (ensime-cancel-connect-retry-timer)
	   (message "Gave up connecting to Swank after %d attempts." attempt))
	  ((eq (process-status server-proc) 'exit)
	   (ensime-cancel-connect-retry-timer)
	   (message "Failed to connect to Swank: server process exited."))
	  (t
	   (when (and (file-exists-p port-file)
		      (zerop (nth 7 (file-attributes port-file))))
	     (message "(Zero length port file)")
	     ;; the file may be in the filesystem but not yet written
	     (unless retries (setq retries 3)))
	   (unless ensime-connect-retry-timer
	     (setq ensime-connect-retry-timer
		   (run-with-timer
		    0.3 0.3
		    #'ensime-timer-call #'ensime-attempt-connection
		    config server-proc (and retries (1- retries))
		    (1+ attempt))))))))

(defvar ensime-connect-retry-timer nil
  "Timer object while waiting for the inferior server to start.")

(defun ensime-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.

The default condition handler for timer functions (see
`timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    (error (debug nil (list "Error in timer" fun args data)))))

(defun ensime-cancel-connect-retry-timer ()
  (when ensime-connect-retry-timer
    (cancel-timer ensime-connect-retry-timer)
    (setq ensime-connect-retry-timer nil)))

(defun ensime-abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (cond (ensime-connect-retry-timer
	 (ensime-cancel-connect-retry-timer)
	 (message "Cancelled connection attempt."))
	(t (error "Not connecting"))))


;;;; Framework'ey bits
;;;
;;; This section contains some standard ENSIME idioms: basic macros,
;;; ways of showing messages to the user, etc. All the code in this
;;; file should use these functions when applicable.
;;;
;;;;; Syntactic sugar


(defun ensime-make-code-link (start end file-path offset &optional face line)
  "Make an emacs button, from start to end in current buffer,
 linking to file-path and offset."
  (make-button start end
	       'face (or face font-lock-keyword-face)
	       'action `(lambda (x)
			  (find-file-other-window ,file-path)
			  (if (integerp ,line)
			      (ensime-goto-line ,line)
			    (goto-char (or ,offset 0)))
			  )))

(defun ensime-make-code-hyperlink (start end http-path &optional face)
  "Make an emacs button, from start to end in current buffer,
 hyperlinking to http-path."
  (make-button start end
	       'face (or face font-lock-keyword-face)
	       'action `(lambda (x)
			  (browse-url ,http-path)
			  (message "Opening documentation in browser..")
			  )))

(defun ensime-http-url-p (s)
  (and (stringp s) (string-match "http://" s)))

(defun ensime-insert-link (text file-path &optional offset face line)
  "Insert text in current buffer and make it into an emacs
 button, linking to file-path and offset. Intelligently decide
 whether to make a source link or an http link based on the file-path."
  (let ((start (point)))
    (cond
     ((and file-path (ensime-http-url-p file-path))
      (progn
	(insert text)
	(ensime-make-code-hyperlink start (point) file-path face)))

     ((and file-path (or (integerp offset) (integerp line)))
      (progn
	(insert text)
	(ensime-make-code-link start (point) file-path offset face line)))

     (t
      (progn
	(insert text))))
    ))


(defun ensime-insert-action-link (text action &optional face)
  "Insert text in current buffer and make it into an emacs
 button, linking to file-path and offset."
  (let ((start (point)))
    (insert text)
    (make-button start (point) 'face
		 (or face font-lock-variable-name-face)
		 'action action)))

(defun ensime-insert-with-face (text face)
  "Insert text in current buffer and color it
 with face"
  (let ((start (point)))
    (insert text)
    (set-text-properties start (point) `(face ,face))))

(defun ensime-flatten-list (list)
  ;;(ensime-flatten-list '((a) b c (d e (q) f g)))
  (mapcan (lambda (x)
	    (if (listp x)
		(ensime-flatten-list x)
	      (list x))) list))

(defun ensime-tokenize-cmd-line (str &optional delim)
  "Interpret a string as a sequence of command-line arguments.
 Break the string at space and tab boundaries, except for double-quoted
 arguments. Returns a list of string tokens.
 "
  ;;(ensime-tokenize-cmd-line "")
  ;;(ensime-tokenize-cmd-line "abc")
  ;;(ensime-tokenize-cmd-line "abc def")
  ;;(ensime-tokenize-cmd-line "abc   def")
  ;;(ensime-tokenize-cmd-line "abc def -sd")
  ;;(ensime-tokenize-cmd-line "abc def -sd \"apple pie\"")
  ;;(ensime-tokenize-cmd-line "abc def -sd \"ap'p'le\"")
  ;;(ensime-tokenize-cmd-line "abc def -sd 'ap\"pl\"e'")
  ;;(ensime-tokenize-cmd-line "'ap\"pl\"e'")
  ;;(ensime-tokenize-cmd-line "'ap\"pl\"e")
  ;;(ensime-tokenize-cmd-line "abc \"sd")

  (let ((ch)
	(cur "")
	(tokens '()))

    (catch 'return
      (while (> (length str) 0)
	(setq ch (substring str 0 1))
	(setq str (substring str 1))

	(cond
	 ((and delim (equal ch delim))
	  (throw 'return (list tokens str)))

	 ((or (equal ch "\"")
	      (equal ch "'"))
	  (if delim
	      (setq cur (concat cur ch))
	    (let ((tmp (ensime-tokenize-cmd-line str ch)))
	      (setq tokens (append tokens (car tmp)))
	      (setq str (cadr tmp)))))

	 ((and (null delim)
	       (integerp (string-match "[ \t]" ch)))
	  (when (> (length cur) 0)
	    (setq tokens (append tokens (list cur)))
	    (setq cur "")))

	 (t (setq cur (concat cur ch))))))

    (when (> (length cur) 0)
      (setq tokens (append tokens (list cur))))
    (list tokens str)
    ))

(defvar ensime-qualified-type-regexp
  "^\\(?:object \\)?\\(\\(?:[a-z0-9_]+\\.\\)*\\)\\(?:\\([^\\.]+\\)\\$\\)?\\([^\\.]+\\$?\\)$"
  "Match strings of form pack.pack1.pack2.Types$Type or pack.pack1.pack2.Type")
(defmacro* ensime-with-name-parts (str (path outer-type-name name) &rest body)
  "Evaluate BODY with path bound to the dot-separated path of
 this type-name, and name bound to the final type name."
  (let ((tmp (gensym)))
    `(let ((matchedp (integerp (string-match
				ensime-qualified-type-regexp
				,str))))
       (let* ((,tmp (if matchedp (match-string 1 ,str) nil))
	      (,path (if (> (length ,tmp) 0)
			 (substring ,tmp 0 (- (length ,tmp) 1)) ,tmp))
	      (,outer-type-name (if matchedp (match-string 2 ,str) nil))
	      (,name (if matchedp (match-string 3 ,str) ,str)))
	 ,@body))))

(defvar ensime-qualified-path-and-name-regexp
  "^\\(\\(?:[a-z0-9_]+\\.\\)*\\)\\([^\\.]*\\)$")
(defmacro* ensime-with-path-and-name (str (path name) &rest body)
  "Evaluate body with path bound to all sections up to the
 last, concatenated, and name bound to the last section."
  (let ((tmp (gensym)))
    `(let ((matchedp (integerp (string-match
				ensime-qualified-path-and-name-regexp
				,str))))
       (let* ((,tmp (if matchedp (match-string 1 ,str) nil))
	      (,path (if (> (length ,tmp) 0)
			 (substring ,tmp 0 (- (length ,tmp) 1)) ,tmp))
	      (,name (if matchedp (match-string 2 ,str) nil)))
	 ,@body))))

(defun ensime-last-name-component (str)
  (if (integerp (string-match "^.*?\\.\\([^\\.]+\\)$"str))
      (match-string 1 str)
    str))

(defun ensime-strip-dollar-signs (str)
  (replace-regexp-in-string "\\$" "" str))

(defmacro ensime-assert-buffer-saved-interactive (&rest body)
  "Offer to save buffer if buffer is modified. Execute body only if
buffer is saved."
  `(if (buffer-modified-p)
       (if (y-or-n-p "Buffer must be saved to continue. Save now? ")
	   (progn
	     (ensime-save-buffer-no-hooks)
	     ,@body))
     (progn
       ,@body)))

(defun ensime-assert-executable-on-path (name)
  (when (null (executable-find name))
    (error (concat name " not found on your emacs exec-path. "
		   "See Troubleshooting section of the ENSIME manual."))))

(defun ensime-kill-txt-props (str)
  "Remove all text-properties from str and return str."
  (set-text-properties 0 (length str) nil str)
  str)


(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.

\(fn (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))


(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
		     (if (eq (car clause) t)
			 `(t ,@(cdr clause))
		       (destructuring-bind ((op &rest rands) &rest body) clause
			 `(,op (destructuring-bind ,rands ,operands
				 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "Elisp destructure-case failed: %S" ,tmp))))))))


(defmacro ensime-define-keys (keymap &rest key-command)
  "Define keys in KEYMAP. Each KEY-COMMAND is a list of (KEY COMMAND)."
  `(progn . ,(mapcar (lambda (k-c) `(define-key ,keymap . ,k-c))
		     key-command)))


(defmacro* with-struct ((conc-name &rest slots) struct &body body)
  "Like with-slots but works only for structs.
\(fn (CONC-NAME &rest SLOTS) STRUCT &body BODY)"
  (flet ((reader (slot) (intern (concat (symbol-name conc-name)
					(symbol-name slot)))))
    (let ((struct-var (gensym "struct")))
      `(let ((,struct-var ,struct))
	 (symbol-macrolet
	     ,(mapcar (lambda (slot)
			(etypecase slot
			  (symbol `(,slot (,(reader slot) ,struct-var)))
			  (cons `(,(first slot) (,(reader (second slot))
						 ,struct-var)))))
		      slots)
	   . ,body)))))

(defun ensime-in-string-or-comment (pos)
  "A helper to determine if the text at point is in a string
   or comment, and therefore should not be considered as part
   of a paren-balancing calculation.

   TODO: Currently this relies on font-lock-mode. Could be
   better."
  (let ((face (plist-get (text-properties-at pos) 'face)))
    (and face
	 (or
	  (equal face 'font-lock-doc-face)
	  (equal face 'font-lock-string-face)
	  (equal face 'font-lock-comment-face)))))

(defun ensime-replace-keywords (template proplist)
  "Replace keywords in the template list with the associated
 values in the provided proplist."
  (let* ((result '()))
    (dolist (ea template)
      (cond
       ((keywordp ea)
	(setq result (cons (plist-get proplist ea) result)))
       (t
	(setq result (cons ea result)))))
    (reverse result)))

(defun ensime-line-col-to-point (file line col)
  "Convert line,column coordinates to a char offset."
  (with-temp-buffer
    (insert-file-contents file)
    (ensime-goto-line line)
    (forward-char col)
    (point)))

(defun ensime-current-line ()
  "Return the vertical position of point..."
  (1+ (count-lines 1 (point))))

(defun ensime-relativise-path (path root)
  "Given a directory named root, and a path f, return f's path
relative to root. If f is not contained by root, return the
absolute path to f."
  (let* ((full-root (directory-file-name (expand-file-name root)))
	 (full-path (expand-file-name path))
	 (index (string-match (concat "^" full-root) full-path)))
    (if (equal index 0)
	(concat "." (substring full-path (length full-root)))
      path)))


(defun ensime-revert-visited-files (files &optional typecheck)
  "files is a list of buffer-file-names to revert or lists of the form
 (visited-file-name disk-file-name) where buffer visiting visited-file-name
 will be reverted to the state of disk-file-name."
  (let ((pt (point)))
    (save-excursion
      (dolist (f files)
	(let* ((dest (cond ((stringp f) f)
			   ((listp f) (car f))))
	       (src (cond ((stringp f) f)
			  ((listp f) (cadr f))))
	       (do-visit (equal dest src)))
	  (when-let (buf (find-buffer-visiting dest))
	    (with-current-buffer buf
	      (insert-file-contents src do-visit nil nil t)
	      (when typecheck
		(ensime-typecheck-current-file)))
	    ))))
    (goto-char pt)
    ))

(defvar ensime-net-processes nil
  "List of processes (sockets) connected to Lisps.")

(defvar ensime-net-process-close-hooks '()
  "List of functions called when a ensime network connection closes.
The functions are called with the process as their argument.")


(defun ensime-net-connect (host port)
  "Establish a connection with a CL."
  (let* ((inhibit-quit nil)
	 (proc (open-network-stream "ENSIME Scala" nil host port))
	 (buffer (ensime-make-net-buffer " *ensime-connection*")))
    (push proc ensime-net-processes)
    (set-process-buffer proc buffer)
    (set-process-filter proc 'ensime-net-filter)
    (set-process-sentinel proc 'ensime-net-sentinel)
    (ensime-set-query-on-exit-flag proc)

    ;; TODO make this smart like slime?
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)

    proc))

(defun ensime-set-query-on-exit-flag (process)
  "Set PROCESS's query-on-exit-flag to `ensime-kill-without-query-p'."
  (when ensime-kill-without-query-p
    ;; avoid byte-compiler warnings
    (let ((fun (if (fboundp 'set-process-query-on-exit-flag)
		   'set-process-query-on-exit-flag
		 'process-kill-without-query)))
      (funcall fun process nil))))

(defun ensime-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (set-buffer-multibyte t)
      (buffer-disable-undo)
      (set (make-local-variable 'kill-buffer-query-functions) nil))
    buffer))

(defun ensime-net-send (sexp proc)
  "Send a SEXP to Lisp over the socket PROC. This is the lowest
 level of communication. The sexp will be read and interpreted
 by the Ensime Server."
  (let* ((msg (concat (ensime-prin1-to-string sexp) "\n"))
	 (string (concat (ensime-net-encode-length (length msg)) msg))
	 (coding-system (cdr (process-coding-system proc))))
    (ensime-log-event sexp)
    (process-send-string proc string)))

(defun ensime-net-close (process &optional debug)
  (setq ensime-net-processes (remove process ensime-net-processes))
  (set-process-sentinel process 'ignore)
  (set-process-filter process 'ignore)
  (delete-process process)
  (run-hook-with-args 'ensime-net-process-close-hooks process)
  ;; killing the buffer also closes the socket
  (kill-buffer (process-buffer process)))

(defun ensime-net-sentinel (process message)
  (message "Server connection closed unexpectedly: %s" message)
  (ensime-net-close process))

;;; Socket input is handled by `ensime-net-filter', which decodes any
;;; complete messages and hands them off to the event dispatcher.

(defun ensime-net-filter (process string)
  "Accept output from the socket and process all complete messages."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (ensime-process-available-input process))

(defun ensime-process-available-input (process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (and
	    (buffer-live-p (process-buffer process))
	    (ensime-net-have-input-p))
      (let ((event (ensime-net-read-or-lose process))
	    (ok nil))
	(ensime-log-event event)
	(unwind-protect
	    (save-current-buffer
	      (ensime-dispatch-event event process)
	      (setq ok t))
	  (unless ok
	    (ensime-run-when-idle
	     'ensime-process-available-input process)))))))

(defun ensime-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (ensime-buffer-size-in-bytes) 6)
       (>= (- (ensime-buffer-size-in-bytes) 6)
	   (ensime-net-decode-length))))

(defun ensime-buffer-size-in-bytes ()
  (- (position-bytes (point-max)) 1))

(defun ensime-run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time
	 (if (featurep 'xemacs) itimer-short-interval 0)
	 nil function args))

(defun ensime-net-read-or-lose (process)
  (condition-case error
      (ensime-net-read)
    (error
     (debug 'error error)
     (ensime-net-close process)
     (error "net-read error: %S" error))))

(defun ensime-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (ensime-net-decode-length))
	 (start (+ 6 (point)))
	 (end (+ start length)))
    (assert (plusp length))
    (goto-char (byte-to-position start))
    (prog1 (read (current-buffer))
      (delete-region (- (byte-to-position start) 6)
		     (byte-to-position end)))
    ))


(defun ensime-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun ensime-net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun ensime-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
	  print-escape-newlines
	  print-length
	  print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))



;;;;; Event logging to *ensime-events*
;;;
;;; The *ensime-events* buffer logs all protocol messages for debugging
;;; purposes. Optionally you can enable outline-mode in that buffer,
;;; which is convenient but slows things down significantly.

(defvar ensime-log-events t
  "*Log protocol events to the *ensime-events* buffer.")

(defvar ensime-outline-mode-in-events-buffer nil
  "*Non-nil means use outline-mode in *ensime-events*.")

(defvar ensime-event-buffer-name "*ensime-events*"
  "The name of the ensime event buffer.")

(defun ensime-log-event (event)
  "Record the fact that EVENT occurred."
  (when ensime-log-events
    (with-current-buffer (ensime-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
	(goto-char (/ (buffer-size) 2))
	(re-search-forward "^(" nil t)
	(delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
	(ensime-pprint-event event (current-buffer)))
      (when (and (boundp 'outline-minor-mode)
		 outline-minor-mode)
	(hide-entry))
      (goto-char (point-max)))))

(defun ensime-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun ensime-events-buffer ()
  "Return or create the event log buffer."
  (or (get-buffer ensime-event-buffer-name)
      (let ((buffer (get-buffer-create ensime-event-buffer-name)))
	(with-current-buffer buffer
	  (buffer-disable-undo)
	  (set (make-local-variable 'outline-regexp) "^(")
	  (set (make-local-variable 'comment-start) ";")
	  (set (make-local-variable 'comment-end) "")
	  (when ensime-outline-mode-in-events-buffer
	    (outline-minor-mode)))
	buffer)))



;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->ENSIME-Server networking concept.
;;;
;;; Emacs has a connection to each ENSIME server process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Servers simultaneously.
;;;
;;; A connection consists of a control socket and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Server process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `ensime-dispatching-connection' if dynamically bound, or
;;;   `ensime-buffer-connection' if this is set buffer-local,
;;;   or the value of `(ensime-owning-connection-for-source-file buffer-file-name)'
;;;   otherwise.
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `(ensime-owning-connection-for-source-file)'.
;;;
;;; When a command creates a new buffer it will set
;;; `ensime-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `ensime-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Server handles commands in
;;; ensime-mode source buffers, and ensime hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.


(defmacro ensime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `ensime-connection'."
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
	(defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
	 (ensime-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
	 `(ensime-with-connection-buffer
	   (,process)
	   (setq (\, (quote (\, real-var))) (\, store))
	   (\, store)))
       '(\, varname))))

(put 'ensime-def-connection-var 'lisp-indent-function 2)
(put 'ensime-indulge-pretty-colors 'ensime-def-connection-var t)

(ensime-def-connection-var ensime-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(ensime-def-connection-var ensime-server-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(ensime-def-connection-var ensime-pid nil
  "The process id of the Lisp process.")

(ensime-def-connection-var ensime-server-implementation-type nil
  "The implementation type of the Lisp process.")

(ensime-def-connection-var ensime-server-implementation-version nil
  "The implementation type of the Lisp process.")

(ensime-def-connection-var ensime-server-implementation-name nil
  "The short name for the Lisp implementation.")

(ensime-def-connection-var ensime-server-implementation-program nil
  "The argv[0] of the process running the Lisp implementation.")

(ensime-def-connection-var ensime-connection-name nil
  "The short name for connection.")

(ensime-def-connection-var ensime-server-process nil
  "The inferior process for the connection if any.")

(ensime-def-connection-var ensime-config nil
  "The project configuration corresponding to this connection.")

(ensime-def-connection-var ensime-communication-style nil
  "The communication style.")

(ensime-def-connection-var ensime-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")

(ensime-def-connection-var ensime-analyzer-ready nil
  "Whether the analyzer has finished its initial run.")

(ensime-def-connection-var ensime-scala-compiler-notes nil
  "Warnings, Errors, and other notes produced by the analyzer.")

(ensime-def-connection-var ensime-java-compiler-notes nil
  "Warnings, Errors, and other notes produced by the analyzer.")

(ensime-def-connection-var ensime-builder-changed-files nil
  "Files that have changed since the last rebuild.")

(ensime-def-connection-var ensime-awaiting-full-typecheck nil
  "Should we show the errors and warnings report on next full-typecheck event?")

(ensime-def-connection-var ensime-num-errors 0
  "Current number of errors in project.")

(ensime-def-connection-var ensime-num-warnings 0
  "Current number of warnings in project.")

(defvar ensime-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `ensime-buffer-connection'.")

(make-variable-buffer-local
 (defvar ensime-buffer-connection nil
   "Network connection to use in the current buffer."))


(defvar ensime-connection-counter 0
  "The number of ENSIME connections made. For generating serial numbers.")

(defun ensime-current-connection ()
  "Return the connection to use for Lisp interaction.
 Return nil if there's no connection. Note, there is some loss of
 precision here, as ensime-connections-for-source-file might return
 more than one connection. "
  (or (ensime-validated-connection ensime-dispatching-connection)
      (ensime-validated-connection ensime-buffer-connection)
      (ensime-validated-connection
       (ensime-owning-connection-for-source-file buffer-file-name))))

(defun ensime-validated-connection (conn)
  "Return conn if connection is non-nil and has a living
 process buffer. nil otherwise."
  (when (and conn (buffer-live-p (process-buffer conn)))
    conn))

(defun ensime-connected-p (&optional conn)
  "Return t if ensime-current-connection would return non-nil.
 Return nil otherwise."
  (let ((conn (or conn (ensime-current-connection))))
    (and conn
	 (buffer-live-p (process-buffer conn)))))


(defun ensime-connection ()
  "Return the connection to use for Lisp interaction.
 Signal an error if there's no connection."
  (let ((conn (ensime-current-connection)))
    (cond ((not conn)
	   (or (ensime-auto-connect)
	       (error "Not connected. M-x ensime to connect")))
	  ((not (eq (process-status conn) 'open))
	   (error "Connection closed."))
	  (t conn))))


(defun ensime-connection-visiting-buffers (conn)
  "Return a list of all buffers associated with the given
 connection."
  (let ((result '()))
    (dolist (buf (buffer-list))
      (let ((f (buffer-file-name buf)))
	(when (and f (ensime-file-belongs-to-connection-p
		      f conn))
	  (setq result (cons buf result)))))
    result))


(defun ensime-file-belongs-to-connection-p (file-in conn)
  "Does the given file belong to the given connection(project)?"
  (let* ((file (file-truename file-in))
	 (config (ensime-config conn))
	 (source-roots (plist-get config :source-roots)))
    (catch 'return
      (dolist (dir source-roots)
	(when (ensime-file-in-directory-p file dir)
	  (throw 'return t))))))


(defun ensime-connections-for-source-file (file-in)
  "Return the connections corresponding to projects that contain
   the given file in their source trees."
  (let ((file (file-truename file-in)))
    (when file
      (let ((result '()))
	(dolist (conn ensime-net-processes)
	  (when-let (conn (ensime-validated-connection conn))
	    (let* ((config (ensime-config conn))
		   (source-roots (plist-get config :source-roots)))
	      (dolist (dir source-roots)
		(when (ensime-file-in-directory-p file dir)
		  (setq result (cons conn result)))))))
	result))))

(defun ensime-probable-owning-connection-for-source-file
  (file-in)
  (ensime-owning-connection-for-source-file file-in t))

(defun ensime-owning-connection-for-source-file (file-in &optional loose)
  "Return the connection corresponding to the single that
 owns the given file. "
  (when file-in
    (let ((file (file-truename file-in)))
      (when file
	(catch 'return
	  ;; First check individual source-roots
	  (dolist (conn ensime-net-processes)
	    (when-let (conn (ensime-validated-connection conn))
	      (let* ((config (ensime-config conn))
		     (project-root (plist-get config :root-dir))
		     (source-roots (plist-get config :source-roots)))
		(if (and loose (ensime-file-in-directory-p file project-root))
		    (throw 'return conn)
		  (dolist (dir source-roots)
		    (when (ensime-file-in-directory-p file dir)
		      (throw 'return conn)))))))
	  )))))



(defun ensime-prompt-for-connection ()
  "Prompt the user to select a server connection. Used in situations where
the active connection is ambiguous."
  (let* ((options
	  (mapcar
	   (lambda (p)
	     (let* ((conf (ensime-config p))
		    (root (plist-get conf :root-dir))
		    (num (ensime-connection-number p)))
	       `(,(format "%s#%s" root num) . ,p)))
	   ensime-net-processes))
	 (keys (mapcar (lambda (opt) (car opt)) options)))
    (let ((key (when keys
		 (completing-read
		  (concat "Which project to use? ("
			  (mapconcat #'identity keys ", ")
			  "): ")
		  keys nil t (car keys)))))
      (cdr (assoc key options)))))


;; FIXME: should be called auto-start
(defcustom ensime-auto-connect 'never
  "Controls auto connection when information from lisp process is needed.
This doesn't mean it will connect right after Ensime is loaded."
  :group 'ensime-mode
  :type '(choice (const never)
		 (const always)
		 (const ask)))

(defun ensime-auto-connect ()
  (cond ((or (eq ensime-auto-connect 'always)
	     (and (eq ensime-auto-connect 'ask)
		  (y-or-n-p "No connection.  Start Ensime? ")))
	 (save-window-excursion
	   (ensime)
	   (while (not (ensime-current-connection))
	     (sleep-for 1))
	   (ensime-connection)))
	(t nil)))

(defun ensime-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((ensime-dispatching-connection process))

    ;; Initialize connection state in the process-buffer of PROC."

    ;; To make life simpler for the user: if this is the only open
    ;; connection then reset the connection counter.
    (when (equal ensime-net-processes (list process))
      (setq ensime-connection-counter 0))

    (ensime-with-connection-buffer
     () (setq ensime-buffer-connection process))

    (setf (ensime-connection-number process)
	  (incf ensime-connection-counter))

    process))

(defmacro* ensime-with-connection-buffer ((&optional process) &rest body)
  "Execute BODY in the process-buffer of PROCESS.
If PROCESS is not specified, `ensime-connection' is used.

\(fn (&optional PROCESS) &body BODY))"
  `(with-current-buffer
       (process-buffer (or ,process (ensime-connection)
			   (error "No connection")))
     ,@body))

(defun ensime-connect (host port)
  "Connect to a running Swank server. Return the connection."
  (interactive (list
		(read-from-minibuffer "Host: " ensime-default-server-host)
		(read-from-minibuffer "Port: " (format "%d" ensime-default-port)
				      nil t)))
  (when (and (interactive-p) ensime-net-processes
	     (y-or-n-p "Close old connections first? "))
    (ensime-disconnect-all))
  (message "Connecting to Swank on port %S.." port)
  (let ()
    (message "Connecting to Swank on port %S.." port)
    (let* ((process (ensime-net-connect host port))
	   (ensime-dispatching-connection process))
      (ensime-setup-connection process))))




(defun ensime-handle-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (ensime-event-sig :connected info)
  (let ((ensime-dispatching-connection connection))
    (destructuring-bind (&key pid server-implementation version
			      &allow-other-keys) info
      (setf (ensime-pid) pid)
      (destructuring-bind (&key name) server-implementation
	(setf (ensime-server-implementation-name) name
	      (ensime-connection-name) (ensime-generate-connection-name name)))
      ))

  (run-hooks 'ensime-connected-hook)
  (message "Connected.")

  ;; Send the project initialization..
  (let ((config (ensime-config connection)))
    (ensime-init-project connection config))

  )


(defun ensime-init-project (conn config)
  "Send configuration to the server process. Setup handler for
 project info that the server will return."
  (ensime-eval-async `(swank:init-project ,config)
		     (ensime-curry #'ensime-handle-project-info
				   conn)))


(defun ensime-handle-project-info (conn info)
  "Handle result of init-project rpc call. Install project information
computed on server into the local config structure."
  (let* ((config (ensime-config conn)))
    (setf config (plist-put config :project-name
			    (or
			     (plist-get config :project-name)
			     (plist-get info :project-name)
			     )))
    (setf config (plist-put config :source-roots
			    (plist-get info :source-roots)))
    (ensime-set-config conn config)
    (force-mode-line-update t)))


(defun ensime-generate-connection-name (server-name)
  (loop for i from 1
	for name = server-name then (format "%s<%d>" server-name i)
	while (find name ensime-net-processes
		    :key #'ensime-connection-name :test #'equal)
	finally (return name)))

(defun ensime-connection-close-hook (process)

  ;; TODO should this be per-connection?
  (ensime-clear-note-overlays))

(add-hook 'ensime-net-process-close-hooks 'ensime-connection-close-hook)



;;; `ensime-rex' is the RPC primitive which is used to implement both
;;; `ensime-eval' and `ensime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defmacro* ensime-rex ((&rest saved-vars)
		       sexp
		       &rest continuations)
  "(ensime-rex (VAR ...) SEXP CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort REASON).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
			 collect (etypecase var
				   (symbol (list var var))
				   (cons var)))
       (ensime-dispatch-event
	(list :swank-rpc ,sexp
	      (lambda (,result)
		(destructure-case ,result
		  ,@continuations)))))))

(put 'ensime-rex 'lisp-indent-function 2)


;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar ensime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun ensime-eval (sexp)
  "Evaluate EXPR on the superior Lisp and return the result."
  (let* ((tag (gensym (format "ensime-result-%d-sym"
			      (1+ (ensime-continuation-counter)))))
	 (ensime-stack-eval-tags (cons tag ensime-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (ensime-rex (tag sexp)
	   sexp

	 ((:ok value)
	  (if (not (member tag ensime-stack-eval-tags))
	      (message
	       "Reply to canceled synchronous eval request tag=%S sexp=%S"
	       tag sexp)
	    (throw tag (list #'identity value))))

	 ((:abort code reason)
	  (message
	   (format
	    "Synchronous RPC Aborted: %s" reason))
	  (throw tag (list #'identity nil))))

       (let ((debug-on-quit t)
	     (inhibit-quit nil)
	     (conn (ensime-connection)))
	 (while t
	   (unless (eq (process-status conn) 'open)
	     (error "Lisp connection closed unexpectedly"))
	   (accept-process-output nil 1 0)))))))


(defun ensime-eval-async (sexp &optional cont)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (ensime-rex (cont (buffer (current-buffer)))
      sexp
    ((:ok result)
     (when cont
       (if (buffer-live-p buffer)
	   (progn
	     (set-buffer buffer)
	     (funcall cont result))
	 (message
	  "ENSIME: Asynchronous return could not find originating buffer.")
	 )))
    ((:abort code reason)
     (message "Asynchronous RPC Aborted: %s" reason)))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; ensime-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :ensime-eval-async)

;;;;; Commands on connections

(defun ensime-disconnect ()
  "Close the current connection."
  (interactive)
  (ensime-net-close (ensime-connection)))

(defun ensime-disconnect-all ()
  "Disconnect all connections."
  (interactive)
  (mapc #'ensime-net-close ensime-net-processes))

(defun ensime-connection-port (connection)
  "Return the remote port number of CONNECTION."
  (if (featurep 'xemacs)
      (car (process-id connection))
    (cadr (process-contact connection))))

(defun ensime-process (&optional connection)
  "Return the ENSIME server process for CONNECTION
 (default `ensime-connection'). Return nil if there's no process
 object for the connection."
  (let ((proc (ensime-server-process connection)))
    (if (and proc
	     (memq (process-status proc) '(run stop)))
	proc)))

;; Non-macro version to keep the file byte-compilable.
(defun ensime-set-server-process (connection process)
  (setf (ensime-server-process connection) process))

(defun ensime-set-config (connection config)
  (setf (ensime-config connection) config))


;; Commonly used functions

(defun ensime-curry (fun &rest args)
  "Partially apply FUN to ARGS.  The result is a new function.
This idiom is preferred over `lexical-let'."
  `(lambda (&rest more) (apply ',fun (append ',args more))))

(defun ensime-rcurry (fun &rest args)
  "Like `ensime-curry' but ARGS on the right are applied."
  `(lambda (&rest more) (apply ',fun (append more ',args))))


;;;;; Protocol event handler (the guts)
;;;
;;; This is the protocol in all its glory. The input to this function
;;; is a protocol event that either originates within Emacs or arrived
;;; over the network from the ENSIME server.
;;;
;;; Each event is a list beginning with a keyword and followed by
;;; arguments. The keyword identifies the type of event. Events
;;; originating from Emacs have names starting with :emacs- and events
;;; from the ENSIME server don't.

(ensime-def-connection-var ensime-rex-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(ensime-def-connection-var ensime-continuation-counter 0
  "Continuation serial number counter.")

(defvar ensime-event-hooks)

(defun ensime-dispatch-event (event &optional process)
  (let ((ensime-dispatching-connection (or process (ensime-connection))))
    (or (run-hook-with-args-until-success 'ensime-event-hooks event)
	(destructure-case event
	  ((:swank-rpc form continuation)
	   (let ((id (incf (ensime-continuation-counter))))
	     (ensime-send `(:swank-rpc ,form ,id))
	     (push (cons id continuation) (ensime-rex-continuations))
	     ))

	  ((:return value id)
	   (let ((rec (assq id (ensime-rex-continuations))))

	     (cond (rec (setf (ensime-rex-continuations)
			      (remove rec (ensime-rex-continuations)))
			(funcall (cdr rec) value)
			(force-mode-line-update t)
			(ensime-event-sig :return-value value))
		   (t
		    (error "Unexpected reply: %S %S" id value)))))


	  ((:full-typecheck-finished)
	   (when (ensime-awaiting-full-typecheck (ensime-connection))
	     (message "Typecheck finished.")
	     (setf (ensime-awaiting-full-typecheck
		    (ensime-connection)) nil)
	     (ensime-show-all-errors-and-warnings))
	   (ensime-event-sig :full-typecheck-finished t))

	  ((:compiler-ready)
	   (ensime-handle-compiler-ready)
	   (ensime-event-sig :compiler-ready t))

	  ((:indexer-ready)
	   (ensime-event-sig :indexer-ready t))

	  ((:scala-notes result)
	   (ensime-add-notes 'scala result))

	  ((:java-notes result)
	   (ensime-add-notes 'java result))

	  ((:clear-all-scala-notes)
	   (ensime-clear-notes 'scala))

	  ((:clear-all-java-notes)
	   (ensime-clear-notes 'java))

	  ((:debug-event evt)
	   (ensime-db-handle-event evt)
	   (ensime-event-sig :debug-event evt))

	  ((:channel-send id msg)
	   (ensime-channel-send (or (ensime-find-channel id)
				    (error "Invalid channel id: %S %S" id msg))
				msg))
	  ((:emacs-channel-send id msg)
	   (ensime-send `(:emacs-channel-send ,id ,msg)))
	  ((:read-from-minibuffer thread tag prompt initial-value)
	   (ensime-read-from-minibuffer-for-swank
	    thread tag prompt initial-value))
	  ((:y-or-n-p thread tag question)
	   (ensime-y-or-n-p thread tag question))
	  ((:emacs-return-string thread tag string)
	   (ensime-send `(:emacs-return-string ,thread ,tag ,string)))
	  ((:new-features features)
	   (setf (ensime-server-features) features))
	  ((:eval-no-wait fun args)
	   (apply (intern fun) args))
	  ((:eval thread tag form-string)
	   (ensime-check-eval-in-emacs-enabled)
	   (ensime-eval-for-lisp thread tag form-string))
	  ((:emacs-return thread tag value)
	   (ensime-send `(:emacs-return ,thread ,tag ,value)))
	  ((:ed what)
	   (ensime-ed what))
	  ((:background-message code detail)
	   (ensime-background-message "%s" detail))
	  ((:reader-error code detail)
	   (ensime-with-popup-buffer
	    ("*Ensime Error*")
	    (princ (format "Invalid protocol message:\n%s\n\n%S"
			   condition packet))
	    (goto-char (point-min)))
	   (error "Invalid protocol message"))
	  ))))

(defun ensime-send (sexp)
  "Send SEXP directly over the wire on the current connection."
  (ensime-net-send sexp (ensime-connection)))


(defun ensime-handle-compiler-ready ()
  "Do any work that should be done the first time the analyzer becomes
 ready for requests."
  (let ((conn (ensime-current-connection)))
    (message "ENSIME ready. %s" (ensime-random-words-of-encouragement))
    (setf (ensime-analyzer-ready conn) t)
    (ensime-sem-high-refresh-all-buffers)
    ))

;;; Words of encouragement

(defun ensime-user-first-name ()
  (let ((name (if (string= (user-full-name) "")
		  (user-login-name)
		(user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar ensime-words-of-encouragement
  `("Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "May the source be with you!"
    "Death to null!"
    "Find closure!"
    "May the _ be with you."
    "M-x be_cool"
    "CanBuildFrom[List[Dream], Reality, List[Reality]]"
    ,(format "%s, this could be the start of a beautiful program."
	     (ensime-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun ensime-random-words-of-encouragement ()
  "Return a string of hackerish encouragement."
  (eval (nth (random (length ensime-words-of-encouragement))
	     ensime-words-of-encouragement)))



;; Compiler Notes (Error/Warning overlays)

;; Note: This might better be a connection-local variable, but
;; afraid that might lead to hanging overlays..

(defvar ensime-note-overlays '()
  "The overlay structures created to highlight notes.")

(defun ensime-all-notes ()
  (append (ensime-scala-compiler-notes (ensime-connection))
	  (ensime-java-compiler-notes (ensime-connection))))


(defun ensime-add-notes (lang result)
  (let ((is-full (plist-get result :is-full))
	(notes (plist-get result :notes)))
    (cond
     ((equal lang 'scala)
      (setf (ensime-scala-compiler-notes (ensime-connection))
	    (append
	     (ensime-scala-compiler-notes (ensime-connection))
	     notes)))

     ((equal lang 'java)
      (setf (ensime-java-compiler-notes (ensime-connection))
	    (append
	     (ensime-java-compiler-notes (ensime-connection))
	     notes))))

    (ensime-make-note-overlays notes)
    (ensime-update-note-counts)
    ))


(defun ensime-clear-notes (lang)
  (cond
   ((equal lang 'scala)
    (setf (ensime-scala-compiler-notes (ensime-connection)) nil))
   ((equal lang 'java)
    (setf (ensime-java-compiler-notes (ensime-connection)) nil)))
  (ensime-clear-note-overlays lang)
  (ensime-update-note-counts))


(defun ensime-make-overlay-at (file line b e msg face)
  "Create an overlay highlighting the given line in
any buffer visiting the given file."
  (let ((beg b)
	(end e))
    (when-let (buf (find-buffer-visiting file))

      ;; If line provided, use line to define region
      (when (integerp line)
	(with-current-buffer buf
	  (save-excursion
	    (ensime-goto-line line)
	    (setq beg (point-at-bol))
	    (setq end (point-at-eol)))))

      ;; If DOS eol's, fix the positioning
      (when (eq 1 (coding-system-eol-type
		   (buffer-local-value
		    'buffer-file-coding-system buf
		    )))
	(with-current-buffer buf
	  (setq beg (- beg (- (line-number-at-pos beg) 1)))
	  (setq end (- end (- (line-number-at-pos end) 1)))))


      (ensime-make-overlay beg end msg face nil buf))
    ))



(defun ensime-make-note-overlays (notes)
  (dolist (note notes)
    (destructuring-bind
	(&key severity msg beg end line col file &allow-other-keys) note

      ;; No empty note overlays!
      (when (eq beg end)
	(setq beg (- beg 1)))

      (let ((lang
	     (cond
	      ((ensime-java-file-p file) 'java)
	      ((ensime-scala-file-p file) 'scala)
	      (t 'scala)))
	    (face
	     (cond
	      ((equal severity 'error)
	       'ensime-errline-highlight)
	      (t
	       'ensime-warnline-highlight))))

	(when-let (ov (ensime-make-overlay-at
		       file nil
		       (+ beg ensime-ch-fix)
		       (+ end ensime-ch-fix)
		       msg face))
	  (overlay-put ov 'lang lang)
	  (push ov ensime-note-overlays))

	))))


(defun ensime-update-note-counts ()
  (let ((notes (ensime-all-notes))
	(num-err 0)
	(num-warn 0)
	(conn (ensime-connection)))
    (dolist (note notes)
      (let ((severity (plist-get note :severity)))
	(cond
	 ((equal severity 'error)
	  (incf num-err))
	 ((equal severity 'warn)
	  (incf num-warn))
	 (t))))
    (setf (ensime-num-errors conn) num-err)
    (setf (ensime-num-warnings conn) num-warn)))


(defun ensime-refresh-all-note-overlays ()
  (let ((notes (if (ensime-connected-p)
		   (append
		    (ensime-java-compiler-notes (ensime-current-connection))
		    (ensime-scala-compiler-notes (ensime-current-connection)))
		 )))
    (ensime-clear-note-overlays)
    (ensime-make-note-overlays notes)
    ))


(defface ensime-errline
  '((((class color) (background dark)) (:background "Firebrick4"))
    (((class color) (background light)) (:background "LightPink"))
    (t (:bold t)))
  "Face used for marking the line on which an error occurs."
  :group 'ensime-ui)

(defface ensime-errline-highlight
  '((((class color) (background dark)) (:background "Firebrick3"))
    (((class color) (background light)) (:background "HotPink"))
    (t (:bold t)))
  "Face used for marking the specific region of an error, if available."
  :group 'ensime-ui)

(defface ensime-warnline
  '((((class color) (background dark)) (:background "DarkBlue"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking the line on which an warning occurs."
  :group 'ensime-ui)

(defface ensime-warnline-highlight
  '((((class color) (background dark)) (:background "dark slate blue"))
    (((class color) (background light)) (:background "DeepSkyBlue1"))
    (t (:bold t)))
  "Face used for marking the specific region of an warning, if available."
  :group 'ensime-ui)


(defun ensime-make-overlay (beg end tooltip-text face &optional mouse-face buf)
  "Allocate a ensime overlay in range BEG and END."
  (let ((ov (make-overlay beg end buf t t)))
    (overlay-put ov 'face           face)
    (overlay-put ov 'mouse-face     mouse-face)
    (overlay-put ov 'help-echo      tooltip-text)
    (overlay-put ov 'ensime-overlay  t)
    (overlay-put ov 'priority 100)
    ov)
  )

(defun ensime-overlays-at (point)
  "Return list of overlays of type 'ensime-overlay at point."
  (let ((ovs (overlays-at point)))
    (remove-if-not
     (lambda (ov) (overlay-get ov 'ensime-overlay))
     ovs)
    ))

(defun ensime-clear-note-overlays (&optional lang)
  "Delete note overlays language. If lang is nil, delete all
 overlays."
  (let ((revised '()))
    (dolist (ov ensime-note-overlays)
      (if (or (null lang)
	      (equal lang (overlay-get ov 'lang)))
	  (delete-overlay ov)
	(setq revised (cons ov revised))))
    (setq ensime-note-overlays revised)))

(defun ensime-next-note-in-current-buffer (notes forward)
  (let ((best-note nil)
	(best-dist most-positive-fixnum))
    (dolist (note notes)
      (if (and (ensime-files-equal-p (ensime-note-file note)
				     buffer-file-name)
	       (/= (ensime-note-beg note) (point)))
	  (let ((dist (cond
		       (forward
			(if (< (ensime-note-beg note) (point))
			    (+ (ensime-note-beg note)
			       (- (point-max) (point)))
			  (- (ensime-note-beg note) (point))))

		       (t (if (> (ensime-note-beg note) (point))
			      (+ (point) (- (point-max)
					    (ensime-note-beg note)))
			    (- (point) (ensime-note-beg note)))))))

	    (when (< dist best-dist)
	      (setq best-dist dist)
	      (setq best-note note))
	    )))
    best-note))

(defun ensime-goto-next-note (forward)
  "Helper to move point to next note. Go forward if forward is non-nil."
  (let* ((conn (ensime-current-connection))
	 (notes (append (ensime-java-compiler-notes conn)
			(ensime-scala-compiler-notes conn)))
	 (next-note (ensime-next-note-in-current-buffer notes forward)))
    (if next-note
	(progn
	  (goto-char (+ ensime-ch-fix (ensime-note-beg next-note)))
	  (message (ensime-note-message next-note)))
      (message (concat
		"No more compilation issues in this buffer. "
		"Use ensime-typecheck-all [C-c C-v a] to find"
		" all issues, project-wide.")))))

(defun ensime-forward-note ()
  "Goto the next compilation note in this buffer"
  (interactive)
  (ensime-goto-next-note t))

(defun ensime-backward-note ()
  "Goto the prev compilation note in this buffer"
  (interactive)
  (ensime-goto-next-note nil))

;; Displaying proposed changes

(defun ensime-insert-change-list (changes)
  "Describe a series of proposed file changes. Used for
 refactoring and undo confirmation buffers."
  (let ((grouped-changed
	 (ensime-group-changes-by-proximity changes)))
    (dolist (ch grouped-changed)
      (let* ((file (plist-get ch :file))
	     (text (plist-get ch :text))
	     (range-start (plist-get ch :from))
	     (range-end (plist-get ch :to))
	     (edits (plist-get ch :edits)))


	;; Make sure edits is not empty
	(when edits

	  (let* ((edits (copy-list edits));; So we can destructively modify
		 (result (ensime-extract-file-chunk
			  file (- range-start 150) (+ range-end 150)))
		 (chunk-text (plist-get result :text))
		 (chunk-start (plist-get result :chunk-start))
		 (chunk-end (plist-get result :chunk-end))
		 (chunk-start-line (plist-get result :chunk-start-line)))


	    ;; Sort in reverse textual order
	    ;; so we can apply edits without disturbing
	    ;; positions further down in chunk.
	    (setq edits (sort edits
			      (lambda (a b)
				(> (plist-get a :from)
				   (plist-get b :from)))))

	    ;; Insert heading for chunk

	    (ensime-insert-with-face file 'font-lock-comment-face)
	    (ensime-insert-with-face
	     (format "\n------------------- @line %s -----------------------\n"
		     chunk-start-line)
	     'font-lock-comment-face)

	    (let ((p (point)))
	      (insert chunk-text)

	      ;; Highlight all the edits in the chunk

	      (dolist (ed edits)
		(let* ((text (plist-get ed :text))
		       (from (+ (plist-get ed :from) ensime-ch-fix))
		       (to (+ (plist-get ed :to) ensime-ch-fix))
		       (len (- to from)))

		  (goto-char (+ p (- from chunk-start)))
		  (delete-char (min len (- (point-max) (point))))
		  (ensime-insert-with-face text 'font-lock-keyword-face)))

	      (goto-char (point-max))
	      (insert "\n\n\n")
	      )))))))


(defun ensime-changes-are-proximate-p (ch1 ch2)
  "Return t if ch1 and ch2 occur nearby in the same file."
  (let* ((len1 (- (plist-get ch1 :to)
		  (plist-get ch1 :from)))
	 (mid1 (+ (plist-get ch1 :from) (/ len1 2)))
	 (len2 (- (plist-get ch2 :to)
		  (plist-get ch2 :from)))
	 (mid2 (+ (plist-get ch2 :from) (/ len2 2))))

    (and (equal (plist-get ch1 :file )
		(plist-get ch2 :file ))
	 (< (abs (- mid1 mid2)) 1000))))


(defun ensime-merge-changes (changes)
  "Return a single change with edits that correspond
 to all edits in all elements of changes."
  (let ((range-start most-positive-fixnum)
	(range-end most-negative-fixnum)
	(edits '()))

    (dolist (ch changes)
      (let ((from (plist-get ch :from))
	    (to (plist-get ch :to)))
	(setq range-start (min range-start from))
	(setq range-end (max range-end to))
	(setq edits (append (plist-get ch :edits)
			    edits))))
    (list
     :file (plist-get ch :file)
     :from range-start
     :to range-end
     :edits edits)))


(defun ensime-group-changes-by-proximity (changes)
  "Create aggregate changes for changes that occur nearby
 eachother in the same file."
  (let ((changes
	 (mapcar
	  (lambda (ch)
	    (list
	     :file (plist-get ch :file)
	     :from (plist-get ch :from)
	     :to (plist-get ch :to)
	     :edits (list
		     (list
		      :from (plist-get ch :from)
		      :to (plist-get ch :to)
		      :text (plist-get ch :text)))))
	  changes))
	(merged '()))

    (while changes
      (let ((ch (pop changes))
	    (neighbors '())
	    (update-merged '()))

	(dolist (m merged)
	  (if (ensime-changes-are-proximate-p m ch)
	      (push m neighbors)
	    (push m update-merged)))

	(push (ensime-merge-changes (cons ch neighbors))
	      update-merged)

	(setq merged update-merged)
	))

    ;; Sort in textual order
    (sort merged (lambda (a b)
		   (< (plist-get a :from)
		      (plist-get b :from))))
    ))


(defun ensime-extract-file-chunk (file-name start end)
  "Return the text of the given file from start to end."
  (with-temp-buffer
    (insert-file-contents file-name)
    (let* ((chunk-start (max start (point-min)))
	   (chunk-end (min end (point-max)))
	   (text (buffer-substring-no-properties chunk-start chunk-end)))
      (list :text text
	    :chunk-start chunk-start
	    :chunk-end chunk-end
	    :chunk-start-line (line-number-at-pos chunk-start)
	    ))))



;; Jump to definition


(defun ensime-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (require 'etags)
  (ring-insert find-tag-marker-ring (point-marker)))

(defun ensime-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (pop-tag-mark))

(defun ensime-edit-definition-other-window ()
  (interactive)
  (ensime-edit-definition 'window))

(defun ensime-edit-definition-other-frame ()
  (interactive)
  (ensime-edit-definition 'frame))

(defun ensime-edit-definition (&optional where)
  "Lookup the definition of the name at point."
  (interactive)

  (let* ((info (ensime-rpc-symbol-at-point))
	 (pos (ensime-symbol-decl-pos info))
	 (offset (ensime-pos-offset pos))
	 (type (ensime-symbol-type info)))
    (cond
     ((ensime-pos-valid-local-p pos)
      (progn
	(ensime-push-definition-stack)
	(ensime-goto-source-location pos where)))

     (type
      (let ((info (ensime-rpc-inspect-type-by-id (ensime-type-id type))))
	(if info
	    (progn
	      (ensime-push-definition-stack)
	      (ensime-type-inspector-show info))
	  (message "Sorry, no definition found."))))

     (t
      (message "Sorry, no definition found.")))))


(defun ensime-files-equal-p (f1 f2)
  "Return t if file-names refer to same file."
  (equal (expand-file-name f1) (expand-file-name f2)))


(defun ensime-window-showing-file (file)
  (catch 'result
    (dolist (w (window-list))
      (let* ((buf (window-buffer w))
	     (window-file (buffer-file-name buf)))
	(when (and window-file
		   (ensime-files-equal-p file window-file))
	  (throw 'result w))))))

(defun ensime-window-showing-buffer (buffer)
  (catch 'result
    (dolist (w (window-list))
      (let* ((buf (window-buffer w)))
	(when (equal buf buffer)
	  (throw 'result w))))))

(defun ensime-point-at-bol (file line)
  (with-current-buffer (find-buffer-visiting file)
    (save-excursion
      (ensime-goto-line line)
      (point)
      )))

(defun ensime-goto-source-location (pos &optional where)
  "Move to the source location POS. Don't open
 a new window or buffer if file is open and visible already."
  (let* ((file (ensime-pos-file pos))
	 (file-visible-window (ensime-window-showing-file file)))

    (when (not file-visible-window)
      (ecase where
	((nil)
	 (find-file file))
	(window
	 (find-file-other-window file)))
      (setq file-visible-window
	    (ensime-window-showing-file file)))

    (let ((buf (window-buffer file-visible-window))
	  (pt (cond
	       ((integerp (ensime-pos-line pos))
		(ensime-point-at-bol file (ensime-pos-line pos)))
	       ((integerp (ensime-pos-offset pos))
		(+ (ensime-pos-offset pos) ensime-ch-fix))
	       (t 0))))
      (with-current-buffer buf
	(goto-char pt))
      (set-window-point file-visible-window pt))))

;; Compilation result interface

(defvar ensime-compile-result-buffer-name "*ENSIME-Compilation-Result*")

(defvar ensime-compile-result-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda()(interactive)
				(ensime-popup-buffer-quit-function)
				))
    (define-key map [?\t] 'forward-button)
    (define-key map [mouse-1] 'push-button)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    map)
  "Key bindings for the build result popup.")

(defface ensime-compile-errline
  '((((class color) (background dark)) (:foreground "#ff5555"))
    (((class color) (background light)) (:foreground "Firebrick4"))
    (t (:bold t)))
  "Face used for marking the line on which an error occurs."
  :group 'ensime-ui)

(defface ensime-compile-warnline
  '((((class color) (background dark)) (:foreground "LightBlue2"))
    (((class color) (background light)) (:foreground "DarkBlue"))
    (t (:bold t)))
  "Face used for marking the line on which an warning occurs."
  :group 'ensime-ui)

(defun ensime-show-compile-result-buffer (notes-in)
  "Show a popup listing the results of the last build."

  (ensime-with-popup-buffer
   (ensime-compile-result-buffer-name t t)
   (use-local-map ensime-compile-result-map)
   (ensime-insert-with-face
    "Latest Compilation Results (q to quit, TAB to jump to next error)"
    'font-lock-constant-face)
   (ensime-insert-with-face
    "\n----------------------------------------\n\n"
    'font-lock-comment-face)
   (if (null notes-in)
       (insert "0 errors, 0 warnings.")
     (save-excursion

       ;; Group notes by their file and sort by
       ;; position in the buffer.
       (let ((notes-by-file (make-hash-table :test 'equal)))
	 (dolist (note notes-in)
	   (let* ((f (ensime-note-file note))
		  (existing (gethash f notes-by-file)))
	     (puthash f (cons note existing) notes-by-file)))
	 (maphash (lambda (file-heading notes-set)
		    (let ((notes (sort (copy-list notes-set)
				       (lambda (a b) (< (ensime-note-beg a)
							(ensime-note-beg b)
							)))))

		      ;; Output file heading
		      (ensime-insert-with-face
		       (concat "\n" file-heading
			       "\n----------------------------------------\n\n")
		       'font-lock-comment-face)

		      ;; Output the notes
		      (dolist (note notes)
			(destructuring-bind
			    (&key severity msg beg
				  end line col file &allow-other-keys) note
			  (let ((face (case severity
					(error 'ensime-compile-errline)
					(warn 'ensime-compile-warnline)
					(info font-lock-string-face)
					(otherwise font-lock-comment-face)
					))
				(header (case severity
					  (error "ERROR")
					  (warn "WARNING")
					  (info "INFO")
					  (otherwise "MISC")
					  ))
				(p (point)))
			    (insert (format "%s: %s : line %s"
					    header msg line))
			    (ensime-make-code-link p (point)
						   file
						   (+ beg ensime-ch-fix)
						   face)))
			(insert "\n\n"))))
		  notes-by-file)))
     (forward-button 1)
     )))


;; Compilation on request

(defun ensime-typecheck-current-file ()
  "Send a request for re-typecheck of current file to all ENSIME servers
 managing projects that contains the current file. File is saved
 first if it has unwritten modifications."
  (interactive)

  (if (buffer-modified-p) (ensime-write-buffer nil t))

  ;; Send the reload requist to all servers that might be interested.
  (dolist (con (ensime-connections-for-source-file buffer-file-name))
    (let ((ensime-dispatching-connection con))
      (ensime-rpc-async-typecheck-file
       buffer-file-name 'identity
       ))))

(defun ensime-typecheck-all ()
  "Send a request for re-typecheck of whole project to the ENSIME server.
   Current file is saved if it has unwritten modifications."
  (interactive)
  (message "Checking entire project...")
  (if (buffer-modified-p) (ensime-write-buffer nil t))
  (setf (ensime-awaiting-full-typecheck (ensime-connection)) t)
  (ensime-rpc-async-typecheck-all 'identity))

(defun ensime-show-all-errors-and-warnings ()
  "Show a summary of all compilation notes."
  (interactive)
  (let ((notes (append (ensime-java-compiler-notes (ensime-connection))
		       (ensime-scala-compiler-notes (ensime-connection)))))
    (ensime-show-compile-result-buffer
     notes)))


(defun ensime-sym-at-point (&optional point)
  "Return information about the symbol at point. If not looking at a
 symbol, return nil."
  (save-excursion
    (goto-char (or point (point)))
    (let ((start nil)
	  (end nil))
      (when (thing-at-point 'symbol)
	(save-excursion
	  (search-backward-regexp "\\W" nil t)
	  (setq start (+ (point) 1)))
	(save-excursion
	  (search-forward-regexp "\\W" nil t)
	  (setq end (- (point) 1)))
	(list :start start
	      :end end
	      :name (buffer-substring-no-properties start end))))))


(defun ensime-insert-import (qualified-name)
  "A simple, hacky import insertion."
  (save-excursion

  (let ((starting-point (point)))
    (search-backward-regexp "^\\s-*package\\s-" nil t)
    (search-forward-regexp "^\\s-*import\\s-" starting-point t)
    (goto-char (point-at-bol))

    ;; No imports yet
    (when (looking-at "^\\s-*package\\s-")
      (goto-char (point-at-eol))
      (newline)
      )

    (when (looking-at "^\\s-*import\\s-")
      (left-char 1)
      (while (progn
	     (if (looking-at "[\n\t ]*import\\s-\\(.+\\)\n")
		 (let ((imported-name (match-string 1)))
		   (string< imported-name qualified-name)
		   )))
        (search-forward-regexp "^\\s-*import\\s-" starting-point t)
        (goto-char (point-at-eol)))
      )
    )

  (newline)
  (insert (format (cond ((ensime-visiting-scala-file-p) "import %s")
                        ((ensime-visiting-java-file-p) "import %s;"))
                  qualified-name))
  (indent-region (point-at-bol) (point-at-eol))))



(defun ensime-import-type-at-point (&optional non-interactive)
  "Suggest possible imports of the qualified name at point.
 If user selects and import, add it to the import list."
  (interactive)
  (let* ((sym (ensime-sym-at-point))
	 (name (plist-get sym :name))
	 (name-start (plist-get sym :start))
	 (name-end (plist-get sym :end))
	 (suggestions (ensime-rpc-import-suggestions-at-point (list name) 10)))
    (when suggestions
      (let* ((names (mapcar
		     (lambda (s)
		       (propertize (plist-get s :name)
				   'local-name
				   (plist-get s :local-name)))
		     (apply 'append suggestions)))
	     (selected-name
	      (if non-interactive (car names)
		(popup-menu*
		 names :point (point)))))
	(when selected-name
	  (save-excursion
	    (when (not (equal selected-name name))
	      (goto-char name-start)
	      (delete-char (- name-end name-start))
	      (insert (get-text-property
		       0 'local-name selected-name)))
	    (let ((qual-name
		   (ensime-strip-dollar-signs
		    (ensime-kill-txt-props selected-name))))
	      (ensime-insert-import qual-name)
	      (ensime-typecheck-current-file)
	      ))
	  )))
    ))

(defvar ensime-dir (file-name-directory load-file-name)
  "The root dir of the Ensime distribution.")

(defun ensime-recompile-el ()
  "Byte-recompilation of all Emacs Lisp files."
  (interactive)
  (byte-recompile-directory ensime-dir 0))

;; Source Formatting

(defun ensime-format-source ()
  "Format the source in the current buffer using the Scalariform
 formatting library."
  (interactive)
  (ensime-with-buffer-written-to-tmp
   (file)
   (message "Formatting...")
   (ensime-rpc-async-format-files
    (list file)
    `(lambda (result)
       (ensime-revert-visited-files (list (list ,buffer-file-name ,file)) t)
       ))))

;; Expand selection

(defvar ensime-selection-overlay nil)
(defvar ensime-selection-stack nil)

(defun ensime-set-selection-overlay (start end)
  "Set the current selection overlay, creating if needed."
  (ensime-clear-selection-overlay)
  (setq ensime-selection-overlay
	(ensime-make-overlay start end nil 'region nil)))

(defun ensime-clear-selection-overlay ()
  (when (and ensime-selection-overlay
	     (overlayp ensime-selection-overlay))
    (delete-overlay ensime-selection-overlay)))

(defun ensime-expand-selection-command ()
  "Expand selection to the next widest syntactic context."
  (interactive)
  (unwind-protect
      (let* ((continue t)
	     (ensime-selection-stack (list (list (point) (point))))
	     (expand-again-key 46)
	     (contract-key 44))
	(ensime-expand-selection (point) (point))
	(while continue
	  (message "(Type . to expand again. Type , to contract.)")
	  (let ((evt (read-event)))
	    (cond

	     ((equal expand-again-key evt)
	      (progn
		(clear-this-command-keys t)
		(ensime-expand-selection (mark) (point))
		(setq last-input-event nil)))

	     ((equal contract-key evt)
	      (progn
		(clear-this-command-keys t)
		(ensime-contract-selection)
		(setq last-input-event nil)))
	     (t
	      (setq continue nil)))))
	(when last-input-event
	  (clear-this-command-keys t)
	  (setq unread-command-events (list last-input-event))))

    (ensime-clear-selection-overlay)))

(defun ensime-set-selection (start end)
  "Helper to set selection state."
  (goto-char start)
  (command-execute 'set-mark-command)
  (goto-char end)
  (ensime-set-selection-overlay start end))

(defun ensime-expand-selection (start end)
  "Expand selection to the next widest syntactic context."
  (ensime-with-buffer-written-to-tmp
   (file)
   (let* ((range (ensime-rpc-expand-selection
		  file start end))
	  (start (plist-get range :start))
	  (end (plist-get range :end)))
     (ensime-set-selection start end)
     (push (list start end) ensime-selection-stack)
     )))

(defun ensime-contract-selection ()
  "Contract to previous syntactic context."
  (pop ensime-selection-stack)
  (let ((range (car ensime-selection-stack)))
    (when range
      (let ((start (car range))
	    (end (cadr range)))
	(ensime-set-selection start end)))))


;; Basic RPC calls

(defun ensime-rpc-method-bytecode (file line)
  (ensime-eval
   `(swank:method-bytecode ,file ,line)))

(defun ensime-rpc-debug-active-vm ()
  (ensime-eval
   `(swank:debug-active-vm)))

(defun ensime-rpc-debug-backtrace (thread-id index count)
  (ensime-eval
   `(swank:debug-backtrace ,thread-id ,index ,count)))

(defun ensime-rpc-async-debug-backtrace (thread-id index count continue)
  (ensime-eval-async
   `(swank:debug-backtrace ,thread-id ,index ,count) continue))

(defun ensime-rpc-debug-value-for-name (thread-id name)
  (ensime-eval
   `(swank:debug-value-for-name ,thread-id ,name)))

(defun ensime-rpc-debug-value (location)
  (ensime-eval
   `(swank:debug-value ,location)))

(defun ensime-rpc-debug-set-value (location new-val)
  (ensime-eval
   `(swank:debug-set-value ,location ,new-val)))

(defun ensime-rpc-debug-start (command-line)
  (ensime-eval
   `(swank:debug-start ,command-line)))

(defun ensime-rpc-debug-attach (hostname port)
  (ensime-eval
   `(swank:debug-attach ,hostname ,port)))

(defun ensime-rpc-debug-stop ()
  (ensime-eval
   `(swank:debug-stop)))

(defun ensime-rpc-debug-next (thread-id)
  (ensime-eval
   `(swank:debug-next ,thread-id)))

(defun ensime-rpc-debug-continue (thread-id)
  (ensime-eval
   `(swank:debug-continue ,thread-id)))

(defun ensime-rpc-debug-run ()
  (ensime-eval
   `(swank:debug-run)))

(defun ensime-rpc-debug-step (thread-id)
  (ensime-eval
   `(swank:debug-step ,thread-id)))

(defun ensime-rpc-debug-step-out (thread-id)
  (ensime-eval
   `(swank:debug-step-out ,thread-id)))

(defun ensime-rpc-debug-list-breakpoints ()
  (ensime-eval
   `(swank:debug-list-breakpoints)))

(defun ensime-rpc-debug-set-break (file line)
  (ensime-eval
   `(swank:debug-set-break ,file ,line)))

(defun ensime-rpc-debug-clear-break (file line)
  (ensime-eval
   `(swank:debug-clear-break ,file ,line)))

(defun ensime-rpc-debug-clear-all-breaks ()
  (ensime-eval
   `(swank:debug-clear-all-breaks)))

(defun ensime-rpc-symbol-at-point ()
  (ensime-eval
   `(swank:symbol-at-point ,buffer-file-name ,(ensime-computed-point))))

(defun ensime-rpc-repl-config ()
  "Get the configuration information needed to launch the scala interpreter
with the current project's dependencies loaded. Returns a property list."
  (ensime-eval
   `(swank:repl-config)))

(defun ensime-rpc-remove-file (file-name)
  (ensime-eval `(swank:remove-file ,file-name)))

(defun ensime-rpc-async-typecheck-file (file-name continue)
  (ensime-eval-async `(swank:typecheck-file ,file-name) continue))

(defun ensime-rpc-async-typecheck-all (continue)
  (ensime-eval-async `(swank:typecheck-all) continue))

(defun ensime-rpc-async-builder-init (continue)
  (ensime-eval-async `(swank:builder-init) continue))

(defun ensime-rpc-async-builder-update (file-names continue)
  (ensime-eval-async `(swank:builder-update-files ,file-names) continue))

(defun ensime-rpc-async-format-files (file-names continue)
  (ensime-eval-async `(swank:format-source ,file-names) continue))

(defun ensime-rpc-expand-selection (file-name start end)
  (ensime-internalize-offset-fields
   (ensime-eval `(swank:expand-selection
		  ,file-name
		  ,(ensime-externalize-offset start)
		  ,(ensime-externalize-offset end)))
   :start
   :end
   ))


(defun ensime-rpc-completions-at-point (&optional max-results case-sens)
  (ensime-eval
   `(swank:completions
     ,buffer-file-name
     ,(ensime-computed-point)
     ,(or max-results 0)
     ,case-sens
     t ;; reload
     )))

(defun ensime-rpc-import-suggestions-at-point (names max-results)
  (ensime-eval
   `(swank:import-suggestions
     ,buffer-file-name
     ,(ensime-computed-point)
     ,names
     ,max-results
     )))

(defun ensime-rpc-async-public-symbol-search
  (names max-results continue)
  (ensime-eval-async
   `(swank:public-symbol-search
     ,names
     ,max-results
     ) continue))

(defun ensime-rpc-uses-of-symbol-at-point ()
  (ensime-eval
   `(swank:uses-of-symbol-at-point
     ,buffer-file-name
     ,(ensime-computed-point)
     )))

(defun ensime-rpc-package-member-completions (path &optional prefix)
  (ensime-eval
   `(swank:package-member-completion ,path ,(or prefix ""))))

(defun ensime-rpc-get-type-by-id (id)
  (if (and (integerp id) (> id -1))
      (ensime-eval
       `(swank:type-by-id ,id))))

(defun ensime-rpc-get-type-by-name (name)
  (ensime-eval
   `(swank:type-by-name ,name)))

(defun ensime-rpc-get-type-by-name-at-point (name)
  (ensime-eval
   `(swank:type-by-name-at-point
     ,name ,buffer-file-name ,(ensime-computed-point))))

(defun ensime-rpc-get-type-at-point ()
  (ensime-eval
   `(swank:type-at-point ,buffer-file-name ,(ensime-computed-point))))

(defun ensime-rpc-inspect-type-at-point ()
  (ensime-eval
   `(swank:inspect-type-at-point ,buffer-file-name ,(ensime-computed-point))))

(defun ensime-rpc-inspect-type-by-id (id)
  (if (and (integerp id) (> id -1))
      (ensime-eval
       `(swank:inspect-type-by-id ,id))))

(defun ensime-rpc-inspect-package-by-path (path)
  (ensime-eval
   `(swank:inspect-package-by-path ,path)))

(defun ensime-rpc-get-call-completion (id)
  (if (and (integerp id) (> id -1))
      (ensime-eval
       `(swank:call-completion ,id))))

(defun ensime-rpc-peek-undo ()
  (ensime-eval
   `(swank:peek-undo)))

(defun ensime-rpc-exec-undo (id)
  (ensime-eval
   `(swank:exec-undo ,id)))

(defun ensime-rpc-refactor-prepare
  (proc-id refactor-type params non-interactive continue blocking)
  (if blocking
      (ensime-eval
       `(swank:prepare-refactor
	 ,proc-id ,refactor-type ,params ,(not non-interactive)))
    (ensime-eval-async
     `(swank:prepare-refactor
       ,proc-id ,refactor-type ,params ,(not non-interactive)) continue)))

(defun ensime-rpc-refactor-exec (proc-id refactor-type continue)
  (ensime-eval-async `(swank:exec-refactor ,proc-id , refactor-type) continue))

(defun ensime-rpc-refactor-cancel (proc-id)
  (ensime-eval-async `(swank:cancel-refactor ,proc-id) #'identity))


(defun ensime-rpc-shutdown-server ()
  (ensime-eval `(swank:shutdown-server)))

(defun ensime-rpc-symbol-designations (file start end requested-types continue)
  (ensime-eval-async `(swank:symbol-designations ,file ,start ,end ,requested-types)
		     continue))


;; Uses UI

(defvar ensime-uses-buffer-name "*Uses*")

(defvar ensime-uses-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'forward-button)
    (define-key map [mouse-1] 'push-button)
    (define-key map (kbd "q") 'ensime-popup-buffer-quit-function)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    map)
  "Key bindings for the uses popup.")

(defun ensime-show-uses-of-symbol-at-point ()
  "Display a hyperlinked list of the source locations
 where the symbol under point is referenced."
  (interactive)
  (let ((uses (ensime-rpc-uses-of-symbol-at-point)))
    (ensime-with-popup-buffer
     (ensime-uses-buffer-name t t)
     (use-local-map ensime-uses-buffer-map)


     (ensime-insert-with-face
      "TAB to advance to next use, q to quit"
      'font-lock-constant-face)
     (insert "\n\n\n")

     (dolist (pos uses)
       (let* ((file (ensime-pos-file pos))

	      (range-start (- (ensime-pos-offset pos) 80))
	      (range-end (+ (ensime-pos-offset pos) 80))
	      (result (ensime-extract-file-chunk
		       file range-start range-end))
	      (chunk-text (plist-get result :text))
	      (chunk-start (plist-get result :chunk-start))
	      (chunk-start-line (plist-get result :chunk-start-line)))

	 (ensime-insert-with-face file 'font-lock-comment-face)
	 (ensime-insert-with-face
	  (format "\n------------------- @line %s -----------------------\n"
		  chunk-start-line)
	  'font-lock-comment-face)

	 (let ((p (point)))

	   ;; Insert the summary chunk
	   (insert chunk-text)

	   ;; Highlight the occurances
	   (let* ((from (+ (plist-get pos :start) ensime-ch-fix))
		  (to (+ (plist-get pos :end) ensime-ch-fix))
		  (len (- to from))
		  (buffer-from (+ p (- from chunk-start)))
		  (buffer-to (+ p (- to chunk-start))))
	     (ensime-make-code-link
	      buffer-from buffer-to file from)))

	 (insert "\n\n\n")
	 ))
     (goto-char (point-min))
     (when uses (forward-button 1))
     )
    (ensime-event-sig :references-buffer-shown)
    ))

;; Type Inspector UI

(defvar ensime-inspector-buffer-name "*Inspector*")

(defvar ensime-indent-level 0
  "In inspector UI, how much to indent.")

(defun ensime-inspector-buffer-p (buffer)
  "Is this an ensime inspector buffer?"
  (eq (get-buffer ensime-inspector-buffer-name) buffer))

(defun ensime-popup-buffer-p (buffer)
  "Is this an ensime popup buffer?"
  (with-current-buffer buffer
    ensime-is-popup-buffer))

(defun ensime-inspector-insert-linked-package-path (path &optional face)
  "For each component of the package path, insert a link to inspect
   that package."
  (let ((pieces (split-string path "\\."))
	(accum ""))
    (dolist (piece pieces)
      (setq accum (concat accum piece))
      (ensime-insert-action-link
       piece
       `(lambda (x)
	  (ensime-inspect-package-by-path ,accum))
       (or face font-lock-type-face))
      (insert ".")
      (setq accum (concat accum "."))
      )))


(defun ensime-inspector-insert-link-to-type-id (text type-id &optional is-obj)
  "A helper for type link insertion. See usage in
 ensime-inspector-insert-linked-type. If is-obj is
 non-nil, use an alternative color for the link."
  (ensime-insert-action-link
   text
   `(lambda (x)
      (ensime-type-inspector-show
       (ensime-rpc-inspect-type-by-id ,type-id)
       ))
   (if is-obj
       font-lock-constant-face
     font-lock-type-face)
   ))

(defun ensime-inspector-insert-linked-type
  (type &optional with-doc-link qualified)
  "Helper utility to output a link to a type.
 Should only be invoked by ensime-inspect-type-at-point"
  (if (ensime-type-is-arrow-p type)
      (ensime-inspector-insert-linked-arrow-type type with-doc-link qualified)

    (let* ((type-args (ensime-type-type-args type))
	   (last-type-arg (car (last type-args)))
	   (is-obj (ensime-type-is-object-p type)))

      (insert (make-string ensime-indent-level ?\s))

      (if qualified
	  (ensime-with-name-parts
	   (ensime-type-full-name type)
	   (path outer-type-name name)
	   (when path
	     (ensime-inspector-insert-linked-package-path path))
	   (if (and outer-type-name (integerp (ensime-outer-type-id type)))
	       (progn
		 (ensime-inspector-insert-link-to-type-id
		  outer-type-name (ensime-outer-type-id type))
		 (insert "$")
		 (ensime-inspector-insert-link-to-type-id
		  name (ensime-type-id type) is-obj))
	     (progn
	       (ensime-inspector-insert-link-to-type-id
		name (ensime-type-id type) is-obj))))

	;; Otherwise, insert short name..
	(ensime-inspector-insert-link-to-type-id
	 (ensime-type-name type) (ensime-type-id type) is-obj))

      (when type-args
	(let ((ensime-indent-level 0))
	  (insert "[")
	  (dolist (tpe type-args)
	    (ensime-inspector-insert-linked-type tpe nil nil)
	    (if (not (eq tpe last-type-arg))
		(insert ", ")))
	  (insert "]")))

      (when with-doc-link
	(let* ((pos (plist-get type :pos))
	       (url (or (ensime-pos-file pos)
			(ensime-make-doc-url type)
			)))
	  (ensime-insert-link " doc" url
			      (+ (or (ensime-pos-offset pos) 0)
				 ensime-ch-fix))))

      )))

(defun ensime-inspector-insert-linked-arrow-type
  (type  &optional with-doc-link qualified)
  "Helper utility to output a link to a type.
   Should only be invoked by ensime-inspect-type-at-point"
  (let*  ((param-sections (ensime-type-param-sections type))
	  (result-type (ensime-type-result-type type)))
    (dolist (sect param-sections)
      (let ((params (plist-get sect :params)))
	(insert "(")
	(let ((last-param (car (last params))))
	  (dolist (p params)
	    (let ((tpe (cadr p)))
	      (ensime-inspector-insert-linked-type tpe nil qualified)
	      (if (not (eq p last-param))
		  (insert ", "))))
	  (insert ") => "))))
    (ensime-inspector-insert-linked-type result-type nil qualified)
    ))


(defun ensime-inspector-insert-linked-member (owner-type m)
  "Helper utility to output a link to a type member.
   Should only be invoked by ensime-inspect-type-at-point"
  (let* ((type (ensime-member-type m))
	 (pos (ensime-member-pos m))
	 (member-name (ensime-member-name m))
	 (url (or (ensime-pos-file pos)
		  (ensime-make-doc-url owner-type m)
		  )))

    (if (or (equal 'method (ensime-declared-as m))
	    (equal 'field (ensime-declared-as m)))
	(progn
	  (ensime-insert-link
	   (format "%s" member-name) url
	   (+ (or (ensime-pos-offset pos) 0) ensime-ch-fix)
	   font-lock-function-name-face)
	  (tab-to-tab-stop)
	  (ensime-inspector-insert-linked-type type nil nil))

      ;; otherwise, assume it's a nested type
      (progn
	(ensime-insert-with-face
	 (ensime-declared-as-str m)
	 'font-lock-comment-face)
	(tab-to-tab-stop)
	(ensime-inspector-insert-linked-type type nil nil)
	))
    ))


(defun ensime-inspect-type-at-point-other-frame ()
  "See ensime-inspect-type-at-point, but in other frame."
  (interactive)
  (let ((ensime-popup-in-other-frame t))
    (ensime-inspect-type-at-point)))

(defun ensime-type-inspect-info-at-point ()
  "Helper to pull the inspect info for object at point."
  (let* ((imported-type-path (ensime-imported-type-path-at-point))
	 (imported-type (when imported-type-path
			  (ensime-rpc-get-type-by-name-at-point
			   imported-type-path))))
    (if imported-type
	;; if imported type under point
	(ensime-rpc-inspect-type-by-id
	 (ensime-type-id imported-type))
      ;; otherwise do normal type inspection
      (ensime-rpc-inspect-type-at-point))))

(defun ensime-inspect-java-type-at-point ()
  "Use the global index to search for type at point.
 Inspect the type selected by user."
  (let* ((sym (ensime-sym-at-point))
	 (name (plist-get sym :name))
	 (name-start (plist-get sym :start))
	 (name-end (plist-get sym :end))
	 (suggestions (ensime-rpc-import-suggestions-at-point (list name) 10)))
    (when suggestions
      (let* ((names (mapcar
		     (lambda (s)
		       (propertize (plist-get s :name)
				   'local-name
				   (plist-get s :local-name)))
		     (apply 'append suggestions)))
	     (selected-name
	      (popup-menu*
	       names :point (point))))
	(when selected-name
	  (ensime-inspect-by-path
	   (ensime-kill-txt-props selected-name))
	  )))))

(defun ensime-inspect-type-at-point ()
  "Display a list of all the members of the type under point, sorted by
   owner type."
  (interactive)
  (let ((pack-path (ensime-package-path-at-point)))

    (cond ((ensime-visiting-java-file-p)
	   (ensime-inspect-java-type-at-point))

	  (t ;; inspect package if package under point
	   (if pack-path (ensime-inspect-package-by-path pack-path)
	     ;; otherwise, inspect type
	     (let* ((inspect-info (ensime-type-inspect-info-at-point)))
	       (ensime-type-inspector-show inspect-info)))))))

(defun ensime-type-inspector-show (info &optional focus-on-member)
  "Display a list of all the members of the type under point, sorted by
   owner type."
  (if (null info)
      (message "Cannot inspect nil type.")
    (let* ((interfaces (plist-get info :interfaces))
	   (type (plist-get info :type))
	   (companion-id (plist-get info :companion-id))
	   (buffer-name ensime-inspector-buffer-name)
	   (ensime-indent-level 0)
	   (focus-point nil))
      (ensime-with-inspector-buffer
       (buffer-name info t)

       ;; We want two main columns. The first, 20 chars wide.
       (let ((tab-stop-list '(20)))
	 (setq wrap-prefix (make-string 21 ?\s))

	 ;; Display main type
	 (let* ((full-type-name (plist-get type :name)))
	   (ensime-insert-with-face (format "%s\n"
					    (ensime-declared-as-str type))
				    font-lock-comment-face)
	   (ensime-inspector-insert-linked-type type t t)
	   (insert "\n")

	   ;; Insert a link to the companion object or class, if extant
	   (when-let (id companion-id)
	     (ensime-inspector-insert-link-to-type-id
	      "(companion)" id
	      (not (ensime-type-is-object-p type))))

	   ;; Display each member, arranged by owner type
	   (dolist (interface interfaces)
	     (let* ((owner-type (plist-get interface :type))
		    (implicit (plist-get interface :via-view))
		    (members (plist-get owner-type :members)))
	       (ensime-insert-with-face
		(format "\n\n%s%s\n"
			(ensime-declared-as-str owner-type)
			(if implicit
			    (concat " (via implicit, " implicit ")") ""))
		font-lock-comment-face)
	       (ensime-inspector-insert-linked-type owner-type t t)
	       (insert "\n")
	       (insert "---------------------------\n")
	       (dolist (m members)
		 (when (and focus-on-member
			    (equal (ensime-member-name m)
				   focus-on-member))
		   (setq focus-point (point)))
		 (ensime-inspector-insert-linked-member owner-type m)
		 (insert "\n")
		 )
	       ))

	   (if (integerp focus-point)
	       (progn (goto-char focus-point)
		      (recenter-top-bottom))
	     (goto-char (point-min)))
	   ))
       ))))



;; Inspector

(defun ensime-path-completions (path predicate flag)
  "Return a list of valid completions of the given qualified path.
See: http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_280.html for the
interface we are implementing."
  ;; Note: when this function is invoked from completing-read, current
  ;; buffer will be mini-buffer. We need to setup connection manually.
  ;; See ensime-completing-read-path...
  (ensime-with-path-and-name
   path (pack name)
   (let* ((members (ensime-rpc-package-member-completions pack name))
	  (candidates (mapcar (lambda (ea)
				(let ((name (plist-get ea :name)))
				  (if (and pack (> (length pack) 0))
				      (concat pack "." name) name)))
			      members)))
     (cond
      ((null flag) (try-completion path candidates predicate))
      ((eq flag t) (all-completions path candidates predicate))
      ((eq 'lambda flag) (member candidates path))
      (t nil)
      ))))

(defun ensime-completing-read-path (prompt &optional initial)
  ;; Note: First thing we do is bind buffer connection so
  ;; completion function will have access.
  (let ((ensime-dispatching-connection
	 (ensime-current-connection)))
    (completing-read prompt #'ensime-path-completions
		     nil nil (or initial (ensime-package-containing-point)))))

(defun ensime-inspect-package-by-path (path)
  (ensime-package-inspector-show
   (ensime-rpc-inspect-package-by-path path)))

(defun ensime-inspect-by-path (&optional path focus-on-member)
  "Open the Inspector on the type or package denoted by path. If path is nil,
read a fully qualified path from the minibuffer."
  (interactive)
  (let* ((case-fold-search nil))
    (let ((p (or path
		 (ensime-completing-read-path
		  "Qualified type or package name: "))))
      (ensime-with-path-and-name
       p (pack name)
       (if (and name (integerp (string-match "^[a-z_0-9]+$" name)))
	   (ensime-inspect-package-by-path p)
	 (let ((type (ensime-rpc-get-type-by-name p)))
	   (if type
	       (let ((info (ensime-rpc-inspect-type-by-id
			    (ensime-type-id type))))
		 (ensime-type-inspector-show info focus-on-member))
	     (message "Could not locate type named '%s'." p))
	   ))))))


(defun ensime-package-path-at-point ()
  "Return the package path at point, or nil if point is not in a package path."
  (let* ((case-fold-search nil)
	 (re "\\(?:package\\|import\\)[ ]+\\(\\(?:[a-z][a-z0-9_]+\\.\\)+[a-z][a-z0-9]+\\)"))
    (save-excursion
      (catch 'return
	(let ((init-point (point))
	      (limit (point-at-eol)))
	  (goto-char (point-at-bol))
	  (while (search-forward-regexp re limit t)
	    (if (and (>= init-point (match-beginning 1))
		     (<= init-point (match-end 1)))
		(throw 'return
		       (ensime-kill-txt-props
			(match-string 1))))))))))


(defun ensime-package-containing-point ()
  "Return the package point is in."
  (save-excursion
    (when (search-backward-regexp
	   "^package \\(\\(?:[a-z0-9_]+\\.\\)*[a-z0-9_]+\\)"
	   (point-min) t)
      (let ((path (match-string 1)))
	(ensime-kill-txt-props path)))))

(defun ensime-imported-type-path-at-point ()
  "Return the qualified name of the type being imported at point."
  (when-let (sym (symbol-at-point))
    (let ((sym-name (ensime-kill-txt-props
		     (symbol-name sym))))
      (when (and (integerp (string-match "^[A-ZA-z_]+$" sym-name))
		 (save-excursion
		   (beginning-of-line)
		   (search-forward-regexp
		    (concat
		     "^\\s-*import \\(\\(?:[a-z0-9_]+\\.\\)*\\)"
		     "\\(?:[A-Z][A-z0-9_\\.]+\\|{[A-z0-9_\\., \n]+}\\)$")
		    (point-at-eol) t)))
	(let ((path (ensime-kill-txt-props (match-string 1))))
	  (concat path sym-name))))))

(defun ensime-inspect-package-at-point ()
  "If cursor is over a package path, inspect that path. Otherwise,
inspect the package of the current source file."
  (interactive)
  (let ((pack (or (ensime-package-path-at-point)
		  (ensime-package-containing-point))))
    (if pack
	(ensime-inspect-by-path pack)
      (message "No package declaration found."))))


(defun ensime-inspect-project-package ()
  "Inspect the package declared as the project package in the config file."
  (interactive)
  (let* ((config (ensime-config))
	 (given (plist-get config :project-package)))
    (ensime-inspect-by-path given)))

(defun ensime-inspector-insert-package (pack)
  "Helper to insert a hyper-linked package name."
  (let ((name (ensime-package-full-name pack))
	(members (ensime-package-members pack)))
    (insert (make-string ensime-indent-level ?\s))
    (ensime-inspector-insert-linked-package-path name font-lock-variable-name-face)
    (insert "\n")
    (let ((ensime-indent-level (+ ensime-indent-level 5)))
      (dolist (ea members)
	(when (not (ensime-package-p ea))
	  (ensime-inspector-insert-linked-type ea nil nil)
	  (ensime-insert-with-face
	   (format " %s" (ensime-declared-as-str ea))
	   font-lock-comment-face)
	  (insert "\n")))
      (dolist (ea members)
	(when (ensime-package-p ea)
	  (ensime-inspector-insert-package ea)
	  ))
      )))

(defun ensime-package-inspector-show (info)
  "Display a list of all the members of the provided package."
  (if (null info)
      (message "Cannot inspect nil package.")
    (let* ((buffer-name ensime-inspector-buffer-name)
	   (ensime-indent-level 0))
      (ensime-with-inspector-buffer
       (buffer-name info t)
       (ensime-inspector-insert-package info)
       (goto-char (point-min))
       ))))

(defvar ensime-inspector-history '()
  "Maintain a history of the info objects viewed in the inspector buffer.")

(defvar ensime-inspector-history-cursor 0
  "Where are we in the history?")

(defvar ensime-inspector-paging-in-progress nil
  "A dynamic variable to inform dynamic extant of user's intent.
   Are we moving in history, or inspecting a new info?")

(defun ensime-inspector-backward-page ()
  "Inspect the info object preceding current in history."
  (interactive)
  (setq ensime-inspector-history-cursor
	(min (- (length ensime-inspector-history) 1)
	     (+ ensime-inspector-history-cursor 1)))
  (ensime-inspector-goto-cursor))

(defun ensime-inspector-forward-page ()
  "Inspect the info object following current in history."
  (interactive)
  (setq ensime-inspector-history-cursor
	(max 0 (- ensime-inspector-history-cursor 1)))
  (ensime-inspector-goto-cursor))


(defun ensime-inspector-goto-cursor ()
  "Helper to jump to a specific point in history."
  (let ((info (nth ensime-inspector-history-cursor
		   ensime-inspector-history))
	(ensime-inspector-paging-in-progress t))

    (cond ((ensime-package-p info)
	   (ensime-package-inspector-show info))

	  ((ensime-type-inspection-p info)
	   (ensime-type-inspector-show info))

	  (t (error
	      (format "Cannot inspect unknown structure: %s"
		      info))))
    ))


(defvar ensime-popup-inspector-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'forward-button)
    (define-key map [mouse-1] 'push-button)
    (define-key map (kbd "q") 'ensime-popup-buffer-quit-function)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    (define-key map (kbd ".") 'ensime-inspector-forward-page)
    (define-key map (kbd ",") 'ensime-inspector-backward-page)
    map)
  "Type and package inspector key bindings.")


(defmacro* ensime-with-inspector-buffer ((name object &optional select)
					 &body body)
  "Extend the standard popup buffer with inspector-specific bindings."
  `(ensime-with-popup-buffer
    (,name t ,select)
    (use-local-map ensime-popup-inspector-map)
    (when (not ensime-inspector-paging-in-progress)

      ;; Clamp the history cursor
      (setq ensime-inspector-history-cursor
	    (max 0 ensime-inspector-history-cursor))
      (setq ensime-inspector-history-cursor
	    (min (- (length ensime-inspector-history) 1)
		 ensime-inspector-history-cursor))

      ;; Remove all elements preceding the cursor (the 'redo' history)
      (setq ensime-inspector-history
	    (subseq ensime-inspector-history
		    ensime-inspector-history-cursor))

      ;; Add the new history item
      (push ,object ensime-inspector-history)

      ;; Set cursor to point to the new item
      (setq ensime-inspector-history-cursor 0)

      )
    ,@body
    ))


;; Interface

(defvar ensime-message-function 'message)

(defun ensime-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
	(if (fboundp 'temp-minibuffer-message) ;; XEmacs
	    (temp-minibuffer-message text)
	  (minibuffer-message text))
      (message "%s" text))))

(defun ensime-message (format &rest args)
  "Like `message' but with special support for multi-line messages.
Single-line messages use the echo area."
  (apply ensime-message-function format args))

(defun ensime-display-warning (message &rest args)
  (display-warning '(ensime warning) (apply #'format message args)))

(defvar ensime-background-message-function 'ensime-display-oneliner)


(defun ensime-background-message (format-string &rest format-args)
  "Display a message in passing.
This is like `ensime-message', but less distracting because it
will never pop up a buffer or display multi-line messages.
It should be used for \"background\" messages such as argument lists."
  (apply ensime-background-message-function format-string format-args))

(defun ensime-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (message  "%s" (ensime-oneliner msg)))))

(defun ensime-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
			   (or (position ?\n string) most-positive-fixnum)
			   (1- (frame-width)))))



;; Data-structure accessors

(defun ensime-search-sym-name (sym)
  (plist-get sym :name))

(defun ensime-search-sym-local-name (sym)
  (plist-get sym :local-name))

(defun ensime-search-sym-pos (sym)
  (plist-get sym :pos))

(defun ensime-search-sym-owner-name (sym)
  (plist-get sym :owner-name))

(defun ensime-search-sym-decl-as (sym)
  (plist-get sym :decl-as))

(defun ensime-symbol-name (sym)
  (plist-get sym :name))

(defun ensime-symbol-decl-pos (sym)
  (plist-get sym :decl-pos))

(defun ensime-symbol-type (sym)
  (plist-get sym :type))

(defun ensime-symbol-is-callable (sym)
  (plist-get sym :is-callable))

(defun ensime-symbol-owner-type-id (sym)
  (plist-get sym :owner-type-id))

(defun ensime-package-name (info)
  (plist-get info :name))

(defun ensime-package-full-name (info)
  (plist-get info :full-name))

(defun ensime-package-members (info)
  (plist-get info :members))

(defun ensime-package-p (info)
  (equal 'package (plist-get info :info-type)))

(defun ensime-type-inspection-p (info)
  (equal 'typeInspect (plist-get info :info-type)))

(defun ensime-type-name (type)
  (plist-get type :name))

(defun ensime-type-name-with-args (type)
  (concat (plist-get type :name)
	  (ensime-type-type-args-postfix type)))

(defun ensime-type-id (type)
  (plist-get type :type-id))

(defun ensime-type-is-object-p (type)
  (equal (plist-get type :decl-as) 'object))

(defun ensime-outer-type-id (type)
  (plist-get type :outer-type-id))

(defun ensime-type-full-name (type)
  (if (plist-get type :arrow-type)
      (plist-get type :name)
    (plist-get type :full-name)))

(defun ensime-type-full-name-with-args (type)
  (if (plist-get type :arrow-type)
      (plist-get type :name)
    (concat
     (plist-get type :full-name)
     (ensime-type-type-args-postfix type))))

(defun ensime-type-type-args-postfix (type)
  (let ((args (ensime-type-type-args type)))
    (if args
	(concat "["
		(mapconcat
		 (lambda(tpe)
		   (ensime-type-name-with-args tpe)) args ", ")
		"]")
      "")))

(defun ensime-declared-as (obj)
  (plist-get obj :decl-as))

(defun ensime-declared-as-str (obj)
  (case (plist-get obj :decl-as)
    (method "method")
    (trait "trait")
    (interface "interface")
    (class "class")
    (object "object")
    (otherwise "type")
    ))

(defun ensime-type-is-arrow-p (type)
  (plist-get type :arrow-type))

(defun ensime-type-param-sections (type)
  (plist-get type :param-sections))

(defun ensime-type-param-types (type)
  "Return types of params in first section."
  (let ((section (car (plist-get type :param-sections))))
    (mapcar
     (lambda (p)
       (cadr p))
     (plist-get section :params)
     )))

(defun ensime-type-result-type (type)
  (plist-get type :result-type))

(defun ensime-type-type-args (type)
  (plist-get type :type-args))

(defun ensime-member-name (member)
  (plist-get member :name))

(defun ensime-member-type (member)
  (plist-get member :type))

(defun ensime-member-pos (member)
  (plist-get member :pos))

(defun ensime-pos-file (pos)
  (plist-get pos :file))

(defun ensime-pos-offset (pos)
  (plist-get pos :offset))

(defun ensime-pos-line (pos)
  (plist-get pos :line))

(defun ensime-pos-valid-local-p (pos)
  (and (stringp (ensime-pos-file pos))
       (file-exists-p (ensime-pos-file pos))
       (integerp (ensime-pos-offset pos))
       (integerp (ensime-pos-offset pos))))

(defun ensime-note-file (note)
  (plist-get note :file))

(defun ensime-note-beg (note)
  (plist-get note :beg))

(defun ensime-note-end (note)
  (plist-get note :end))

(defun ensime-note-line (note)
  (plist-get note :line))

(defun ensime-note-message (note)
  (plist-get note :msg))

;; Portability

(defun ensime-computed-point ()
  "Subtract one to convert to 0-indexed buffer offsets.
 Additionally, in buffers with windows-encoded line-endings,
 add the appropriate number of CRs to compensate for characters
 that are hidden by Emacs."
  (ensime-externalize-offset (point)))

(defun ensime-externalize-offset (offset)
  (+ offset (- ensime-ch-fix)
     (if (eq 1 (coding-system-eol-type buffer-file-coding-system))
	 (- (line-number-at-pos offset) 1)
       0)
     ))

(defun ensime-internalize-offset (offset)
  (+ offset ensime-ch-fix))

(defun ensime-internalize-offset-fields (plist &rest keys)
  (dolist (key keys)
    (setq plist (plist-put
		 plist key
		 (ensime-internalize-offset
		  (plist-get plist key)))))
  plist)

;; Popup Buffer

;;;;; Temporary popup buffers

(defvar ensime-popup-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'ensime-popup-buffer-quit-function)
    (define-key map [mouse-1] 'push-button)
    map)
  "Keymap for `ensime-popup-buffer-mode'.")

(define-minor-mode ensime-popup-buffer-mode
  "Mode for displaying read only stuff"
  nil
  nil
  (make-sparse-keymap))

(add-to-list 'minor-mode-alist
	     '(ensime-popup-buffer-mode (:eval (ensime-modeline-string))))

(defvar ensime-popup-restore-data nil
  "Data needed when closing popup windows.
This is used as buffer local variable.
The format is (POPUP-WINDOW SELECTED-WINDOW OLD-BUFFER).
POPUP-WINDOW is the window used to display the temp buffer.
That window may have been reused or freshly created.
SELECTED-WINDOW is the window that was selected before displaying
the popup buffer.
OLD-BUFFER is the buffer that was previously displayed in POPUP-WINDOW.
OLD-BUFFER is nil if POPUP-WINDOW was newly created.

See `view-return-to-alist' for a similar idea.")

(make-variable-buffer-local
 (defvar ensime-is-popup-buffer nil
   "So we can query later whether this is a popup buffer."))

;; Interface
(defmacro* ensime-with-popup-buffer ((name &optional connection select)
				     &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
CONNECTION is the value for `ensime-buffer-connection'.
If nil, no explicit connection is associated with
the buffer.  If t, the current connection is taken.
"
  `(let* ((vars% (list ,(if (eq connection t) '(ensime-connection) connection)))
	  (standard-output (ensime-make-popup-buffer ,name vars%)))
     (with-current-buffer standard-output
       (prog1
	   (progn
	     ,@body)
	 (assert (eq (current-buffer) standard-output))
	 (setq buffer-read-only t)
	 (set-window-point (ensime-display-popup-buffer ,(or select 'nil))
			   (point))))))


(defun ensime-make-popup-buffer (name buffer-vars)
  "Return a temporary buffer called NAME.
The buffer also uses the minor-mode `ensime-popup-buffer-mode'."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-syntax-table lisp-mode-syntax-table)
    (ensime-init-popup-buffer buffer-vars)
    (use-local-map ensime-popup-buffer-map)
    (setq ensime-is-popup-buffer t)
    (current-buffer)))

(defun ensime-init-popup-buffer (buffer-vars)
  (ensime-popup-buffer-mode 1)
  (multiple-value-setq (ensime-buffer-connection)
    buffer-vars))

(defun ensime-display-popup-buffer (select)
  "Display the current buffer.
   Save the selected-window in a buffer-local variable, so that we
   can restore it later."
  (let ((selected-window (selected-window))
	(old-windows))
    (walk-windows (lambda (w)
		    (if (not (ensime-popup-buffer-p (window-buffer w)))
			(push (cons w (window-buffer w)) old-windows)))
		  nil t)
    (let ((new-window
	   (cond
	    (ensime-popup-in-other-frame
	     (display-buffer-other-frame (current-buffer)))
	    (t (display-buffer (current-buffer))))))
      (unless ensime-popup-restore-data
	(set (make-local-variable 'ensime-popup-restore-data)
	     (list new-window
		   selected-window
		   (cdr (find new-window old-windows :key #'car)))))
      (when select
	(select-window new-window))
      new-window)))

(defun ensime-close-popup-window ()
  (when ensime-popup-restore-data
    (destructuring-bind (popup-window selected-window old-buffer)
	ensime-popup-restore-data
      (kill-local-variable 'ensime-popup-restore-data)
      (bury-buffer)
      (when (eq popup-window (selected-window))
	(cond ((and (not old-buffer) (not (one-window-p)))
	       (delete-window popup-window))
	      ((and old-buffer (buffer-live-p old-buffer))
	       (set-window-buffer popup-window old-buffer))
	      ))
      (when (window-live-p selected-window)
	(select-window selected-window)))
    ))


(defmacro ensime-save-local-variables (vars &rest body)
  (let ((vals (make-symbol "vals")))
    `(let ((,vals (mapcar (lambda (var)
			    (if (ensime-local-variable-p var)
				(cons var (eval var))))
			  ',vars)))
       (prog1 (progn . ,body)
	 (mapc (lambda (var+val)
		 (when (consp var+val)
		   (set (make-local-variable (car var+val)) (cdr var+val))))
	       ,vals)))))


(make-variable-buffer-local
 (defvar ensime-popup-buffer-quit-function 'ensime-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

(defun ensime-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the value of `ensime-popup-buffer-quit-function'."
  (interactive)
  (funcall ensime-popup-buffer-quit-function kill-buffer-p))

(defun ensime-popup-buffer-quit (&optional kill-buffer-p)
  "Get rid of the current (temp) buffer without asking.
  Restore the window configuration unless it was changed since we
  last activated the buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (ensime-close-popup-window)
    (when kill-buffer-p
      (kill-buffer buffer))))


;;;;; Connection listing

(define-derived-mode ensime-connection-list-mode fundamental-mode
  "Ensime-Connections"
  "ENSIME Connection List Mode.

\\{ensime-connection-list-mode-map}
\\{ensime-popup-buffer-mode-map}"
  (when ensime-truncate-lines
    (set (make-local-variable 'truncate-lines) t)))

(ensime-define-keys ensime-connection-list-mode-map
		    ("g"         'ensime-update-connection-list)
		    ((kbd "C-k") 'ensime-quit-connection-at-point)
		    ("R"         'ensime-restart-connection-at-point))

(defun ensime-connection-at-point ()
  (or (get-text-property (point) 'ensime-connection)
      (error "No connection at point")))

(defun ensime-quit-connection-at-point (connection)
  (interactive (list (ensime-connection-at-point)))
  (ensime-quit-connection connection)
  (ensime-update-connection-list))

(defun ensime-quit-connection (connection)
  (ensime-rpc-shutdown-server)
  (let ((end (time-add (current-time) (seconds-to-time 3))))
    (while (memq connection ensime-net-processes)
      (when (time-less-p end (current-time))
	(message "Quit timeout expired.  Disconnecting.")
	(delete-process connection))
      (sit-for 0 100))
    ))

(defun ensime-restart-connection-at-point (connection)
  (interactive (list (ensime-connection-at-point)))
  (let ((ensime-dispatching-connection connection))
    (ensime-restart-inferior-lisp)))


(defvar ensime-connections-buffer-name "*ENSIME Connections*")

(defun ensime-list-connections ()
  "Display a list of all connections."
  (interactive)
  (ensime-with-popup-buffer (ensime-connections-buffer-name)
			    (ensime-connection-list-mode)
			    (ensime-draw-connection-list)))

(defun ensime-update-connection-list ()
  "Display a list of all connections."
  (interactive)
  (let ((pos (point))
	(inhibit-read-only t))
    (erase-buffer)
    (ensime-draw-connection-list)
    (goto-char pos)))

(defun ensime-draw-connection-list ()
  (let ((default-pos nil)
	(fstring "%s%2s  %-10s  %-17s  %-7s %-s\n"))
    (insert (format fstring " " "Nr" "Name" "Port" "Pid" "Type")
	    (format fstring " " "--" "----" "----" "---" "----"))
    (dolist (p (reverse ensime-net-processes))
      (ensime-insert-propertized
       (list 'ensime-connection p)
       (format fstring
	       " "
	       (ensime-connection-number p)
	       (ensime-connection-name p)
	       (or (process-id p) (process-contact p))
	       (ensime-pid p)
	       (ensime-server-implementation-type p))))
    ))




;; Interface Helpers

(defmacro ensime-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
	 (add-text-properties ,start (point) ,props)))))

(defun ensime-add-face (face string)
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

(defsubst ensime-insert-propertized (props &rest args)
  "Insert all ARGS and then add text-PROPS to the inserted text."
  (ensime-propertize-region props (apply #'insert args)))

(defmacro ensime-with-rigid-indentation (level &rest body)
  "Execute BODY and then rigidly indent its text insertions.
Assumes all insertions are made at point."
  (let ((start (gensym)) (l (gensym)))
    `(let ((,start (point)) (,l ,(or level '(current-column))))
       (prog1 (progn ,@body)
	 (ensime-indent-rigidly ,start (point) ,l)))))

(put 'ensime-with-rigid-indentation 'lisp-indent-function 1)

(defun ensime-indent-rigidly (start end column)
  ;; Similar to `indent-rigidly' but doesn't inherit text props.
  (let ((indent (make-string column ?\ )))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (and (<= start (point))
		  (progn
		    (insert-before-markers indent)
		    (zerop (forward-line -1))))))))

(defun ensime-insert-indented (&rest strings)
  "Insert all arguments rigidly indented."
  (ensime-with-rigid-indentation nil
    (apply #'insert strings)))

(defun ensime-property-bounds (prop)
  "Return two the positions of the previous and next changes to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun ensime-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)




;; Testing helpers

(defun ensime-event-sig (event &optional value)
  "Signal an event. Send to testing harness if it exists.
   Used to drive asynchronous regression tests."
  (if (fboundp 'ensime-test-sig)
      (ensime-test-sig event value)))



(provide 'ensime)
