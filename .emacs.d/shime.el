;;; shime.el --- Superior Haskell Integration Mode for Emacs
;;
;; Copyright (c) 2010, Chris Done
;; All rights reserved. See below for license.
;;
;;; Commentary:
;;
;; A major mode for interacting with a Haskell inferior process.
;;
;; * Currently only supports GHCi.
;; * Not tested on OS X or Windows. Should work on both.
;;
;;; License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the
;; following conditions are met:
;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the
;;       following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the
;;       following disclaimer in the documentation and/or other
;;       materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL Chris Done BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
;; THE POSSIBILITY OF SUCH DAMAGE.
;;
;; Installation
;;
;; - Place `shime.el' in your emacs load path.
;; - Add the following line to your .emacs file:
;;   (autoload 'shime "shime" nil t)
;;
;; To customize shime:
;;  M-x customize-group RET shime RET

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar shime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'shime-key-ret)
    (define-key map (kbd "TAB") 'shime-key-tab)
    (define-key map (kbd "DEL") 'shime-key-del)
    (define-key map (kbd "C-j") 'shime-key-ret)
    (define-key map (kbd "M-p") 'shime-key-history-prev)
    (define-key map (kbd "M-n") 'shime-key-history-next)
    (define-key map (kbd "C-a") 'shime-key-home)
    map)
  "Shime mode map.")

(define-derived-mode shime-mode nil "Shime"
  "Superior Haskell Integration Mode for Emacs.

\\{shime-mode-map}"
  :group 'shime
  (setq next-error-function 'shime-next-error-function)
  (make-local-variable 'shime-error-positions)
  (make-local-variable 'shime-current-error-index))

(add-hook 'shime-mode-hook 'shime-set-cabal-commands)

;; Customization

(defcustom shime-default-ghci-path "ghci"
  "Default GHCi path."
  :group 'shime
  :type 'string)

(defcustom shime-default-shell-path "bash"
  "Default Cabal path."
  :group 'shime
  :type 'string)

(defcustom shime-cabal-program-path "cabal"
  "Default Cabal path."
  :group 'shime
  :type 'string)

(defcustom shime-default-language "en"
  "Default language."
  :group 'shime
  :type 'string)

(defcustom shime-default-session-name "shime"
  "Default session name."
  :group 'shime
  :type 'string)

(defcustom shime-ghci-prompt-regex "^[^ \r\n\t]*> "
  "Regex to match the prompt string."
  :group 'shime
  :type 'string)

(defcustom shime-collapse-errors nil
  "Collapse GHCi errors onto a single line."
  :group 'shime
  :type 'boolean)

(defcustom shime-collapse-packages t
  "Collapse GHCi package loads onto a single line."
  :group 'shime
  :type 'boolean)

(defcustom shime-show-package-versions nil
  "Show Hackage version numbers for
packages (e.g. array-0.3.0.1)."
  :group 'shime
  :type 'boolean)

(defcustom shime-display-dir-on-load nil
  "Display the directory of the file name being loaded."
  :group 'shime
  :type 'boolean)

(defcustom shime-jump-to-first-error t
  "Jump to the first error in a Shime buffer."
  :group 'shime
  :type 'boolean)

(defcustom shime-prompt-hook nil
  "Hook to run at each prompt in the Shime buffer."
  :group 'shime
  :type 'hook)
(add-hook 'shime-prompt-hook 'shime-maybe-goto-error)
;; Constants

(defvar shime-strings-en "English language strings.")
(setq shime-strings-en
      `((process-died . "The Shime process died. Restart it? ")
        (program-not-found
         . "Unable to find Shime program '%s', what's the right path? ")
        (could-not-start . "Shime could not start.")
        (enter-session-name . "Session name: ")
        (kill-session . "Kill Shime session: ")
        (kill-process . "Kill Shime process: ")
        (kill-buffer . "Kill Shime buffer: ")
        (start-shime . "Start Shime? ")
        (ghci-died-restart?
         . "The GHCi process '%s' ended. Restart it? ")
        (restarting-ghci-process . "Restarting GHCi process...")
        (buffer-no-processes . "No processes attached to this buffer!")
        (choose-buffer-process . "Choose buffer process: ")
        (ask-change-root . "Do you want to change the root directory? ")
        (new-load-root . "New load root: ")
        (new-cabal-root . "New Cabal root: ")
        (choose-session . "Choose session: ")
        (cabal-command-finished . "Cabal command finished.")
        (choose-buffer-ghci-process . "Choose GHCi process: ")
        (choose-buffer-cabal-process . "Choose Cabal process: ")
        (needed-a-session . "The command needed a Shime session. Aborted.")
        (buffer-session-was-set . "Buffer session set to: %s")
        (buffer-session-was-set-default
         . ,(concat "Buffer session set to (default): %s"
                   " (Use `M-x shime-choose-buffer-session` to change.)"))
        (buffer-ghci-process-was-set.  "Buffer GHCi process set to: %s")
        (buffer-ghci-process-was-set-default
         . ,(concat "Buffer GHCi process set to (default): %s" 
                   " (Use `M-x shime-choose-buffer-ghci-process` to change.)"))
        (buffer-cabal-process-was-set
         . "Buffer Cabal process set to: %s")
        (buffer-cabal-process-was-set-default
         . ,(concat "Buffer Cabal process set to (default): %s"
                 " (Use `M-x shime-choose-buffer-cabal-process` to change.)"))
        (choose-buffer-session . "Choose session for this buffer: ")
        (enter-session-name-exists
         . "Session already exists, please enter a different session name: ")
        (session-already-started
         . "Shime session(s) already started. Start a new session? ")
        (recieved-data-from-rogue-process
         . "Recieved data from rogue process %s")
        (recieved-data-from-unattached-process
         . "Recieved data from unattached process %s")
        (recieved-data-for-inactive-session
         .  "Recieved data from process %s on inactive session %s")))

(defvar shime-languages
  "All the available languages. Re-evaluate this when
 updating an individual language at runtime.")
(setq shime-languages `(("en" . ,shime-strings-en)))

(defvar shime-cabal-commands
  '("install"
    "update"
    "list"
    "info"
    "upgrade"
    "fetch"
    "unpack"
    "check"
    "sdist"
    "upload"
    "report"
    "init"
    "configure"
    "build"
    "copy"
    "haddock"
    "clean"
    "hscolour"
    "register"
    "test"
    "help"))

(defun shime-set-cabal-commands ()
  "Parse 'cabal --help' and set `shime-cabal-commands'.
If cabal doesn't exist, `shime-cabal-commands' is left
unchanged."
  (with-temp-buffer
    (insert (shell-command-to-string (concat
                                      shime-cabal-program-path
                                      " --help")))
    (goto-char (point-min))

    (when (re-search-forward "cabal" nil t 2)
      (narrow-to-region (re-search-forward "Commands:\n")
                        (re-search-forward "^\n"))
      (goto-char (point-min))

      (let (cmds)
        (while (re-search-forward "^  \\([a-z]+\\)" nil t)
          (push (match-string 1) cmds))
        (when cmds (setq shime-cabal-commands (reverse cmds)))))))

;; Globals

(defvar shime-sessions '()
  "List of sessions.")

(defvar shime-processes '()
  "List of Shime processes.")

(defvar shime-buffers '()
  "List of Shime buffers.")

;; Buffer Local variables

(defvar shime-session-of-buffer nil
  "The Shime session associated with the current buffer.")
(make-variable-buffer-local 'shime-session-of-buffer)

(defvar shime-cabal-process-of-buffer nil
  "The Shime cabal process associated with the current buffer.")
(make-variable-buffer-local 'shime-cabal-process-of-buffer)

(defvar shime-ghci-process-of-buffer nil
  "A buffer local variable of the associated GHCi process.")
(make-variable-buffer-local 'shime-ghci-process-of-buffer)

;; Data types

(defstruct
  shime-config
  "Config options for a Shime session."
  language 
  name)

(defstruct
  shime-session
  "Session information."
  name
  config
  processes
  buffers
  active-p)

(defstruct
  shime-process
  "Process information, usually for cabal or GHCi."
  program-path
  name
  session
  filter
  sentinel
  process
  buffer
  type
  pwd
  data
  block-data
  block-state)

(defstruct
  shime-buffer
  "Buffer information."
  name
  buffer
  session
  processes
  ghci-process)

(defun shime-make-session (name config)
  "Make a Session object."
  (let ((session (make-shime-session
                  :config config
                  :name name
                  :processes '()
                  :buffers '()
                  :active-p nil)))
    (if (assoc name shime-sessions)
        (error "Unable to make Shime session named %s, already exists." name)
      (progn (add-to-list 'shime-sessions (cons name session))
             session))))

(defun shime-make-config (name)
  "Make a Shime config object."
  (make-shime-config :name name))

(defun shime-start-process-for-shime-process (process)
  "Start a process using the details given in the shime-process
object and attach itself to it."
  (let* ((process-connection-type nil)
         (path (shime-executable-find (shime-process-program-path process)))
         (process-ref (start-process (shime-process-name process) nil path)))
    (set-process-filter process-ref (shime-process-filter process))
    (set-process-sentinel process-ref (shime-process-sentinel process))
    (setf (shime-process-process process) process-ref)))

(defun shime-make-process (session name program-path filter sentinel type pwd)
  "Make a Shime process object."
  (let ((process (make-shime-process
                  :program-path program-path
                  :name name
                  :session session
                  :filter filter
                  :sentinel sentinel
                  :process nil
                  :type type
                  :pwd pwd
                  :data ""
                  :block-data ""
                  :block-state nil)))
    (shime-start-process-for-shime-process process)
    (add-to-list 'shime-processes (cons name process))
    process))

(defun shime-make-buffer (session name)
  "Make a Shime buffer object associated with a session."
  (if (get-buffer name)
      ;; TODO: Look up to see if there is an existing Shime
      ;; session for that buffer, if not, offer to delete or
      ;; usurp the buffer.
      (error "Unable to make Shime buffer named %s, already exists"
             name)
    (let ((buffer (make-shime-buffer
                   :name name
                   :buffer (get-buffer-create name)
                   :session session
                   :processes '()
                   :ghci-process nil)))
      (add-to-list 'shime-buffers (cons name buffer))
      (with-current-buffer (shime-buffer-buffer buffer)
        (shime-mode))
      buffer)))

;; Interactive procedures

;;;###autoload
(defun shime ()
  "Start a Shime session."
  (interactive)
  (if (null shime-sessions)
      (shime-start-session
       :name shime-default-session-name
       :config (shime-make-config shime-default-session-name))
    (shime-maybe-start-session)))

(defun shime-start-named-session ()
  "Start a session with a given name."
  (interactive)
  (let ((name (shime-prompt-for-session-name)))
    (shime-start-session
     :name name
     :config (shime-make-config name))))

(defun shime-kill-session ()
  "Kill a Shime session and all associated processes and buffers."
  (interactive)
  (shime-kill-session-by-name
   (ido-completing-read (shime-string 'kill-session)
                        (mapcar 'car shime-sessions))))

(defun shime-kill-buffer ()
  "Kill a Shime buffer."
  (interactive)
  (shime-kill-buffer-by-name
   (ido-completing-read (shime-string 'kill-buffer)
                        (mapcar 'car shime-buffers))))

(defun shime-kill-process ()
  "Kill a Shime process."
  (interactive)
  (shime-kill-process-by-name
   (ido-completing-read (shime-string 'kill-process)
                        (mapcar 'car shime-processes))))

(defun shime-choose-load-root ()
  "Prompt to set the root load path (defaults to current directory)."
  (interactive)
  (shime-with-buffer-ghci-process process
    (shime-prompt-load-root process (shime-process-pwd process))))

(defun shime-choose-cabal-root ()
  "Prompt to set the root Cabal path (defaults to current directory)."
  (interactive)
  (shime-with-buffer-ghci-process process
    (shime-prompt-cabal-root process "")))

(defun shime-cabal-configure ()
  "Run the Cabal configure command."
  (interactive)
  (shime-cabal-command "configure"))

(defun shime-cabal-build ()
  "Run the Cabal build command."
  (interactive)
  (shime-cabal-command "build"))

(defun shime-cabal-clean ()
  "Run the Cabal clean command."
  (interactive)
  (shime-cabal-command "clean"))

(defun shime-cabal-install ()
  "Run the Cabal install command."
  (interactive)
  (shime-cabal-command "install"))

(defun shime-cabal-ido (&optional custom)
  "Interactively choose a cabal command to run."
  (interactive "P")
  (let ((command (ido-completing-read "Command: " shime-cabal-commands)))
    (if custom
        (shime-cabal-command (read-from-minibuffer "Command: " command))
      (shime-cabal-command command))))

(defun shime-format (key &rest args)
  "Lookup Shime string KEY and format it with ARGS."
  (apply 'format (shime-string key) args))

(defun shime-message (key &rest args)
  "Lookup the Shime string KEY and format it with ARGS."
  (message (shime-format key args)))

(defun shime-choose-buffer-session-or-default ()
  "Choose the session for this buffer or just default if there's only one session."
  (interactive)
  (shime-with-any-session
   (if (= (length shime-sessions) 1)
       (progn (setq shime-session-of-buffer (car (car shime-sessions)))
              (shime-message 'buffer-session-was-set-default
                             shime-session-of-buffer)
              shime-session-of-buffer)
     (shime-choose-buffer-session))))

(defun shime-choose-buffer-session ()
  "Choose the session for this buffer."
  (interactive)
  (shime-with-any-session
   (setq shime-session-of-buffer
         (ido-completing-read (shime-string 'choose-buffer-session)
                              (mapcar 'car shime-sessions))))
  (shime-message 'buffer-session-was-set shime-session-of-buffer)
  shime-session-of-buffer)

(defun shime-choose-buffer-ghci-process-or-default ()
  "Choose the buffer for this buffer or just default if there's only one buffer."
  (interactive)
  (shime-with-session session
    (if (= (length (shime-session-ghci-processes session)) 1)
        (progn (setq shime-ghci-process-of-buffer
                     (shime-process-name
                      (car (shime-session-ghci-processes session))))
               (shime-message 'buffer-ghci-process-was-set-default
                               shime-ghci-process-of-buffer)
               shime-ghci-process-of-buffer)
      (shime-choose-buffer-ghci-process))))

(defun shime-choose-buffer-ghci-process ()
  "Choose the buffer for this buffer."
  (interactive)
  (shime-with-session session
    (setq shime-ghci-process-of-buffer
          (ido-completing-read (shime-string 'choose-buffer-ghci-process)
                               (mapcar 'shime-process-name
                                       (shime-session-ghci-processes session)))))
  (shime-message 'buffer-ghci-process-was-set shime-ghci-process-of-buffer)
  shime-ghci-process-of-buffer)

(defun shime-choose-buffer-cabal-process-or-default ()
  "Choose the buffer for this buffer or just default if there's only one buffer."
  (interactive)
  (shime-with-session session
    (if (= (length (shime-session-cabal-processes session)) 1)
        (progn (setq shime-cabal-process-of-buffer
                     (shime-process-name
                      (car (shime-session-cabal-processes session))))
               (shime-message 'buffer-cabal-process-was-set-default
                              shime-cabal-process-of-buffer)
               shime-cabal-process-of-buffer)
      (shime-choose-buffer-cabal-process))))

(defun shime-choose-buffer-cabal-process ()
  "Choose the buffer for this buffer."
  (interactive)
  (shime-with-session session
    (setq shime-cabal-process-of-buffer
          (ido-completing-read (shime-string 'choose-buffer-cabal-process)
                               (mapcar 'shime-process-name
                                       (shime-session-cabal-processes session)))))
  (shime-message 'buffer-cabal-process-was-set
                 shime-cabal-process-of-buffer)
  shime-cabal-process-of-buffer)

(defun shime-cabal-command (cmd)
  "Run cabal CMD."
  (interactive)
  (shime-with-buffer-cabal-process process
    (when (buffer-modified-p) (save-buffer))
    (if (shime-process-pwd process)
        (shime-cabal-send-cmd process cmd)
      (shime-prompt-cabal-root process
                               (file-name-directory (buffer-file-name)))
      (shime-cabal-command cmd))))

(defun shime-echo-command (buffer str)
  "Insert STR into BUFFER with the shime-interactive-command face."
  (with-current-buffer (shime-buffer-buffer buffer)
    ;; Don't overwrite the prompt because it delimits error messages.
    ;; We use it for jump to first error.
    (insert "\n")
    (shime-delete-line)
    (shime-buffer-echo buffer
                       (propertize str 'face 'shime-interactive-command))))

(defun shime-load-file ()
  "Load the file associated with the current buffer with the
  current session GHCi process."
  (interactive)
  (shime-with-buffer-ghci-process process
    (let* ((file (buffer-file-name))
           (file-dir (file-name-directory file))
           (proc-buffer (shime-process-buffer process)))
      (when (buffer-modified-p) (save-buffer))
      (if (shime-process-pwd process)
          (progn
            (unless (shime-relative-to (shime-process-pwd process) file-dir)
              (when (shime-ask-change-root)
                (shime-prompt-load-root process file-dir)))
            (let ((file-load-display
                   (if (shime-relative-to (shime-process-pwd process) file-dir)
                       (if shime-display-dir-on-load
                           file
                         (shime-path-filename file))
                     file)))

              (with-current-buffer (shime-buffer-buffer proc-buffer)
                (goto-char (point-max)))

              (shime-echo-command proc-buffer
                                  (format "load %s\n" file-load-display)))
            (shime-ghci-send-expression process (concat ":load " file)))
        (shime-set-load-root process file-dir)
        (shime-load-file)))))

(defun shime-reset-everything-because-it-broke ()
  "Reset everything because it broke."
  (interactive)
  (setq shime-sessions nil
        shime-buffers nil
        shime-processes nil))

;; Key binding handlers

(defun shime-key-home ()
  "Handle the home key press."
  (interactive)
  (goto-char (line-beginning-position))
  (search-forward-regexp shime-ghci-prompt-regex
                         (line-end-position)
                         t
                         1))
(defun shime-key-ret ()
  "Handle the return key press."
  (interactive)
  (if (shime-at-error)
      (shime-goto-error)
    (shime-with-buffer-ghci-process process
      (let ((proc-buffer (shime-process-buffer process))
            (prompt-p (save-excursion
                        (goto-char (line-beginning-position))
                        (looking-at shime-ghci-prompt-regex)))
            (line (buffer-substring-no-properties
                   (match-end 0)
                   (line-end-position))))
        (if prompt-p
            (progn
              (shime-history-ensure-created)
              (unless (string= "" line)
                (push line shime-history-of-buffer)
                (setq shime-history-index-of-buffer -1))
              (shime-buffer-ghci-send-expression proc-buffer process line))
          ;; If we're not at a prompt, send an empty line to
          ;; the REPL, this'll trigger it sending a new prompt,
          ;; which is probably what we want. At least in the
          ;; case of M-x erase buffer.
          ;; TODO: take another look at this to re-evaluate.
          (shime-buffer-ghci-send-expression proc-buffer process ""))))))

(defun shime-key-tab ()
  "Handle the tab key press."
  (interactive)
  ;; Do something.
  )

(defun shime-key-del ()
  "Handle the backspace key press."
  (interactive)
  (unless (looking-back shime-ghci-prompt-regex (line-beginning-position))
    (backward-delete-char 1)))

(defun shime-key-history-prev ()
  "Show previous history item."
  (interactive)
  (shime-history-toggle 1))

(defun shime-key-history-next ()
  "Show previous history item."
  (interactive)
  (shime-history-toggle -1))

;; Macros

(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, and if the result is non-nil bind it to VAR and
evaluate BODY.

\(fn (VAR VALUE) &rest BODY)"
  (declare (indent 1))
  `(let ((,var ,value))
     (when ,var ,@body)))


(defmacro* if-let ((var value) then &rest else)
  "Evaluate VALUE, and if the result is non-nil bind it to VAR and
evaluate THEN, else evaluate ELSE.

\(fn (VAR VALUE) THEN &rest ELSE)"
  (declare (indent 2))
  `(let ((,var ,value))
     (if ,var
         ,then
       ,@else)))

(defun shime-split-string-with-newlines (string &optional separators omit-nulls)
  "`split-string', but append \"\n\" to each split but the last."
  (let ((split (split-string string separators omit-nulls)))
    (append (mapcar (lambda (str)
                      (concat str "\n"))
                    (butlast split))
            (last split))))

(defmacro shime-with-process-buffered-lines (process input line-name &rest body)
  "Parse INPUT from PROCESS and run BODY on LINE-NAME."
  (declare (indent 3))
  (let ((lines (make-symbol "lines-sym"))
        (parsed-lines (make-symbol "parsed-lines-sym"))
        (last-line (make-symbol "last-line-sym")))
    `(let* ((buffer (shime-process-buffer ,process))
            ;; Grab stored text and the new input.
            (full-lines (concat (shime-process-data ,process)
                                ,input))
            ;; Keep newlines in the string so we know with certainity
            ;; what GHCi returned.  Otherwise, we screw up expressions
            ;; like `putStrLn "a\n\nb"' by collapsing the two
            ;; newlines.
            (,lines (shime-split-string-with-newlines
                     full-lines
                     "[\r\n]"
                     nil))
            (,last-line (car (or (last ,lines) '("")))))
       (with-current-buffer (shime-buffer-buffer buffer)
         (mapc (lambda (,line-name) ,@body)
               (if (string-match shime-ghci-prompt-regex ,last-line)
                   ;; We see the prompt, so we reset the process data
                   ;; and pass the whole input to ,@body
                   (progn
                     (setf (shime-process-data ,process) "")
                     ,lines)
                 ;; We don't see the prompt, so there might still be
                 ;; output from GHCi.  We store the last line until
                 ;; next time.
                 (setf (shime-process-data ,process) ,last-line)
                 (butlast ,lines)))))))

(defmacro shime-with-process-session (process process-name session-name &rest body)
  "Get the process object and session for a processes."
  (declare (indent 3))
  `(if-let (process (assoc (process-name ,process) shime-processes))
       (if-let (session (shime-process-session (cdr process)))
           (if (not (shime-session-active-p session))
               (shime-message 'recieved-data-for-inactive-session
                              (process-name ,process) "")
             (let ((,session-name session)
                   (,process-name (cdr process)))
               ,@body))
         (shime-message 'recieved-data-from-unattached-process
                        (process-name ,process)))
     (shime-message 'recieved-data-from-rogue-process
                    (process-name ,process))))

(defmacro shime-with-any-session (&rest body)
  "The code this call needs a session. Ask to create one if needs be."
  (declare (indent 0))
  `(if (null shime-sessions)
       (if (y-or-n-p (shime-string 'start-shime))
           (progn (shime)
                  ,@body)
         (shime-message 'needed-a-session))
     ,@body))

(defmacro shime-with-session (name &rest body)
  "The code this call needs a session. Ask to create one if needs be."
  (declare (indent 1))
  `(shime-with-any-session
    (if (= 1 (length shime-sessions))
        (let ((,name (cdar shime-sessions)))
          ,@body)
      (let ((,name (assoc (shime-choose-session) shime-sessions)))
        (if ,name
            (let ((,name (cdr ,name)))
              ,@body)
          (shime-message 'needed-a-session))))))

;; TODO: Maybe a bit more interactivity.
(defmacro shime-with-buffer-ghci-process (name &rest body)
  (declare (indent 1))
  (let ((sym (make-symbol "sym-sym")) (cons (make-symbol "cons-sym")))
    `(when-let (,sym (shime-get-buffer-ghci-process))
       (when-let (,cons (assoc ,sym shime-processes))
         (let ((,name (cdr ,cons)))
           ,@body)))))

;; TODO: Maybe a bit more interactivity.
(defmacro shime-with-buffer-cabal-process (name &rest body)
  (declare (indent 1))
  (let ((sym (make-symbol "sym-sym")) (cons (make-symbol "cons-sym")))
    `(when-let (,sym (shime-get-buffer-cabal-process))
       (when-let (,cons (assoc ,sym shime-processes))
         (let ((,name (cdr ,cons)))
           ,@body)))))

;; Procedures

(defun shime-history-ensure-created ()
  "Ensure the local variable for history is created."
  (unless (default-boundp 'shime-history-of-buffer)
    (setq shime-history-of-buffer '(""))
    (setq shime-history-index-of-buffer 0)
    (make-local-variable 'shime-history-of-buffer)
    (make-local-variable 'shime-history-index-of-buffer)))

(defun shime-history-toggle (direction)
  "Toggle the prompt contents by cycling the history."
  (shime-history-ensure-created)
  (setq shime-history-index-of-buffer
        (+ shime-history-index-of-buffer
           direction))
  (shime-buffer-clear-prompt)
  (insert (nth (mod shime-history-index-of-buffer
                    (length shime-history-of-buffer))
               shime-history-of-buffer)))

(defun shime-buffer-clear-prompt ()
  "Clear the current prompt."
  ;; TODO: Maybe check that we're in an actual Shime buffer, in
  ;; case a maurading fool enters and splashes water over
  ;; everyone's dogs.
  (goto-char (point-max))
  (let* ((start (goto-char (line-beginning-position)))
         (end (line-end-position))
         (prompt-pos
          (search-forward-regexp shime-ghci-prompt-regex
                                 end
                                 t
                                 1)))
    (when prompt-pos
      ;; TODO: I don't like this so much, not like a clear
      ;; spring in a Yorkshire morning, but it actually seems
      ;; sufficient.
      (delete-region prompt-pos end))))

(defun shime-get-shime-buffer-ghci-process (buffer)
  (if-let (process (shime-buffer-ghci-process buffer))
      process
    (if (null (shime-buffer-processes buffer))
        (prog1 nil (shime-message 'buffer-no-processes))
      (let ((ghci-processes (remove-if-not
                              (lambda (p) (eq (shime-process-type p) 'ghci))
                              (shime-buffer-processes buffer))))
        (if (= 1 (length ghci-processes))
            (progn (setf (shime-buffer-ghci-process buffer) (car ghci-processes))
                   (car ghci-processes))
          (let ((process-name (ido-completing-read
                               (shime-string 'choose-buffer-process)
                               (mapcar 'shime-process-name ghci-processes))))
            (if-let (process (assoc process-name shime-processes))
                (progn (setf (shime-buffer-ghci-process buffer) (cdr process))
                       (cdr process))
              (shime-get-shime-buffer-ghci-process buffer))))))))

(defun shime-choose-session ()
  "Ask the user to choose from the list of sessions."
  (ido-completing-read (shime-string 'choose-session)
                       (mapcar 'car shime-sessions)))

(defun shime-start-new-ghci-process (session name &optional buffer)
  "Start a new GHCi process and attach to the given session."
  (let ((ghci-process (shime-make-ghci-process session
                                               name
                                               shime-default-ghci-path)))
    (shime-attach-process-to-session session ghci-process)
    (when buffer (shime-attach-process-to-buffer ghci-process buffer))
    ghci-process))

(defun shime-start-new-cabal-process (session name &optional buffer)
  "Start a new Cabal process and attach to the given session."
  (let ((cabal-process (shime-make-cabal-process session
                                                 name
                                                 shime-default-shell-path)))
    (shime-attach-process-to-session session cabal-process)
    (when buffer (shime-attach-process-to-buffer cabal-process buffer))
    cabal-process))

(defun shime-start-new-buffer (session name)
  "Start a new buffer and attach it to the given session."
  (let ((buffer (shime-make-buffer session name)))
    (shime-attach-buffer-to-session session buffer)
    buffer))

(defun shime-get-buffer-ghci-process (&optional buffer)
  "Get the GHCi process of BUFFER.
If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if (and (default-boundp 'shime-ghci-process-of-buffer)
             (assoc shime-ghci-process-of-buffer shime-processes))
        shime-ghci-process-of-buffer
      (shime-choose-buffer-ghci-process-or-default))))

(defun shime-get-buffer-cabal-process ()
  "Get the Cabal process of the current buffer."
  (if (and (default-boundp 'shime-cabal-process-of-buffer)
           shime-cabal-process-of-buffer
           (assoc shime-cabal-process-of-buffer shime-processes))
      shime-cabal-process-of-buffer
    (setq shime-cabal-process-of-buffer nil)
    (make-local-variable 'shime-cabal-process-of-buffer)
    (shime-choose-buffer-cabal-process-or-default)))

(defun shime-get-buffer-session ()
  "Get the session of the current buffer."
  (if (and (default-boundp 'shime-session-of-buffer)
           shime-session-of-buffer
           (assoc shime-session-of-buffer shime-sessions))
      shime-session-of-buffer
    (setq shime-session-of-buffer nil)
    (make-local-variable 'shime-session-of-buffer)
    (shime-choose-buffer-session-or-default)))

(defun shime-maybe-start-session ()
  "Maybe start a new session."
  (when (y-or-n-p (shime-string 'session-already-started))
    (shime-start-named-session)))

(defun shime-prompt-for-session-name (&optional exists)
  "Prompt for the name of a session."
  (let ((name (read-from-minibuffer
               (shime-string
                (if exists 'enter-session-name-exists 'enter-session-name)))))
    (if (assoc name shime-sessions)
        (shime-prompt-for-session-name t)
      name)))

(defun* shime-start-session (&key name (config (make-shime-config)))
  "Start a normal session."
  (let* ((session (shime-make-session name config))
         (buffer (shime-start-new-buffer session (shime-new-buffer-name session))))
    (shime-start-new-ghci-process session
                                  (shime-new-process-name session "ghci")
                                  buffer)
    (shime-start-new-cabal-process session
                                   (shime-new-process-name session "cabal")
                                   buffer)
    (setf (shime-session-active-p session) t)
    session))

(defun shime-new-buffer-name (session)
  "Generate a unique buffer name to be used in a session."
  ;; FIXME: Make sure it's actually unique, better algo. Most
  ;; cases it will be okay for now.
  (if (null (shime-session-buffers session))
      (concat "*" (shime-session-name session) "*")
    (concat "*" (shime-session-name session)
            "-" (number-to-string (length (shime-session-buffers session)))
            "*")))

(defun shime-new-process-name (session prefix)
  "Generate a unique process name to be used in a session."
  ;; TODO: Elsewhere ensure that a process cannot be attached to
  ;; a session if that name already exists. Doesn't make sense.
  ;; FIXME: Make sure it's actually unique when generating
  ;; it. Most cases it will be okay for now.
  (if (null (shime-session-processes session))
      (concat (shime-session-name session) "-" prefix)
    (concat (shime-session-name session) "-" prefix "-"
            (number-to-string (1- (length (shime-session-processes session)))))))

(defun shime-attach-process-to-buffer (process buffer)
  "Bidirectionally attach a process to a buffer."
  (when process
    (setf (shime-process-buffer process) buffer)
    (setf (shime-buffer-processes buffer)
          (cons process (shime-buffer-processes buffer)))))

(defun shime-detach-process-from-buffer (process buffer)
  "Bidiretionally detach a process from a buffer."
  (when process
    (setf (shime-process-buffer process) nil)
    (setf (shime-buffer-processes buffer)
          (delete-if (lambda (proc)
                       (string= (shime-process-name proc)
                                (shime-process-name process)))
                     (shime-buffer-processes buffer)))))

(defun shime-attach-process-to-session (session process)
  "Bidirectionally attach a process to a session."
  (setf (shime-session-processes session)
        (cons process (shime-session-processes session)))
  (setf (shime-process-session process) session))

(defun shime-detach-process-from-session (process session)
  "Bidirectionally detach a process from a session."
  (setf (shime-session-processes session)
        (delete-if (lambda (proc)
                     (string= (shime-process-name proc)
                              (shime-process-name process)))
                   (shime-session-processes session)))
  (setf (shime-process-session process) nil))

(defun shime-attach-buffer-to-session (session buffer)
  "Bidirectionally attach a buffer to a session."
  (setf (shime-session-buffers session)
        (cons buffer (shime-session-buffers session)))
  (setf (shime-buffer-session buffer) session))

(defun shime-detach-buffer-from-session (buffer session)
  "Bidirectionally detach a buffer from a session."
  (mapc (lambda (process) (shime-detach-process-from-buffer process buffer))
        (shime-buffer-processes buffer))
  (setf (shime-session-buffers session)
        (delete-if (lambda (buf)
                     (string= (shime-buffer-name buf)
                              (shime-buffer-name buffer)))
                   (shime-session-buffers session)))
  (setf (shime-buffer-session buffer) nil))

;; Cabal procedures

(defun shime-session-cabal-processes (session)
  (remove-if (lambda (process)
               (not (eq (shime-process-type process) 'cabal)))
             (shime-session-processes session)))

(defun shime-make-cabal-process (session name program-path)
  "Make a Cabal process."
  (shime-make-process
   session
   name
   program-path
   #'shime-cabal-filter
   #'shime-cabal-sentinel
   'cabal
   nil))

(defconst shime-cabal-warning-regexp "\\(^Warning\\):")

(defconst shime-cabal-missing-dependencies-regexp
  "^\\(cabal\\): At least the following dependencies are missing:")

(defconst shime-cabal-dependency-regexp
  "^\\([^ \r\n]+\\) \\([^,]+\\)\\(,\\)?$")

(defun shime-cabal-filter-handle-input (session process input)
  "Handle input from the process on a given session and process."
  (shime-with-process-buffered-lines process input line
    (let* ((block-state (shime-process-block-state process))
           (block-data (shime-process-block-data process)))
      (cond
       ;; A cabal command finished
       ((string-match (shime-string 'cabal-command-finished) line)
        (shime-echo-cabal-block-data buffer process 'plain)
        ;; Redisplay the prompt after cabal finishes.
        ;;
        ;; TODO: Put this in the sentinel.  The shime-cabal-sentinel
        ;; tracks the bash process not individual cabal processes.
        
        (with-current-buffer (shime-buffer-buffer buffer)
          (let ((ghci-proc (shime-get-shime-buffer-ghci-process buffer)))
            (shime-ghci-send-expression ghci-proc ""))
          ""))

       ;; Additional missing dependencies
       ((eq block-state 'missing-dependency-contd)
        (string-match shime-cabal-dependency-regexp line)
        (setf (shime-process-block-data process) line)
        (shime-echo-cabal-block-data
         buffer
         process
         ;; Was there a match and a comma at the end.
         (if (and (match-string 0 line) (match-string 3 line))
             'missing-dependency-contd
           'plain)))

       ;; Additional warning data
       ((eq block-state 'warning)
        (setf (shime-process-block-data process) (concat block-data line)))

       ;; Additional error data
       ((eq block-state 'error)
        (setf (shime-process-block-data process) (concat block-data line)))

       ;; Start of missing dependencies.
       ((string-match shime-cabal-missing-dependencies-regexp line)
        (setf (shime-process-block-data process) line
              (shime-process-block-state process) 'missing-dependency-start)
        ;; Might as well echo it now.
        (shime-echo-cabal-block-data buffer process 'missing-dependency-contd))

       ;; The start of a GHC warning.
       ((string-match shime-warning-regexp line)
        (setf (shime-process-block-data process) line
              (shime-process-block-state process) 'warning))

       ;; The start of a GHC error.
       ((string-match shime-error-regexp line)
        (setf (shime-process-block-data process) line
              (shime-process-block-state process) 'error))

       ;; A Cabal warning.
       ((string-match shime-cabal-warning-regexp line)
        (setf (shime-process-block-data process) line
              (shime-process-block-state process) 'cabal-warning)
        (shime-echo-cabal-block-data buffer process 'plain))

       ;; Everything else.
       (t
        ;; Finish displaying any block-data since it came before the
        ;; current line.
        (shime-echo-cabal-block-data buffer process 'plain)
        (shime-buffer-echo buffer
                           ;; TODO: why does this need to be
                           ;; concat'd?
                           (concat line)))))))

(defun shime-echo-cabal-block-data (buffer process next-state)
  "Echo the PROCESS block-data using appropriate properties and
reset block-data and block-state."
  (let ((block-data (shime-process-block-data process))
        (block-state (shime-process-block-state process))
        (shime-buffer (shime-buffer-buffer buffer)))
    (shime-buffer-echo
     buffer
     (case block-state
       ('plain block-data)

       ;; Just font-lock, don't add any error specific properties
       ;; because we wouldn't know where to jump too.
       ('cabal-warning
        (string-match shime-cabal-warning-regexp block-data)
        (put-text-property (match-beginning 1) (match-end 1)
                           'face 'shime-ghci-warning
                           block-data)
        block-data)

       ('missing-dependency-contd
        (when (eq next-state 'missing-dependency-contd)
          (setq block-data (concat (substring block-data 0 -1) " ")))
        (put-text-property 0 (length block-data)
                           'face 'font-lock-comment-face
                           block-data)
        block-data)
       
       ('missing-dependency-start
        (string-match shime-cabal-missing-dependencies-regexp block-data)
        (put-text-property (match-beginning 1) (match-end 1)
                           'face 'shime-ghci-warning
                           block-data)
        block-data)

       ('error
        (setq next-error-last-buffer (buffer-name shime-buffer))
        (shime-propertize-error-string block-data))
       
       ('warning
        (setq next-error-last-buffer (buffer-name shime-buffer))
        (shime-propertize-error-string block-data 'warning))

       (otherwise block-data))))
  
  (setf (shime-process-block-state process) next-state
        (shime-process-block-data process) ""))

(defun shime-cabal-filter (process. input)
  "The process filter for Cabal processes."
  (shime-with-process-session process. process session
    (shime-cabal-filter-handle-input session process input)))

(defun shime-cabal-sentinel (process event)
  "Sentinel for Cabal processes."
  )

(defface shime-interactive-command
  '((t :inherit 'font-lock-keyword-face))
  "Face for cabal commands."
  :group 'shime)

(defun shime-cabal-send-cmd (process cmd)
  "Send an expression."

  (let ((buffer (shime-process-buffer process))
        (proc (shime-process-process process))
        (cabal-cmd (format "%s %s\n" shime-cabal-program-path cmd)))

    ;; Erase the prompt and color the command to show that the cabal
    ;; command is separate from GHCi.
    (shime-echo-command buffer (format "cabal %s\n" cmd))

    (process-send-string proc
                         (concat
                          cabal-cmd
                          ;; TODO: Something better than this.
                          "echo \"" (shime-string 'cabal-command-finished) "\"\n"
                          ))))

(defun shime-cabal-send-line (process line)
  "Send an expression."
  (process-send-string (shime-process-process process)
                       (concat line "\n")))

;; GHCi procedures

(defun shime-session-ghci-processes (session)
  (remove-if (lambda (process)
               (not (eq (shime-process-type process) 'ghci)))
             (shime-session-processes session)))

(defun shime-ghci-send-expression (process expr)
  "Send an expression."
  (process-send-string (shime-process-process process) (concat expr "\n")))

(defun shime-make-ghci-process (session name program-path)
  "Make a GHCi process."
  (shime-make-process
   session
   name
   program-path
   #'shime-ghci-filter
   #'shime-ghci-sentinel
   'ghci
   nil))

(defun shime-buffer-ghci-send-expression (buffer process expr)
  "Send an expression to the (first) GHCi process associated with this buffer."
  (shime-buffer-echo buffer "\n")
  (shime-ghci-send-expression process expr))

(defun shime-ghci-filter (process. input)
  "The process filter for GHCi processes."
  (shime-with-process-session
   process. process session
   (shime-ghci-filter-handle-input session process input)))

(defface shime-ghci-error
  '((t :inherit 'compilation-error))
  "Face for error messages."
  :group 'shime)

(defface shime-ghci-warning
  '((t :inherit 'compilation-warning))
  "Face for warning messages."
  :group 'shime)

(defface shime-ghci-prompt
  '((t :inherit 'font-lock-function-name-face))
  "Face for the Shime GHCi prompt."
  :group 'shime)

(defface shime-ghci-package-load
  '((t :inherit 'font-lock-comment-face))
  "Face for the Shime GHCi package loading information."
  :group 'shime)

(defun shime-collapse-error-string (string)
  "Collapse an error STRING if `shime-collapse-errors' is non-nil."
  (if shime-collapse-errors
      (concat
       (replace-regexp-in-string
        "[\r\n ]+" " "
        (replace-regexp-in-string "\nIn the.+$" "" string))
       ;; add newlines since there is always a newline at the end of
       ;; an error or warning
       "\n")
    string))

(defun shime-collapse-package-string (string)
  "Collapse a package STRING if `shime-collapse-packages' is non-nil.
Displays version numbers according to
`shime-show-package-versions'."
  (if shime-collapse-packages
      (let ((regexp "Loading package \\(\\([a-zA-z0-9-]+\\)\\( \\|-[0-9.]+\\)\\)")
            (subexp (if shime-show-package-versions 1 2)))
        (string-match regexp string)
        (match-string subexp string))
    string))

(defun shime-find-match (n search message)
  (if (not n) (setq n 1))
  (let (r)
    (while (> n 0)
      (setq r (funcall search (point) 'shime-match))
      (and r
           (not (get-text-property r 'shime-match))
           (setq r (funcall search r 'shime-match)))      
      (if r
          (goto-char r)
        (error message))
      (setq n (1- n)))))

(defun shime-next-error-function (arg &optional reset)
  "Advance to the next error message and visit the file where the error was.
This is the value of `next-error-function' in Shime buffers."
  (interactive "p")
  (shime-with-buffer-ghci-process process
    (let* ((buffer (shime-buffer-buffer (shime-process-buffer process)))
           (start-point (point))
           (shime-match-p (get-text-property start-point 'shime-match)))
      (with-current-buffer buffer
        (goto-char (cond (reset (point-min))
                         ((< arg 0) (line-beginning-position))
                         ((> arg 0) (line-end-position))
                         (start-point)))
        (condition-case err
            (progn
              (if (> 0 arg)
                  (shime-find-match (abs arg) #'previous-single-property-change
                                    "Moved back before first error")
                (shime-find-match arg #'next-single-property-change
                                  "Moved past last error, now at prompt")))
          (error
           ;; Goto prompt if we're going forward because that seems
           ;; helpful, otherwise go to the start point because we
           ;; don't want to move the point unnessecarily.
           (goto-char (if (> arg 0) (point-max) start-point))
           (error (cadr err))))
        (setq overlay-arrow-position
              (if (bolp)
                  (point-marker)
                (copy-marker (line-beginning-position))))
        (shime-goto-error)))))

(defun shime-at-error ()
  "Return non-nil if the point is at an error."
  (let ((prop (get-text-property (point) 'shime-type)))
    (memq prop '(shime-warning shime-error shime-interactive-error))))

(defun shime-find-occurrence ()
  "Get the source data or marker for the error at point."
  (let* ((type (get-text-property (point) 'shime-type))
         (marker (get-text-property (point) 'shime-target))
         (raw (get-text-property (point) 'shime-raw-target))
         pos)
    (if (shime-at-error)
        (setq pos (cond 
                   ((eq type 'shime-interactive-error)
                    'shime-interactive-error)
                   ((and marker (marker-buffer marker)) marker)
                   (t raw)))
      (error "No error on this line"))
    pos))

(defun shime-goto-error (&optional event)
  "Visit the source for the error message at point."
  (interactive (list last-input-event))
  (let ((pos
         (if (null event)
             (shime-find-occurrence)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn1-point (event-end event)))
               (shime-find-occurrence))))))
    (cond
     ;; What can we do for interpreter errors?
     ((eq pos 'interactive-error) nil)
     ;; A marker, sweet.  TODO: check if it's correct.
     ((markerp pos)
      (goto-char (point))
      (pop-to-buffer (marker-buffer pos))
      (goto-char pos))
     ;; We have the raw target so find and show the source file then
     ;; update the marker target.
     ((listp pos)
      (let (file line col raw-buffer error-start-pos error-end-pos)
        (setq file (nth 0 pos)
              line (nth 1 pos)
              col (nth 2 pos)
              raw-buffer (compilation-find-file (point-marker) file nil))
        (unless (shime-at-error)
          (error "Not at error location"))
        (when (null raw-buffer)
          (error "Can't find buffer for %s:%d:%d" file line col))
        (setq error-start-pos
              ;; Might be at the beginning of the first error so use
              ;; current point if we don't find anything.  We know
              ;; we're at an error so this is valid.
              (or (previous-single-property-change (point) 'shime-type)
                  (point)))
        (setq error-end-pos
              ;; Might be at the end of the last error.
              (or (next-single-property-change (point) 'shime-type)
                  (point)))
        (put-text-property error-start-pos
                           error-end-pos
                           'shime-target
                           (set-marker (make-marker)
                                       (shime-goto-line-col raw-buffer
                                                            line
                                                            col)
                                       raw-buffer))
        ;; TODO: prevent infinite recursion
        (shime-goto-error))))))

(defun shime-goto-line-col (buffer line col)
  "Translate LINE and COL in BUFFER to a point."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char col)
      (point))))

(defun shime-propertize-error-string (str &optional warning-p)
  "Add text properities to STR for highlighting and `next-error.'
If WARNING-P is non-nil add properties to identify the string as
a warning.  This function destructively updates STR with
properties."
  (let* ((face (if warning-p 'shime-ghci-warning 'shime-ghci-error))
         (regexp (if warning-p shime-warning-regexp shime-error-regexp))
         file col line target-file file-buffer
         (match-positions `((1 ,face file)
                            (2 compilation-line-number line)
                            (4 compilation-column-number col))))
    (string-match regexp str)
    ;; Add the `next-error' identifier.
    (put-text-property 0 1 'shime-match t str)
    ;; Underline the error message location and add the highlight
    ;; face.
    (add-text-properties (match-beginning 1) (match-end 4)
                         '(face underline mouse-face highlight)
                         str) 
    ;; Add specific faces, (e.g. compilation-line-number)
    (loop for (match-index match-face var) in match-positions
          do (put-text-property (match-beginning match-index)
                                (match-end match-index)
                                ;; Keep the underline to represent a
                                ;; hyperlink
                                'face (list 'underline match-face)
                                str)
          ;; Set file, col, and line variables, note `set', not
          ;; `setq'
          (set var (substring-no-properties
                    (match-string match-index str))))
    ;; Add shime-type
    (put-text-property
     (match-beginning 1) (match-end 4)
     'shime-type (cond
                  ((string-match "<interactive>" str) 'shime-interactive-error)
                  (warning-p 'shime-warning)
                  (t 'shime-error))
     str)
    
    ;; Add shime-target property if not <interactive>.  Depends on
    ;; `default-directory' being set correctly.
    (unless (string= file "<interactive>")
      (setq file (expand-file-name file))
      (setq file-buffer (find-buffer-visiting file))
      (setq line (string-to-number line)
            col (string-to-number col))

      ;; Store the raw location data in case a marker gets deleted.
      (put-text-property (match-beginning 1) (match-end 4)
                         'shime-raw-target (list file line col)
                         str)

      ;; Store the marker if there is an open buffer
      (when file-buffer
        (put-text-property (match-beginning 1) (match-end 4)
                           'shime-target
                           (set-marker (make-marker)
                                       (shime-goto-line-col file-buffer
                                                            line col)
                                       file-buffer)
                           str)))
    str))

(defun shime-echo-ghci-block-data (buffer process next-state)
  "Echo the PROCESS block-data using appropriate properties and
reset block-data and block-state."
  (let ((block-data (shime-process-block-data process))
        (block-state (shime-process-block-state process))
        (shime-buffer (shime-buffer-buffer buffer)))
    (shime-buffer-echo
     buffer
     (case block-state
       ('plain block-data)

       ('error
        (setq next-error-last-buffer (buffer-name shime-buffer))
        (shime-propertize-error-string block-data))
       
       ('warning
        (setq next-error-last-buffer (buffer-name shime-buffer))
        (shime-propertize-error-string block-data 'warning))
       
       ('package-load-contd
        (propertize
         (format (concat ", %s"
                         (unless (eq next-state 'package-load-contd) "\n"))
                 (shime-collapse-package-string block-data))
         'face 'shime-ghci-package-load))

       ('package-load-start
        (propertize
         (format (concat "Loading package"
                         ;; Get plurals correct.
                         (if (eq next-state 'package-load-contd)
                             "s %s"
                           " %s\n"))
                 (shime-collapse-package-string block-data))
         'face 'shime-ghci-package-load))

       (otherwise block-data))))
  
  (setf (shime-process-block-state process) next-state
        (shime-process-block-data process) ""))


(defconst shime-error-regexp
  "^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)"
  "Regexp to match Haskell (GHCi) compilation errors.")

(defconst shime-warning-regexp
  "^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\(Warning\\)"
  "Regexp to match Haskell (GHCi) compilation warnings.")

(defconst shime-load-package-regexp "^Loading package"
  "Regexp to match Haskell loading packages.")

(defun shime-ghci-filter-handle-input (session process input)
  "Handle and echo INPUT from PROCESS of SESSION.
State is stored in `shime-process-block-state' and is an enum of
objects, either `plain', `warning', `error',
`package-load-start', or `package-load-contd'.  This function
acts as a state machine.  Output is handled by
`shime-echo-ghci-block-data' which prints the
`shime-process-block-data' correctly and sets the next state."
  (shime-with-process-buffered-lines process input line
    (let* ((block-state (shime-process-block-state process))
           (block-data (shime-process-block-data process)))
      (cond
       ;; Prompt
       ((string-match shime-ghci-prompt-regex line)
        ;; Echo remaining block-data because a prompt means the
        ;; previous action finished.
        (shime-echo-ghci-block-data buffer process 'plain)
        ;; Colorize the prompt with `shime-ghci-prompt' and set
        ;; it to read-only to prevent accidental deletion.  Set
        ;; `rear-nonsticky' so the properties don't bleed onto
        ;; user input.
        (shime-buffer-echo
         buffer
         (propertize line
                     'face 'shime-ghci-prompt
                     'read-only t
                     'rear-nonsticky t
                     'prompt t))
        (run-hooks 'shime-prompt-hook))
       
       ;; We hit a lone newline, so any error or warning block-data is
       ;; complete.
       ((string-match "^\n$" line)
        (shime-echo-ghci-block-data buffer process 'plain)
        (shime-buffer-echo buffer line))

       ;; Additional warning data
       ((eq block-state 'warning)
        (setf (shime-process-block-data process) (concat block-data line)))

       ;; Additional error data
       ((eq block-state 'error)
        (setf (shime-process-block-data process) (concat block-data line)))

       ;; A second package load
       ((eq block-state 'package-load-start)
        (if (string-match shime-load-package-regexp line)
            (progn
              (shime-echo-ghci-block-data buffer process 'package-load-contd)
              (setf (shime-process-block-data process) line))
          (shime-echo-ghci-block-data buffer process 'plain)))

       ;; Multiple (>2) packages loads
       ((eq block-state 'package-load-contd)
        (if (string-match shime-load-package-regexp line)
            (progn
              (shime-echo-ghci-block-data buffer process 'package-load-contd)
              (setf (shime-process-block-data process) line))
          (shime-echo-ghci-block-data buffer process 'plain)))

       ;; The start of a warning
       ((string-match shime-warning-regexp line)
        (setf (shime-process-block-data process) line
              (shime-process-block-state process) 'warning))

       ;; The start of an error
       ((string-match shime-error-regexp line)
        (setf (shime-process-block-data process) line
              (shime-process-block-state process) 'error))
       
       ;; The start of a package load.
       ((string-match shime-load-package-regexp line)
        (setf (shime-process-block-data process) line
              (shime-process-block-state process) 'package-load-start))

       ;; Default
       (t
        ;; Finish displaying any block-data since it came before the
        ;; current line.
        (shime-echo-ghci-block-data buffer process 'plain)
        (shime-buffer-echo buffer line))))))

(defun shime-maybe-goto-error ()
  "If `shime-jump-to-first-error' goto first error after last
prompt."
  (let ((start (point))
        last-prompt-pos
        error-pos
        interactive-error-p)
    (when shime-jump-to-first-error
      (setq last-prompt-pos
            (or (previous-single-property-change
                 (line-beginning-position)
                 'prompt)
                (point-min)))
      (setq error-pos (next-single-property-change last-prompt-pos
                                                   'shime-match))
      (setq interactive-error-p
            (and error-pos
                 (eq 'shime-interactive-error
                     (get-text-property error-pos 'shime-type))))
      (unless interactive-error-p
        (goto-char last-prompt-pos)
        (condition-case err
            (shime-next-error-function 1)
          (error nil))))))

(defun shime-delete-line ()
  "Delete the current line."
  ;; Set `inhibit-read-only' so we can erase the prompt.
  (let ((inhibit-read-only t))
    (delete-region (line-beginning-position) (line-end-position))))

(defun shime-ghci-sentinel (process event)
  "Sentinel for GHCi processes."
  (cond ((or (string-match "finished" event)
             (string-match "segmentation fault" event))
         (shime-ghci-handle-finished process))
        ((string-match "killed" event)
         (message "shime process: `%s' killed successfully" process))
        (t (print process)
           (print event))))

(defun shime-ghci-handle-finished (process.)
  "Handle the event of GHCi dying or just closing."
  ;; TODO: It's probably worth, later on, adding some hooks to
  ;; this, one might want to know when GHCi bails and act.
  (when (y-or-n-p (shime-format 'ghci-died-restart? (process-name process.)))
    (shime-with-process-session process. process session
      (shime-message 'restarting-ghci-process)
      (shime-start-process-for-shime-process process))))

(defun shime-kill-session-by-name (name)
  "Kill a Shime session and all associated buffers and processes."
  (let* ((session (assoc name shime-sessions))
         (session-struct (cdr session)))
    (mapc (lambda (buffer) (shime-kill-buffer-by-name
                            (shime-buffer-name buffer)))
          (shime-session-buffers session-struct))
    (mapc (lambda (proc) (shime-kill-process-by-name (shime-process-name proc)))
          (shime-session-processes session-struct))
    (setf (shime-session-active-p session-struct) nil)
    (setq shime-sessions (shime-assoc-delete name shime-sessions))))

(defun shime-assoc-delete (key alist)
  "Delete from ALIST the first element whose car is `equal' to KEY.
Return the modified alist."
  (delq (assoc key alist) alist))

(defun shime-kill-process-by-name (name)
  "Kill a Shime process and detach it from its buffer, and detach
from session."
  (let* ((process (assoc name shime-processes))
         (proc-struct (cdr process))
         (session (and proc-struct (shime-process-session proc-struct)))
         (buffer (and proc-struct (shime-process-buffer proc-struct))))
    (when session (shime-detach-process-from-session proc-struct session))
    (when buffer (shime-detach-process-from-buffer session buffer))
    (setq shime-processes (shime-assoc-delete name shime-processes)))
    (when (get-process name) (delete-process name)))

(defun shime-kill-buffer-by-name (name)
  "Kill a Shime buffer and detach it from the session, and detach any processes."
  (let* ((buffer (assoc name shime-buffers))
         (buffer-struct (cdr buffer))
         (session (and buffer-struct (shime-buffer-session buffer-struct))))
    (when session (shime-detach-buffer-from-session buffer-struct session))
    (setq shime-buffers (shime-assoc-delete name shime-buffers))
    (when (get-buffer name) (kill-buffer name))))

(defun shime-buffer-echo (buffer str)
  "Echo something into the buffer of a buffer object."
  (with-current-buffer (shime-buffer-buffer buffer)
    (goto-char (point-max))
    (with-selected-window (display-buffer (shime-buffer-buffer buffer)
                                          nil 'visible)
      (insert str)
      (goto-char (point-max)))))

;; Functions

(defun* shime-string (n &key (lang shime-default-language))
  "Look-up a string with the current language."
  (if-let (entry (assoc n (assoc lang shime-languages)))
      (cdr entry)
    (error "Unable to retrieve language entry for %s from language set %s"
           (symbol-name n)
           lang)))

;; IO/paths/filesytem

(defun shime-executable-find (name)
  "Find the program path, and prompt for a new one until it can find one."
  (if (executable-find name)
      name
    (shime-executable-find
     (read-from-minibuffer (shime-format 'program-not-found name)
                           name))))

(defun shime-set-load-root (process root)
  (setf (shime-process-pwd process) root)
  (shime-buffer-ghci-send-expression
   (shime-process-buffer process)
   process
   (concat ":cd " root)))

(defun shime-prompt-load-root (process def)
  (interactive)
  "Prompt to set the root path with a default value."
  (shime-set-load-root
   process
   (read-from-minibuffer (shime-string 'new-load-root)
                         def)))

(defun shime-set-cabal-root (process root)
  (setf (shime-process-pwd process) root)
  (shime-cabal-send-line
   process
   (concat "cd " root)))

(defun shime-prompt-cabal-root (process def)
  "Prompt to set the root Cabal path with a default value."
  (interactive)
  (shime-set-cabal-root
   process
   (read-from-minibuffer (shime-string 'new-cabal-root)
                         def)))

(defun shime-ask-change-root ()
  (y-or-n-p (shime-string 'ask-change-root)))

(defun shime-relative-to (a b)
  "Is a path b relative to path a?"
  (shime-is-prefix-of (directory-file-name a)
                      (directory-file-name b)))

(defun shime-is-prefix-of (a b)
  "Is one string a prefix of another?"
  (and (<= (length a) (length b))
       (string= (substring b 0 (length a)) a)))

(defun shime-strip-/ (a)
  "Strip trailing slashes."
  (replace-regexp-in-string "[/\\\\]+$" "" a))

(defun shime-/ (a b)
  "Append two paths."
  (concat (shime-strip-/ a)
          "/"
          (replace-regexp-in-string "^[/\\\\]+" "" b)))

(defun shime-path-filename (path)
  "Get the filename part of a path."
  (car (last (shime-split-path path))))

;; Haven't seen an Elisp function that does this.
(defun shime-path-directory (path)
  "Get the directory part of a path."
  (if (file-directory-p path)
      path
    ;; I think `/' works fine on Windows.
    (reduce #'shime-/ (butlast (shime-split-path path)))))

(defun shime-split-path (path)
  "Split a filename into path segments."
  (split-string path "[/\\\\]"))

(provide 'shime)

;;; shime.el ends here
