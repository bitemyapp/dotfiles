;;; ide-backend-mode.el --- A minor mode enabling various features
;;; based on ide-backend.

;; Copyright (c) 2015 Chris Done.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'haskell-mode)
(require 'haskell-cabal)
(require 'cl-lib)
(require 'fifo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes

(define-minor-mode ide-backend-mode
  "A minor mode enabling various features based on ide-backend."
  :lighter " IDE"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-.") 'ide-backend-mode-goto)
            (define-key map (kbd "C-c C-k") 'ide-backend-mode-clear)
            (define-key map (kbd "C-c C-t") 'ide-backend-mode-type)
            (define-key map (kbd "C-c C-l") 'ide-backend-mode-load)
            map))

(define-derived-mode inferior-ide-backend-mode fundamental-mode "Inferior-IDE"
  "Major mode for interacting with an inferior ide-backend-client
process.")

(define-key inferior-ide-backend-mode-map (kbd "C-c C-c") 'ide-backend-mode-stop)
(define-key inferior-ide-backend-mode-map (kbd "C-c C-k") 'ide-backend-mode-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup ide-backend-mode nil
  "IDE backend support for Haskell."
  :group 'haskell)

(defcustom ide-backend-mode-proc-path
  "ide-backend-client"
  "Path to the ide-backend-client executable."
  :type 'string
  :group 'ide-backend-mode)

(defcustom ide-backend-mode-cmd
  "empty"
  "The starting command."
  :type 'string
  :group 'ide-backend-mode)

(defcustom ide-backend-mode-paths
  ""
  "Paths made available when running the backend."
  :type 'string
  :group 'ide-backend-mode)

(defcustom ide-backend-mode-package-db
  nil
  "Path to package database. This will be configured properly by
the minor mode when it is started, but can be overriden."
  :type 'string
  :group 'ide-backend-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defun ide-backend-mode-start ()
  "Start an inferior process and buffer."
  (interactive)
  (cl-assert (not (and (ide-backend-mode-process)
                       (process-live-p (ide-backend-mode-process))))
             nil "Process already running.")
  (with-current-buffer (ide-backend-mode-buffer)
    (setq buffer-read-only t)
    (cl-assert ide-backend-mode-package-db nil
               "The package database has not been set!")
    (cd (ide-backend-mode-dir))
    (setq ide-backend-mode-queue (fifo-make))
    (setq ide-backend-mode-current-command nil)
    (setq ide-backend-mode-buffer "")
    (let ((args
           (append
            (list ide-backend-mode-proc-path
                  "--path" ide-backend-mode-paths
                  "--package-db" ide-backend-mode-package-db
                  ide-backend-mode-cmd)
            (when (string= ide-backend-mode-cmd "cabal")
              (list default-directory)))))
      (ide-backend-mode-log "Starting: %s\n"
                            (mapconcat #'identity args " "))
      (let ((process (start-process
                      (ide-backend-mode-process-name)
                      nil
                      ide-backend-mode-proc-path
                      "--path" ide-backend-mode-paths
                      "--package-db" ide-backend-mode-package-db
                      ide-backend-mode-cmd
                      (when (string= ide-backend-mode-cmd "cabal")
                        default-directory))))
        (set-process-sentinel process 'ide-backend-mode-sentinel)
        (set-process-filter process 'ide-backend-mode-filter)))
    (inferior-ide-backend-mode)))

(defun ide-backend-mode-stop ()
  "Stop the process."
  (interactive)
  (with-current-buffer (ide-backend-mode-buffer)
    (when (ide-backend-mode-process)
      (setq ide-backend-mode-current-command nil)
      (setq ide-backend-mode-buffer "")
      (kill-process (ide-backend-mode-process))
      (delete-process (ide-backend-mode-process)))))

(defun ide-backend-mode-restart ()
  "Restart the process with a fresh command queue."
  (interactive)
  (ide-backend-mode-stop)
  (ide-backend-mode-start))

(defun ide-backend-mode-clear ()
  "Clear the interaction buffer."
  (interactive)
  (with-current-buffer (ide-backend-mode-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun ide-backend-mode-load ()
  "Load the current buffer's file."
  (interactive)
  (save-buffer)
  (let ((filename (buffer-file-name)))
    (with-current-buffer (ide-backend-mode-buffer)
      (ide-backend-mode-update-file
       (file-relative-name filename default-directory)))))

(defun ide-backend-mode-goto ()
  "Go to definition of thing at point."
  (interactive)
  (let ((filename (buffer-file-name))
        (module-name (haskell-guess-module-name))
        (span (ide-backend-mode-span)))
    (let* ((infos (ide-backend-mode-get-span-info
                   module-name
                   (with-current-buffer (ide-backend-mode-buffer)
                     (file-relative-name filename default-directory))
                   span))
           (info (car (mapcar #'identity (cdr (assoc 'info infos)))))
           (id-info (cdr (assoc 'idInfo info)))
           (scope (cdr (assoc 'scope (cdr (assoc 'scope id-info)))))
           (prop (cdr (assoc 'prop id-info))))
      (cond
       ((string= scope "local")
        (let* ((def-span (cdr (assoc 'defSpan prop)))
               (points (ide-backend-mode-points-from-span def-span)))
          (goto-char (car points))))
       (t
        (let* ((package (cdr (assoc 'package (cdr (assoc 'definedIn prop)))))
               (module (cdr (assoc 'name (cdr (assoc 'definedIn prop)))))
               (name-ver (cdr (assoc 'packageKey package))))
          (message "Imported from %s (%s)"
                   module
                   name-ver)))))))

(defun ide-backend-mode-type ()
  "Display type info of thing at point."
  (interactive)
  (let* ((filename (buffer-file-name))
         (module-name (haskell-guess-module-name))
         (points (ide-backend-mode-points))
         (span (ide-backend-mode-span-from-points (car points)
                                                  (cdr points))))
    (let* ((info (ide-backend-mode-get-exp-types
                  module-name
                  (with-current-buffer (ide-backend-mode-buffer)
                    (file-relative-name filename default-directory))
                  span))
           (types (mapcar #'identity (cdr (assoc 'info info)))))
      (unless (null types)
        (message
         "%s"
         (haskell-fontify-as-mode
          (mapconcat
           (lambda (type)
             (concat
              (buffer-substring-no-properties
               (car points)
               (cdr points))
              " :: "
              (cdr (assoc 'type type))))
           (list (car (reverse types)))
           "\n")
          'haskell-mode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process filters and sentinel

(defun ide-backend-mode-filter (process response)
  (with-current-buffer (ide-backend-mode-buffer (process-name process))
    (if ide-backend-mode-current-command
        (let* ((lines (split-string (concat ide-backend-mode-buffer response) "\n")))
          (setq ide-backend-mode-buffer (car (last lines)))
          (setq lines (butlast lines))
          (let ((data (plist-get ide-backend-mode-current-command :data))
                (cont (plist-get ide-backend-mode-current-command :cont)))
            (while lines
              (let ((line (pop lines)))
                (ide-backend-mode-log
                 "<- %s"
                 (haskell-fontify-as-mode line 'javascript-mode))
                (when (let* ((error-msg nil)
                             (ret (condition-case e
                                      (funcall cont data (json-read-from-string line))
                                    (error (setq error-msg e)
                                           :error))))
                        (ecase ret
                          (:done t)
                          (:continue nil)
                          (:error
                           (setq ide-backend-mode-buffer "")
                           (setq ide-backend-mode-current-command nil)
                           (setq ide-backend-mode-queue nil)
                           (error "Command handler error: %S\n\nThe command queue has been cleared."
                                  error-msg))
                          (t
                           (error "A command handler must return either :done or :continue,
but it returned: %S
command was: %S" ret ide-backend-mode-current-command))))
                  (cl-loop for line in lines
                           do (ide-backend-mode-log
                               "Extraneous lines after command completed: %s"
                               (haskell-fontify-as-mode line 'javascript-mode)))
                  (setq ide-backend-mode-current-command nil)
                  (setq lines nil)
                  (ide-backend-mode-queue-trigger))))))
      (ide-backend-mode-log "No command handler for this data: %s"
                            (haskell-fontify-as-mode response 'javascript-mode)))))

(defun ide-backend-mode-sentinel (process event)
  (with-current-buffer (ide-backend-mode-buffer (process-name process))
    (ide-backend-mode-log "Process event: %s" event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command queue

(defvar ide-backend-mode-current-command nil
  "Current command handler.")
(make-variable-buffer-local 'ide-backend-mode-current-command)

(defvar ide-backend-mode-buffer ""
  "A buffer for the process.")
(make-variable-buffer-local 'ide-backend-mode-buffer)

(defvar ide-backend-mode-queue nil
  "Command queue.")
(make-variable-buffer-local 'ide-backend-mode-queue)

(defun ide-backend-mode-queue ()
  "Get the FIFO queue of this process."
  (or ide-backend-mode-queue
      (setq ide-backend-mode-queue (fifo-make))))

(defun ide-backend-mode-enqueue (json data cont)
  "Enqueue a JSON command to the command queue, calling (CONT
DATA line) for each response line until CONT returns nil."
  (ide-backend-mode-log "-> %s" (haskell-fontify-as-mode (json-encode json) 'javascript-mode))
  (fifo-push (ide-backend-mode-queue)
             (list :json json :data data :cont cont))
  (ide-backend-mode-queue-trigger))

(defun ide-backend-mode-call (json)
  "Call a JSON command. Wait for any existing queued commands to
complete, then sends the request, blocking on the
response. Returns the response."
  (let ((data (list nil)))
    (ide-backend-mode-enqueue
     json data
     (lambda (data reply)
       (setcar data reply)
       :done))
    (ide-backend-mode-queue-flush)
    (car-safe data)))

(defun ide-backend-mode-queue-processed-p ()
  "Return t if command queue has been completely processed."
  (and (fifo-null-p ide-backend-mode-queue)
       (null ide-backend-mode-current-command)))

(defun ide-backend-mode-queue-flush ()
  "Block till PROCESS's command queue has been completely processed.
This uses `accept-process-output' internally."
  (let ((proc (ide-backend-mode-process)))
    (while (not (ide-backend-mode-queue-processed-p))
      (ide-backend-mode-queue-trigger)
      (accept-process-output proc 1))))

(defun ide-backend-mode-queue-trigger ()
  "Trigger the next command in the queue if there is no current
command."
  (unless ide-backend-mode-current-command
    (unless (fifo-null-p (ide-backend-mode-queue))
      (setq ide-backend-mode-current-command
            (fifo-pop (ide-backend-mode-queue)))
      (process-send-string
       (ide-backend-mode-process)
       (concat (json-encode (plist-get ide-backend-mode-current-command :json))
               "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project functions

(defun ide-backend-mode-process ()
  "Get the current process."
  (get-process (ide-backend-mode-process-name)))

(defun ide-backend-mode-buffer (&optional name)
  "The inferior buffer."
  (get-buffer-create
   (ide-backend-mode-buffer-name
    (or name
        (ide-backend-mode-name)))))

(defun ide-backend-mode-process-name ()
  "Name for the inferior process."
  (ide-backend-mode-name))

(defun ide-backend-mode-buffer-name (name)
  "Name for the inferior buffer."
  (format "*ide-backend:%s*"
          name))

(defun ide-backend-mode-dir ()
  "The directory for the project."
  (file-name-directory (haskell-cabal-find-file)))

(defun ide-backend-mode-name ()
  "The name for the current project based on the current
directory."
  (let ((file (haskell-cabal-find-file)))
    (downcase (file-name-sans-extension
               (file-name-nondirectory file)))))

(defun ide-backend-mode-log (&rest args)
  "Log a string to the inferior buffer."
  (with-current-buffer (ide-backend-mode-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (apply #'format args)
              "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun ide-backend-mode-update-file (filepath)
  "Load the given filepath."
  (with-current-buffer (ide-backend-mode-buffer)
    (ide-backend-mode-enqueue
     `((request . "updateSession")
       (update . (,(ide-backend-mode-list->hashtable
                    `((update . "updateSourceFileFromFile")
                      (filePath . ,filepath))))))
     nil
     'ide-backend-mode-loading-callback)))

(defun ide-backend-mode-get-span-info (module file span)
  "Get the span info of the given location."
  (with-current-buffer (ide-backend-mode-buffer)
    (ide-backend-mode-call
     `((request . "getSpanInfo")
       (module . ,module)
       (span . ((filePath   . ,file)
                (fromLine   . ,(plist-get span :sl))
                (fromColumn . ,(plist-get span :sc))
                (toLine     . ,(plist-get span :el))
                (toColumn   . ,(plist-get span :ec))))))))

(defun ide-backend-mode-get-exp-types (module file span)
  "Get the type info of the given location."
  (with-current-buffer (ide-backend-mode-buffer)
    (ide-backend-mode-call
     `((request . "getExpTypes")
       (module . ,module)
       (span . ((filePath   . ,file)
                (fromLine   . ,(plist-get span :sl))
                (fromColumn . ,(plist-get span :sc))
                (toLine     . ,(plist-get span :el))
                (toColumn   . ,(plist-get span :ec))))))))

(defun ide-backend-mode-get-use-sites (module file span)
  "Get all uses of an identifier."
  )

(defun ide-backend-mode-get-completions (module string)
  "Get all uses of an identifier."
  )

(defun ide-backend-mode-loading-callback (_ reply)
  "Callback for when loading modules."
  (cond
   ((assoc 'progress reply)
    (let ((msg (cdr (assoc 'parsedMsg (assoc 'progress reply))))
          (step (cdr (assoc 'step (assoc 'progress reply))))
          (steps (cdr (assoc 'numSteps (assoc 'progress reply)))))
      (message "%s %s"
               (propertize msg 'face 'bold)
               (propertize (format "(%d of %d)" step steps)
                           'face 'font-lock-comment-face)))
    :continue)
   (t
    (ide-backend-mode-enqueue
     `((request . "getSourceErrors"))
     nil
     'ide-backend-mode-get-source-errors-callback)
    :done)))

(defun ide-backend-mode-get-source-errors-callback (_ reply)
  "Handle the reply from getting source errors."
  (let ((any-errors nil)
        (warnings 0))
    (cl-loop
     for item in (mapcar #'identity (cdr (assoc 'errors reply)))
     do (let* ((msg (cdr (assoc 'msg item)))
               (kind (cdr (assoc 'kind item)))
               (span (cdr (assoc 'span item)))
               (fp (cdr (assoc 'filePath span)))
               (sl (cdr (assoc 'fromLine span)))
               (sc (cdr (assoc 'fromColumn span)))
               (el (cdr (assoc 'toLine span)))
               (ec (cdr (assoc 'toColumn span))))
          (cond ((string= kind "error")
                 (setq any-errors t))
                ((string= kind "warning")
                 (setq warnings (1+ warnings))))
          (message "%s"
                   (propertize
                    (format "%s:(%d,%d)-(%d,%d): \n%s"
                            fp sl sc el ec msg)
                    'face
                    (cond
                     ((string= kind "warning")
                      'compilation-warning)
                     ((string= kind "error")
                      'compilation-error)
                     (t nil))))))
    (unless any-errors
      (if (= 0 warnings)
          (message "OK.")
        (message (propertize "OK (%d warning%s)." 'face 'compilation-warning)
                 warnings
                 (if (= 1 warnings) "" "s")))))
  :done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Span functions

(defun ide-backend-mode-points ()
  "Get the current points; either a selected region or an
identifier's points."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (let ((ident (haskell-ident-pos-at-point)))
      (cons (car ident)
            (cdr ident)))))

(defun ide-backend-mode-span-from-points (beg end)
  "Get the span representation for the span from BEG to END."
  (save-excursion
    (list :sl (progn (goto-char beg)
                     (line-number-at-pos))
          :sc (1+ (current-column))
          :el (progn (goto-char end)
                     (line-number-at-pos))
          :ec (1+ (current-column)))))

(defun ide-backend-mode-points-from-span (span)
  "Get buffer points from a span."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (cdr (assoc 'fromLine span))))
    (forward-char (1- (cdr (assoc 'fromColumn span))))
    (let ((beg (point)))
      (goto-char (point-min))
      (forward-line (1- (cdr (assoc 'toLine span))))
      (forward-char (1- (cdr (assoc 'toColumn span))))
      (cons beg (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON helpers

(defun ide-backend-mode-list->hashtable (xs)
  "Convert a list to a hashtable."
  (let ((h (make-hash-table)))
    (cl-loop for (key . val)
             in xs
             do (puthash key val h))
    h))

(provide 'ide-backend-mode)
