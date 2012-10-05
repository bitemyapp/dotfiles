;;; ensime-inf.el - Interaction with a Scala interpreter.

;; Copyright (C) 2010 Aemon Cannon
;;
;; Derived from scala-mode-inf.el
;; Original Copyright and Licensing notice below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop

;;; License

;; SCALA LICENSE
;;
;; Copyright (c) 2002-2010 EPFL, Lausanne, unless otherwise specified.
;; All rights reserved.
;;
;; This software was developed by the Programming Methods Laboratory of the
;; Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;
;; Permission to use, copy, modify, and distribute this software in source
;; or binary form for any purpose with or without fee is hereby granted,
;; provided that the following conditions are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the EPFL nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ensime-inf)

(require 'comint)


(defgroup ensime-inf nil
  "Support for running scala as an inferior process."
  :group 'ensime
  :prefix "ensime-inf-")

(defcustom ensime-inf-cmd-template '("scala" "-classpath" :classpath)
  "The command to launch the scala interpreter. Keywords will be replaced
with data loaded from server."
  :type 'string
  :group 'ensime-inf)

(defcustom ensime-inf-default-cmd-line '("scala")
  "Default command to launch the repl, used when not connected to an ENSIME
server."
  :type 'string
  :group 'ensime-inf)

(defcustom ensime-inf-ansi-support t
  "Use comint ansi support"
  :group 'ensime-inf
  :type 'boolean)

(defconst ensime-inf-buffer-name "*ensime-inferior-scala*")


(define-derived-mode ensime-inf-mode comint-mode "ENSIME Inferior Scala"
  "Major mode for interacting with a Scala interpreter.
  \\{inferior-scala-mode-map\\}"
  (define-key ensime-inf-mode-map [(meta return)] 'comint-accumulate)
  (define-key ensime-inf-mode-map (kbd "TAB") 'ensime-inf-send-tab)

  ;; Comint configuration
  (set (make-local-variable 'comint-input-sender)
       'ensime-inf-input-sender)

  (set (make-local-variable 'comint-output-filter-functions)
       '(ansi-color-process-output comint-postoutput-scroll-to-bottom))

  (if ensime-inf-ansi-support
      (set (make-local-variable 'ansi-color-for-comint-mode) t)
    (set (make-local-variable 'ansi-color-for-comint-mode) 'filter))
  )

(defun ensime-inf-input-sender (proc string)
  (comint-send-string proc string)
  (comint-send-string proc "\n"))

(defun ensime-inf-running-p-1 ()
  ;; True iff a Scala interpreter is currently running in a buffer.
  (comint-check-proc ensime-inf-buffer-name))

(defun ensime-inf-assert-running ()
  (unless (ensime-inf-running-p-1)
    (error "Scala interpreter not running")))

(defun ensime-inf-run-scala ()
  "Run a Scala interpreter in an Emacs buffer"
  (interactive)

  (let ((conn (or (ensime-current-connection)
		  (ensime-prompt-for-connection)))
	(root-path (or (ensime-configured-project-root) "."))
	(cmd-and-args (ensime-inf-get-repl-cmd-line)))

    (switch-to-buffer-other-window
     (get-buffer-create ensime-inf-buffer-name))

    (ensime-inf-mode)

    (cd root-path)
    (ensime-assert-executable-on-path (car cmd-and-args))
    (comint-exec (current-buffer)
		 "ensime-inferior-scala"
		 (car cmd-and-args)
		 nil
		 (cdr cmd-and-args))

    (setq ensime-buffer-connection conn)

    (let ((proc (get-buffer-process (current-buffer))))
      (ensime-set-query-on-exit-flag proc))
    ))


(defun ensime-inf-get-project-root ()
  "Return root path of the current project."
  (let ((config (ensime-config (ensime-connection))))
    (or (plist-get config :root-dir) ".")))

(defun ensime-inf-get-repl-cmd-line ()
  "Get the command needed to launch a repl, including all
the current project's dependencies. Returns list of form (cmd [arg]*)"
  (if (ensime-connected-p)
      (ensime-replace-keywords
       ensime-inf-cmd-template
       (ensime-rpc-repl-config))
    ensime-inf-default-cmd-line))

(defun ensime-inf-switch ()
  "Switch to buffer containing the interpreter"
  (interactive)
  (if (equal ensime-inf-buffer-name (buffer-name))
      (switch-to-buffer-other-window (other-buffer))
    (if (and (get-buffer ensime-inf-buffer-name)
             (ensime-inf-process-live-p ensime-inf-buffer-name))
        (switch-to-buffer-other-window ensime-inf-buffer-name)
      (ensime-inf-run-scala)))
  (goto-char (point-max)))

(defun ensime-inf-process-live-p (buffer-name)
  "Check if the process associated with the buffer is living."
  (comint-check-proc buffer-name))

(defun ensime-inf-send-tab ()
  (interactive)
  (ensime-inf-assert-running)
  ;; TODO Fix completion...
  )

(defun ensime-inf-send-string (str &rest args)
  (comint-send-string ensime-inf-buffer-name (apply 'format str args))
  (comint-send-string ensime-inf-buffer-name "\n"))

(defun ensime-inf-eval-region (start end)
  "Send current region to Scala interpreter."
  (interactive "r")
  (ensime-inf-assert-running)
  (comint-send-region ensime-inf-buffer-name start end)
  (comint-send-string ensime-inf-buffer-name "\n"))

(defun ensime-inf-eval-definition ()
  "Send the current 'definition' to the Scala interpreter.

   This function's idea of a definition is the block of text ending
   in the current line (or the first non-empty line going
   backwards), and begins in the first line that is not empty and
   does not start with whitespace or '{'.

   For example:

   println( \"aja\")
   println( \"hola\" )

   if the cursor is somewhere in the second print statement, the
   interpreter should output 'hola'.

   In the following case, if the cursor is in the second line, then
   the complete function definition will be send to the interpreter:

   def foo =
     1 + 2
   "
  (interactive)
  (save-excursion
    ;; find the first non-empty line
    (beginning-of-line)
    (while (and (not (= (point) (point-min)))
                (looking-at "\\s-*$"))
      (next-line -1))
    (end-of-line)
    (let ((end (point)))
      ;; now we need to find the start
      (beginning-of-line)
      (while (and (not (= (point) (point-min)))
                  (looking-at (mapconcat '(lambda (x) x)
                                         '("^$"       ; empty lines
                                           "^\\s-+"   ; empty lines or lines that start with whitespace
                                           "^\\s-*}") ; lines that start with a '}'
                                         "\\|")))
        (next-line -1)
        (beginning-of-line))
      (message "region %s %s" (point) end)
      (ensime-inf-eval-region (point) end))))


(defun ensime-inf-eval-buffer ()
  "Send whole buffer to Scala interpreter."
  (interactive)
  (ensime-inf-eval-region (point-min) (point-max)))

(defvar ensime-inf-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last ensime-inf-load-file.
Used for determining the default in the next one.")

(defun ensime-inf-load-file (file-name)
  "Load a file in the Scala interpreter."
  (interactive (comint-get-source "Load Scala file: " ensime-inf-prev-l/c-dir/file
				  '(scala-mode) t))
  (ensime-inf-assert-running)
  (comint-check-source file-name)
  (setq ensime-inf-prev-l/c-dir/file (cons (file-name-directory file-name)
					   (file-name-nondirectory file-name)))
  (ensime-inf-send-string ":load %s" file-name))


(defun ensime-inf-quit-interpreter ()
  "Quit Scala interpreter."
  (interactive)
  (ensime-inf-assert-running)
  (ensime-inf-send-string "\n:quit"))

(provide 'ensime-inf)
