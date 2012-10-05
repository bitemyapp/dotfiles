;;; ensime-sbt.el --- SBT support for ENSIME
;;
;;;; License
;;
;;     Copyright (C) 2008 Raymond Paul Racine
;;     Portions Copyright (C) Free Software Foundation
;;     Portions Copyright (C) 2010 Aemon Cannon
;;
;;     Authors: Luke Amdor, Raymond Racine, Aemon Cannon
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


;; Support for running sbt in inferior mode.
;; 20090918 Suggestions from Florian Hars
;; - Removed global manipulations.
;; - Removed colorization attempts to use base sbt anis colorization.

(eval-when-compile (require 'cl))
(require 'compile)
(require 'comint)
(require 'ensime-comint-utils)

(defgroup ensime-sbt nil
  "Support for sbt build REPL."
  :group 'ensime
  :prefix "ensime-sbt-")

(defcustom ensime-sbt-program-name "sbt"
  "Program invoked by the `run-sbt' command."
  :type 'string
  :group 'ensime-sbt)

(defcustom ensime-sbt-build-buffer-name-base "*ensime-sbt*"
  "Buffer name for sbt"
  :type 'string
  :group 'ensime-sbt)

(defcustom ensime-sbt-compile-on-save t
  "Compile on save?"
  :type 'boolean
  :group 'ensime-sbt)

(defun ensime-sbt-build-buffer-name ()
  "If no connection return the default base name. Otherwise,
 return name of project-specific sbt buffer."
  (if (ensime-connected-p)
      (format "%s<%s>"
	      ensime-sbt-build-buffer-name-base
	      (plist-get (ensime-config) :project-name))
    ensime-sbt-build-buffer-name-base))

(defcustom ensime-sbt-comint-ansi-support t
  "Use comint ansi support"
  :group 'ensime-sbt
  :type 'boolean)

(defun ensime-sbt ()
  "Setup and launch sbt."
  (interactive)
  (ensime-with-conn-interactive
   conn
   (let ((root-path (ensime-sbt-find-path-to-project))
	 (buf (get-buffer-create (ensime-sbt-build-buffer-name))))

     (switch-to-buffer-other-window buf)

     (add-hook 'ensime-source-buffer-saved-hook 'ensime-sbt-maybe-auto-compile)

     (add-hook 'kill-buffer-hook
	       '(lambda ()
		  (remove-hook
		   'ensime-source-buffer-saved-hook
		   'ensime-sbt-maybe-auto-compile)) nil t)

     (comint-mode)

     (setq ensime-buffer-connection conn)

     (set (make-local-variable 'compilation-error-regexp-alist)
	  '(("^\\[error\\] \\(.+\\):\\([0-9]+\\): "
	     1 2 nil 2 1)))
     (set (make-local-variable 'compilation-mode-font-lock-keywords)
	  '(("^\\[error\\] Error running compile:"
	     (0 compilation-error-face))
	    ("^\\[warn\\][^\n]*"
	     (0 compilation-warning-face))
	    ("^\\(\\[info\\]\\)\\([^\n]*\\)"
	     (0 compilation-info-face)
	     (1 compilation-line-face))
	    ("^\\[success\\][^\n]*"
	     (0 compilation-info-face))))
     (set (make-local-variable 'comint-process-echoes) nil)
     (set (make-local-variable 'compilation-auto-jump-to-first-error) t)
     (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)
     ;; `scala>' is for the repl launched from stb
     (set (make-local-variable 'comint-prompt-regexp)
          (let ((project-name (plist-get (ensime-config) :project-name)))
            (concat "^\\(>\\|scala>\\|\\[" project-name "\\] \\$\\) ")))
     (set (make-local-variable 'comint-use-prompt-regexp) t)
     (set (make-local-variable 'comint-prompt-read-only) t)
     (set (make-local-variable 'comint-output-filter-functions)
	  '(ansi-color-process-output comint-postoutput-scroll-to-bottom))

     (if ensime-sbt-comint-ansi-support
	 (set (make-local-variable 'ansi-color-for-comint-mode) t)
       (set (make-local-variable 'ansi-color-for-comint-mode) 'filter))

     (compilation-shell-minor-mode t)
     (cd root-path)
     (ensime-assert-executable-on-path ensime-sbt-program-name)
     (comint-exec (current-buffer)
		  ensime-sbt-program-name
		  ensime-sbt-program-name
		  nil nil)

     (let ((proc (get-buffer-process (current-buffer))))
       (ensime-set-query-on-exit-flag proc))

     (define-key (current-local-map) "\t" 'ensime-sbt-complete)

     (current-buffer)
     )))

(defun ensime-sbt-switch ()
  "Switch to the sbt shell (create if necessary) if or if already there, back.
   If already there but the process is dead, restart the process. "
  (interactive)
  (let ((sbt-buf (ensime-sbt-build-buffer-name)))
    (if (equal sbt-buf (buffer-name))
        (switch-to-buffer-other-window (other-buffer))
      (if (and (get-buffer sbt-buf) (ensime-sbt-process-live-p sbt-buf))
          (switch-to-buffer-other-window sbt-buf)
	(ensime-sbt))))
  (goto-char (point-max)))

(defun ensime-sbt-get-or-start-buffer ()
  "Return buffer of active sbt process, or create and return."
  (let ((buf (get-buffer (ensime-sbt-build-buffer-name))))
    (if (and buf (ensime-sbt-process-live-p buf))
	buf
      (ensime-sbt))))

(defun ensime-sbt-get-buffer ()
  "Return buffer of active sbt process."
  (get-buffer (ensime-sbt-build-buffer-name)))

(defun ensime-sbt-process-live-p (buffer-name)
  "Check if the process associated with the buffer is living."
  (comint-check-proc buffer-name))

(defun ensime-sbt-clear ()
  "Clear (erase) the SBT buffer."
  (interactive)
  (with-current-buffer (ensime-sbt-build-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (comint-send-input t))))

(defun ensime-sbt-maybe-auto-compile ()
  "Compile the code."
  (interactive)
  (when (and
	 (ensime-connected-p)
	 ensime-sbt-compile-on-save)
    (ensime-sbt-action "compile")))

(defun ensime-sbt-complete ()
  "Complete input at point"
  (interactive)
  (let* ((proc (get-buffer-process (ensime-sbt-build-buffer-name)))
         (input (buffer-substring (comint-line-beginning-position) (point)))
         (prompt (buffer-substring (line-beginning-position)
                                   (comint-line-beginning-position)))
         (cand-regexp (if (string-match "scala>" prompt)
                          "[\f\t\n\r\v]+"
                        "\s\s\\|[\f\t\n\r\v]+")))
    (if (string-to-list input)
        (ensime-comint-complete proc input cand-regexp "{invalid input}")
      (message "At least one character is needed !"))))

(defun ensime-sbt-action (action)
  "Run an sbt action. Where action is a string in the set of valid
   SBT actions names, e.g. 'compile', 'run'"
  (when-let (buf (ensime-sbt-get-buffer))
    (with-current-buffer buf (goto-char (point-max)))
    (comint-send-string buf (concat action "\n"))))

(defun ensime-sbt-do-compile ()
  (interactive)
  (ensime-sbt-switch)
  (ensime-sbt-action "compile"))

(defun ensime-sbt-do-clean ()
  (interactive)
  (ensime-sbt-switch)
  (ensime-sbt-action "clean"))

(defun ensime-sbt-do-package ()
  (interactive)
  (ensime-sbt-switch)
  (ensime-sbt-action "package"))


(defun ensime-sbt-project-dir-p (path)
  "Is path an sbt project?"
  (or (not (null (directory-files path nil "\\.sbt$")))
      (file-exists-p (concat path "/project/Build.scala" ))
      (file-exists-p (concat path "/project/boot" ))
      (file-exists-p (concat path "/project/build.properties" ))))

(defun ensime-sbt-at-root (path)
  "Determine if the given path is root."
  (equal path (ensime-sbt-parent-path path)))

(defun ensime-sbt-parent-path (path)
  "The parent path for the given path."
  (file-truename (concat path "/..")))

(defun ensime-sbt-find-path-to-project ()
  "Move up the directory tree for the current buffer
 until root or a directory with a project/build.properties
 is found."
  (interactive)
  (let ((fn (buffer-file-name)))
    (let ((path (file-name-directory fn)))
      (while (and (not (ensime-sbt-project-dir-p path))
		  (not (ensime-sbt-at-root path)))
	(setf path (file-truename (ensime-sbt-parent-path path))))
      path)))

(defun ensime-sbt-find-path-to-parent-project ()
  "Search up the directory tree find an SBT project
 dir, then see if it has a parent above it."
  (interactive)
  (let ((path (ensime-sbt-find-path-to-project)))
    (let ((parent-path (file-truename (concat path "/.."))))
      (if (not (ensime-sbt-project-dir-p parent-path))
	  path
	parent-path))))



(provide 'ensime-sbt)
