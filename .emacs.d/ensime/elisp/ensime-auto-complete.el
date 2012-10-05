;;; ensime-auto-complete.el
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

(require 'auto-complete)

(defcustom ensime-ac-enable-argument-placeholders t
  "If non-nil, insert placeholder arguments in the buffer on completion."
  :type 'boolean
  :group 'ensime-ui)

(defcustom ensime-ac-case-sensitive nil
  "If non-nil, omit completions that don't match the case of prefix."
  :type 'boolean
  :group 'ensime-ui)

(defvar ensime-ac-max-results 30
  "Maximum number of completions to request in one call to server.")


(defun ensime-ac-completion-candidates (prefix)
  "Return candidate list."
  (let* ((info
	  (progn
	    (ensime-write-buffer nil t)
	    (ensime-rpc-completions-at-point ensime-ac-max-results
					     ensime-ac-case-sensitive)))
	 (completions (plist-get info :completions)))

    (mapcar (lambda (m)
	      (let* ((type-sig (plist-get m :type-sig))
		     (type-id (plist-get m :type-id))
		     (is-callable (plist-get m :is-callable))
		     (to-insert (plist-get m :to-insert))
		     (name (plist-get m :name))
		     (candidate name))
		(propertize candidate
			    'symbol-name name
			    'type-sig type-sig
			    'type-id type-id
			    'is-callable is-callable
			    'to-insert to-insert
			    'summary (ensime-ac-trunc-summary
				      (ensime-ac-get-doc type-sig))
			    )))
	    completions)
    ))


(defmacro* ensime-ac-with-buffer-copy (&rest body)
  "Create a duplicate of the current buffer, copying all contents.
Bind ensime-buffer-connection and buffer-file-name to the given values.
Execute forms in body in the context of this new buffer. The idea is that
we can abuse this buffer, even saving its contents to disk, and all the
changes will be forgotten."
  `(let ((buf (current-buffer))
	 (file-name buffer-file-name)
	 (p (point))
	 (conn (ensime-current-connection)))

     (unwind-protect
	 (with-temp-buffer
	   (let ((ensime-buffer-connection conn)
		 (buffer-file-name file-name))
	     (insert-buffer-substring buf)
	     (goto-char p)
	     ,@body
	     ))
       ;; Make sure we overwrite any changes
       ;; written from temp buffer.
       (ensime-write-buffer nil t)
       )))


(defun ensime-ac-trunc-summary (str)
  (let ((len (length str)))
    (if (> len 40)
	(concat (substring str 0 40) "...")
      str)))

(defun ensime-ac-get-doc (item)
  "Return doc for given item."
  (format "%s" item))

(defun ensime-ac-candidate-to-insert (item)
  "Return to-insert for given item."
  (get-text-property 0 'to-insert item))

(defun ensime-pt-at-end-of-prev-line ()
  (save-excursion (forward-line -1)
		  (min
		   (- (point) 1)
		   (point-at-eol))))

(defun ensime-ac-completion-prefix ()
  "Starting at current point. Find the point of completion."
  (let ((point (re-search-backward "\\(\\W\\|[\t ]\\)\\([^\\. ]*\\)?"
				   (point-at-bol) t)))
    (if point (1+ point))
    ))


(defun ensime-ac-complete-action ()
  "Defines action to perform when user selects a completion candidate.
If the candidate is a callable symbol, add the meta-info about the
params and param types as text-properties of the completed name. This info will
be used later to give contextual help when entering arguments."

  (let* ((candidate candidate) ;;Grab from dynamic environment..
	 (name candidate)
	 (type-id (get-text-property 0 'type-id candidate))
	 (is-callable (get-text-property 0 'is-callable candidate))
	 (to-insert (ensime-ac-candidate-to-insert candidate))
	 (name-start-point (- (point) (length name))))

    ;; If an alternate to-insert string is available, delete the
    ;; candidate inserted into buffer and replace with to-insert
    (when to-insert
      (delete-char (- (length name)))
      (insert to-insert))

    ;; If this member is callable, use the type-id to lookup call completion
    ;; information to show parameter hints.
    (when is-callable

      (let* ((call-info (ensime-rpc-get-call-completion type-id))
	     (param-sections (ensime-type-param-sections call-info)))
	(when (and call-info param-sections)

	  ;; Insert space or parens depending on the nature of the
	  ;; call
	  (save-excursion
	    (let* ((is-operator
		    (and (= 1 (length param-sections))
			 (= 1 (length (plist-get
				       (car param-sections) :params)))
			 (null (string-match "[A-z]" name)))))
	      (if ensime-ac-enable-argument-placeholders
		  (let ((args (ensime-ac-call-info-argument-list
			       call-info is-operator)))
		    (cond
		     (is-operator (insert (concat " " args)))
		     (t (insert args))))
		(cond
		 (is-operator (insert " "))
		 (t (insert "()"))))))

	  (if (car param-sections)
	      (progn
		;; Save param info as a text properties of the member name..
		(add-text-properties name-start-point
				     (+ name-start-point (length name))
				     (list 'call-info call-info
					   ))

		;; Setup hook function to show param help later..
		(add-hook 'post-command-hook
			  'ensime-ac-update-param-help nil t)
		;; This command should trigger help hook..
		(forward-char))

	    ;; Otherwise, skip to the end
	    (forward-char 2))

	  )))))


(defun ensime-ac-get-active-param-info ()
  "Search backward from point for the param info of the call that
   we are currently completing."
  (save-excursion
    (catch 'return
      (let ((lbound (point-at-bol)) ;; TODO <-- what about multiline param lists
	    (balance 0))
	(backward-char 1)
	(while (> (point) lbound)
	  (cond
	   ((ensime-in-string-or-comment (point)) nil)
	   ((looking-at "\\s)") (decf balance))
	   ((looking-at "\\s(") (incf balance))
	   (t
	    (let ((call-info (get-text-property (point) 'call-info)))
	      (if (and (or (> balance 0)) call-info)
		  (throw 'return (list
				  :name-end-point (point)
				  :call-info call-info))))))
	  (backward-char 1))))))


(defun ensime-ac-update-param-help ()
  "When entering the arguments to a call, display a tooltip
   with the param names and types of the call."
  (let ((info (ensime-ac-get-active-param-info)))
    (if info
	(let* (;; To be used for tooltip positioning..
	       (name-end (plist-get info :name-end-point))
	       (call-info (plist-get info :call-info))
	       (signature (ensime-ac-call-info-signature call-info)))
	  (message signature))
      (remove-hook 'post-command-hook 'ensime-ac-update-param-help t))))


(defun ensime-ac-call-info-argument-list (call-info &optional is-operator)
  "Return a pretty string representation of argument list."
  (let ((param-sections (plist-get call-info :param-sections)))
    (mapconcat
     (lambda (sect)
       (let* ((params (plist-get sect :params))
	      (is-implicit (plist-get sect :is-implicit))
	      (result
	       (concat (if is-operator "" "(")
		       (mapconcat
			(lambda (nm-and-tp)
			  (format
			   "%s:%s"
			   (propertize (car nm-and-tp)
				       'face font-lock-variable-name-face)
			   (propertize (ensime-type-name-with-args
					(cadr nm-and-tp))
				       'face font-lock-type-face)
			   ))
			params ", ") (if is-operator "" ")"))))
	 (if is-implicit
	     (propertize result 'face font-lock-comment-face)
	   result)
	 ))
     param-sections "=>" )))


(defun ensime-ac-call-info-signature (call-info)
  "Return a pretty string representation of a call-info object."
  (let ((param-sections (plist-get call-info :param-sections))
	(result-type (plist-get call-info :result-type)))
    (concat
     (ensime-ac-call-info-argument-list call-info)
     " => "
     (propertize
      (ensime-type-name-with-args result-type)
      'face font-lock-type-face)
     )))


(ac-define-source ensime-completions
  '((document . ensime-ac-get-doc)
    (candidates . (ensime-ac-completion-candidates ac-prefix))
    (prefix . ensime-ac-completion-prefix)
    (action . ensime-ac-complete-action)
    (requires . 0)
    (symbol . "f")
    ))


(defun ensime-ac-enable ()
  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-ensime-completions))

  (make-local-variable 'ac-use-comphist)
  (setq ac-use-comphist nil)

  (make-local-variable 'ac-auto-show-menu)
  (setq ac-auto-show-menu 0.5)

  (make-local-variable 'ac-candidates-cache)
  (setq ac-candidates-cache nil)

  (make-local-variable 'ac-auto-start)
  (setq ac-auto-start nil)

  (make-local-variable 'ac-expand-on-auto-complete)
  (setq ac-expand-on-auto-complete t)

  (make-local-variable 'ac-use-fuzzy)
  (setq ac-use-fuzzy nil)

  (make-local-variable 'ac-dwim)
  (setq ac-dwim nil)

  (make-local-variable 'ac-use-quick-help)
  (setq ac-use-quick-help t)

  (make-local-variable 'ac-delete-dups)
  (setq ac-delete-dups nil)

  (make-local-variable 'ac-ignore-case)
  (setq ac-ignore-case t)

  (make-local-variable 'ac-trigger-key)
  (ac-set-trigger-key "TAB")

  (auto-complete-mode 1)
  )

(defun ensime-ac-disable ()
  (auto-complete-mode 0)
  )

(provide 'ensime-auto-complete)