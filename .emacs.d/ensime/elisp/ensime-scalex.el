;;; ensime-scalex.el
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

(require 'json)
(eval-when-compile (require 'cl))
(require 'url)

(defvar ensime-scalex-mode nil
  "Enables the ensime-scalex minor mode.")

(defvar ensime-scalex-buffer-name "*ensime-scalex*"
  "Buffer to use for ensime-scalex.")

(defvar ensime-scalex-target-buffer-name "*ensime-scalex-results*"
  "Buffer name for target-buffer.")

(defvar ensime-scalex-target-buffer nil
  "Buffer to which the ensime-scalex is applied to.")

(defvar ensime-scalex-target-window nil
  "Window to which the ensime-scalex is applied to.")

(defvar ensime-scalex-originating-buffer nil
  "Window from which the search was initiated.")

(defvar ensime-scalex-window-config nil
  "Old window configuration.")

(defvar ensime-scalex-mode-string ""
  "String in mode line for additional info.")

(defvar ensime-scalex-current-results '()
  "The most recent ensime-scalex result list.")

(defvar ensime-scalex-current-selected-result '()
  "The currently selected ensime-scalex result.")

(defvar ensime-scalex-text ""
  "The active filter text.")

(defvar ensime-scalex-min-length 1
  "The minimum length a search must be
 before rpc call is placed..")

(defvar ensime-scalex-max-results 20
  "The max number of results to return per api call.")


(defvar ensime-scalex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-q" 'ensime-scalex-quit)
    (define-key map "\C-c\C-c" 'ensime-scalex-search)
    (define-key map "\C-q" 'ensime-scalex-quit)
    (define-key map "\C-n" 'ensime-scalex-next-match)
    (define-key map "\C-p" 'ensime-scalex-prev-match)
    (define-key map [(return)] 'ensime-scalex-choose-current-result)
    map)
  "Keymap used by ensime-scalex.")

(defvar ensime-scalex-target-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'ensime-scalex-quit)
    (define-key map "\C-n" 'ensime-scalex-next-match)
    (define-key map "\C-p" 'ensime-scalex-prev-match)
    (define-key map [(return)] 'ensime-scalex-choose-current-result)
    map)
  "Keymap used by ensime-scalex.")


(defstruct ensime-scalex-result
  "A ensime-scalex search result.

  * summary
     The full body of text presented in the results list,
     may contain leading and trailing text, in addition to the match.

  * metadata

  * match-file-name
    The filename of the buffer containing the match

  * match-start
    The point in buffer at which the match started

  * match-end
    The point in buffer at which the match ended

  * match-line
    The line number in buffer match started

  * summary-start
    The offset at which summary begins in the results buffer.

  * data
  "
  (summary nil)
  (metadata nil)
  (match-file-name nil)
  (match-start nil)
  (match-end nil)
  (match-line nil)
  (summary-start 0)
  (data nil)
  )


(defun ensime-scalex ()
  "The main entrypoint for ensime-scalex-mode.
   Initiate an incremental search of all live buffers."
  (interactive)
  (ensime-with-conn-interactive
   conn
   (if (and (string= (buffer-name) ensime-scalex-buffer-name)
	    (memq major-mode '(ensime-scalex-mode)))

       (message "Already in ensime-scalex buffer")

     (when buffer-file-name
       (setq ensime-scalex-originating-buffer (current-buffer)))

     (setq ensime-scalex-window-config (current-window-configuration))

     (setq ensime-scalex-target-buffer
	   (switch-to-buffer-other-window
	    (get-buffer-create ensime-scalex-target-buffer-name)))
     (setq ensime-scalex-target-window (selected-window))
     (use-local-map ensime-scalex-target-buffer-map)
     (setq ensime-buffer-connection conn)

     (select-window (split-window (selected-window) (- (window-height) 4)))
     (switch-to-buffer (get-buffer-create ensime-scalex-buffer-name))
     (setq ensime-buffer-connection conn)
     (erase-buffer)
     (ensime-scalex-mode)
     (ensime-scalex-update-target-buffer)
     )))



(defun ensime-scalex-mode ()
  "Major mode for incrementally seaching through all open buffers."
  (interactive)
  (setq major-mode 'ensime-scalex-mode
        mode-name "ensime-scalex-mode")

  (use-local-map ensime-scalex-mode-map)

  (setq	ensime-scalex-mode-string  ""
	ensime-scalex-mode-valid-string ""
	mode-line-buffer-identification
	'(25 . ("%b" ensime-scalex-mode-string ensime-scalex-valid-string)))

  (ensime-scalex-update-modestring)
  (make-local-variable 'ensime-scalex-kill-buffer)
  (add-hook 'kill-buffer-hook 'ensime-scalex-kill-buffer nil t)
  )


(defun ensime-scalex-quit ()
  "Quit the ensime-scalex mode."
  (interactive)
  (kill-buffer ensime-scalex-buffer-name)
  (set-window-configuration ensime-scalex-window-config))


(defun ensime-scalex-choose-current-result ()
  "Jump to the target of the currently selected ensime-scalex-result."
  (interactive)
  (when (and (ensime-scalex-result-p ensime-scalex-current-selected-result)
	     (get-buffer ensime-scalex-buffer-name))
    (switch-to-buffer ensime-scalex-buffer-name)
    (let* ((r ensime-scalex-current-selected-result)
	   (url (ensime-scalex-result-match-file-name r)))
      (browse-url url))))


(defun ensime-scalex-next-match ()
  "Go to next match in the ensime-scalex target window."
  (interactive)
  (if (and ensime-scalex-current-results
	   ensime-scalex-current-selected-result)
      (let* ((i (position ensime-scalex-current-selected-result
			  ensime-scalex-current-results))
	     (len (length ensime-scalex-current-results))
	     (next (if (< (+ i 1) len)
		       (nth (+ i 1) ensime-scalex-current-results)
		     (nth 0 ensime-scalex-current-results))))
	(setq ensime-scalex-current-selected-result next)
	(ensime-scalex-update-result-selection)
	)))


(defun ensime-scalex-insert-import-of-current-result ()
  "Insert an import statement for the currently selected type."
  (interactive)
  (when (and ensime-scalex-current-selected-result
	     ensime-scalex-originating-buffer)
    (let* ((item (ensime-scalex-result-data
		  ensime-scalex-current-selected-result))
	   (qualified-name (ensime-scalex-sym-name item)))
      (with-current-buffer ensime-scalex-originating-buffer
	(insert (format "import %s\n" qualified-name))))))


(defun ensime-scalex-prev-match ()
  "Go to previous match in the ensime-scalex target window."
  (interactive)
  (if (and ensime-scalex-current-results
	   ensime-scalex-current-selected-result)
      (let* ((i (position ensime-scalex-current-selected-result
			  ensime-scalex-current-results))
	     (len (length ensime-scalex-current-results))
	     (next (if (> i 0)
		       (nth (- i 1) ensime-scalex-current-results)
		     (nth (- len 1) ensime-scalex-current-results))))
	(setq ensime-scalex-current-selected-result next)
	(ensime-scalex-update-result-selection)
	)))


(defun ensime-scalex-request-sentinel (proc msg)
  (when (equal (process-status proc) 'closed)
    (let ((buf (process-buffer proc)))
      (when buf
	(with-current-buffer buf
	  (let* ((json
		  (buffer-substring-no-properties
		   (point-min) (point-max)))
		 (parsed (json-read-from-string json)))
	    (when-let (results (cdr (assoc 'results parsed)))
	      (when (buffer-live-p ensime-scalex-target-buffer)
		(let ((results (ensime-scalex-make-results results)))
		  (setq ensime-scalex-current-results results)
		  (ensime-scalex-update-target-buffer)
		  (ensime-event-sig :scalex-buffer-populated)
		  )))
	    )
	  )
	(kill-buffer buf))
      )))

(defun ensime-scalex-api-request (q &optional page per-page)
  "Hit the scalex api."
  (let* ((url (concat
	      "http://api.scalex.org/?q="
	      (url-hexify-string q)
	      "&page=" (number-to-string (or page 1))
	      "&per_page=" (number-to-string
			    (or per-page
				ensime-scalex-max-results))
	      ))
         (buf (url-retrieve-synchronously url)))
    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (let* ((json
              (buffer-substring-no-properties
               (point) (point-max)))
             (parsed (json-read-from-string json)))
        (when-let (results (cdr (assoc 'results parsed)))
                  (when (buffer-live-p ensime-scalex-target-buffer)
                    (let ((results (ensime-scalex-make-results results)))
                      (setq ensime-scalex-current-results results)
                      (ensime-scalex-update-target-buffer)
                      (ensime-event-sig :scalex-buffer-populated))))))
    (kill-buffer buf)))

(defun ensime-scalex-search ()
  "Launch a new search."
  (interactive)
  (let ((new-query (buffer-string)))
    (setq ensime-scalex-text new-query)
    (when (>= (length new-query) ensime-scalex-min-length)
      (ensime-scalex-api-request new-query)
      ;; (with-current-buffer ensime-scalex-target-buffer
      ;;   (setq ensime-scalex-current-results nil)
      ;;   (ensime-scalex-update-target-buffer))
      )
    (force-mode-line-update)))

;;
;; Non-interactive functions below
;;

(defvar ensime-scalex-selection-overlay nil
  "Overlay that highlights the currently selected search result.")

(defun ensime-scalex-update-result-selection ()
  "Move cursor to current result selection in target buffer."
  (when (and ensime-scalex-current-results
	     ensime-scalex-current-selected-result)
    (with-current-buffer ensime-scalex-target-buffer
      (when ensime-scalex-selection-overlay
	(delete-overlay ensime-scalex-selection-overlay))
      (let ((target-point (ensime-scalex-result-summary-start
			   ensime-scalex-current-selected-result)))
	(goto-char target-point)
	(setq ensime-scalex-selection-overlay
	      (ensime-make-overlay target-point (point-at-eol)
				   nil 'ensime-warnline))
	(set-window-point (ensime-window-showing-buffer
			   ensime-scalex-target-buffer)
			  target-point)
	)
      )))


(defun ensime-scalex-assert-buffer-in-window ()
  "Assert that `ensime-scalex-target-buffer' is displayed in
 `ensime-scalex-target-window'."
  (if (not (eq ensime-scalex-target-buffer
	       (window-buffer ensime-scalex-target-window)))
      (set-window-buffer ensime-scalex-target-window
			 ensime-scalex-target-buffer)))

(defun ensime-scalex-update-modestring ()
  "Update the variable `ensime-scalex-mode-string' displayed in the mode line."
  (force-mode-line-update))

(defun ensime-scalex-kill-buffer ()
  "When the ensime-scalex buffer is killed, kill the target buffer."
  (remove-hook 'kill-buffer-hook 'ensime-scalex-kill-buffer)
  (if (buffer-live-p ensime-scalex-target-buffer)
      (kill-buffer ensime-scalex-target-buffer)))

(defun ensime-scalex-buffers-to-search ()
  "Return the list of buffers that are suitable for searching."
  (let ((all-buffers (buffer-list)))
    (remove-if
     (lambda (b)
       (let ((b-name (buffer-name b)))
	 (or (null (buffer-file-name b))
	     (equal b-name ensime-scalex-target-buffer-name)
	     (equal b-name ensime-scalex-buffer-name)
	     (equal b-name "*Messages*"))))
     all-buffers)))


(defun ensime-scalex-make-results (info)
  "Map the results of the rpc call into search result
 structures."
  (let ((items info))
    (mapcar
     (lambda (item)
       (make-ensime-scalex-result
	:summary
	(concat
	 (cdr (assoc 'qualifiedName item))
	 "\n"
	 (ensime-add-face 'font-lock-type-face
			  (cdr (assoc 'signature item)))

	 (let ((dep (cdr (assoc 'deprecation item))))
	   (if dep
	       (concat
		"\n"
		(ensime-add-face 'ensime-errline-highlight
				 (format "DEPRECATED:(%s)"
					 (cdr (assoc 'txt dep)))))
	     ""))

	 (let ((comment (cdr (assoc 'comment item))))
	   (if comment
	       (concat
		"\n"
		(ensime-add-face 'font-lock-comment-face
				 (cdr (assoc
				       'txt
				       (cdr (assoc 'short comment))))
				 ))
	     ""))

	 )

	:metadata
	""

	:match-file-name
	(cdr (assoc 'docUrl item))

	:data
	item

	)) items)))


(defun ensime-scalex-update-target-buffer ()
  "This is where the magic happens. Update the result list."
  (save-excursion
    (set-buffer ensime-scalex-target-buffer)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (erase-buffer)
    (ensime-insert-with-face
     (concat "Enter space-separated keywords. "
	     "C-c C-c to search. "
	     "C-n, C-p to navigate.\n"
	     "RETURN to browse documention. C-q to quit.")
     'font-lock-constant-face)
    (insert "\n")
    (ensime-insert-with-face
     (format "%s results" (length ensime-scalex-current-results))
     font-lock-comment-face)
    (insert "\n\n")

    (when ensime-scalex-current-results
      (setq ensime-scalex-current-selected-result
	    (first ensime-scalex-current-results)))


    (dolist (r ensime-scalex-current-results)

      ;; Save this for later use, for next/prev actions
      (setf (ensime-scalex-result-summary-start r) (point))

      (let ((p (point))
	    (text (ensime-scalex-result-summary r)))
	;; Insert the actual text, highlighting the matched substring
	(insert (format "%s  \n" text))
	(ensime-scalex-highlight-matches text p))

      (insert "\n\n")
      )

    (setq buffer-read-only t)
    (ensime-scalex-update-result-selection)
    ))


(defun ensime-scalex-highlight-matches (text start-pt)
  (let ((keywords (split-string ensime-scalex-text " ")))
    (dolist (key keywords)
      (let ((start (string-match key text))
	    (len (length key)))
	(when (integerp start)
	  (add-text-properties
	   (+ start-pt start)
	   (+ start-pt start len)
	   '(comment nil face font-lock-keyword-face))))
      )))


(provide 'ensime-scalex)
