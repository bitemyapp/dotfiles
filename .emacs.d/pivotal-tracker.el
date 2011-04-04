;; pivotal-tracker.el
;; Author: John Andrews <john.m.andrews@gmail.com>
;; Created: 2010.11.14

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;; Usage

;; Before using the tracker you must customize your pivotal API key. 
;; You can obtain the key from the 'My Profile' link in the Pivotal Tracker
;; web application.
;; M-x customize-group RET pivotal RET

;; Issues + Feature Requests

;; Development is hosted on github
;; https://github.com/jxa/pivotal-tracker.git

(require 'xml)
(require 'url)

(defgroup pivotal nil
  "Pivotal Tracker")

(defcustom pivotal-api-token ""
  "API key found on the /profile page of pivotal tracker"
  :group 'pivotal
  :type 'string)

(defconst pivotal-base-url "http://www.pivotaltracker.com/services/v3"
  "format string to use when creating endpoint urls")

(defconst pivotal-states `("unstarted" "started" "finished" "delivered" "accepted" "rejected")
  "story status will be one of these values")

(defconst pivotal-current-iteration-number -1)

(defvar *pivotal-current-project*)
(defvar *pivotal-iteration* pivotal-current-iteration-number)

;;;;;;;; INTERACTIVE USER FUNS

(defun pivotal ()
  "launch pivotal-projects window, or just switch to it"
  (interactive)
  (let ((buffer (get-buffer "*pivotal-projects*")))
    (if buffer
        (switch-to-buffer buffer)
      (pivotal-get-projects))))

(defun pivotal-get-projects ()
  "show a buffer of all projects you have access to"
  (interactive)
  (assert-pivotal-api-token)
  (pivotal-api (pivotal-url "projects") "GET" 'pivotal-projects-callback))

(defun pivotal-get-current ()
  "show a buffer of all stories in the currently selected iteration"
  (interactive)
  (pivotal-get-iteration *pivotal-iteration*))

(defun pivotal-get-iteration (iteration)
  (let ((query-string (if (= pivotal-current-iteration-number iteration)
                          "iterations/current"
                        (format "iterations/backlog?offset=%s&limit=1" iteration))))

    (assert-pivotal-api-token)
    (pivotal-api (pivotal-url "projects" *pivotal-current-project* query-string)
                 "GET"
                 'pivotal-iteration-callback)))

(defun pivotal-next-iteration ()
  "replace iteration view with the next upcoming iteration"
  (interactive)
  (setq *pivotal-iteration* (+ 1 *pivotal-iteration*))
  (pivotal-get-iteration *pivotal-iteration*))

(defun pivotal-previous-iteration ()
  "replace iteration view with previous iteration. if you try to go before 0 it just reloads current"
  (interactive)
  (setq *pivotal-iteration*
        (if (= pivotal-current-iteration-number *pivotal-iteration*)
            pivotal-current-iteration-number
          (- *pivotal-iteration* 1)))
  (pivotal-get-iteration *pivotal-iteration*))

(defun pivotal-set-project ()
  "set the current project, and load the current iteration for that project"
  (interactive)
  (setq *pivotal-current-project*
        (progn
          (beginning-of-line)
          (re-search-forward "\\([0-9]+\\)" (point-at-eol))
          (match-string 1)))
  (pivotal-get-current))

(defun pivotal-get-story (id)
  "Open a single story for view / edit"
  (interactive)
  (assert-pivotal-api-token)
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" id)
               "GET"
               'pivotal-story-callback))

(defun pivotal-toggle-visibility ()
  "show/hide story detail"
  (interactive)
  (progn
    (let ((cur-invisible (member (pivotal-story-at-point) buffer-invisibility-spec)))
      (if cur-invisible
          (pivotal-show)
        (pivotal-hide)))
    (force-window-update (current-buffer))))

(defun pivotal-estimate-story (estimate)
  "assign an estimate to the story on the current line"
  (interactive "NEstimate: ")
  (message "going to set estimate to %s" estimate)
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point))
             "PUT"
             'pivotal-update-current-story
             (format "<story><estimate>%s</estimate></story>" estimate)))

(defun pivotal-set-status ()
  "transition status according to the current status. assigns the story to user."
  (interactive)
  (let ((new-state (completing-read "Status: " pivotal-states nil t)))
    (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point))
                 "PUT"
                 'pivotal-update-current-story
                 (format "<story><current_state>%s</current_state></story>" new-state))))

(defun pivotal-add-comment (comment)
  "prompt user for comment and add it to the current story"
  (interactive "sAdd Comment: ")
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point) "notes")
               "POST"
               'pivotal-add-comment-callback
               (format "<note><text>%s</text></note>" (xml-escape-string comment))))


;;;;;;;; CALLBACKS


(defun pivotal-iteration-callback (status)
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (with-current-buffer (get-buffer-create "*pivotal-iteration*")
      (pivotal-mode)
      (delete-region (point-min) (point-max))
      (switch-to-buffer (current-buffer))
      (pivotal-insert-iteration xml))))

(defun pivotal-projects-callback (status)
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (with-current-buffer (get-buffer-create "*pivotal-projects*")
      (pivotal-project-mode)
      (delete-region (point-min) (point-max))
      (switch-to-buffer (current-buffer))
      (pivotal-insert-projects xml))))

(defun pivotal-story-callback (status)
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (delete-region (point-min) (point-max))
    (insert (pivotal-format-story xml)) (rename-buffer (concat "*pivotal-" (pivotal-story-attribute xml 'id) "*"))
    (switch-to-buffer (current-buffer))))

(defun pivotal-update-current-story (status)
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (if (eq :error (car status))
        (message "Error: %s" (pivotal-parse-errors xml))
      (with-current-buffer (get-buffer-create "*pivotal-iteration*")
        (pivotal-remove-story-at-point)
        (pivotal-insert-story xml)))))

(defun pivotal-add-comment-callback (status)
  (let* ((xml (pivotal-get-xml-from-current-buffer))
         (comment (pivotal-format-comment (car xml))))
    (with-current-buffer (get-buffer-create "*pivotal-iteration*")
      (pivotal-append-to-current-story comment))))

(defun pivotal-parse-errors (xml)
  (mapconcat (lambda (error)
               (car (last error)))
             (xml-get-children (car xml) 'error)
             " "))



;;;;;;;; MODE DEFINITIONS



(defconst pivotal-font-lock-keywords
  `(("^\\(\\[.*?\\]\\)+" 0 font-lock-doc-face)
    ("^\\!\\(.*?\\)\\!$") 0 font-lock-keyword-face))

(define-derived-mode pivotal-mode fundamental-mode "Pivotal" 
  (suppress-keymap pivotal-mode-map)
  (define-key pivotal-mode-map (kbd "t") 'pivotal-toggle-visibility)
  (define-key pivotal-mode-map (kbd "C-m") 'pivotal-toggle-visibility)
  (define-key pivotal-mode-map (kbd "R") 'pivotal-get-current)
  (define-key pivotal-mode-map (kbd "n") 'next-line)
  (define-key pivotal-mode-map (kbd "p") 'previous-line)
  ;; add some bindings for my vim friends
  (define-key pivotal-mode-map (kbd "j") 'next-line)
  (define-key pivotal-mode-map (kbd "k") 'previous-line)

  (define-key pivotal-mode-map (kbd "N") 'pivotal-next-iteration)
  (define-key pivotal-mode-map (kbd "P") 'pivotal-previous-iteration)
  (define-key pivotal-mode-map (kbd "E") 'pivotal-estimate-story)
  (define-key pivotal-mode-map (kbd "C") 'pivotal-add-comment)
  (define-key pivotal-mode-map (kbd "S") 'pivotal-set-status)
  (define-key pivotal-mode-map (kbd "L") 'pivotal)
  (setq font-lock-defaults '((pivotal-font-lock-keywords) nil t))
  (font-lock-mode))

(define-derived-mode pivotal-project-mode fundamental-mode "PivotalProjects" 
  (suppress-keymap pivotal-project-mode-map)
  (define-key pivotal-project-mode-map (kbd "R") 'pivotal-get-projects)
  (define-key pivotal-project-mode-map (kbd "n") 'next-line)
  (define-key pivotal-project-mode-map (kbd "p") 'previous-line)
  ;; add some bindings for my vim friends
  (define-key pivotal-project-mode-map (kbd "j") 'next-line)
  (define-key pivotal-project-mode-map (kbd "k") 'previous-line)

  (define-key pivotal-project-mode-map (kbd ".") 'pivotal-set-project)
  (define-key pivotal-project-mode-map (kbd "C-m") 'pivotal-set-project))


;;;;;;;;; SUPPORTING FUNS


(defun pivotal-url (&rest parts-of-url)
  (apply 'concat
         pivotal-base-url
         (mapcar (lambda (part) (concat "/" part)) parts-of-url)))

(defun pivotal-api (url method callback &optional xml-data)
  (let ((url-request-method method)
        (url-request-data xml-data)
        (url-request-extra-headers `(("X-TrackerToken" . ,pivotal-api-token)
                                     ("Content-Type" . "application/xml"))))
    (url-retrieve url callback)))

(defun assert-pivotal-api-token ()
  (assert (not (string-equal "" pivotal-api-token)) t "Please set pivotal-api-token: M-x customize-group RET pivotal RET"))

(defun pivotal-get-xml-from-current-buffer ()
  (let ((xml (cdr (xml-parse-fragment))))
    (kill-buffer)
    xml))

(defun pivotal-insert-projects (xml)
  "render projects one per line in their own buffer"
  (let ((projects (pivotal-get-project-data xml)))
    (mapc (lambda (project)
            (insert (format "%7.7s %s\n" (car project) (cadr project))))
          projects)))

(defun pivotal-get-project-data (xml)
  "return a list of (id name) pairs"
  (mapcar (lambda (proj)
            (list (pivotal-element-value proj 'id)
                  (pivotal-element-value proj 'name)))
          (xml-get-children (car xml) 'project)))

(defun pivotal-insert-iteration (iteration-xml)
  "extract story information from xml and insert it into current buffer"
  (insert (if (= pivotal-current-iteration-number *pivotal-iteration*)
              "! CURRENT ITERATION !\n"
            (format "! ITERATION %s !\n" *pivotal-iteration*)))
  (mapc 'pivotal-insert-story
        (pivotal-extract-stories-from-iteration-xml iteration-xml)))

(defun pivotal-insert-story (story)
  "insert single story into current buffer"
  (let* ((start-point (point))
         (_ (insert (pivotal-format-story-oneline story)))
         (end-of-oneline (point))
         (_ (insert (pivotal-format-story story)))
         (end-of-detail (point)))
    (pivotal-mark-story start-point end-of-detail (pivotal-story-attribute story 'id))
    (pivotal-mark-invisibility end-of-oneline end-of-detail)
    (pivotal-hide end-of-oneline)))

(defun pivotal-append-to-current-story (text)
  (progn
    (pivotal-show)
    (let* ((story-id (pivotal-story-id-at-point (point)))
           (bounds (pivotal-story-boundaries (point)))
           (story-end (second bounds))
           (_ (goto-char story-end))
           (_ (insert text))
           (new-end (point)))
      (pivotal-mark-story story-end new-end story-id)
      (pivotal-mark-invisibility story-end new-end))))

(defun pivotal-invisibility-id (story-id)
  (intern (concat "pivotal-" story-id)))

(defun pivotal-mark-story (min max story-id)
  (put-text-property min max 'pivotal-story-id story-id))

(defun pivotal-mark-invisibility (min max)
  (let ((overlay (make-overlay min max)))
    (overlay-put overlay 'invisible (pivotal-story-at-point min))))

(defun pivotal-hide (&optional position)
  (add-to-invisibility-spec (pivotal-story-at-point position)))

(defun pivotal-show (&optional position)
  (remove-from-invisibility-spec (pivotal-story-at-point position)))

(defun pivotal-story-at-point (&optional position)
  (let* ((buf-point (if position position (point)))
         (story-id (get-text-property buf-point 'pivotal-story-id))
         (invis-id (pivotal-invisibility-id story-id)))
    invis-id))

(defun pivotal-story-id-at-point (&optional position)
  (let* ((story-sym (pivotal-story-at-point position))
         (story-str (symbol-name story-sym)))
    (string-match "pivotal-\\([0-9]+\\)" story-str)
    (match-string 1 story-str)))

(defun pivotal-format-story (story)
  (format "
%s
---
%s #%s
Status:       %s
Requested By: %s
Owned By:     %s
--- Description
%s
--- Comments
%s
"
          (pivotal-story-attribute story 'name)
          (pivotal-story-attribute story 'story_type)
          (pivotal-story-attribute story 'id)
          (pivotal-story-attribute story 'current_state)
          (pivotal-story-attribute story 'requested_by)
          (pivotal-story-attribute story 'owned_by)
          (pivotal-story-attribute story 'description)
          (pivotal-comments story)))

(defun pivotal-format-story-oneline (story)
  (let ((owner (pivotal-story-attribute story 'owned_by))
        (estimate (pivotal-story-attribute story 'estimate))
        (story-name (pivotal-story-attribute story 'name))
        (status (pivotal-story-attribute story 'current_state)))
    (format "[%4.4s][%1.1s][%9.9s] %.80s\n" owner estimate status story-name)))

(defun pivotal-remove-story-at-point ()
  "delete all characters that belong to the current story. Put point at the first char of the next story."
  (interactive)
  (let* ((bounds (pivotal-story-boundaries (point)))
         (first-point (first bounds))
         (last-point (second bounds)))
    (delete-region first-point last-point)
    (if (< (point) (point-max))
        (forward-char))))

(defun pivotal-story-boundaries (point)
  (let ((story-id (get-text-property (point) 'pivotal-story-id))
        (first-point (point))
        (last-point (point)))
    (while (pivotal-point-has-story-id (- first-point 1) story-id)
      (setq first-point (- first-point 1)))
    (while (pivotal-point-has-story-id (+ last-point 1) story-id)
      (setq last-point (+ last-point 1)))
    (list first-point last-point)))

(defun pivotal-point-has-story-id (point story-id)
  (if (and (<= point (point-max)) (>= point (point-min)))
      (string-equal (get-text-property point 'pivotal-story-id) story-id)
    nil))

(defun pivotal-extract-stories-from-iteration-xml (iteration-xml)
  (let ((stories (pivotal-xml-collection (car iteration-xml) `(iteration stories story))))
    (sort stories 'pivotal-sort-stories)))

(defun pivotal-sort-stories (story1 story2)
  (<= (pivotal-display-priority story1) (pivotal-display-priority story2)))

(defun pivotal-display-priority (story)
  (case (intern (pivotal-story-attribute story 'current_state))
    (accepted  1)
    (delivered 2)
    (finished  3)
    (started   4)
    (unstarted 5)
    (otherwise 6)))

(defun pivotal-story-attribute (xml attribute)
  (let*
      ((story (if (eq 'story (car xml))
                  xml
                (car xml)))
       (value (pivotal-element-value story attribute)))
    (if (symbolp value)
        (symbol-name value)
      value)))

(defun pivotal-element-value (xml element)
  (let ((node (xml-get-children xml element)))
    (caddar node)))

(defun pivotal-xml-collection (xml structure)
  "return a collection of nodes found by the given structure"
  (let ((results nil)
        (node xml))
    (mapc (lambda (element)
            (progn
              (setq results (xml-get-children node element))
              (setq node (first results))))
          structure)
    results))
  
(defun pivotal-comments (story)
  (let ((notes (pivotal-xml-collection story `(notes note)))
        (comments ""))
    (mapcar (lambda (note)
              (setq comments (concat comments (pivotal-format-comment note))))
            notes)
    comments))

(defun pivotal-format-comment (note)
  (format "%s  --  %s at %s\n"
          (pivotal-element-value note 'text)
          (pivotal-element-value note 'author)
          (pivotal-element-value note 'noted_at)))

(provide 'pivotal-tracker)




;;;;;;;;; TEST CODE


(when nil

  (global-set-key (kbd "C-=") (lambda () (interactive) (message "Point: %S. Story: %S" (point) (get-text-property (point) 'pivotal-story-id))))

  (defun load-test-xml (file)
    (with-current-buffer (find-file-noselect file)
      (let ((xml (cdr (xml-parse-fragment))))
        (kill-buffer)
        xml)))

  (let* ((xml (load-test-xml "iterations.xml")))
    (pivotal-comments (first (pivotal-extract-stories-from-iteration-xml xml))))
  
  (let ((xml (load-test-xml "iterations.xml")))
    (pivotal-xml-collection (first xml) `(iteration stories story notes note)))

  (defun pivotal-test-callback (status)
    (message "%s" status))

  (pivotal-api (pivotal-url "")
               "GET"
               'pivotal-status-callback)

  (defun testr (i)
    (message "%s" i))

  (mapc 'testr `(one two three))

  )


