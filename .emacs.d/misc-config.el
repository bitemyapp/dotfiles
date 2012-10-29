;; Normal config stuff
(global-set-key (kbd "C-z") nil) ; fuck everything about this.

;; Manually sets alt key to meta
(setq x-alt-keysym 'meta)

;; Shut off the obnoxious bell
(setq ring-bell-function 'ignore)

(require 'ido)
(ido-everywhere t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-show-dot-for-dired t)

;; Custom variables for various things
(custom-set-variables
 '(css-electric-keys nil)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(org-support-shift-select (quote always)))

(if (display-graphic-p)
    (progn
      (if (boundp 'tool-bar-mode)
	  (tool-bar-mode -1))

      (if (boundp 'tool-bar-mode)
	  (scroll-bar-mode -1))

      (if (boundp 'tool-bar-mode)
	  (menu-bar-mode -1))))

;; Line and column numbers in mode-line
(if (boundp 'line-number-mode)
    (line-number-mode 1))
(if (boundp 'column-number-mode)
    (column-number-mode 1))

(global-set-key (kbd "C-c d e f") 'describe-face)

(global-set-key (kbd "C-c f r") 'fill-region)

(setq frame-title-format "%b")
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq auto-save-default nil)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))
(global-set-key (kbd "<S-tab>") 'un-indent-by-removing-4-spaces)

;; aliases because I am l'lazy
(defalias 'couc 'comment-or-uncomment-region)
(global-set-key (kbd "C-c c m") 'couc)

(global-set-key (kbd "C-c e r") 'eval-region)

; Alternative to M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

; Artist Mode
(global-set-key (kbd "C-c c a m") 'artist-mode)

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'dired-x)

(defun reset-highlight ()
  (interactive)
  (global-hi-lock-mode 0)
  (global-hi-lock-mode 1))

(defun highlight-this ()
  (interactive)
  (reset-highlight)
  (highlight-regexp (regexp-quote (word-at-point))))

(global-set-key (kbd "C-c r h") 'reset-highlight)
(global-set-key (kbd "C-c h t") 'highlight-this)

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)

(iswitchb-mode 1)
 (defun iswitchb-local-keys ()
      (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
                (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))
    (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

(defun reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))

(defun nuke-newline ()
  (interactive)
  (replace-string " " ""))

(defun nuke-spaces ()
  (interactive)
  (replace-string "?\n" ""))

;; deletes selected text
(delete-selection-mode t)

(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))
    (put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))

(defun duplicate-start-of-line ()
  (let ((text (buffer-substring (point)
                                (beginning-of-thing 'line))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(global-set-key (kbd "C-c <down>") 'duplicate-start-of-line-or-region)

(defun accum (pattern)
  (interactive "sPattern: ")
  (beginning-of-buffer)
  (let ((matched ""))
    (while (re-search-forward pattern nil t)
      (setq matched (concat matched (buffer-substring (match-beginning 1) (match-end 0)) "\n")))
    (switch-to-buffer "*Dump Matched*")
    (concat matched)
    (insert matched)))

(defun delete-line-numbers ()
  (interactive)
  (replace-regexp "^[0-9]* *" "")
)
(global-set-key (kbd "C-c d l n") 'delete-line-numbers)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)

(global-set-key (kbd "M-g") 'goto-line) ; Impatience.

(defun revert-all-buffers ()
   "Refreshes all open buffers from their respective files"
   (interactive)
   (let* ((list (buffer-list))
          (buffer (car list)))
     (while buffer
       (when (buffer-file-name buffer)
         (set-buffer buffer)
         (revert-buffer t t t))
       (setq list (cdr list))
       (setq buffer (car list))))
  (message "Refreshing open files"))

(global-set-key (kbd "C-c r e v") 'revert-all-buffers)

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

(global-set-key (kbd "C-c k o b") 'kill-other-buffers)

(global-set-key (kbd "C-c c p") 'compile)

(global-set-key (kbd "C-c n j i") 'nrepl-jack-in)

(defun unhtml (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      )))

(defun my-fixup-whitespace ()
  (interactive "*")
  (if (or (eolp)
          (save-excursion
            (beginning-of-line)
            (looking-at "^\\s *$")))
      (delete-blank-lines)
      (fixup-whitespace)))

(defun get-point (symbol &optional arg)
      "get the point"
      (funcall symbol arg)
      (point)
     )

(add-hook 'find-file-hook 'find-file-check-line-endings)
(defun dos-file-endings-p ()
  (string-match "dos" (symbol-name buffer-file-coding-system)))

(defun find-file-check-line-endings ()
  (when (dos-file-endings-p)
    (set-buffer-file-coding-system 'undecided-unix)
    (set-buffer-modified-p nil)))

(require 're-builder)
(setq reb-re-syntax 'string) ; elisp/read regex syntax is...undesirable.

(require 'refheap) ; for pasting to refheap
(global-set-key (kbd "C-c r p b") 'refheap-paste-buffer)
(global-set-key (kbd "C-c r p r") 'refheap-paste-region)

(when (fboundp 'winner-mode)
      (winner-mode 1))

; finds hashbang notation, if it does it chmod +x's it upon save.
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

; change yes or no prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Edit Server shortcut
(global-set-key (kbd "C-c d o n e") 'edit-server-done)

;; code blocks highlight natively
(setq org-src-fontify-natively t)
(setq org-support-shift-select t)

;; (require 'generic-x)

;;   '("account" "user"     "password")                     ;; some keywords

;; (define-generic-mode 
;;   'remarkup-mode                         ;; name of the mode to create
;;   '("~~")                           ;; comments start with '!!'
;;   '(("=" . 'font-lock-operator)     ;; '=' is an operator
;;     (";" . 'font-lock-builtin))     ;; ';' is a a built-in 
;;   '("\\.rm$")                      ;; files for which to activate this mode 
;;    nil                              ;; other functions to call
;;   "A mode for remarkup"            ;; doc string for this mode
;; )
