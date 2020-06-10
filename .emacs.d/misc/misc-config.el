;; (require 'f)

;; Normal config stuff
(global-set-key (kbd "C-z") nil) ; fuck everything about this.

(global-set-key (kbd "M-*") 'pop-tag-mark)

(defun expand-yasnippet-acute ()
  "Expand the yasnippet named `foobar'."
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "acute")))

(global-set-key (kbd "C-'") 'expand-yasnippet-acute)

(defun expand-yasnippet-ae ()
  "Expand the yasnippet named `foobar'."
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "ae")))

(global-set-key (kbd "C-c c a e") 'expand-yasnippet-ae)

;; There's something similar in vc-git.el: vc-git-grep.  Naturally,
;; though, I prefer this :)

;; --perl-regexp seems to be the most flexible flavor; in particular,
;; it's the only one I know of that supports '\b' to mean "word
;; boundary".  Unfortunately, not all gits have the requisite support;
;; for example, on OS X with homebrew, I had to pass "--with-pcre" to
;; "brew install git".

;; -I means don't search through binary files

;; --no-color, oddly enough, is required to allow emacs to colorize the output

(defcustom git-grep-switches "--perl-regexp -I -n --no-color --ignore-case"
  "Switches to pass to `git grep'."
  :type 'string)

(defcustom git-grep-default-work-tree (expand-file-name "~")
  "Top of your favorite git working tree.  \\[git-grep] will search from here if it cannot figure out where else to look."
  :type 'directory
  )

(when (require 'vc-git nil t)

  ;; Uncomment this to try out the built-in-to-Emacs function.
  ;;(defalias 'git-grep 'vc-git-grep)

  (defun git-grep (command-args)
    "Run `git-grep' in the current git repo.

You can hit \\<grep-mode-map>\\[recompile] to refresh the buffer if you've saved changes to some files."
    (interactive
     (let ((root (vc-git-root default-directory)))
       (when (not root)
         (setq root git-grep-default-work-tree)
         (message "git-grep: %s doesn't look like a git working tree; searching from %s instead" default-directory root))
       (list (read-shell-command "Run git-grep (like this): "
                                 (format (concat
                                          "cd %s && "
                                          "git grep %s -e %s")
                                         (shell-quote-argument (expand-file-name root))
                                         git-grep-switches

                                         ;; don't snarf stuff from the
                                         ;; buffer if we're not
                                         ;; looking at a file.
                                         ;; Perhaps we should also
                                         ;; check to see if the file
                                         ;; is part of a git repo.
                                         (let ((thing (and buffer-file-name (thing-at-point 'symbol))))
                                           (or (and thing (progn
                                                            (set-text-properties 0 (length thing) nil thing)
                                                            (shell-quote-argument (regexp-quote thing))))
                                               "")))
                                 'git-grep-history))))
    (let ((grep-use-null-device nil)
          (process-environment (cons "PAGER=cat" process-environment)))
      (grep command-args))))

(global-set-key (kbd "C-c g") 'git-grep)

;; (defun ggrep ()
;;   "Optionally prompt user to enter a grep string X, if nothing is the current target."
;;   (interactive)
;;   (projectile-grep (thing-at-point 'word) (f--traverse-upwards (f-exists? (f-expand ".git" it)) (f-dirname (f-this-file)))))


;; Manually sets alt key to meta
(setq x-alt-keysym 'meta)
(setq mac-command-modifier 'super)

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
 '(shift-select-mode t)
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
(auto-fill-mode -1)
(setq line-move-visual nil)
(setq org-support-shift-select 'always)

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

;; (require 'refheap) ; for pasting to refheap
;; (global-set-key (kbd "C-c r p b") 'refheap-paste-buffer)
;; (global-set-key (kbd "C-c r p r") 'refheap-paste-region)

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

(show-paren-mode 1)

(global-set-key (kbd "C-c r g") 'rgrep)

(global-set-key (kbd "C-c g") 'rope-goto-definition)

(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer) 
      (recompile))))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook)))

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; (require 'gimme-cat)
;; (global-set-key (kbd "C-c c a t") 'gimme-cat)
;; (global-set-key (kbd "C-c k c a t") 'close-gimmecat-buffers)

;; (require 'find-file-in-project)
;; (global-set-key (kbd "C-x f") 'find-file-in-project)
(require 'find-file-in-repository)
(global-set-key (kbd "C-x f") 'find-file-in-repository)

(fset 'css-after
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 59 13 right backspace return tab] 0 "%d")) arg)))
(global-set-key (kbd "C-c C-a") 'css-after)
(fset 'css-pre
   [?\C-s ?\{ ?\C-m return tab])
(global-set-key (kbd "C-c C-b") 'css-pre)

;; (require 'multi-term)
;; (setq multi-term-program "/bin/zsh")

;; (require 'dash-at-point)
;; (autoload 'dash-at-point "dash-at-point"
;;             "Search the word at point with Dash." t nil)
;; (global-set-key "\C-cd" 'dash-at-point)

(setq-default indent-tabs-mode nil)

(defun my/clean-buffer-formatting ()
  "Indent and clean up the buffer"
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

(global-set-key "\C-cn" 'my/clean-buffer-formatting)

(define-minor-mode my/pair-programming-mode
  "Toggle visualizations for pair programming.

Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Pairing"
  ;; The minor mode bindings.
  '()
  :group 'my/pairing
  (linum-mode (if my/pair-programming-mode 1 -1)))

(define-global-minor-mode my/global-pair-programming-mode
  my/pair-programming-mode
  (lambda () (my/pair-programming-mode 1)))

(global-set-key "\C-c\M-p" 'my/global-pair-programming-mode)

;; Sticky Windows
;; (require 'sticky-windows)
;; (global-set-key     [(control x) (?0)]        'sticky-window-delete-window)
;; (global-set-key     [(control x) (?1)]        'sticky-window-delete-other-windows)
;; (global-set-key     [(control x) (?9)]        'sticky-window-keep-window-visible)

;; Uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defmacro with-system (type &rest body)
  "Evaluate body if `system-type' equals type."
  `(when (eq system-type ,type)
     ,@body))


(defun string-starts-with (string prefix)
  "Returns non-nil if string STRING starts with PREFIX, otherwise nil."
  (and (>= (length string) (length prefix))
       (string-equal (substring string 0 (length prefix)) prefix)))

(defadvice display-warning
    (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
  "Ignore the warning about the `.emacs.d' directory being in `load-path'."
  (unless (and (eq type 'initialization)
               (string-starts-with message "Your `load-path' seems to contain\nyour `.emacs.d' directory"))
    ad-do-it))

(defun texttt (&optional arg)
  "wraps word in texttt"
  (interactive "p") (kmacro-exec-ring-item (quote ([C-left 92 116 101 120 116 116 116 123 C-right 125] 0 "%d")) arg))
(global-set-key (kbd "C-c t t") 'texttt)

(defun emph (&optional arg)
  "wraps word in emph"
  (interactive "p") (kmacro-exec-ring-item (quote ([C-left 92 101 109 112 104 123 C-right 125] 0 "%d")) arg))
(global-set-key (kbd "C-c e e") 'emph)

(defun emph (&optional arg)
  "wraps word in emph"
  (interactive "p")
  (replace-string "*" "\asterisk{}")
  (replace-string "ae" "{\ae}")
  (replace-string "á" "\\'{a}")
  (replace-string "í" "\\'{e}")
  (replace-string "í" "\\'{i}")
  (replace-string "ó" "\\'{o}")
  (replace-string "í" "\\'{u}")
  (replace-string "í" "\\'{i}")
  (replace-string "í" "\\'{i}")
  (replace-string "í" "\\'{i}")
  (replace-string "í" "\\'{A}")
  (replace-string "í" "\\'{E}")
  (replace-string "í" "\\'{I}")
  (replace-string "Ó" "\\'{O}")
  (replace-string "Ó" "\\'{U}")
  )
;; (require 'cc-mode)
;; (require 'graphviz-dot-mode)
