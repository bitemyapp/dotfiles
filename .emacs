(global-set-key (kbd "C-z") nil)

(defvar safe-load-error-list ""
        "*List of files that reported errors when loaded via safe-load")

(defun safe-load (file &optional noerror nomessage nosuffix)
  "Load a file.  If error when loading, report back, wait for
   a key stroke then continue on"
  (interactive "f")
  (condition-case nil (load file noerror nomessage nosuffix)
    (error
      (progn
       (setq safe-load-error-list  (concat safe-load-error-list  " " file))
       (message "****** [Return to continue] Error loading %s" safe-load-error-list )
        (sleep-for 1)
       nil))))

(defun safe-load-check ()
 "Check for any previous safe-load loading errors.  (safe-load.el)"
  (interactive)
  (if (string-equal safe-load-error-list "") ()
               (message (concat "****** error loading: " safe-load-error-list))))

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.2")
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(add-to-list 'load-path "~/.emacs.d/haskell-mode")

(require 'scala-mode-auto)

(load "~/.emacs.d/haskell-mode/haskell-mode.el")
(require 'haskell-mode)
(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

; manually sets alt key to meta
(setq x-alt-keysym 'meta)

(setq ring-bell-function 'ignore)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(css-electric-keys nil)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(org-support-shift-select (quote always))
 '(pivotal-api-token "8ce844bfbc3de5022ac77fba060f3cd2"))

(if (boundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (boundp 'tool-bar-mode)
    (scroll-bar-mode -1))

(if (boundp 'tool-bar-mode)
    (menu-bar-mode -1))

; line and column numbers in mode-line
(if (boundp 'line-number-mode)
    (line-number-mode 1))
(if (boundp 'column-number-mode)
    (column-number-mode 1))

(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)


(setq frame-title-format "%b")
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq auto-save-default nil)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; aliases because I am l'lazy
(defalias 'couc 'comment-or-uncomment-region)
(global-set-key (kbd "C-c c m") 'couc)

(global-set-key (kbd "C-c e r") 'eval-region)

; Alternative to M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

; Artist Mode
(global-set-key (kbd "C-c c a m") 'artist-mode)

(setq tramp-default-method "scpc")
(transient-mark-mode 1)
(setq x-select-enable-clipboard t)

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'ido)
(ido-everywhere t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-show-dot-for-dired t)

(require 'dired-x)

(require 'tabbar)
(if (not tabbar-mode)
	(tabbar-mode))
(setq tabbar-buffer-groups-function
	(lambda ()
    (list "All Buffers")))
(setq tabbar-buffer-list-function
	(lambda ()
     	  (remove-if
     	   (lambda(buffer)
     	     (find (aref (buffer-name buffer) 0) " *"))
     	   (buffer-list))))

(global-set-key (kbd "s-{") 'tabbar-backward)
(global-set-key (kbd "s-}") 'tabbar-forward)

;; Fixed sudo/ssh multi-hop
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(eval-after-load "tramp"
  '(progn
     (defvar sudo-tramp-prefix
       "/sudo:"
       (concat "Prefix to be used by sudo commands when building tramp path "))
     (defun sudo-file-name (filename)
       (set 'splitname (split-string filename ":"))
       (if (> (length splitname) 1)
         (progn (set 'final-split (cdr splitname))
                (set 'sudo-tramp-prefix "/sudo:")
                )
         (progn (set 'final-split splitname)
                (set 'sudo-tramp-prefix (concat sudo-tramp-prefix "root@localhost:")))
         )
       (set 'final-fn (concat sudo-tramp-prefix (mapconcat (lambda (e) e) final-split ":")))
       (message "splitname is %s" splitname)
       (message "sudo-tramp-prefix is %s" sudo-tramp-prefix)
       (message "final-split is %s" final-split)
       (message "final-fn is %s" final-fn)
       (message "%s" final-fn)
       )

     (defun sudo-find-file (filename &optional wildcards)
       "Calls find-file with filename with sudo-tramp-prefix prepended"
       (interactive "fFind file with sudo ")
       (let ((sudo-name (sudo-file-name filename)))
         (apply 'find-file
                (cons sudo-name (if (boundp 'wildcards) '(wildcards))))))

     (defun sudo-reopen-file ()
       "Reopen file as root by prefixing its name with sudo-tramp-prefix and by clearing buffer-read-only"
       (interactive)
       (let*
           ((file-name (expand-file-name buffer-file-name))
            (sudo-name (sudo-file-name file-name)))
         (progn
           (setq buffer-file-name sudo-name)
           (rename-buffer sudo-name)
           (setq buffer-read-only nil)
           (message (concat "File name set to " sudo-name)))))

     (global-set-key (kbd "C-c o s") 'sudo-reopen-file)))

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

(require 'ruby-mode)

(setq auto-mode-alist (cons '("Gemfile" . ruby-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode))

(add-to-list 'load-path "which-folder-ace-jump-mode-file-in/")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-f") 'ace-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python dev stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
          interpreter-mode-alist)

      python-mode-hook
      '(lambda () (progn
           (set-variable 'py-indent-offset 4)
           (set-variable 'py-smart-indentation t)
           (set-variable 'indent-tabs-mode nil) )))

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; ;; (autoload 'pymacs-apply "pymacs")
;; ;; (autoload 'pymacs-call "pymacs")
;; ;; (autoload 'pymacs-eval "pymacs" nil t)
;; ;; (autoload 'pymacs-exec "pymacs" nil t)
;; ;; (autoload 'pymacs-load "pymacs" nil t)
;; (setq ropemacs-enable-autoimport 't)

(global-set-key (kbd "C-M-n") 'next-error)

(setq ipython-command "/usr/local/bin/ipython")
(require 'ipython)

(require 'auto-complete)
(global-auto-complete-mode t)

;; end python dev

(require 'color-grep)

(require 'markdown-mode)
(add-to-list 'auto-mode-alist
	     '("\\.md$" . markdown-mode))
(add-hook 'text-mode-hook (lambda ()
			    (turn-on-auto-fill)
			    (setq-default line-spacing 5)
			    (setq indent-tabs-mode nil)))

(require 'rst)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'rect-mark)

;;; Magit
(require 'magit)
(global-set-key (kbd "C-c m g") 'magit-status)

(require 'coffee-mode)

(defun reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))


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

(defun deactivate-hacker-type ()
  (interactive)
  (define-key fake-hacker-type-map [remap self-insert-command] nil)
)

(defun inject_contents (&optional n)
  (interactive)
  (setq end (+ start insert_by))
  (insert-file-contents filename nil start end)
  (forward-char insert_by)
  (setq start (+ start insert_by))
)

(defun hacker-type (arg)
  (interactive (list (read-file-name "Filename: ")))
  (setq fake-hacker-type-map (make-sparse-keymap))
  (setq filename arg)
  (setq start 0)
  (setq insert_by 3)

  (define-key fake-hacker-type-map [remap self-insert-command] 'inject_contents)
  (use-local-map fake-hacker-type-map)
)

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

;; keyboard scroll one line at a time
(setq scroll-step 1)

(require 'go-mode-load)

(defun ipdb ()
    (interactive)
    (insert "import sys; sys.stdout = sys.__stdout__; import ipdb; ipdb.set_trace()"))

(global-set-key (kbd "C-c p d b") 'ipdb)
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

(global-set-key (kbd "C-c c j i") 'clojure-jack-in)

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

(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(require 'yaml-mode)

(safe-load "~/.emacs.d/clojure-mode.el")
(require 'clojure-mode)

(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

(require 'dired+) ;; Enhance dired

(require 're-builder)
(setq reb-re-syntax 'string) ; elisp/read regex syntax is...undesirable.

(when (fboundp 'winner-mode)
      (winner-mode 1))

; finds hashbang notation, if it does it chmod +x's it upon save.
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

; change yes or no prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Erlang mode
(setq load-path (cons "~/.emacs.d/erlang" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/bin" exec-path))
(require 'erlang-start)

(require 'rainbow-mode)

;; (require 'textmate)
;; (textmate-mode)

(require 'sws-mode)
(require 'jade-mode)
(require 'stylus-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(add-to-list 'load-path
              "~/.emacs.d/yasnippet")

(require 'yasnippet)
(yas/global-mode 1)
(global-set-key (kbd "C-c e x") 'yas/expand) ; Impatience.

(require 'restclient)

(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-bitemyapp)
(color-theme-bitemyapp)

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script>" "</script>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))

;;                  (css-mode "<style>" "</style>")))

(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(require 'nyan-mode)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white" :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Monospace")))))

(nyan-mode)
(nyan-start-animation)
