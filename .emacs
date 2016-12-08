(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory
		    (or load-file-name (buffer-file-name))))

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
;; (setq load-file-rep-suffixes '(""))

;; Use package management!
(require 'package)

(setq package-archives
      (append '(("melpa" . "https://melpa.org/packages/"))
              package-archives))

(package-initialize)

(setq package-list '(;; abyss-theme
                     ;; cider
                     ;; clojure-mode
                     ; company
                     ; company-ghci
                     ; csv-mode
                     ;; dark-krystal-theme
                     ;; dracula-theme
                     ; elixir-mode
                     ; elm-mode
                     ; erlang
                     ; f
                     ;; flycheck
                     ;; go-mode
                     ;; groovy-mode
                     ;; intero
                     ;; haskell-mode
                     hcl-mode
                     ; idris-mode
                     ; js2-mode
                     ; json-mode
                     ;; magit
                     markdown-mode
                     ;; material-theme
                     ;; monokai-theme
                     ; nix-mode
                     ;; phoenix-dark-mono-theme
                     ;; phoenix-dark-pink-theme
                     ; php-mode
                     ; projectile
                     ; protobuf-mode
                     ; puppet-mode
                     ; python-mode
                     ; racer
                     rainbow-delimiters
                     rainbow-mode
                     rust-mode
                     scss-mode
                     shakespeare-mode
                     tabbar
                     ; toml-mode
                     ; twittering-mode
                     ;; ubuntu-theme
                     undo-tree
                     ; virtualenv
                     ; w3m
                     ;; warm-night-theme
                     ; writeroom-mode
                     yaml-mode
                     yasnippet))

;; rm -rf ~/.emacs.d/elpa to reload
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; General/Misc stuff
(add-to-list 'load-path "~/.emacs.d/misc/")
(load-library "misc-config.el")

;; Agda
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))
;; (setq agda2-highlight-level

;; auto-complete
;; (add-to-list 'load-path "~/.emacs.d/auto-complete/")
;; (load-library "ac-config.el")

;; Clojure
;; (add-to-list 'load-path "~/.emacs.d/clojure/")
;; (load-library "clojure-config.el")

;; Coq
;; (setq coq-prog-args "-emacs-U")
;; (setq coq-prog-name "/usr/bin/coqtop")
;; (add-to-list 'load-path "~/.emacs.d/proofgeneral/")
;; (let ((default-directory "~/.emacs.d/proofgeneral/"))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (setq proof-splash-enable nil)
;; (load-library "pg-init.el")

;; Dockerfile
;; (add-to-list 'load-path "~/.emacs.d/dockerfile-mode/")
;; (require 'dockerfile-mode)
;; (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Elixir
;; (require 'elixir-mode)

;; Erlang
;; (require 'erlang)

;; flycheck
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (autoload 'flycheck-haskell-setup "flycheck-haskell")

;; go
;; (require 'go-mode-autoloads)

;; groovy
;; (require 'groovy-mode)
;; (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))

;; calls runhaskell which doesn't work
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; Haskell
(add-to-list 'load-path "~/.emacs.d/haskell")
(load-library "haskell-config.el")
;; Override haskell-mode's BS.
;; (add-hook 'haskell-mode-hook 'intero-mode)

;; (require 'company)
;; (require 'company-ghci)
;; (setq company-idle-delay 0)
;; (add-hook 'haskell-mode-hook 'intero-mode)

;; Stack
;; (add-to-list 'load-path "~/work/stack-ide/stack-mode/")
;; (require 'stack-mode)
;; (add-hook 'haskell-mode-hook 'stack-mode)

;; Helm
;; [Facultative] Only if you have installed async.
;; (add-to-list 'load-path "~/.emacs.d/emacs-async")
;; (add-to-list 'load-path "~/.emacs.d/helm")
;; (add-to-list 'load-path "~/.emacs.d/emacs-helm-ag")
;; (add-to-list 'load-path "~/.emacs.d/helm-swoop")
;; (require 'helm-config)
;; (require 'helm-swoop)
;; (require 'helm-ag)
;; (helm-mode 1)
;; (global-set-key (kbd "C-x C-m") 'helm-M-x)
;; (global-set-key (kbd "C-s") 'helm-swoop)
;; (global-set-key (kbd "C-c p") 'helm-ag-project-root)


;; highlight-symbol
;; (add-to-list 'load-path "~/.emacs.d/idle-highlight/")
;; (load-library "idle-highlight.el")
;; (require 'idle-highlight-mode)
;; (add-hook 'text-mode-hook (lambda () (idle-highlight-mode t)))
;; (add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))
;; (setq idle-highlight-idle-time 0.5)

;; (require 'highlight-symbol)
;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;; (setq highlight-symbol-idle-delay 0.2)

;; JavaScript
;; (require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js2-mode))

;; JSON
;; (require 'json-mode)
;; (add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . json-mode))

;; latex
;; (add-to-list 'auto-mode-alist '("\\.tex$" . tex-mode))

;; Magit
;; (require 'magit)
;; (global-set-key (kbd "C-c m g") 'magit-status)

;; Markdown
;; (require 'markdown-mode)
;; (add-to-list 'auto-mode-alist
;;          '("\\.md$" . markdown-mode))

;; mu4e
;; (require 'mu4e)

;; Mustache
;; (require 'mustache-mode)

;; Nix
;; (require 'nix-mode)

;; OCaml
;; (add-to-list 'load-path "~/.emacs.d/ocaml")
;; (load "~/.emacs.d/ocaml/tuareg-site-file")

;; (load-library "ocaml-config.el")
;; (add-to-list 'load-path "~/.opam/4.02.1/share/emacs/site-lisp/")
;; (load "~/.opam/4.02.1/share/emacs/site-lisp/tuareg-site-file")

;; PHP
;; (require 'php-mode)

;; Projectile
;; (require 'projectile)
;; (global-set-key (kbd "C-c p g") 'projectile-grep)

;; Protobuf
;; (require 'protobuf-mode)

;; Puppet
;; (require 'puppet-mode)
;; (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; Python
;; (add-to-list 'load-path "~/.emacs.d/python")
;; (load-library "python-config.el")

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; rainbow-mode for CSS
(require 'rainbow-mode)

;; Rust mode
(require 'rust-mode)

;; ;; (setq racer-cmd "$HOME/.cargo/bin/racer")
;; (setq racer-rust-src-path (concat (getenv "HOME") "/work/rustc/src"))
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)

;; SCSS
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; Shakespeare
(require 'shakespeare-mode)

;; Speedbar
;; (require 'speedbar)
;; (speedbar-add-supported-extension ".hs")

;; Tabbar
(add-to-list 'load-path "~/.emacs.d/tabbar")
(if (display-graphic-p)
    (load-library "tabbar-config.el"))

;; Terraform
(add-to-list 'load-path "~/.emacs.d/terraform-mode")
(require 'terraform-mode)

;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; very large files
;; (add-to-list 'load-path "~/.emacs.d/vlfi")
;; (require 'vlf)

;; distraction free writing
;; (require 'writeroom-mode)
;; (global-set-key (kbd "C-c w r") 'writeroom-mode)

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; yasnippet
;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
(setq yas-snippet-dirs '("~/.emacs.d/yasnippet/snippets"))
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

;; (defun yas/goto-end-of-active-field ()
;;   (interactive)
;;   (let* ((snippet (car (yas--snippets-at-point)))
;;         (position (yas--field-end (yas--snippet-active-field snippet))))
;;     (if (= (point) position)
;;         (move-end-of-line 1)
;;       (goto-char position))))

;; (defun yas/goto-start-of-active-field ()
;;   (interactive)
;;   (let* ((snippet (car (yas--snippets-at-point)))
;;         (position (yas--field-start (yas--snippet-active-field snippet))))
;;     (if (= (point) position)
;;         (move-beginning-of-line 1)
;;       (goto-char position))))

;; (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
;; (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
;; (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
;; (setq yas-verbosity 1)
;; (setq yas-wrap-around-region t)

;; default browser
;; (setq browse-url-browser-function 'w3m-goto-url-new-session)

;; Desktop mode
;; (setq desktop-load-locked-desktop t)
;; (setq desktop-path '("~/.emacs.d/"))
;; (setq desktop-dirname "~/.emacs.d/")
;; (setq desktop-base-file-name "emacs-desktop")
;; (desktop-read desktop-dirname)
;; (defun my-desktop-save ()
;;     (interactive)
;;     ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
;;     (desktop-save desktop-dirname))
;; (add-hook 'auto-save-hook 'my-desktop-save)

;; Visuals
(add-to-list 'load-path "~/.emacs.d/color-themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes")
;; (require 'bitemyapp-theme)
;; (load-theme 'misterioso t)
;; (load-theme 'dracula t)
;; (load-theme 'material t)
;; (load-theme 'aurora t)
;; (load-theme 'phoenix-dark-pink t)
;; (load-theme 'ubuntu t)
;; (load-theme 'phoenix-dark-mono t)
;; (load-theme 'abyss t)
(load-theme 'gruvbox t)
;; (set-face-foreground 'font-lock-string-face "red")
;; (set-face-foreground 'font-lock-comment-face "light pink")

(with-system 'darwin
  (custom-set-faces
    '(default ((t (:height 160 :family "Ubuntu Mono"))))))

(with-system 'gnu/linux
  (custom-set-faces
    '(default ((t (:height 160 :family "Ubuntu Mono"))))))

(when (> (display-pixel-height) 1200)
  ;; retina
  (custom-set-faces
    '(default ((t (:height 180 :family "Ubuntu Mono"))))))

;; (custom-set-faces '(default ((t (:height 260 :family "Ubuntu Mono")))))

(setq mac-option-modifier 'meta)

(setq magit-last-seen-setup-instructions "1.4.0")
