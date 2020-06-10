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

(setq package-list '(abyss-theme
                     ;; cider
                     ;; clojure-mode
                     company
                     ;; company-ghci
                     ; csv-mode
                     dark-krystal-theme
                     dash
                     dracula-theme
                     editorconfig
                     ; elixir-mode
                     ; elm-mode
                     ; erlang
                     flycheck
                     flycheck-rust
                     go-mode
                     ;; groovy-mode
                     ;; intero
                     ;; haskell-mode
                     hcl-mode
                     ; idris-mode
                     js2-mode
                     json-mode
                     magit
                     ;; markdown-mode
                     material-theme
                     ;; merlin
                     monokai-theme
                     ; nix-mode
                     phoenix-dark-mono-theme
                     phoenix-dark-pink-theme
                     php-mode
                     ; projectile
                     ; protobuf-mode
                     ; puppet-mode
                     ; python-mode
                     ;; racer
                     rainbow-delimiters
                     rainbow-mode
                     ;; reason-mode
                     rust-mode
                     scss-mode
                     shakespeare-mode
                     tabbar
                     tide
                     typescript-mode
                     ;; toml-mode
                     ; twittering-mode
                     ;; ubuntu-theme
                     undo-tree
                     use-package
                     ; virtualenv
                     ; w3m
                     warm-night-theme
                     web-beautify
                     web-mode
                     ; writeroom-mode
                     ;; yaml-mode
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
(require 'use-package)

;; Agda
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))
;; (setq agda2-highlight-level

;; Clojure
;; (add-to-list 'load-path "~/.emacs.d/clojure/")
;; (load-library "clojure-config.el")

(use-package company
  ;; :init (add-hook 'prog-mode-hook 'company-mode)
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))

;; Coq
;; (setq coq-prog-args "-emacs-U")
;; (setq coq-prog-name "/usr/bin/coqtop")
;; (add-to-list 'load-path "~/.emacs.d/proofgeneral/")
;; (let ((default-directory "~/.emacs.d/proofgeneral/"))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (setq proof-splash-enable nil)
;; (load-library "pg-init.el")

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

;; Dockerfile
(add-to-list 'load-path "~/.emacs.d/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Editorconfig
(require 'editorconfig)
(editorconfig-mode 1)

;; Elixir
;; (require 'elixir-mode)

;; Erlang
;; (require 'erlang)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (autoload 'flycheck-haskell-setup "flycheck-haskell")

;; calls runhaskell which doesn't work
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; Go
;; (require 'go-mode-autoloads)

;; Groovy
;; (require 'groovy-mode)
;; (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))

;; Haskell
(add-to-list 'load-path "~/.emacs.d/haskell")
(load-library "haskell-config.el")

;; JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js2-mode))
(setq js-indent-level 2)

;; JSON
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . json-mode))

;; LaTeX
;; (add-to-list 'auto-mode-alist '("\\.tex$" . tex-mode))
;; Only change sectioning colour
(eval-after-load "tex-mode" '(fset 'tex-font-lock-suscript 'ignore))

;; magit
(require 'magit)

;; Markdown
;; (add-to-list 'load-path "~/.emacs.d/markdown-mode/")
;; (require 'markdown-mode)
;; (add-to-list 'auto-mode-alist
;;          '("\\.md$" . markdown-mode))
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

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

;; Rust mode and accoutrements

;; (require 'rust-mode)
;; (add-hook 'rust-mode-hook #'company-mode)
(use-package rust-mode)
;; (use-package cargo
;;   :hook (rust-mode . cargo-minor-mode))

(use-package lsp-rust
  :ensure t
  :disabled t
  :after lsp-mode
  :init
  (add-hook 'rust-mode-hook #'lsp-rust-enable))

(with-eval-after-load 'lsp-mode
  (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
  (require 'lsp-rust))

;; (add-hook 'rust-mode-hook #'lsp-rust-enable)
;; (add-hook 'rust-mode-hook #'flycheck-mode)

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

;; SCSS
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; Shakespeare
(require 'shakespeare-mode)

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

(require 'typescript-mode)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
<<<<<<< HEAD
=======

(require 'web-beautify) ;; Not necessary if using ELPA package
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

;; very large files
;; (add-to-list 'load-path "~/.emacs.d/vlfi")
;; (require 'vlf)

;; yaml-mode
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(use-package yaml-mode
  :ensure t
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode)))

;; yasnippet
;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
(setq yas-snippet-dirs '("~/.emacs.d/yasnippet/snippets"))
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

;; Visuals
(add-to-list 'load-path "~/.emacs.d/color-themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/color-themes")
(load-theme 'abyss t)

(add-to-list 'load-path "~/.emacs.d/emacs-which-key")
(require 'which-key)
(which-key-mode)

(setq mac-command-modifier 'meta)

(with-system 'darwin
  (custom-set-faces
    '(default ((t (:height 180 :family "Hack"))))))

(with-system 'gnu/linux
  (custom-set-faces
    '(default ((t (:height 140 :family "Ubuntu Mono"))))))

;; (when (> (display-pixel-height) 1200)
;;   ;; retina
;;   (custom-set-faces
;;     '(default ((t (:height 180 :family "Ubuntu Mono"))))))

;; (custom-set-faces '(default ((t (:height 100 :family "Ubuntu Mono")))))
;; (custom-set-faces '(default ((t (:height 120 :family "Ubuntu Mono")))))
;; (custom-set-faces '(default ((t (:height 140 :family "Ubuntu Mono")))))
;; (custom-set-faces '(default ((t (:height 160 :family "Ubuntu Mono")))))
;; (custom-set-faces '(default ((t (:height 180 :family "Ubuntu Mono")))))
;; (custom-set-faces '(default ((t (:height 220 :family "Ubuntu Mono")))))
;; (custom-set-faces '(default ((t (:height 240 :family "Ubuntu Mono")))))
;; (custom-set-faces '(default ((t (:height 320 :family "Ubuntu Mono")))))

(setq mac-option-modifier 'meta)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-electric-keys nil)
 '(ido-mode 'both nil (ido))
 '(inhibit-startup-screen t)
 '(org-support-shift-select 'always t)
 '(package-selected-packages
   '(web-beautify yaml-mode toml-mode markdown-mode dhall-mode yasnippet warm-night-theme use-package undo-tree typescript-mode tabbar shakespeare-mode scss-mode reason-mode rainbow-mode rainbow-delimiters racer php-mode phoenix-dark-pink-theme phoenix-dark-mono-theme monokai-theme merlin material-theme magit json-mode js2-mode hcl-mode go-mode editorconfig dracula-theme dash dark-krystal-theme abyss-theme))
 '(shift-select-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Ubuntu Mono")))))
