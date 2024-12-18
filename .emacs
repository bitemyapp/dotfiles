;; bitemyapp's .emacs

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

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

(setq package-archives
      (append '(("gnu" . "https://elpa.gnu.org/packages/"))
              package-archives))

(package-initialize)

(setq package-list '(abyss-theme
                     ;; cider
                     ;; clojure-mode
                     ;; company
                     ;; company-ghci
                     ; csv-mode
                     dark-krystal-theme
                     dash
                     dracula-theme
                     ef-themes
                     ;; editorconfig
                     ; elixir-mode
                     ; elm-mode
                     ; erlang
                     ;; flycheck
                     go-mode
                     ;; groovy-mode
                     ;; intero
                     ;; haskell-mode
                     hcl-mode
                     ; idris-mode
                     js2-mode
                     json-mode
                     just-mode
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
                     racer
                     ;; rainbow-delimiters
                     ;; rainbow-mode
                     reason-mode
                     rust-mode
                     scss-mode
                     shakespeare-mode
                     tabbar
                     tide
                     typescript-mode
                     ;; toml-mode
                     ; twittering-mode
                     ;; ubuntu-theme
                     ;; undo-tree
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
;; (require 'editorconfig)
;; (editorconfig-mode 1)

;; Elixir
;; (require 'elixir-mode)

;; Erlang
;; (require 'erlang)

;; flycheck
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
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
;; (add-to-list 'load-path "~/.emacs.d/haskell")
;; (load-library "haskell-config.el")

;; JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js2-mode))
(setq js-indent-level 2)

;; JSON
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . json-mode))

;; Just
(use-package justl
  :ensure t
  :custom
  (justl-executable "$HOME/.cargo/bin/just"))

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

(setq exec-path (append exec-path '("~/.local/bin")))
(setq rustic-analyzer-command '("~/.local/bin/rust-analyzer"))

(use-package rustic)

;; Rust mode and accoutrements
;; (use-package company
;;   ;; :init (add-hook 'prog-mode-hook 'company-mode)
;;   :hook (prog-mode . company-mode)
;;   :config (setq company-tooltip-align-annotations t)
;;           (setq company-minimum-prefix-length 1))

;; (use-package flycheck
;;   ;; :init (add-hook 'prog-mode-hook 'flycheck-mode)
;;   :hook (prog-mode . flycheck-mode)
;;   )

(use-package lsp-mode)
;; (use-package lsp-mode
;;   :ensure t
;;   :disabled t)

(use-package lsp-ui
  ;; :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :hook (lsp-mode . lsp-ui-mode)
  )

;; (use-package company-lsp
;;   :ensure t
;;   :after company lsp-mode
;;   :init
;;   (push 'company-lsp company-backends))

(lsp-inlay-hints-mode)
(setq lsp-inlay-hint-enable t)
(setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
(setq lsp-rust-analyzer-display-closure-return-type-hints t)
(setq lsp-rust-analyzer-display-parameter-hints t)
;; (setq lsp-rust-analyzer-display-reborrow-hints t)

;; (require 'rust-mode)
;; (add-hook 'rust-mode-hook #'company-mode)
;; (use-package rust-mode)

;; (use-package racer
;;   :ensure t
;;   :after rust-mode
;;   :diminish racer-mode
;;   :init
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook (lambda () (setq eldoc-documentation-function nil))))

;; (use-package company-racer)
;; (use-package flycheck-rust)
;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package flycheck-rust
;;   :init (with-eval-after-load
;;             'rust-mode (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
;;   )

;; (use-package lsp-rust
;;   :config (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;   :init (add-hook 'rust-mode-hook 'lsp-rust-enable))

;; (use-package lsp-rust
;;   :ensure t
;;   :disabled t
;;   :after lsp-mode
;;   :init
;;   (add-hook 'rust-mode-hook #'lsp-rust-enable))

;; TODOs
(use-package hl-todo
       :ensure t
       :custom-face
       (hl-todo ((t (:inherit hl-todo :italic t))))
       :hook ((prog-mode . hl-todo-mode)
              (yaml-mode . hl-todo-mode)))

;; TOML
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
;; (require 'undo-tree)
;; (global-undo-tree-mode)

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
;; (load-theme 'abyss t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; (load-theme 'ef-bio t)
;; (load-theme 'ef-dark t)
;; (load-theme 'ef-night t)
;; (load-theme 'dracula t)
(use-package nordic-night-theme
  :ensure t
  :config
  ;; Use this for the darker version
  ;; (load-theme 'nordic-midnight t)

  (load-theme 'nordic-night t))

(add-to-list 'load-path "~/.emacs.d/emacs-which-key")
(require 'which-key)
(which-key-mode)

(setq mac-command-modifier 'meta)

(with-system 'darwin
  (custom-set-faces
    '(default ((t (:height 140 :family "FiraCode Nerd Font Mono"))))))

;; (with-system 'gnu/linux
;;   (custom-set-faces
;;     '(default ((t (:height 120 :family "Ubuntu Mono"))))))

(with-system 'gnu/linux
  (custom-set-faces
    '(default ((t (:height 90 :family "FiraCode Nerd Font Mono"))))))

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
(setq mac-command-modifier 'super)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
