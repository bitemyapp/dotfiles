(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory
		    (or load-file-name (buffer-file-name))))

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Use package management!
(require 'package)

(setq package-archives
      (append '(("org"       . "http://orgmode.org/elpa/")
                ("melpa"     . "http://melpa.milkbox.net/packages/")
                ("marmalade" . "http://marmalade-repo.org/packages/"))
              package-archives))

(package-initialize)

(setq package-list '(auto-complete
                     cider
                     clojure-mode
                     company
                     csv-mode
                     erlang
                     ghc
                     haskell-mode
                     js2-mode
                     json-mode
                     magit
                     markdown-mode
                     monokai-theme
                     nix-mode
                     php-mode
                     protobuf-mode
                     puppet-mode
                     pymacs
                     python-mode
                     rainbow-delimiters
                     rainbow-mode
                     scss-mode
                     tabbar
                     undo-tree
                     virtualenv))

;; rm -rf ~/.emacs.d/elpa to reload
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; General/Misc stuff
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/misc/")
(load-library "misc-config.el")

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(load-library "ac-config.el")

;; Clojure
(add-to-list 'load-path "~/.emacs.d/clojure/")
(load-library "clojure-config.el")

;; Coq
(setq coq-prog-name "/usr/bin/coqtop")
;; (setq coq-prog-args "-emacs-U")
(add-to-list 'load-path "~/.emacs.d/proofgeneral/")
(let ((default-directory "~/.emacs.d/proofgeneral/"))
  (normal-top-level-add-subdirs-to-load-path))
(setq proof-splash-enable nil)
(load-library "pg-init.el")

;; Erlang
(require 'erlang-start)

;; Haskell
(add-to-list 'load-path "~/.emacs.d/haskell/")
(load-library "haskell-config.el")

;; JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js2-mode))

;; JSON
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . json-mode))

;; Magit
(require 'magit)
(global-set-key (kbd "C-c m g") 'magit-status)

;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist
         '("\\.md$" . markdown-mode))

;; Mustache
(require 'mustache-mode)

;; Nix
(require 'nix-mode)

;; OCaml
(add-to-list 'load-path "~/.emacs.d/ocaml")
(load-library "ocaml-config.el")

;; PHP
(require 'php-mode)

;; Protobuf
(require 'protobuf-mode)

;; Puppet
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; Python
(add-to-list 'load-path "~/.emacs.d/python")
(load-library "python-config.el")

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; rainbow-mode for CSS
(require 'rainbow-mode)

;; SCSS
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; Speedbar
(require 'speedbar)
(speedbar-add-supported-extension ".hs")

;; Tabbar
(add-to-list 'load-path "~/.emacs.d/tabbar")
(if (display-graphic-p)
    (load-library "tabbar-config.el"))

;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Desktop mode
(setq desktop-load-locked-desktop t)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(desktop-read desktop-dirname)
(defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (desktop-save desktop-dirname))
(add-hook 'auto-save-hook 'my-desktop-save)

;; Visuals
(load-theme 'wombat)

(with-system 'darwin
  (custom-set-faces
    '(default ((t (:height 140 :family "Menlo"))))))

(with-system 'gnu/linux
  (custom-set-faces
    '(default ((t (:height 120 :family "Ubuntu Mono"))))))

(when (> (display-pixel-height) 1080)
  ;; retina
  (custom-set-faces
    '(default ((t (:height 220 :family "Ubuntu Mono"))))))
