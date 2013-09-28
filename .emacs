;; Dependencies
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d")

(setq package-list '(;; ac-nrepl
                     clojure-mode
                     clojure-test-mode
                     dash-at-point
                     less-css-mode
                     nrepl
                     nrepl-ritz
                     ;; paredit
                     puppet-mode))

(require 'package)
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; Miscellaneous keyboard and personal preferences
(load-library "misc-config.el")

;; auto-complete
(load-library "ac-config.el")

;; Clojure
(load-library "clojure-config.el")

;; CoffeeScript
(require 'coffee-mode)

;; color-grep
(require 'color-grep)

;; Drag stuff
(require 'drag-stuff)
(drag-stuff-mode t)

;; Enhance dired
(require 'dired+)
(require 'dired-x)

;; Elixir
(require 'elixir-mode)

;; Erlang
(load-library "erlang-config.el")

;; Go
(load-library "go-config.el")

;; Haskell
(load-library "haskell-config.el")

;; Jinja
(require 'jinja)
(fset 'html-mode 'jinja-mode)

;; (load-library "multi-web-mode-config.el")

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;;; Magit
(require 'magit)
(global-set-key (kbd "C-c m g") 'magit-status)

;; Markdown
(load-library "markdown-config.el")

;; Mustache
(require 'mustache-mode)

;; OCaml
(load-library "ocaml-config.el")

;; PHP
(require 'php-mode)

;; Protobuf
(require 'protobuf-mode)

;; Puppet
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; Python
(load-library "python-config.el")

;; rainbow-mode for CSS
(require 'rainbow-mode)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; rect mark
(require 'rect-mark)

;; Rest client
(require 'restclient)

;; rst
(require 'rst)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Ruby
(load-library "ruby-config.el")

;; Rust
(require 'rust-mode)

;; Scala
(require 'scala-mode-auto)
(load-library "ensime-config.el")

;; SCSS
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; Slime
;; (load-library "slime-config.el")

;; Tabbar
(if (display-graphic-p)
    (load-library "tabbar-config.el"))

;; Web-mode
(load-library "web-mode-config.el")

;; Vala
(load-library "vala-config.el")

;; Yaml
(load-library "yaml-config.el")

;; Color theme
(require 'color-theme)
(color-theme-initialize)
;; (require 'soothe-theme)
;; (require 'tomorrow-night-theme)
;; (require 'tomorrow-theme)
(require 'tomorrow-night-bright-theme)
;; (require 'color-theme-bitemyapp)
;; (color-theme-bitemyapp)
;; (require 'color-theme-solarized)
;; (load-library "color-theme-solarized.el")
;; (color-theme-solarized 'dark)

(if (string= system-type "darwin")
(custom-set-faces
 '(default ((t (:background "black" :foreground "white" :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "Menlo")))))

(custom-set-faces
 '(default ((t (:background "black" :foreground "white" :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "Monospace"))))))

;; fucking auto-fill
(defun auto-fill-mode (args)
  (message "fuck off"))
