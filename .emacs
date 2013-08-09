;; Dependencies
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d")

(require 'package)
(add-to-list 'package-archives
       '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
                      coffee-mode
                      drag-stuff
                      elixir-mix
                      elixir-mode
                      flymake-coffee
                      flymake-elixir
                      jinja2-mode
                      mustache-mode
                      nrepl
                      sass-mode
                      scala-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Miscellaneous keyboard and personal preferences
(load-library "misc-config.el")

;; auto-complete
(load-library "ac-config.el")

;; Clojure
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("gantryfile" . clojure-mode))

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
