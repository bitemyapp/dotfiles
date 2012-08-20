;; Dependencies

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d")

;; Miscellaneous keyboard and personal preferences
(load-library "misc-config.el")

;; auto-complete
(load-library "ac-config.el")

;; Clojure
(require 'clojure-mode)

;; coffee-script
(require 'coffee-mode)

;; color-grep
(require 'color-grep)

;; DCPU-16
(require 'dcpu16-mode)

;; Drag stuff
(require 'drag-stuff)
(drag-stuff-mode t)

;; Enhance dired
(require 'dired+)
(require 'dired-x)

;; Erlang
(load-library "erlang-config.el")

;; Go
(require 'go-mode-load)

;; Haskell
(load-library "haskell-config.el")

;; Jade
(load-library "jade-config.el")

;; Jinja
(require 'jinja)
(fset 'html-mode 'jinja-mode)

;;; Magit
(require 'magit)
(global-set-key (kbd "C-c m g") 'magit-status)

;; Python
(load-library "python-config.el")

;; rainbow-mode for CSS
(require 'rainbow-mode)

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

;; Slime
(load-library "slime-config.el")

;; Tabbar
(load-library "tabbar-config.el")

;; Vala
(load-library "vala-config.el")

;; Yaml
(load-library "yaml-config.el")

;; Nyan-mode
(require 'nyan-mode)

;; Color theme
(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-bitemyapp)
(color-theme-bitemyapp)

(if (string= system-type "darwin")
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white" :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Monaco")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white" :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Monospace"))))))

(nyan-mode)
(nyan-start-animation)
