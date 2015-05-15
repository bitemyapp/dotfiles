(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory
		    (or load-file-name (buffer-file-name))))

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Use package management!
(require 'package)

(setq package-archives
      (append '(("org"        . "http://orgmode.org/elpa/")
                ("melpa"      . "http://melpa.milkbox.net/packages/")
                ("marmalade"  . "http://marmalade-repo.org/packages/"))
              package-archives))

(package-initialize)

(setq package-list '(auto-complete
                     cider
                     clojure-mode
                     company
                     csv-mode
                     dark-krystal-theme
                     erlang
                     ghc
                     haskell-mode
                     idris-mode
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
                     twittering-mode
                     undo-tree
                     virtualenv
                     warm-night-theme
                     yaml-mode))

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

;; Agda
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))
;; (setq agda2-highlight-level

;; auto-complete
;; (add-to-list 'load-path "~/.emacs.d/auto-complete/")
;; (load-library "ac-config.el")

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
(require 'erlang)

;; Haskell
(add-to-list 'load-path "~/.emacs.d/haskell/")
(load-library "hindent.el")
(load-library "haskell-config.el")

;; highlight-symbol
(add-to-list 'load-path "~/.emacs.d/idle-highlight/")
(load-library "idle-highlight.el")
(require 'idle-highlight-mode)
(add-hook 'text-mode-hook (lambda () (idle-highlight-mode t)))
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))
(setq idle-highlight-idle-time 0.5)

;; (require 'highlight-symbol)
;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;; (setq highlight-symbol-idle-delay 0.2)

;; JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js2-mode))

;; JSON
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'\\|\\.jshintrc\\'" . json-mode))

;; latex
(add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))

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

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(setq yas-snippet-dirs '("~/.emacs.d/yasnippet/snippets"))
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
(setq yas-verbosity 1)
(setq yas-wrap-around-region t)

;; default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

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
(add-to-list 'load-path "~/.emacs.d/color-themes")
;; (require 'bitemyapp-theme)

(load-theme 'misterioso t)

(with-system 'darwin
  (custom-set-faces
    '(default ((t (:height 140 :family "Ubuntu Mono"))))))

(with-system 'gnu/linux
  (custom-set-faces
    '(default ((t (:height 120 :family "Ubuntu Mono"))))))

(when (> (display-pixel-height) 1080)
  ;; retina
  (custom-set-faces
    '(default ((t (:height 160 :family "Ubuntu Mono"))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-electric-keys nil)
 '(custom-safe-themes (quote ("2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "c87cc60d01cf755375759d165c1d60d9586c6a31f0b5437a0378c2a93cfc8407" "789844278c5a75283b5015c1fc7bebe7e4cf97843b8f8cffe21fafa05e81e90a" "6209442746f8ec6c24c4e4e8a8646b6324594308568f8582907d0f8f0260c3ae" "e4bc8563d7651b2fed20402fe37b7ab7cb72869f92a3e705907aaecc706117b5" default)))
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-notify-p t)
 '(haskell-process-args-ghci (quote nil))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(hindent-style "johan-tibell")
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(org-support-shift-select (quote always))
 '(shift-select-mode t)
 '(shm-auto-insert-bangs t)
 '(shm-auto-insert-skeletons t)
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "cyan"))))
 '(agda2-highlight-function-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-postulate-face ((t (:foreground "cyan"))))
 '(agda2-highlight-primitive-face ((t (:foreground "light gray"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "light gray"))))
 '(agda2-highlight-record-face ((t (:foreground "cyan"))))
 '(font-latex-bold-face ((t nil)))
 '(font-latex-doctex-documentation-face ((t nil)))
 '(font-latex-doctex-preprocessor-face ((t nil)))
 '(font-latex-italic-face ((t nil)))
 '(font-latex-math-face ((t nil)))
 '(font-latex-sectioning-0-face ((t nil)))
 '(font-latex-sectioning-1-face ((t nil)))
 '(font-latex-sectioning-2-face ((t nil)))
 '(font-latex-sectioning-3-face ((t nil)))
 '(font-latex-sectioning-4-face ((t nil)))
 '(font-latex-sectioning-5-face ((t nil)))
 '(font-latex-sedate-face ((t nil)))
 '(font-latex-slide-title-face ((t nil)))
 '(font-latex-string-face ((t nil)))
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil)))
 '(font-latex-verbatim-face ((t nil)))
 '(font-latex-warning-face ((t nil)))
 '(idris-semantic-type-face ((t (:foreground "deep sky blue"))))
 '(tex-verbatim ((t nil))))
(setq magit-last-seen-setup-instructions "1.4.0")
