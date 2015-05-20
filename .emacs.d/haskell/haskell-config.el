;; Haskell

(add-to-list 'load-path "~/.emacs.d/haskell/haskell-mode/")
(add-to-list 'load-path "~/.emacs.d/haskell/hindent/elisp/")

(require 'haskell-mode)
(require 'hindent)
(require 'haskell)
(require 'haskell-indentation)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'haskell-font-lock)

(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.lhs$" . tex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.purs$" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(custom-set-variables 
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-args-ghci '())
 '(haskell-notify-p t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-mode-eval-pretty nil)
 ;; '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(shm-auto-insert-skeletons t)
 '(shm-auto-insert-bangs t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(hindent-style "gibiansky"))

(setq haskell-interactive-mode-eval-mode 'haskell-mode)

(setq haskell-process-path-ghci "ghci")
(setq haskell-process-args-ghci '("-ferror-spans"))

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
(define-key haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-i") 'hindent-reformat-buffer)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
(add-hook 'haskell-mode-hook #'hindent-mode)

;; (require 'company)
;; (add-hook 'haskell-mode-hook 'company-mode)
;; (add-to-list 'company-backends 'company-ghc)
;; (custom-set-variables '(company-ghc-show-info t))

(provide 'haskell-config)

;; haskell-config.el ends here
