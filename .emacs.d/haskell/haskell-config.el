;; Haskell

(require 'haskell-mode)
(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'ghc)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook 'ghc-init)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(require 'hs-lint)

(custom-set-variables '(haskell-tags-on-save t))

(require 'company)
(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

(provide 'haskell-config)

;; haskell-config.el ends here
