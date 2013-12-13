;; Haskell
(require 'haskell-mode)
(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (require 'scion) ;; IDE style autocomplete server
;; (setq scion-program "/Users/callen/Library/Haskell/bin/scion-server")
;; (defun my-haskell-hook ()
;;   ;; Whenever we open a file in Haskell mode, also activate Scion
;;   (scion-mode 1)
;;   ;; Whenever a file is saved, immediately type check it and
;;   ;; highlight errors/warnings in the source.
;;   (scion-flycheck-on-save 1))
;; (add-hook 'haskell-mode-hook 'my-haskell-hook)

;; Use ido-mode completion (matches anywhere, not just beginning)
;; (setq scion-completing-read-function 'ido-completing-read) ; dangerous
(require 'ghc)
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))
