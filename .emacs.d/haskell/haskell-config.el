;; Haskell

(add-to-list 'load-path "~/.emacs.d/haskell/haskell-mode/")
(add-to-list 'load-path "~/.emacs.d/haskell/hindent/elisp/")

(require 'haskell-mode)
(require 'hindent)
;; (require 'haskell-process)
;; (require 'haskell-simple-indent)
(require 'haskell)
(require 'haskell-indentation)
(require 'haskell-interactive-mode)
(require 'haskell-font-lock)

(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.lhs$" . tex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.purs$" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; (require 'ghc)
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook 'ghc-init)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; (defun ghc-jump-to-def (&optional ask)
;;   (interactive "P")
;;   (let* ((expr0 (ghc-things-at-point))
;;          (expr (if (or ask (not expr0)) (ghc-read-expression expr0) expr0))
;;          (info (ghc-get-info expr)))
;;     (when (and info
;;                (not (string= info "Cannot show info")))
;;       (let* ( (re "Defined at \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)")
;;               (filename (progn (string-match re info)
;;                                (match-string 1 info)))

;;               (row (progn (string-match re info)
;;                           (match-string 2 info)))

;;               (col (progn (string-match re info)
;;                           (match-string 3 info))))

;;         (find-file filename)
;;         (goto-line (string-to-number row))
;;         (beginning-of-line)
;;         (forward-char (string-to-number col))))))

;; (defvar ghc-jumpdef-key "\C-c\C-d")
;; (define-key haskell-mode-map ghc-jumpdef-key 'ghc-jump-to-def)

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
 '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(shm-auto-insert-skeletons t)
 '(shm-auto-insert-bangs t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(hindent-style "johan-tibell"))

(setq haskell-interactive-mode-eval-mode 'haskell-mode)

(setq haskell-process-path-ghci "ghci")
(setq haskell-process-args-ghci '("-ferror-spans"))

;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
(define-key haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-i") 'hindent-reformat-buffer)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
(add-hook 'haskell-mode-hook #'hindent-mode)

;; (setq ns-right-alternate-modifier nil)
;; (setq debug-on-error t)

;; (add-hook 'haskell-mode-hook 'haskell-interactive-mode)
;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
;; (add-hook 'w3m-display-hook 'w3m-haddock-display)

;; (global-set-key (kbd "C-c c c i") 'ghc-check-insert-from-warning)

;; (require 'company)
;; (add-hook 'haskell-mode-hook 'company-mode)
;; (add-to-list 'company-backends 'company-ghc)
;; (custom-set-variables '(company-ghc-show-info t))

(provide 'haskell-config)

;; haskell-config.el ends here
