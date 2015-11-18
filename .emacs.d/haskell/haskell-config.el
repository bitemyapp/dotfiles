;; Haskell

;; (add-to-list 'load-path "~/.emacs.d/haskell/haskell-mode/")
(add-to-list 'load-path "~/.emacs.d/haskell/hindent/elisp/")

(require 'hindent)
(require 'haskell-mode)
(require 'haskell)

;; (require 'haskell-indentation-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
;; (require 'haskell-font-lock)

(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.lhs$" . tex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.purs$" . haskell-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Shakefile" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(custom-set-variables 
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-args-ghci '())
 ;; '(haskell-process-args-cabal-repl
 ;;   '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
 '(haskell-notify-p t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-suggest-hoogle-imports nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)

 ;; '(haskell-process-do-cabal-format-string ":!cd %s && unset GHC_PACKAGE_PATH && %s")
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(shm-auto-insert-skeletons t)
 '(shm-auto-insert-bangs t)
 '(haskell-process-show-debug-tips nil))

(setq haskell-interactive-mode-eval-mode 'haskell-mode)

(setq hindent-style "gibiansky")
(setq haskell-process-path-ghci "ghci")
(setq haskell-process-args-ghci '("-ferror-spans"))
(custom-set-variables
 '(haskell-process-type 'stack-ghci))

;; (custom-set-variables
;;   '(haskell-process-type 'stack-ghci)
;;   '(haskell-process-log t)
;;   '(haskell-indent-spaces 4)
  ;; '(haskell-process-args-ghci "ghci")
  ;; '(haskell-process-path-ghci "stack"))
;; (setq 'haskell-process-type 'stack-ghci)

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload-file)
;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)

;; nicer output than haskell-process-do-type
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-doc-show-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-s") 'haskell-mode-stylish-buffer)

;; GHCi-ng
;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

;; ghc-mod
;; (define-key haskell-mode-map (kbd "C-c C-t") 'ghc-show-type)
;; (define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info)
;; (define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info)

;; (define-key haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
;; (define-key interactive-haskell-mode-map (kbd "M-,") 'haskell-who-calls)
;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c <backtab>") 'indent-rigidly-left-to-tab-stop)
(define-key haskell-mode-map (kbd "C-c <tab>") 'indent-rigidly-right-to-tab-stop)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
(define-key haskell-mode-map (kbd "C-c C-f C-b") 'hindent-reformat-buffer)

(define-key interactive-haskell-mode-map [f5] 'haskell-process-load-or-reload)
(define-key interactive-haskell-mode-map [f12] 'turbo-devel-reload)
(define-key interactive-haskell-mode-map [f12] 'haskell-process-cabal-build-and-restart)
(define-key interactive-haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key interactive-haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key interactive-haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key interactive-haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
(define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (let ((buffer
           (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                              (haskell-session-current-dir (haskell-session))
                              sym))))
      (with-current-buffer buffer
        (rename-buffer "*who-calls*")
        (switch-to-buffer-other-window buffer)))))

;; (require 'hs-lint)
;; (defun lint-hook ()
;;   (local-set-key "\C-c l" 'hs-lint))
;; (add-hook 'haskell-mode-hook 'lint-hook)

;; (add-to-list 'load-path "/Users/callen/work/stack-ide/stack-ide/stack-mode/")
;; (require 'stack-mode)
;; (add-hook 'haskell-mode-hook 'stack-mode)

;; (require 'ghc)

;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(add-hook 'haskell-mode-hook #'hindent-mode)

;; (require 'company)
;; (add-hook 'haskell-mode-hook 'company-mode)
;; (add-to-list 'company-backends 'company-ghc)
;; (custom-set-variables '(company-ghc-show-info t))

(provide 'haskell-config)

;; haskell-config.el ends here
