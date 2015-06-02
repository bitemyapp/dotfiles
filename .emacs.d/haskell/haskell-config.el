;; Haskell

;; (add-to-list 'load-path "~/.emacs.d/haskell/haskell-mode/")
(add-to-list 'load-path "~/.emacs.d/haskell/hindent/elisp/")

(require 'haskell-mode)
;; (require 'hindent)
(require 'haskell)
(require 'haskell-indentation)
;; (require 'haskell-indent)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'haskell-font-lock)

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

(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.lhs$" . tex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.purs$" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(custom-set-variables 
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-args-ghci '())
 ;; '(haskell-process-args-cabal-repl
 ;;   '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
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
 '(hindent-style "chris-done"))

(setq haskell-interactive-mode-eval-mode 'haskell-mode)

(setq haskell-process-path-ghci "ghci")
(setq haskell-process-args-ghci '("-ferror-spans"))

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

(define-key haskell-mode-map (kbd "C-c C-t") 'ghc-show-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info)

;; (define-key haskell-mode-map (kbd "C-?") 'haskell-mode-find-uses)
(define-key interactive-haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key interactive-haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;; (define-key haskell-mode-map (kbd "C-i") 'hindent-reformat-buffer)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

(require 'ghc)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; (add-hook 'haskell-mode-hook #'hindent-mode)

;; (require 'company)
;; (add-hook 'haskell-mode-hook 'company-mode)
;; (add-to-list 'company-backends 'company-ghc)
;; (custom-set-variables '(company-ghc-show-info t))

(provide 'haskell-config)

;; haskell-config.el ends here
