;; Haskell

(require 'haskell-mode)
(setq auto-mode-alist (cons '("\.hs$" . haskell-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.purs$" . haskell-mode) auto-mode-alist))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'ghc)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook 'ghc-init)
(defun ghc-jump-to-def (&optional ask)
  (interactive "P")
  (let* ((expr0 (ghc-things-at-point))
         (expr (if (or ask (not expr0)) (ghc-read-expression expr0) expr0))
         (info (ghc-get-info expr)))
    (when (and info
               (not (string= info "Cannot show info")))
      (let* ( (re "Defined at \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)")
              (filename (progn (string-match re info)
                               (match-string 1 info)))

              (row (progn (string-match re info)
                          (match-string 2 info)))

              (col (progn (string-match re info)
                          (match-string 3 info))))

        (find-file filename)
        (goto-line (string-to-number row))
        (beginning-of-line)
        (forward-char (string-to-number col))))))
(defvar ghc-jumpdef-key "\C-c\C-d")
(define-key haskell-mode-map ghc-jumpdef-key 'ghc-jump-to-def)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(require 'hs-lint)

(custom-set-variables '(haskell-tags-on-save t))
(global-set-key (kbd "C-c c c i") 'ghc-check-insert-from-warning)

(require 'company)
(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

(provide 'haskell-config)

;; haskell-config.el ends here
