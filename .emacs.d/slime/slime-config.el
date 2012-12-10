;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
;; (setq inferior-lisp-program "/usr/local/bin/mit-scheme")
(require 'slime)
(global-set-key (kbd "C-c <tab>") 'slime-complete-symbol)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'lisp-mode-hook 'auto-complete-mode)

(defun start-slime ()
  (interactive)
  (slime)
  (slime-mode)
  (auto-complete-mode))

(global-set-key (kbd "C-c s l") 'start-slime)

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "sbcl")
