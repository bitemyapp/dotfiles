(require 'markdown-mode)
(add-to-list 'auto-mode-alist
         '("\\.md$" . markdown-mode))
(add-hook 'text-mode-hook (lambda ()
                (turn-on-auto-fill)
                (setq-default line-spacing 5)
                (setq indent-tabs-mode nil)))
