(setq ring-bell-function 'ignore)
(toggle-scroll-bar -1)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(css-electric-keys nil)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t))
(tool-bar-mode 0)
(setq frame-title-format "%b")
(setq make-backup-files nil) 
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(require 'ido)

(require 'color-theme)
(color-theme-initialize)
(color-theme-midnight)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 99 :width normal :foundry "unknown" :family "Droid Sans Mono")))))

 (require 'tabbar)
 (tabbar-mode)
 (setq tabbar-buffer-groups-function
           (lambda ()
             (list "All Buffers")))
 (setq tabbar-buffer-list-function
     	(lambda ()
     	  (remove-if
     	   (lambda(buffer)
     	     (find (aref (buffer-name buffer) 0) " *"))
     	   (buffer-list))))

(global-set-key (kbd "TAB") 'self-insert-command)

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)

(iswitchb-mode 1)
