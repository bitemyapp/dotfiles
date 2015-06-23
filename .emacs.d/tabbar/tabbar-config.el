(require 'tabbar)

(if (not tabbar-mode)
    (tabbar-mode))

(setq tabbar-buffer-groups-function
    (lambda ()
    (list "All Buffers")))

(setq tabbar-buffer-list-function
    (lambda ()
          (remove-if
           (lambda(buffer)
             (find (aref (buffer-name buffer) 0) " *"))
           (buffer-list))))

(global-set-key (kbd "s-{") 'tabbar-backward)
(global-set-key (kbd "s-}") 'tabbar-forward)
