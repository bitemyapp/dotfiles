(require 'shime)
(autoload 'shime "shime" nil t)

(global-set-key (kbd "C-c s h") 'shime)
(global-set-key (kbd "C-c s f l") 'shime-load-file)
(global-set-key (kbd "C-c s c b") 'shime-cabal-build)
