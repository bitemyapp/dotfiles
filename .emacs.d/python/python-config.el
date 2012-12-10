(require 'python)

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(setq ropemacs-enable-autoimport 't)

(global-set-key (kbd "C-M-n") 'next-error)

(require 'virtualenv)

(defun ipdb ()
    (interactive)
    (insert "import sys; sys.stdout = sys.__stdout__; import ipdb; ipdb.set_trace()"))

(global-set-key (kbd "C-c p d b") 'ipdb)
