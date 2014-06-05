(require 'python-mode)

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (setq ropemacs-enable-autoimport 't)

(require 'virtualenv)

(defun ipdb ()
    (interactive)
    (insert "import sys; sys.stdout = sys.__stdout__; import ipdb; ipdb.set_trace()"))

(global-set-key (kbd "C-c p d b") 'ipdb)

(defun pprint ()
  (interactive)
  (insert "import pprint; pp = pprint.PrettyPrinter(indent=4); pp.pprint("))

(global-set-key (kbd "C-c p p r") 'pprint)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook 'auto-complete-mode)
;; (add-hook 'python-mode-hook 'jedi:ac-setup)
;; (setq jedi:setup-keys t)                      ; optional
;; (setq jedi:complete-on-dot t)                 ; optional
;; Bad virtualenv support. Really bad.
