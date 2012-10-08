(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
          interpreter-mode-alist)

      python-mode-hook
      '(lambda () (progn
           (set-variable 'py-indent-offset 4)
           (set-variable 'py-smart-indentation t)
           (set-variable 'indent-tabs-mode nil) )))

;; .ropeproject/config.py
;; at the bottom
;; prefs.add('python_path', '~/path/to/virtualenv/lib/python2.6/site-packages')
;; or
;; At the top
;; VIRTUAL_ENV = "/home/yourname/envs/envname/"
;; ACTIVATE_FILE = VIRTUAL_ENV + "bin/activate_this.py"
;; execfile(ACTIVATE_FILE, dict(__file__=ACTIVATE_FILE))

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (setq ropemacs-enable-autoimport 't)

(global-set-key (kbd "C-M-n") 'next-error)

(setq ipython-command "/usr/local/bin/ipython")
(require 'ipython)

(require 'virtualenv)

(defun ipdb ()
    (interactive)
    (insert "import sys; sys.stdout = sys.__stdout__; import ipdb; ipdb.set_trace()"))

(global-set-key (kbd "C-c p d b") 'ipdb)
