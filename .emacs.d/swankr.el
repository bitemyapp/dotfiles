(require 'ess-help)

(defun swankr-operator-before-point ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1)
      (slime-symbol-at-point))))

(add-hook 'R-mode-hook
          (defun swankr/R-mode-hook ()
            (slime-mode 1)
            (set (make-local-variable 'slime-operator-before-point-function) 'swankr-operator-before-point)
            (local-set-key (kbd "(") 'slime-space)))
