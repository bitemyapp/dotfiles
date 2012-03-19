; -*- mode: emacs-lisp; mode: rainbow;-*-

(defun color-theme-bitemyapp ()
  (interactive)
  (color-theme-install
   '(color-theme-bitemyapp
      ((background-color . "#000000")
      (background-mode . dark)
      (border-color . "#1a1a1a")
      (cursor-color . "#ffffff")
      (foreground-color . "#ffffff")
      (mouse-color . "black"))
     (fringe ((t (:background "#1a1a1a"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
     (region ((t (:background "#0d4519"))))
     (font-lock-builtin-face ((t (:foreground "#FF0055"))))
     (font-lock-comment-face ((t (:foreground "#559f25"))))
     (font-lock-function-name-face ((t (:foreground "#55DDDD"))))
     (font-lock-keyword-face ((t (:foreground "#3c95f5"))))
     (font-lock-string-face ((t (:foreground "#ffff45"))))
     (font-lock-type-face ((t (:foreground "#338833"))))
     (font-lock-constant-face ((t (:foreground "#eeeeec"))))
     (font-lock-variable-name-face ((t (:foreground "#44ff44"))))
     (font-lock-preprocessor-face ((t (:foreground "#a82255"))))
     (minibuffer-prompt ((t (:foreground "#cf72b7" :bold t))))
     (font-lock-warning-face ((t (:foreground "#FF0000" :bold t))))
     )))

(provide 'color-theme-bitemyapp)
