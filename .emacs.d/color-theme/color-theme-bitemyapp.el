; -*- mode: emacs-lisp; mode: rainbow;-*-
;;; color-theme-bitemyapp.el --- a tasteful color theme designed for the colorblind
;; Copyright 2012 Christopher Allen
;; Author: Christopher Allen
;; URL: http://github.com/bitemyapp/dotfiles
;; Version: 1.0
;; Package-Requires: ((color-theme))

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
     (font-lock-comment-face ((t (:foreground "#459f25"))))
     (font-lock-function-name-face ((t (:foreground "#33FFFF"))))
     (font-lock-keyword-face ((t (:foreground "#3c65dd"))))
     (font-lock-string-face ((t (:foreground "#ffff22"))))
     (font-lock-type-face ((t (:foreground "#338833"))))
     (font-lock-constant-face ((t (:foreground "#eeeeec"))))
     (font-lock-variable-name-face ((t (:foreground "#22CC22"))))
     (font-lock-preprocessor-face ((t (:foreground "#a82255"))))
     (minibuffer-prompt ((t (:foreground "#cf72b7" :bold t))))
     (font-lock-warning-face ((t (:foreground "#FF0000" :bold t))))
     )))

(provide 'color-theme-bitemyapp)

;;; color-theme-bitemyapp.el ends here
