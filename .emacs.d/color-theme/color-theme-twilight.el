;;; color-theme-twilight.el --- Twilight Color Theme for Emacs.

;; Copyright (C) 2008 Marcus Crafter <crafterm@redartisan.com>

;; Author: Marcus Crafter
;; Adapted-By: Yesudeep Mangalapilly
;; Keywords: textmate twilight color theme
;; URL: https://github.com/crafterm/twilight-emacs
;; Version: 0.2
;; Package-Requires: ((color-theme "6.6.1"))

;; This file is NOT a part of GNU Emacs.

;;; License:

;; MIT License
;; -----------
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall
;; be included in all copies or substantial portions of the
;; Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Usage:
;;
;; Defines a colour scheme resembling that of the original TextMate Twilight colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/twilight-emacs/color-theme-twilight.el")


;; TODO: Several areas still require improvement such as recognition of code that ruby-mode doesn't
;; yet pick up (eg. parent classes), Rails/Merb keywords, or non Ruby code related areas
;; (eg. dired, HTML, etc). Please feel free to customize further and send in any improvements,
;; patches most welcome.
;;
;; Credits due to the excellent TextMate Twilight theme
;;
;; Thanks to Travis Jeffery for ido-mode and fixes to the minibuffer-prompt to fit in with the rest of the theme

;;; Code:

(eval-when-compile
  (require 'color-theme))


(defun color-theme-twilight ()
  "TextMate Twilight theme for GNU Emacs."
  (interactive)
  (color-theme-install
	'(color-theme-twilight
	  ((background-color . "#141414")
		(background-mode . dark)
		(border-color . "black")
		(cursor-color . "#DDDD00")
		(foreground-color . "#F8F8F8")
		(mouse-color . "sienna1"))
	  (default ((t (:background "#141414" :foreground "#CACACA"))))
	  (blue ((t (:foreground "blue"))))
	  (border-glyph ((t (nil))))
	  (buffers-tab ((t (:background "#141414" :foreground "#CACACA"))))
	  (font-lock-builtin-face ((t (:foreground "#CACACA"))))
	  (font-lock-comment-face ((t (:foreground "#5F5A60"))))
	  (font-lock-constant-face ((t (:foreground "#CF6A4C"))))
	  (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
	  (font-lock-function-name-face ((t (:foreground "#9B703F"))))
	  (font-lock-keyword-face ((t (:foreground "#CDA869"))))
	  (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
	  (font-lock-reference-face ((t (:foreground "SlateBlue"))))

	  ;; Enhanced-Ruby-Mode
	  (ruby-string-delimiter-face  ((t (:foreground "#5A6340"))))
	  (ruby-regexp-delimiter-face ((t (:foreground "orange"))))
	  (ruby-heredoc-delimiter-face ((t (:foreground "#9B859D"))))
	  (ruby-op-face ((t (:foreground "#CDA869"))))

	  (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
	  (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

	  (minibuffer-prompt ((t (:foreground "#5F5A60"))))
	  (ido-subdir ((t (:foreground "#CF6A4C"))))
	  (ido-first-match ((t (:foreground "#8F9D6A"))))
	  (ido-only-match ((t (:foreground "#8F9D6A"))))
	  (mumamo-background-chunk-submode ((t (:background "#222222")))) 

	  (linum ((t (:background "#141314" :foreground "#2D2B2E"))))
	  (hl-line ((t (:background "#212121"))))  
          (region ((t (:background "#373446"))))
	  (yas/field-highlight-face ((t (:background "#27292A"))))
	  (mode-line ((t (:background "grey75" :foreground "black" :height 0.8))))
          (mode-line-inactive ((t (:background "grey10" :foreground "grey40" :box (:line-width -1 :color "grey20") :height 0.8))))

          (magit-item-highlight ((t (:background "#191930"))))
          (magit-diff-add ((((class color) (background dark)) (:foreground "green"))))
          (org-hide ((((background dark)) (:foreground "#141414"))))
          (outline-4 ((t (:foreground "#8F8A80"))))

	  (font-lock-string-face ((t (:foreground "#8F9D6A"))))
	  (font-lock-type-face ((t (:foreground "#9B703F"))))
	  (font-lock-variable-name-face ((t (:foreground "#7587A6"))))
	  (font-lock-warning-face ((t (:background "#EE799F" :foreground "red"))))
	  (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
	  (region ((t (:background "#27292A"))))
	  (highlight ((t (:background "#111111"))))
	  (highline-face ((t (:background "SeaGreen"))))
	  (left-margin ((t (nil))))
	  (text-cursor ((t (:background "yellow" :foreground "black"))))
	  (toolbar ((t (nil))))
	  (underline ((nil (:underline nil))))
	  (zmacs-region ((t (:background "snow" :foreground "blue")))))))


(color-theme-twilight)

(provide 'color-theme-twilight)

;;; color-theme-twilight.el ends here