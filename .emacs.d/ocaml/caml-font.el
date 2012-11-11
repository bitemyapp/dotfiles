;; useful colors


(defun caml-make-face-italic (face)
  (condition-case nil (make-face-italic face) (error nil)))
(defun caml-make-face-bold (face)
  (condition-case nil (make-face-bold face) (error nil)))
(defun caml-make-face-unitalic (face)
  (condition-case nil (make-face-unitalic face) (error nil)))
(defun caml-make-face-unbold (face)
  (condition-case nil (make-face-unbold face) (error nil)))


(cond
; ((and (x-display-color-p)
;       (not (memq 'font-lock-type-face (face-list))))
 (t
  ; make the necessary faces
  (make-face 'caml-font-lock-comment-face)
  (set-face-foreground 'caml-font-lock-comment-face "Firebrick")
  (caml-make-face-italic 'caml-font-lock-comment-face)
  (make-face 'caml-font-lock-reference-face)
  (set-face-foreground 'caml-font-lock-reference-face "Firebrick")
  (make-face 'caml-font-lock-local-face)
  (set-face-foreground 'caml-font-lock-local-face "Pink4")
  (make-face 'caml-font-lock-string-face)
  (set-face-foreground  'caml-font-lock-string-face "DarkGreen")
  (make-face 'caml-font-lock-function-name-face)
  (set-face-foreground 'caml-font-lock-function-name-face "Blue2")
  (make-face 'caml-font-lock-keyword-face)
  (set-face-foreground 'caml-font-lock-keyword-face "purple")
  (caml-make-face-bold 'caml-font-lock-keyword-face)
  (make-face 'caml-font-lock-variable-name-face)
  (set-face-foreground 'caml-font-lock-variable-name-face "DarkOliveGreen4")
  (make-face 'caml-font-lock-type-face)
  (set-face-foreground 'caml-font-lock-type-face "darkorange2")
  (caml-make-face-bold 'caml-font-lock-type-face)
  (make-face 'caml-font-lock-reference-face)
  (set-face-foreground 'caml-font-lock-reference-face "CadetBlue")
))


; The same definition is in caml.el:
; we don't know in which order they will be loaded.
(defvar caml-quote-char "'"
  "*Quote for character constants. \"'\" for Objective Caml, \"`\" for Caml-Light.")

; (defconst caml-font-lock-keywords
(setq caml-font-lock-keywords

  (list
;comments
   '("\\(^\\|[^\"]\\)\\((\\*[^*]*\\*+\\([^)*][^*]*\\*+\\)*)\\)"
     2 'caml-font-lock-comment-face)
;character literals
   (cons (concat caml-quote-char "\\(\\\\\\([ntbr" caml-quote-char "\\]\\|"
		 "[0-9][0-9][0-9]\\)\\|.\\)" caml-quote-char
		 "\\|\"[^\"\\]*\\(\\\\\\(.\\|\n\\)[^\"\\]*\\)*\"")
	 'font-lock-string-face)
;labels (and open)
   '("\\([?]?\\<[A-Za-zיטא][A-Za-zיטא0-9_']*:\\)\\([^:=]\\|\\'\\|$\\)" 1
     'caml-font-lock-variable-name-face)
   '("\\<\\(assert\\|open\\|include\\)\\>\\|[?]?\\<:[A-Za-zיטא][A-Za-zיטא0-9_']*\\>"
     . 'caml-font-lock-variable-name-face)
;modules and constructors
   '("\\(\\<\\|:\\)\\([A-Z][A-Za-zיטא0-9_']*\\)\\>"
     2 'caml-font-lock-function-name-face)
   '("`[A-Za-zיטא][A-Za-zיטא0-9_']*\\>" . 'caml-font-lock-function-name-face)
;definition
   (cons (concat
	  "\\<\\(a\\(nd\\|s\\)\\|c\\(onstraint\\|lass\\)"
	  "\\|ex\\(ception\\|ternal\\)\\|fun\\(ct\\(ion\\|or\\)\\)?"
	  "\\|in\\(herit\\|itializer\\)?\\|let"
	  "\\|m\\(ethod\\|utable\\|odule\\)"
	  "\\|of\\|p\\(arser\\|rivate\\)\\|rec\\|type"
	  "\\|v\\(al\\(ue\\)?\\|irtual\\)\\)\\>")
	 (quote 'caml-font-lock-type-face))
;blocking
   '("\\(\\<\\|:\\)\\(begin\\|end\\|object\\|s\\(ig\\|truct\\)\\)\\>"
     2 'caml-font-lock-keyword-face)
;control
   (cons (concat
	  "\\<\\(do\\(ne\\|wnto\\)?\\|else\\|for\\|if"
	  "\\|lazy\\|match\\|new\\|or\\|t\\(hen\\|o\\|ry\\)"
	  "\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>"
	  "\\|\|\\|->\\|&\\|#")
	 (quote 'caml-font-lock-reference-face))
   '("\\<raise\\>" . 'caml-font-lock-comment-face)))

(defconst inferior-caml-font-lock-keywords
  (append
   (list
;inferior
    '("^[#-]" . 'caml-font-lock-comment-face)
;labels
    '("[? \t]:[A-Za-zיטא][A-Za-zיטא0-9_']*\\>" . 'caml-font-lock-variable-name-face))
   caml-font-lock-keywords))
    
;; font-lock commands are similar for caml-mode and inferior-caml-mode
(setq caml-mode-hook
      '(lambda ()
	 (cond
	  ((fboundp 'global-font-lock-mode)
	   (make-local-variable 'font-lock-defaults)
	   (setq font-lock-defaults
		 '(caml-font-lock-keywords nil nil ((?' . "w") (?_ . "w")))))
	  (t
	   (setq font-lock-keywords caml-font-lock-keywords)))
	 (setq font-lock-keywords-only t)
	 (font-lock-mode 1)))

(setq inferior-caml-mode-hooks
      '(lambda ()
	 (cond
	  ((fboundp 'global-font-lock-mode)
	   (make-local-variable 'font-lock-defaults)
	   (setq font-lock-defaults
		 '(inferior-caml-font-lock-keywords
		   nil nil ((?' . "w") (?_ . "w")))))
	  (t
	   (setq font-lock-keywords inferior-caml-font-lock-keywords)))
	 (setq font-lock-keywords-only t)
	 (font-lock-mode 1)))

(provide 'caml-font)
