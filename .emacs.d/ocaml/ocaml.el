; (setq load-path (cons "~/lib/gnu-emacs/tuareg-mode/" load-path))
(setq load-path (cons "~/lib/gnu-emacs/caml-mode/" load-path))
(require 'caml)
(require 'inf-caml)

(defun caml-sit-for (second &optional mili redisplay)
   (if running-xemacs
       (sit-for (if mili (+ second (* mili 0.001)) s) redisplay)
     (sit-for second mili redisplay)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                            Inferior Caml mode

; Augment Caml mode, so you can process Caml code in the source files.

(require 'comint)

(defvar caml-display-when-eval nil
  "*If true, display the inferior caml buffer when evaluating expressions.")

(defvar inferior-caml-mode-map nil)
(if inferior-caml-mode-map nil
  (setq inferior-caml-mode-map
        (copy-keymap comint-mode-map)))

(defun caml-show-buffer () (interactive)
  (caml-run-process-if-needed)
  (caml-show-subshell)
  (let ((buf (current-buffer))
        (caml-buf  (get-buffer inferior-caml-buffer-name))
        (count 0))
    (while
        (and (< count 4)
             (not (equal (buffer-name (current-buffer))
                         inferior-caml-buffer-name)))
      (goto-next-window)
      (setq count (+ count 1)))
    (if  (equal (buffer-name (current-buffer))
                inferior-caml-buffer-name)
        (end-of-buffer))
    (while
        (> count 0)
      (goto-previous-window)
      (setq count (- count 1)))
    ))
  

(define-key caml-mode-map "\M-\C-x" 'caml-eval-phrase)
(define-key caml-mode-map "\M-\C-e" 'caml-just-eval-phrase)
(define-key caml-mode-map "\C-c\C-r" 'caml-eval-region)
(define-key caml-mode-map "\C-c\C-l" 'caml-eval-buffer)
(define-key caml-mode-map "\C-c\C-s" 'caml-show-buffer)
(define-key caml-mode-map "\C-x\C-l" 'caml-show-buffer)
(define-key caml-mode-map "\M-\C-l" 'caml-show-buffer)
; (define-key caml-mode-map "\M-\C-q" 'caml-indent-phrase)
(define-key caml-mode-map "\M-j" 'caml-indent-phrase)

(if running-xemacs nil
  (define-key caml-mode-map [C-return] 'caml-save-and-make)
  )
(define-key caml-mode-map "\M-\C-m" 'next-error)

(defvar inferior-caml-program "ocaml"
  "*Default program name for invoking an inferior Caml from Emacs.")

(defconst inferior-caml-buffer-subname "inferior-caml")
(defconst inferior-caml-buffer-name
  (concat "*" inferior-caml-buffer-subname "*"))

(defvar inferior-caml-output nil)
(defun inferior-caml-signal-output (s)
  (if (string-match "[^ ]" s) (setq inferior-caml-output t)))

(defun inferior-caml-mode ()
  "Major mode for interacting with an inferior Caml process.
Runs a Caml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in Caml mode.

\\{inferior-caml-mode-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp "^# ?")
  (setq major-mode 'inferior-caml-mode)
  (setq mode-name "Inferior Caml")
  (use-local-map inferior-caml-mode-map)
  (setq local-abbrev-table nil)
  (setq comint-output-filter-functions
        (list (function inferior-caml-signal-output)))
  (setq compilation-ask-about-save nil)
  (abbrev-mode -1)

  (run-hooks 'inferior-caml-mode-hooks))

(defun run-caml (&optional cmd)
  "Run an inferior Caml process.
Input and output via buffer `*inferior-caml*'."
  (interactive
   (list (if (not (comint-check-proc inferior-caml-buffer-name))
	     (read-from-minibuffer "Caml toplevel to run: "
				   inferior-caml-program))))
  (caml-run-process-if-needed cmd)
  (switch-to-buffer-other-window inferior-caml-buffer-name))

(defun caml-run-process-if-needed (&optional cmd)
  (if (not cmd)
      (if (comint-check-proc inferior-caml-buffer-name)
	  (setq cmd inferior-caml-program)
	(setq cmd (read-from-minibuffer "Caml toplevel to run: "
					inferior-caml-program))))
  (setq inferior-caml-program cmd)
  (if (not (comint-check-proc inferior-caml-buffer-name))
      (let ((cmdlist (caml-args-to-list cmd))
            (process-connection-type nil))
	(set-buffer (apply (function make-comint)
			   inferior-caml-buffer-subname
			   (car cmdlist) nil (cdr cmdlist)))
	(inferior-caml-mode)
        t)
    nil))

(defun caml-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (caml-args-to-list (substring string (+ 1 where)
							(length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (caml-args-to-list (substring string pos
							(length string)))))))))

(defun caml-eval-region (start end)
  "Send the current region to the inferior Caml process."
  (interactive "r")
  (save-excursion (caml-run-process-if-needed))
  (save-excursion
    (comint-send-region inferior-caml-buffer-name start end)
    (goto-char end)
    (skip-chars-backward " \t\n")
    (if (not (and (>= (point) 2)
		  (prog2 (backward-char 2) (looking-at ";;"))))
	(comint-send-string inferior-caml-buffer-name ";;\n"))
    (if caml-display-when-eval
        (display-buffer inferior-caml-buffer-name t))))


;; a few changes and more functions

;; to send phrases for toplevel so that it can be 

(defun caml-skip-blank-forward ()
  (if (looking-at "[ \t\n]*\\((\\*\\([^*]\\|[^(]\\*[^)]\\)*\\*)[ \t\n]*\\)*")
      (goto-char (match-end 0))))

(defun caml-find-phrase (&optional min-pos max-pos)
  "Find the CAML phrase containing the point.
Return the positin of the beginning of the phrase, and move point
to the end.
"
  (interactive)
  (while
      (and (search-backward ";;" min-pos 'move)
           (or (caml-in-literal-p)
               (and caml-last-comment-start (caml-in-comment-p)))
           ))
  (if (looking-at ";;") (forward-char 2))
  (caml-skip-blank-forward)
  (let ((beg (point)))
    (while
        (and (search-forward ";;" max-pos 1)
             (or (caml-in-literal-p)
                 (and caml-last-comment-start (caml-in-comment-p)))
             ))
    (if (eobp) (newline))
    beg))

(defun caml-just-eval-phrase (arg &optional min max)
  "Send the phrase containing the point to the CAML process."
  (interactive "p")
  (let ((beg))
    (while (> arg 0)
      (setq arg (- arg 1))
      (setq beg  (caml-find-phrase min max))
      (caml-eval-region beg (point))
      (comint-send-string inferior-caml-buffer-name "\n")
      )
    beg))

(defun caml-indent-region (beg end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) end)
      (goto-char beg)
      (while (< (point) (point-max))
        (caml-indent-command)
        (forward-line 1)
        ))))
     
(defun caml-indent-phrase (arg)
  (interactive "p")
  (save-excursion
    (while (> arg 0)
      (setq arg (- arg 1))
      (caml-indent-region (caml-find-phrase) (point)))))

(defun caml-wait-output (&optional before after)
  (let ((c 1))
    (caml-sit-for 0 (or before 1))
    (let ((c 1))
      (while (and (not inferior-caml-output) (< c 99) (caml-sit-for 0 c t))
        (setq c (+ c 1))))
    (caml-sit-for (or after 0) 1)))

(defvar caml-previous-output)
(defun caml-eval-phrase (arg &optional min max)
  "Send the phrase to Caml toplevel process
   Return nil if noerror and position of error if any."
  (interactive "p")
  (if (save-excursion (caml-run-process-if-needed))
      (progn
        (setq inferior-caml-output nil)
        (caml-wait-output 10 1)))
  (if (< arg 1) (caml-just-eval-phrase 1 min max)
    (let ((proc (get-buffer-process inferior-caml-buffer-name))
          (buf (current-buffer))
          (previous-output) (orig) (beg) (end) (error))
      (save-window-excursion
        (while (and (> arg 0) (not error))
          (setq previous-output (marker-position (process-mark proc)))
          (setq caml-previous-output previous-output)
          (setq inferior-caml-output nil)
          (setq orig (caml-just-eval-phrase 1 min max))
          (caml-wait-output)
          (switch-to-buffer inferior-caml-buffer-name  nil)
          (goto-char previous-output)
          (cond ((re-search-forward
                  " *Characters \\([01-9][01-9]*\\)-\\([1-9][01-9]*\\):\n[^W]"
                  (point-max) t)
                 (setq beg (+ orig (string-to-int (caml-match-string 1))))
                 (setq end (+ orig (string-to-int (caml-match-string 2))))
                 (switch-to-buffer buf)
                 (goto-char beg)
                 (setq error beg)
                 )
                ((looking-at
                  "Toplevel input:\n[>]\\([^\n]*\\)\n[>]\\(\\( *\\)^*\\)\n")
                 (let ((expr (caml-match-string 1))
                       (column (-   (match-end 3) (match-beginning 3)))
                       (width (-   (match-end 2) (match-end 3))))
                   (if (string-match  "^\\(.*\\)[<]EOF[>]$" expr)
                       (setq expr (substring expr (match-beginning 1) (match-end 1))))
                   (switch-to-buffer buf)
                   (re-search-backward
                    (concat "^" (regexp-quote expr) "$")
                    (- orig 10))
                   (goto-char (+ (match-beginning 0) column))
                   (setq end (+ (point) width)))
                 (setq error beg))
                ((looking-at
                  "Toplevel input:\n>[.]*\\([^.].*\n\\)\\([>].*\n\\)*[>]\\(.*[^.]\\)[.]*\n")
                 (let* ((e1 (caml-match-string 1))
                        (e2 (caml-match-string 3))
                        (expr
                         (concat
                          (regexp-quote e1) "\\(.*\n\\)*" (regexp-quote e2))))
                   (switch-to-buffer buf)
                   (re-search-backward expr orig 'move)
                   (setq end (match-end 0)))
                 (setq error beg))
                (t
                 (switch-to-buffer buf)))
          (setq arg (- arg 1))
          )
        (pop-to-buffer inferior-caml-buffer-name)
        (if error
            (goto-char (point-max))
          (goto-char previous-output)
          (goto-char (point-max)))
        (pop-to-buffer buf))
      (if error (progn (beep) (caml-overlay-region (point) end))
        (if inferior-caml-output
            (message "No error")
          (message "No output yet...")
          ))
      error)))

(defun caml-insert-last-output ()
  "Insert the result of the evaluation of previous phrase"
  (interactive)
  (let ((pos (process-mark (get-buffer-process inferior-caml-buffer-name))))
  (insert-buffer-substring inferior-caml-buffer-name
                           caml-previous-output (- pos 2))))

(defun caml-overlay-region (beg end &optional wait)
  (interactive "%r")
  (cond ((fboundp 'make-overlay)
         (if caml-error-overlay ()
           (setq caml-error-overlay (make-overlay 1 1))
           (overlay-put caml-error-overlay 'face 'region))
         (unwind-protect
             (progn
               (move-overlay caml-error-overlay beg end (current-buffer))
               (beep) (if wait (read-event) (caml-sit-for 60)))
           (delete-overlay caml-error-overlay)))))  

(defun caml-compilation-finish (buffer status)
  (if (string-match "^finished" status) nil
    (compilation-parse-errors nil nil)
    (if (null compilation-error-list) nil
        (run-with-timer 0.1 nil 'next-error)
      )))
                         
(defun caml-save-and-make (ask)
  (interactive "p")
  (save-buffer (current-buffer))
  (setq compilation-ask-about-save nil)
  (setq compilation-finish-function 'caml-compilation-finish)
  (if (not compile-command) (setq compile-command "make"))
  (if (> ask 1)
      (setq compile-command
      (read-from-minibuffer "compile: " compile-command)))
  (compile compile-command))
                         
(setq caml-shell-active t)

(defun caml-eval-buffer (p)
  (interactive "p")
  (let ((here (point)) (error))
    (goto-char (point-min))
    (setq error (caml-eval-phrase 200 (point-min) here))
    (if error (set-mark (error)))
    (goto-char here)))

(defun caml-show-structured-comments ()
  (interactive)
  (font-lock-mode 0)
  (let ((begend) (beg) (end) (here (point)))
    (goto-char (point-min))
    (setq begend (point))
 ;    (remove-text-properties begend (point-max) '(face) (current-buffer))
    (while
        (re-search-forward "\\((\\*<[^*]*\\*)\n?\\|(\\*[^*]*>\\*)\\)"
                           (point-max) 'move)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (goto-char beg)
      (if (looking-at "(\\*[^*]*>\\*)") (error "Openning"))
      (add-text-properties begend end '(face bg:Salmon)
                           (current-buffer))
      (goto-char end)
      (setq begend end)
      (re-search-forward "\\((\\*<[^*]*\\*)\\|(\\*[^*]*>\\*)\n?\\)"
                         (point-max) 'move)
      (setq end (match-end 0))
      (goto-char (match-beginning 0))
      (if (looking-at "(\\*<[^*]*\\*)") (error "Closing"))
      (add-text-properties begend end '(face bg:LimeGreen)
                           (current-buffer))
      (goto-char end)
      (setq begend end)
      )
      (add-text-properties begend (point-max) '(face bg:Salmon)
                           (current-buffer))
    (goto-char here)
    ))

(provide 'ocaml)
(if window-system (require 'caml-font))
