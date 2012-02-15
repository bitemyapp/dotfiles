;;; w3m-e21.el --- The stuffs to use emacs-w3m on Emacs 21 and 22

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Yuuichi Teranishi  <teranisi@gohome.org>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This file contains the stuffs to use emacs-w3m on Emacs 21 and 22.
;; For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m-util)
(require 'w3m-proc)
(require 'w3m-image)
(require 'w3m-favicon)
(require 'w3m-fsf)
(require 'w3m-ccl)
(require 'wid-edit)

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-current-process)
  (defvar w3m-current-url)
  (defvar w3m-current-title)
  (defvar w3m-display-inline-images)
  (defvar w3m-favicon-image)
  (defvar w3m-form-use-fancy-faces)
  (defvar w3m-icon-directory)
  (defvar w3m-mode-map)
  (defvar w3m-modeline-process-status-on)
  (defvar w3m-process-queue)
  (defvar w3m-show-graphic-icons-in-header-line)
  (defvar w3m-show-graphic-icons-in-mode-line)
  (defvar w3m-toolbar)
  (defvar w3m-toolbar-buttons)
  (defvar w3m-use-favicon)
  (defvar w3m-use-header-line)
  (defvar w3m-use-tab)
  (defvar w3m-work-buffer-name)
  (defvar w3m-work-buffer-list)
  (autoload 'w3m-image-type "w3m")
  (autoload 'w3m-retrieve "w3m"))

(eval-and-compile
  (unless (fboundp 'frame-current-scroll-bars)
    (defalias 'frame-current-scroll-bars 'ignore))
  (unless (fboundp 'window-fringes)
    (defalias 'window-fringes 'ignore))
  (unless (fboundp 'image-size)
    (defalias 'image-size 'ignore)))

;;; Coding system.

(defun w3m-make-ccl-coding-system
  (coding-system mnemonic docstring decoder encoder)
  "Define a new CODING-SYSTEM by CCL programs DECODER and ENCODER.
CODING-SYSTEM, DECODER and ENCODER must be symbol."
  (make-coding-system coding-system 4 mnemonic docstring
		      (cons decoder encoder)))

(eval-and-compile
  (defconst w3m-ccl-get-ucs-codepoint-with-emacs-unicode
    `((if (r1 == ,(charset-id 'latin-iso8859-1))
	  ((r1 = (r0 + 128)))
	(if (r1 == ,(charset-id 'mule-unicode-0100-24ff))
	    ((r1 = ((((r0 & #x3f80) >> 7) - 32) * 96))
	     (r0 &= #x7f)
	     (r1 += (r0 + 224)))	; 224 == -32 + #x0100
	  (if (r1 == ,(charset-id 'mule-unicode-2500-33ff))
	      ((r1 = ((((r0 & #x3f80) >> 7) - 32) * 96))
	       (r0 &= #x7f)
	       (r1 += (r0 + 9440)))	; 9440 == -32 + #x2500
	    (if (r1 == ,(charset-id 'mule-unicode-e000-ffff))
		((r1 = ((((r0 & #x3f80) >> 7) - 32) * 96))
		 (r0 &= #x7f)
		 (r1 += (r0 + 57312)))	; 57312 == -32 + #xe000
	      ,(if (fboundp 'ccl-compile-lookup-character)
		   '((lookup-character utf-subst-table-for-encode r1 r0)
		     (if (r7 == 0)	; lookup failed
			 (r1 = #xfffd)))
		 '((r1 = #xfffd)))))))
      (if (r1 == #xfffd)
	  (write-repeat ?~)		; unknown character.
	(r0 = r1)))
    "CCL program to convert multibyte char to ucs with emacs-unicode."))

(if (get 'utf-translation-table-for-encode 'translation-table-id)
    ;; For Emacs 21.3 and later.
    (eval
     ;; Compute the ccl program in each time to load this module in order to
     ;; keep the compatibility of this module between Emacs 21.3 and later.
     '(define-ccl-program w3m-euc-japan-encoder
	`(4
	  (loop
	   ,@w3m-ccl-write-euc-japan-character
	   (translate-character utf-translation-table-for-encode r1 r0)
	   ,@w3m-ccl-get-ucs-codepoint-with-emacs-unicode
	   ,@w3m-ccl-generate-ncr))))
  ;; For Emacs 21.[12] that does not have `utf-translation-table-for-encode'.
  (define-ccl-program w3m-euc-japan-encoder
    `(4
      (loop
       ,@w3m-ccl-write-euc-japan-character
       ,@w3m-ccl-get-ucs-codepoint-with-emacs-unicode
       ,@w3m-ccl-generate-ncr))))

(if (get 'utf-translation-table-for-encode 'translation-table-id)
    ;; For Emacs 21.3 and later.
    (eval
     ;; Compute the ccl program in each time to load this module in order to
     ;; keep the compatibility of this module between Emacs 21.3 and later.
     '(define-ccl-program w3m-iso-latin-1-encoder
	`(4
	  (loop
	   ,@w3m-ccl-write-iso-latin-1-character
	   (translate-character utf-translation-table-for-encode r1 r0)
	   ,@w3m-ccl-get-ucs-codepoint-with-emacs-unicode
	   ,@w3m-ccl-generate-ncr))))
  ;; For Emacs 21.[12] that does not have `utf-translation-table-for-encode'.
  (define-ccl-program w3m-iso-latin-1-encoder
    `(4
      (loop
       ,@w3m-ccl-write-iso-latin-1-character
       ,@w3m-ccl-get-ucs-codepoint-with-emacs-unicode
       ,@w3m-ccl-generate-ncr))))

(unless (fboundp 'w3m-ucs-to-char)
  (defun w3m-ucs-to-char (codepoint)
    (or (decode-char 'ucs codepoint) ?~)))

(defun w3m-add-local-hook (hook function &optional append)
  "Add to the buffer-local value of HOOK the function FUNCTION."
  (add-hook hook function append t))

;; Function which returns non-nil when the current display device can
;; show images inline.
(defalias 'w3m-display-graphic-p 'display-images-p)

(defun w3m-display-inline-images-p ()
  "Returns non-nil when images can be displayed under the present
circumstances."
  (and w3m-display-inline-images (display-images-p)))

(defun w3m-create-image (url &optional no-cache referer size handler)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used.
If second optional argument REFERER is non-nil, it is used as Referer: field.
If third optional argument SIZE is non-nil, its car element is used as width
and its cdr element is used as height."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-create-image url no-cache referer size handler))
    (lexical-let ((set-size size)
		  (url url)
		  image size)
      (w3m-process-do-with-temp-buffer
	  (type (progn
		  (set-buffer-multibyte nil)
		  (w3m-retrieve url 'raw no-cache nil referer handler)))
	(when (w3m-image-type-available-p (setq type (w3m-image-type type)))
	  (setq image (create-image (buffer-string) type t :ascent 'center))
	  (if (and w3m-resize-images set-size)
	      (progn
		(set-buffer-multibyte t)
		(setq size (image-size image 'pixels))
		(if (and (null (car set-size)) (cdr set-size))
		    (setcar set-size
			    (/ (* (car size) (cdr set-size)) (cdr size))))
		(if (and (null (cdr set-size)) (car set-size))
		    (setcdr set-size
			    (/ (* (cdr size) (car set-size)) (car size))))
		(if (or (not (eq (car size)
				 (car set-size)))  ; width is different
			(not (eq (cdr size)
				 (cdr set-size)))) ; height is different
		    (lexical-let ((image image))
		      (w3m-process-do
			  (resized (w3m-resize-image
				    (plist-get (cdr image) :data)
				    (car set-size)(cdr set-size)
				    handler))
			(if resized (plist-put (cdr image) :data resized))
			image))
		  image))
	    image))))))

(defun w3m-create-resized-image (url rate &optional referer size handler)
  "Resize an cached image object.
URL is the image file's url.
RATE is resize percentage.
If REFERER is non-nil, it is used as Referer: field.
If SIZE is non-nil, its car element is used as width
and its cdr element is used as height."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-create-image url nil referer size handler))
    (lexical-let ((url url)
		  (rate rate)
		  image)
      (w3m-process-do-with-temp-buffer
	  (type (progn
		  (set-buffer-multibyte nil)
		  (w3m-retrieve url 'raw nil nil referer handler)))
	(when (w3m-image-type-available-p (setq type (w3m-image-type type)))
	  (setq image (create-image (buffer-string) type t :ascent 'center))
	  (progn
	    (set-buffer-multibyte t)
	    (w3m-process-do
		(resized (w3m-resize-image-by-rate
			  (plist-get (cdr image) :data)
			  rate
			  handler))
	      (if resized (plist-put (cdr image) :data resized))
	      image)))))))

(defun w3m-insert-image (beg end image &rest args)
  "Display image on the current buffer.
Buffer string between BEG and END are replaced with IMAGE."
  (let ((face (get-text-property beg 'face)))
    (add-text-properties beg end (list 'display image
				       'intangible image
				       'invisible nil))
    ;; Hide underlines behind inline images.
    ;; Gerd Moellmann <gerd@gnu.org>, the maintainer of Emacs 21, wrote in
    ;; the article <86heyi7vks.fsf@gerd.segv.de> in the list emacs-pretest-
    ;; bug@gnu.org on 18 May 2001 that to show an underline of a text even
    ;; if it has an image as a text property is the feature of Emacs 21.
    ;; However, that behavior is not welcome to the w3m buffers, so we do
    ;; to fix it with the following stuffs.
    (when (and face
	       (face-underline-p face))
      (put-text-property beg end 'face nil)
      (put-text-property beg end 'w3m-underline-face face))))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END."
  (remove-text-properties beg end '(display nil intangible nil))
  (let ((underline (get-text-property beg 'w3m-underline-face)))
    (when underline
      (put-text-property beg end 'face underline))))

(defun w3m-image-type-available-p (image-type)
  "Return non-nil if an image with IMAGE-TYPE can be displayed inline."
  (and (display-images-p)
       (image-type-available-p image-type)))

;;; Form buttons
(defface w3m-form-button-face
  '((((type x w32 mac) (class color))
     :background "lightgrey" :foreground "black"
     :box (:line-width 2 :style released-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify buttons in forms."
  :group 'w3m-face)

(defface w3m-form-button-mouse-face
  '((((type x w32 mac) (class color))
     :background "DarkSeaGreen1" :foreground "black"
     :box (:line-width 2 :style released-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify focused buttons in forms."
  :group 'w3m-face)

(defface w3m-form-button-pressed-face
  '((((type x w32 mac) (class color))
     :background "lightgrey" :foreground "black"
     :box (:line-width 2 :style pressed-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify pressed buttons in forms."
  :group 'w3m-face)

(defvar w3m-form-button-keymap
  (let ((map (copy-keymap widget-keymap)))
    (substitute-key-definition 'widget-forward nil map)
    (substitute-key-definition 'widget-backward nil map)
    map))

(define-widget 'w3m-form-button 'push-button
  "Widget for w3m form button."
  :keymap w3m-form-button-keymap
  :action (function (lambda (widget &optional e)
		      (eval (widget-get widget :w3m-form-action)))))

(defun w3m-form-make-button (start end properties)
  "Make button on the region from START to END."
  (if w3m-form-use-fancy-faces
      (progn
	(unless (memq (face-attribute 'w3m-form-button-face :box)
		      '(nil unspecified))
	  (and (eq ?\[ (char-after start))
	       (eq ?\] (char-before end))
	       (save-excursion
		 (goto-char start)
		 (delete-char 1)
		 (insert " ")
		 (goto-char end)
		 (delete-char -1)
		 (insert " ")
		 (setq start (1+ start)
		       end (1- end)))))
	(let ((w (widget-convert-button
		  'w3m-form-button start end
		  :w3m-form-action (plist-get properties 'w3m-action))))
	  (overlay-put (widget-get w :button-overlay) 'evaporate t))
	(add-text-properties start end properties))
    (add-text-properties start end (append '(face w3m-form-face)
					   properties))))

(defun w3m-setup-widget-faces ()
  (make-local-variable 'widget-button-face)
  (make-local-variable 'widget-mouse-face)
  (make-local-variable 'widget-button-pressed-face)
  (setq widget-button-face 'w3m-form-button-face)
  (setq widget-mouse-face 'w3m-form-button-mouse-face)
  (setq widget-button-pressed-face 'w3m-form-button-pressed-face))

;;; Toolbar
(defcustom w3m-use-toolbar (w3m-image-type-available-p 'xpm)
  "Non-nil activates toolbar of w3m."
  :group 'w3m
  :type 'boolean)

(defvar w3m-e21-toolbar-configurations
  '((auto-resize-tool-bars       . t)
    (auto-raise-tool-bar-buttons . t)
    ;;(tool-bar-button-margin      . 0)
    ;;(tool-bar-button-relief      . 2)
    ))

(defun w3m-e21-setup-toolbar (keymap defs)
  (let ((configs w3m-e21-toolbar-configurations)
	config)
    (while (setq config (pop configs))
      (set (make-local-variable (car config)) (cdr config))))
  ;; Invalidate the default bindings.
  (let ((keys (cdr (key-binding [tool-bar] t)))
	item)
    (while (setq item (pop keys))
      (when (setq item (car-safe item))
	(define-key keymap (vector 'tool-bar item) 'undefined))))
  (let ((n (length defs))
	def)
    (while (>= n 0)
      (setq n (1- n)
	    def (nth n defs))
      (define-key keymap (vector 'tool-bar (aref def 1))
	(list 'menu-item (aref def 3) (aref def 1)
	      :enable (aref def 2)
	      :image (symbol-value (aref def 0)))))))

(defun w3m-e21-make-toolbar-buttons (buttons)
  (dolist (button buttons)
    (let ((up (expand-file-name (concat button "-up.xpm")
				w3m-icon-directory))
	  (down (expand-file-name (concat button "-down.xpm")
				  w3m-icon-directory))
	  (disabled (expand-file-name (concat button "-disabled.xpm")
				      w3m-icon-directory))
	  (icon (intern (concat "w3m-toolbar-" button "-icon")))
	  (props '(:ascent
		   center
		   :color-symbols (("backgroundToolBarColor" . "None")))))
      (unless (boundp icon)
	(if (file-exists-p up)
	    (progn
	      (setq up (apply 'create-image up 'xpm nil props))
	      (if (file-exists-p down)
		  (setq down (apply 'create-image down 'xpm nil props))
		(setq down nil))
	      (if (file-exists-p disabled)
		  (setq disabled (apply 'create-image disabled 'xpm nil props))
		(setq disabled nil))
	      (set icon (vector down up disabled disabled)))
	  (error "Icon file %s not found" up))))))

(defun w3m-setup-toolbar ()
  (when (and w3m-use-toolbar
	     w3m-icon-directory
	     (file-directory-p w3m-icon-directory)
	     (file-exists-p (expand-file-name "antenna-up.xpm"
					      w3m-icon-directory)))
    (w3m-e21-make-toolbar-buttons w3m-toolbar-buttons)
    (w3m-e21-setup-toolbar w3m-mode-map w3m-toolbar)))

(defalias 'w3m-update-toolbar 'ignore)

;;; Header line & Tabs
(defcustom w3m-tab-width 16
  "w3m tab width."
  :group 'w3m
  :set (lambda (symbol value)
	 (custom-set-default symbol
			     (if (and (numberp value) (> value 0))
				 value
			       16)))
  :type '(integer :size 0))

(defface w3m-tab-unselected-face
  '((((type x w32 mac) (class color))
     :background "Gray50" :foreground "Gray20"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "blue" :foreground "black")))
  "*Face to fontify unselected tabs."
  :group 'w3m-face)

(defface w3m-tab-unselected-retrieving-face
  '((((type x w32 mac) (class color))
     :background "Gray50" :foreground "OrangeRed"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "blue" :foreground "OrangeRed")))
  "*Face to fontify unselected tabs which are retrieving their pages."
  :group 'w3m-face)

(defface w3m-tab-selected-face
  '((((type x w32 mac) (class color))
     :background "Gray85" :foreground "black"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "cyan" :foreground "black"))
    (t (:underline t)))
  "*Face to fontify selected tab."
  :group 'w3m-face)

(defface w3m-tab-selected-retrieving-face
  '((((type x w32 mac) (class color))
     :background "Gray85" :foreground "red"
     :box (:line-width -1 :style released-button))
    (((class color))
     (:background "cyan" :foreground "red"))
    (t (:underline t)))
  "*Face to fontify selected tab which is retrieving its page."
  :group 'w3m-face)

(defface w3m-tab-background-face
  '((((type x w32 mac) (class color))
     :background "LightSteelBlue" :foreground "black")
    (((class color))
     (:background "white" :foreground "black")))
  "*Face to fontify background of tab line."
  :group 'w3m-face)

(defvar w3m-modeline-spinner-map nil
  "Keymap used on the spinner in the mode-line.")

(defvar w3m-spinner-map-help-echo "mouse-2 kills the current process"
  "String used for the :help-echo property on the spinner.")

(defun w3m-setup-header-line ()
  (setq header-line-format
	(cond (w3m-use-tab
	       '(:eval (w3m-tab-line)))
	      (w3m-use-header-line
	       (list
		(propertize
		 "Location: "
		 'face 'w3m-header-line-location-title-face)
		'(:eval
		  (propertize
		   (if (stringp w3m-current-url)
		       (replace-regexp-in-string "%" "%%" w3m-current-url)
		     "")
		   'face 'w3m-header-line-location-content-face
		   'local-map (let ((map (make-sparse-keymap)))
				(define-key map [header-line mouse-2]
				  'w3m-goto-url)
				map)
		   'help-echo "mouse-2 prompts to input URL")))))))

(eval-when-compile
  ;; Shut up the byte-compiler in old Emacs 21.
  (unless (fboundp 'force-window-update)
    (defalias 'force-window-update 'ignore)))

(eval-and-compile
  (defalias 'w3m-force-window-update
    (if (and (fboundp 'force-window-update)
	     (not (eq (symbol-function 'force-window-update) 'ignore)))
	(lambda (&optional window) "\
Force redisplay of WINDOW which defaults to the selected window."
	  (force-window-update (or window (selected-window))))
      (lambda (&optional ignore) "\
Wobble the selected window size to force redisplay of the header-line."
	(let ((window-min-height 0))
	  (enlarge-window 1)
	  (unless (eq (next-window nil 'ignore-minibuf) (selected-window))
	    (shrink-window 1)))))))

(defun w3m-tab-drag-mouse-function (event buffer)
  (let ((window (posn-window (event-end event)))
	mpos)
    (when (framep window) ; dropped at outside of the frame.
      (setq window nil
	    mpos (mouse-position))
      (and (framep (car mpos))
	   (car (cdr mpos))
	   (cdr (cdr mpos))
	   (setq window (window-at (car (cdr mpos))
				   (cdr (cdr mpos))
				   (car mpos)))))
    (when window
      (unless (eq (window-buffer window) buffer)
	(select-window window)
	(switch-to-buffer buffer)
	(w3m-force-window-update window)))))

(defun w3m-tab-click-mouse-function (event buffer)
  (let ((window (posn-window (event-start event))))
    (select-window window)
    (switch-to-buffer buffer)
    (w3m-force-window-update window)))

(defvar w3m-tab-map nil)
(make-variable-buffer-local 'w3m-tab-map)

(defvar w3m-tab-spinner-map nil)
(make-variable-buffer-local 'w3m-tab-spinner-map)

(defun w3m-tab-make-keymap ()
  (unless w3m-tab-map
    (setq w3m-tab-map (make-sparse-keymap))
    (let* ((buffer (current-buffer))
	   (drag-action `(lambda (e)
			   (interactive "e")
			   (w3m-tab-drag-mouse-function e ,buffer)))
	   (up-action `(lambda (e)
			 (interactive "e")
			 (w3m-tab-click-mouse-function e ,buffer))))
      (define-key w3m-tab-map [header-line down-mouse-1] 'ignore)
      (define-key w3m-tab-map [header-line down-mouse-2] 'ignore)
      (define-key w3m-tab-map [header-line drag-mouse-1] drag-action)
      (define-key w3m-tab-map [header-line drag-mouse-2] drag-action)
      (define-key w3m-tab-map [header-line mouse-1] up-action)
      (define-key w3m-tab-map [header-line mouse-2] up-action)))
  (unless w3m-tab-spinner-map
    (setq w3m-tab-spinner-map (make-sparse-keymap))
    (define-key w3m-tab-spinner-map [header-line mouse-2]
      `(lambda (e)
	 (interactive "e")
	 (save-current-buffer
	   ;; Why the `(w3m-process-stop BUFFER)' doesn't work?
	   (set-buffer ,(current-buffer))
	   (call-interactively 'w3m-process-stop))))))

(defvar w3m-tab-line-format nil
  "Internal variable used to keep contents to be shown in the header-line.
This is a buffer-local variable.")
(make-variable-buffer-local 'w3m-tab-line-format)

(defvar w3m-tab-timer nil
  "Internal variable used to say time has not gone by after the tab-line
was updated last time.  It is used to control the `w3m-tab-line'
function running too frequently, set by the function itself and
cleared by a timer.")
(make-variable-buffer-local 'w3m-tab-timer)

(defvar w3m-tab-half-space
  (propertize " " 'display '(space :width 0.5))
  "The space of half width.")

(defvar w3m-tab-separator
  (propertize " "
	      'face 'w3m-tab-background-face
	      'display '(space :width 0.5))
  "String used to separate tabs.")

(defun w3m-tab-line ()
  (or (and w3m-tab-timer w3m-tab-line-format)
      (let* ((current (current-buffer))
	     (buffers (w3m-list-buffers))
	     (breadth 1)
	     (fringes (window-fringes))
	     (width (+ (window-width)
		       (/ (float (+ (or (car fringes) 0)
				    (or (nth 1 fringes) 0)))
			  (frame-char-width))
		       ;; Assume that the vertical scroll-bar has
		       ;; the width of two space characters.
		       (if (car (frame-current-scroll-bars)) 2 0)))
	     (nbuf (length buffers))
	     (graphic (and window-system
			   w3m-show-graphic-icons-in-header-line))
	     (margin (if window-system
			 (+ (if graphic 3.0 0.5)
			    ;; Right and left shadows.
			    (/ 2.0 (frame-char-width)))
		       1))
	     (spinner (when w3m-process-queue
			(w3m-make-spinner-image)))
	     buffer title data datum process favicon keymap face icon line)
	(setq w3m-tab-timer t)
	(run-at-time 0.1 nil
		     (lambda (buffer)
		       (when (buffer-live-p buffer)
			 (with-current-buffer buffer
			   (setq w3m-tab-timer nil)
			   (when (and (eq (selected-window)
					  (get-buffer-window buffer))
				      w3m-process-queue)
			     (inline (w3m-force-window-update))))))
		     current)
	(save-current-buffer
	  (while buffers
	    (set-buffer (setq buffer (pop buffers)))
	    (setq title (w3m-current-title))
	    (setq breadth
		  (max breadth
		       ;; There may be a wide character in the beginning of
		       ;; the title.
		       (if (> (length title) 0)
			   (char-width (aref title 0))
			 0)))
	    (push (list (eq current buffer)
			w3m-current-process
			title
			(when w3m-use-favicon w3m-favicon-image)
			w3m-tab-map)
		  data)))
	(setq width (if (> (* nbuf (+ margin w3m-tab-width)) width)
			(max (truncate (- (/ width nbuf) margin)) breadth)
		      w3m-tab-width))
	(while data
	  (setq datum (pop data)
		current (car datum)
		process (nth 1 datum)
		title (nth 2 datum)
		favicon (nth 3 datum)
		keymap (nth 4 datum)
		face (if process
			 (if current
			     'w3m-tab-selected-retrieving-face
			   'w3m-tab-unselected-retrieving-face)
		       (if current
			   'w3m-tab-selected-face
			 'w3m-tab-unselected-face))
		icon (when graphic
		       (cond
			(process
			 (when spinner
			   (propertize
			    " "
			    'display spinner
			    'face face
			    'local-map w3m-tab-spinner-map
			    'help-echo w3m-spinner-map-help-echo)))
			(favicon
			 (propertize
			  " "
			  'display favicon
			  'face face
			  'local-map keymap
			  'help-echo title))))
		breadth (cond (icon width)
			      (graphic (+ 2 width))
			      (t width)))
	  (push
	   (list
	    icon
	    (propertize
	     (concat
	      (when graphic w3m-tab-half-space)
	      (replace-regexp-in-string
	       "%" "%%"
	       (if (and (> (string-width title) breadth)
			(> breadth 6))
		   (truncate-string-to-width
		    (concat (truncate-string-to-width title (- breadth 3))
			    "...")
		    breadth nil ?.)
		 (truncate-string-to-width title breadth nil ?\ ))))
	     'face face
	     'local-map keymap
	     'help-echo title)
	    w3m-tab-separator)
	   line))
	(setq w3m-tab-line-format
	      (concat (apply 'concat (apply 'nconc line))
		      (propertize (make-string (window-width) ?\ )
				  'face 'w3m-tab-background-face))))))

(defun w3m-update-tab-line ()
  "Update tab line by a side effect."
  (when w3m-use-tab
    ;; The following form will cause a problem if a user doesn't set the
    ;; cursor color explicitly, because the `cursor-color' value is always
    ;; black in such a case whatever the color of the default face may be.
    ;; cf. <URL:http://news.gmane.org/group/gmane.emacs.w3m/thread=4605>

    ;;(set-cursor-color (frame-parameter (selected-frame) 'cursor-color))))

    ;; We use wobbling of the window size instead.
    (let ((window-min-height 0))
      (enlarge-window 1)
      (unless (eq (next-window nil 'ignore-minibuf) (selected-window))
	(shrink-window 1)))))

(defun w3m-e21-switch-to-buffer (buffer &optional norecord)
  "Run `switch-to-buffer' and redisplay the header-line.
Redisplaying is done by wobbling the window size."
  (interactive "BSwitch to buffer: ")
  (prog1
      (switch-to-buffer buffer norecord)
    (when (and header-line-format
	       (eq major-mode 'w3m-mode))
      (w3m-force-window-update))))

(defun w3m-e21-subst-switch-to-buffer-keys ()
  "Substitute keys for `switch-to-buffer' with `w3m-e21-switch-to-buffer'."
  (substitute-key-definition 'switch-to-buffer 'w3m-e21-switch-to-buffer
			     w3m-mode-map global-map))

(add-hook 'w3m-mode-setup-functions 'w3m-tab-make-keymap)
(add-hook 'w3m-mode-setup-functions 'w3m-setup-header-line)
(add-hook 'w3m-mode-setup-functions 'w3m-setup-widget-faces)
(add-hook 'w3m-mode-setup-functions 'w3m-e21-subst-switch-to-buffer-keys)
(add-hook 'w3m-select-buffer-hook 'w3m-force-window-update)

;; Graphic icons.
(defcustom w3m-space-before-modeline-icon ""
  "String of space character(s) to be put in front of the mode-line icon.
It may be better to use one or more spaces if you are using oblique or
italic font in the modeline."
  :group 'w3m
  :type 'string)

(defvar w3m-spinner-image-file nil
  "Image file used to show a spinner in the header-line.")

(defvar w3m-spinner-image-frames 3
  "Number of frames which the spinner image contains.")

(defvar w3m-spinner-image-index 0
  "Counter used to rotate spinner images.  This is a buffer-local variable.")
(make-variable-buffer-local 'w3m-spinner-image-index)

;; Images to be displayed in the modeline.
(defvar w3m-modeline-process-status-on-icon nil)
(defvar w3m-modeline-image-status-on-icon nil)
(defvar w3m-modeline-status-off-icon nil)
(defvar w3m-modeline-ssl-image-status-on-icon nil)
(defvar w3m-modeline-ssl-status-off-icon nil)

(defun w3m-initialize-graphic-icons (&optional force)
  "Make icon images which will be displayed in the mode-line."
  (interactive "P")
  (let ((defs '((w3m-modeline-status-off-icon
		 "state-00.xpm"
		 w3m-modeline-status-off)
		(w3m-modeline-image-status-on-icon
		 "state-01.xpm"
		 w3m-modeline-image-status-on)
		(w3m-modeline-ssl-status-off-icon
		 "state-10.xpm"
		 w3m-modeline-ssl-status-off)
		(w3m-modeline-ssl-image-status-on-icon
		 "state-11.xpm"
		 w3m-modeline-ssl-image-status-on)))
	def icon file status keymap)
    (while defs
      (setq def (car defs)
	    defs (cdr defs)
	    icon (car def)
	    file (nth 1 def)
	    status (nth 2 def))
      (if (and window-system
	       w3m-show-graphic-icons-in-mode-line
	       (display-images-p)
	       (image-type-available-p 'xpm)
	       w3m-icon-directory
	       (file-directory-p w3m-icon-directory)
	       (file-exists-p
		(setq file (expand-file-name file w3m-icon-directory))))
	  (progn
	    (when (or force (not (symbol-value icon)))
	      (unless keymap
		(setq keymap (make-mode-line-mouse-map 'mouse-2
						       'w3m-reload-this-page)))
	      (set icon (propertize
			 "  "
			 'display (create-image file 'xpm nil :ascent 'center)
			 'local-map keymap
			 'help-echo "mouse-2 reloads this page"))
	      (put icon 'risky-local-variable t)
	      (put status 'risky-local-variable t))
	    (when (stringp (symbol-value status))
	      ;; Save the original status strings as properties.
	      (put status 'string (symbol-value status)))
	    (set status (list "" 'w3m-space-before-modeline-icon icon)))
	;; Don't use graphic icons.
	(when (get status 'string)
	  (set status (get status 'string)))))
    ;; Spinner
    (when (and (or force (not w3m-spinner-image-file))
	       (image-type-available-p 'gif)
	       w3m-icon-directory
	       (file-directory-p w3m-icon-directory)
	       (file-exists-p
		(setq file (expand-file-name "spinner.gif"
					     w3m-icon-directory))))
      (setq w3m-spinner-image-file file)
      (define-key (setq w3m-modeline-spinner-map (make-sparse-keymap))
	[mode-line mouse-2]
	'w3m-process-stop)
      (put 'w3m-modeline-process-status-on 'risky-local-variable t)
      (put 'w3m-modeline-process-status-on-icon 'risky-local-variable t))
    (if (and window-system
	     w3m-show-graphic-icons-in-mode-line
	     w3m-spinner-image-file)
	(progn
	  (when (stringp w3m-modeline-process-status-on)
	    ;; Save the original status strings as properties.
	    (put 'w3m-modeline-process-status-on 'string
		 w3m-modeline-process-status-on))
	  (setq w3m-modeline-process-status-on
		'(""
		  w3m-space-before-modeline-icon
		  w3m-modeline-process-status-on-icon)))
      (when (get 'w3m-modeline-process-status-on 'string)
	(setq w3m-modeline-process-status-on
	      (get 'w3m-modeline-process-status-on 'string))))))

(defun w3m-make-spinner-image ()
  "Make an image used to show a spinner.
It should be called periodically in order to spin the spinner."
  (when w3m-spinner-image-file
    (unless (< (incf w3m-spinner-image-index) w3m-spinner-image-frames)
      (setq w3m-spinner-image-index 0))
    (let ((image (create-image w3m-spinner-image-file 'gif nil
			       :ascent 'center :mask 'heuristic
			       :index w3m-spinner-image-index)))
      (setq w3m-modeline-process-status-on-icon
	    (propertize "  "
			'display image
			'local-map w3m-modeline-spinner-map
			'help-echo w3m-spinner-map-help-echo))
      image)))

(provide 'w3m-e21)

;;; w3m-e21.el ends here
