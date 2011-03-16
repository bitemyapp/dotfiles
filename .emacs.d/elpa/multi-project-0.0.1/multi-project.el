;;; multi-project.el --- Work with multiple projects

;; Copyright (C) 2010

;; Author: Shawn Ellis <shawn.ellis17@gmail.com>
;; Version: 0.0.1
;; Package-Version: $Id: multi-project.el 257 2010-07-14 13:23:22Z  $
;; Keywords: project
;;

;; multi-project.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; multi-project.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;; The muli-project-roots define the projects managed by
;; multi-project.  The project lists are composed of the name, project
;; directory, name of sub directory found within the project directory
;; for cursor placement, and the TAGS file to use.  If the TAGS entry
;; is missing, multi-project will look in the project directory for a
;; TAGS file.  To use multi-project just add the following lines
;; within your .emacs file:
;; 
;; (require 'multi-project)
;; (setq multi-project-roots '(("proj1" "C:/devenv/proj1/trunk" "Proj1SubDir"
;; 			     "C:/devenv/proj1/trunk/TAGS")
;; 			    ("proj2" "C:/devenv/proj2/trunk" "Proj2SubDir")
;; 			    ("remote" "ellis@host:")))
;;
;; The multi-project-compilation-command variable can be set to a function
;; that provides a customized compilation command.  For example,
;;
;; (defun my-compilation-command (project-list)
;;   (cond ((string-match "proj1" (car project-list))
;; 	 (concat "ant -f " (nth 1 project-list) "/" (nth 2 project-list)
;; 		 "/build.xml"))
;; 	(t
;; 	 (concat "make -C " (nth 1 project-list) "/" (nth 2 project-list)))))
;;
;; (setq multi-project-compilation-command 'my-compilation-command)
;;
;; The following bindings are created for multi-project
;; C-xpj - Project jump              Displays a list of projects
;; C-xpc - Project compile           Run the compilation command for a project
;; C-xpa - Anchor a project          Stores the project to be retrieved via
;;                                   multi-project-last
;; C-xpl - Last project from Anchor  Jumps to the project stored via the anchor
;; C-xpf - MultiProject Find file    Interactively search project files
;;
;; When displaying the list of projects the following bindings are present:
;; s     - Search projects:          Searches from the list of possible projects
;; C-n   - Next project              Move the cursor to the next project
;; C-p   - Previous project          Move the cursor to the previous project
;; a     - Anchor a project          Makes the project available to last project
;; r     - Reset search              Resets the project search
;; q     - quit
;;

;;; Code:

(require 'compile)
(require 'etags)

(defgroup multi-project nil
  "Support for working with multiple projects."
  :prefix "multi-project"
  :group 'convenience)
  
(defcustom multi-project-roots nil
  "A list describing the project, filesystem root, subdirectory under the root, and the TAGS location."
  :group 'multi-project)

(defcustom multi-project-compilation-command 'multi-project-compile-command
  "The fuction to use when compiling a project."
  :group 'multi-project)

(defcustom multi-project-last nil
  "Visits the last project that was switched to."
  :group 'multi-project)

(defvar multi-project-overlay nil
  "Overlay used to highlight the current selection.")

(defvar multi-project-previous-input nil
  "Prior input when performing a search." )

(defvar multi-project-previous-file-input nil
  "Prior input when performing a file search." )

(defconst multi-project-buffer "*mp*"
  "Buffer used for finding projects.")

(defface multi-project-selection-face
  ;; check if inherit attribute is supported
  (if (assq :inherit custom-face-attributes)
      '((t (:inherit highlight :underline nil)))

    '((((class color) (background light))
       (:background "darkseagreen2"))
      (((class color) (background dark))
       (:background "darkolivegreen"))
      (t (:inverse-video t))))
  "Face for highlighting the currently selected file name."
  :group 'multi-project)

(defvar multi-project-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'multi-project-next-line)
    (define-key map (kbd "C-n") 'multi-project-next-line)
    (define-key map (kbd "n") 'multi-project-next-line)
    (define-key map (kbd "<up>") 'multi-project-previous-line)
    (define-key map (kbd "C-p") 'multi-project-previous-line)
    (define-key map (kbd "p") 'multi-project-previous-line)
    (define-key map (kbd "<prior>") 'multi-project-previous-page)
    (define-key map (kbd "<RET>") 'multi-project-display-select)
    (define-key map (kbd "f") 'multi-project-display-select)
    (define-key map (kbd "a") 'multi-project-display-anchor)
    (define-key map (kbd "o") 'multi-project-display-select-other-window)
    (define-key map (kbd "<C-return>") 'multi-project-display-select-other-window)
    (define-key map (kbd "q") 'multi-project-quit)
    (define-key map (kbd "s") 'multi-project-display-search)
    (define-key map (kbd "r") 'multi-project-display-reset)
    (define-key map (kbd "t") 'multi-project-display-change-tags)
    map)
  "Keymap for multi-project.")

(defvar multi-project-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<RET>") 'multi-project-exit-minibuffer)
    (define-key map (kbd "<down>") 'multi-project-next-line)
    (define-key map (kbd "<up>") 'multi-project-previous-line)
    (define-key map (kbd "C-p") 'multi-project-previous-line)
    (define-key map (kbd "C-n") 'multi-project-next-line)
    map)
  "Keymap for multi-project-minibuffer.")

(defun multi-project-dired (projectdir directory &optional searchdirectory otherwindow)
  "Run `dired` on a particular project.
The PROJECTDIR argument specifies the directory and the DIRECTORY
argument is used to place the cursor on a directory within
PROJECTDIR.  The SEARCHDIRECTORY argument specifies a different
directory for the cursor instead of DIRECTORY.  Optional argument
OTHERWINDOW if non-nil, then open up a buffer in a different
windows."
  (if projectdir
      (let ((directorypath projectdir)
            (dir directory))

        (if searchdirectory
            (setq dir searchdirectory))

        (when directorypath
          (if otherwindow
              (dired-other-window directorypath)
            (dired directorypath))
          (goto-char (point-min))
          (when dir
            (goto-char (re-search-forward (concat "[0-9]+:[0-9]+ " dir) nil t))
            (backward-word 1))))))

(defun multi-project-dired-solution (solutionlist &optional searchdirectory otherwindow)
  "Open up a dired window based upon the project.
Argument SOLUTIONLIST
Optional argument SEARCHDIRECTORY
Optional argument OTHERWINDOW open another window."
  (multi-project-dired (nth 1 solutionlist) (nth 2 solutionlist) searchdirectory otherwindow))

(defun multi-project-filter-name (project lst)
  "Filter based upon the the PROJECT name of the LST."
  (car
   (delq nil
	 (mapcar (lambda (x) (and (string= project (car x)) x)) lst))))

(defun multi-project-filter-dir (projectdir lst)
  "Filter based upon the PROJECTDIR of the LST."
  (car
   (delq nil
	 (mapcar
	  (lambda (x) (and (string-match (expand-file-name (nth 1 x))
					 (expand-file-name projectdir))
			   x)) lst))))


(defun multi-project-find-by-directory ()
  "Return the project list from the set of defined projects in multi-projects-roots."
  (interactive)
  (let ((result)
        (directory (replace-regexp-in-string "/plink:" "/" default-directory)))

    (setq directory (expand-file-name (replace-regexp-in-string "/$" "" directory)))
    (setq result (multi-project-filter-dir directory multi-project-roots))
    result))

(defun multi-project-find-by-name(projectname)
  "Returns the project list that corresponds to the project name"
  (multi-project-filter-name projectname multi-project-roots))

(defun multi-project-prompt ()
  "Prompts for the project to work with."
  (let ((result)
        (solution)
        (prompt nil))
    (dolist (item (reverse multi-project-roots) prompt)
      (setq prompt (append (car item) " " prompt)))
    (setq solution (read-from-minibuffer (concat "Project: " prompt "? ") nil))
    (setq result (multi-project-find-by-name solution))
    result))
    
(defun multi-project-compile-command (project-list)
  "Provide a compilation command based upon the PROJECT-LIST."
  (when project-list
    (let ((tree (nth 1 project-list))
          (solution (nth 2 project-list)))
      (when (file-remote-p tree)
	  (setq tree (replace-regexp-in-string "/.*:" ""
					       (expand-file-name tree)))
	  (setq tree (replace-regexp-in-string "/$" "" tree)))
      
      (concat "make -C " tree))))

(defun multi-project-compile-prompt (command)
  "Read the compilation COMMAND from the minibuffer."
  (read-from-minibuffer "Compile command: "
                        command nil nil
                        (if (equal (car compile-history) command)
                            '(compile-history . 1)
                          'compile-history)))

(defun multi-project-compile-buffer-name (mode-name)
  "Return the compilation buffer name based upon the project and MODE-NAME."

  (let ((projectlist (multi-project-find-by-directory)))
    (cond (projectlist
	   (concat "*" (car projectlist) "-" (downcase mode-name) "*"))
	  (t
	   (concat "*" (downcase mode-name) "*")))))

;;;###autoload
(defun multi-project-compile ()
  "Compiles a project based upon the current directory of the buffer."
  (interactive)
  (let ((solutionlist (multi-project-find-by-directory)))
    (cond ((and solutionlist (boundp 'compile-history) compile-history
                (string-match (funcall multi-project-compilation-command solutionlist)
                              (car compile-history)))
           (setq compile-command (car compile-history)))

          (solutionlist
           (setq compile-command
                 (funcall multi-project-compilation-command solutionlist)))
          
          (t
           (setq solutionlist (multi-project-find-by-name multi-project-last))
           (when solutionlist
             (setq compile-command
                   (funcall multi-project-compilation-command solutionlist)))))

    ; Set the function for naming the compilation buffer
    (unless compilation-buffer-name-function
      (setq compilation-buffer-name-function 'multi-project-compile-buffer-name))
    (compile (multi-project-compile-prompt compile-command))))

(defun multi-project-find-root (parentDir childDir)
  "Takes two directories as arguments and return the first directory path that is different Argument PARENTDIR The parent directory of the child.  Argument CHILDDIR A directory found under the parent."
  (interactive)
  
  (let ((tlst (split-string childDir "[/\\]"))
        (lst (split-string parentDir "[/\\]"))
        (fpath)
        (tfpath)
        (index 0)
        (root))
    (while lst
      (setq fpath (car lst))
      (setq lst (cdr lst))
      (setq tfpath (nth index tlst))
      (setq index (1+ index))
      
      (if (string-equal fpath tfpath)
          (if root
              (setq root (append root (list fpath)))
            (setq root (list fpath)))))

    (if (nth index tlst)
        (setq root (append root (list (nth index tlst)))))
    (mapconcat 'identity root "/")))

(defun multi-project-basename (directory)
  "Return the basename of a DIRECTORY."
  (let ((lst (split-string directory "[/\\]")))
    (car (last lst))))

;;;###autoload
(defun multi-project-root ()
  "Jumps to the root of a project based upon current directory."
  (interactive)
  (let ((solutionlist (multi-project-find-by-directory)))
    (if solutionlist
        (let ((searchdir (multi-project-find-root (nth 1 solutionlist)
                                                  default-directory)))
          (multi-project-dired (nth 1 solutionlist) (nth 2 solutionlist)
                               (multi-project-basename searchdir)))
      (multi-project-display-projects))))

(defun multi-project-dirname (filename)
  "Return the directory name of FILENAME."
  (let ((filelist)
        (result))
    (setq filelist (reverse (split-string filename "/")))
    (mapc (lambda (x) (setq result (concat x "/" result)))
          (cdr filelist))
    (directory-file-name result)))

;;;###autoload
(defun multi-project-change-tags(&optional project)
  "Visits tags file based upon current directory"
  (interactive)
  (let ((solutionlist))
    
    (if project
        (setq solutionlist (multi-project-find-by-name project))
      (setq solutionlist (multi-project-find-by-directory)))

    (if solutionlist
        (let ((filename (nth 3 solutionlist)))

	  ;; if TAGS wasn't specified look for one in the top level
	  ;; directory
	  (unless filename
	    (setq filename (concat (nth 1 solutionlist) "/" "TAGS")))

          ;; if TAGS still doesn't exist visit the parent to see if the
          ;; the file exists
          (unless (file-exists-p filename)
            (setq filename (concat (multi-project-dirname (nth 1 solutionlist))
                                   "/" "TAGS")))

          (when (file-exists-p filename)
            (let ((large-file-warning-threshold nil)
                  (tags-add-tables nil))
              (when (get-buffer "TAGS")
                (kill-buffer "TAGS"))
              (visit-tags-table filename)
              (message "TAGS changed to %s" tags-file-name)))))))

;;;###autoload
(defun multi-project-last()
  "Jumps to the last chosen project"
  (interactive)
  (if multi-project-last nil
    (multi-project-anchor))
  
  (let ((result))
    (setq result (multi-project-find-by-name multi-project-last))
    (if result
        (multi-project-dired-solution result))))

;;;###autoload
(defun multi-project-anchor()
  "Chooses a project that will be constant no matter the default directory"
  (interactive)
  (setq multi-project-last (car (multi-project-find-by-directory)))
  (if multi-project-last
      (message "%s anchored" multi-project-last))
  (when multi-project-last
    (multi-project-last)))

(defun multi-project-display-anchor()
  (interactive)
  (let ((project-list (multi-project-select)))
    (when project-list
      (setq multi-project-last (car project-list))
      (message "%s anchored" multi-project-last))))

;;;###autoload
(defun multi-project-display-change-tags()
  (interactive)
  (let ((project-list (multi-project-select)))
    (when project-list
      (multi-project-change-tags (car project-list))
      (message "Loaded tags for %s " (car project-list)))))

          
(defun multi-project-insert-line(key fs)
  (let ((numtabs (ceiling (- (* 2 tab-width) (length key)) tab-width)))
    (if (>= (length key) tab-width)
        (setq numtabs 1))

    (insert key)

    (while (> numtabs 0)
      (insert "\t")
      (setq numtabs (- numtabs 1)))
    (insert fs "\n")))


;;;###autoload
(defun multi-project-display-projects()
  "Displays a buffer with the various projects"
  (interactive)
  (multi-project-create-display multi-project-previous-input)
  (switch-to-buffer multi-project-buffer))

(defun multi-project-display-reset()
  "Resets the filter used for the projects"
  (interactive)
  (setq multi-project-previous-input nil)
  (multi-project-display-projects))


(defun multi-project-create-display(&optional projectkey)
  "Inserts the configured projects into the multi-project buffer"
  (get-buffer-create multi-project-buffer)

  (with-current-buffer multi-project-buffer
    (multi-project-minor-mode 1)
    (setq buffer-read-only nil)
    (setq multi-project-roots (sort multi-project-roots (lambda (a b) (string< (car a) (car b)))))
    (erase-buffer)
    (dolist (item multi-project-roots)
      (if (and projectkey
               (string-match projectkey (car item)))
          (multi-project-insert-line (car item) (nth 1 item)))

      (if (equal projectkey nil)
          (multi-project-insert-line (car item) (nth 1 item))))
    (setq buffer-read-only t)

    (goto-char (point-min))

    (setq multi-project-overlay (make-overlay (point-min) (point-min)))
    (overlay-put multi-project-overlay 'face 'multi-project-selection-face)
    (multi-project-mark-line)))


(defun multi-project-mark-line ()
  "Mark the current line."
  (move-overlay multi-project-overlay (point-at-bol) (point-at-eol)))

(defun multi-project-move-selection (buf movefunc movearg)
  "Move the selection marker to a new position in BUF determined by MOVEFUNC and MOVEARG."
  (unless (= (buffer-size (get-buffer buf)) 0)
    (save-selected-window
      (select-window (get-buffer-window buf))

      (condition-case nil
          (funcall movefunc movearg)
        (beginning-of-buffer (goto-char (point-min)))
        (end-of-buffer (goto-char (point-max))))

      ;; if line end is point-max then it's either an incomplete line or
      ;; the end of the output, so move up a line
      (if (= (point-at-eol) (point-max))
          (forward-line -1))

      (multi-project-mark-line))))

(defun multi-project-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (multi-project-move-selection multi-project-buffer 'next-line -1))
 
(defun multi-project-next-line ()
  "Move selection to the next line."
  (interactive)
  (multi-project-move-selection multi-project-buffer 'next-line 1))

(define-minor-mode multi-project-minor-mode
  "Minor mode for working with multiple projects"
  nil
  " MP"
  multi-project-map)
  
(defun multi-project-quit ()
  "Kill the MP buffer."
  (interactive)
  (quit-window))

(defun multi-project-select ()
  "Select the project from the displayed list."
  (interactive)
  (let ((selectedline (buffer-substring (point-at-bol) (point-at-eol)))
        (solution)
        (project-list))
    (setq solution (split-string selectedline "[\t ]"))
    (setq project-list (multi-project-find-by-name (car solution)))
    project-list))

(defun multi-project-display-select (&optional otherwindow)
  "Select the project and visit the project's tree.
Optional argument OTHERWINDOW if true, the display is created in a secondary window.e."
  (interactive)
  (let ((project-list (multi-project-select)))
    (when project-list
      (multi-project-change-tags (car project-list))
      (multi-project-dired-solution project-list nil otherwindow))))
                      
(defun multi-project-display-select-other-window ()
  "Select the project, but places it in another window."
  (interactive)
  (multi-project-display-select t))

(defun multi-project-check-input()
  "Check for input"
  (let ((input (minibuffer-contents)))
    (if (not (string-equal input multi-project-previous-input))
        (progn
          (multi-project-create-display input)
          (setq multi-project-previous-input input)))))

(defun multi-project-exit-minibuffer()
  "Exit from the minibuffer"
  (interactive)
  (exit-minibuffer))

(defun multi-project-display-search ()
  "Search the list of projects for keywords."
  (interactive)
  (add-hook 'post-command-hook 'multi-project-check-input)

  (unwind-protect
      (let ((minibuffer-local-map multi-project-minibuffer-map))
        (read-string "substring: "))
    (remove-hook 'post-command-hook 'multi-project-check-input))

  (with-current-buffer multi-project-buffer
    (multi-project-display-select)))
  

(defconst multi-project-file-buffer "*mp-find-file*"
  "Buffer used for finding files.")

(defun multi-project-find-files (pattern)
  "Find a list of files based upon a regular expression PATTERN."
  (let ((result nil))
    (save-excursion
      (let ((large-file-warning-threshold nil)
            (tags-add-tables nil))
        (when  (visit-tags-table-buffer)
          (unless tags-table-files (tags-table-files))

          (dolist (file tags-table-files)
            (when (and (string-match pattern (file-name-nondirectory file)) file)
              (setq result (cons file result)))))))
    (sort result (lambda (a b) (string< a b)))))
  
(defvar multi-project-file-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'multi-project-file-next-line)
    (define-key map (kbd "C-n") 'multi-project-file-next-line)
    (define-key map (kbd "<up>") 'multi-project-file-previous-line)
    (define-key map (kbd "C-p") 'multi-project-file-previous-line)
    (define-key map (kbd "<RET>") 'multi-project-exit-minibuffer)
    map)
  "Keymap for multi-project-file mode.")

(defun multi-project-file-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (multi-project-move-selection multi-project-file-buffer 'next-logical-line -1))
 
(defun multi-project-file-next-line ()
  "Move selection to the next line."
  (interactive)
  (save-excursion multi-project-file-buffer
                  (multi-project-move-selection multi-project-file-buffer
                                                'next-logical-line 1)))

(defun multi-project-find-file-display (input)
  "Display the list of files that match INPUT from the minibuffer."
  (interactive)

  (with-current-buffer multi-project-file-buffer
    (let ((result nil))
      (setq result (multi-project-find-files input))
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (item result)
        (insert item "\n"))

      (if (= (point) (point-max))
	  (goto-char (point-min)))

      (setq buffer-read-only t)

      (multi-project-mark-line))))

(defun multi-project-check-file-input()
  "Check for input"
  (if (sit-for 0.2)
      (let ((input (minibuffer-contents)))
        (if (and (not (string-equal input multi-project-previous-file-input))
                 (>= (length input) 1))
            (progn
              (multi-project-find-file-display input)
              (setq multi-project-previous-file-input input))))))

(defun multi-project-file-select ()
  "Select from the list of files presented."
  (with-current-buffer multi-project-file-buffer
    (let ((filename (buffer-substring (point-at-bol) (point-at-eol))))
      (save-excursion
	(visit-tags-table-buffer)
	(find-file filename)))))
  
;;;###autoload
(defun multi-project-find-file ()
  "Search a TAGS file for a particular file that match a user's input."
  (interactive)

  (unless (get-buffer "TAGS")
    (error "Unable to get TAGS for project"))

  (add-hook 'post-command-hook 'multi-project-check-file-input)

  (switch-to-buffer multi-project-file-buffer)
  (setq multi-project-overlay (make-overlay (point-min) (point-min)))
  (overlay-put multi-project-overlay 'face 'multi-project-selection-face)

  (unwind-protect
      (let ((minibuffer-local-map multi-project-file-minibuffer-map))
	(read-string "Filename substring: "))
    (remove-hook 'post-command-hook 'multi-project-check-file-input))

  (with-current-buffer multi-project-file-buffer
    (multi-project-file-select))
  (kill-buffer multi-project-file-buffer))

(global-set-key "\C-xpa" 'multi-project-anchor)
(global-set-key "\C-xpl" 'multi-project-last)
(global-set-key "\C-xpr" 'multi-project-root)
(global-set-key "\C-xpj" 'multi-project-display-projects)
(global-set-key "\C-xpc" 'multi-project-compile)
(global-set-key "\C-xpv" 'multi-project-change-tags)
(global-set-key "\C-xpf" 'multi-project-find-file)

(provide 'multi-project)

;;; multi-project.el ends here
