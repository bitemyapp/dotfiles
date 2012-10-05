;;; ensime-config.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.



(defvar ensime-config-file-name ".ensime"
  "The default file name for ensime project configurations.")

(add-to-list 'auto-mode-alist '("\\.ensime$" . emacs-lisp-mode))

(defun ensime-config-fix-path (f root)
  ;; (ensime-config-fix-path "/home/aemon/rabbits.txt" "/home/aemon/")
  ;; (ensime-config-fix-path "~/rabbits.txt" "/home/aemon/dogs")
  (let ((rel (ensime-relativise-path f root)))
    (if (integerp (string-match "^~" rel))
        (expand-file-name rel)
      rel)))

(defun ensime-config-gen (&optional default-root)
  "Interactively generate a new .ensime configuration file."
  (interactive)
  (catch 'done
    (let* ((root (expand-file-name
                  (read-directory-name "Find project root: " default-root)))
           (conf-file (concat root "/" ensime-config-file-name)))

      ;; Check if config already exists for this project...
      (if (file-exists-p conf-file)
          (if (yes-or-no-p (format (concat "Found an existing "
                                           "%s file for this project. "
                                           "Backup existing file and "
                                           "continue? ")
				   ensime-config-file-name))
              (rename-file conf-file (concat conf-file ".bak") t)
            (throw 'done nil)))

      ;; Try to infer the project type...
      (let ((guessed-proj-type (ensime-config-guess-type root)))
        (if (yes-or-no-p
             (format (concat "Your project seems "
                             "to be of type '%s', "
                             "continue with this "
                             "assumption? ") guessed-proj-type))
            (ensime-config-build root guessed-proj-type)
          (let* ((options '("custom" "custom-with-ivy" "maven" "sbt"))
                 (proj-type (completing-read
                             (concat "What type of project is this? ("
                                     (mapconcat #'identity options ", ")
                                     "): ")
                             options)))
            (ensime-config-build root (make-symbol proj-type)))))
      nil
      )))


(defun ensime-config-read-proj-package ()
  (read-string
   "What is the name of your project's main package? e.g. com.myproject: "
   ))

(defun ensime-config-read-project-name ()
  (read-string "What is your project's name? "))

(defun ensime-config-read-source-dirs (root)
  (list (ensime-config-fix-path
         (read-directory-name
          "Where is the project's source located? " root) root)))

(defun ensime-config-read-jar-dir (prompt root)
  (list (ensime-config-fix-path
         (read-directory-name prompt root) root)))

(defun ensime-config-read-target-dir (root)
  (ensime-config-fix-path
   (read-directory-name
    "Where are classes written by the compiler? " root) root))

(defmacro ensime-set-key (conf key val)
  `(setq ,conf (plist-put ,conf ,key ,val)))


(defun ensime-config-build-maven (root)
  (let ((conf '()))

    (ensime-set-key conf :project-name
                    (ensime-config-read-project-name))

    (ensime-set-key conf :project-package
                    (ensime-config-read-proj-package))

    (ensime-set-key conf :use-maven t)

    conf
    ))

(defun ensime-config-build-custom-with-ivy (root)
  (let ((conf '()))

    (ensime-set-key conf :project-name
                    (ensime-config-read-project-name))

    (ensime-set-key conf :project-package
                    (ensime-config-read-proj-package))

    (ensime-set-key conf :use-ivy t)

    (when (yes-or-no-p
           "Does your project use custom ivy configurations? ")
      (ensime-set-key conf :ivy-compile-conf
                      (read-string
                       "What config should be used to compile? (space separated): " "compile"))
      (ensime-set-key conf :ivy-runtime-conf
                      (read-string
                       "What config should be used at runtime? (space separated): " "runtime")))

    (when (yes-or-no-p
           "Is your ivy.xml located somewhere other than the root of your project? ")
      (ensime-set-key conf :ivy-file
                      (ensime-config-fix-path
                       (read-file-name "Locate your ivy.xml file: " root "ivy.xml") root)))

    (ensime-set-key conf :sources
                    (ensime-config-read-source-dirs root))

    (when (yes-or-no-p
           "Is there an unmanaged directory of jars you'd like to include in your dependencies? ")
      (ensime-set-key conf :compile-jars
                      (list (ensime-config-fix-path
                             (read-directory-name
                              "Where are the jars located? " root) root))))

    (when (yes-or-no-p
           "Is the Scala standard library located somewhere else? ")
      (ensime-set-key
       conf :compile-jars
       (append (plist-get conf :compile-jars)
	       (list (ensime-config-fix-path
		      (read-directory-name
		       "Where are is the Scala library located? "
		       root) root)))))

    (ensime-set-key conf :target
                    (ensime-config-read-target-dir root))

    conf
    ))

(defun ensime-config-build-sbt (root)
  (message (concat
	    "Use the sbt command 'ensime generate' to create a .ensime file."
	    "\nThen, run M-x ensime."
	    ))
  nil
  )

(defun ensime-config-build-custom (root)
  (let ((conf '()))

    (ensime-set-key conf :project-name
                    (ensime-config-read-project-name))

    (ensime-set-key conf :project-package
                    (ensime-config-read-proj-package))

    (ensime-set-key conf :sources
                    (ensime-config-read-source-dirs root))

    (ensime-set-key conf :compile-jars
                    (ensime-config-read-jar-dir
                     "Where are the project's dependency jars located? "
                     root))

    (when (yes-or-no-p
           "Is the Scala standard library located somewhere else? ")
      (ensime-set-key
       conf :compile-jars
       (append (plist-get conf :compile-jars)
	       (list (ensime-config-fix-path
		      (read-directory-name
		       (concat
			"Where are is the Scala "
			"library located? " root)) root)))))

    (ensime-set-key conf :target
		    (ensime-config-read-target-dir root))


    conf
    ))

(defun ensime-config-build (root proj-type)
  (let* ((builder-func (intern-soft
			(concat
			 "ensime-config-build-"
			 (symbol-name proj-type))))
	 (conf (funcall builder-func root))
	 (conf-file (concat root "/" ensime-config-file-name)))
    (when conf
      (with-temp-file conf-file
	(ensime-config-insert-config conf))
      (message (concat "Your project config "
		       "has been written to %s. "
		       "Use 'M-x ensime' to launch "
		       "ENSIME.") conf-file)
      )))

(defun ensime-config-insert-config (conf)
  (insert (concat ";; This config was generated using "
		  "ensime-config-gen. Feel free to customize "
		  "its contents manually.\n\n"))
  (insert "(\n\n")
  (let ((c conf))
    (while c
      (let ((a (pop c))
	    (b (pop c)))
	(insert (format "%S" a))
	(insert " ")
	(insert (format "%S" b))
	(insert "\n\n")
	)))
  (insert ")\n"))


(defun ensime-config-guess-type (root)
  "Return a best guess of what type of project is located at root."
  (cond ((ensime-config-is-sbt-test root) 'sbt)
	((ensime-config-is-maven-test root) 'maven)
	((ensime-config-is-ivy-test root) 'custom-with-ivy)
	(t 'custom)))

(defun ensime-config-is-maven-test (root)
  (file-exists-p (concat root "/pom.xml")))

(defun ensime-config-is-ivy-test (root)
  (or
   (file-exists-p (concat root "/ivy.xml"))
   (file-exists-p (concat root "/ivy/ivy.xml"))
   (file-exists-p (concat root "/build/ivy.xml"))))

(defun ensime-config-is-sbt-test (root)
  (or (not (null (directory-files root nil "\\.sbt$")))
      (file-exists-p (concat root "/project/boot" ))
      (file-exists-p (concat root "/project/build.properties" ))))

(defun ensime-config-find-file (file-name)
  "Search up the directory tree starting at file-name
   for a suitable config file to load, return it's path. Return nil if
   no such file found."
  ;;(ensime-config-find-file "~/projects/ensime/")
  ;;(ensime-config-find-file "~/projects/ensime/src/main")
  ;;(ensime-config-find-file "~/projects/ensime/src/main/scala")
  ;;(ensime-config-find-file "~/projects/ensime/src/main/scala/")
  ;;(ensime-config-find-file "~/projects/ensime/.ensime")
  (let* ((dir (file-name-directory file-name))
	 (possible-path (concat dir ensime-config-file-name)))
    (when (and dir (file-directory-p dir))
      (if (file-exists-p possible-path)
	  possible-path
	(if (not (equal dir (directory-file-name dir)))
	    (ensime-config-find-file (directory-file-name dir)))))))

(defun ensime-config-find-and-load (&optional force-dir)
  "Query the user for the path to a config file, then load it."
  (let* ((hint (or force-dir buffer-file-name default-directory))
	 (guess (when hint (ensime-config-find-file hint)))
	 (file (if ensime-prefer-noninteractive guess
		 (read-file-name
		  "ENSIME Project file: "
		  (if guess (file-name-directory guess))
		  guess
		  nil
		  (if guess (file-name-nondirectory guess))
		  ))))

    ;; Should be ok to just give the project directory..
    (let ((file (if (and (file-directory-p file)
			 (file-exists-p (concat file "/"
						ensime-config-file-name)))
		    (concat file "/" ensime-config-file-name)
		  file)))

      (if (or (not (file-exists-p file))
	      (file-directory-p file))

	  ;; If doesn't exist, maybe create one on the spot
	  (if (y-or-n-p (concat
			 "Could not find an ENSIME project file."
			 " Would you like to generate one? "))

	      (ensime-config-gen (file-name-directory file))

	    (progn (message (concat "Please see the ENSIME manual for"
				    " instructions on how to write or"
				    " generate a config file."))
		   nil))

	;; If does exist, load it.
	(ensime-config-load file))

      )))


(defun ensime-config-load (file-name)
  "Load and parse a project config file. Return the resulting plist.
   The :root-dir setting will be deduced from the location of the project file."
  (let ((dir (expand-file-name (file-name-directory file-name))))
    (save-excursion
      (let ((config
	     (let ((buf (find-file-read-only file-name ensime-config-file-name))
		   (src (buffer-substring-no-properties
			 (point-min) (point-max))))
	       (kill-buffer buf)
	       (condition-case error
		   (read src)
		 (error
		  (error "Error reading configuration file, %s: %s" src error)
		  ))
	       )))
	;; We use the project file's location as the project root.
	(ensime-set-key config :root-dir dir)
	(ensime-config-maybe-set-active-subproject config)
	config)
      )))

(defun ensime-config-maybe-set-active-subproject (config)
  "If the subprojects key exists in the config, prompt the
 user for the desired subproject, and add an active-subproject
 value to the config."
  (when-let (sps (plist-get config :subprojects))

    ;; For testing purposes..
    (if (or ensime-prefer-noninteractive
	    (= (length sps) 1))
	(ensime-set-key
	 config :active-subproject
	 (plist-get (car sps) :module-name))

      ;; Otherwise prompt the user
      (let* ((options
	      (mapcar
	       (lambda (sp)
		 (let ((nm (plist-get sp :module-name)))
		   `(,nm . ,nm)))  sps))
	     (keys (mapcar (lambda (opt) (car opt)) options)))
	(let ((key (when keys
		     (completing-read
		      (concat "Which project? ("
			      (mapconcat #'identity keys ", ")
			      "): ")
		      keys nil t (car keys)))))
	  (when-let (chosen (cdr (assoc key options)))
	    (ensime-set-key config :active-subproject chosen)
	    ))
	))))



(provide 'ensime-config)
