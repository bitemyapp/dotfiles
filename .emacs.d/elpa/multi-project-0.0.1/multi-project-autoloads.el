;;; multi-project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (multi-project-find-file multi-project-display-projects
;;;;;;  multi-project-display-change-tags multi-project-anchor multi-project-last
;;;;;;  multi-project-change-tags multi-project-root multi-project-compile)
;;;;;;  "multi-project" "multi-project.el" (19840 13782))
;;; Generated autoloads from multi-project.el

(autoload 'multi-project-compile "multi-project" "\
Compiles a project based upon the current directory of the buffer.

\(fn)" t nil)

(autoload 'multi-project-root "multi-project" "\
Jumps to the root of a project based upon current directory.

\(fn)" t nil)

(autoload 'multi-project-change-tags "multi-project" "\
Visits tags file based upon current directory

\(fn &optional PROJECT)" t nil)

(autoload 'multi-project-last "multi-project" "\
Jumps to the last chosen project

\(fn)" t nil)

(autoload 'multi-project-anchor "multi-project" "\
Chooses a project that will be constant no matter the default directory

\(fn)" t nil)

(autoload 'multi-project-display-change-tags "multi-project" "\
Not documented

\(fn)" t nil)

(autoload 'multi-project-display-projects "multi-project" "\
Displays a buffer with the various projects

\(fn)" t nil)

(autoload 'multi-project-find-file "multi-project" "\
Search a TAGS file for a particular file that match a user's input.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("multi-project-pkg.el") (19840 13782 517576))

;;;***

(provide 'multi-project-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multi-project-autoloads.el ends here
