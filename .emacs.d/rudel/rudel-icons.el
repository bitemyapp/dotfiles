;;; rudel-icons.el --- Icons used by Rudel
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, icons
;; X-RCS: $Id:$
;;
;; This file is part of Rudel.
;;
;; Rudel is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Rudel is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This file loads all icons used in Rudel.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'image)


;;; Image constants
;;

(defconst rudel-icons-image-formats '(svg png)
  "Image formats to try (in that order) when loading Rudel icons.")

(defconst rudel-icons-directory
  (file-name-as-directory
   (concat (file-name-directory load-file-name)  "icons"))
  "Directory that holds Rudel icon files.")


;;; Helper macro
;;

(defmacro rudel-defimage (name &optional docstring)
  "Load image from Rudel icon directory and define image named NAME.
Optional argument DOCSTRING is the documentation string to
associate with the image."
  (declare (doc-string 2))
  (let ((icon  (intern (format "rudel-icon-%s" name)))
	(specs (mapcar
		(lambda (type)
		  `(:type   ,type
		    :ascent center
		    :mask   heuristic
		    :file   ,(concat rudel-icons-directory
				     name "." (symbol-name type))))
		rudel-icons-image-formats)))
    `(defimage ,icon
       (,@specs)
       ,(or docstring
	    (format "%s icon." (capitalize name)))))
  )


;;; Image definitions
;;

(rudel-defimage "person")
(rudel-defimage "document")
(rudel-defimage "connected")
(rudel-defimage "disconnected")
(rudel-defimage "plaintext")
(rudel-defimage "encrypted")

(provide 'rudel-icons)
;;; rudel-icons.el ends here
