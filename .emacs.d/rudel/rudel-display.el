;;; rudel-display.el --- Display functions for Rudel objects
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, display, icons, text, representation
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
;; This file contains methods for `rudel-user' and `rudel-document'
;; classes of the generic function `rudel-display-string'. Specialized
;; methods can be written for derived classes when appropriate.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel) ;; make sure `rudel-user' and `rudel-document' are
		 ;; defined
(require 'rudel-icons)


;;; Display functions for basic objects
;;

(defmethod rudel-display-string ((this rudel-user)
				 &optional use-images align)
  "Return a textual representation of THIS for user interface purposes.
When USE-IMAGES is non-nil, add an icon that indicates a user to
the text representation.
When ALIGN is non-nil, align the text representation. If ALIGN is
t, align it to a fixed width. When ALIGN is a number, align it to
a width equal to that number."
  (with-slots ((name :object-name) color) this
    (propertize
     (concat
      (when use-images
	(propertize "*" 'display rudel-icon-person))
      name)
     'face (list :background color)))
  )

(defmethod rudel-display-string ((this rudel-document)
				 &optional use-images align)
  "Return a textual representation of THIS for user interface purposes.
When USE-IMAGES is non-nil, add an icon that indicates a document
to the text representation.
When ALIGN is non-nil, align the text representation. If ALIGN is
t, align it to a fixed width. When ALIGN is a number, align it to
a width equal to that number."
  (with-slots ((name :object-name)) this
    (concat
     (when use-images
       (propertize "*" 'display rudel-icon-document))
     name))
  )


;;; Display functions for composite structures
;;

(defun rudel-display-object-list-string (objects
					 &optional separator use-images align)
  "Return a textual representation of USER-LIST for user interface purposes.
String representations of users are separated by the string
SEPARATOR or \" \" when SEPARATOR is nil.
USE-IMAGES and ALIGN are passed to the `rudel-display-string'
method for the user class."
  (mapconcat
   (lambda (object)
     (rudel-display-string object use-images align))
   objects
   (or separator " "))
  )

(provide 'rudel-display)
;;; rudel-display.el ends here
