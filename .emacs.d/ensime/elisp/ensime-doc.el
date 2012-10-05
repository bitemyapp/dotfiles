;;; ensime-doc.el
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


(defcustom ensime-scaladoc-stdlib-url-base
  "http://www.scala-lang.org/api/current/"
  "URL base for constructing scaladoc stdlib links."
  :type 'string
  :group 'ensime)

(defcustom ensime-scaladoc-compiler-url-base
  "http://www.scala-lang.org/archives/downloads/distrib/files/nightly/docs/compiler/"
  "URL for constructing scaladoc compiler links."
  :type 'string
  :group 'ensime)

(defcustom ensime-javadoc-stdlib-url-base
  "http://java.sun.com/javase/6/docs/api/"
  "URL for constructing javadoc links."
  :type 'string
  :group 'ensime)

(defvar ensime-doc-lookup-map
  '(
    ("^java\\." . ensime-make-java-doc-url)
    ("^scala\\.tools\\.nsc\\." . ensime-make-scala-compiler-doc-url)
    ("^scala\\." . ensime-make-scala-doc-url)
    ("^android\\." . ensime-make-android-doc-url)
    )
  "A map from regular expression strings to functions.
When ENSIME requests the documentation for a type or member,
the qualified name of the type will be matched against each
regex in this map. The corresponding function will be applied
to the type and or member objects and will return a url string.")

(defun ensime-make-doc-url (type &optional member)
  "Given a type and an optional member object, yields an http url for
browsing the documentation for those objects."
  (catch 'return
    (dolist (ea ensime-doc-lookup-map)
      (let ((re (car ea))
            (func (cdr ea)))
        (when (integerp (string-match re (ensime-type-full-name type)))
          (throw 'return (funcall func type member)))))))

(defun ensime-make-doc-url-for-sym (sym)
  "Given a symbol, yield a http url for
browsing the documentation for that symbol."
  (let* ((owner-type-id (ensime-symbol-owner-type-id sym))
	 (is-member (and (ensime-symbol-is-callable sym)
			 (integerp owner-type-id)))
	 (tpe
	  (if is-member (ensime-rpc-get-type-by-id
			 owner-type-id)
	    (ensime-symbol-type sym))))
    (ensime-make-doc-url tpe (when is-member sym))))


(defun ensime-show-doc-for-symbol-at-point ()
  "Browse to documentation for the symbol at current point."
  (interactive)
  (let* ((sym (ensime-rpc-symbol-at-point))
	 (url (ensime-make-doc-url-for-sym sym)))
    (if url
	(progn
	  (message "Looking up doc for %s..."
		   (ensime-symbol-name sym))
	  (browse-url url))
      (message "No documentation found."))))



(defun ensime-make-android-doc-url (type &optional member)
  "Given a scala type, and optionally a type member, construct the
   corresponding scaladoc url."
  (ensime-make-java-doc-url-helper
   "http://developer.android.com/reference/" type member))

(defun ensime-make-scala-doc-url (type &optional member)
  (ensime-make-scala-doc-url-helper
   ensime-scaladoc-stdlib-url-base type member))

(defun ensime-make-scala-compiler-doc-url (type &optional member)
  (ensime-make-scala-doc-url-helper
   ensime-scaladoc-compiler-url-base type member))

(defun ensime-make-scala-doc-url-helper
  (url-base type &optional member)
  "Given a scala type, and optionally a type member, construct the
   corresponding scaladoc url."
  (let* ((full-type-name (ensime-type-full-name type)))
    (let* ((s (replace-regexp-in-string "\\." "/" full-type-name)))
      (concat url-base
              s
              ".html"
              (if member
                  (let* ((name (ensime-member-name member)))
                    (concat "#" full-type-name "#" name))))
      )))


(defvar ensime-javadoc-type-replacements
  '(
    ("^scala.Any$" . "java.lang.Object")
    ("^scala.Int$" . "int")
    ("^scala.Double$" . "double")
    ("^scala.Short$" . "short")
    ("^scala.Byte$" . "byte")
    ("^scala.Long$" . "long")
    ("^scala.Float$" . "float")
    ("^scala.Boolean$" . "boolean")
    ("^scala.Char$" . "char")
    ("^scala.Unit$" . "void"))
  "When creating javadoc urls,
   use this mapping to replace scala types with java types.")

(defun ensime-javadoc-replace-types (str)
  "Replace scala primitive type names with jave primitive names."
  (dolist (rep ensime-javadoc-type-replacements)
    (setq str (replace-regexp-in-string
               (car rep) (cdr rep) str)))
  str)

(defun ensime-javadoc-type-name (type)
  "Return a the java-friendly name for this type."
  (let ((type-args (ensime-type-type-args type)))
    (if (and (equal (ensime-type-name type) "Array")
             type-args)
        (let ((element-tpe (car type-args)))
          (concat (ensime-javadoc-replace-types
                   (ensime-type-full-name element-tpe)) "[]"))
      (ensime-javadoc-replace-types (ensime-type-full-name type)))))

(defun ensime-make-java-doc-url (type &optional member)
  (ensime-make-java-doc-url-helper ensime-javadoc-stdlib-url-base type member))

(defun ensime-make-java-doc-url-helper (base-url type &optional member)
  "Given a java type, and optionally a java type member, construct the
   corresponding javadoc url."
  (let* ((full-type-name (ensime-type-full-name type))
         (without-dollar (replace-regexp-in-string "\\$" "" full-type-name))
         (with-slashes (replace-regexp-in-string "\\." "/" without-dollar)))
    (concat base-url
            with-slashes
            ".html"
            (if member
                (let* ((memb-name (ensime-member-name member))

                       ;; If member is a constructor...
                       (name (if (equal memb-name "this")
                                 (ensime-type-name type) memb-name))

                       (type (ensime-member-type member))
                       (param-types (ensime-type-param-types type)))
                  (concat
                   "#" name
                   "("
                   (mapconcat
                    (lambda (tpe)
                      (ensime-javadoc-type-name tpe)) param-types ", ")
                   ")"))))
    ))


(provide 'ensime-doc)