;;; rudel-backend.el --- A generic backend management mechanism for Rudel
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, backend, factory
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
;; This file contains a generic mechanism that handles registration,
;; query and instantiation of Rudel backends for any number of
;; functional categories.
;;
;; The class and collaboration design is as follows: for each
;; category, which is identified by a symbol, there is a factory
;; object (an instance of `rudel-backend-factory') that is responsible
;; for creating backend objects of the category.  Examples of
;; categories are 'transport', 'protocol' and 'session-initiation'.
;; In addition to creating backend object, factories also allow
;; querying backends based on desired capabilities and load backend
;; implementations only when required.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'warnings)

(require 'eieio)


;;; Class rudel-backend
;;

(defclass rudel-backend ()
  ((version      :initarg  :version
		 :type     list
		 :documentation
		 "A list of the form (MAJOR MINOR [MICRO
WHATEVER*]) describing the version of the backend.")
   (capabilities :initarg  :capabilities
		 :type     list
		 :initform nil
		 :documentation
		 "A list of symbols, or lists whose car is a
symbol, that each describe one capability of the backend."))
  "Base class for backend classes."
  :abstract t)

(defmethod rudel-capable-of-p ((this rudel-backend) capability)
  "Return t if the backend THIS is capable of CAPABILITY."
  (with-slots (capabilities) this
    (member capability capabilities)))


;;; Class rudel-backend-factory
;;

;;;###autoload
(defclass rudel-backend-factory ()
  ((backends  :initarg   :backends
	      :type       hash-table
	      :documentation
	      "Mapping of symbolic names to classes (prior to
instantiation) or objects (after instantiation) for all backends
known to the factory object.")
   (factories :type       hash-table
	      :allocation :class
	      :documentation
	      "Mapping of backend categories to responsible
factory objects."))
  "Factory class that holds an object for each known backend
category. Objects manage backend implementation for one backend
category each.")
(oset-default rudel-backend-factory factories
	      (make-hash-table :test #'eq))

(defmethod initialize-instance ((this rudel-backend-factory) &rest slots)
  "Initialize slots of THIS with SLOTS."
  (when (next-method-p)
    (call-next-method))
  (oset this :backends (make-hash-table :test #'eq)))

;;;###autoload
(defmethod rudel-get-factory :static ((this rudel-backend-factory)
				      category)
  "Return the factory responsible for CATEGORY.
If there is no responsible factory, create one and return it."
  (with-slots (factories) this
    (or (gethash category factories)
	(puthash category (rudel-backend-factory category) factories)))
  )

;;;###autoload
(defmethod rudel-add-backend ((this rudel-backend-factory)
			      name class &optional replace)
  "Add factory class CLASS with name NAME to THIS.
if REPLACE is non-nil, replace a registered implementation of the
same name."
  (with-slots (backends) this
    (when (or (not (gethash name backends))
	      replace)
      (puthash name class backends))))

(defmethod rudel-get-backend ((this rudel-backend-factory) name)
  "Return backend object for name NAME or nil if there is none.
The returned backend is of the form (NAME . OBJECT).

Backends are loaded, if necessary."
  ;; Load all available backends
  (rudel-load-backends this)

  ;; Find the backend and return it.
  (with-slots (backends) this
    (let ((backend (gethash name backends)))
      (when backend
	(cons name backend))))
  )

(defmethod rudel-all-backends ((this rudel-backend-factory)
			       &optional only-loaded)
  "Return a list of all backends registered with THIS.
Each list element is of the form (NAME . CLASS-OR-OBJECT).
If optional argument ONLY-LOADED is non-nil, return only elements
for which CLASS-OR-OBJECT is an object."
  (let ((backend-list))
    (with-slots (backends) this
      (maphash (lambda (name class)
		 (when (or (not only-loaded)
			   (object-p class))
		   (push (cons name class) backend-list)))
	       backends))
    backend-list)
  )

(defmethod rudel-suitable-backends ((this rudel-backend-factory) predicate)
  "Return a list of backends which satisfy PREDICATE.
Each list element is of the form (NAME . OBJECT).
Backends are loaded, if necessary."
  ;; Load all available backends.
  (rudel-load-backends this)

  ;; Retrieve and return all backends, filtering the list using
  ;; PREDICATE. Backends that could not be loaded, are ignored.
  (if predicate
      (remove-if-not
       (lambda (cell)
	 (funcall predicate (cdr cell)))
       (rudel-all-backends this t))
    (rudel-all-backends this t))
  )

(defmethod rudel-load-backends ((this rudel-backend-factory))
  "Load backends in THIS factory if necessary.
Loading errors are not reported explicitly, but can be detected
by checking for backends that still are classes rather than
objects."
  ;; Map lambda that loads unloaded backends over all backends. Store
  ;; objects back after loading.
  (with-slots (backends) this
    (maphash
     (lambda (name class)
       (unless (object-p class)
	 (condition-case error
	     (puthash name (make-instance
			    class (symbol-name name)) backends)
	   (error
	    ;; Store this error on the name symbol of the backend for
	    ;; later display.
	    (put name 'rudel-backend-last-load-error
		 (error-message-string error))

	    ;; Emit a warning.
	    (display-warning
	     '(rudel backend)
	     (format "Could not load backend `%s': %s"
		     name
		     (error-message-string error))
	     :warning)))))
	 backends))
  )


;;; High-level frontend functions
;;

(defsubst rudel-backend-cons-p (cell)
  "Check whether CELL is a cons of a backend name and object."
  (and (consp cell)
       (symbolp (car cell))
       (object-p (cdr cell))))

;;;###autoload
(defun rudel-backend-get (category name)
  "A shortcut for getting backend NAME of category CATEGORY.
The returned backend is of the form (NAME . OBJECT)."
  (rudel-get-backend (rudel-backend-get-factory category) name))

;;;###autoload
(defun rudel-backend-get-factory (category)
  "A shortcut for getting the factory object for CATEGORY."
  (rudel-get-factory rudel-backend-factory category))

(defun rudel-backend-suitable-backends (category predicate)
  "Return backends from category CATEGORY that satisfy PREDICATE.
Each list element is of the form (NAME . OBJECT)."
  (rudel-suitable-backends
   (rudel-backend-get-factory category)
   predicate))

(defun rudel-backend-choose (category &optional predicate)
  "Choose a backend from CATEGORY satisfying PREDICATE automatically or by asking the user.
The returned backend is of the form (NAME . CLASS-OR-OBJECT)."
  (let ((backends (rudel-backend-suitable-backends
		   category predicate)))
    (unless backends
      (error "No backends available"))

    (if (= (length backends) 1)
	;; If there is only one backend, we can choose that one right
	;; away displaying a message to avoid confusing the user.
	(let ((backend (nth 0 backends)))
	  (message "Using backend `%s'" (symbol-name (car backend)))
	  (sit-for 0.5)
	  backend)

      ;; When we have more than one backend, we have to ask the user,
      ;; which one she wants.
      (require 'rudel-interactive)
      (rudel-read-backend
       backends
       (format "%s backend: " (capitalize (symbol-name category)) )
       'object)))
  )


;;; User interaction functions
;;

(defun rudel-backend-dump (&optional load)
  "Create display information about backends in a buffer.
If LOAD is non-nil, load all backends before display. This makes
available information available for the backends"
  (interactive "p")
  ;; Setup a new buffer.
  (set-buffer (get-buffer-create "*Rudel Backends*"))
  (erase-buffer)
  (set-window-buffer nil (current-buffer))
  (maphash
   (lambda (category factory)
     ;; Load backends if requested.
     (unless (zerop load)
       (rudel-load-backends factory))

     ;; Insert header for this category.
     (insert (propertize
	      (format "Category %s\n" category)
	      'face 'bold))
     (insert (apply #'format
		    "  %-20s %-6s %-7s %s\n"
		    (mapcar
		     (lambda (header)
		       (propertize header 'face 'italic))
		     '("name" "loaded" "version" "capabilities"))))

     ;; Insert all backends provided by this factory.
     (dolist (backend (rudel-all-backends factory))
       (if (or (object-p (cdr backend))
	       (null (get (car backend)
			  'rudel-backend-last-load-error)))
	   (insert (rudel-backend--format-backend-normal backend))
	 (insert (rudel-backend--format-backend-error backend))))

     ;; One empty line between backend categories.
     (insert "\n"))
   (oref rudel-backend-factory factories))
  (current-buffer))

(defun rudel-backend--format-backend-normal (backend)
  "Format name, version and other properties of BACKEND."
  (format "  %-20s %-6s %-7s (%s)\n"
	  ;; Backend name
	  (propertize
	   (symbol-name (car backend))
	   'face 'font-lock-type-face)
	  ;; Backend loading status
	  (propertize
	   (prin1-to-string (object-p (cdr backend)))
	   'face 'font-lock-variable-name-face)
	  ;; Backend version
	  (propertize
	   (if (object-p (cdr backend))
	       (mapconcat #'prin1-to-string
			  (oref (cdr backend) :version)
			  ".")
	     "?")
	   'face 'font-lock-constant-face)
	  ;; Backend capabilities
	  (propertize
	   (if (object-p (cdr backend))
	       (mapconcat #'prin1-to-string
			  (oref (cdr backend) :capabilities)
			  " ")
	     "?")
	   'face 'font-lock-constant-face))
  )

(defun rudel-backend--format-backend-error (backend)
  "Format name and loading error message of BACKEND."
  (format "  %-20s %s %s\n"
	  ;; Backend name
	  (propertize
	   (symbol-name (car backend))
	   'face 'font-lock-type-face)
	  ;; Loading error message
	  (propertize
	   "error:"
	   'face 'font-lock-warning-face)
	  (propertize
	   (get (car backend) 'rudel-backend-last-load-error)
	   'face 'font-lock-comment-face))
  )

(provide 'rudel-backend)
;;; rudel-backend.el ends here
