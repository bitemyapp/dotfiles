;;; rudel-infinote-text-document.el --- Infinote text document class
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, document, text
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
;; This file contains the `rudel-infinote-text-document' class which
;; implements text documents in the infinote backend. (See
;; rudel-infinote-node.el for an overview)


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-infinote-document)


;;; Class rudel-infinote-text-document
;;

(defclass rudel-infinote-text-document (rudel-infinote-document)
  ()
  "Objects of this class implement infinote text documents.")

(provide 'rudel-infinote-text-document)
;;; rudel-infinote-text-document.el ends here
