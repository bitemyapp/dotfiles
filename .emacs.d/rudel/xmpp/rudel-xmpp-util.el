;;; rudel-xmpp-util.el --- Miscellaneous functions for the Rudel XMPP backend
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, backend, miscellaneous
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
;; This file contains the assemble function
;; `rudel-xmpp-assemble-stream' which is used by
;; `rudel-xmpp-make-transport-filter-stack' to parametrize assembling
;; and parsing transport filters to handle XML data transparently.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-xml)

(require 'rudel-transport-util)


;;; Fragmentation and assembling functions.
;;

(defun rudel-xmpp-assemble-stream (data storage)
  "Extract complete XML stanzas from DATA and STORAGE.
Return a list the first element of which is a list of strings
that contain the serialized forms of the stanzas. The second
element is a string containing the rest of the data or nil DATA
does not contains any incomplete stanzas."
  ;; TODO mention limitations

  ;; Form a string by concatenating STORAGE and DATA. Form the stream
  ;; header, find the end of stream:features.
  (let* ((string (concat storage data))
	 (end    (or (search "</stream:features>" string)
		     (search "<stream:features/>" string)))
	 (end    (when end
		   (+ end 18))))
    ;; If the end of stream:features has been found, artificially
    ;; close stream:stream, then find top-level tags.
    (if (not end)
	;; No end, put everything into storage.
	(list nil string)
      ;; Otherwise find top-level tags. This can still leave
      ;; incomplete tags.
      (destructuring-bind (tags buffer)
	  (rudel-xml-toplevel-tags
	   (concat (substring string 0 end)
		   "</stream:stream>"
		   (replace-regexp-in-string
		    "</stream:stream>"
		    ""
		    (substring string end))))
	(list
	 ;; Remove processing instructions.
	 (remove-if
	  (lambda (tag)
	    (= (aref tag 1) ??))
	  tags)
	 buffer)))) ;; TODO wrong
  )

;; One problem: peer can send
;; <stream:stream xmlns:stream="http://etherx.jabber.org/streams"
;;                xmlns="jabber:client"
;;                version="1.0"
;;                from="gunhead">
;; and then
;; <stream:features/>
;; we cannot assemble this properly; look into the RFC


;;; Transport filter stack construction
;;

(defun rudel-xmpp-make-transport-filter-stack (transport)
  "Construct an XMPP protocol filter stack on top of TRANSPORT."
  (rudel-transport-make-filter-stack
   transport
   '((rudel-assembling-transport-filter
      :assembly-function rudel-xmpp-assemble-stream)
     (rudel-parsing-transport-filter
      :parse-function    string->xml
      :generate-function xml->string))))

(provide 'rudel-xmpp-util)
;;; rudel-xmpp-util.el ends here
