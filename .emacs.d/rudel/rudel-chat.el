;;; rudel-chat.el --- Handling of chat messages
;;
;; Copyright (C) 2008, 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, chat, message
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
;; This file contains some functions that deal with incoming chat
;; messages. Backends that support receiving chat message should
;; dispatch them using `rudel-chat-dispatch-message'. Chat messages
;; will be processed in a customizable way from there.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;


;;; Customization
;;

(defcustom rudel-chat-handler-function #'rudel-chat-handle-buffer-top
  "A function that is called when chat messages arrive."
  :group 'rudel
  :type  '(choice (const :tag "Display messages in the echo area"
			 rudel-chat-handle-message)
		  (const :tag "Log messages into a buffer, \
inserting at the top"
			 rudel-chat-handle-buffer-top)
		  (const :tag "Log messages into a buffer, \
inserting at the bottom"
			 rudel-chat-handle-buffer-bottom)
		  (function :tag "Other function"))
  )


;;; Variables and constants
;;

(defconst rudel-chat-buffer-name "*rudel-chat-log*"
  "Name of the buffer into which received chat message should be
inserted.")


;;; Interface functions
;;

(defun rudel-chat-dispatch-message (sender message)
  "Dispatch SENDER and MESSAGE to customizable handler function."
  (funcall rudel-chat-handler-function sender message))


;;; Handler functions
;;

(defun rudel-chat-handle-message (sender text)
  "Display SENDER and MESSAGE in the echo area."
  (message "%s says: %s"
	   (rudel-chat-format-sender sender)
	   text))

(defun rudel-chat-handle-buffer-top (sender text)
  "Insert SENDER and MESSAGE at the beginning a buffer."
  (let ((buffer (or (get-buffer rudel-chat-buffer-name)
		    (pop-to-buffer rudel-chat-buffer-name))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (insert (format "%s: %s\n"
		      (rudel-chat-format-sender sender)
		      text))))
  )

(defun rudel-chat-handle-buffer-bottom (sender text)
  "Insert SENDER and MESSAGE at the end of a buffer."
  (let ((buffer (or (get-buffer rudel-chat-buffer-name)
		    (pop-to-buffer rudel-chat-buffer-name))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "\n%s: %s"
		      (rudel-chat-format-sender sender)
		      text))))
  )


;;; Miscellaneous functions
;;

(defun rudel-chat-format-sender (user)
  "Format USER handling nil values."
  (if user
      (object-name-string user)
    "<unknown sender>"))

(provide 'rudel-chat)
;;; rudel-chat.el ends here
