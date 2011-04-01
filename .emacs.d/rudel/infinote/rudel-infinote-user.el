;;; rudel-infinote-user.el --- Infinote user class
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, user
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


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel) ;; TODO organize this

(require 'rudel-util) ;; for `rudel-impersonator' and `rudel-delegator'


;;; Class `rudel-infinote-user'
;;

(defclass rudel-infinote-user (rudel-user)
  ()
  "Objects of this class represent participants of infinote
sessions.")


;;; Class `rudel-infinote-document-user'
;;

(defclass rudel-infinote-document-user (rudel-user
					rudel-impersonator
					rudel-delegator)
  ((session-user              :initarg  :session-user
			      :type     rudel-infinote-user-child
			      :documentation
			      "")
   (impersonation-target-slot :initform session-user)
   (delegation-target-slot    :initform session-user)
   (id                        :initarg  :id
			      :type     integer
			      :reader   rudel-id
			      :writer   rudel-set-id
			      :documentation
			      "A number identifying the user
inside the session.")
   (status                    :initarg  :status
			      :type     symbol
			      :reader   rudel-status
			      :writer   rudel-set-status
			      :documentation
			      "Status of the user. Some
well-known values are:
'active: The user did something recently.
'inactive: The user did not do something for some time.
'unavailable: The host that joined the user to the session
unsubscribed from the session."))
  "Objects of this class are used to represent users subscribed
to infinote documents."
  :method-invocation-order :c3)

(provide 'rudel-infinote-user)
;;; rudel-infinote-user.el ends here
