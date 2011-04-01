;;; rudel-compat.el --- Compatibility code for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;; Copyright (C) 2009 Phil Hagelberg
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;;         Phil Hagelberg <phil@enigma>
;; Keywords: rudel, compatibility
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
;; This file contains compatibility code required to make Rudel work
;; with different versions of Emacs.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;


;;; `read-color' replacement
;;

(unless (fboundp 'read-color)
  (defun read-color (prompt &rest ignored)
    "Poor man's read color without completion.
You have to take care to only enter valid color names."
    (read-string prompt)))


;;; `string-match-p' replacement
;;

(unless (functionp 'string-match-p)
  (defsubst string-match-p (regexp string &optional start)
    "Same as `string-match' except this function does not change the match data"
    (let ((inhibit-changing-match-data t))
      (string-match regexp string start))))


;;; `coding-system-from-name' replacement
;;

(defun rudel-get-coding-system (name)
  (if (functionp 'coding-system-from-name)
      (coding-system-from-name name)
    ;; May need to try a little harder here for Emacs 22 depending on
    ;; what kind of encoding names are given us.
    (intern name)))


;;; Spinner Progress Reporter
;;

(defvar progress-spinner-values ["-" "\\" "|" "/"])

(defun progress-reporter-spinner-p (reporter)
  "Return t if REPORTER has an unknown max value."
  (null (aref (cdr reporter) 2)))

(defun progress-reporter-force-update (reporter &optional value new-message)
  "Report progress of an operation in the echo area unconditionally.

First two parameters are the same as for
`progress-reporter-update'.  Optional NEW-MESSAGE allows you to
change the displayed message."
  (let ((parameters (cdr reporter)))
    (when new-message
      (aset parameters 3 new-message))
    (when (aref parameters 0)
      (aset parameters 0 (float-time)))
    (if (progress-reporter-spinner-p reporter)
	(progress-reporter-spin reporter)
      (progress-reporter-do-update reporter value))))

(defsubst progress-reporter-update (reporter &optional value)
  "Report progress of an operation in the echo area.

The first parameter, REPORTER, should be the result of a call to
`make-progress-reporter'. For reporters for which the max value
is known, the second argument determines the actual progress of
operation; it must be between MIN-VALUE and MAX-VALUE as passed
to `make-progress-reporter'.

However, if the change since last echo area update is too small
or not enough time has passed, then do nothing (see
`make-progress-reporter' for details).

In this case, this function is very inexpensive, you need not
care how often you call it."
  (if (progress-reporter-spinner-p reporter)
      (progress-reporter-spin reporter)
    (when (>= value (car reporter))
      (progress-reporter-do-update reporter value))))

(defun progress-reporter-spin (reporter)
  "Advance indicator of spinning REPORTER."
  (let* ((parameters (cdr reporter))
	 (index      (+ (aref parameters 1) 1)))
    (aset parameters 1 index)
    (let ((message-log-max nil)) ; No logging
      (message "%s %s"
	       (aref progress-spinner-values (mod index 4))
	       (aref parameters 3)))))

(defun make-progress-reporter (message &optional min-value max-value
				       current-value min-change min-time)
  "Return progress reporter object to be used with `progress-reporter-update'.

MESSAGE is shown in the echo area.  When at least 1% of operation
is complete, the exact percentage will be appended to the
MESSAGE.  When you call `progress-reporter-done', word \"done\"
is printed after the MESSAGE.  You can change MESSAGE of an
existing progress reporter with `progress-reporter-force-update'.

If provided, MIN-VALUE and MAX-VALUE designate starting (0%
complete) and final (100% complete) states of operation.  The
latter should be larger; if this is not the case, then simply
negate all values.  Optional CURRENT-VALUE specifies the progress
by the moment you call this function.  You should omit it or set
it to nil in most cases since it defaults to MIN-VALUE.

Optional MIN-CHANGE determines the minimal change in percents to
report (default is 1%.)  Optional MIN-TIME specifies the minimal
time before echo area updates (default is 0.2 seconds.)  If
`float-time' function is not present, then time is not tracked at
all.  If OS is not capable of measuring fractions of seconds,
then this parameter is effectively rounded up.

If MIN-VALUE and MAX-VALUE are unknown, they may be omitted to
return a \"pulsing\" progress reporter."
  (unless min-time
    (setq min-time 0.2))
  (let ((reporter
	 (cons min-value ;; Force a call to `message' now
	       (vector (if (and (fboundp 'float-time)
				(>= min-time 0.02))
			   (float-time) nil)
		       (or min-value 0)
		       max-value
		       message
		       (if min-change (max (min min-change 50) 1) 1)
		       min-time))))
    (progress-reporter-update reporter (or current-value min-value))
    reporter))

(provide 'rudel-compat)
;;; rudel-compat.el ends here
