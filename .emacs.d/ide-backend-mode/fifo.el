;;; fifo.el --- FIFO queue.

;; Copyright (c) 2015 Chris Done.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

(defun fifo-make ()
  "Make a fifo queue."
  (cons 0 nil))

(defun fifo-push (q a)
  "Push a new item onto the queue."
  (cl-assert (consp q) nil "Must be a queue.")
  (setcar q (1+ (car q)))
  (let ((next q)
        (continue t))
    (while (not (null (cdr next)))
      (setq next (cdr next)))
    (setcdr next (cons a nil)))
  q)

(defun fifo-pop (q)
  "Pop the next item on the queue."
  (cl-assert (consp q) nil "Must be a queue.")
  (cl-assert (consp (cdr q)) nil "No items to pop from queue.")
  (setcar q (1- (car q)))
  (let ((a (car (cdr q))))
    (setcdr q (cdr (cdr q)))
    a))

(defun fifo-size (q)
  "Get the size of the queue."
  (cl-assert (consp q) nil "Must be a queue.")
  (car q))

(defun fifo-null-p (q)
  "Is the queue empty?"
  (= (fifo-size q) 0))

(provide 'fifo)
