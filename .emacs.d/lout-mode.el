;;; lout-mode.el --- Major mode for the Lout typesetting system

;; Copyright (C) 2012 Chris Allen
;; Author: Chris Allen <cma@bitemyapp.com>
;; URL: http://github.com/bitemyapp/lout-mode
;; Version: 0.01
;; Keywords: typesetting

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the Lout
;; typesetting system. (http://savannah.nongnu.org/projects/lout/)

;;; Installation:

;; (require 'lout-mode)

(defgroup lout-mode nil
  "A mode for Lout"
  :prefix "lout-mode-"
  :group 'applications)

(defun lout-mode-version ()
  "Currently package.el doesn't support prerelease version numbers."
  "0.01")
