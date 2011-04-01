;;; rudel-color.el --- Color manipulation functions for Rudel
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, color, color space
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
;; This file contains utility functions for color manipulation. There
;; are conversion functions:
;;
;; + `rudel-rgb->hsv'    RGB -> HSV color space (for triples)
;; + `rudel-hsv->rgb'    HSV -> RGB color space (for triples)
;;
;; + `rudel-hsv->string' triple -> string for RGB triples
;; + `rudel-hsv->string' triple -> string for HSV triples
;;
;; Note: some function are based on hexrgb.el by Drew Adams, which is
;; available here:
;; http://www.emacswiki.org/emacs/hexrgb.el


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;


;;; RGV <-> HSV conversion
;;
;; Based on hexrgb.el by Drew Adams, which is available here:
;; http://www.emacswiki.org/emacs/hexrgb.el

(defun rudel-rgb->hsv (red green blue)
  "Convert RED, GREEN, BLUE components to HSV (hue, saturation, value).
Each input component is 0.0 to 1.0, inclusive.
Returns a list of HSV components of value 0.0 to 1.0, inclusive.

Note: this function is based on `hexrgb-rgb-to-hsv' from
hexrgb.el by Drew Adams. It is available here:
http://www.emacswiki.org/emacs/hexrgb.el"
  (let* ((min        (min red green blue))
         (max        (max red green blue))
         (value      max)
         (delta      (- max min))
         (hue        0.0)
	 (saturation 0.0))
    (when (/= 0.0 delta) ;; TODO should be approx=
      (setq saturation (/ delta max))
      (when (/= 0.0 saturation) ;; TODO should be approx=
	;; Color
	(setq hue
	      (cond
	       ;; Between yellow & magenta.
	       ((= red max) ;; TODO should be approx=
		(/ (- green blue) delta))
	       ;; Between cyan & yellow.
	       ((= green max) ;; TODO should be approx=
		(+ 2.0 (/ (- blue red) delta)))
	       ;; Between magenta & cyan.
	       (t
		(+ 4.0 (/ (- red green) delta)))))
	(setq hue (/ hue 6.0))
	(when (<= hue 0.0)
	  (incf hue))))

    (list hue saturation value))
  )

(defun rudel-hsv->rgb (hue saturation value)
  "Convert HUE, SATURATION, VALUE components to RGB (red, green, blue).
Each input component is 0.0 to 1.0, inclusive.
Returns a list of RGB components of value 0.0 to 1.0, inclusive.

Note: this function is based on `hexrgb-hsv-to-rgb' from
hexrgb.el by Drew Adams. It is available here:
http://www.emacswiki.org/emacs/hexrgb.el"
  (if (= 0.0 saturation) ;; TODO should be approx=
      ;; Gray
      (list value value value)
    ;; Color
    (let* ((hue     (* hue 6.0))
	   (int-hue (floor hue))
	   (fract   (- hue int-hue))
	   (pp      (* value (- 1 saturation)))
	   (qq      (* value (- 1 (* saturation fract))))
	   (ww      (* value (- 1 (* saturation (- 1 (- hue int-hue)))))))
      (case int-hue
	((0 6)
	 (list value ww pp))
	(1
	 (list qq value pp))
	(2
	 (list pp value ww))
	(3
	 (list pp qq value))
	(4
	 (list ww pp value))
	(otherwise
	 (list value pp qq)))))
  )

(defun rudel-rgb->string (red green blue)
  "Convert (RED GREEN BLUE) to string representation."
  (format "#%04x%04x%04x"
	  (* 65535 red)
	  (* 65535 green)
	  (* 65535 blue)))

(defun rudel-hsv->string (hue saturation value)
  "Convert (HUE SATURATION VALUE) to string representation."
  (apply #'rudel-rgb->string (rudel-hsv->rgb hue saturation value)))


;;; Utility functions
;;

(defun rudel-color-background-value ()
  "Return a HSV value component suitable for the current background."
  (let* ((background (face-background 'default))
	 (white-ish  (< (color-distance "white" background)
			(color-distance "black" background))))
    (if white-ish 0.9 0.3)))

(provide 'rudel-color)
;;; rudel-color.el ends here
