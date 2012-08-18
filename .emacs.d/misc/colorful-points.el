(defvar colorful-points nil
  "Alist mapping window -> overlay to color its point.")
(make-variable-buffer-local 'colorful-points)

(defvar colorful-points-colors '("blue" "purple" "green" "orange" "yellow" "red")
  "Colors that the points can be.  Chosen in order.")

(define-minor-mode colorful-points-mode "Give each point in buffer a unique color." nil
  " Colorful" nil
  (if colorful-points-mode
      (progn ;; turning on
        (make-variable-buffer-local 'after-change-functions)
        (setq after-change-functions (cons #'colorful-points-after-change after-change-functions))
        (colorful-points-after-change))
    (progn ;; turning off
      (mapc #'delete-overlay (mapcar #'cdr colorful-points))
      (setq after-change-functions
            (delete #'colorful-points-after-change after-change-functions)))))

(defun colorful-points--next-color ()
  "Return a color for the next point.  If you've used up all the colors in COLORFUL-POINTS-COLORS, we return a random one instead of an unused one."
  (let ((colors colorful-points-colors))
    (loop for color in
          (mapcar (lambda (cell) (getf (overlay-get (cdr cell) 'face) :background)) colorful-points)
          do (setq colors (remove color colors))))
    (or (car colors) (nth (random (length colorful-points-colors)) colorful-points-colors)))

(defun colorful-points--overlay-for-window (win)
  "Return the overlay representing WIN's point for the current buffer."
  (let ((maybe-win (assoc win colorful-points)))
    (if maybe-win (cdr maybe-win)
      (let ((new (make-overlay 0 0 (current-buffer) t nil)))
        (prog1 new
               (setq colorful-points (cons (cons win new) colorful-points))
               (overlay-put new 'face (list :background (colorful-points--next-color))))))))

(defun colorful-points-after-change (&rest ignored)
  "Called to update the points after every change to the buffer.
Optional argument IGNORED is ignored."
  (loop for (point . overlay) in
        (mapcar (lambda (win) (cons (window-point win) (colorful-points--overlay-for-window win)))
                (remove-if-not (lambda (window)
                                 (equal (window-buffer window) (current-buffer)))
                               (loop for frame in (frame-list) nconcing (window-list frame))))
        do (let ((first point)
                 (last (1+ point))
                 (eol (save-excursion (goto-char point) (line-end-position)))
                 (next-line-blank (save-excursion
                                    (goto-char point)
                                    (= (line-beginning-position 2) (line-end-position 2))))
                 (this-line-blank (save-excursion
                                    (goto-char point)
                                    (= (line-beginning-position) (line-end-position)))))
             ;; (cond ((or (eq first eol) (eq first (point-max)))
             ;;        (setq first (1- point))
             ;;        (setq last point)))
             ;; TODO - make this look nicer
             (move-overlay overlay first last))))
