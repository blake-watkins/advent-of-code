(in-package :aoc)

(defun point+ (&rest points)
  (apply #'map 'list #'+ points))

(defun point- (&rest points)
  (apply #'map 'list #'- points))

(defun point* (k point)
  (mapcar (lambda (c) (* c k)) point))

(defun point-signum (point)
  (mapcar #'signum point))

(defun point-abs (point)
  (mapcar #'abs point))

(defun point-max (&rest points)
  (apply #'map 'list #'max (remove-if #'null points)))

(defun point-min (&rest points)
  (apply #'map 'list #'min (remove-if #'null points)))
