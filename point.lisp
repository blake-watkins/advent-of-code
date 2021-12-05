(in-package :aoc)

(defun point+ (&rest points)
  (apply #'map 'list #'+ points))

(defun point- (&rest points)
  (apply #'map 'list #'- points))

(defun point-signum (point)
  (mapcar #'signum point))

(defun point-abs (point)
  (mapcar #'abs point))
