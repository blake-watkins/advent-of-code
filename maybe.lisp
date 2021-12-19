(in-package :aoc)

;;; MAYBE MONAD

(defun maybe-unit (x)
  (list :just x))

(defun maybe-bind (ma f)
  (if (eq (car ma) :just)
      (funcall f (second ma))
      (list :nothing)))

(defun maybe-fail ()
  (list :nothing))

(defun maybe-mplus (ma mb)
  (if (eq (first ma) :nothing)
      mb
      ma))

(defun maybe-mzero ()
  (maybe-fail))

(defmacro run-maybe (m)
  `(let ((aoc:*unit-function* 'maybe-unit)
         (aoc:*bind-function* 'maybe-bind))
     ,m))

(defmacro from-maybe (ma &optional (default nil))
  `(let ((aoc:*unit-function* 'maybe-unit)
         (aoc:*bind-function* 'maybe-bind))
     (if (eq (car ,ma) :just)
	 (second ,ma)
	 ,default)))

(defun is-just (ma)
  (eq (first ma) :just))

(defun is-nothing (ma)
  (eq (first ma) :nothing))

