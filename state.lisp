(in-package :aoc)

(defun state-unit (x)
  (lambda (s)
    (list x s)))

(defun state-bind (ma f)
  (lambda (s)
    (destructuring-bind (a s-dash) (funcall ma s)
      (let ((mb (funcall f a)))
        (funcall mb s-dash)))))

(defmacro run-state (m s)
  `(let ((*unit-function* 'state-unit)
         (*bind-function* 'state-bind))
     (car (funcall ,m ,s))))


