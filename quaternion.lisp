(in-package :aoc)

(defun q-compose (q1 q2)
  (if (listp q1)
      (destructuring-bind (a1 b1 c1 d1) q1
        (destructuring-bind (a2 b2 c2 d2) q2
          (list (+ (* a1 a2) (* -1 b1 b2) (* -1 c1 c2) (* -1 d1 d2))
                (+ (* a1 b2) (* b1 a2)    (* c1 d2)    (* -1 d1 c2))
                (+ (* a1 c2) (* -1 b1 d2) (* c1 a2)    (* d1 b2))
                (+ (* a1 d2) (* b1 c2)    (* -1 c1 b2) (* d1 a2)))))
      (mapcar (lambda (x) (* q1 x)) q2)))

(defun q-conjugate (q)
  (cons (first q) (mapcar #'- (cdr q))))

(defun q-norm-2 (q)
  (reduce #'+ q :key (lambda (x) (* x x)) :initial-value 0))

(defun q-norm (q)
  (sqrt (q-norm-2 q)))

(defun q-normalize (q)
  (q-compose (/ 1 (q-norm q)) q))

(defun q-reciprocal (q)
  (q-compose (/ 1 (q-norm-2 q)) (q-conjugate q)))

(defun q-round (q)
  (mapcar #'round q))

(defun q-rotor (angle axis)
  (cons (cos (/ angle 2)) (mapcar (lambda (c) (* (sin (/ angle 2)) c)) axis)))

(defun q-conjugate-by (q x)
  (q-compose x (q-compose q (q-reciprocal x))))

(defun q-rotate-vector (vector rotor)
  (cdr (q-conjugate-by (cons 0 vector) rotor)))
