(in-package :aoc)

;; INTERVAL FUNCTIONS
;; Interval is a list of two elements, START and END. Endpoints are included.

(defun interval-start (interval) (first interval))
(defun interval-end (interval) (second interval))

(defun interval-size (interval)
  "Find size of interval."
  (destructuring-bind (start end) interval
    (1+ (- end start))))

(defun intervals-size (intervals)
  "Find size of all intervals. Assumes disjoint intervals."
  (reduce #'+ intervals :key #'interval-size))

(defun interval-intersect (interval-a interval-b)
  "Find intersection of two intervals. Returns intersection as interval or NIL."
  (destructuring-bind (start-a end-a) interval-a
    (destructuring-bind (start-b end-b) interval-b
      (unless (or (> start-a end-b) (> start-b end-a))
	  (list (max start-a start-b) (min end-a end-b))))))

(defun interval-contains (interval value)
  "Return T if INTERVAL contains VALUE, otherwise NIL. "
  (<= (interval-start interval) value (interval-end interval)))

(defun intervals-contain (intervals value)
  "Return an interval containing VALUE if one exists, otherwise NIL."
  (find-if (lambda (interval) (interval-contains interval value))
           intervals))

(defun intervals-intersect (interval intervals)
  "Find the intersection of INTERVAL with all of INTERVALS. Returns list of intersections."
  (remove-if #'null (mapcar (lambda (interval-b)
                              (interval-intersect interval interval-b))
                            intervals)))

(defun intervals-normalize (intervals)
  "Take list of INTERVALS, which could be in any order or overlapping. Return list of disjoint, ordered intervals of biggest possible size."
  (labels ((normalize-rec (current intervals)
	     (cond
               ((null current) nil)
               ((null intervals) (list current))
	       (t (destructuring-bind (compare . rest-intervals) intervals
                    (if (<= (interval-start compare) (1+ (interval-end current)))
		        (normalize-rec
                         (list (interval-start current)
                               (max (interval-end current) (interval-end compare)))
		         rest-intervals)
		        (cons current
                              (normalize-rec (first rest-intervals)
                                             (cdr rest-intervals)))))))))
    (let ((intervals (sort intervals #'< :key #'first)))
      (when intervals
	(normalize-rec (first intervals) (cdr intervals))))))
