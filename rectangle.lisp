(in-package :aoc)

(defclass rect ()
  ((r-c :initarg :r-c)
   (w-h :initarg :w-h)
   (id :initarg :id :reader rect-id)))

(defun rect-top (rect)
  (first (slot-value rect 'r-c)))
(defun rect-bottom (rect)
  (+ (first (slot-value rect 'r-c))
     (second (slot-value rect 'w-h))))
(defun rect-left (rect)
  (second (slot-value rect 'r-c)))
(defun rect-right (rect)
  (+ (second (slot-value rect 'r-c))
     (first (slot-value rect 'w-h))))

(defun rect-size (rect)
  (reduce #'* (slot-value rect 'w-h)))

(defun rect-compare (rect-a rect-b)
  (sycamore:or-compare
   (sycamore-util:gsymbol-compare (slot-value rect-a 'r-c)
                                  (slot-value rect-b 'r-c))
   (sycamore-util:gsymbol-compare (slot-value rect-a 'w-h)
                                  (slot-value rect-b 'w-h))))

(defmethod print-object ((object rect) stream)
  (format stream "#<RECT #~a ~a ~a>"
          (slot-value object 'id)
          (slot-value object 'r-c)
          (slot-value object 'w-h)))

(defun make-rect (r-c &key w-h r-c2 (id nil))
  (destructuring-bind (r c) r-c
    (unless w-h
      (if r-c2
          (setf w-h (list (- (second r-c2)
                             (second r-c))
                          (- (first r-c2)
                             (first r-c))))
          (return-from make-rect nil)))
    (destructuring-bind (w h) w-h
      (if (or (< r 0)
              (< c 0)
              (<= w 0)
              (<= h 0))
          nil
          (make-instance 'rect :r-c r-c :w-h w-h :id id)))))

(defun rect-intersection (rect-a rect-b)
  ;; Returns the intersection of two rectangles as a RECT, returns NIL if no intersection.
  (let ((left (max (rect-left rect-a) (rect-left rect-b)))
        (right (min (rect-right rect-a) (rect-right rect-b)))
        (top (max (rect-top rect-a) (rect-top rect-b)))
        (bottom (min (rect-bottom rect-a) (rect-bottom rect-b))))
    (make-rect (list top left) :r-c2 (list bottom right))))


(defun rect-difference (rect-a rect-b)
  ;; Returns the difference RECT-A \ RECT-B as a list of rectangles. 
  (let ((r1 (make-rect (list (rect-top rect-a)
                             (rect-left rect-a))
                       :r-c2 (list (rect-bottom rect-a)
                                   (min (max (rect-left rect-a)
                                             (rect-left rect-b))
                                        (rect-right rect-a)))))
        (r2 (make-rect (list (rect-top rect-a)
                             (max (min (rect-right rect-a)
                                       (rect-right rect-b))
                                  (rect-left rect-a)))
                       :r-c2 (list (rect-bottom rect-a)
                                   (rect-right rect-a))))
        (r3 (make-rect (list (rect-top rect-a)
                             (min (max (rect-left rect-a)
                                       (rect-left rect-b))
                                  (rect-right rect-a)))
                       :r-c2 (list (max (min (rect-bottom rect-a)
                                             (rect-top rect-b))
                                        (rect-top rect-a))
                                   (max (min (rect-right rect-a)
                                             (rect-right rect-b))
                                        (rect-left rect-a)))))
        (r4 (make-rect (list (max (min (rect-bottom rect-a)
                                       (rect-bottom rect-b))
                                  (rect-top rect-a))
                             (min (max (rect-left rect-a)
                                       (rect-left rect-b))
                                  (rect-right rect-a)))
                       :r-c2 (list (rect-bottom rect-a)
                                   (max (min (rect-right rect-a)
                                             (rect-right rect-b))
                                        (rect-left rect-a))))))
    (loop for x in (list r1 r3 r2 r4)
          when x
          collect x)))
