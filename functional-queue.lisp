(in-package :advent-of-code)

;;; FUNCTIONAL QUEUE 
(defstruct queue
  (front '())
  (back  '()))

(defun queue-empty-p (queue)
  (and (null (queue-front queue))
       (null (queue-back queue))))

;; Pushes item onto queue. If as-list, treats item as a list of items
;; to be pushed.
(defun queue-push-back (item queue &key (as-list nil))
  (let ((new-queue (make-queue)))
    (setf (queue-front new-queue) (queue-front queue))
    (setf (queue-back new-queue)
	  (if as-list
	      (append (reverse item) (queue-back queue))
	      (cons item (queue-back queue))))
    new-queue))
  

(defun queue-pop-front (queue)
  (if (queue-empty-p queue)
      (values nil queue nil)
      (if (null (queue-front queue))
	  (let ((new-queue (make-queue))
		(reversed (reverse (queue-back queue))))
	    (setf (queue-front new-queue) (cdr reversed))
	    (values (car reversed) new-queue t))
	  (let ((new-queue (make-queue)))
	    (setf (queue-front new-queue) (cdr (queue-front queue)))
	    (setf (queue-back new-queue) (queue-back queue))
	    (values (car (queue-front queue)) new-queue t)))))

(defmacro queue-pop-frontf (queue)
  (with-gensyms (val new-queue present-p)
    `(multiple-value-bind (,val ,new-queue ,present-p) (queue-pop-front ,queue)
       (setf ,queue ,new-queue)
       (values ,val ,present-p))))

(defmacro queue-push-backf (item queue &key (as-list nil))
  `(setf ,queue (queue-push-back ,item ,queue :as-list ,as-list)))

