(ql:quickload "split-sequence")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop
       for line = (read-line stream nil)
       while line
       collect (clean-line line))))

(defun clean-line (line)
  (parse-integer (string-right-trim '(#\Return) line)))

(defun fuel-from-mass (mass)
  (let ((fuel (max 0 (- (floor mass 3) 2))))
    (if (= fuel 0)
	fuel
	(+ fuel (fuel-from-mass fuel)))))

(defun parse-instruction (instruction)
  (multiple-value-bind (modes-int opcode) (floor instruction 100)
    (values opcode
	    (loop with modes = '()
	       repeat 3
	       do
		 (multiple-value-bind (rest mode) (floor modes-int 10)
		   (setf modes (cons (if (= mode 0) :position :immediate) modes))
		   (setf modes-int rest))
	       finally (return (nreverse modes))))))

(defun parse-program (program-text)
  (let ((program (split-sequence:split-sequence #\Comma program-text)))
    (setf program (mapcar #'parse-integer program))
    (make-array (length program) :initial-contents program)))

(defun run (program)
  (loop with idx = 0
     for (opcode new-idx) = (process idx program)
     while (not (= opcode 99))
     do (setf idx new-idx)))

(defun get-parameter (parameter-index mode program)
  (ecase mode
    (:immediate (aref program parameter-index))
    (:position  (aref program (aref program parameter-index)))))

(defun set-parameter (parameter-index value program)
  (setf (aref program (aref program parameter-index)) value))

(defmacro get-param (n)
  `(get-parameter (+ ,n idx) (nth ,(1- n) modes) program))

(defmacro set-param (n value)
  `(set-parameter (+ ,n idx) ,value program))

(defun process (idx program &optional (io-streams nil))
  (multiple-value-bind (opcode modes) (parse-instruction (aref program idx))
    (ecase opcode
      ;; Addition & Multiplication
      ((1 2) (let* ((op1 (get-param 1))
		    (op2 (get-param 2))
		    (res (if (= 1 opcode)
			     (+ op1 op2)
			     (* op1 op2))))
	       (set-param 3 res)
	       (setf idx (+ 4 idx))))
      ;; Input
      (3 (if (not io-streams)
	     (progn
	       (format t ">")
	       (set-param 1 (read)))
	     (set-param 1 (funcall io-streams :in)))
	 (setf idx (+ 2 idx)))
      ;; Output
      (4 (let ((out (get-param 1)))
	   (if (not io-streams)	    
	       (format t "~a~%" out)
	       (funcall io-streams :out out)))
	 (setf idx (+ 2 idx)))
      ;; Jump True / False
      ((5 6) (let* ((op1 (get-param 1))
		    (op2 (get-param 2))
		    (is-zerop (= 0 op1)))
	       (if (or (and (= opcode 5) (not is-zerop))
		       (and (= opcode 6) is-zerop))
		   (setf idx op2)
		   (setf idx (+ 3 idx)))))
      ;; Less Than / Equals
      ((7 8) (let* ((op1 (get-param 1))
		    (op2 (get-param 2))
		    (res (if (if (= opcode 7)
				 (< op1 op2)
				 (= op1 op2))
			     1
			     0)))
	       (set-param 3 res)
	       (setf idx (+ 4 idx))))
	       
      (99 (setf idx (1+ idx))))
    (list opcode idx)))

(ql:quickload "pettomato-deque")
(use :pettomato-deque)

(defun io-object (input)
  (let ((output nil))
    (lambda (method &rest rest)
      (ecase method
	(:in (let ((item (car input)))
	       (setf input (cdr input))
	       item))
	(:out (setf output (cons (car rest) output)))
	(:get-output output)))))
			  
(defparameter *amplifier-controller-software* "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")

(defun amplifier (input-signal phase-setting program)
  (let ((io-object (io-object (list phase-setting input-signal))))
    (loop with idx = 0 for (opcode new-idx) = (process idx program io-object)
       while (not (= opcode 99))
       do (setf idx new-idx))
    (car (funcall io-object :get-output))))

(defun amplification-circuit (phase-settings program-source)
  (let ((signal 0))
    (loop
       for setting in phase-settings
       for program = (parse-program program-source)
       do (setf signal (amplifier signal setting program)))
    signal))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
              append (mapcar (lambda (l) (cons element l))
                             (all-permutations (remove element list)))))))


;; Problem 3

(defun move-left (pos) (make-pos (1- (pos-x pos)) (pos-y pos)))
(defun move-right (pos) (make-pos (1+ (pos-x pos)) (pos-y pos)))
(defun move-up (pos) (make-pos (pos-x pos) (1+ (pos-y pos))))
(defun move-down (pos) (make-pos (pos-x pos) (1- (pos-y pos))))
(defun pos-y (pos) (cdr pos))
(defun make-pos (x y) (cons x y))
(defun pos-x (pos) (car pos))

(defun pos-equal-p (a b)
  (and (= (pos-x a) (pos-x b))
       (= (pos-y a) (pos-y b))))

(defun pos-manhattan (pos)
  (+ (abs (pos-x pos)) (abs (pos-y pos))))

(defun pos-less (a b)
  (let ((ma (pos-manhattan a))
	(mb (pos-manhattan b)))
    (or (< ma mb)
	(and (= ma mb)
	     (or (< (pos-x a) (pos-x b))
		 (and (= (pos-x a) (pos-x b))
		      (< (pos-y a) (pos-y b))))))))

(defun parse-move (move)
  (let ((amount (parse-integer (subseq move 1)))
	(fn (ecase (char move 0)
	      (#\R #'move-right)
	      (#\L #'move-left)
	      (#\U #'move-up)
	      (#\D #'move-down))))
    (values fn amount)))


(defun get-squares-of-move (start-position start-steps move)
  (labels ((get-squares-iter (position steps function amount result)
	     (if (= 0 amount)
		 (values (nreverse result) position steps)
		 (let ((new-position (funcall function position)))
		   (get-squares-iter new-position
				     (1+ steps)
				     function
				     (1- amount)
				     (cons (cons new-position steps) result))))))
    (multiple-value-bind (function amount) (parse-move move)
      (get-squares-iter start-position start-steps function amount '()))))

(defun get-squares-of-path (start-position start-steps path)
  (labels ((get-squares-iter (position steps moves result)
	     (if (null moves)
		 (values (apply #'nconc (nreverse result)) position steps)
		 (multiple-value-bind (squares new-position new-steps)
		     (get-squares-of-move position steps (car moves))
		   (get-squares-iter new-position
				     new-steps
				     (cdr moves)
				     (cons squares result))))))
    (let ((moves (split-sequence:split-sequence #\Comma path)))
      (get-squares-iter start-position start-steps moves '()))))

(defun memoize (key value hash-table)
  (multiple-value-bind (result present-p) (gethash key hash-table)
    (if (and present-p (<= result value))
	result
	(setf (gethash key hash-table) value))))

(defun sorted-intersection (list1 list2 predicate
			    &key (key (lambda (x) x)))			    
  (labels ((sorted-intersection-iter (list1 list2 predicate result)
	     (if (or (null list1) (null list2))
		 result
		 (let ((a (funcall key (car list1)))
		       (b (funcall key (car list2))))
		   (cond ((funcall predicate a b)
			  (sorted-intersection-iter (cdr list1) list2
						    predicate
						    result))
			 ((funcall predicate b a)
			  (sorted-intersection-iter list1 (cdr list2)
						    predicate
						    result))
			 (t (sorted-intersection-iter (cdr list1) (cdr list2)
						      predicate
						      (cons a result))))))))
    (sorted-intersection-iter list1 list2 predicate '())))

(defun process-path (path)
  (let ((squares (get-squares-of-path (make-pos 0 0) 1 path))
	(hash-table (make-hash-table :test 'equal)))
    (loop for (square . steps) in squares do (memoize square steps hash-table))
    (setf squares (sort squares #'pos-less :key #'car))
    (values squares hash-table)))

(defun solve-puzzle-3 (path1 path2)
  (multiple-value-bind (s1 ht1) (process-path path1)
    (multiple-value-bind (s2 ht2) (process-path path2)
      (let* ((intersections (sorted-intersection s1 s2 #'pos-less :key #'car))
	     (costs (mapcar (lambda (i) (cons i (+ (gethash i ht1)
						   (gethash i ht2))))
			    intersections)))
	(reduce (lambda (i1 i2) (if (< (cdr i1) (cdr i2)) i1 i2)) costs)))))
    
	 
(defun digits (num)
  (labels ((digits-iter (num result)
	     (if (< num 10)
		 (cons num result)
		 (multiple-value-bind (quotient remainder) (floor num 10)
		   (digits-iter quotient (cons remainder result))))))
    (digits-iter num '())))

(defun has-adjacent-p (digits)
  (labels ((has-adjacent-p-rec (last digits)
	     (cond ((null digits) 'nil)
		   ((= last (car digits)) t)
		   (t (has-adjacent-p-rec (car digits) (cdr digits))))))
    (if (< (length digits) 2)
	'nil
	(has-adjacent-p-rec (car digits) (cdr digits)))))

(defun has-length-2-adjacent-p (digits)
  (labels ((has-length-2-adjacent-p-rec (last len digits)
	     (cond ((null digits) (= len 2))
		   ((= last (car digits))
		    (has-length-2-adjacent-p-rec (car digits)
						 (1+ len)
						 (cdr digits)))
		   (t (if (= len 2)
			  t
			  (has-length-2-adjacent-p-rec (car digits)
						       1
						       (cdr digits)))))))
    (if (< (length digits) 2)
	'nil
	(has-length-2-adjacent-p-rec (car digits) 1 (cdr digits)))))
								       
    
(defun ascending-p (digits)
  (labels ((ascending-p-rec (last digits)
	     (cond ((null digits) t)
		   ((< (car digits) last) 'nil)
		   (t (ascending-p-rec (car digits) (cdr digits))))))
    (if (< (length digits) 2)
	t
	(ascending-p-rec (car digits) (cdr digits)))))

(defun generate-password (initial-digits final previous-digit digits)
  (loop for digit
     from (max (car initial-digits) previous-digit)
     upto 9
     do
       (let ((new-digits (cons digit digits)))
	 (if (null (cdr initial-digits))
	     (if (has-adjacent-p (nreverse new-digits))
		 (list new-digits)
		 '())
	     (generate-password (cdr initial-digits) final digit new-digits)))))


;; Problem 6


(defvar *test6* '(A (B (C (D (E F (J (K L))) I)) (G H))))
(defun get-num-orbits (depth primary)
  (+ depth (loop for x in (gethash primary *ht6*) summing (get-num-orbits (1+ depth) x))))

(defparameter *ht6* (make-hash-table :test 'equal))

(defun add-satellite (primary satellite)
  (let ((satellites (gethash primary *ht6*)))
    (when (not (member satellite satellites :test 'equal))
      (setf (gethash primary *ht6*) (cons satellite satellites)))))

(defun add-satellite (primary satellite)
  (setf (gethash satellite *ht6*) primary))

(defun parse-orbits (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	 while line
       do (add-satellite (subseq line 0 3) (subseq line 4 7)))))

(defun primaries (satellite)
  (nreverse (loop 
	       collecting satellite
	       while (not (equal satellite "COM"))
	       do (setf satellite (gethash satellite *ht6*)))))

(defun strip-common-prefix (list1 list2)
  (if (string= (car list1) (car list2))
      (strip-common-prefix (cdr list1) (cdr list2))
      (list list1 list2)))



;;; Problem 7

(ql:quickload "pettomato-deque")
(use-package :pettomato-deque)

(defparameter *input2* "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,2,6,23,27,1,6,27,31,2,31,9,35,1,35,6,39,1,10,39,43,2,9,43,47,1,5,47,51,2,51,6,55,1,5,55,59,2,13,59,63,1,63,5,67,2,67,13,71,1,71,9,75,1,75,6,79,2,79,6,83,1,83,5,87,2,87,9,91,2,9,91,95,1,5,95,99,2,99,13,103,1,103,5,107,1,2,107,111,1,111,5,0,99,2,14,0,0")
    
(defun solve-2 (input)
  (let ((state (make-instance 'computer-state :program-text input))
	(computer (with-state-monad
		    (set-memory 1 12)
		    (set-memory 2 2)
		    (run-until-halt)
		    (get-memory 0))))
    (funcall computer state)))

(defun solve-2-2 (input)
  (loop
     named outer
     for noun from 0 to 99
     do (loop
	   for verb from 0 to 99
	   do
	     (let ((state (make-instance 'computer-state
					 :program-text input))
		   (computer (with-state-monad
			       (set-memory 1 noun)
			       (set-memory 2 verb)
			       (run-until-halt)
			       (get-memory 0))))
	       (when (= 19690720 (funcall computer state))
		 (return-from outer (list noun verb)))))))

(defun solve-5 (input)
  (let ((state (make-instance 'computer-state
			      :program-text input))
	(computer (with-state-monad
		    (provide-input 5)
		    (run-until-halt)
		    (get-output))))
    (funcall computer state)))


(defun get-amplifier (program-text phase-setting)
  (let ((amp-state (make-instance 'computer-state :program-text program-text)))
    (setf amp-state (nth-value 1 (funcall (provide-input phase-setting)
					  amp-state)))
    (lambda (input-signal)
      (multiple-value-bind (output-signal next-state)
	  (funcall (with-state-monad
		     (provide-input input-signal)
		     (assign status (run-until-output-or-halt))
		     (if (not status)
			 (state-unit nil)
			 (get-output)))
		   amp-state)
	(setf amp-state next-state)
	output-signal))))


(defun run-circuit (program-text phase-settings)
  (let* ((num-amplifiers (length phase-settings))
	 (amplifiers (make-array num-amplifiers))
	 (wires (make-array num-amplifiers)))
    (loop
       for phase-setting in phase-settings
       for i from 0
       do
	 (setf (aref amplifiers i) (get-amplifier program-text phase-setting))
	 (setf (aref wires i) 0))
    (loop
       with continue = t
       for current-amplifier = 0 then (mod (+ 1 current-amplifier)
					   num-amplifiers)
       do
	 (let* ((input-signal (aref wires current-amplifier))
		(output-signal (funcall (aref amplifiers current-amplifier)
					input-signal)))	   
	   (if output-signal
	       (setf (aref wires (mod (+ 1 current-amplifier)
				      num-amplifiers))
		     output-signal)
	       (setf continue nil)))
       while continue)
    (aref wires 0)))



(defun solve-7 (input)
  (loop
     for setting in (all-permutations '(4 3 2 1 0))
     maximizing (loop
		   with signal = 0
		   for amp-setting in setting
		   do 
		     (let ((amplifier (get-amplifier input amp-setting)))
		       (setf signal (funcall amplifier signal)))
		   finally (return signal))))

(defun solve-7-2 (input)
  (loop
     for setting in (all-permutations '(5 6 7 8 9))
     maximizing (run-circuit input setting)))


;; Problem 8

(defun get-layers (input width height)
  (let ((layer-size (* width height))
	(image-size (length input)))
    (loop
       for x = 0 then (+ x layer-size)
       while (< x image-size)
       collect (subseq input x (+ x layer-size)))))

(defun count-char-on-layer (char layer)
  (loop for c across layer when (char= c char) sum 1))

(defun solve-8 (input width height)
  (let* ((layers (get-layers input width height))
	 (min-layer (reduce #'(lambda (min-so-far cur)
				(if (< (count-char-on-layer #\0 cur)
				       (count-char-on-layer #\0 min-so-far))
				    cur
				    min-so-far))
			    layers)))
    (* (count-char-on-layer #\1 min-layer)
       (count-char-on-layer #\2 min-layer))))

(defun find-first-nontransparent (&rest chars)
  (ecase (car chars)
    (#\0 #\Space)
    (#\1 #\*)
    (#\2 (apply #'find-first-nontransparent (cdr chars)))))

(defun solve-8-2 (input width height)
  (let* ((layers (get-layers input width height)))
    (format t "~{~a~%~}~%" (get-layers (apply #'map 'string #'find-first-nontransparent layers) width 1))))
    
;; Problem 9

(defun solve-9 (input)
  (let ((state (make-instance 'computer-state
			      :program-text input))
	(computer (with-state-monad
		    (provide-input 1)
		    (run-until-halt)
		    (get-all-output))))
    (funcall computer state)))

(defun solve-9-2 (input)
  (let ((state (make-instance 'computer-state
			      :program-text input))
	(computer (with-state-monad
		    (provide-input 2)
		    (run-until-halt)
		    (get-all-output))))
    (funcall computer state)))


;; Problem 10

(defparameter *input10* ".#..#
.....
#####
....#
...##")

(defun parse-file ()
  (one-or-more (with-monad
		 (assign line (one-or-more (parse-character ".#")))
		 (parse-newline)
		 (unit line))))

(defun direction-from (source target)
  (if (equal source target)
      (list 0 0)
      (let* ((dx (- (first target) (first source)))
	     (dy (- (second target) (second source)))
	     (common-factor (gcd dx dy)))
	(list (/ dx common-factor) (/ dy common-factor)))))

(defun angle-from (source target)
  (if (equal source target)
      nil
      (let ((vector (direction-from source target)))
	(if (and (= 0 (first vector)) (< (second vector) 0))
	    0
	    (+ PI
	       (atan (- (first vector)) (second vector)))))))

(defun distance-from (source target)
  (sqrt (reduce #'+ (map 'list
			 #'(lambda (source target) (expt (- target source) 2))
			 source
			 target))))
		
  

;;compare whether two lists differ, compares elementwise until a <, = or > b
(defun compare (pos-a pos-b)
  (labels ((list-sorter (list-a list-b)
	     (reduce #'(lambda (last cur) (if (= last 0) cur last))
		     (mapcar #'(lambda (a b) (- b a)) list-a list-b))))
  (let ((val (list-sorter pos-a pos-b)))
    (cond ((> val 0) 1)
	  ((< val 0) -1)
	  (t 0)))))

(defun asteroid-list (input)
  (let ((parsed (run-parser (parse-file) input)))
    (loop
       for line in parsed
       for y from 0
       append (loop
		 for char in line
		 for x from 0
		 when (char= char #\#)
		 collect (list x y)))))

(defun solve-10 (input)
  (let ((asteroid-list (asteroid-list input)))
    (loop
       for asteroid in asteroid-list
       for visible-directions =
	 (mapcar #'(lambda (a) (direction-from asteroid a)) asteroid-list)
       for unique-directions =
	 (apply #'tree-set #'compare visible-directions)
       maximizing (- (tree-set-count unique-directions) 1))))

(defun get-visible-asteroids (asteroid asteroid-list)
  (let ((direction-distance-map (make-tree-map #'compare)))
    (loop
       for target in asteroid-list
       do
	 (unless (= 0 (compare asteroid target))
	   (let* ((direction (direction-from asteroid target))
		  (closest (tree-map-find direction-distance-map
					  direction
					  '())))
	     (if (or (null closest)
		     (< (distance-from asteroid target)
			(distance-from asteroid closest)))
		 (setf direction-distance-map
		       (tree-map-insert direction-distance-map
					direction
					target))))))
    (tree-map-values direction-distance-map)))

(defun solve-10-2 (asteroid input)
  (let* ((asteroid-list (asteroid-list input))
	 (visible-asteroids (get-visible-asteroids asteroid asteroid-list))
	 (asteroid-angles (mapcar
			   #'(lambda (value) (cons value
						   (angle-from asteroid value)))
			   visible-asteroids))
	 (target (car (nth 199 (sort asteroid-angles #'< :key #'cdr)))))
    (+ (* 100 (first target))
       (second target))))


;; Problem 11


(defclass hpr-state ()
  ((pos       :initarg :pos       :initform '(0 0))
   (direction :initarg :direction :initform :up)
   (painted   :initarg :painted   :initform (make-tree-map #'compare))
   (computer  :initarg :computer  :initform nil)))

(defmethod initialize-instance :after ((state hpr-state)
				       &key program-text template)
  (when program-text
    (setf (slot-value state 'computer)
	  (make-instance 'computer-state :program-text program-text)))
  (when template
    (loop
       for field in '(pos direction painted computer)
       do (setf (slot-value state field) (slot-value template field)))))

(defun panel-painted-p ()
  (lambda (state)
    (with-slots (painted pos) state
      (values (nth-value 2 (tree-map-find painted pos :black)) state))))

(defun paint-panel (color)
  (lambda (state)
    (with-slots (painted pos) state
      (let ((new-painted (tree-map-insert painted pos color))
	    (new-state (make-instance 'hpr-state
				      :template state)))
	(setf (slot-value new-state 'painted) new-painted)
	(values color new-state)))))

(defun turn-robot (direction)
  (lambda (state)
    (with-slots (pos (old-direction direction)) state
      (let* ((new-direction
	      (ecase direction
		(:left
		 (ecase old-direction
		   (:up :left) (:left :down) (:down :right) (:right :up)))
		(:right
		 (ecase old-direction
		   (:up :right) (:right :down) (:down :left) (:left :up)))))
	     (new-pos
	      (destructuring-bind (pos-x pos-y) pos
		(ecase new-direction
		  (:up    (list pos-x (- pos-y 1)))
		  (:down  (list pos-x (+ pos-y 1)))
		  (:left  (list (- pos-x 1) pos-y))
		  (:right (list (+ pos-x 1) pos-y)))))
	     (new-state (make-instance 'hpr-state
				       :template state)))
	(setf (slot-value new-state 'pos) new-pos)
	(setf (slot-value new-state 'direction) new-direction)
	(values (cons new-pos new-direction) new-state)))))
		      
	
(defun sense-panel ()
  (lambda (state)
    (with-slots (painted pos) state
      (let ((color (tree-map-find painted pos :black)))
	(values color state)))))

(defun number-to-direction (number)
  (ecase number (0 :left) (1 :right)))

(defun number-to-color (number)
  (ecase number (0 :black) (1 :white)))

(defun color-to-number (color)
  (ecase color (:black 0) (:white 1)))

(defun run-program-hpr (program)
  (lambda (state)
    (with-slots ((computer-state computer)) state
	(multiple-value-bind (value new-computer-state)
	    (funcall program computer-state)
	  (let ((new-state (make-instance 'hpr-state
					  :template state)))
	    (setf (slot-value new-state 'computer) new-computer-state)
	    (values value new-state))))))
		   
(defun step-robot ()
  ;; Provide input to the computer, paint and turn robot
  (with-state-monad
    (assign panel-color (sense-panel))
    (run-program-hpr (provide-input (color-to-number panel-color)))
    (assign color-p (run-program-hpr (run-until-output-or-halt)))
    (if color-p
	(with-state-monad
	  (assign new-color (run-program-hpr (get-output)))
	  (assign direction-p (run-program-hpr (run-until-output-or-halt)))
	  (if direction-p
	      (with-state-monad
		(assign new-direction (run-program-hpr (get-output)))

		(paint-panel (number-to-color new-color))
		(turn-robot (number-to-direction new-direction))
		(state-unit t))
	      (state-unit nil)))
	(state-unit nil))))
	  
;;New rules
(defun run-robot (input)
  (labels ((run-robot ()
	     (with-state-monad
	       (assign continue (step-robot))
	       (if continue
		   (run-robot)
		   (state-unit nil)))))
    (let ((robot-state (make-instance 'hpr-state :program-text input)))
      (funcall (state-then (paint-panel :white) (run-robot)) robot-state))))

(defun picture (painted)
  (let ((keys (tree-map-keys painted)))
    (flet ((comparator (test key)
	     (reduce #'(lambda (last cur) (if (funcall test cur last) cur last))
		   keys
		   :key key)))
      (let ((min-x (comparator #'< #'car))
	    (max-x (comparator #'> #'car))
	    (min-y (comparator #'< #'second))
	    (max-y (comparator #'> #'second)))
	(let ((squares
	       (loop for y from min-y to max-y
		  collect (loop for x from min-x to max-x
			     collect (tree-map-find painted (list x y)
						    :black)))))
	  (loop for line in squares
	     do (loop for square in line
		   do (format t "~a" (if (eq square :black) #\Space #\*)))
	       (format t "~%")))))))


;; Problem 12
(defparameter *input12* "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>")

(defun parse-moon ()
  (with-monad
    (parse-string "<x=")
    (assign x (parse-number))
    (parse-string ", y=")
    (assign y (parse-number))
    (parse-string ", z=")
    (assign z (parse-number))
    (parse-string ">")
    (unit (list x y z))))

(defun parse-file ()
  (one-or-more (with-monad
		 (assign res (parse-moon))
		 (parse-newline)
		 (unit res))))


(defun get-gravity (positions velocities)
  (let ((velocity (make-tree-map #'compare)))
    (setf velocity (tree-map-insert-alist velocity (map 'list
							#'cons
							positions
							velocities)))
	 
    (labels ((compare-components (a b)
	       (cond ((< a b) -1)
		     ((= a b)  0)
		     (t 1)))
	     (gravity-between (pair)
	       (destructuring-bind (a b) pair
		 (let* ((diff (map 'list #'compare-components a b))
			(old-velocity-a (tree-map-find velocity a '(0 0 0)))
			(new-velocity-a
			 (map 'list #'- old-velocity-a diff))
			(old-velocity-b (tree-map-find velocity b '(0 0 0)))
			(new-velocity-b
			 (map 'list #'+ old-velocity-b diff)))
		   (setf velocity (tree-map-insert velocity a new-velocity-a))
		   (setf velocity (tree-map-insert velocity b new-velocity-b))))))
      (map-combinations #'gravity-between positions :length 2)
      (loop
	 for position in positions
	 collect (tree-map-find velocity position)))))

(defun add-positions-and-velocities (positions velocities)
  (map 'list
       #'(lambda (position velocity)
	   (map 'list #'+ position velocity))
       positions
       velocities))

(defun total-energy (positions velocities)
  (reduce #'+
	  (loop
	     for position in positions
	     for velocity in velocities
	     collect (let ((potential (apply #'+ (mapcar #'abs position)))
			   (kinetic (apply #'+ (mapcar #'abs velocity))))
		       (* potential kinetic)))))
(defun solve-12 (input)
  (loop
     with positions = (run-parser (parse-file) input)
     with velocities = (loop repeat (length positions) collect '(0 0 0))
     repeat 10
     do
       (setf velocities (get-gravity positions velocities))
       (setf positions (add-positions-and-velocities positions velocities))
     collect positions))

(defun solve-component (n input)
  (loop
     named outer
     with seen-x = (make-tree-map #'compare)
     with positions = (run-parser (parse-file) input)
     with velocities = (loop repeat (length positions) collect '(0 0 0))
     repeat 1000000
     for i from 0
     do
       
       (let* ((x-pos (mapcar #'(lambda (x) (nth n x)) positions))
	      (x-vel (mapcar #'(lambda (x) (nth n x)) velocities))
	      (x-cat (append x-pos x-vel)))
	 (if (tree-map-contains seen-x x-cat) 
	     (return-from outer i)
	     (setf seen-x (tree-map-insert seen-x x-cat i))))


       (setf velocities (get-gravity positions velocities))
       (setf positions (add-positions-and-velocities positions velocities))))


;; Problem 13

(defclass arcade-state ()
  ((painted   :initarg :painted   :initform (make-tree-map #'compare))
   (ball-pos  :initarg :ball-pos  :initform nil)
   (paddle-pos :initarg :paddle-pos :initform nil)
   (computer  :initarg :computer  :initform nil)))

(defmethod initialize-instance :after ((state arcade-state)
				       &key program-text template)
  (when program-text
    (setf (slot-value state 'computer)
	  (make-instance 'computer-state :program-text program-text)))
  (when template
    (loop
       for field in '(painted computer ball-pos paddle-pos)
       do (setf (slot-value state field) (slot-value template field)))))

(defun run-program-arcade (program)
  (lambda (state)
    (with-slots ((computer-state computer)) state
	(multiple-value-bind (value new-computer-state)
	    (funcall program computer-state)
	  (let ((new-state (make-instance 'arcade-state
					  :template state)))
	    (setf (slot-value new-state 'computer) new-computer-state)
	    (values value new-state))))))

(defun get-position (item)
  (lambda (state)
    (values (slot-value state (ecase item
				(:ball 'ball-pos)
				(:paddle 'paddle-pos)))
	    state)))

(defun draw-tile (x y tile-id)
  (lambda (state)
    (with-slots (painted) state
      (let ((new-painted (tree-map-insert painted (list x y) tile-id))
	    (new-state (make-instance 'arcade-state
				      :template state)))
	(format t "draw tile ~a ~a ~a~%" x y tile-id)
	(setf (slot-value new-state 'painted) new-painted)
	(cond ((= tile-id 4) (setf (slot-value new-state 'ball-pos) (list x y)))
	      ((= tile-id 3) (setf (slot-value new-state 'paddle-pos) (list x y))))
	      
	(values tile-id new-state)))))

(defun move-joystick ()
  (with-state-monad 
    (assign ball-pos (get-position :ball))
    (assign paddle-pos (get-position :paddle))
    (if (not (and ball-pos paddle-pos))
	(state-unit nil)
	(let* ((diff (- (car ball-pos) (car paddle-pos)))
	       (instruction (cond ((> diff 0) 1)
				  ((= diff 0)  0)
				  ((< diff 0) -1))))
	  (run-program-arcade
	   (set-input-source #'(lambda () instruction)))))))

		    
(defun step-arcade ()
  (flet ((run-then-get-output ()
	     (state-then (run-until-output-or-halt) (get-output))))
    (with-state-monad
      (assign x (run-program-arcade (run-then-get-output)))
      (assign y (run-program-arcade (run-then-get-output)))
      (assign tile-id (run-program-arcade (run-then-get-output)))
      (if (and x y tile-id)
	  (if (and (= x -1) (= y 0))
	      (progn (format t "score = ~a~%" tile-id)
		     (state-unit t))
	      (with-state-monad
		(draw-tile x y tile-id)
		(move-joystick)
		(state-unit t)))
	  (state-unit nil)))))
	  
(defun run-arcade-until-halt ()
  (with-state-monad
    (assign continue (step-arcade))
    (if continue
	(run-arcade-until-halt)
	(state-unit nil))))

(defun picture (painted)
  (let ((keys (tree-map-keys painted)))
    (flet ((comparator (test key)
	     (reduce #'(lambda (last cur) (if (funcall test cur last) cur last))
		   keys
		   :key key)))
      (let ((min-x (comparator #'< #'car))
	    (max-x (comparator #'> #'car))
	    (min-y (comparator #'< #'second))
	    (max-y (comparator #'> #'second)))
	(let ((squares
	       (loop for y from min-y to max-y
		  collect (loop for x from min-x to max-x
			     collect (tree-map-find painted (list x y)
						    0)))))
	  (loop for line in squares
	     do (loop for square in line
		   do (format t "~a"
			      (ecase square
				(0 #\Space)
				(1 #\#)
				(2 #\O)
				(3 #\_)
				(4 #\*))))
	       (format t "~%")))))))

;; Problem 14

(defparameter *input14* "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(defun parse-chemical ()
  (with-monad
    (assign amount (parse-number))
    (parse-space)
    (assign name (parse-characters #'upper-case-p))
    (unit (cons name amount))))

(defun parse-chemical-list ()
  (with-monad
    (assign first (parse-chemical))
    (assign rest (zero-or-more (with-monad
				 (parse-string ", ")
				 (parse-chemical))))
    (unit (cons first rest))))

(defun parse-reaction ()
  (with-monad
    (assign reactants (parse-chemical-list))
    (parse-string " => ")
    (assign product (parse-chemical))
    (unit (list product reactants))))

(defun parse-file ()
  (one-or-more (with-monad
		 (assign reaction (parse-reaction))
		 (parse-newline)
		 (unit reaction))))

(defun string-compare (string1 string2)
  (cond ((string< string1 string2) -1)
	((string= string1 string2) 0)
	(t 1)))

;;post-order traversal
(defun topological-sort (product reactions)
  (labels ((topological-sort-rec (product reactions visited)
	     (if (tree-set-find visited product)
		 (values '() visited)
		 (if (string= product "ORE")
		     (values (list "ORE") (tree-set-insert visited "ORE"))
		     (destructuring-bind (amount reactants)
			 (tree-map-find reactions product)
		       (declare (ignore amount))
		       (setf visited (tree-set-insert visited product))
		       (let ((result '()))
			 (loop
			    for reactant in reactants
			    do
			      (multiple-value-bind (new-result new-visited)
				  (topological-sort-rec (car reactant) reactions visited)
				(setf visited new-visited)
				(setf result (append result new-result))))
			 (values (append result (list product))
				 visited)))))))
    (nreverse (topological-sort-rec product
			  reactions
			  (make-tree-set #'string-compare)))))
	
(defun solve-14 (needed-fuel input)
  (let* ((reactions
	  (alist-tree-map
	   (mapcar
	    #'(lambda (reaction)
		(let ((product-name (caar reaction))
		      (product-amount (cdar reaction))
		      (reactants (cdr reaction)))
		  (cons product-name (cons product-amount reactants))))
	    (run-parser (parse-file) input))
	   #'string-compare))
	 (order (topological-sort "FUEL" reactions))
	 (required (make-tree-map #'string-compare)))
    (setf required (tree-map-insert required "FUEL" needed-fuel))
    (loop
       for product in order
       do
	 (when (string/= product "ORE")
	   (destructuring-bind (produced-amount reactants)
	       (tree-map-find reactions product)
	     (let* ((required-amount (tree-map-find required product))
		    (number-reactions (ceiling required-amount produced-amount)))
	       (loop
		  for reactant in reactants
		  do
		    (let ((reactant-name (car reactant))
			  (reactant-required-amount (cdr reactant)))
		      (setf required
			    (tree-map-insert required
					     reactant-name
					     (+ (tree-map-find required
							       reactant-name
							       0)
						(* reactant-required-amount
						   number-reactions))))))))))
       (tree-map-find required "ORE")))


(defun test-function (n)
  (- 1000000000000
     (solve-14 n (read-file "input14")) ))

(defun binary-search (min max function)
  (if (or (= min max)
	  (= min (- max 1)))
      min
      (let* ((test-num (floor (+ min max) 2))
	     (test (funcall function test-num)))
	(cond ((< test 0)
	       (binary-search min test-num function))
	      ((= test 0)
	       test)
	      (t
	       (binary-search test-num max function))))))

;; Problem 15

(defun compare-list (pos-a pos-b)
  (labels ((list-sorter (list-a list-b)
	     (reduce #'(lambda (last cur) (if (= last 0) cur last))
		     (mapcar #'(lambda (a b) (- b a)) list-a list-b))))
  (let ((val (list-sorter pos-a pos-b)))
    (cond ((> val 0) 1)
	  ((< val 0) -1)
	  (t 0)))))

(defclass droid-state ()
  ((pos       :initarg :pos       :initform '(0 0))
   (world     :initarg :world
	      :initform (tree-map-insert (make-tree-map #'compare-list)
					 '(0 0)
					 :empty))
	      
   (computer  :initarg :computer  :initform nil)))

(defmethod initialize-instance :after ((state droid-state)
				       &key program-text template)
  (when program-text
    (setf (slot-value state 'computer)
	  (make-instance 'computer-state :program-text program-text)))
  (when template
    (loop
       for field in '(pos world computer)
       do (setf (slot-value state field) (slot-value template field)))))

(defun get-world-square (pos)
  (lambda (state)
    (with-slots (world) state
      (values (tree-map-find world pos :unexplored) state))))

(defun set-world-square (pos value)
  (lambda (state)
    (with-slots (world) state
      (let ((new-world (tree-map-insert world pos value))
	    (new-state (make-instance 'droid-state
				      :template state)))
	(setf (slot-value new-state 'world) new-world)
	(values value new-state)))))

(defun get-pos ()
  (lambda (state)
    (with-slots (pos) state
      (values pos state))))

(defun set-pos (value)
  (lambda (state)
    (with-slots (pos) state
      (let ((new-state (make-instance 'droid-state
				      :template state)))
	(setf (slot-value new-state 'pos) value)
	(values value new-state)))))

(defun square-in-direction (pos dir)
  (destructuring-bind (pos-x pos-y) pos
    (ecase dir
      (:up    (list pos-x (- pos-y 1)))
      (:down  (list pos-x (+ pos-y 1)))
      (:left  (list (- pos-x 1) pos-y))
      (:right (list (+ pos-x 1) pos-y)))))

(defun direction-to-number (direction)
  (ecase direction (:up 1) (:down 2) (:left 3) (:right 4)))

(defun number-to-status (number)
  (ecase number (nil nil) (0 :wall) (1 :moved) (2 :moved-to-oxygen-system)))

(defun move-droid (direction)
  (with-state-monad
    (assign pos (get-pos))
    (assign status (run-program-droid
		    (with-state-monad
		      (provide-input (direction-to-number direction))
		      (run-until-output-or-halt)
		      (get-output))))
    (let ((status (number-to-status status))
	  (target (square-in-direction pos direction)))
      (ecase status
	(nil (state-unit status))
	(:wall  (with-state-monad
		  (set-world-square target :wall)
		  (state-unit status)))
	(:moved (with-state-monad
		  (set-world-square target :empty)
		  (set-pos target)
		  (state-unit status)))
	(:moved-to-oxygen-system
	 (with-state-monad
	   (set-world-square target :oxygen)
	   (set-pos target)
	   (state-unit status)))))))
		
       
(defun run-program-droid (program)
  (lambda (state)
    (with-slots ((computer-state computer)) state
	(multiple-value-bind (value new-computer-state)
	    (funcall program computer-state)
	  (let ((new-state (make-instance 'droid-state
					  :template state)))
	    (setf (slot-value new-state 'computer) new-computer-state)
	    (values value new-state))))))
		   
	  

(defun picture (world)
  (let ((keys (tree-map-keys world)))
    (flet ((comparator (test key)
	     (reduce #'(lambda (last cur) (if (funcall test cur last) cur last))
		   keys
		   :key key)))
      (let ((min-x (comparator #'< #'car))
	    (max-x (comparator #'> #'car))
	    (min-y (comparator #'< #'second))
	    (max-y (comparator #'> #'second)))
	(let ((squares
	       (loop for y from min-y to max-y
		  collect (loop for x from min-x to max-x
			     collect (tree-map-find world (list x y)
						    :unexplored)))))
	  (loop for line in squares
	     do (loop for square in line
		   do (format t "~a"
			      (ecase square
				(:unexplored #\Space)
				(:empty #\.)
				(:wall #\#)
				(:oxygen #\o))))
	       (format t "~%")))))))

(defun move (command)
  (multiple-value-bind (result new-state)
      (funcall command *droid*)
    (setf *droid* new-state)
    (picture (slot-value *droid* 'world))
    result))

(defun clockwise (dir)
  (ecase dir (:down :left) (:left :up) (:up :right) (:right :down)))

(defun anticlockwise (dir)
  (ecase dir (:down :right) (:right :up) (:up :left) (:left :down)))

(defun find-walk-direction (wall-direction)
  (let ((walk-direction (clockwise wall-direction)))
    (with-state-monad
      (assign status (move-droid walk-direction))
      (ecase status
	(:wall (state-unit walk-direction))
	((:moved :moved-to-oxygen-system)
	 (with-state-monad 
	   (assign status (move-droid wall-direction))
	   (ecase status
	     (:wall (state-unit wall-direction))
	     ((:moved :moved-to-oxygen-system)
	      (state-unit (anticlockwise wall-direction))))))))))
       
(defun walk-wall (n wall-direction)
  (if (= n 0)
      (state-unit wall-direction)
      (with-state-monad
	(assign new-direction (find-walk-direction wall-direction))
	(walk-wall (- n 1) new-direction))))

(defconstant +neighbours+ '((1 0) (-1 0) (0 1) (0 -1)))

(defun add-positions (a b)
  (map 'list #'+ a b))

(defun get-neighbours (position)
  (mapcar #'(lambda (diff) (add-positions position diff)) +neighbours+))

(defun moveable-p (position world)
  (ecase (tree-map-find world position)
    (:wall nil)
    ((:empty :oxygen) t)))

(defun num-active-neighbours (cube cubes)
  (loop
     for neighbour in (get-neighbours cube)
     when (active-p neighbour cubes)
     summing 1))


(defun solve-15-2 (start compare-fn neighbour-fn)
  (let ((visited  (make-tree-map compare-fn))
	(frontier (make-amortized-queue)))
    
    (setf frontier (amortized-enqueue frontier (cons start 0)))
    
    (loop
       named outer
       with max-time = 0
       for (new-frontier current) = (multiple-value-list
				     (amortized-dequeue frontier))
       do
	 (setf frontier new-frontier)
	 (let ((current-name (car current))
	       (current-time (cdr current)))
	   (when (not (tree-map-find visited current-name nil))
	     (setf visited (tree-map-insert visited
					    current-name
					    current-time))
	     (when (<= max-time current-time)
	       (setf max-time current-time))
	     
	     (loop
		for neighbour in (funcall neighbour-fn
					  current-name)
		do (setf frontier
			 (amortized-enqueue frontier
					    (cons neighbour
						  (+ 1 current-time)))))))
	 while (not (amortized-queue-empty-p frontier))
	 finally (return-from outer max-time))))

(defun shortest-path (start end compare-fn neighbour-fn)
  (let ((visited  (make-tree-map compare-fn))
	(frontier (make-amortized-queue)))
    
    (setf frontier (amortized-enqueue frontier (cons start nil)))
    
    (loop
       named outer
       for (new-frontier current) = (multiple-value-list
				     (amortized-dequeue frontier))
       do
	 (setf frontier new-frontier)
	 (let ((current-name (car current))
	       (current-from (cdr current)))
	   (when (not (tree-map-find visited current-name nil))
	     (setf visited (tree-map-insert visited
					    current-name
					    current-from))
	     (when (= 0 (funcall compare-fn current-name end))
	       (return-from outer current-name))
	     
	     (loop
		for neighbour in (funcall neighbour-fn
					  current-name)
		do (setf frontier
			 (amortized-enqueue frontier
					    (cons neighbour
						  current-name)))))))
    (nreverse (loop
		 for cur = end then (tree-map-find visited cur)
		 for steps from 0
		 collect cur
		 until (= 0 (funcall compare-fn cur '(0 0)))))))

	      
       
;; Problem 16

(defun parse-file ()
  (one-or-more (parse-digit)))

(defun repeat-pattern (pattern)
  (let ((ret (copy-list pattern)))
    (setf (cdr (last ret)) ret)))

(defun expand-pattern (pattern n)
  (loop
     for x in pattern
     append (loop repeat n collect x)))

(defun combine (input pattern)
  (mod (abs (reduce #'+ (map 'list #'* input (cdr pattern)))) 10))

(defun solve-16 (input phases)
  (let* ((input (run-parser (parse-file) input))
	 (base-pattern '(0 1 0 -1)))
    (loop
       repeat phases
       do
	 (setf input
	       (loop
		  for i from 0 below (length input)
		  collect (combine input
				   (repeat-pattern (expand-pattern base-pattern
								   (+ i 1)))))))
    input))

(defun get-input-at-index (index input)
  (aref input (mod index (length input))))

(defun get-pattern-at-index (index expanded pattern)
  (aref pattern (floor (+ index 1) expanded)))

(defun solve-16 (input phases)
  (let* ((input (run-parser (parse-file) input))
	 (input (make-array (length input) :initial-contents input))
	 (base-pattern (make-array 4 :initial-contents '(0 1 0 -1))))
    (loop
     for start-idx from 5976277 (+ 5976277 8)
     do (loop
	 repeat phases
	 do (loop
	     for i from start-idx below (length input)
	     do (setf (aref input i)
		      (loop
		       with sum = 0
		       for j from i below (length input)
		       do (setf sum
				(mod (+ sum (* (get-input-at-index j input)
					       (get-pattern-at-index j
								     start-idx
								     base-pattern)))
				     10))
		       finally sum))))
			 
     (incf (aref input start-idx) 
	   collect (combine input
			    (repeat-pattern (expand-pattern base-pattern
							    (+ i 1)))))))
    input)

(defun solve-16-2 (input)
  (let* ((input (run-parser (parse-file) input))
	 (input (make-array (length input) :initial-contents input))	 
	 (start 0293510)
	 (end (* (length input) 10000) )
	 (phases 100)
	 (ret '())
	 )
    (loop
       with phase-values = (loop repeat phases collecting 0)
       for i from (- end 1) downto start
       do (setf phase-values (cdr (nreverse (reduce #'(lambda (acc cur)
				     (cons (mod (+ cur (car acc)) 10) acc))
				 phase-values
				 :initial-value (list (get-input-at-index i input))))))
	 (if (< i (+ start 8)) (setf ret (cons (elt phase-values 99) ret)))
       finally (return ret))))



;; Problem 17

(defun compare-list (pos-a pos-b)
  (labels ((list-sorter (list-a list-b)
	     (reduce #'(lambda (last cur) (if (= last 0) cur last))
		     (mapcar #'(lambda (a b) (- b a)) list-a list-b))))
  (let ((val (list-sorter pos-a pos-b)))
    (cond ((> val 0) 1)
	  ((< val 0) -1)
	  (t 0)))))

(defclass ascii-state ()
  ((pos       :initarg :pos       :initform '(0 0))
   (world     :initarg :world
	      :initform (tree-map-insert (make-tree-map #'compare-list)
					 '(0 0)
					 :empty))
	      
   (computer  :initarg :computer  :initform nil)))

(defmethod initialize-instance :after ((state ascii-state)
				       &key program-text template)
  (when program-text
    (setf (slot-value state 'computer)
	  (make-instance 'computer-state :program-text program-text)))
  (when template
    (loop
     for field in '(pos world computer)
     do (setf (slot-value state field) (slot-value template field)))))

(defun get-world-square (pos)
  (lambda (state)
    (with-slots (world) state
      (values (tree-map-find world pos :empty) state))))

(defun set-world-square (pos value)
  (lambda (state)
    (with-slots (world) state
      (let ((new-world (tree-map-insert world pos value))
	    (new-state (make-instance 'ascii-state
				      :template state)))
	(setf (slot-value new-state 'world) new-world)
	(values value new-state)))))

(defun get-pos ()
  (lambda (state)
    (with-slots (pos) state
      (values pos state))))

(defun set-pos (value)
  (lambda (state)
    (with-slots (pos) state
      (let ((new-state (make-instance 'ascii-state
				      :template state)))
	(setf (slot-value new-state 'pos) value)
	(values value new-state)))))

(defun square-in-direction (pos dir)
  (destructuring-bind (pos-x pos-y) pos
    (ecase dir
      (:up    (list pos-x (- pos-y 1)))
      (:down  (list pos-x (+ pos-y 1)))
      (:left  (list (- pos-x 1) pos-y))
      (:right (list (+ pos-x 1) pos-y)))))

(defun direction-to-number (direction)
  (ecase direction (:up 1) (:down 2) (:left 3) (:right 4)))


(defun run-program-ascii (program)
  (lambda (state)
    (with-slots ((computer-state computer)) state
      (multiple-value-bind (value new-computer-state)
	  (funcall program computer-state)
	(let ((new-state (make-instance 'ascii-state
					:template state)))
	  (setf (slot-value new-state 'computer) new-computer-state)
	  (values value new-state))))))

(defun get-world (program-text)
  (let ((computer (make-instance 'computer-state
			       :program-text program-text))
	(program
	 (with-state-monad
	   (run-until-halt)
	   (get-all-output))))
    (funcall program computer)))
;;    ))
(defun print-world (world)
  (format nil "~{~a~}" (mapcar #'code-char world)))
(defun intersection-p (x y width height world)
  (flet ((item-at (x y)
	   (elt (elt world y) x)))
    (and (< 0 x (- width 1))
	 (< 0 y (- height 1))
	 (every #'(lambda (x) (= (char-code #\#) (item-at (car x) (cdr x))))
		(list (cons x y)
		      (cons (1+ x) y)
		      (cons (1- x) y)
		      (cons x (1+ y))
		      (cons x (1- y)))))))

(defun get-intersections (world)
  (loop for i from 0 below (length (car world))
     append (loop for j from 0 below (length world)
	       when (intersection-p i j
				    (length (car world)) (length world)
				    world)
	       collect (list i j))))


(defun get-square (pos world)
  (if (and (<= 0 (first pos) (1- (length (car world))))
	   (<= 0 (second pos) (1- (length world))))
      (elt (elt world (second pos)) (first pos))
      nil))

(defun clockwise (dir)
  (ecase dir (:down :left) (:left :up) (:up :right) (:right :down)))

(defun anticlockwise (dir)
  (ecase dir (:down :right) (:right :up) (:up :left) (:left :down)))

(defun get-path (path pos dir world)
  (let ((straight (square-in-direction pos dir))
	(left (square-in-direction pos (anticlockwise dir)))
	(right (square-in-direction pos (clockwise dir))))
    
    (let ((straight-square (get-square straight world)))
      (if (and straight-square (= straight-square (char-code #\#)))
	  (get-path (cons :forward path) straight dir world)
	  (let ((left-square (get-square left world)))
	    (if (and left-square (= left-square (char-code #\#)))
		(get-path (cons :forward (cons :left path)) left (anticlockwise dir) world)
		(let ((right-square (get-square right world)))
		  (if (and right-square (= right-square (char-code #\#)))
		      (get-path (cons :forward (cons :right path)) right (clockwise dir) world)
		      (reverse path)))))))))

(defun collapse-path (path)
  (reverse
   (reduce #'(lambda (acc cur)
	       (cond 
		 ((find cur '(:right :left)) (cons cur acc))
		 ((and (listp (car acc))
		       (eq (caar acc) :forward))
		  (cons (cons :forward (1+ (cdar acc))) (cdr acc)))
		 (t (cons (cons :forward 1) acc))))
	   path
	   :initial-value '())))

(defun collapse (path template replacement)
  (let ((found-idx (search template path :test 'equal)))
    (if found-idx
	(append (subseq path 0 found-idx)
		replacement
		(collapse (subseq path (+ found-idx (length template)))
			  template
			  replacement))
	path)))

(defun collapse-all (path)
  (let ((templates
	 '(((:right (:forward . 8))
	    (:a))
	   ((:right (:forward . 12))
	    (:b))
	   ((:left (:forward . 8))
	    (:c))
	   ((:right (:forward . 6))
	    (:d))
	   ((:right (:forward . 4))
	    (:e))
	   ((:d :d) (:f))
	   ((:e :e) (:g))
	   ((:a :f :a) (:h))
	   ((:b :c :b) (:i))
	   ((:a :c :a) (:j))
	   ((:j :g) (:k))
	   )))
    (loop for (template replacement) in templates
       do (setf path (collapse path template replacement)))
    path))

(defun convert-input (input)
  (apply #'append
	 (mapcar #'(lambda (str)
		     (append (map 'list #'char-code str)
			     (list (char-code #\Newline))))
		 input)))

(defun solve-17-2 (program-text)
  (let* ((computer (make-instance 'computer-state
				  :program-text program-text))
	 (input '("A,B,A,B,C,C,B,C,B,A"
		  "R,12,L,8,R,12"
		  "R,8,R,6,R,6,R,8"
		  "R,8,L,8,R,8,R,4,R,4"
		  "n"))
	 (program
	  (with-state-monad
	    (set-memory 0 2)
	    (provide-input (convert-input input))
	    (run-until-halt)
	    (get-all-output))))
    (funcall program computer)))



;; i: r,12,l,8,r,12
;; h: r,8,r,6,r,6,r,8
;; k: r,8,l,8,r,8,r,4,r,4
;; main: i,h,i,h,k,k,h.k.h.i



;; Problem 18

(defparameter *input18* "#########
#b.A.@.a#
#########")

(defparameter *input18* "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################")

(defparameter *input18-2* "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

(ql:quickload "sycamore")
(defclass world-state ()
  ((map            :initarg :map            :initform nil)
   (keys           :initarg :keys           :initform nil)
   (doors          :initarg :doors          :initform nil)
   (vertices       :initarg :vertices       :initform nil)
   (edges          :initarg :edges          :initform nil)))

(defclass robot-state ()
  ((vertex         :initarg :vertex         :initform nil)
   (opened-doors   :initarg :opened-doors   :initform
		   (sycamore:make-tree-set #'sycamore-util:gsymbol-compare))))

(defmethod initialize-instance :after ((state robot-state)
				       &key template)
  (when template
    (loop
     for field in '(vertex opened-doors)
     do (setf (slot-value state field) (slot-value template field)))))

(defun move-to-vertex (destination robot)
  (with-slots (vertex opened-doors) robot
    (let ((new-robot (make-instance 'robot-state :template robot)))
      (setf (slot-value new-robot 'vertex) destination)
      (when (lower-case-p destination)
        (setf (slot-value new-robot 'opened-doors)
              (sycamore:tree-set-insert opened-doors (char-upcase destination))))
      new-robot)))

(defun collected-key-p (key robot)
  (sycamore:tree-set-member-p (slot-value robot 'collected-keys) key))

(defun get-square (position world)
  (with-slots (map) world
    (gethash position map '(:wall))))

(defun parse-file ()
  (parse-list
   (with-monad
     (one-or-more (either (parse-character "#.@")
                          (parse-character #'digit-char-p)
			  (parse-character #'upper-case-p)
			  (parse-character #'lower-case-p))))
   (parse-newline)))

(defun build-world (input)
  (let ((map (make-hash-table :test 'equal))
	(keys (make-hash-table))
	(doors (make-hash-table))
	(vertices (make-hash-table))
	(parsed (run-parser (parse-file) input)))
    (loop
       for y from 0 below (length parsed)
       for row in parsed
       do (loop
	     for x from 0 below (length row)
	     for char in row
	     do (setf (gethash (list x y) map)
		      (cond 
			((char= char #\#) '(:wall))
			((char= char #\.) '(:empty))
			((char= char #\@)
			 (setf (gethash char vertices) (list x y))
			 '(:empty))
                        ((digit-char-p char)
                         (setf (gethash char vertices) (list x y))
                         '(:empty))
			((lower-case-p char)
			 (setf (gethash char keys) (list x y))
			 (setf (gethash char vertices) (list x y))
			 `(:key ,char))
			((upper-case-p char)
			 (setf (gethash char doors) (list x y))
			 (setf (gethash char vertices) (list x y))
			 `(:door ,char))))))
    (let ((ret-world (make-instance 'world-state
			            :map map
			            :keys keys
			            :doors doors
			            :vertices vertices)))
      
      (setf (slot-value ret-world 'edges) (get-edges ret-world))
      ret-world)))

(defconstant +neighbours+ '((1 0) (-1 0) (0 1) (0 -1)))

(defun add-positions (a b)
  (map 'list #'+ a b))

(defun get-neighbours (position)
  (mapcar #'(lambda (diff) (add-positions position diff)) +neighbours+))

(defun traversable-p (position world)
  (let ((square (get-square position world)))
    (ecase (car square)
      ((:empty :key :door) t)
      (:wall nil))))

(defun traversable-neighbours (position world)
  (remove-if-not #'(lambda (pos) (traversable-p pos world))
	         (get-neighbours position)))

;; Returns hash table mapping position -> distance
(defun distance-to-squares (position world)
  "Find distance to all nearby squares (that aren't beyond a key or door)"
  (flet ((get-neighbour-fn (pos)
	   (let* ((square (get-square pos world))
		  (square-type (car square)))
	     (if (or (eq square-type :empty)
		     (and (find square-type '(:key :door))
			  (equal pos position)))
		 (traversable-neighbours pos world)
		 '()))))
    (shortest-paths position #'get-neighbour-fn :test 'equal)))

;; Returns list of (key/door distance) lists 
(defun distance-to-keys-doors (position world)
  "Find distance to all nearby keys and doors (that aren't beyond another)"
  (let ((distance-to-squares (distance-to-squares position world))
	(ret '()))
    (maphash
     #'(lambda (vertex vertex-position)
	 (when (and (not (equal vertex-position position))
		    (gethash vertex-position distance-to-squares))
	   (push (list vertex (gethash vertex-position distance-to-squares)) ret)))
     (slot-value world 'vertices))
    ret))

;; Returns hash table mapping key/door -> list of neighbouring (key/door dist)
(defun get-edges (world)
  "Find edge alist"
  (let ((vertices (slot-value world 'vertices))
	(edges (make-hash-table)))
    (maphash
     #'(lambda (vertex vertex-position)
	 (setf (gethash vertex edges)
               (distance-to-keys-doors vertex-position world)))
     vertices)
    edges))

;; Returns the shortest path to all vertices from vertex, and a set of all doors
;; that are along that path
(defun get-shortest-paths (vertex world)
  (let ((distance-to (make-hash-table))
        (parent-of (make-hash-table)))
    (flet ((vertex-fn (vertex parent distance)
             (setf (gethash vertex parent-of) parent)
             (when (or (char= #\@ vertex) (lower-case-p vertex))
               (let ((doors-between (sycamore:make-tree-set
                                     #'sycamore-util:gsymbol-compare)))
                 (loop for ancestor = vertex then (gethash ancestor parent-of)
                       until (not ancestor)
                       when (upper-case-p ancestor)
                       do (sycamore:tree-set-insertf doors-between ancestor))
                 (setf (gethash vertex distance-to) (list distance doors-between)))))
           (neighbour-fn (vertex)
	     (gethash vertex (slot-value world 'edges))))
      (dijkstra vertex #'vertex-fn #'neighbour-fn))
    distance-to))



(defun compare-robot (robot1 robot2)
  (with-slots ((v1 vertex) (doors1 opened-doors)) robot1
    (with-slots ((v2 vertex) (doors2 opened-doors)) robot2
      (sycamore-util:or-compare (sycamore-util:gsymbol-compare v1 v2)
                                (sycamore:tree-set-compare doors1 doors2)))))

(defun compare-state (state1 state2)
  (sycamore-util:or-compare (compare-lists (first state1) (first state2))
                            (sycamore:tree-set-compare (second state1) (second state2))))

(defun solve-18 (input)
  (let ((world (build-world input))
        (states (sycamore:make-tree-map #'compare-state))
        (shortest-paths (make-hash-table))
        (robots (loop for i below 4 collecting (code-char (+ i (char-code #\0))))))
    (loop for vertex being the hash-keys of (slot-value world 'vertices)
          do (setf (gethash vertex shortest-paths)
                   (get-shortest-paths vertex world)))
    (labels
        ((memoized-score (robots doors)
           (let ((state (list robots doors)))
             (if (sycamore:tree-map-contains states state)
	         (sycamore:tree-map-find states state)
	         (let ((result (score robots doors)))
		   (setf states (sycamore:tree-map-insert states
							  state
							  result))
		   result))))
         
         (best-robot-move (robot opened-doors)
           )
         (score (robots opened-doors)
           (loop for i below 4
                 for robot = (elt robots i)
                 for neighbours = (gethash robot shortest-paths)
                 for moved = nil
                 for min-dist = 
                    (loop for neighbour being the hash-keys of neighbours
                          using (hash-value (distance required-doors))
                          when (and (not (digit-char-p neighbour))
                                    (not (sycamore:tree-set-member-p opened-doors
                                                                     (char-upcase neighbour)))
                                    (sycamore:tree-set-subset-p required-doors
                                                                opened-doors))
                          minimize
                             (let ((new-robots (concatenate 'list
                                                            (subseq robots 0 i)
                                                            (list neighbour)
                                                            (subseq robots (1+ i))))
                                   (new-opened-doors
                                     (sycamore:tree-set-insert opened-doors
                                                               (char-upcase neighbour))))
                               (setf moved t)
                               (+ distance
                                  (memoized-score new-robots
                                                  new-opened-doors))))
                 when moved
                 minimize min-dist)))
      (memoized-score robots (sycamore:make-tree-set #'sycamore-util:gsymbol-compare)))))



(defparameter *input18-2* "#######
#a.#Cd#
##0#1##
#######
##2#3##
#cB#Ab#
#######")

(defparameter *input18-3* "###############
#d.ABC.#.....a#
######0#1######
###############
######2#3######
#b.....#.....c#
###############")


(defun solve-19 (program-text)
  (let* ((computer (make-instance 'computer-state
				  :program-text program-text)))
    (format t "~a"
            (loop for r below 50
                  sum (loop for c below 50
                            when (= 1 (let ()
                                        (funcall program computer)))
                            sum 1)))))


(defun solve-19-2 (program-text)
  (let ((computer (make-instance 'computer-state
                                 :program-text program-text))
        (bounds (make-hash-table ))
        (size 100))
    (flet ((get-beam-state (x y)
             (let ((program (with-state-monad                                
                              (provide-input x)
                              (provide-input y)
                              (run-until-output-or-halt)
                              (get-output))))
               (funcall program computer))))
      
      

      (loop for y from (+ 7 size) to 1500
            for start-x = (loop for x from 0
                                until (= 1 (get-beam-state x y))
                                finally (return x))
            then (loop for x from start-x
                       until (= 1 (get-beam-state x y))
                       finally (return x))
            for end-x = (1- (+ start-x size))
            for end-y = (1+ (- y size))
            until (= 1 (get-beam-state end-x end-y))
            finally (return (+ (* 10000 start-x) end-y))))))

(defparameter *input20*
"         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       ")
(defparameter *input20* "                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               ")

(defparameter *input20* "             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     ")
(defun parse-file ()
  (parse-list (one-or-more (either (parse-character "#. ")
                                   (parse-character #'upper-case-p)))
              (parse-newline)))


(defun find-vertical-portals (row-idx map &optional (portal-dir :down)
                                                    (col-start 0)
                                                    col-end)
  (let ((row (elt map row-idx)))
    (loop for c from col-start below (if (null col-end) (length row) col-end)
          for char = (elt row c)
          when (and  (upper-case-p char)
                     (upper-case-p (elt (elt map (1+ row-idx)) c)))
          collect (list (intern (format nil
                                        "~a~a"
                                        (elt row c)
                                        (elt (elt map (1+ row-idx)) c)))
                        (+ row-idx (if (eq portal-dir :down) 2 -1))
                        c))))

(defun find-horizontal-portals (col-idx map &optional (portal-dir :right)
                                                      (row-start 0)
                                                       row-end)
  (loop for r from row-start below (if (null row-end) (length map) row-end)
        for char = (elt (elt map r) col-idx)
        when (and (upper-case-p char)
                  (upper-case-p (elt (elt map r) (1+ col-idx))))
        collect (list (intern (format nil
                                      "~a~a"
                                      char
                                      (elt (elt map r) (1+ col-idx))))
                      r
                      (+ col-idx (if (eq portal-dir :right) 2 -1)))))

(defun outside-portals (map)
  (append
   (find-vertical-portals 0 map :down)
   (find-vertical-portals (- (length map) 2) map :up)
   (find-horizontal-portals 0 map :right)
   (find-horizontal-portals (- (length (car map)) 2) map :left)))

(defun inside-portals (map)
  (let* ((start-pos (loop named outer
                          for r from 2
                          do (loop for c from 2
                                   below (- (length (car map)) 2 1)
                                   for char = (elt (elt map r) c)
                                   when (char= #\Space char)
                                   do (return-from outer (list r c)))))
         (end-row (loop for r from (first start-pos)
                        for char = (elt (elt map r) (second start-pos))
                        when (or (char= #\# char) (char= #\. char))
                        do (return r)))
         (end-col (loop for c from (second start-pos)
                        for char = (elt (elt map (first start-pos)) c)
                        when (or (char= #\# char) (char= #\. char))
                        do (return c))))
    (append
     (find-vertical-portals (first start-pos) map :up
                            (second start-pos)
                            end-col)
     (find-vertical-portals (- end-row 2) map :down
                            (second start-pos)
                            end-col)
     (find-horizontal-portals (second start-pos) map :left
                              (first start-pos)
                              end-row)
     (find-horizontal-portals (- end-col 2) map :right
                              (first start-pos)
                              end-row))))

(defconstant +neighbours+ '((1 0) (-1 0) (0 1) (0 -1)))

(defun add-positions (a b)
  (map 'list #'+ a b))

(defun traversable-p (pos level map start end)
  (and (char= #\. (elt (elt map (first pos)) (second pos)))
       (or (= 0 level)
           (not (or (equal pos start)
                    (equal pos end))))))

(defun get-neighbours (position level map portals start end)
  
  (let ((ret (mapcar #'(lambda (diff)
                         (list (add-positions position diff) level))
                     +neighbours+)))
    (setf ret (remove-if-not #'(lambda (pos)
                                 (traversable-p (car pos)
                                                level
                                                map
                                                start
                                                end))
                             ret))
    (setf ret (remove-if #'(lambda (pos)
                             (let ((portal (gethash pos portals)))
                               (and portal
                                    (= level 0)
                                    (= (second portal) -1))))
                         ret))
    (let ((portal (gethash position portals)))
      (when (and portal
                 (or (> level 0)
                     (> (second portal) 0)))
        (push (list (first portal) (+ level (second portal))) ret)))

    ret))

(defun solve-20 (input)
  (let* ((map (run-parser (parse-file) input))
         (inside-portals (inside-portals map))
         (outside-portals (outside-portals map))
         (start-pos nil)
         (end-pos nil)
         (portals (make-hash-table :test 'equal)))
    (loop for (name . pos) in outside-portals
          for match = (assoc name inside-portals)
          do (when (eq name 'aa) (setf start-pos pos))
             (when (eq name 'zz) (setf end-pos pos))
             (when match
               (setf (gethash pos portals) (list (cdr match) -1))
               (setf (gethash (cdr match) portals) (list  pos 1))))
    (flet ((neighbour-fn (pos-level)
             (get-neighbours (first pos-level) (second pos-level) map portals start-pos end-pos)))
      (let ((paths (shortest-paths (list start-pos 0) #'neighbour-fn :test 'equal :end-vertex (list end-pos 0))))
        (gethash (list end-pos 0) paths)
        ))))

(defun get-computer (input)
  (let ((computer (make-instance 'computer-state
                                 :program-text input)))
    computer))


(defun solve21 (input)
  (let* ((computer (make-instance 'computer-state
                                  :program-text input))
         (ss-program "NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT A T
OR H T
AND T J
RUN
")
         (program (with-state-monad
                   (provide-input ss-program)
                   (run-until-halt)
                   (get-all-output :ascii t))))
    (execute-program computer program)))



(defun factory-deck (n)
  (let ((ret (fset:empty-seq)))
    (dotimes (i n)
      (fset:push-last ret i))
    ret))

(defun deal-new-stack (deck)
  (fset:reverse deck))

(defun cut-cards (n deck)
  (if (plusp n)
      (fset:concat (fset:subseq deck n (fset:size deck))
                   (fset:subseq deck 0 n))
      (let ((idx (+ (fset:size deck) n)))
        (fset:concat (fset:subseq deck idx (fset:size deck))
                     (fset:subseq deck 0 idx)))))

(defun deal-with-increment (n deck)
  (let* ((size (fset:size deck))
         (n-inv (invmod n size))
         (ret (fset:empty-seq)))
    (dotimes (i size)
      (fset:push-last ret (fset:lookup deck (mod  (* i n-inv) size))))
    ret))

(defun parse-file ()
  (parse-list (either (with-monad
                        (parse-string "deal with increment ")
                        (assign num (parse-number))
                        (unit `(:deal-with-increment ,num)))
                      (with-monad
                        (parse-string "cut ")
                        (assign num (parse-number))
                        (unit `(:cut-cards ,num)))

                      (with-monad
                        (parse-string "deal into new stack")
                        (unit `(:deal-into-new-stack))))
              (parse-newline)))



(defun solve-22 (y pow n input)
  (labels ((deal-into-new-stack ()
             (list -1 -1))
           (cut-cards (idx)
             (list (- idx) 1))
           (deal-with-increment (idx)
             (list 0 idx))

           (compose-functions (f g)
             (destructuring-bind (a b) f
               (destructuring-bind (c d) g
                 (list (mod (+ a (* b c)) n)
                       (mod (* b d) n)))))
           
           (square-function (f)
             (compose-functions f f))

           (apply-function (f x)
             (destructuring-bind (a b) f
               (mod (+ a (* b x)) n)))
           
           (get-inverse-function (f)
             (destructuring-bind (a b) f
               (let* ((inv-b (invmod b n)))
                 (list (mod (- n (* inv-b a)) n) inv-b))))

           (f ()
             (let ((fns (run-parser (parse-file) input)))
               (reduce
                #'(lambda (a b) (compose-functions b a))
                (mapcar #'(lambda (fn)
                            (ecase (car fn)
                              (:deal-into-new-stack
                               (deal-into-new-stack))
                              (:cut-cards
                               (cut-cards (second fn)))
                              (:deal-with-increment
                               (deal-with-increment (second fn)))))
                        fns)))))
    
    (let ((cur-fn (get-inverse-function (f))))
      (loop while (> pow 0)
            do (when (oddp pow) 
                 (setf y (apply-function cur-fn y)))
               (setf cur-fn (square-function cur-fn))
               (setf pow (floor pow 2)))
      y)))


;; Problem 23

(defun solve-23 (input)
  (let* ((num 50)
         (computers (make-array num)))
    (dotimes (i num)
      (setf (aref computers i)
            (make-instance 'computer-state :program-text input))
      (execute-program (aref computers i)
                       (with-state-monad
                         (provide-input i)
                         (run-until-input-or-halt))))

    (let ((messages (make-queue)))
      (dotimes (i num)
         (queue-push-backf `(,i (-1)) messages))

      (loop named outer
            with nat = ()
            while (not (queue-empty-p messages))
            do (loop while (not (queue-empty-p messages))
                     for (idx data) = (queue-pop-frontf messages)
                     do (if (= idx 255)
                            (progn
                              (when (equal data (car nat))
                                (return-from outer data))
                              (setf nat (cons data nat)))
                            (dolist (item data)
                              (execute-program (aref computers idx)
                                               (provide-input item)))))
            
               (dotimes (i num)
                 (let ((output (execute-program (aref computers i)
                                                (with-state-monad
                                                  (run-until-input-or-halt)
                                                  (get-all-output)))))
                   (loop while output
                         do (queue-push-backf `(,(first output)
                                                (,(second output)
                                                 ,(third output)))
                                              messages)
                            (setf data (nthcdr 3 output)))))
            
               (when (queue-empty-p messages)
                 (queue-push-backf `(0 ,(first nat)) messages))))))


;; Problem 24

(defun parse-file ()
  (parse-list (one-or-more (parse-character "#.")) (parse-newline)))

(defparameter *neighbours* '((1 0 0) (-1 0 0) (0 1 0) (0 -1 0)))

(defparameter *other-neighbours*
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (lower . uppers) in
             `(((1 2) . ,(loop for c below 5 collect `(0 ,c)))
               ((2 3) . ,(loop for r below 5 collect `(,r 4)))
               ((2 1) . ,(loop for r below 5 collect `(,r 0)))
               ((3 2) . ,(loop for c below 5 collect `(4 ,c)))) 
          do (dolist (upper uppers)
               (push (append upper '(1)) (gethash lower ht))
               (push (append lower '(-1)) (gethash upper ht))))
    ht))

(defun get-neighbours (bug)
  (let ((immediate-neighbours
          (mapcar #'(lambda (x) (map 'list #'+ bug x)) *neighbours*))
        (other-neighbours
          (mapcar #'(lambda (x) (list (first x)
                                      (second x)
                                      (+ (third x)
                                         (third bug))))
                  (gethash (subseq bug 0 2) *other-neighbours*))))
    (loop for neighbour in (append immediate-neighbours
                                   other-neighbours)
          when (and (<= 0 (first neighbour) 4)
                    (<= 0 (second neighbour) 4)
                    (not (and (= (first neighbour) 2)
                              (= (second neighbour) 2))))
          collect neighbour)))

(defun next (bugs)
  (let ((to-check (tree-set #'sycamore-util:gsymbol-compare))
        (new-bugs bugs))
    
    (do-tree-set (bug bugs)
      (tree-set-insertf to-check bug)
      (dolist (neighbour (get-neighbours bug))
        (tree-set-insertf to-check neighbour)))
    
    (do-tree-set (cur to-check)
      (let ((num-bugs (length
                       (remove-if #'(lambda (x)
                                      (not (tree-set-find bugs x)))
                                  (get-neighbours cur)))))
        
        (when (/= num-bugs 1)
          (setf new-bugs (tree-set-remove new-bugs cur)))
        (when (and (<= 1 num-bugs 2)
                   (not (tree-set-find bugs cur)))
          (setf new-bugs (tree-set-insert new-bugs cur)))))
    new-bugs))

(defun print-bugs (bugs &optional (l 0))
  (format t "~{~{~a~}~%~}"
          (loop for r below 5
                collect (loop for c below 5
                              collect (if (tree-set-find bugs (list r c l))
                                          #\#
                                          #\.)))))
(defun biodiversity (bugs)
  (loop with pow = 1
        with ret = 0
        for r below 5
        do (dotimes (c 5)
             (when (tree-set-find bugs (list r c))
               (incf ret pow))
             (setf pow (* pow 2)))
        finally (return ret)))

(defun solve-24 (input)
  (let ((bugs (tree-set #'sycamore-util:gsymbol-compare)))
    (loop for line in (run-parser (parse-file) input)
          for r from 0
          do (loop for char in line
                   for c from 0
                   do (when (char= char #\#)
                        (tree-set-insertf bugs (list r c 0)))))

    (loop
      repeat 200
      do (setf bugs (next bugs)))
    
    bugs))

;; Problem 25

(defun utils ()
  (dolist (pair (fours '("weather machine"
				"antenna"
				"easter egg"
				"fuel cell"
				"space law space brochure"
				"astrolabe")))
    (format t (command (format nil "take ~a~%take ~a~%take ~a~%take ~a~%south~%drop ~a~%drop ~a~%drop ~a~%drop ~a" 
		               (first pair)
		               (second pair)
		               (third pair)
		               (fourth pair)
		               (first pair)
		               (second pair)
		               (third pair)
		               (fourth pair)))))

  (defun command (input) 
    (push input *commands*)
    (execute-program *computer*
		     (with-state-monad
		       (provide-input input)
		       (provide-input 10)
		       (run-until-input-or-halt)
		       (get-all-output :ascii t))))

  (defun go-back ()
    (setf *commands* (cdr *commands*))
    (setf *computer* (make-instance 'computer-state :program-text
				    (get-problem 25 2019)))
    (format t "~a" (execute-program *computer*
				    (with-state-monad
				      (run-until-input-or-halt)
				      (get-all-output :ascii t))))
    (loop for command in (reverse *commands*)
	  do (format t "~a" (execute-program *computer* 
					     (with-state-monad
		  			       (provide-input command)
					       (provide-input 10)
					       (run-until-input-or-halt)
					       (get-all-output :ascii t)))))))
