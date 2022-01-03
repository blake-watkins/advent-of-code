(in-package :advent-of-code)

;;; UTILITY MACROS

(defmacro aif (expr then &optional else)
  `(let ((it ,expr))
     (if it
	 ,then
	 ,(when else else))))

(defmacro awhen (test &body body)
  "Just like when expect the symbol IT will be
  bound to the result of TEST in BODY."
  `(let ((it ,test))
     (when it
       ,@body)))


(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; Allow iterate macro to work over fset sets seqs and maps. Iterates over map keys.
  (defmacro-clause (for item in-fset set-seq-map)
    (with-gensyms (iterator)
      `(progn
         (with ,iterator)
         (initially (setf ,iterator (fset:iterator ,set-seq-map)))
         (for ,item next (if (funcall ,iterator :more?)
                             (funcall ,iterator :get)
                             (terminate))))))
  
  (defmacro-clause (for item in-fset-bag bag)
    (with-gensyms (iterator)
      `(progn
         (with ,iterator)
         (initially (setf ,iterator (fset:iterator ,bag :pairs? t)))
         (for ,item next (if (funcall ,iterator :more?)
                             (multiple-value-list (funcall,iterator :get))
                             (terminate)))))))

;;; FUNCTIONS FOR READING / WRITING FILES  & DOWNLOADING INPUT

(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun write-file (outfile output)
  (with-open-file (ostream outfile :direction :output
				   :if-does-not-exist :create
				   :if-exists :supersede)
    (format ostream "~a" output)))

(defun get-problem (day &optional (year 2020))
  (let ((filename (format nil "input~a" day)))
    (if (probe-file filename)
	(read-file filename)
	(let ((content (download-input day year)))
          (when content
	    (write-file filename content))
	  content))))


;;; COMBINATORICS 

(defun permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
              append (mapcar (lambda (l) (cons element l))
                             (permutations (remove element list)))))))

(defun pairs (list)
  (loop for (a . rest) on list
     nconc (loop for b in rest collect (list a b))))

(defun triples (list)
  (loop for (a . rest) on list
     nconc (loop for (b . rest2) on rest
	      nconc (loop for c in rest2 collect (list a b c)))))

(defun fours (list)
  (loop
    for (a . rest) on list
    nconc (loop
            for (b . rest2) on rest
            nconc (loop
                    for (c . rest3) on rest2
                    nconc (loop for d in rest3 collect (list a b c d))))))
;;; NUMBER THEORY

(defun egcd (a b)
  "Returns the gcd of a and b and two integers x and y such that ax + by = gcd."
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r))) 
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s))) 
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u))) 
       (q nil))
      ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))       
    (setq q (floor (/ (cdr r) (car r))))))                     

(defun chinese-remainder-theorem (&rest pairs)
  (reduce
   (lambda (pair-x pair-y)
     (destructuring-bind (a-x n-x) pair-x
       (destructuring-bind (a-y n-y) pair-y
         (multiple-value-bind (gcd m-x m-y) (egcd n-x n-y)
           (unless (= gcd 1)
             (error "Moduli are not coprime"))
           (let* ((n (* n-x n-y))
                  (a (+ (* a-x m-y n-y)
                        (* a-y m-x n-x))))
             (list (mod a n) n))))))
   pairs))

(defun invmod (a m)
  "Returns modular inverse of a mod m. Must be coprime."
  (multiple-value-bind (r s k) (egcd a m)
    (declare (ignore k))
    (unless (= 1 r) (error "invmod: Values ~a and ~a are not coprimes." a m))  
     (mod s m)))

;; https://rosettacode.org/wiki/Modular_exponentiation#Common_Lisp
(defun expt-mod (a n m)
  "Returns (mod (expt a n) m) efficiently. N should be non-negative integer"
  (loop with c = 1 while (plusp n) do
       (if (oddp n) (setf c (mod (* a c) m)))
       (setf n (ash n -1))
       (setf a (mod (* a a) m))
     finally (return c)))

(defun totient (n)
  "Returns the euler totient function - not optimized"
  (loop for i from 1 to n
     when (= 1 (gcd i n))
     sum 1))

(defun geometric-sum (a n m modulus)
  "Find geometric sum A^N + A^(N+1) + ... + A^(M-1) + A^M (mod MODULUS)"
  (labels ((geom-sum-int (a n modulus)
	     (mod  (cond ((< n 0) 0)
			 ((= n 0) 1)
			 ((= n 1) (+ a 1))
			 ((oddp n)
			  (* (+ a 1)
			     (geom-sum-int (expt-mod a 2 modulus)
					   (/ (- n 1) 2)
					   modulus)))
			 ((evenp n)
			  (+ (* (+ a 1)
				(geom-sum-int (expt-mod a 2 modulus)
					      (- (/ n 2) 1)
					      modulus))
			     (expt-mod a n modulus))))
		   modulus)))
    (mod (- (geom-sum-int a (max n m) modulus)
            (geom-sum-int a (- (min n m) 1) modulus))
         modulus)))


;; https://en.wikipedia.org/wiki/Baby-step_giant-step
;; n is a prime
;; alpha is the generator of the group
;; returns an x s.t (= beta (expt-mod alpha x n))
(defun baby-step-giant-step (n alpha beta)
  (let* ((m (ceiling (sqrt n)))
	 (store (make-hash-table :test 'eq)))
    (loop
       for j from 0 below m
       for alpha-pow-j = 1 then (mod (* alpha-pow-j alpha) n)
       do (setf (gethash alpha-pow-j store) j))
    (let ((alpha-neg-m (expt-mod (invmod alpha n) m n)))
      (loop
	 for i from 0 below m
	 for y = beta then (mod (* y alpha-neg-m) n)
	 until (gethash y store)
	 finally (return (+ (* i m) (gethash y store)))))))


;;; GRAPH ALGORITHMS


(defmacro-clause (for vertex
                      in-bfs-from start-vertex
                      neighbours neighbours-fn
                      &optional test (test 'eql) single (single nil))
  (with-gensyms ( next)
    `(progn
       (with ,next)
       (initially (setf ,next (breadth-first-search ,start-vertex
                                                    ,neighbours-fn
                                                    ,@(when test
                                                        `(:test ,test))
                                                    ,@(when single
                                                        `(:single ,single)))))
       (for ,vertex next (or (funcall ,next) (terminate))))))

;; Breadth First Search
(defun breadth-first-search (vertices neighbours-fn &key (test 'eql) (single nil) )
  (let ((visited (make-hash-table :test test))
	(frontier (make-queue)))
    (if (and (not single)
             (listp (car vertices)))
        (iter (for vertex in vertices)
              (setf frontier (queue-push-back (list vertex nil 0 vertex)
                                              frontier)))
        (setf frontier (queue-push-back (list vertices nil 0 vertices)
                                        frontier)))

    (labels ((add-neighbours (vertex distance root frontier)
               (queue-push-back
		(mapcar #'(lambda (v) (list v vertex (1+ distance) root))
			(funcall neighbours-fn vertex))
		frontier
		:as-list t))
             (next ()
               (loop while (not (queue-empty-p frontier))
                     do (multiple-value-bind (vertex-info new-frontier)
                            (queue-pop-front frontier)
                          (setf frontier new-frontier)
                          
	                  (destructuring-bind (vertex
                                               from-vertex
                                               distance
                                               root)
                              vertex-info
	                    (when (or (not (gethash vertex visited))
                                      (and (= distance
                                              (first (gethash vertex visited)))
                                           (not (find root
                                                      (second
                                                       (gethash vertex visited))
                                                      :test test))))
                              (if (not (gethash vertex visited))
                                  (setf (gethash vertex visited)
                                        (list distance (list root)))
                                  (destructuring-bind (distance roots)
                                      (gethash vertex visited)
                                    (setf (gethash vertex visited)
                                          (list distance (cons root roots)))))
                              
                              (setf frontier (add-neighbours vertex
                                                             distance
                                                             root
                                                             frontier))
                              (return-from next
                                (list vertex
                                      from-vertex
                                      distance
                                      root))))))))

      #'next)))

;; finds the shortest paths from vertex to all other vertices
;; neighbours-fn will be called with vertex and should return list of neighbours
(defun shortest-paths (vertex neighbours-fn &key (test 'eql) (end-vertex nil))
  (let ((distances (make-hash-table :test test)))
    (iterate (for (cur-vertex parent distance)
                  in-bfs-from vertex
                  neighbours neighbours-fn)
             (setf (gethash cur-vertex distances) distance)
             (until (and end-vertex (funcall test cur-vertex end-vertex))))
    distances))


;; Dijkstra's algorithm
;; vertex - starting vertex
;; vertex-fn - should be a function accepting three arguments: vertex, parent, distance. will be called on each
;;             reachable vertex
;; neighbours-fn - should be a function accepting a vertex and returning a list of neighbours and their distance
;;                 from the vertex
(defun dijkstra (vertex vertex-fn neighbours-fn)
  (let ((visited (fset:empty-set))
	(distance-to (fset:empty-map))
	(frontier (make-instance 'cl-heap:priority-queue)))
    (fset:includef distance-to vertex 0)
    (cl-heap:enqueue frontier (list vertex nil) 0)
    
    (loop until (= 0 (cl-heap:queue-size frontier))
	  for (current current-parent) = (cl-heap:dequeue frontier)
	  for current-distance = (fset:lookup distance-to current)
	  unless (fset:lookup visited current)
	  do 
	     (fset:includef visited current)
	     (funcall vertex-fn current current-parent current-distance)

	     (loop for (neighbour neighbour-distance)
		   in (funcall neighbours-fn current)
		   unless (fset:lookup visited neighbour)
		   do
		      (let ((tentative-distance (+ current-distance
						   neighbour-distance)))
			(cl-heap:enqueue frontier
					 (list neighbour current)
					 tentative-distance)
			    
			(if (or (not (fset:lookup distance-to neighbour))
				(< tentative-distance
				   (fset:lookup distance-to neighbour)))
			    (fset:includef distance-to neighbour tentative-distance)))))
    distance-to))

(defun a-star (vertex vertex-fn neighbours-fn heuristic-fn)
  (let ((visited (fset:empty-set))
	(g-score (fset:empty-map))
	(open-set (make-instance 'cl-heap:priority-queue)))
    (fset:includef g-score vertex 0)
    (cl-heap:enqueue open-set (list vertex nil) 0)
    
    (loop until (= 0 (cl-heap:queue-size open-set))
	  for (current current-parent) = (cl-heap:dequeue open-set)
	  for current-distance = (fset:lookup g-score current)
	  unless (fset:lookup visited current)
	  do
	     (fset:includef visited current)
	     (funcall vertex-fn current current-parent current-distance)

	     (loop for (neighbour neighbour-distance)
		   in (funcall neighbours-fn current)
		   unless (fset:lookup visited neighbour)
		   do
		      (let ((tentative-distance (+ current-distance
						   neighbour-distance)))

			(when (or (null (fset:lookup g-score neighbour))
				  (< tentative-distance
				     (fset:lookup g-score neighbour)))
			  (fset:includef g-score neighbour tentative-distance)
                          (cl-heap:enqueue open-set
					   (list neighbour current)
					   (+ tentative-distance
                                              (funcall heuristic-fn
                                                       neighbour)))))))
    nil))

(defun summed-area-table (fn max-dim)
  "MAX-DIM should be a number or a two element list. If a number, returns a square table of size MAX-DIM x MAX-DIM. If a two element list, returns a rectangular table of dimension MAX-DIM. The table contains the sum of all values of the function (FN R C) above and to the left of each square."
  (let* ((max-dim (if (numberp max-dim) (list max-dim max-dim) max-dim))
         (table (make-array max-dim :initial-element 0)))
    (flet ((get-val (r c)
             (if (and (<= 0 r (1- (first max-dim)))
                      (<= 0 c (1- (second max-dim))))
                 (aref table r c)
                 0)))
      
      (iter (for r below (first max-dim))
            (iter (for c below (second max-dim))
                  (let ((sum (+ (get-val r (1- c))
                                (get-val (1- r) c)
                                (- (get-val (1- r) (1- c)))
                                (funcall fn r c))))

                    (setf (aref table r c) sum))))
      table)))

(defun string-to-character-list (s)
  (iter (for c in-string s) (collect c)))

(defun character-list-to-string (cl)
  (format nil "~{~a~}" cl))

(defun digits-to-int (digits &key (base 2))
  (reduce #'(lambda (last cur) (+ (* base last) cur)) digits :initial-value 0))

(defun int-to-digits (n &key (base 2))
  (labels ((int-to-digits-rec (acc n)
	     (if (/= 0 n)
		 (int-to-digits-rec (cons (mod n base) acc) (floor n base))
		 acc)))
    (if (= n 0)
        '(0)
        (int-to-digits-rec '() n)
        )))

(defun manhattan (a b)
  (apply #'+ (map 'list #'(lambda (a1 b1) (abs (- a1 b1))) a b)))

;;; https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm
(defun edit-distance (a b &key (test #'eql))
  (let ((distance (make-array (list (1+ (length a)) (1+ (length b)))
                              :initial-element 0)))
    (iter
      (for i from 1 to (length a))
      (setf (aref distance i 0) i))
    (iter
      (for j from 1 to (length b))
      (setf (aref distance 0 j) j))
    (iter
      (for j from 1 to (length b))
      (iter
        (for i from 1 to (length a))
        (let ((substitution-cost
                (if (funcall test (elt a (1- i)) (elt b (1- j))) 0 1)))
          (setf (aref distance i j)
                (min (1+ (aref distance (1- i) j))
                     (1+ (aref distance i (1- j)))
                     (+ (aref distance (1- i) (1- j)) substitution-cost))))))
    (aref distance (length a) (length b))))

(defmethod print-object ((object hash-table) stream)
  (let ((*print-pretty* nil))
    (format stream "#HASH{~{~{(~S : ~S)~}~^ ~}}"
            (loop for key being the hash-keys of object
	       using (hash-value value)
	       collect (list key value)))))


(defun hash-table-from-alist (alist &key (test 'eql))
  (iter
    (with ret = (make-hash-table :test test))
    (for (key . val) in alist)
    (setf (gethash key ret) val)
    (finally (return ret))))

(defun hash-table-from-list-list (list-of-lists)
  (iter
    (with ret = (make-hash-table :test 'equal))
    (for r below (length list-of-lists))
    (iter
      (for c below (length (first list-of-lists)))
      (setf (gethash (list r c) ret)  (elt (elt list-of-lists r) c)))
    (finally (return ret))))

(defun hash-table-dimensions (hash-table)
  (with-hash-table-iterator (item hash-table)
    (iter
      (with ret = nil)
      (for (item-p key nil) = (multiple-value-list (item)))
      (while item-p)
      (setf ret (if (null ret) key (mapcar #'max ret key)))
      (finally (return ret)))))

(defun map-from-list-list (list-of-lists)
  (iter
    (with ret = (fset:empty-map))
    (for r below (length list-of-lists))
    (iter
      (for c below (length (first list-of-lists)))
      (fset:includef ret (list r c) (elt (elt list-of-lists r) c)))
    (finally (return ret))))

(defun map-dimensions (map)
  (iter
    (with max = (fset:arb map))
    (for pos in-fset map)
    (setf max (mapcar #'max pos max))
    (finally (return max))))




