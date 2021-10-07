(ql:quickload "split-sequence")


;; Problem 1

(defparameter *input1* "1721
979
366
299
675
1456")

(defun parse-file ()
  (parse-list (parse-number) (parse-newline)))

(defun solve-1 (input)
  (let* ((nums (run-parser (parse-file) input))
	 (pairs (pairs nums)))
    (mapcar #'(lambda (pair) (apply #'* pair))
	    (remove-if #'(lambda (pair) (/= 2020 (apply #'+ pair)))
		       pairs))))

(defun solve-1-2 (input)
  (let* ((nums (run-parser (parse-file) input))
	 (triples (triples nums)))
    (mapcar #'(lambda (triple) (apply #'* triple))
	    (remove-if #'(lambda (triple) (/= 2020 (apply #'+ triple)))
		       triples))))

    
;; Problem 2
(defparameter *input2* "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(defun parse-line ()
  (with-monad
    (assign start (parse-number))
    (parse-character #\-)
    (assign end (parse-number))
    (parse-space)
    (assign seek-char (parse-character #'lower-case-p))
    (parse-string ": ")
    (assign password (parse-characters #'lower-case-p))
    (unit (list password seek-char start end))))

(defun parse-file ()
  (parse-list (parse-line) (parse-newline)))

(defun test-password (password-rule)
  (destructuring-bind (password character range-start range-end) password-rule
    (let ((count (loop for c across password when (char= c character) sum 1)))
      (and (<= range-start count)
	   (<= count range-end)))))

(defun test-password-2 (password-rule)
  (destructuring-bind (password character char-pos-1 char-pos-2) password-rule
    (let ((a (char= character (char password (1- char-pos-1))))
	  (b (char= character (char password (1- char-pos-2)))))
      (if a (not b) b))))

(defun solve-2 (input)
  (loop for password-rule in (run-parser (parse-file) input)
     when (test-password-2 password-rule)
     sum 1))


;; Problem 3

(defparameter *input3* "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(defun parse-file ()
  (parse-list (parse-characters "#.") (parse-newline)))

(defun test-slope (slope lines)
  (let ((pos (list 0 0))
	(count 0)
	(width (length (first lines))))
    (loop 
       while (< (second pos) (length lines))
       do
	 (when (char= #\#
		      (char (nth (second pos) lines) (mod (first pos) width)))
	   (incf count))
	 (incf (first pos) (first slope))
	 (incf (second pos) (second slope)))
    count))

(defun solve-3 (input)
  (test-slope '(3 1) (run-parser (parse-file) input)))

(defun solve-3-2 (input)
  (let* ((course (run-parser (parse-file) input))
	 (slopes '((1 1) (3 1) (5 1) (7 1) (1 2)))
	 (trees (mapcar #'(lambda (slope) (test-slope slope course)) slopes)))
    (apply #'* trees)))


;; Problem 4

(defparameter *input4* "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022")

(defun parse-field ()
  (with-monad 
    (assign field-name (parse-characters #'lower-case-p))
    (parse-character #\:)
    (assign field-value (parse-characters (complement #'whitespace-char-p)))
    (unit (cons field-name field-value))))

(defun parse-passport ()
  (parse-list (parse-field) (either (parse-newline) (parse-space))))

(defun parse-file ()
  (parse-list (parse-passport) (parse-newline)))


(defun parse-passport (line)
  (let ((pp (make-hash-table :test 'equal)))
    (loop
       for pair in (split-sequence #\Space line)
       do (when (string/= "" pair)
	    (setf (gethash (subseq pair 0 3) pp) (subseq pair 4 (length pair)))))
    pp))

(defun parse-passports (filename)
  (with-open-file (stream filename)
    (let ((passports '())
	  (pp ""))
      (loop for line = (read-line stream nil)	 
	 while line
	 do (setf line (string-right-trim '(#\Return) line))
	   (if (string= "" line)
	       (progn (setf passports (cons (parse-passport pp) passports))
		      (setf pp ""))
	       (setf pp (concatenate 'string pp " " line)))
	 finally
	   (setf passports (cons (parse-passport pp) passports)))
      passports)))

(defun test-passport (pp)
  (every #'(lambda (required-field)
	     (nth-value 1 (gethash required-field pp)))
	 '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))

(defun test-passport2 (pp)
  (macrolet ((get-integer-field (field-name)
	       `(parse-integer (gethash ,field-name pp))))
    (and (every #'(lambda (required-field)
		    (nth-value 1 (gethash required-field pp)))
		'("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
	 (<= 1920 (get-integer-field "byr") 2002)
	 (<= 2010 (get-integer-field "iyr") 2020)
	 (<= 2020 (get-integer-field "eyr") 2030)
	 (let* ((hgt (gethash "hgt" pp))
		(hgt-type (subseq hgt (- (length hgt) 2) (length hgt)))
		(hgt-value (parse-integer (subseq hgt 0 (- (length hgt) 2)))))
	   (cond ((string= hgt-type "cm") (<= 150 hgt-value 193))
		 ((string= hgt-type "in") (<= 59 hgt-value 76))
		 (t nil)))
	 (and (string= "#" (subseq (gethash "hcl" pp) 0 1))
	      (every #'(lambda (char) (find char "0123456789abcdef" :test 'equal))
		     (subseq (gethash "hcl" pp) 1 7)))
	 (find (gethash "ecl" pp) '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test 'equal)
	 (and (= (length (gethash "pid" pp)) 9)
	      (every #'(lambda (char) (find char "0123456789" :test 'equal))
		     (gethash "pid" pp))))))
       
(defun test-field (field-name min max pp)
  (with-gensyms (field)
    `(let ((,field (parse-integer (gethash ,field-name ,pp))))
       (and (<= ,field ,max)
	    (>= ,field ,min)))))


;; Problem 5
(defun parse-seat-char ()
  (flet ((convert-char (char)
	   (ecase char (#\F 0) (#\B 1) (#\R 1) (#\L 0))))
    (with-monad
      (assign char (parse-character "FBRL"))
      (unit (convert-char char)))))

(defun parse-file ()
  (one-or-more
   (with-monad
     (assign digits (one-or-more (parse-seat-char)))
     (parse-newline)
     (unit (digits-to-int digits)))))

(defun solve-5 (input)
  (apply #'max (run-parser (parse-file) input)))

(defun solve-5-2 (input)
  (let ((seats (run-parser (parse-file) input)))
    (setf seats (sort seats #'<))
    (loop
       for i from (car seats)
       for seat in seats
       until (/= i seat)
       finally (return i))))

;;; Problem 6
(defun parse-person ()
  (parse-line (one-or-more (parse-lower-case))))

(defun parse-group ()
  (parse-line (one-or-more (parse-person))))

(defun parse-file ()
  (one-or-more (parse-group)))


(defun solve-6 (input)
  (let ((groups (run-parser (parse-file) input)))
    (reduce #'+
	    (mapcar #'(lambda (group) (length (reduce #'union group)))
		    groups))))

(defun solve-6-2 (input)
  (let ((groups (run-parser (parse-file) input)))
    (reduce #'+
	    (mapcar #'(lambda (group) (length (reduce #'intersection group)))
		    groups))))


;;;  Problem 7

(defun parse-adjective-phrase ()
  (with-monad
    (assign adjective (parse-word))
    (parse-space)
    (assign color (parse-word))
    (parse-space)
    (parse-word)
    (unit (concatenate 'string adjective " " color))))

(defun parse-number-type-of-bags ()
  (with-monad
    (assign number (parse-number))
    (parse-space)
    (assign adjective (parse-adjective-phrase))
    (unit (cons number adjective))))

(defun parse-bag-list ()
  (either (with-monad
	    (parse-string "no other bags")
	    (unit '()))
	  (parse-list (parse-number-type-of-bags) ", ")))

(defun parse-rule ()
  (with-monad
    (assign container-adjectives (parse-adjective-phrase))
    (parse-string " contain ")
    (assign contained-adjectives (parse-bag-list))
    (parse-character #\.)
    (unit (cons container-adjectives
		contained-adjectives))))

(defun parse-file ()
  (one-or-more (parse-line (parse-rule))))

(defparameter *input7* "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")


(defun solve-7 (input)
  (let ((results (run-parser (parse-file) input))
	(containers (make-hash-table :test 'equal)))
    (loop
       for (container . contents) in results
       do (loop
	     for content in contents
	     do (push container (gethash (cdr content) containers))))
    
    (let ((visited (make-hash-table :test 'equal)))
      (labels ((dfs (node)
		 (setf (gethash node visited) t)
		 (let ((immediate-parents (gethash node containers)))
		   (1+ (loop
			  for parent in immediate-parents
			  unless (gethash parent visited)
			  sum (dfs parent))))))
	(- (dfs "shiny gold") 1)))))

(defun solve-7-2 (input)
  (let ((results (run-parser (parse-file) input))
	(contents (make-hash-table :test 'equal))
	(memoized-number (make-hash-table :test 'equal)))
    (loop for rule in results
       do (setf (gethash (car rule) contents) (cdr rule)))
    (labels ((get-number (node)
	       (multiple-value-bind (memoized memoizedp)
		   (gethash node memoized-number)
		 (if memoizedp
		     memoized
		     (progn
		       (let ((number (loop for child in (gethash node contents)
					summing (* (car child)
						   (get-number (cdr child))))))
			 (setf (gethash node memoized-number) (+ 1 number))
			 (+ 1 number)))))))
      (- (get-number "shiny gold") 1))))
	       
(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))


;; Problem 8
;;provides switch
(ql:quickload "alexandria")
(use-package 'alexandria)
(defun parse-opcode ()
  (with-monad
    (assign opcode (parse-word))
    (unit (eswitch (opcode :test 'equal)
	    ("nop" 'nop)
	    ("acc" 'acc)
	    ("jmp" 'jmp)))))

(defun parse-instruction ()
  (with-monad
    (assign opcode (parse-opcode))
    (parse-space)
    (assign argument (parse-number))
    (unit (list opcode argument '()))))

(defun parse-file ()
  (zero-or-more (with-monad
		  (assign instruction (parse-instruction))
		  (parse-newline)
		  (unit instruction))))

(defparameter *input8* "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defun solve-8 (input)
  (let* ((contents (run-parser (parse-file) input))
	 (ram (make-array (list (length contents) 3)
			  :initial-contents contents))
	 (ip 0)
	 (acc 0))
    (loop while (not (aref ram ip 2))
       do
	 (setf (aref ram ip 2) t)
	 (ecase (aref ram ip 0)
	   (nop '())
	   (acc (incf acc (aref ram ip 1)))
	   (jmp (incf ip (- (aref ram ip 1) 1))))
	 (incf ip)
       finally
	 (return acc))))

(defun alter-idx (idx array)
  (setf (aref array idx 0)
	(case (aref array idx 0)
	  (nop 'jmp)
	  (acc 'acc)
	  (jmp 'nop)))
  array)

(defun solve-8-2 (input)
  (let* ((contents (run-parser (parse-file) input))
	 (init-ram (make-array (list (length contents) 3)
			       :initial-contents contents))
	 (init-ram-length (array-dimension init-ram 0)))
    (loop for idx to init-ram-length
       do
	 (multiple-value-bind (finalip finalacc)
	     (let ((ram (copy-array init-ram))
		   (ip 0)
		   (acc 0))
	       (setf ram (alter-idx idx ram))
	       (loop while (and (>= ip 0)
				(< ip init-ram-length)
				(not (aref ram ip 2)))
		  do
		    (setf (aref ram ip 2) t)
		    (ecase (aref ram ip 0)
		      (nop '())
		      (acc (incf acc (aref ram ip 1)))
		      (jmp (incf ip (- (aref ram ip 1) 1))))
		    (incf ip)
		  finally
		    (return (values ip acc))))
	   (when (= finalip init-ram-length)
	     (return finalacc))))))



;; Problem 9

(defconstant +preamble-length+ 25)
(defparameter *input9* "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defun parse-file ()
  (zero-or-more (with-monad
		  (assign number (parse-number))
		  (parse-newline)
		  (unit number))))

(defun get-contents-array (input)
  (let* ((contents (run-parser (parse-file) input))
	 (contents (make-array (length contents) :initial-contents contents)))
    contents))

(defun test-idx (idx array)
  (let ((ret nil))
    (loop named outer
       for a from (- idx +preamble-length+) to idx
       do (loop for b from (+ a 1) to idx
	     do (when (= (aref array idx)
			 (+ (aref array a) (aref array b)))
		  (setf ret t)
		  (return-from outer))))
    ret))

(defun solve-9 (input)
  (let ((contents (get-contents-array input))
	(contents-len (array-dimension contents 0)))
    (loop
       for idx from +preamble-length+ to contents-len
       while (test-idx idx contents)
       finally (return (aref contents idx)))))

(defun sum-array (minidx maxidx array)
  (loop for idx from minidx to maxidx summing (aref array idx)))

(defparameter *target* 530627549)

(defun solve-9-2 (input)
  (let ((contents (get-contents-array input)))
    (loop
       with a = 0
       with b = 0
       for sum = (sum-array a b contents)
       until (= sum *target*)
       do
	 (cond ((< sum *target*) (incf b))
	       ((> sum *target*) (incf a)))
       finally
	 (multiple-value-bind (max min)
	     (loop for idx from a to b
		maximizing (aref contents idx) into max
		minimizing (aref contents idx) into min
		finally (return (values max min)))
	   (return (+ max min))))))



;;Puzzle 10

(defparameter *input10* "16
10
15
5
1
11
7
19
6
12
4")

(defun parse-file (input)
  (run-parser (one-or-more (with-monad
			     (assign number (parse-number))
			     (parse-newline)
			     (unit number)))	 
	      input))

(defun solve-10-visitor (last cur)
  (destructuring-bind (last-val differences) last
    (ecase (- cur last-val)
      ((1 2 3) (incf (nth (- (- cur last-val) 1) differences))))
    (list cur differences)))
      
(defun solve-10 (input)
  (let ((adapters (parse-file input)))
    (setf adapters (sort adapters #'<))
    (let ((differences (nth 1 (reduce #'solve-10-visitor
				      adapters
				      :initial-value (list 0 (list 0 0 0))))))
      (* (first differences)
	 (+ 1 (third differences))))))

(defun solve-10-2 (input)
  (let* ((adapters (parse-file input))
	 (max-adapter (+ (loop for x in adapters maximizing x) 3))
	 (paths-to (make-hash-table)))
    (setf adapters (cons max-adapter adapters))
    (setf adapters (sort adapters #'<))
    (setf (gethash 0 paths-to) 1)
    (loop for x in adapters
       do (let ((number-to-here (loop for y from (- x 3) to x
				   when (nth-value 1 (gethash y paths-to))
				   sum (gethash y paths-to))))
	    (setf (gethash x paths-to) number-to-here)))
    (gethash max-adapter paths-to)))


;; Problem 11

(defparameter *input11* "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")
(defconstant +neighbours+ '((-1 -1) (0 -1) (1 -1)
			    (-1  0)        (1  0)
			    (-1  1) (0  1) (1  1)))

(defun parse-square ()
  (with-monad
    (assign char (parse-character "L.#"))
    (unit (ecase char
	    (#\L :unoccupied)
	    (#\. :floor)
	    (#\# :occupied)))))

(defun parse-file ()
  (one-or-more (with-monad
		  (assign row (one-or-more (parse-square)))
		  (parse-newline)
		  (unit row))))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun count-occupied-neighbours (x y array)
  (destructuring-bind (width height) (array-dimensions array)
    (loop
       for neighbour in +neighbours+
       for neighbour-x = (+ (first neighbour) x)
       and neighbour-y = (+ (second neighbour) y)
       when (and (<= 0 neighbour-x (- width 1))
		 (<= 0 neighbour-y (- height 1))
		 (eq (aref array neighbour-x neighbour-y) :occupied))
       sum 1)))

;;new rules
(defun count-occupied-neighbours (x y array)
  (loop
     for (neighbour-x neighbour-y) in +neighbours+
     when (eq :occupied (first-visible-seat x y
					    neighbour-x neighbour-y
					    array))
     sum 1))

(defun first-visible-seat (x y direction-x direction-y array)
  (destructuring-bind (width height) (array-dimensions array)
    (loop
       with ret = :floor
       for view-x = (+ x direction-x) then (+ view-x direction-x)
       for view-y = (+ y direction-y) then (+ view-y direction-y)
       while (and (<= 0 view-x (- width 1))
		  (<= 0 view-y (- height 1))
		  (eq (aref array view-x view-y) :floor))
       finally
	 (when (and (<= 0 view-x (- width 1))
		    (<= 0 view-y (- height 1)))
	   (setf ret (aref array view-x view-y)))
	 (return ret))))
	
	 
(defun count-all-occupied-seats (array)
  (destructuring-bind (width height) (array-dimensions array)
    (loop for x from 0 below width
       summing (loop for y from 0 below height
		  summing (if (eq (aref array x y) :occupied) 1 0)))))

(defun next-seat-state (x y array)
  (let ((occupied-count (count-occupied-neighbours x y array))
	(state (aref array x y)))
    (case state
      (:unoccupied (when (= occupied-count 0)
		     (setf state :occupied)))
      (:occupied (when (>= occupied-count 5) ;new-rules
		   (setf state :unoccupied))))
    state))

(defun next-state (state)
  (let ((next-state (copy-array state)))
    (destructuring-bind (width height) (array-dimensions state)
      (let ((changed nil))
	(loop for x from 0 below width
	   do (loop for y from 0 below height
		 do
		   (setf (aref next-state x y) (next-seat-state x y state))
		   (when (not (eq (aref next-state x y)
				  (aref state x y)))
		     (setf changed t))))
	(values next-state changed)))))

(defun solve-11 (input)
  (let ((state (list-to-2d-array (run-parser (parse-file) input))))
    (loop
       for (next-state changed) = (multiple-value-list (next-state state))
       while changed
       do
	 (setf state next-state)
       finally
	 (return (count-all-occupied-seats state)))))




;; Problem 12

(defparameter *input12* "F10
N3
F7
R90
F11")

(defun parse-navigation ()
  (either (with-monad (assign move-direction (parse-character "NSEWF"))
		      (assign move-amount (parse-number))
		      (unit (cons (ecase move-direction
				    (#\N :north)
				    (#\S :south)
				    (#\E :east)
				    (#\W :west)
				    (#\F :forward))
				  move-amount)))
	  (with-monad (assign turn-direction (parse-character "LR"))
		      (assign turn-amount (parse-number))
		      (unit (cons (ecase turn-direction
				    (#\L :left)
				    (#\R :right))
				  (floor turn-amount 90))))))
(defun parse-file ()
  (one-or-more (with-monad (assign navigation (parse-navigation))
			   (parse-newline)
			   (unit navigation))))

(defun move (pos amount direction)
  (destructuring-bind (x y) pos
    (ecase direction
      (:north (list x (- y amount)))
      (:south (list x (+ y amount)))
      (:east  (list (+ x amount) y))
      (:west  (list (- x amount) y)))))

(defun rotate-waypoint (point centre amount direction)
  (flet ((rotate-one (point direction)
	   (ecase direction
	     (:right (list (- (second point)) (first point)))
	     (:left  (list (second point) (- (first point)))))))
    (let ((difference (map 'list #'- point centre)))
      (loop
       for new-difference = difference then (rotate-one new-difference direction)
       repeat amount
       finally (return (map 'list #'+ new-difference centre))))))
   

(defun turn (heading amount direction)
  (flet ((turn-one ( heading direction)
	   (ecase direction
	     (:left  (ecase heading
		       (:north :west)
		       (:west  :south)
		       (:south :east)
		       (:east  :north)))
	     (:right (ecase heading
		       (:north :east)
		       (:east  :south)
		       (:south :west)
		       (:west  :north))))))
    (loop
       for new-heading = heading then (turn-one new-heading direction)
       repeat amount
       finally (return new-heading))))
    
(defun solve-12 (input)
  (loop
     with pos = '(0 0)
     with direction = :east
     for instruction in (run-parser (parse-file) input)
     do (ecase (car instruction)
	  ((:north :south :west :east)
	   (setf pos (move pos (cdr instruction) (car instruction))))
	  (:forward
	   (setf pos (move pos (cdr instruction) direction)))
	  ((:left :right)
	   (setf direction (turn direction
				 (cdr instruction)
				 (car instruction)))))
     finally
       (return (manhattan pos))))

(defun solve-12-2 (input)
  (loop
     with pos = '(0 0)
     with waypoint = '(10 -1)
     for instruction in (run-parser (parse-file) input)
     do (ecase (car instruction)
	  ((:north :south :west :east)
	   (setf waypoint (move waypoint (cdr instruction) (car instruction))))
	  ((:left :right)
	   (setf waypoint (rotate-waypoint waypoint
					   '(0 0)
					   (cdr instruction)
					   (car instruction))))
	  (:forward
	   (setf pos (map 'list
			  #'(lambda (p w) (+ p (* w (cdr instruction))))
			  pos
			  waypoint))))
       finally (return (manhattan pos))))

(defun manhattan (pos)
  (reduce #'(lambda (x y) (+ x (abs y))) pos :initial-value 0))
	     
	 
;;; Problem 13

(defun parse-bus-id ()
  (either (with-monad
	    (assign id (parse-number))
	    (unit (list id)))
	  (with-monad
	    (parse-character #\x)
	    (unit '()))))

(defun parse-file ()
  (with-monad
    (assign earliest-departure (parse-number))
    (parse-newline)
    (assign first (parse-bus-id))
    (assign rest (one-or-more (then (parse-character #\,)
				    (parse-bus-id))))
    (unit (list earliest-departure (append first (apply #'append rest))))))


(defun solve-12 (input)
  (let* ((parsed (run-parser (parse-file) input))
	 (departure-time (car parsed))
	 (ids (cadr parsed))
	 (ids-waits (mapcar #'(lambda (id)
				(list id
				      (- id
					 (nth-value 1 (floor departure-time
							     id)))))
			    ids)))
    (apply #'* (reduce #'(lambda (a b) (if (< (second b) (second a)) b a)) ids-waits))))

(defun egcd (a b)
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r))) ; (r+1 r) i.e. the latest is first.
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s))) ; (s+1 s)
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u))) ; (t+1 t)
       (q nil))
      ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))       ; exit when r+1 = 0 and return r s t
    (setq q (floor (/ (cdr r) (car r))))))                     ; inside loop; calculate the q

(defun crt (a1 n1 a2 n2)
  (multiple-value-bind (ign m1 m2) (egcd n1 n2)
    (declare (ignore ign))
    (list (mod (+ (* a1 m2 n2) (* a2 m1 n1)) (* n1 n2))
	  (* n1 n2))))

(defun parse-string (input)
  (loop
     for x in (split-sequence:split-sequence #\, input)
     for idx from 0
     when (string/= x "x")
     collect (list idx (parse-integer x))))

(defun solve-13-2 (input)
  (car (reduce #'(lambda (last cur)
		   (apply #'crt (append last (list (- (second cur)
						      (first cur))
						   (second cur))))) 
	       (parse-string input))))


;; Problem 14

(defparameter *input14* "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(defun parse-mask ()
  (flet ((map-character (c) (ecase c
			      (#\1 1)
			      (#\0 0)
			      (#\X :x))))
    (with-monad (parse-string "mask")
		(parse-space)
		(parse-character #\=)
		(parse-space)
		(assign chars (one-or-more (parse-character "10X")))
		(unit (list :mask (mapcar #'map-character chars))))))

(defun parse-mem ()
  (with-monad (parse-string "mem")
	      (parse-character #\[)
	      (assign address (parse-number))
	      (parse-character #\])
	      (parse-space)
	      (parse-character #\=)
	      (parse-space)
	      (assign value (parse-number))
	      (unit (list :mem address value))))

(defun parse-file ()
  (one-or-more (with-monad
		 (assign line (either (parse-mask)
				      (parse-mem)))
		 (parse-newline)
		 (unit line))))

(defun number-to-bits (n)
  (labels ((number-to-bits-iter (n bit-length acc)
	     (if (= 0 bit-length)
		 acc
		 (multiple-value-bind (quotient remainder) (floor n 2)
		   (number-to-bits-iter quotient
					(- bit-length 1)
					(cons remainder acc))))))
    (number-to-bits-iter n 36 '())))

(defun bits-to-number (bits)
  (reduce #'(lambda (prev cur) (+ (* 2 prev) cur))
	  bits
	  :initial-value 0))

(defun mask-bits (bits mask)
  (flet ((mask-bit (bit maskbit)
	   (ecase maskbit
	     (:x bit)
	     ((0 1) maskbit))))
    (map 'list #'mask-bit bits mask)))

(defun solve-14 (input)
  (let ((memory (make-hash-table))
	(mask '()))
    (loop
       for instruction in (run-parser (parse-file) input)
       do
	 (ecase (car instruction)
	   (:mask (setf mask (second instruction)))
	   (:mem (setf (gethash (second instruction) memory)
		       (bits-to-number (mask-bits (number-to-bits (third instruction))
						  mask))))))
    (loop
       for v being each hash-value of memory
       summing v)))

(defun solve-14-2 (input)
  (let ((memory (make-hash-table))
	(mask '()))
    (loop
       for instruction in (run-parser (parse-file) input)
       do
	 (ecase (car instruction)
	   (:mask (setf mask (second instruction)))
	   (:mem
	    (mapcar #'(lambda (addr) (setf (gethash addr memory)
					   (third instruction)))
		    (bits-to-numbers (number-to-bits (second instruction))
				     mask)))))

    (loop
       for v being each hash-value of memory
       summing v)))
  
;;New rules
(defun bits-to-numbers (bits mask)
  (labels ((set-bit (bit prev)
	     (mapcar #'(lambda (x) (+ (* 2 x) bit)) prev))
	   (bits-to-numbers-iter (bits mask acc)
	     (if (null mask)
		 acc
		 (let* ((new-acc (ecase (car mask)
				   (0 (set-bit (car bits) acc)) 
				   (1 (set-bit 1 acc))
				   (:x (append
					(set-bit 0 acc)
					(set-bit 1 acc))))))
		   (bits-to-numbers-iter (cdr bits) (cdr mask) new-acc)))))
    
    (bits-to-numbers-iter bits mask '(0))))
					 

;; Problem 15

(defun parse-number-list (&optional (separator #\,))
  (one-or-more (with-monad
		 (either (parse-character separator)
			 (unit t))
		 (parse-number))))
(defun parse-file ()
  (parse-number-list))

(defun compare (a b)
  (cond ((< a b) -1)
	((= a b) 0)
	(t 1)))


(defun solve-15 (n input)
  (let ((start-numbers (run-parser (parse-file) input))
	(seen-at (make-tree-map #'compare)))
    (flet ((speak-number-at (n turn)
	     (multiple-value-bind (v k s) (tree-map-find seen-at n '())
	       (declare (ignore k))
	       (setf seen-at (tree-map-insert seen-at n (list turn (car v))))
	       (not s))))
      (let ((last-number 0)
	    (first-time-spoken nil))
	(loop
	   for i from 1
	   for num in start-numbers
	   do
	     (setf last-number num)
	     (setf first-time-spoken (speak-number-at num i)))
	(loop
	   for i from (+ 1 (length start-numbers)) to n
	   do
	     (if first-time-spoken
		 (progn
		   (setf last-number 0)
		   (setf first-time-spoken (speak-number-at 0 i)))
		 (let ((spoken-at (tree-map-find seen-at last-number)))
		   (setf last-number (- (car spoken-at) (second spoken-at)))
		   (setf first-time-spoken (speak-number-at last-number i))))
	   finally (return last-number))))))

    
;; Problem 16

(defparameter *input16* "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(defun parse-range ()
  (with-monad 
    (assign start (parse-number))
    (parse-string "-")
    (assign end (parse-number))
    (unit (cons start end))))

(defun parse-ranges ()
  (with-monad
    (assign rangea (parse-range))
    (parse-string " or ")
    (assign rangeb (parse-range))
    (unit (list rangea rangeb))))

(defun parse-field ()
  (with-monad
    (assign field-name (parse-characters #'(lambda (x) (char/= x #\:))))
    (parse-string ": ")
    (assign field-ranges (parse-ranges))
    (unit (cons field-name field-ranges))))

(defun parse-fields ()
  (one-or-more (with-monad
		 (assign field (parse-field))
		 (parse-newline)
		 (unit field))))

(defun parse-ticket ()
  (parse-number-list))

(defun parse-tickets ()
  (one-or-more (with-monad
		 (assign ticket (parse-ticket))
		 (parse-newline)
		 (unit ticket))))

(defun parse-file ()
  (with-monad
    (assign fields (parse-fields))
    (parse-characters #'(lambda (x) (char/= x #\y)))
    (parse-string "your ticket:")
    (parse-newline)
    (assign my-ticket (parse-ticket))
    (parse-characters #'(lambda (x) (char/= x #\n)))
    (parse-string "nearby tickets:")
    (parse-newline)
    (assign tickets (parse-tickets))
    (unit (cons fields (cons my-ticket tickets)))))

(defun value-valid-for-field (value field)
  (some #'(lambda (x) (<= (car x) value (cdr x))) (cdr field)))

(defun value-valid-for-fields (value fields)
  (remove-if #'(lambda (field)
		 (not (value-valid-for-field value field)))
	     fields))

(defun invalid-values (ticket fields)
  (remove-if #'(lambda (value)
		 (some #'(lambda (field) (value-valid-for-field value field))
		       fields))
	     ticket))

(defun invalid-ticket-p (ticket fields)
  (some #'(lambda (value)
	    (every #'(lambda (field)
		       (not (value-valid-for-field value field)))
		   fields))
	ticket))
		 
(defun solve-16 (input)
  (destructuring-bind (fields &rest tickets)
      (run-parser (parse-file) input)
    (loop
       for ticket in tickets
       summing (apply #'+ (invalid-values ticket fields)))))

(defun get-possible-fields (tickets fields)
  (let ((possible-fields 
	 (loop
	    for i from 0 to (- (length (car tickets)) 1)
	    collect (mapcar #'car fields))))
    (loop
       for ticket in tickets
       do
	 (let* ((possible-fields-for-ticket
		 (mapcar #'(lambda (value)
			     (mapcar #'car
				     (value-valid-for-fields value fields)))
			 ticket))
		(new-possible-fields
		 (map 'list
		      #'(lambda (a b) (intersection a b :test 'equal))
		      possible-fields-for-ticket
		      possible-fields)))
	   (setf possible-fields new-possible-fields))
       finally (return possible-fields))))

(loop repeat (length possible-fields) collect nil)

(defun find-actual-fields (actual-fields possible-fields)
  (setf actual-fields
	(map 'list
	     #'(lambda (actual possible)
		 (if actual
		     actual
		     (if (= (length possible) 1) (car possible) nil)))
	     actual-fields
	     possible-fields))
  (values actual-fields
	  (mapcar #'(lambda (x) (set-difference x actual-fields))
		  possible-fields)))

(defun solve-16-2 (input)
  (destructuring-bind (fields my-ticket &rest tickets)
      (run-parser (parse-file) input)
    (setf tickets (remove-if #'(lambda (ticket)
				 (invalid-ticket-p ticket fields))
			     tickets))
    (setf tickets (cons my-ticket tickets))

    (let* ((possible-fields (get-possible-fields tickets fields))
	   (actual-fields
	    (loop repeat (length possible-fields) collect nil)))
      (loop
	 repeat (length my-ticket)
	 do
	   (multiple-value-bind (new-actual-fields new-possible-fields)
	       (find-actual-fields actual-fields
				   possible-fields)
	     (setf actual-fields new-actual-fields)
	     (setf possible-fields new-possible-fields))
	 finally (return actual-fields))
      (apply #'*
	     (loop
		for field-name in actual-fields
		for field-value in my-ticket
		when (and (>= (length field-name) 9)
			  (string= (subseq field-name 0 9) "departure"))
		collect field-value)))))
				   


;; Problem 17

(defparameter *input17* ".#.
..#
###")

(defconstant +neighbours+ '((-1 -1 -1) (-1 -1 0) (-1 -1 1) (-1 0 -1) (-1 0 0) (-1 0 1) (-1 1 -1) (-1 1 0) (-1 1 1) (0 -1 -1) (0 -1 0) (0 -1 1) (0 0 -1) (0 0 1) (0 1 -1) (0 1 0) (0 1 1) (1 -1 -1) (1 -1 0) (1 -1 1) (1 0 -1) (1 0 0) (1 0 1) (1 1 -1) (1 1 0) (1 1 1)))

(defconstant +neighbours+ '((-1 -1 -1 -1) (-1 -1 -1 0) (-1 -1 -1 1) (-1 -1 0 -1) (-1 -1 0 0) (-1 -1 0 1)
 (-1 -1 1 -1) (-1 -1 1 0) (-1 -1 1 1) (-1 0 -1 -1) (-1 0 -1 0) (-1 0 -1 1)
 (-1 0 0 -1) (-1 0 0 0) (-1 0 0 1) (-1 0 1 -1) (-1 0 1 0) (-1 0 1 1)
 (-1 1 -1 -1) (-1 1 -1 0) (-1 1 -1 1) (-1 1 0 -1) (-1 1 0 0) (-1 1 0 1)
 (-1 1 1 -1) (-1 1 1 0) (-1 1 1 1) (0 -1 -1 -1) (0 -1 -1 0) (0 -1 -1 1)
 (0 -1 0 -1) (0 -1 0 0) (0 -1 0 1) (0 -1 1 -1) (0 -1 1 0) (0 -1 1 1)
 (0 0 -1 -1) (0 0 -1 0) (0 0 -1 1) (0 0 0 -1) (0 0 0 1) (0 0 1 -1) (0 0 1 0)
 (0 0 1 1) (0 1 -1 -1) (0 1 -1 0) (0 1 -1 1) (0 1 0 -1) (0 1 0 0) (0 1 0 1)
 (0 1 1 -1) (0 1 1 0) (0 1 1 1) (1 -1 -1 -1) (1 -1 -1 0) (1 -1 -1 1)
 (1 -1 0 -1) (1 -1 0 0) (1 -1 0 1) (1 -1 1 -1) (1 -1 1 0) (1 -1 1 1)
 (1 0 -1 -1) (1 0 -1 0) (1 0 -1 1) (1 0 0 -1) (1 0 0 0) (1 0 0 1) (1 0 1 -1)
 (1 0 1 0) (1 0 1 1) (1 1 -1 -1) (1 1 -1 0) (1 1 -1 1) (1 1 0 -1) (1 1 0 0)
			    (1 1 0 1) (1 1 1 -1) (1 1 1 0) (1 1 1 1)))

(defun parse-square ()
  (with-monad
    (assign char (parse-character ".#"))
    (unit (ecase char
	    (#\. :inactive)
	    (#\# :active)))))

(defun parse-file ()
  (one-or-more (with-monad
		  (assign row (one-or-more (parse-square)))
		  (parse-newline)
		  (unit row))))

(defun add-cubes (a b)
  (map 'list #'+ a b))

(defun get-neighbours (cube)
  (mapcar #'(lambda (diff) (add-cubes cube diff)) +neighbours+))

(defun active-p (cube cubes)
  (tree-set-find cubes cube))

(defun num-active-neighbours (cube cubes)
  (loop
     for neighbour in (get-neighbours cube)
     when (active-p neighbour cubes)
     summing 1))
		       
;;compare whether two lists differ, compares elementwise until a <, = or > b
(defun compare (pos-a pos-b)
  (labels ((list-sorter (list-a list-b)
	     (reduce #'(lambda (last cur) (if (= last 0) cur last))
		     (mapcar #'(lambda (a b) (- b a)) list-a list-b))))
  (let ((val (list-sorter pos-a pos-b)))
    (cond ((> val 0) 1)
	  ((< val 0) -1)
	  (t 0)))))


(defun step-program (active)
  (let ((new-active (make-tree-set #'compare)))
    (map-tree-set nil
		  #'(lambda (square)
		      (case (num-active-neighbours square active)
			((2 3) (setf new-active
				     (tree-set-insert new-active square)))))
		  active)
			  
    (map-tree-set nil
    		  #'(lambda (square)
    		      (let ((neighbours (get-neighbours square)))
    			(loop
    			   for neighbour in neighbours
    			   do (when (and (not (active-p neighbour active))
    					 (= 3 (num-active-neighbours neighbour
    								     active)))
    				(setf new-active
    				      (tree-set-insert new-active
    						       neighbour))))))
				
    		  active)
    new-active))

(defun solve-17 (input)
  (let ((initial (run-parser (parse-file) input))
	(active (make-tree-set #'compare)))
    (loop
       for y from 0
       for line in initial
       do (loop
	     for x from 0
	     for square in line
	     when (eq square :active)
	     do (setf active (tree-set-insert active (list x y 0 0)))))

    (loop
       repeat 6
       do (setf active (step-program active))
       finally (return (tree-set-count active)))))
	 
;; Problem 18
	 
(defun parse-constant ()
  (parse-number))

(defun parse-operator ()
  (with-monad
    (parse-character #\Space)
    (assign op (parse-character "*+"))
    (parse-character #\Space)
    (unit (ecase op (#\* :mul) (#\+ :add)))))


(defun parse-expression-dash ()
  (either (with-monad
	    (assign op (parse-operator))
	    (assign ex (parse-expression))
	    (unit (list op ex)))
	  (unit nil)))
    
(defun parse-expression ()
  (either (with-monad
	    (assign num (parse-number))
	    (assign edash (parse-expression-dash))
	    (if edash
		(unit (cons (car edash) (cons num (cdr edash))))
		(unit num)))
	  (with-monad
	    (parse-character #\()
	    (assign e1 (parse-expression))
	    (parse-character #\))
	    (assign edash (parse-expression-dash))
	    (if edash
		(unit (cons (car edash) (cons (cons :eval e1) (cdr edash))))
		(unit (cons :eval e1))))))

(defun leaf-p (tree)
  (or (numberp tree)
      (eq (car tree) :eval)))

(defun inorder (acc tree fn)
  (if (numberp tree)
      (funcall fn acc tree)
      (ecase (car tree)
	(:eval (funcall fn acc tree))
	((:mul :add)
	 (setf acc (inorder acc (second tree) fn))
	 (setf acc (funcall fn acc (first tree)))
	 (inorder acc (third tree) fn)))))
    

(defun tree-to-list (tree)
  (let ((walker #'(lambda (acc x) (cons x acc))))
    (nreverse (inorder '() tree walker))))

(defun evaluate-expression (tree)
  (cond ((numberp tree)
	 tree)
	((eq (car tree) :eval)
	 (evaluate-expression (cdr tree)))
	(t
	 (let ((list (tree-to-list tree)))
	   (loop
	      while (>= (length list) 3)
	      do
		(let ((op (ecase (second list)
			    (:add #'+) (:mul #'*))))
		  (setf list (cons (funcall op
					    (evaluate-expression (first list))
					    (evaluate-expression (third list)))
				   (cdddr list)))))
	   (car list)))))

(defun precedence (op)
  (ecase op (:add 1) (:mul 0)))

(defun opcode-to-function (op)
  (ecase op (:add #'+) (:mul #'*)))

(defun collapse-one (val1 op val2)
  (funcall (opcode-to-function op) val1 val2))

(defun collapse-stack (stack &optional (fully-collapse-p nil))
  (when (= (length stack) 5)
    (destructuring-bind (val1 op1 val2 op2 val3) stack
      (let ((precedence1 (precedence op1))
	    (precedence2 (precedence op2)))
	(if (>= precedence1 precedence2)
	    (setf stack (list (collapse-one val1 op1 val2) op2 val3))
	    (setf stack (list val1 op1 (collapse-one val2 op2 val3)))))))
  
  (when (and (= (length stack) 3)
	     fully-collapse-p)
    (destructuring-bind (val1 op1 val2) stack
      (setf stack (list (collapse-one val1 op1 val2)))))

  stack)

	   
;; New rules
(defun evaluate-expression (expr)
  (labels ((evaluate-expression-rec (seen rest)
	     (if (null rest)
		 seen
		 (progn
		   (let ((value (car rest)))
		     (when (and (listp value)
				(eq (car value) :eval))
		       (setf value (evaluate-expression (cdr value))))
		     (setf seen (append seen (list value))))
		   (evaluate-expression-rec (collapse-stack seen) (cdr rest))))))
    (car (collapse-stack (evaluate-expression-rec '()
						  (tree-to-list expr))
			 t))))

(defun parse-file ()
  (one-or-more
   (with-monad
    (assign expr (parse-expression))
    (parse-newline)
    (unit expr))))

(defun solve-18 (input)
  (let ((expressions (run-parser (parse-file) input)))
    (loop for expr in expressions
       summing (evaluate-expression expr))))


;; Problem 19

(ql:quickload "sycamore")
(use-package :sycamore)

(defparameter *rules* nil)

(defparameter *input19* "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb")

(defun rule-character (char)
  (parse-character char))

(defun rule-list (rules)
  (if (null rules)
      (unit t)
      (with-monad
	(tree-map-find *rules* (car rules))
	(rule-list (cdr rules)))))
   
(defun parse-character-rule ()
  (with-monad
    (parse-character #\")
    (assign char (parse-character #'lower-case-p))
    (parse-character #\")
    (unit (rule-character char))))

(defun parse-rule ()
  (either (parse-character-rule)
	  (either (with-monad
		    (assign option1 (parse-number-list #\Space))
		    (parse-string " | ")
		    (assign option2 (parse-number-list #\Space))
		    (unit (either (rule-list option1)
				  (rule-list option2))))
		  (with-monad
		    (assign option1 (parse-number-list #\Space))
		    (unit (rule-list option1))))))

(defun parse-rule-line ()
  (with-monad 
    (assign idx (parse-number))
    (parse-string ": ")
    (assign val (parse-rule))
    (parse-newline)
    (unit (cons idx val))))

(defun parse-message-line ()
  (with-monad
    (assign message (parse-characters #'lower-case-p))
    (parse-newline)
    (unit message)))

(defun parse-file ()
  (with-monad
    (assign rules (one-or-more (parse-rule-line)))
    (parse-newline)
    (assign messages (one-or-more (parse-message-line)))
    (let ((rule-map (alist-tree-map rules #'compare-numbers)))
      (unit (list rule-map messages)))))

(defun solve-19 (input)
  (destructuring-bind (rules messages) (run-parser (parse-file) input)
    (let ((*rules* rules))
      (loop
	 for message in messages
	 when (let ((res (funcall (tree-map-find *rules* 0) message)))
		(and res (= 0 (length (cdar res)))))
	 summing 1))))

(defun matcher ()
  (either (with-monad
	    (tree-map-find *rules* 42)
	    (assign matches-31 (one-or-more (tree-map-find *rules* 31)))
	    (assign done (parse-empty-p))
	    (if done
		(unit (cons 1 (length matches-31)))
		(fail)))
	  (with-monad
	    (tree-map-find *rules* 42)
	    (assign matches-rec (matcher))
	    (unit (cons (+ 1 (car matches-rec))
			(cdr matches-rec))))))

(defun solve-19-2 (input)
  (destructuring-bind (rules messages) (run-parser (parse-file) input)
    (let ((*rules* rules))
      (loop
	 for message in messages
	 when
	   (let ((res (run-parser (matcher) message)))
	     (and res (>= (car res) (+ 1 (cdr res)))))
	 sum 1))))


;; Problem 20

(defparameter *input20* "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###")

(defun compare-tiles (a b)
  (compare-numbers a b))

(defun parse-tile ()
  (with-monad
    (parse-string "Tile ")
    (assign tile-num (parse-number))
    (parse-character #\:)
    (parse-newline)
    (assign rows (one-or-more
		  (with-monad
		    (assign res (parse-characters "#."))
		    (parse-newline)
		    (unit res))))
    (parse-newline)
    (unit (cons tile-num rows))))

(defun parse-file ()
  (one-or-more (parse-tile)))

(defun chars-to-fingerprint (chars)
  (reduce #'(lambda (last cur) (+ (* 2 last) cur))
	  (map 'list #'(lambda (char) (if (char= char #\#) 1 0)) chars)
	  :initial-value 0))

(defun last-char (string)
  (char string (- (length string) 1)))

(defun first-char (string)
  (char string 0))

(defun reverse-int (int)
  (loop
     with acc = 0
     for i from 0 below 10
     do
       (setf acc (+ (* acc 2) (mod int 2)))
       (setf int (floor int 2))
     finally (return acc)))

(defun print-tile (tile)
  (format nil "~{~a~%~}" tile))

(defun flip-tile (tile)
  (loop
     for i below (length tile)
     collect (format nil "~{~c~}" (mapcar #'(lambda (row) (char row i)) tile))))

(defun rotate-tile (tile)
  (loop
     for i below (length tile)
     collect (format nil "~{~c~}"
		     (mapcar #'(lambda (row) (char row (- (length tile)
							  i
							  1)))
			     tile))))
(defun extract-tile-fingerprint (tile)
  (list (chars-to-fingerprint (car tile))
	(chars-to-fingerprint (mapcar #'last-char tile))
	(chars-to-fingerprint (reverse (car (last tile))))
	(chars-to-fingerprint (mapcar #'first-char (reverse tile)))))

(defun rotate-fingerprint (tile)
  (append (cdr tile) (list (car tile))))

(defun flip-fingerprint (tile)
  (mapcar #'reverse-int (reverse tile)))

(defun has-side-p (side tile)
  (or (find side tile)
      (find side (flip-fingerprint tile))))

(defun find-tile-with-side (side tiles)
  (car (fold-tree-map
	#'(lambda (acc tile-id sides)
	    (cond ((find side sides) (cons (cons tile-id sides) acc))
		  ((find side (flip-fingerprint sides))
		   (cons (cons tile-id (flip-fingerprint sides)) acc))
		  (t acc)))
	'()
	tiles)))

(defun match-tile-right (tile tiles)
  (let* ((right-side (second tile))
	 (match-side (reverse-int right-side))
	 (match-tile (find-tile-with-side match-side tiles))
	 (match-tile-id (car match-tile))
	 (match-tile-sides (cdr match-tile)))
    (when match-tile      
      (loop
	 repeat 4
	 while (not (= match-side (fourth match-tile-sides)))
	 do
	   (setf match-tile-sides (rotate-fingerprint match-tile-sides))))
    (cons match-tile-id match-tile-sides)))

(defun match-tile-down (tile tiles)
  (let* ((down-side (third tile))
	 (match-side (reverse-int down-side))
	 (match-tile (find-tile-with-side match-side tiles))
	 (match-tile-id (car match-tile))
	 (match-tile-sides (cdr match-tile)))
    (when match-tile      
      (loop
	 repeat 4
	 while (not (= match-side (first match-tile-sides)))
	 do
	   (setf match-tile-sides (rotate-fingerprint match-tile-sides))))
    (cons match-tile-id match-tile-sides)))

(defun get-tiles (input)
  (alist-tree-map (run-parser (parse-file) input) #'compare-tiles))

(defun get-row (left-tile-id left-tile-sides tiles)
  (labels ((get-row-rec (acc cur-tile-id cur-tile-sides tiles)
	     (if (= (length acc) 11)
		 (nreverse (cons cur-tile-id acc))
		 (let* ((tiles (tree-map-remove tiles cur-tile-id))
			(match-tile (match-tile-right cur-tile-sides tiles))
			(match-tile-id (car match-tile))
			(match-tile-sides (cdr match-tile)))
		   (get-row-rec (cons cur-tile-id acc)
				match-tile-id
				match-tile-sides
				tiles)))))
    (get-row-rec '()
		 left-tile-id
		 left-tile-sides
		 tiles)))

(defun get-picture-ids (left-id left-sides tile-sides)
  (loop
     with ret = '()
     repeat 12
     do
       (let ((row (get-row left-id left-sides tile-sides)))
	 (setf ret (cons row ret))
	 (loop
	    for id in row
	    do (setf tile-sides (tree-map-remove tile-sides id))))
		   
       (destructuring-bind (next-id &rest next-sides)
	   (match-tile-down left-sides tile-sides)
	 (setf left-id next-id)
	 (setf left-sides next-sides))
     finally (return (nreverse ret))))

(defun get-picture-row (left-picture tile-ids tile-sides tile-pictures)
  (reverse
   (reduce #'(lambda (acc right-id)
	       (cons (match-tile-picture-right
		      (car acc)
		      (tree-map-find tile-sides right-id)
		      (tree-map-find tile-pictures right-id))
		     acc))
	   (cdr tile-ids)
	   :initial-value (list left-picture))))

(defun solve-20-3 (input)
  (let* ((tile-pictures (get-tiles input))
	 (tile-sides
	  (fold-tree-map #'(lambda (acc key value)
			     (tree-map-insert acc
					      key
					      (extract-tile-fingerprint value)))
			 (make-tree-map #'compare-tiles)
			 tile-pictures))
	 (left-id 3187)
	 (left-sides (tree-map-find tile-sides left-id))
	 (picture-tiles (get-picture-ids left-id left-sides tile-sides)))
    (loop
       for row-tile-ids in picture-tiles
       for left-picture = (tree-map-find tile-pictures 3187) then
	 (match-tile-picture-down left-picture
				  (tree-map-find tile-sides (car row-tile-ids))
				  (tree-map-find tile-pictures
						 (car row-tile-ids)))
       collect (get-picture-row left-picture
				row-tile-ids
				tile-sides
				tile-pictures))))

(defun match-tile-picture-right (cur-picture right-sides right-picture)
  (let ((cur-sides (extract-tile-fingerprint cur-picture))
	(ret nil))
    (loop
       repeat 4
       do
	 (when (= (second cur-sides) (reverse-int (fourth right-sides)))
	   (setf ret right-picture))
	 (setf right-sides (rotate-fingerprint right-sides))
	 (setf right-picture (rotate-tile right-picture)))
    (setf right-sides (flip-fingerprint right-sides))
    (setf right-picture (flip-tile right-picture))
    (loop
       repeat 4
       do
	 (when (= (second cur-sides) (reverse-int (fourth right-sides)))
	   (setf ret right-picture))
	 (setf right-sides (rotate-fingerprint right-sides))
	 (setf right-picture (rotate-tile right-picture)))
    ret))

(defun match-tile-picture-down (cur-picture down-sides down-picture)
  (let ((cur-sides (extract-tile-fingerprint cur-picture))
	(ret nil))
    (loop
       repeat 4
       do
	 (when (= (third cur-sides) (reverse-int (first down-sides)))
	   (setf ret down-picture))
	 (setf down-sides (rotate-fingerprint down-sides))
	 (setf down-picture (rotate-tile down-picture)))
    (setf down-sides (flip-fingerprint down-sides))
    (setf down-picture (flip-tile down-picture))
    (loop
       repeat 4
       do
	 (when (= (third cur-sides) (reverse-int (first down-sides)))
	   (setf ret down-picture))
	 (setf down-sides (rotate-fingerprint down-sides))
	 (setf down-picture (rotate-tile down-picture)))
    ret))

(defun strip-border (tile)
  (loop
     for i from 1 below (- (length tile) 1)
     collect (subseq (elt tile i) 1 (- (length (elt tile i)) 1))))

(defun strip-borders (tiles)
  (loop
     for row in tiles
     collect (loop
		for tile in row
		collect (strip-border tile))))

(defparameter *solved-output*
  (loop for row in (strip-borders (solve-20-3 (read-file "input20")))
     append (loop for i from 0 below (length (first row))
	       collect (apply #'concatenate 'string (loop for tile in row
						       collect (elt tile i))))))

(defparameter *sea-monster* '("                  # "
 			      "#    ##    ##    ###"
 			      " #  #  #  #  #  #   "))
;;(defparameter *sea-monster* '("  #" "   " " # "))

(defun find-monster (monster picture)
  (labels ((monster-at-p (x y)
	     (loop
		with ret = t
		named outer
		for i from 0
		repeat (length (car monster))
		do (loop
		      for j from 0
		      repeat (length monster)
		      when (and (char= #\# (char (elt monster j) i))
				(char/= #\#
					(char (elt picture (+ j y))
					      (+ i x))))
		      do
			(setf ret nil)
			(return-from outer ret))
		finally (return-from outer ret))))
    (loop
       for i from 0 below (- (length (car picture)) (length (car monster)))
       append (loop for j from 0 below (- (length picture) (length monster))
		 when (monster-at-p i j)
		   collect (cons i j)))))


;; Problem 21

(defun compare-strings (a b)
  (cond ((string< a b ) -1)
	((string= a b ) 0)
	(t 1)))
(defparameter *input21* "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(defun parse-file ()
  (one-or-more
   (with-monad
     (assign ingredients (parse-list #'parse-word #\Space))
     (parse-string " (contains ")
     (assign allergens (parse-list #'parse-word ", "))
     (parse-string ")")
     (parse-newline)
     (unit (list  ingredients allergens)))))

(defun possibly-contain (allergen foods allergens)
  (setf foods (apply #'tree-set #'compare-strings foods))
  
  (multiple-value-bind (value key present-p)
      (tree-map-find allergens allergen)

    (declare (ignore key))
    
    (if present-p
	(setf value (tree-set-intersection-difference value foods))
	(setf value foods))

    (tree-map-insert allergens allergen value)))

(defun get-solved (allergens)
  (let ((solved (make-tree-set #'compare-strings)))
    (do-tree-map ((allergen foods) allergens solved)
      (declare (ignore allergen))
      (when (= 1 (tree-set-count foods))
	(setf solved
	      (tree-set-insert solved
			       (nth-value 1 (tree-set-remove-min foods))))))))

(defun update-allergens (allergens solved)
  (do-tree-map ((allergen foods) allergens allergens)
    (when (> (tree-set-count foods) 1)
      (setf allergens
	    (tree-map-insert allergens
			     allergen
			     (nth-value 1
					(tree-set-intersection-difference
					 foods
					 solved)))))))

(defun solve-21 (input)
  (let ((foods (run-parser (parse-file) input))
	(allergens (make-tree-map #'compare-strings)))
    (loop
       for (ingredients food-allergens) in foods
       do (loop for allergen in food-allergens
	     do (setf allergens
		      (possibly-contain allergen ingredients allergens))))

    (loop
       with last-solved-len = 0
       for solved = (get-solved allergens)
       while (/= (tree-set-count solved) last-solved-len)
       do
	 (setf allergens (update-allergens allergens solved))
	 (setf last-solved-len (tree-set-count solved)))

    (let ((allergen-ingredients
	   (tree-set-list
	    (fold-tree-map #'(lambda (acc key cur)
			       (declare (ignore key))
			       (tree-set-union acc cur))
			   (make-tree-set #'compare-strings)
			   allergens))))
      (loop
	 for (ingredients allergens) in foods
	 summing (length (set-difference ingredients
					 allergen-ingredients
					 :test 'equal))))

    (let ((dangerous-ingredient-list
	   (fold-tree-map #'(lambda (acc key cur)
			      (cons (cons key
					  (tree-set-min cur))
				    acc))
			  '()
			  allergens)))
      (setf dangerous-ingredient-list
	    (sort dangerous-ingredient-list #'string< :key #'car))
      (reduce #'(lambda (acc cur)
		  (concatenate 'string acc "," cur))
	      (mapcar #'cdr dangerous-ingredient-list)))))


;; Problem 22

(defparameter *input22* "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")
(defun parse-file ()
  (with-monad
    (parse-string "Player 1:")
    (parse-newline)
    (assign player1 (parse-list #'parse-number (format nil "~c~c" #\return #\newline)))
    (parse-newline)
    (parse-newline)
    (parse-string "Player 2:")
    (assign player2 (parse-list #'parse-number (format nil "~c~c" #\return #\newline)))
    (unit (list player1 player2))))
				
(defun compare-games (game-a game-b)
  (let ((diff-a (compare-lists (car game-a) (car game-b))))
    (if (/= 0 diff-a)
	diff-a
	(compare-lists (cdr game-a) (cdr game-b)))))
  
(defun game (qa qb)
  (let ((configurations (make-tree-set #'compare-games)))
    (loop
       while (and (not (emptyp qa))
		  (not (emptyp qb)))
       do
	 (if (tree-set-member-p configurations
				(cons qa qb))
	     (return-from game (values :a qa qb))
	     (setf configurations (tree-set-insert configurations
						   (cons qa qb))))
	 (let ((new-qa (cdr qa))
	       (fa (car qa))
	       (new-qb (cdr qb))
	       (fb (car qb)))

	   (if (and (<= fa (length new-qa))
		    (<= fb (length new-qb)))
	       (multiple-value-bind (winner ign-a ign-b)
		   (game (subseq new-qa 0 fa)
			 (subseq new-qb 0 fb))
		 (declare (ignore ign-a ign-b))
		 (if (eq winner :b)
		     (progn (setf qa new-qa)
			    (setf qb (append new-qb (list fb fa))))
		     (progn (setf qb new-qb)
			    (setf qa (append new-qa (list fa fb))))))
	       (if (< fa fb)
		   (progn (setf qa new-qa)
			  (setf qb (append new-qb (list fb fa))))
		   (progn (setf qb new-qb)
			  (setf qa (append new-qa (list fa fb)))))))))
  (if (emptyp qa)
      (values :b qa qb)
      (values :a qa qb)))

(defun solve-22 (input)
  (destructuring-bind (a b) (run-parser (parse-file) input)
    (multiple-value-bind (winner qa qb) (game a b)
	(let ((winner-q (if (eq winner :a) qa qb)))
	  (loop
	     for i from 0
	     for val in (reverse winner-q)
	     sum (* val (1+ i)))))))
      
;; Problem 24

(defun parse-direction ()
  (either (then (parse-string "se") (unit '(1 -1)))
	  (then (parse-string "sw") (unit '(0 -1)))
	  (then (parse-string "ne") (unit '(0 1)))
	  (then (parse-string "nw") (unit '(-1 1)))
	  (then (parse-string "e") (unit '(1 0)))
	  (then (parse-string "w") (unit '(-1 0)))))

(defun parse-file ()
  (parse-list (one-or-more (parse-direction))
	      (parse-newline)))

(defun get-tile (directions)
  (reduce #'add-positions directions :initial-value '(0 0)))

(defun flip-tile (color)
  (ecase color (:black :white) (:white :black)))

(defconstant +neighbours+ '((-1 1) (0 1) (1 0) (1 -1) (0 -1) (-1 0)))

(defun get-neighbours (pos)
  (mapcar #'(lambda (n) (add-positions pos n)) +neighbours+))

(defun new-state (pos ht)
  (let* ((cur (gethash pos ht :white))
	 (black-neighbours
	  (length (remove-if #'(lambda (col) (eq col :white))
			     (mapcar #'(lambda (n) (gethash n ht :white))
				     (get-neighbours pos))))))
    (if (or (and (eq cur :black)
		 (or (= 0 black-neighbours)
		     (> black-neighbours 2)))
	    (and (eq cur :white)
		 (= 2 black-neighbours)))
	(flip-tile cur)
	cur)))

(defun solve-24 (input)
  (let ((tiles (mapcar #'get-tile (run-parser (parse-file) input)))
	(ht (make-hash-table :test 'equal)))
    (loop for tile in tiles
       do (let ((cur (gethash tile ht :white)))
	    (setf (gethash tile ht) (flip-tile cur))))
    (loop
       repeat 100
       for new-ht = (make-hash-table :test 'equal)
       do (loop
	     for key being the hash-keys of ht
	     do
	       (setf (gethash key new-ht) (new-state key ht))
	       (loop
		  for neighbour in (get-neighbours key)
		  do (setf (gethash neighbour new-ht) (new-state neighbour ht))))
	 (setf ht new-ht))

    (loop for value being the hash-values of ht
       when (eq :black value)
       sum 1)
    ))

;; Problem 25

(defun parse-file ()
  (with-monad
    (assign a (parse-number))
    (parse-newline)
    (assign b (parse-number))
    (unit (list a b))))



(defun solve-25 (input)
  (let ((n 20201227)
	(g 7))
    (destructuring-bind (g-a g-b) (run-parser (parse-file) input)
      (let ((a (baby-step-giant-step n g g-a)))
	(expt-mod g-b a n)))))
