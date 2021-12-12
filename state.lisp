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


;;; Define STATE class with the given slots
;;; Define FROM-TEMPLATE (TEMPLATE &key <slots>) function. Creates a new state
;;;   initialized to the slots of TEMPLATE but new values for given slots. 
;;; Define GET-<slot> function in the game state monad for each slot
;;; Define SET-<slot> function in the game state monad for each slot
(defmacro def-state (slots)
  (let ((slots-gensyms (mapcar (lambda (s) (cons s (gensym (symbol-name s))))
                               slots)))
    `(progn
       (defclass state ()
         ,(mapcar
           (lambda (name)
             `(,name
               :initarg ,(intern (symbol-name name) :keyword)))
           slots))

       (defun make-state
           (&key ,@ slots)
         (make-instance
          'state
          ,@(apply #'append
                   (mapcar
                    (lambda (s)
                      (list (intern (symbol-name s) :keyword) s))
                    slots))))
       
       (defun from-template
           (template
            &key ,@ (mapcar
                     (lambda (s)
                       (list s nil (cdr (assoc s slots-gensyms))))
                     slots))
         (make-instance
          'state
          ,@(apply #'append
                   (mapcar
                    (lambda (s)
                      (list (intern (symbol-name s) :keyword)
                            `(if ,(cdr (assoc s slots-gensyms))
                                 ,s
                                 (slot-value template ',s))))
                    slots))))

       ,@(mapcar
          (lambda (s)
            (let ((gs (gensym "STATE")))
              `(defun ,(intern (concatenate 'string
                                            "GET-"
                                            (string-upcase (symbol-name s))))
                   ()
                 (lambda (,gs)
		   (list (slot-value ,gs ',s) ,gs)))))
          slots)

       ,@ (mapcar
           (lambda (s)
             (let ((gs (gensym "STATE")))
               `(defun ,(intern (concatenate 'string
                                             "SET-"
                                             (string-upcase (symbol-name s))))
                    (value)
                  (lambda (,gs)
		    (list ()
			  (from-template ,gs
					 ,(intern (symbol-name s)
                                                  :keyword)
					 value))))))
           slots))))

