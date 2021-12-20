(in-package :aoc)

;;; ZIPPER
;; http://learnyouahaskell.com/zippers
;; Zipper is a two element list - first element is the focus of the structure,
;; second element is the information needed to move around as a list of breadcrumbs

(defun tree-from-zipper (zipper)
  (first (from-maybe (funcall (go-topmost) zipper))))

(defun zipper-from-tree (tree)
  (list tree nil))

;; Zipper information
(defun zipper-tree (zipper)
  (first zipper))

(defun zipper-crumbs (zipper)
  (second zipper))

(defun zipper-depth (zipper)
  "How deep is the zipper. Zero is the root level."
  (length (zipper-crumbs zipper)))

(defun zipper-rootp (zipper)
  (null (zipper-crumbs zipper)))

(defun zipper-leafp (zipper)
  (numberp (zipper-tree zipper)))

(defun zipper-siblingp (type)
  "TYPE should be :LEFT or :RIGHT. Returns t if zipper is at that type of child"
  (lambda (zipper)
    (let ((crumbs (zipper-crumbs zipper)))
      (and crumbs (eq type (caar crumbs))))))

;; Zipper movement.
;; All movement return results in a Maybe. Will return :nothing if moved off the
;; tree

;; goLeft :: Zipper a -> Maybe (Zipper a)
(defun go-left ()
  (lambda (zipper)
    (if (zipper-leafp zipper)
	(maybe-fail)      
	(destructuring-bind ((left right) crumbs) zipper
          (unit (list left (cons (list :left right) crumbs)))))))

(defun go-right ()
  (lambda (zipper)
    (if (zipper-leafp zipper)
	(maybe-fail)
	(destructuring-bind ((left right) crumbs) zipper
          (unit (list right (cons (list :right left) crumbs)))))))

(defun go-up ()
  (lambda (zipper)
    (if (zipper-rootp zipper)
	(maybe-fail)
	(destructuring-bind (tree ((crumb-type crumb) . crumbs)) zipper
          (if (eq crumb-type :left)
              (unit (list (list tree crumb) crumbs))
              (unit (list (list crumb tree) crumbs)))))))

(defun go-topmost ()
  (iterate-untilm #'zipper-rootp (go-up)))

(defun go-leftmost ()
  (iterate-untilm #'zipper-leafp (go-left)))

(defun go-rightmost ()
  (iterate-untilm #'zipper-leafp (go-right)))

(defun go-right-sibling ()
  "Go to the closest right sibling in the tree. Fails if none exists. "
  (concatm (iterate-untilm (zipper-siblingp :left) (go-up)) (go-up) (go-right)))

(defun go-left-sibling ()
  "Go to the closest left sibling in the tree. Fails if none exists. "
  (concatm (iterate-untilm (zipper-siblingp :right) (go-up)) (go-up) (go-left)))

(defun go-next ()
  "Go to next inorder leaf of the tree. Fails if none exists. "
  (concatm (go-right-sibling) (go-leftmost)))

(defun go-prev ()
  "Go to previous inorder leaf of the tree. Fails is none exists. "
  (concatm (go-left-sibling) (go-rightmost)))

;; Zipper modification functions

;; modify :: (a -> b) -> Zipper a -> Maybe Zipper b
(defun modify (f)
  "Returns zipper with a the value at the focus modified by the function if at a leaf. Otherwise fails. "
  (lambda (zipper)
    (if (zipper-leafp zipper)
	(unit (list (funcall f (zipper-tree zipper))
		    (zipper-crumbs zipper)))
	(maybe-fail))))

(defun attach (new-tree)
  (lambda (zipper)
    (unit (list new-tree (zipper-crumbs zipper)))))

(defmacro with-zipper (zipper &body body)
  `(funcall (concatm
             ,@body)
            ,zipper))
;;find-in-tree :: Tree a -> (a -> Bool) -> Maybe Zipper a
(defun find-in-tree (tree pred &optional (depth 0) (crumbs nil))
  "Find the first node in the tree that satisfies PRED. Return a zipper to that node. Return :nothing if there is none. "
  (cond
    ((funcall pred tree depth) (unit (list tree crumbs)))
    ((numberp tree) (maybe-fail))
    (t (let* ((left-crumbs (cons (list :left (second tree)) crumbs))
	      (left (find-in-tree (first tree)
				  pred
				  (1+ depth)
				  left-crumbs)))
	 (if (is-just left)
	     left
	     (let* ((right-crumbs (cons (list :right (first tree)) crumbs))
		    (right (find-in-tree (second tree)
					 pred
					 (1+ depth)
					 right-crumbs)))
	       (if (is-just right)
		   right
		   (maybe-fail))))))))

