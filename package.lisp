(defpackage :advent-of-code
  (:nicknames :aoc)
  (:import-from :drakma :http-request :cookie-jar :cookie)
  (:import-from :sycamore :gsymbol-compare :or-compare)
  (:use :cl :iterate)
  (:export
   :get-problem

   :aif
   :awhen
   :it
   :with-gensyms
   
   ;; Combinatorics
   :pairs
   :triples
   :fours
   :permutations
   :combinations
   
   ;; Number Theory
   :chinese-remainder-theorem
   :expt-mod
   :invmod
   :geometric-sum
   
   ;; Monad
   *unit-function*
   *bind-function*
   :unit
   :bind
   :then
   :with-monad
   :assign
   :either
   :mapm_
   :whenm
   :mapfsetm_
   :concatm
   :iterate-untilm
   
   ;; Maybe
   :maybe-fail
   :maybe-mplus
   :maybe-zero
   :run-maybe
   :from-maybe
   :is-just
   :is-nothing
   
   ;; State
   :run-state
   :make-state
   :def-state
   
   ;; Parser
   :parser-fail
   :run-parser
   :make-indexed-string
   :zero-or-more
   :one-or-more
   :two-of
   :n-of
   :up-to-n-of
   :parse-list
   :parse-subparser
   :parse-empty-p
   :parse-eof
   :parse-character
   :parse-characters
   :parse-any-character
   :parse-until
   :parse-bracketed
   :whitespace-char-p
   :parse-space
   :parse-whitespace
   :parse-newline
   :parse-line
   :parse-lines
   :parse-lower-case
   :parse-upper-case
   :parse-alphanumeric
   :parse-word
   :parse-keyword
   :parse-string
   :parse-digit
   :parse-number
   :parse-number-list

   ;; Row Column Parser
   :make-rc-string
   :run-rc-parser
   :lift-parser-rc
   :parse-rc-get-rc
   :parse-rc-set-rc
   :parse-rc-character
   :parse-rc-newline
   
   ;; Functional Queue
   :make-queue
   :queue-pop-front
   :queue-push-back
   :queue-pop-frontf
   :queue-push-backf
   :queue-empty-p

   ;; Union Find
   :make-uf
   :uf-find
   :uf-union
   :uf-make-set
   :uf-set-size
   :uf-roots

   ;; Points
   :point+
   :point-
   :point*
   :point-signum
   :point-abs
   :point-max
   :point-min
   
   ;; Rectangles
   :rect-id
   :rect-top
   :rect-bottom
   :rect-left
   :rect-right
   :rect-size
   :rect-compare
   :make-rect
   :rect-intersection
   :rect-difference

   ;; Graphs
   :shortest-paths
   :breadth-first-search
   :summed-area-table
   :dijkstra
   :a-star
   :in-bfs-from
   :in-dijkstra-from

   ;; Zipper
   :tree-from-zipper
   :zipper-from-tree
   :zipper-tree
   :zipper-crumbs
   :zipper-depth
   :zipper-rootp
   :zipper-leafp
   :zipper-siblingp
   :go-left
   :go-right
   :go-up
   :leftmost
   :rightmost
   :topmost
   :go-left-sibling
   :go-right-sibling
   :go-next
   :go-prev
   :modify
   :attach
   :with-zipper
   :find-in-tree
   
   ;; Utilities
   :string-to-character-list
   :character-list-to-string
   :digits-to-int
   :int-to-digits
   :manhattan
   :edit-distance
   :hash-table-from-alist
   :hash-table-from-list-list
   :hash-table-dimensions
   :map-from-list-list
   :map-dimensions

   ;; Intervals
   :interval-start
   :interval-end
   :interval-size
   :interval-contains
   :intervals-contain
   :intervals-size   
   :interval-intersect
   :intervals-intersect
   :intervals-normalize

   ;; Quarternions
   :q-compose
   :q-conjugate
   :q-norm
   :q-normalize
   :q-reciprocal
   :q-round
   :q-rotor
   :q-rotate-vector
   :q-conjugate-by))
