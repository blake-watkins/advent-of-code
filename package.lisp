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

   ;; Number Theory
   :chinese-remainder-theorem
   :expt-mod
   :invmod
   :geometric-sum
   
   ;; Parser
   :make-indexed-string
   :make-rc-string
   :unit
   :bind
   :parser-fail
   :then
   :with-monad
   :assign
   :run-parser
   :either
   :zero-or-more
   :one-or-more
   :two-of
   :n-of
   :parse-list
   :parse-empty-p
   :parse-eof
   :parse-character
   :parse-characters
   :parse-any-character
   :parse-until
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
   :parse-string
   :parse-digit
   :parse-number
   :parse-number-list

   ;; Row Column Parser
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
   :in-bfs-from
   
   ;; Utilities
   :digits-to-int
   :int-to-digits
   :manhattan))
