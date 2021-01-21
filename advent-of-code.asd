(asdf:defsystem "advent-of-code"
  :description "Advent of Code"
  :version "0.0.1"
  :author "Blake Watkins <blakewatkins@gmail.com>"
  :licence "Creative Commons Attribution 4.0 International License."
  :depends-on ("drakma" "cl-heap" "sycamore" "iterate")
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "functional-queue" :depends-on ("package"))
               (:file "parser" :depends-on ("package"))
               (:file "union-find" :depends-on ("package"))
               (:file "rectangle" :depends-on ("package"))))
