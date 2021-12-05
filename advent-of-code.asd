(asdf:defsystem "advent-of-code"
  :description "Advent of Code"
  :version "0.0.1"
  :author "Blake Watkins <blakewatkins@gmail.com>"
  :licence "GNU General Public License (GPL) version 3"
  :depends-on ("drakma" "cl-heap" "sycamore" "iterate" "fset")
  :components ((:file "package")
               (:file "download-input" :depends-on ("package"))
               (:file "util" :depends-on ("package" "download-input"))
               (:file "functional-queue" :depends-on ("package"))
               (:file "parser" :depends-on ("package"))
               (:file "union-find" :depends-on ("package"))
               (:file "point" :depends-on ("package"))
               (:file "rectangle" :depends-on ("package"))))
