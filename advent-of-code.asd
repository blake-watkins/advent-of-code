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
               (:file "monad" :depends-on ("package"))
               (:file "maybe" :depends-on ("monad" "package"))
               (:file "zipper" :depends-on ("maybe" "package"))
               (:file "state" :depends-on ("monad" "package"))
               (:file "parser" :depends-on ("monad" "package"))
               (:file "union-find" :depends-on ("package"))
               (:file "point" :depends-on ("package"))
               (:file "rectangle" :depends-on ("package"))))
