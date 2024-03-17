;;;; lisp-js.asd

(asdf:defsystem #:lisp-js
  :description "Interpreter for a subset of JavaScript"
  :author "Alex Chenot"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "lisp-js")))
