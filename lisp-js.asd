;;;; lisp-js.asd

(asdf:defsystem #:lisp-js
  :description "Interpreter for a subset of JavaScript"
  :author "Alex Chenot"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre
               #:alexandria)
  :components ((:file "package")
               (:file "src/lisp-js")
               (:file "src/tokenizer")
               (:file "src/js-convert")
               (:file "src/pratt-parser")
               (:file "src/pratt-parselets")))
