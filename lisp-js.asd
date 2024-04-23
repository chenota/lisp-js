;;;; lisp-js.asd

(require "asdf")

(asdf:defsystem #:lisp-js
  :description "Interpreter for a subset of JavaScript"
  :author "Alex Chenot"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :entry-point "lisp-js:main"
  :build-operation program-op
  :build-pathname "lispjs"
  :depends-on (#:cl-ppcre
               #:alexandria
               #:parse-number)
  :components ((:file "package")
               (:file "src/lisp-js")
               (:file "src/tokenizer")
               (:file "src/js-convert")
               (:file "src/pratt-parser")
               (:file "src/pratt-parselets")
               (:file "src/eval")
               (:file "src/js-op")
               (:file "src/js-env")))
