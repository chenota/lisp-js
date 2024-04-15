;;;; lisp-js.asd

(require :asdf)

:build-operation "program-op" ;; leave as is
:build-pathname "ljs.o"
:entry-point "lisp-js:main"

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
               (:file "src/pratt-parselets")
               (:file "src/eval")
               (:file "src/js-op")
               (:file "src/js-env")))
