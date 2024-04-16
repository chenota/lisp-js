build:
	sbcl --load 'lisp-js.asd' --eval '(ql:quickload :lisp-js)' --eval '(asdf:make :lisp-js)' --eval '(quit)'

load:
	sbcl --eval '(ql:quickload :lisp-js)' --eval '(in-package :lisp-js)'