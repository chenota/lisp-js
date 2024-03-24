load-sb: lisp-js.asd package.lisp src
	sbcl --eval "(ql:quickload :lisp-js)" --eval "(in-package :lisp-js)"