(ql:quickload :lisp-js)
(in-package :lisp-js)

(defparameter my-token-stream (tokenize-string "-1 + -1"))