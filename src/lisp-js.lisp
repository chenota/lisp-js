;;;; lisp-js.lisp

(in-package #:lisp-js)

(defparameter *is-exit* nil)
(defparameter *exit-code* 0)

(defun eval-str (str)
    (stmt-eval (expr (tokenize-string str))))

(defun main nil 
    (loop while (not *is-exit*) do  
        (progn
            ;; Print carrot
            (princ "> ")
            ;; Wait for output to complete
            (finish-output)
            (handler-case 
                ;; Normal execution
                (format t "~A~%" (eval-str (read-line)))
                ;; Handle error messages
                (error (e) (format t "~A~%" e))))))