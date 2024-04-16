;;;; lisp-js.lisp

(in-package #:lisp-js)

(defparameter *is-exit* nil)

(defun eval-str (str)
    (stmt-eval (expr (tokenize-string str))))

(defun pretty-print (value)
    (alexandria:switch ((first value) :test 'eq)
        (:StrVal (concatenate 'string "'" (second value) "'"))
        (:NumVal (second (to-str value)))
        (:BoolVal (second (to-str value)))
        (:UndefVal "undefined")
        (:NullVal "null")
        (:RefVal (pretty-print (resolve-reference value)))
        (:ObjRef (pretty-print (resolve-object value)))
        (:ObjVal 
            (if (member :list value :test 'eq)
                (concatenate 'string 
                    "[ "
                    (reduce 
                        (lambda (acc new)
                            (concatenate 'string acc (if (string= acc "") "" ", ") (pretty-print (cdr new))))
                        (second value)
                        :initial-value "")
                    " ]")
                (concatenate 'string 
                    "{ "
                    (reduce 
                        (lambda (new acc)
                            (concatenate 'string acc (if (string= acc "") "" ", ") (second (car new)) ": " (pretty-print (cdr new))))
                        (second value)
                        :initial-value ""
                        :from-end t)
                    " }")))
        (:ClosureVal "[Function]")
        (:ExitFn "[Function]")
        (:InputFn "[Function]")
        (:PrintFn "[Function]")
        (:NumberFn "[Function]")
        (:StringFn "[Function]")
        (:BooleanFn "[Function]")
        (t (format nil "Unimplemented: ~A" value))))

(defun main nil 
    (loop while (not *is-exit*) do  
        (progn
            ;; Print carrot
            (princ "> ")
            ;; Wait for output to complete
            (finish-output)
            (handler-case 
                ;; Normal execution
                (format t "~A~%" (pretty-print (eval-str (read-line))))
                ;; Handle error messages
                (error (e) (format t "~A~%" e))))))