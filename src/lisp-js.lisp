;;;; lisp-js.lisp

(in-package #:lisp-js)

(defparameter *is-exit* nil)

(defun eval-str (str)
    (stmt-eval (expr (tokenize-string str))))

;; Program entry point
(defun main nil 
    ;; Check for command line arguments
    (if (> (length sb-ext:*posix-argv*) 1)
        ;; Eval first file if given
        (handler-case 
            ;; Normal execution
            (eval-str (uiop:read-file-string (second sb-ext:*posix-argv*)))
            ;; Handle error messages
            (error (e) (format t "~A~%" e)))
        ;; If no args, run command line
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
                    (error (e) (format t "~A~%" e)))))))