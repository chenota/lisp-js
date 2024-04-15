;;;; js-convert.lisp

(in-package #:lisp-js)

;; Token-specific string to number
(defun token-str-to-num (str)
    (let* 
        ;; Split the string up by decimal point and exponent e
        ((split-e (cl-ppcre:split "e" str))
         (split-point (cl-ppcre:split "\\." (first split-e)))
         ;; Get whole part of number if is not ""
         (whole-part
            (if  
                (equal (first split-point) "")
                0
                (parse-integer (first split-point))))
         ;; If something after decimal point, parse into integer
         (decimal-part
            (if
                (= (length split-point) 1)
                0
                (parse-integer (second split-point))))
        ;; If e exists, also parse number after e
         (exponent-part
            (if  
                (= (length split-e) 1)
                0
                (parse-integer (second split-e)))))
        ;; Final result: (whole + decimal) * (10^exponent)
        (float 
            (* 
                (+
                    whole-part
                    (/ decimal-part (expt 10 (length (write-to-string decimal-part)))))
                (expt 10 exponent-part)))))

;; Token-specific string to boolean
(defun token-str-to-bool (str)
    (alexandria:switch (str :test 'equal) 
        ("false" nil)
        ("true" t)
        (t nil)))

(defun to-str (value)
    `(:StrVal 
        ,(alexandria:switch ((first value) :test 'eq)
            (:StrVal (second value))
            (:NumVal (format nil "~,,,,F" (second value)))
            (:BoolVal (if (second value) "true" "false"))
            (:UndefVal "undefined")
            (:NullVal "null")
            (:IdentVal (second value))
            (t (error (format t "Error: At end of to-str function with value ~%~A" value))))))

(defun to-num (value)
    `(:NumVal
        ,(alexandria:switch ((first value) :test 'eq)
            (:NumVal (second value))
            ;; If string, must try to make number out of it then fail
            (:StrVal 
                ;; Trim leading and lagging whitespace from string
                (handler-case (token-str-to-num (string-trim '(#\Space #\Tab #\Newline) (second value)))
                    (error () :NaN)))
            (:BoolVal (if (second value) 1 0))
            (:UndefVal :NaN)
            (:NullVal 0)
            (t (error (format t "Error: At end of to-num function with value ~%~A" value))))))

(defun to-bool (value)
    `(:BoolVal
        ,(alexandria:switch ((first value) :test 'eq)
            (:BoolVal (second value))
            (:NumVal (if (or (= (second value) 0.0) (eq (second value) :NaN)) nil t))
            (:StrVal (if (equal (second value) "") nil t))
            (:UndefVal nil)
            (:NullVal nil)
            (t (error (format t "Error: At end of to-bool function with value ~%~A" value))))))