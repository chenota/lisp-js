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