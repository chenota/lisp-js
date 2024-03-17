;;;; js-convert.lisp

(in-package #:lisp-js)

(defun str-to-num (str)
    0)

(defun token-str-to-bool (str)
    (alexandria:switch (str :test 'equal) 
        ("false" nil)
        ("true" t)
        (t nil)))