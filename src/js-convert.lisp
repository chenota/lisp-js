;;;; js-convert.lisp

(in-package #:lisp-js)

;; Token-specific string to number
(defun token-str-to-num (str)
    (parse-number:parse-number str))

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
            (:NumVal 
                (if (eq (second value) :NaN)
                    "NaN"
                    (format nil "~,,,,F" (second value))))
            (:BoolVal (if (second value) "true" "false"))
            (:UndefVal "undefined")
            (:NullVal "null")
            (:IdentVal (second value))
            (t ""))))

(defun to-num (value)
    `(:NumVal
        ,(alexandria:switch ((first value) :test 'eq)
            (:NumVal (second value))
            ;; If string, must try to make number out of it then fail
            (:StrVal 
                ;; Trim leading and lagging whitespace from string
                (handler-case (token-str-to-num (second value))
                    (error () :NaN)))
            (:BoolVal (if (second value) 1 0))
            (:UndefVal :NaN)
            (:NullVal 0)
            (t :NaN))))

(defun to-bool (value)
    `(:BoolVal
        ,(alexandria:switch ((first value) :test 'eq)
            (:BoolVal (second value))
            (:NumVal (if (or (= (second value) 0.0) (eq (second value) :NaN)) nil t))
            (:StrVal (if (equal (second value) "") nil t))
            (:UndefVal nil)
            (:NullVal nil)
            (t t))))

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
        (:SizeFn "[Function]")
        (t (format nil "Unimplemented: ~A" value))))