;;;; js-op.lisp

(in-package #:lisp-js)

(defun js-plus (left-val right-val)
    (if 
        ;; If stringval involved, do special string handling
        (or 
            (eq (first left-val) :StrVal) 
            (eq (first right-val) :Strval))
        `(:StrVal 
            ,(concatenate 
                'string 
                (second (to-str left-val)) 
                (second (to-str right-val))))
        ;; Otherwise, check for NaN or undefined
        (let  
            ((lnum (to-num left-val))
             (rnum (to-num right-val)))
            (if  
                (or  
                    (eq (second lnum) :NaN)
                    (eq (second rnum) :NaN))
                `(:NumVal :NaN)
                `(:NumVal ,(+ (second lnum) (second rnum)))))))

(defun js-minus (left-val right-val)
    (let  
            ((lnum (to-num left-val))
             (rnum (to-num right-val)))
            (if  
                (or  
                    (eq (second lnum) :NaN)
                    (eq (second rnum) :NaN))
                `(:NumVal :NaN)
                `(:NumVal ,(- (second lnum) (second rnum))))))

(defun js-times (left-val right-val)
    (let  
            ((lnum (to-num left-val))
             (rnum (to-num right-val)))
            (if  
                (or  
                    (eq (second lnum) :NaN)
                    (eq (second rnum) :NaN))
                `(:NumVal :NaN)
                `(:NumVal ,(* (second lnum) (second rnum))))))

(defun js-div (left-val right-val)
    (let  
            ((lnum (to-num left-val))
             (rnum (to-num right-val)))
            (if  
                (or  
                    (eq (second lnum) :NaN)
                    (eq (second rnum) :NaN))
                `(:NumVal :NaN)
                `(:NumVal ,(/ (second lnum) (second rnum))))))

(defun js-lt (left-val right-val)
    ;; Check for strings
    (if 
        (and 
            (eq (first left-val) :StrVal) 
            (eq (first right-val) :StrVal))
        ;; Do string alphabetical comparison
        ;; Doing not not to convert non-nil value to true if is non-nil
        `(:BoolVal ,(not (not(string< (second left-val) (second right-val)))))
        ;; Otherwise, evaluate as numbers
        (let  
            ((lnum (to-num left-val))
             (rnum (to-num right-val)))
            (if  
                (or  
                    (eq (second lnum) :NaN)
                    (eq (second rnum) :NaN))
                `(:BoolVal nil)
                
                `(:BoolVal ,(< (second lnum) (second rnum)))))))

(defun js-lte (left-val right-val)
    ;; Check for strings
    (if 
        (and 
            (eq (first left-val) :StrVal) 
            (eq (first right-val) :StrVal))
        ;; Do string alphabetical comparison
        ;; Doing not not to convert non-nil value to true if is non-nil
        `(:BoolVal ,(not (not(string<= (second left-val) (second right-val)))))
        ;; Otherwise, evaluate as numbers
        (let  
            ((lnum (to-num left-val))
             (rnum (to-num right-val)))
            (if  
                (or  
                    (eq (second lnum) :NaN)
                    (eq (second rnum) :NaN))
                `(:BoolVal nil)
                
                `(:BoolVal ,(<= (second lnum) (second rnum)))))))

(defun js-gt (left-val right-val)
    ;; Check for strings
    (if 
        (and 
            (eq (first left-val) :StrVal) 
            (eq (first right-val) :StrVal))
        ;; Do string alphabetical comparison
        ;; Doing not not to convert non-nil value to true if is non-nil
        `(:BoolVal ,(not (not(string> (second left-val) (second right-val)))))
        ;; Otherwise, evaluate as numbers
        (let  
            ((lnum (to-num left-val))
             (rnum (to-num right-val)))
            (if  
                (or  
                    (eq (second lnum) :NaN)
                    (eq (second rnum) :NaN))
                `(:BoolVal nil)
                
                `(:BoolVal ,(> (second lnum) (second rnum)))))))

(defun js-gte (left-val right-val)
    ;; Check for strings
    (if 
        (and 
            (eq (first left-val) :StrVal) 
            (eq (first right-val) :StrVal))
        ;; Do string alphabetical comparison
        ;; Doing not not to convert non-nil value to true if is non-nil
        `(:BoolVal ,(not (not(string>= (second left-val) (second right-val)))))
        ;; Otherwise, evaluate as numbers
        (let  
            ((lnum (to-num left-val))
             (rnum (to-num right-val)))
            (if  
                (or  
                    (eq (second lnum) :NaN)
                    (eq (second rnum) :NaN))
                `(:BoolVal nil)
                
                `(:BoolVal ,(>= (second lnum) (second rnum)))))))

(defun js-rem (left-val right-val)
    ;; Cast values to numbers
    (let  
        ((lnum (to-num left-val))
         (rnum (to-num right-val)))
        ;; Check for NaN
        (if  
            (or  
                (eq (second lnum) :NaN)
                (eq (second rnum) :NaN))
            `(:NumVal :NaN)
            `(:NumVal ,(rem (second lnum) (second rnum))))))

(defun js-logor (left-val right-val)
    (if (second (to-bool left-val))
        left-val
        right-val))

(defun js-logand (left-val right-val)
    (if (second (to-bool left-val))
        right-val
        left-val))

(defun js-bitor (left-val right-val)
    ;; Cast values to numbers
    (let  
        ((lnum (to-num left-val))
         (rnum (to-num right-val)))
        ;; Check for NaN
        (if  
            (and  
                (eq (second lnum) :NaN)
                (eq (second rnum) :NaN))
            '(:NumVal 0.0)
            (if 
                (eq (second rnum) :NaN)
                lnum 
                (if 
                    (eq (second lnum) :NaN)
                    rnum 
                    `(:NumVal ,(logior (floor (second lnum)) (floor (second rnum)))))))))

(defun js-bitand (left-val right-val)
    ;; Cast values to numbers
    (let  
        ((lnum (to-num left-val))
         (rnum (to-num right-val)))
        ;; Check for NaN
        (if  
            (or  
                (eq (second lnum) :NaN)
                (eq (second rnum) :NaN))
            '(:NumVal 0.0)
            `(:NumVal ,(logand (floor (second lnum)) (floor (second rnum)))))))

(defun js-xor (left-val right-val)
    ;; Cast values to numbers
    (let  
        ((lnum (to-num left-val))
         (rnum (to-num right-val)))
        ;; Check for NaN
        (if  
            (and  
                (eq (second lnum) :NaN)
                (eq (second rnum) :NaN))
            '(:NumVal 0.0)
            (if 
                (eq (second rnum) :NaN)
                lnum 
                (if 
                    (eq (second lnum) :NaN)
                    rnum 
                    `(:NumVal ,(logxor (floor (second lnum)) (floor (second rnum)))))))))

(defun js-lshift (left-val right-val)
    ;; Cast values to numbers
    (let  
        ((lnum (to-num left-val))
         (rnum (to-num right-val)))
        ;; Check for NaN
        (if  
            (and  
                (eq (second lnum) :NaN)
                (eq (second rnum) :NaN))
            '(:NumVal 0.0)
            (if 
                (eq (second rnum) :NaN)
                lnum 
                (if 
                    (eq (second lnum) :NaN)
                    '(:NumVal 0.0)
                    (if 
                        (< (floor (second rnum)) 0)
                        '(:NumVal 0.0)
                        `(:NumVal ,(ash (floor (second lnum)) (floor (second rnum))))))))))

(defun js-rshift (left-val right-val)
    ;; Cast values to numbers
    (let  
        ((lnum (to-num left-val))
         (rnum (to-num right-val)))
        ;; Check for NaN
        (if  
            (and  
                (eq (second lnum) :NaN)
                (eq (second rnum) :NaN))
            '(:NumVal 0.0)
            (if 
                (eq (second rnum) :NaN)
                lnum 
                (if 
                    (eq (second lnum) :NaN)
                    '(:NumVal 0.0)
                    (if 
                        (< (floor (second rnum)) 0)
                        '(:NumVal 0.0)
                        `(:NumVal ,(ash (floor (second lnum)) (floor (* -1 (second rnum)))))))))))

(defun js-in (left-val right-val)
    ;; Cast values to numbers
    (let  
        ((lstr (to-str left-val))
         (robj (resolve-object right-val)))
        ;; Check for object on right
        (if  
            (eq (first robj) :ObjVal)
            `(:BoolVal
                ,(reduce 
                    (lambda (acc new)
                        (if  
                            (string= (second (car new)) (second lstr))
                            t  
                            acc))
                    (second robj)
                    :initial-value nil))
            (error (format nil "TypeError: Cannot use 'in' operator to search for ~s in ~s" (pretty-print lstr) (pretty-print right-val))))))

(defun js-bitnot (val)
    (let ((num (to-num val)))
        (if (eq (second num) :NaN)
            `(:NumVal -1.0)
            `(:NumVal ,(lognor (floor (second num)) (floor (second num)))))))

(defun js-negate (val)
    (let ((num (to-num val)))
        (if (eq (second num) :NaN)
            `(:NumVal :NaN)
            `(:NumVal ,(* -1.0 (second num))))))

(defun js-abs (val)
    (let ((num (to-num val)))
        (if (eq (second num) :NaN)
            `(:NumVal :NaN)
            `(:NumVal ,(abs (second num))))))

(defun js-not (val)
    `(:BoolVal ,(not (second (to-bool val)))))

;; Follows IsLooselyEqual semantic
(defun js-eq (left-val right-val)
    ;; If same type, follow streq semantic
    (if (eq (first left-val) (first right-val))
        (js-streq left-val right-val)
        ;; Check combos of values
        (alexandria:switch (`(,(first left-val) ,(first right-val)) :test equal)
            ;; null == undefined
            ('(:NullVal :UndefVal) '(:BoolVal t))
            ('(:UndefVal :NullVal) '(:BoolVal t))
            ;; If num and str, convert str to num and check equality
            ('(:NumVal :StrVal) (js-streq left-val (to-num right-val)))
            ('(:StrVal :NumVal) (js-streq (to-num left-val) right-val))
            ;; Checking if either is a boolean
            ;; If so, convert boolean to num then recursively re-evaluate
            (t 
                (if (eq (first left-val) :BoolVal)
                    (js-eq (to-num left-val) right-val)
                    (if (eq (first right-val) :BoolVal)
                        (js-eq left-val (to-num right-val))
                        ;; TODO: Add weird rules for objects and non-objects
                        ;; If nothing, return false
                        '(:BoolVal nil)))))))

;; Follows IsStrictlyEqual semantic
(defun js-streq (left-val right-val)
    (if (not (eq (first left-val) (first right-val)))
        '(:BoolVal nil)
        `(:BoolVal ,(alexandria:switch ((first left-val) :test 'eq)
            (:NumVal (equal (second left-val) (second right-val)))
            (:NullVal t)
            (:UndefVal t)
            (:StrVal (equal (second left-val) (second right-val)))
            (:BoolVal (eq (second left-val) (second right-val)))
            (:ObjRef (= (second left-val) (second right-val)))
            (t nil)))))

(defun js-typeof (val)
    `(:StrVal 
        ,(alexandria:switch ((first val) :test 'eq)
            (:StrVal "string")
            (:BoolVal "boolean")
            (:NumVal "number")
            (:UndefVal "undefined")
            (:NullVal "null")
            (:ObjVal "object")
            (:ClosureVal "function")
            (:RefVal (second (js-typeof (get-heap val))))
            (:ObjRef (second (js-typeof (get-heap val))))
            (t (error (format nil "Error: typeof not available for ~A" val))))))