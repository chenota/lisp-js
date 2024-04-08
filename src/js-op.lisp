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
            ('(:NumVal :StrVal) `(:BoolVal ,(= (second left-val) (second (to-num right-val)))))
            ('(:StrVal :NumVal) `(:BoolVal ,(= (second (to-num left-val)) (second right-val))))
            ;; Checking if either is a boolean
            ;; If so, convert boolean to num then recursively re-evaluate
            (t 
                (if (eq (first left-val) :BoolVal)
                    (js-eq (to-num left-val) right-val)
                    (if (eq (first right-val) :BoolVal)
                        (js-eq left-val (to-num right-val))
                        ;; TODO: Add rules for objects
                        ;; If nothing, return false
                        '(:BoolVal nil)))))))

;; Follows IsStrictlyEqual semantic
(defun js-streq (left-val right-val)
    (if (not (eq (first left-val) (first right-val)))
        '(:BoolVal nil)
        `(:BoolVal ,(alexandria:switch ((first left-val) :test 'eq)
            (:NumVal (= (second left-val) (second right-val)))
            (:NullVal t)
            (:UndefVal t)
            (:StrVal (equal (second left-val) (second right-val)))
            (:BoolVal (eq (second left-val) (second right-val)))
            ;; TODO: Add rules for comparing by identity
            (t nil)))))