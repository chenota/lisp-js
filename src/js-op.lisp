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