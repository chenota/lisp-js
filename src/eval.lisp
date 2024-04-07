;;;; eval.lisp

(in-package #:lisp-js)

;; Recursively evaluate an expression
(defun expr-eval (expr)
    ;; Get type of expression and eval accordingly
    (alexandria:switch ((first expr) :test 'eq)
        ;; Binary operators
        (:BopExpr
            ;; Get info from children
            (destructuring-bind 
                (_ operator left right)
                expr 
                (declare (ignore _))
                (let 
                    ;; Evaluate left and right sides to get their values
                    ((left-val (expr-eval left))
                     (right-val (expr-eval right)))
                    ;; Check which operator
                    (alexandria:switch (operator :test 'eq)
                        ;; Plus has unique behavior when string is involved
                        (:PlusBop (js-plus left-val right-val))
                        ;; -, *, and / all should just convert to number then evaluate
                        (:MinusBop (js-minus left-val right-val))
                        (:TimesBop (js-times left-val right-val))
                        (:DivBop (js-div left-val right-val))))))
        ;; If all else fails, attempt to evaluate as a value
        (t (val-eval expr))))

;; Val-eval is just an identity function, but may need to update in future or get rid of
(defun val-eval (val)
    val)