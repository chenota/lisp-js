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
                ;; Check which operator
                (alexandria:switch (operator :test 'eq)
                    ;; Basic arithmetic operators
                    ;; Plus has unique behavior when string is involved
                    (:PlusBop (js-plus (expr-eval left) (expr-eval right)))
                    ;; -, *, and / all should just convert to number then evaluate
                    (:MinusBop (js-minus (expr-eval left) (expr-eval right)))
                    (:TimesBop (js-times (expr-eval left) (expr-eval right)))
                    (:DivBop (js-div (expr-eval left) (expr-eval right)))
                    ;; Inequality operators
                    (:LtBop (js-lt (expr-eval left) (expr-eval right)))
                    (:LteBop (js-lte (expr-eval left) (expr-eval right)))
                    (:GtBop (js-gt (expr-eval left) (expr-eval right)))
                    (:GteBop (js-gte (expr-eval left) (expr-eval right)))
                    ;; Equality operators
                    (:EqBop (js-eq (expr-eval left) (expr-eval right)))
                    (:StrEqBop (js-streq (expr-eval left) (expr-eval right)))
                    (:InEqBop `(:BoolVal ,(not (second (js-eq (expr-eval left) (expr-eval right))))))
                    (:StrInEqBop `(:BoolVal ,(not (second (js-streq (expr-eval left) (expr-eval right))))))
                    ;; Logical operators
                    (:LogOrBop
                        (let ((first-operand (expr-eval left)))
                            (if (second (to-bool first-operand))
                                first-operand
                                (expr-eval right))))
                    (:LogAndBop
                        (let ((first-operand (expr-eval left)))
                            (if (second (to-bool first-operand))
                                (expr-eval right)
                                first-operand))))))
        ;; Prefix operators
        (:PreOpExpr
            (destructuring-bind 
                (_ operator operand) 
                expr 
                (declare (ignore _))
                (alexandria:switch (operator :test 'eq)
                    (:NegUop (js-negate (expr-eval operand)))
                    (:PosUop (js-abs (expr-eval operand))))))
        ;; Ternary
        (:TernExpr
            (destructuring-bind
                (_ test pass fail)
                expr 
                (declare (ignore _))
                (if (second (to-bool (expr-eval test)))
                    (expr-eval pass)
                    (expr-eval fail))))
        ;; If all else fails, attempt to evaluate as a value
        (t (val-eval expr))))

;; Val-eval is just an identity function, but may need to update in future or get rid of
(defun val-eval (val)
    val)