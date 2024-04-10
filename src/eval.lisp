;;;; eval.lisp

(in-package #:lisp-js)

(defun stmt-eval (stmt)
    ;; Get type of statement and eval accordingly
    (alexandria:switch ((first stmt) :test 'eq)
        ;; Eval list of statements
        (:StmtList
            ;; Extract list of stmts from AST 
            (destructuring-bind 
                (_ &rest args)
                stmt
                (declare (ignore _))
                ;; Eval each statement in list
                ;; basically reduce just goes through everything and returns final result
                (reduce 
                    (lambda (acc new) (stmt-eval new))
                    args 
                    ;; Initialize w/ undefined just to be safe
                    :initial-value '(:UndefVal nil))))
        ;; Const and let statements
        (:AssignStmt
            ;; Extract parts of assign
            (destructuring-bind
                (_ is-const ident right)
                stmt
                (declare (ignore _))
                ;; Make sure not redefining in current scope
                (if 
                    (search-current-frame (second ident))
                    ;; If so, throw error
                    (error (format nil "SyntaxError: Identifier '~A' has already been declared~%" (second ident)))
                    ;; Otherwise, good to go
                    (progn 
                        ;; Check if let or const
                        (if is-const 
                            ;; If const, store directly on stack
                            (push-current-frame (second ident) (expr-eval right))
                            ;; If not const, assumed to be let, push to heap and store pointer on stack
                            (push-current-frame (second ident) (push-heap (expr-eval right))))
                        ;; Always return undefined
                        '(:UndefVal nil)))))
        ;; x = y
        (:GenericAssign
            ;; Extract parts of assign
            (destructuring-bind
                (_ left right)
                stmt 
                (declare (ignore _))
                ;; Attempt to get variable on lhs of operator
                (let ((lval (expr-eval left)))
                    ;; Check if left is a reference
                    (if (eq (first lval) :RefVal)
                        ;; Set heap reference to new value and return value it was set to
                        (progn 
                            (set-heap lval (resolve-reference (expr-eval right)))
                            lval)
                        (error "TypeError: Assignment to constant variable")))))
        ;; If all fail, eval as expr
        (t (expr-eval stmt))))

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
                    (:PlusBop (js-plus (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    ;; -, *, and / all should just convert to number then evaluate
                    (:MinusBop (js-minus (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    (:TimesBop (js-times (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    (:DivBop (js-div (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    ;; Inequality operators
                    (:LtBop (js-lt (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    (:LteBop (js-lte (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    (:GtBop (js-gt (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    (:GteBop (js-gte (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    ;; Equality operators
                    (:EqBop (js-eq (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    (:StrEqBop (js-streq (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right))))
                    (:InEqBop `(:BoolVal ,(not (second (js-eq (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right)))))))
                    (:StrInEqBop `(:BoolVal ,(not (second (js-streq (resolve-reference (expr-eval left)) (resolve-reference (expr-eval right)))))))
                    ;; Logical operators
                    (:LogOrBop
                        (let ((first-operand (expr-eval left)))
                            (if (second (to-bool (resolve-reference first-operand)))
                                first-operand
                                (expr-eval right))))
                    (:LogAndBop
                        (let ((first-operand (expr-eval left)))
                            (if (second (to-bool (resolve-reference first-operand)))
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
                    (:PosUop (js-abs (expr-eval operand)))
                    (:BangUop (js-not (expr-eval operand)))
                    (:IncUop 
                        (let ((operandval (expr-eval operand)))
                            ;; Check if incrementing reference
                            (if (eq (first operandval) :RefVal)
                                (set-heap operandval (js-plus '(:NumVal 1) (to-num (resolve-reference operandval))))
                                (error "ReferenceError: Invalid right-hand side expression in prefix operation"))))
                    (:DecUop 
                        (let* 
                            ;; Calculate new operand value
                            ((operand-as-num (to-num (expr-eval operand)))
                             (op-result (js-minus operand-as-num '(:NumVal 1))))
                            ;; Update reference and return
                            (progn 
                                (stmt-eval `(:GenericAssign ,operand ,op-result))
                                op-result)))
                    (t (error (format nil "Made it to the end of PreOpExpr eval with ~A" operator))))))
        ;; Postfix operators
        (:PostOpExpr
            (destructuring-bind 
                (_ operator operand) 
                expr 
                (declare (ignore _))
                (alexandria:switch (operator :test 'eq)
                    (:IncUop 
                        (let* 
                            ;; Calculate new operand value
                            ((operand-as-num (to-num (expr-eval operand)))
                             (op-result (js-plus operand-as-num '(:NumVal 1))))
                            ;; Update reference and return
                            (progn 
                                (stmt-eval `(:GenericAssign ,operand ,op-result))
                                operand-as-num)))
                    (:DecUop 
                        (let* 
                            ;; Calculate new operand value
                            ((operand-as-num (to-num (expr-eval operand)))
                             (op-result (js-minus operand-as-num '(:NumVal 1))))
                            ;; Update reference and return
                            (progn 
                                (stmt-eval `(:GenericAssign ,operand ,op-result))
                                operand-as-num)))
                    (t (error (format nil "Made it to the end of PostOpExpr eval with ~A" operator))))))
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
    (alexandria:switch ((first val) :test 'eq)
        ;; If find identval, attempt to search the stack for the variable and return its value
        ;; if it exists.
        (:IdentVal 
            (let ((var-val (search-stack (second val))))
                (if var-val
                    (val-eval var-val) 
                    (error (format nil "ReferenceError: ~A is not defined~%" (second val))))))
        (t val)))

(defun resolve-reference (val)
    (alexandria:switch ((first val) :test 'eq)
        (:RefVal (get-heap val))
        (t val)))