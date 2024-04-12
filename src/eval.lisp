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
                    (lambda (acc new) 
                        ;; Stop executing if find RETURN wrapper
                        (if (eq (first acc) :RETURN)
                            acc 
                            (stmt-eval new)))
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
                            (push-to-current-frame (second ident) (resolve-reference (expr-eval right)))
                            ;; If not const, assumed to be let, push to heap and store pointer on stack
                            (push-to-current-frame (second ident) (push-heap (resolve-reference (expr-eval right)))))
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
        (:Block
            ;; Extract parts of block 
            (destructuring-bind
                (_ body)
                stmt 
                (declare (ignore _))
                (progn
                    ;; New scope 
                    (push-empty-frame)
                    ;; Evaluate, capture result
                    (let ((eval-result (stmt-eval body)))
                        ;; Pop scope
                        (pop-frame)
                        ;; Return
                        eval-result))))
        (:ReturnStmt
            (destructuring-bind  
                (_ body)
                stmt 
                (declare (ignore _))
                `(:RETURN ,(expr-eval body))))
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
                                (progn 
                                    (set-heap operandval (js-plus (to-num (resolve-reference operandval)) '(:NumVal 1)))
                                    (resolve-reference operandval))
                                (error "ReferenceError: Invalid right-hand side expression in prefix operation"))))
                    (:DecUop 
                        (let ((operandval (expr-eval operand)))
                            ;; Check if incrementing reference
                            (if (eq (first operandval) :RefVal)
                                (progn 
                                    (set-heap operandval (js-minus (to-num (resolve-reference operandval)) '(:NumVal 1)))
                                    (resolve-reference operandval))
                                (error "ReferenceError: Invalid right-hand side expression in prefix operation"))))
                    (t (error (format nil "Made it to the end of PreOpExpr eval with ~A" operator))))))
        ;; Postfix operators
        (:PostOpExpr
            (destructuring-bind 
                (_ operator operand) 
                expr 
                (declare (ignore _))
                (alexandria:switch (operator :test 'eq)
                    (:IncUop 
                        (let ((operandval (expr-eval operand)))
                            ;; Check if incrementing reference
                            (if (eq (first operandval) :RefVal)
                                (progn 
                                    (set-heap operandval (js-plus (to-num (resolve-reference operandval)) '(:NumVal 1)))
                                    (js-minus (to-num (resolve-reference operandval)) '(:NumVal 1)))
                                (error "ReferenceError: Invalid left-hand side expression in postfix operation"))))
                    (:DecUop 
                        (let ((operandval (expr-eval operand)))
                            ;; Check if incrementing reference
                            (if (eq (first operandval) :RefVal)
                                (progn 
                                    (set-heap operandval (js-minus (to-num (resolve-reference operandval)) '(:NumVal 1)))
                                    (js-plus (to-num (resolve-reference operandval)) '(:NumVal 1)))
                                (error "ReferenceError: Invalid left-hand side expression in postfix operation"))))
                    (t (error (format nil "Made it to the end of PostOpExpr eval with ~A" operator))))))
        ;; Ternary
        (:TernExpr
            (destructuring-bind
                (_ test pass fail)
                expr 
                (declare (ignore _))
                (if (second (to-bool (resolve-reference (expr-eval test))))
                    (expr-eval pass)
                    (expr-eval fail))))
        ;; List
        (:ListExpr
            (destructuring-bind 
                (_ vals)
                expr 
                (declare (ignore _))
                `(:ObjRef 
                    ,(second 
                        (push-heap 
                            `(:ListObj 
                                ,(mapcar 
                                    (lambda (x) (push-heap (resolve-reference (expr-eval x)))) 
                                    vals)))))))
        ;; Index
        (:IdxExpr 
            (destructuring-bind
                (_ left idx)
                expr 
                (declare (ignore _))
                (let*
                    ;; Evaluate arguments
                    ((idxval (to-num (resolve-reference (expr-eval idx))))
                     (leftval (resolve-reference (expr-eval left)))
                     (leftobj (resolve-object leftval)))
                    ;; Do a bunch of checks then return
                    (if 
                        (and  
                            ;; List being indexed?
                            (eq (first leftobj) :ListObj)
                            ;; Not indexing w/ NaN?
                            (not (eq (second idxval) :NaN))
                            ;; Index >= 0?
                            (>= (second idxval) 0)
                            ;; Index < len(list)?
                            (< (second idxval) (length (second leftobj))))
                        ;; If all pass, index is valid so get value
                        (nth (floor (second idxval)) (second leftobj))
                        ;; Invalid index; return undefined
                        '(:UndefVal nil)))))
        ;; Function definition
        (:FuncExpr
            (destructuring-bind  
                (_ name params body)
                expr 
                (declare (ignore _))
                ;; Change FuncExpr to ClosureVal and also add all accessible variables
                `(:ObjRef ,(second (push-heap `(:ClosureObj ,name ,params ,body ,(compress-stack)))))))
        ;; Function call
        (:CallExpr
            (destructuring-bind
                (_ value args)
                expr
                (declare (ignore _))
                (let ((closval (resolve-object (resolve-reference (expr-eval value)))))
                    (if (eq (first closval) :ClosureObj)
                        (destructuring-bind 
                            (_ name params body env)
                            closval
                            (declare (ignore _))
                            (progn
                                ;; Push closure env to stack 
                                (push-frame env)
                                ;; Try to fill all params
                                (loop for param in params do  
                                    ;; Check if args left
                                    (if args 
                                        ;; If args left...
                                        (progn 
                                            ;; Push new value associated w/ argument
                                            (push-to-current-frame param (push-heap (resolve-reference (expr-eval (car args)))))
                                            ;; Update args list
                                            (setq args (cdr args)))
                                        ;; If not, return undefined
                                        (push-to-current-frame param '(:UndefVal nil))))
                                ;; Add fn name to env as const if exists
                                (if name 
                                    (push-to-current-frame name closval)
                                    nil)
                                ;; Finally, execute function body, pop stack, sanity check value
                                (let ((exec-result (stmt-eval body)))
                                    ;; Pop stack
                                    (pop-frame)
                                    ;; Block functions must return
                                    (if (eq (first body) :Block)
                                        (if (eq (first exec-result) :RETURN)
                                            (second exec-result)
                                            '(:UndefVal nil))
                                        exec-result))))
                        (error (format nil "TypeError: ~A is not a function" closval))))))
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
        ;; Sanity check: RETURN should never show up here
        (:RETURN
            (error "SyntaxError: Illegal return"))
        (t val)))

(defun resolve-reference (val)
    (alexandria:switch ((first val) :test 'eq)
        (:RefVal (get-heap val))
        (:RETURN (error "SyntaxError: Illegal return"))
        (t val)))

(defun resolve-object (val)
    (alexandria:switch ((first val) :test 'eq)
        (:ObjRef (get-heap val))
        (:RETURN (error "SyntaxError: Illegal return"))
        (t val)))

(defun resolve-all (val)
    (alexandria:switch ((first val) :test 'eq)
        (:RefVal (get-heap val))
        (:ObjRef (get-heap val))
        (:RETURN (error "SyntaxError: Illegal return"))
        (t val)))