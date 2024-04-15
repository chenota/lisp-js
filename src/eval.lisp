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
                    (error (format nil "SyntaxError: Identifier '~A' has already been declared" (second ident)))
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
                (let* ((lval (expr-eval left)))
                    ;; Check if left is a reference
                    (if (eq (first lval) :RefVal)
                        ;; Set heap reference to new value and return value it was set to
                        (progn 
                            (set-heap lval (resolve-reference (expr-eval right)))
                            lval)
                        ;; If not refval, see if is idx or dot assignment
                        (alexandria:switch ((first left) :test 'eq)
                            (:IdxExpr
                                ;; Get object being indexed
                                (let ((lval-res (resolve-reference (expr-eval (second left))))
                                      (rval (resolve-reference (expr-eval right))))
                                    ;; make sure object being indexed is actually an object
                                    (if (eq (first lval-res) :ObjRef)
                                        (progn
                                            ;; Update heap w/ new object
                                            (set-heap
                                                lval-res 
                                                `(:ObjVal 
                                                    ;; Cons new value onto existing object
                                                    ,(cons
                                                        ;; New value is conscell
                                                        (cons 
                                                            ;; Key is coerced into a string
                                                            (to-str (resolve-reference (expr-eval (third left)))) 
                                                            ;; Value is evaluated right side
                                                            (push-heap rval))
                                                        (second (get-heap lval-res)))))
                                            rval)
                                        ;; If not object, return right side and move on
                                        (expr-eval right))))
                            (:DotExpr 
                                ;; Get object being indexed
                                (let ((lval-res (resolve-reference (expr-eval (second left))))
                                      (rval (resolve-reference (expr-eval right))))
                                    ;; make sure object being indexed is actually an object
                                    (if (eq (first lval-res) :ObjRef)
                                        (progn 
                                            ;; Update heap w/ new object
                                            (set-heap
                                                lval-res 
                                                `(:ObjVal 
                                                    ;; Cons new value onto existing object
                                                    ,(cons
                                                        ;; New value is conscell
                                                        (cons 
                                                            ;; Key is coerced into a string
                                                            `(:StrVal ,(third left))
                                                            ;; Value is evaluated right side
                                                            (push-heap rval))
                                                        (second (get-heap lval-res)))))
                                            rval)
                                        ;; If not object, throw a syntax error
                                        (error (format nil "SyntaxError: Invalid or unexpected token ~A" lval-res)))))
                            (t (error "TypeError: Assignment to constant variable")))))))
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
        (:IfStmt
            (destructuring-bind
                (_ test if-blk else-blk)
                stmt 
                (declare (ignore _))
                ;; Run the test
                (if (second (to-bool (resolve-reference (expr-eval test))))
                    ;; Run if-blk if true
                    (stmt-eval if-blk)
                    ;; Check if else exists then run if false
                    (if else-blk
                        (stmt-eval else-blk)
                        '(:UndefVal nil)))))
        (:ForStmt
            (destructuring-bind
                (_ init test after body)
                stmt
                (declare (ignore _))
                (progn 
                    ;; New scope b/c init could possibly create new variable
                    (push-empty-frame)
                    ;; Eval given statement
                    (stmt-eval init)
                    ;; Run loop
                    (let ((last-value '(:UndefVal nil)))
                        (loop while (second (to-bool (resolve-reference (expr-eval test)))) do 
                            (setq last-value (resolve-reference (stmt-eval body)))
                            (expr-eval after))
                        ;; Pop stack
                        (pop-frame)
                        ;; Return last value
                        last-value))))
        (:WhileStmt
            (destructuring-bind  
                (_ test body)
                stmt 
                (declare (ignore _))
                (let ((last-value '(:UndefVal nil)))
                    ;; Check condition before looping
                    (loop while (second (to-bool (resolve-reference (expr-eval test)))) do 
                        (setq last-value (resolve-reference (stmt-eval body))))
                    ;; Return last value
                    last-value)))
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
                            `(:ObjVal
                                ,(let ((index -1)) 
                                    (mapcar 
                                        (lambda (x) (incf index) 
                                            (cons (to-str `(:NumVal ,index)) (push-heap (resolve-reference (expr-eval x))))) 
                                        vals))))))))
        ;; Generic object
        (:ObjExpr
            (destructuring-bind
                (_ vals)
                expr 
                (declare (ignore _))
                `(:ObjRef
                    ,(second
                        (push-heap
                            `(:ObjVal
                                ,(mapcar 
                                    (lambda (x) 
                                        (cons (to-str (car x)) (push-heap (resolve-reference (expr-eval (cdr x))))))
                                    vals)))))))
        (:IdxExpr 
            (destructuring-bind
                (_ left idx)
                expr 
                (declare (ignore _))
                (let*
                    ;; Evaluate arguments
                    ((idxval (resolve-reference (expr-eval idx)))
                     (leftval (resolve-reference (expr-eval left)))
                     (leftobj (resolve-object leftval)))
                    (alexandria:switch ((first leftobj) :test 'eq)
                        (:ObjVal 
                            (let ((accval (to-str idxval)))
                                (reduce
                                    (lambda (acc new)
                                        (if (second (js-streq (car new) accval))
                                            (cdr new)
                                            acc))
                                    (second leftobj)
                                    :initial-value '(:UndefVal nil))))
                        (t '(:UndefVal nil))))))
        (:DotExpr
            (destructuring-bind
                (_ left idx)
                expr 
                (declare (ignore _))
                (let*
                    ;; Evaluate arguments
                    ((leftval (resolve-reference (expr-eval left)))
                     (leftobj (resolve-object leftval)))
                    (alexandria:switch ((first leftobj) :test 'eq)
                        (:ObjVal 
                            (let ((accval `(:StrVal ,idx)))
                                (reduce
                                    (lambda (acc new)
                                        (if (second (js-streq (car new) accval))
                                            (cdr new)
                                            acc))
                                    (second leftobj)
                                    :initial-value '(:UndefVal nil))))
                        (t '(:UndefVal nil))))))
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
                    (alexandria:switch ((first closval) :test 'eq)
                        ;; User-defined function
                        (:ClosureObj
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
                                            exec-result)))))
                        ;; Print fn prints all arguments
                        (:PrintFn
                            (progn 
                                (loop for x in args do 
                                    (format t "~A~%" (second (to-str (resolve-reference (expr-eval x))))))
                                '(:UndefVal nil)))
                        (t (error (format nil "TypeError: ~A is not a function" closval)))))))
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
                    (error (format nil "ReferenceError: ~A is not defined" (second val))))))
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