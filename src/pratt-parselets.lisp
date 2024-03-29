;;;; pratt-parselets.lisp

(in-package #:lisp-js)

(defparameter grouping-bp 5)

;; Return the value identifier associated with each token type
(defun get-primitive (token)
    (alexandria:switch ((first token) :test 'eq)
        (:NUMBER :NumVal)
        (:BOOLEAN :BoolVal)
        (:STRING :StrVal)
        (:IDENTIFIER :IdentVal)
        (t (error (format nil "Error: Reached end of primitive map with token ~A~%" token)))))

;; Parse a primitive value
(defun parse-primitive (token-stream)
    ;; Return new XVal and token stream
    ;; to the right of this operation
    (values 
        `(,(get-primitive (first token-stream)) ,(second (first token-stream)))
        (cdr token-stream)))

(defun parse-undefined (token-stream)
    ;; Return undefined and token stream
    ;; to the right of this operation
    (values 
        `(:UndefVal nil)
        (cdr token-stream)))

;; Return the Uop identifier associated with each operator
(defun get-uop (token)
    (alexandria:switch ((first token) :test 'eq)
        (:MINUS :NegUop)
        (t (error (format nil "Error: Reached end of uop map with token ~A~%" token)))))

;; Parselet for infix negative
(defun parse-prefix-operator (token-stream)
    ;; Get binding power of prefix operator
    (multiple-value-bind 
        (_ r-bp)
        (prefix-binding-power (first token-stream))
        (declare (ignore _))
        ;; Evaluate right side of operator
        (multiple-value-bind
            (right new-token-stream)
            ;; Token stream is advanced during this function call,
            ;; no need to advance it separately
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new token stream and parsed infix operator
            (values
                `(,(get-uop (first token-stream)) ,right)
                new-token-stream))))

(defun parse-parens (token-stream)
    ;; Check if next token is a closing paren
    (if  
        (eq (caadr token-stream) :RPAREN)
        ;; If so, return a UNIT value
        (values 
            '(:UNIT nil)
            (cddr token-stream))
        ;; Otherwise, parse the stream after the opening paren
        (multiple-value-bind 
            (right new-token-stream)
            ;; Reset binding power back to one then evaluate
            (expr-bp (cdr token-stream) grouping-bp)
            ;; Check for errors
            (progn 
                (if 
                    (not (eq (first (first new-token-stream)) :RPAREN))
                    (error "Parsing Error: No closing paren!")
                    nil)
                ;; Return parsed expr, bump past rparen in new token stream
                (values 
                    right
                    (cdr new-token-stream))))))

(defun parse-function (token-stream)
    (progn 
        ;; Advance token stream, don't really care about function keyword
        (setq token-stream (cdr token-stream))
        ;; Check for function name, which is an ident
        (let  
            ((fn-name 
                (if 
                    (eq (caar token-stream) :IDENTIFIER)
                    ;; If next token is identifier, then extract token value and advance token stream
                    (let 
                        ;; Have to store name before advance past it
                        ((temp-name (car token-stream)))
                        (progn 
                            (setq token-stream (cdr token-stream))
                            temp-name))
                    ;; Otherwise, set function name to nil
                    nil)))
            ;; Check that lparen is next token
            (if  
                (eq (caar token-stream) :LPAREN)
                ;; If lparen is next, keep parsing right side to get parameters
                (multiple-value-bind 
                    (params new-token-stream)
                    ;; Reset binding power back to one then evaluate
                    (expr-bp token-stream grouping-bp)
                    ;; Check that next token is a bracket
                    (if  
                        (eq (caar new-token-stream) :LBRACKET)
                        ;; If so, evaluate to get block
                        (multiple-value-bind  
                            (block newer-token-stream)
                            (expr-bp new-token-stream grouping-bp)
                            ;; Return funcexpr and token stream
                            (values 
                                `(:FuncExpr ,fn-name ,params ,block)
                                newer-token-stream))
                        ;; Otherwise, throw error
                        (error "Error: Expected LBRACKET after function parameters"))
                    )
                ;; Otherwise, throw error
                (error "Error: Expected LPAREN after function keyword")))))

(defun parse-bracket (token-stream)
     ;; Parse the stream after the opening bracket
    (multiple-value-bind 
        (right new-token-stream)
        ;; Reset binding power back to one then evaluate
        (expr-bp (cdr token-stream) 2)
        ;; Check for errors
        (progn 
            (if 
                (not (eq (caar new-token-stream) :RBRACKET))
                (error "No closing bracket!")
                nil)
            ;; Return parsed expr, bump past rbracket in new token stream
            (values 
                `(:Block ,right)
                (cdr new-token-stream)))
    ))

(defun parse-list (token-stream)
    ;; Parse the stream after the opening bracket
    (multiple-value-bind 
        (right new-token-stream)
        ;; Reset binding power back to two then evaluate
        (expr-bp (cdr token-stream) grouping-bp)
        ;; Check for errors
        (progn 
            (if 
                (not (eq (first (first new-token-stream)) :RSQBRACKET))
                (error "Parsing Error: No closing square bracket!")
                nil)
            ;; Return parsed expr, bump past r sq bracket in new token stream
            (values 
                `(:ListExpr ,right)
                (cdr new-token-stream)))
    ))

(defun parse-const (token-stream)
    ;; Parse the stream after the const keyword
    (multiple-value-bind 
        (right new-token-stream)
        ;; Set BP to two so stops at semicolon
        (expr-bp (cdr token-stream) grouping-bp)
        ;; A const must be associated with a generic assignment
        (if (eq (car right) :GENERICASSIGN)
            (values 
                `(:AssignStmt t ,@(cdr right))
                new-token-stream)
            (error "Error: A const must be followed by a generic assign!"))))

(defun parse-let (token-stream)
    ;; Parse the stream after the const keyword
    (multiple-value-bind 
        (right new-token-stream)
        ;; Set BP to two so stops at semicolon
        (expr-bp (cdr token-stream) grouping-bp)
        ;; A const must be associated with a generic assignment OR an identifier
        (alexandria:switch ((car right) :test 'eq)
            ;; If let x = y, do same thing as const
            (:GENERICASSIGN
                (values 
                    `(:AssignStmt nil ,@(cdr right))
                new-token-stream))
            ;; If just let x;, assign undefined to x
            (:IDENTVAL
                (values 
                    `(:AssignStmt nil ,right (:UndefVal nil))
                new-token-stream))
            ;;(t (error "Error: A let must be followed by a generic assign or an identifier"))
            (t (error (format nil "~A~%" right))))))

(defun parse-return (token-stream)
    ;; Parse the stream after the return keyword
    (multiple-value-bind 
        (right new-token-stream)
        ;; Reset BP to two so stops at semicolon
        (expr-bp (cdr token-stream) grouping-bp)
        ;; Return a returnstmt with whatever is to the right
        (values 
            `(:ReturnStmt ,right)
            new-token-stream)))

;; Maps token type to its null denotation parselet
(defun null-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:NUMBER 'parse-primitive)
        (:BOOLEAN 'parse-primitive)
        (:STRING 'parse-primitive)
        (:IDENTIFIER 'parse-primitive)
        (:UNDEFINED 'parse-undefined)
        (:MINUS 'parse-prefix-operator)
        (:LPAREN 'parse-parens)
        (:LBRACKET 'parse-bracket)
        (:LSQBRACKET 'parse-list)
        (:FUNCTION 'parse-function)
        (:CONST 'parse-const)
        (:LET 'parse-let)
        (:RETURN 'parse-return)
        (t (error (format nil "Error: Reached end of null denotations map with token ~A~%" token)))))

;; Return the Bop identifier associated with each token type
(defun get-bop (token)
    (alexandria:switch ((first token) :test 'eq)
        (:PLUS :PlusBop)
        (:MINUS :MinusBop)
        (:TIMES :TimesBop)
        (:DIV :DivBop)
        (:POWER :PowBop)
        (:BITOR :BitOrBop)
        (:BITAND :BitAndBop)
        (:LOGOR :LogOrBop)
        (:LOGAND :LogAndBop)
        (t (error (format nil "Error: Reached end of infix operators map with token ~A~%" token)))))

;; Generic parse infix operator function
(defun parse-infix-operator (token-stream left)
    ;; Get binding powers of infix operator
    (multiple-value-bind 
        (l-bp r-bp)
        (infix-binding-power (first token-stream))
        ;; Evaluate right side of operator
        (multiple-value-bind
            (right new-token-stream)
            ;; Token stream is advanced during this function call,
            ;; no need to advance it separately
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new token stream and parsed infix operator
            (values
                `(:BopExpr ,(get-bop (first token-stream)) ,left ,right)
                new-token-stream))))

(defun parse-semicolon (token-stream left)
    ;; Get binding powers of semicolon
    (multiple-value-bind 
        (l-bp r-bp)
        (infix-binding-power (first token-stream))
        ;; Evaluate right side of semicolon
        (multiple-value-bind
            (right new-token-stream)
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new token stream and statement wrapper
            (values
                (if 
                    (eq (car right) :STMTLIST)
                    `(:STMTLIST (,left ,@(car (cdr right))))
                    `(:STMTLIST (,left ,right)))
                new-token-stream))))

(defun parse-comma (token-stream left)
    ;; Get binding powers of comma
    (multiple-value-bind 
        (l-bp r-bp)
        (infix-binding-power (first token-stream))
        ;; Evaluate right side of comma
        (multiple-value-bind
            (right new-token-stream)
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new token stream and comma wrapper
            (values
                (if 
                    (eq (car right) :COMMALIST)
                    `(:COMMALIST (,left ,@(car (cdr right))))
                    `(:COMMALIST (,left ,right)))
                new-token-stream))))

(defun parse-index (token-stream left)
    (multiple-value-bind
        (right new-token-stream)
        ;; Evaluate bracket as if it was a null denotation
        (expr-bp token-stream grouping-bp)
        ;; Going to return a listexpr, basically just rename it to idxexpr and add left side
        (values 
            `(:IdxExpr ,left ,(cadr right))
            new-token-stream)))

;; Return the assignment operator identifier associated with each token type
(defun get-assign-op (token)
    (alexandria:switch ((first token) :test 'eq)
        (:ASSIGN :GenericAssign)
        (t (error (format nil "Error: Reached end of infix operators map with token ~A~%" token)))))

;; Generic parse assignment operator function
(defun parse-assign-operator (token-stream left)
    ;; Get binding powers of infix operator
    (multiple-value-bind 
        (l-bp r-bp)
        (infix-binding-power (first token-stream))
        ;; Evaluate right side of operator
        (multiple-value-bind
            (right new-token-stream)
            ;; Token stream is advanced during this function call,
            ;; no need to advance it separately
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new token stream and parsed infix operator
            (values
                `(,(get-assign-op (first token-stream)) ,left ,right)
                new-token-stream))))

(defun parse-arrow-func (token-stream left)
    ;; Get binding powers of arrow
    (multiple-value-bind 
        (l-bp r-bp)
        (infix-binding-power (first token-stream))
        ;; Evaluate right side of arrow
        (multiple-value-bind 
            (right new-token-stream)
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new arrow func expression
            (values 
                `(:ArrowFuncExpr ,left ,right)
                new-token-stream))))

(defun parse-colon (token-stream left)
    ;; Get binding powers of arrow
    (multiple-value-bind 
        (l-bp r-bp)
        (infix-binding-power (first token-stream))
        ;; Evaluate right side of arrow
        (multiple-value-bind 
            (right new-token-stream)
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new arrow func expression
            (values 
                `(:ColonExpr ,left ,right)
                new-token-stream))))

;; Maps token type to its left denotation parselet
(defun left-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:PLUS 'parse-infix-operator)
        (:MINUS 'parse-infix-operator)
        (:TIMES 'parse-infix-operator)
        (:DIV 'parse-infix-operator)
        (:POWER 'parse-infix-operator)
        (:LOGOR 'parse-infix-operator)
        (:LOGAND 'parse-infix-operator)
        (:BITOR 'parse-infix-operator)
        (:BITAND 'parse-infix-operator)
        (:ASSIGN 'parse-assign-operator)
        (:SEMICOLON 'parse-semicolon)
        (:COMMA 'parse-comma)
        (:LSQBRACKET 'parse-index)
        (:ARROW 'parse-arrow-func)
        (:COLON 'parse-colon)
        (t (error (format nil "Error: Reached end of left denotations map with token ~A~%" token)))))