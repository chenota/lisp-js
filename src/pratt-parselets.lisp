;;;; pratt-parselets.lisp

(in-package #:lisp-js)

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
    ;; Parse the stream after the opening paren
    (multiple-value-bind 
        (right new-token-stream)
        ;; Reset binding power back to one then evaluate
        (expr-bp (cdr token-stream) 2)
        ;; Check for errors
        (progn 
            (if 
                (not (eq (first (first new-token-stream)) :RPAREN))
                (error "Parsing Error: No closing paren!")
                nil)
            ;; Return parsed expr, bump past rparen in new token stream
            (values 
                right
                (cdr new-token-stream)))
    ))

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
                    (expr-bp token-stream 2)
                    ;; Check that next token is a bracket
                    (if  
                        (eq (caar new-token-stream) :LBRACKET)
                        ;; If so, evaluate to get block
                        (multiple-value-bind  
                            (block newer-token-stream)
                            (expr-bp new-token-stream 2)
                            ;; Return funcexpr and token stream
                            (values 
                                `(:FuncExpr ,fn-name ,params ,block)
                                newer-token-stream))
                        ;; Otherwise, throw error
                        (error "Error: Expected LBRACKET after function parameters"))
                    )
                ;; Otherwise, throw error
                (error "Error: Expected LPAREN after function keyword")))))

(defun parse-sqbracket (token-stream)
     ;; Parse the stream after the opening bracket
    (multiple-value-bind 
        (right new-token-stream)
        ;; Reset binding power back to one then evaluate
        (expr-bp (cdr token-stream) 2)
        ;; Check for errors
        (progn 
            (if 
                (not (eq (caar new-token-stream) :RBRACKET))
                (error "Parsing Error: No closing bracket!")
                nil)
            ;; Return parsed expr, bump past rbracket in new token stream
            (values 
                `(:Block ,right)
                (cdr new-token-stream)))
    ))

;; Maps token type to its null denotation parselet
(defun null-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:NUMBER 'parse-primitive)
        (:BOOLEAN 'parse-primitive)
        (:STRING 'parse-primitive)
        (:IDENTIFIER 'parse-primitive)
        (:MINUS 'parse-prefix-operator)
        (:LPAREN 'parse-parens)
        (:LBRACKET 'parse-sqbracket)
        (:FUNCTION 'parse-function)
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
        (t nil)))

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
                `(:BopExpr ,left ,(get-bop (first token-stream)) ,right)
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

;; Maps token type to its left denotation parselet
(defun left-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:PLUS 'parse-infix-operator)
        (:MINUS 'parse-infix-operator)
        (:TIMES 'parse-infix-operator)
        (:DIV 'parse-infix-operator)
        (:POWER 'parse-infix-operator)
        (:BITOR 'parse-infix-operator)
        (:BITAND 'parse-infix-operator)
        (:SEMICOLON 'parse-semicolon)
        (:COMMA 'parse-comma)
        (t (error (format nil "Error: Reached end of left denotations map with token ~A~%" token)))))