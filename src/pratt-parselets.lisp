;;;; pratt-parselets.lisp

(in-package #:lisp-js)

(defparameter grouping-bp 4)

;; Helper function: Extracts strings from list of identvals
(defun extract-ident-str (identlist)
    (reduce 
        (lambda (new acc)
            (if (eq (car new) :IdentVal)
                (cons (cadr new) acc)
                (error (format nil "ParserError: Extract-ident-str expected IDENTVAL, got ~A" new))))
        identlist
        :initial-value nil 
        :from-end t))

;; Return the value identifier associated with each token type
(defun get-primitive (token)
    (alexandria:switch ((first token) :test 'eq)
        (:NUMBER :NumVal)
        (:BOOLEAN :BoolVal)
        (:STRING :StrVal)
        (:IDENTIFIER :IdentVal)
        (:PRINT :PrintFn)
        (:EXIT :ExitFn)
        (:INPUT :InputFn)
        (:NUMBERFN :NumberFn)
        (:STRINGFN :StringFn)
        (:BOOLEANFN :BooleanFn)
        (:RANDOMFN :RandomFn)
        (:SIZE :SizeFn)
        (:KEYS :KeysFn)
        (t (error (format nil "ParserError: Reached end of primitive map with token ~A" token)))))

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

(defun parse-null (token-stream)
    ;; Return null and token stream
    ;; to the right of this operation
    (values 
        `(:NullVal nil)
        (cdr token-stream)))

;; Return the Uop identifier associated with each operator
(defun get-uop (token)
    (alexandria:switch ((first token) :test 'eq)
        (:MINUS :NegUop)
        (:PLUS :PosUop)
        (:BITNOT :BitNotUop)
        (:BANG :BangUop)
        (:INCREMENT :IncUop)
        (:DECREMENT :DecUop)
        (:TYPEOF :TypeofUop)
        (t (error (format nil "ParserError: Reached end of uop map with token ~A" token)))))

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
                `(:PreOpExpr ,(get-uop (first token-stream)) ,right)
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
            (expr-bp (cdr token-stream) 1)
            ;; Check for errors
            (progn 
                (if 
                    (not (eq (caar new-token-stream) :RPAREN))
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
                        ((temp-name (cadar token-stream)))
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
                    (parse-parens token-stream)
                    ;; Check that next token is a bracket
                    (if  
                        (eq (caar new-token-stream) :LBRACKET)
                        ;; If so, evaluate to get block
                        (multiple-value-bind  
                            (block newer-token-stream)
                            (parse-bracket new-token-stream)
                            ;; Return funcexpr and token stream
                            (values 
                                ;; Parse params so flat list of strings
                                (alexandria:switch ((car params) :test 'eq)
                                    (:UNIT `(:FuncExpr ,fn-name nil ,block))
                                    (:COMMALIST `(:FuncExpr ,fn-name ,(extract-ident-str (cadr params)) ,block))
                                    (t `(:FuncExpr ,fn-name ,(extract-ident-str (list params)) ,block)))
                                newer-token-stream))
                        ;; Otherwise, throw error
                        (error "Error: Expected LBRACKET after function parameters"))
                    )
                ;; Otherwise, throw error
                (error "Error: Expected LPAREN after function keyword")))))

(defun parse-bracket (token-stream)
    ;; Check for immediate closing bracket
    (if (eq (first (second token-stream)) :RBRACKET)
        (values 
            `(:ObjExpr nil)
            (cddr token-stream))
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
                    ;; Since 'block' of colonexprs is actually an object, check for that condition here
                    (if (eq (first right) :CommaList)
                        ;; Attempt to parse as object
                        (reduce 
                            (lambda (acc new)
                                (if 
                                    (and 
                                        (eq (first new) :ColonExpr)
                                        (eq (first acc) :ObjExpr))
                                    `(:ObjExpr ,(cons (cons (second new) (third new)) (second acc)))
                                    `(:Block ,right)))
                            (second right)
                            :initial-value '(:ObjExpr nil))
                        ;; Check if just one colon
                        (if (eq (first right) :ColonExpr)
                            `(:ObjExpr ,(list (cons (second right) (third right))))
                            ;; Normal if not commalist or colonexpr
                            `(:Block ,right)))
                    (cdr new-token-stream))))))

(defun parse-list (token-stream)
    ;; Check if immediate closing bracket
    (if (eq (caadr token-stream) :RSQBRACKET)
        ;; If so, return empty list expression
        (values
            `(:ListExpr nil)
            ;; Bump past [ and ] in token stream
            (cddr token-stream))
        ;; Otherwise, evaluate until reach closing bracket
        (multiple-value-bind 
            (right new-token-stream)
            ;; Reset binding power back to two then evaluate
            (expr-bp (cdr token-stream) grouping-bp)
            ;; Check for errors
            (if 
                (not (eq (first (first new-token-stream)) :RSQBRACKET))
                (error "Parsing Error: List must be closed by ]")
                (values 
                    ;; If commalist inside of brackets, extract values from commalist
                    (if (eq (car right) :COMMALIST)
                        `(:ListExpr ,@(cdr right))
                        `(:ListExpr (,right)))
                    (cdr new-token-stream))))))

(defun parse-const (token-stream)
    ;; Parse the stream after the const keyword
    (multiple-value-bind 
        (right new-token-stream)
        ;; Set BP to two so stops at semicolon
        (expr-bp (cdr token-stream) 3)
        ;; A const must be associated with a generic assignment
        (if (eq (car right) :GENERICASSIGN)
            (values 
                `(:AssignStmt t ,@(cdr right))
                new-token-stream)
            (error "Error: A const must be followed by a generic assign"))))

(defun parse-let (token-stream)
    ;; Parse the stream after the const keyword
    (multiple-value-bind 
        (right new-token-stream)
        ;; Set BP to two so stops at semicolon
        (expr-bp (cdr token-stream) 3)
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
            (t (error "ParserError: A let must be followed by a generic assign or an identifier")))))

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

(defun parse-if (token-stream)
    ;; Get rid of if keyword
    (progn 
        (setq token-stream (cdr token-stream))
        ;; Check if next thing is an lparen
        (if (eq (caar token-stream) :LPAREN)
            ;; If so, parse lparen as null denotation
            (multiple-value-bind 
                (inparens new-token-stream)
                (parse-parens token-stream)
                ;; Check if next token is an lbracket
                (if (eq (caar new-token-stream) :LBRACKET)
                    ;; If so, parse as a null denotation
                    (multiple-value-bind
                        (if-blk newer-token-stream)
                        (parse-bracket new-token-stream)
                        ;; Check if else after bracket
                        (if (eq (caar newer-token-stream) :ELSE)
                            ;; If so, parse whatever is after and return result
                            (multiple-value-bind 
                                (else-blk newest-token-stream)
                                ;; Skip past else so it doesn't get evaluated
                                (expr-bp (cdr newer-token-stream) 3)
                                (values 
                                    `(:IfStmt ,inparens ,if-blk ,else-blk)
                                    newest-token-stream))
                            ;; Otherwise, just return current result
                            (values 
                                `(:IfStmt ,inparens ,if-blk nil)
                                newer-token-stream)))
                    ;; If not, parse whatever is after and return result
                    (multiple-value-bind
                        (if-blk newer-token-stream)
                        (expr-bp new-token-stream 3)
                        (values 
                            `(:IfStmt ,inparens ,if-blk nil)
                            newer-token-stream))))
            ;; Otherwise, error
            (error (format nil "ParserError: Expected LPAREN after IF keyword, got ~A" (caar token-stream))))))

(defun parse-for (token-stream)
    ;; Check for lparen next in token-stream
    (if (eq (caadr token-stream) :LPAREN)
        ;; If so, parse paren expression 
        (multiple-value-bind 
            (conds new-token-stream)
            (parse-parens (cdr token-stream))
            ;; Check if get semicolon list
            (if (and (eq (car conds) :STMTLIST) (= (length conds) 4))
                ;; If so, move on and parse the rest
                (multiple-value-bind 
                    (right newer-token-stream)
                    (expr-bp new-token-stream 3)
                    (values
                        `(:ForStmt ,(second conds) ,(third conds) ,(fourth conds) ,right)
                        newer-token-stream))
                ;; Error: Must have STMT
                (error (format nil "ParserError: Must have three statements in FOR loop~A" conds))))
    ;; Otherwise, error
    (error (format nil "ParserError: A for loop must be followed by an LPAREN token"))))

(defun parse-while (token-stream)
    ;; Check for lparen next in token-stream
    (if (eq (caadr token-stream) :LPAREN)
        ;; If so, parse paren expression 
        (multiple-value-bind 
            (cond new-token-stream)
            (parse-parens (cdr token-stream))
            ;; Get expression after while loop
            (multiple-value-bind 
                (right newer-token-stream)
                (expr-bp new-token-stream 3)
                (values 
                    `(:WhileStmt ,cond ,right)
                    newer-token-stream)))
    ;; Otherwise, error
    (error (format nil "Error: A while loop must be followed by an LPAREN token"))))

(defun parse-nan (token-stream)
    (values 
        `(:NumVal :NaN)
        (cdr token-stream)))

;; Maps token type to its null denotation parselet
(defun null-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:NUMBER 'parse-primitive)
        (:BOOLEAN 'parse-primitive)
        (:STRING 'parse-primitive)
        (:IDENTIFIER 'parse-primitive)
        (:UNDEFINED 'parse-undefined)
        (:MINUS 'parse-prefix-operator)
        (:PLUS 'parse-prefix-operator)
        (:BITNOT 'parse-prefix-operator)
        (:BANG 'parse-prefix-operator)
        (:INCREMENT 'parse-prefix-operator)
        (:DECREMENT 'parse-prefix-operator)
        (:LPAREN 'parse-parens)
        (:LBRACKET 'parse-bracket)
        (:LSQBRACKET 'parse-list)
        (:FUNCTION 'parse-function)
        (:CONST 'parse-const)
        (:LET 'parse-let)
        (:RETURN 'parse-return)
        (:IF 'parse-if)
        (:FOR 'parse-for)
        (:WHILE 'parse-while)
        (:PRINT 'parse-primitive)
        (:EXIT 'parse-primitive)
        (:INPUT 'parse-primitive)
        (:NUMBERFN 'parse-primitive)
        (:STRINGFN 'parse-primitive)
        (:BOOLEANFN 'parse-primitive)
        (:SIZE 'parse-primitive)
        (:RANDOMFN 'parse-primitive)
        (:NaN 'parse-nan)
        (:NULL 'parse-null)
        (:TYPEOF 'parse-prefix-operator)
        (:KEYS 'parse-primitive)
        (t (error (format nil "ParserError: Reached end of null denotations map with token ~A" token)))))

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
        (:XOR :XorBop)
        (:LSHIFT :LShiftBop)
        (:RSHIFT :RShiftBop)
        (:URSHIFT :URShiftBop)
        (:LT :LtBop)
        (:LTE :LteBop)
        (:GT :GtBop)
        (:GTE :GteBop)
        (:EQ :EqBop)
        (:STREQ :StrEqBop)
        (:INEQ :InEqBop)
        (:STRINEQ :StrInEqBop)
        (:REM :RemBop)
        (:IN :InBop)
        (t (error (format nil "ParserError: Reached end of infix operators map with token ~A" token)))))

;; Generic parse infix operator function
(defun parse-infix-operator (token-stream left)
    ;; Get binding powers of infix operator
    (multiple-value-bind 
        (_ r-bp)
        (infix-binding-power (first token-stream))
        (declare (ignore _))
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
        (_ r-bp)
        (infix-binding-power (first token-stream))
        (declare (ignore _))
        ;; Evaluate right side of semicolon
        (handler-case 
            (multiple-value-bind
                (right new-token-stream)
                (expr-bp (cdr token-stream) r-bp)
                ;; Return new token stream and statement wrapper
                (values
                    (if 
                        (eq (car right) :STMTLIST)
                        `(:STMTLIST ,left ,@(cdr right))
                        `(:STMTLIST ,left ,right))
                    new-token-stream))
            ;; If error, return left and move on
            (error 
                ()
                (values 
                    left
                    (cdr token-stream))))))

(defun parse-comma (token-stream left)
    ;; Get binding powers of comma
    (multiple-value-bind 
        (_ r-bp)
        (infix-binding-power (first token-stream))
        (declare (ignore _))
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
        (parse-list token-stream)
        ;; Going to return a listexpr, basically just rename it to idxexpr and add left side
        (values 
            `(:IdxExpr ,left ,@(cadr right))
            new-token-stream)))

(defun parse-call (token-stream left)
    (multiple-value-bind
        (right new-token-stream)
        ;; Evaluate parens as if it was a null denotation
        (parse-parens token-stream)
        ;; Make a call expr
        (values 
            (alexandria:switch ((car right) :test 'eq)
                (:UNIT `(:CallExpr ,left nil))
                (:COMMALIST `(:CallExpr ,left ,(cadr right)))
                (t `(:CallExpr ,left (,right))))
            new-token-stream)))

;; Return the assignment operator identifier associated with each token type
(defun get-assign-op (token)
    (alexandria:switch ((first token) :test 'eq)
        (:ASSIGN :GenericAssign)
        (:ASPLUS :PlusAssign)
        (:ASMINUS :MinusAssign)
        (:ASTIMES :TimesAssign)
        (:ASREM :RemAssign)
        (:ASLSHIFT :LShiftAssign)
        (:ASRSHIFT :RShiftAssign)
        (:ASURSHIFT :URShiftAssign)
        (:ASBITAND :BitAndAssign)
        (:ASXOR :XorAssign)
        (:ASBITOR :BitOrAssign)
        (:ASLOGAND :LogAndAssign)
        (:ASLOGOR :LogOrAssign)
        (t (error (format nil "ParserError: Reached end of infix operators map with token ~A" token)))))

;; Generic parse assignment operator function
(defun parse-assign-operator (token-stream left)
    ;; Get binding powers of infix operator
    (multiple-value-bind 
        (_ r-bp)
        (infix-binding-power (first token-stream))
        (declare (ignore _))
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
        (_ r-bp)
        (infix-binding-power (first token-stream))
        (declare (ignore _))
        ;; Evaluate right side of arrow
        (multiple-value-bind 
            (right new-token-stream)
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new arrow func expression
            (values 
                ;; Parse params so flat list of strings
                (alexandria:switch ((car left) :test 'eq)
                    (:UNIT `(:FuncExpr nil nil ,right))
                    (:COMMALIST `(:FuncExpr nil ,(extract-ident-str (cadr left)) ,right))
                    (t `(:FuncExpr nil ,(extract-ident-str (list left)) ,right)))
                new-token-stream))))

(defun parse-colon (token-stream left)
    ;; Get binding powers of arrow
    (multiple-value-bind 
        (_ r-bp)
        (infix-binding-power (first token-stream))
        (declare (ignore _))
        ;; Evaluate right side of arrow
        (multiple-value-bind 
            (right new-token-stream)
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new arrow func expression
            (values 
                `(:ColonExpr ,left ,right)
                new-token-stream))))

(defun get-postfix (token)
    (alexandria:switch ((first token) :test 'eq)
        (:INCREMENT :IncUop)
        (:DECREMENT :DecUop)
        (t (error (format nil "ParserError: Reached end of postfix operator map with token ~A" token)))))

(defun parse-postfix-operator (token-stream left)
    (values 
        `(:PostOpExpr ,(get-postfix (car token-stream)) ,left)
        (cdr token-stream)))

(defun parse-ternary (token-stream left)
    ;; Get binding powers of ternary
    (multiple-value-bind
        (_ r-bp)
        (infix-binding-power (first token-stream))
        (declare (ignore _))
        ;; Evaluate right side of ternary
        (multiple-value-bind
            (if-true new-token-stream)
            (expr-bp (cdr token-stream) r-bp)
            ;; Check if next token is a colon
            (if (eq (caar new-token-stream) :Colon)
                ;; Get if-false clause
                (multiple-value-bind
                    (if-false newer-token-stream)
                    (expr-bp (cdr new-token-stream) (infix-binding-power (first token-stream)))
                    ;; Extract right and left from colon operator as true/false
                    (values 
                        `(:TernExpr ,left ,if-true ,if-false)
                        newer-token-stream))
                ;; Otherwise, error
                (error (format nil "ParserError: ternary expression requires a :~A" new-token-stream))))))

(defun parse-dot (token-stream left)
    ;; Get binding powers of dot
    (multiple-value-bind
        (_ r-bp)
        (infix-binding-power (first token-stream))
        (declare (ignore _))
        ;; Eval right
        (multiple-value-bind
            (right new-token-stream)
            (expr-bp (cdr token-stream) r-bp)
            ;; Check next token, make sure is identifier
            (if (eq (car right) :IdentVal)
                ;; Make DotExpr if valid
                (values 
                    `(:DotExpr ,left ,(second right))
                    new-token-stream)
                ;; Error if not identifier
                (error (format nil "Error: A DOT must be followed by an identval"))))))

(defun parse-special-assignment (token-stream left)
    ;; Get binding powers of infix operator
    (multiple-value-bind 
        (_ r-bp)
        (infix-binding-power (first token-stream))
        (declare (ignore _))
        ;; Evaluate right side of operator
        (multiple-value-bind
            (right new-token-stream)
            ;; Token stream is advanced during this function call,
            ;; no need to advance it separately
            (expr-bp (cdr token-stream) r-bp)
            ;; Return new token stream and parsed infix operator
            (values
                `(:SpecialAssign ,(get-assign-op (first token-stream)) ,left ,right)
                new-token-stream))))

;; Maps token type to its left denotation parselet
(defun left-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:PLUS 'parse-infix-operator)
        (:MINUS 'parse-infix-operator)
        (:TIMES 'parse-infix-operator)
        (:DIV 'parse-infix-operator)
        (:POWER 'parse-infix-operator)
        (:LT 'parse-infix-operator)
        (:LTE 'parse-infix-operator)
        (:GT 'parse-infix-operator)
        (:GTE 'parse-infix-operator)
        (:EQ 'parse-infix-operator)
        (:STREQ 'parse-infix-operator)
        (:INEQ 'parse-infix-operator)
        (:STRINEQ 'parse-infix-operator)
        (:LOGOR 'parse-infix-operator)
        (:LOGAND 'parse-infix-operator)
        (:BITOR 'parse-infix-operator)
        (:BITAND 'parse-infix-operator)
        (:XOR 'parse-infix-operator)
        (:LSHIFT 'parse-infix-operator)
        (:RSHIFT 'parse-infix-operator)
        (:URSHIFT 'parse-infix-operator)
        (:ASSIGN 'parse-assign-operator)
        (:SEMICOLON 'parse-semicolon)
        (:COMMA 'parse-comma)
        (:LSQBRACKET 'parse-index)
        (:LPAREN 'parse-call)
        (:ARROW 'parse-arrow-func)
        (:COLON 'parse-colon)
        (:INCREMENT 'parse-postfix-operator)
        (:DECREMENT 'parse-postfix-operator)
        (:TERNARY 'parse-ternary)
        (:DOT 'parse-dot)
        (:REM 'parse-infix-operator)
        (:ASPLUS 'parse-special-assignment)
        (:ASMINUS 'parse-special-assignment)
        (:ASTIMES 'parse-special-assignment)
        (:ASREM 'parse-special-assignment)
        (:ASLSHIFT 'parse-special-assignment)
        (:ASRSHIFT 'parse-special-assignment)
        (:ASURSHIFT 'parse-special-assignment)
        (:ASBITAND 'parse-special-assignment)
        (:ASXOR 'parse-special-assignment)
        (:ASBITOR 'parse-special-assignment)
        (:ASLOGAND 'parse-special-assignment)
        (:ASLOGOR 'parse-special-assignment)
        (:IN 'parse-infix-operator)
        (t (error (format nil "ParserError: Reached end of left denotations map with token ~A" token)))))