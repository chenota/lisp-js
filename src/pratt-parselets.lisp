;;;; pratt-parselets.lisp

(in-package #:lisp-js)

;; Return the value identifier associated with each token type
(defun get-primitive (token)
    (alexandria:switch ((first token) :test 'eq)
        (:NUMBER :NumVal)
        (:BOOLEAN :BoolVal)
        (:STRING :StrVal)))

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
        (:MINUS :NegUop)))

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
        ;; Reset binding power back to zero then evaluate
        (expr-bp (cdr token-stream) 1)
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

;; Maps token type to its null denotation parselet
(defun null-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:NUMBER 'parse-primitive)
        (:BOOLEAN 'parse-primitive)
        (:STRING 'parse-primitive)
        (:MINUS 'parse-prefix-operator)
        (:LPAREN 'parse-parens)
        (t nil)))

;; Return the Bop identifier associated with each token type
(defun get-bop (token)
    (alexandria:switch ((first token) :test 'eq)
        (:PLUS :PlusBop)
        (:MINUS :MinusBop)
        (:TIMES :TimesBop)
        (:DIV :DivBop)
        (:POWER :PowBop)
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

;; Maps token type to its left denotation parselet
(defun left-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:PLUS 'parse-infix-operator)
        (:MINUS 'parse-infix-operator)
        (:TIMES 'parse-infix-operator)
        (:DIV 'parse-infix-operator)
        (:POWER 'parse-infix-operator)
        (t nil)))