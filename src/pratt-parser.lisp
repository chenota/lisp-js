;;;; pratt-parser.lisp

(in-package #:lisp-js)

;; Main expression parser
(defun expr (token-stream)
    (multiple-value-bind
        (syntax-tree _)
        (expr-bp token-stream 0)
        (declare (ignore _))
        syntax-tree))

;; Evaluate expressions with respect to binding power
;; This is the "main function" of the pratt parser
(defun expr-bp (token-stream min-bp)
    ;; Get the first token in token-stream.
    ;; This is the token to the left of everything else.
    ;; Call this token's null denotation to parse it.
    (multiple-value-bind
        (left new-token-stream)
        (funcall (null-denotations (first token-stream)) token-stream)
        (progn 
            ;; Update token stream to token stream after null denotation has been called
            (setq token-stream new-token-stream)
            ;; Loop while haven't reached the end of the token stream
            (loop while t do  
                (let 
                    ;; Next value after atom is assumed to be some operator
                    ((op (first token-stream)))
                    (progn
                        ;; If op is EOF, return
                        (if (eq (first op) :EOF) (loop-finish) nil)
                        ;; Get left and right binding power of the op
                        (multiple-value-bind
                            (l-bp r-bp)
                            (infix-binding-power op)
                            (progn 
                                ;; If left binding power of op is less than minimum binding power,
                                ;; break loop
                                (if (< l-bp min-bp) (loop-finish) nil)
                                ;; Call the left denotation of the current operator
                                ;; this will also change the token stream, so need to capture
                                ;; the new token stream as well as the parsed right side
                                (multiple-value-bind
                                    (new-left new-token-stream)
                                    (funcall (left-denotations (first token-stream)) token-stream left)
                                    (progn 
                                        (setq left new-left)
                                        (setq token-stream new-token-stream))))))))
            ;; Finally, return left and updated token stream
            (values left token-stream))))

;; Binding power for infix operations
;; returns (values left-bp right-bp) for each infix operator
(defun infix-binding-power (token)
    (alexandria:switch ((first token) :test 'eq)
        ;; Additive
        (:PLUS (values 50 51))
        (:MINUS (values 50 51))
        ;; Multiplicative
        (:TIMES (values 55 56))
        (:DIV (values 55 56))
        ;; Exponentiation (right associative)
        (:POWER (values 60 61))
        ;; Bit shift operators
        (:LSHIFT (values 45 46))
        (:RSHIFT (values 45 46))
        (:URSHIFT (values 45 46))
        ;; Relational operators
        (:LT (values 40 41))
        (:LTE (values 40 41))
        (:GT (values 40 41))
        (:GTE (values 40 41))
        ;; Equality operators
        (:EQ (values 35 36))
        (:STREQ (values 35 36))
        (:INEQ (values 35 36))
        (:STRINEQ (values 35 36))
        ;; Bitwise AND
        (:BITAND (values 30 31))
        ;; Bitwise XOR
        (:XOR (values 28 29))
        ;; Bitwise OR
        (:BITOR (values 25 26))
        ;; Logical AND
        (:LOGAND (values 22 23))
        ;; Logical OR
        (:LOGOR (values 20 21))
        ;; Assignment (right-associative)
        (:ASSIGN (values 16 15))
        (:ASPLUS (values 16 15))
        (:ASMINUS (values 16 15))
        (:ASEXPONENT (values 16 15))
        (:ASTIMES (values 16 15))
        (:ASDIV (values 16 15))
        (:ASMOD (values 16 15))
        (:ASLSHIFT (values 16 15))
        (:ASRSHIFT (values 16 15))
        (:ASURSHIFT (values 16 15))
        (:ASBITAND (values 16 15))
        (:ASXOR (values 16 15))
        (:ASBITOR (values 16 15))
        (:ASLOGAND (values 16 15))
        (:ASLOGOR (values 16 15))
        (:TERNARY (values 16 15))
        (:ARROW (values 16 15))
        ;; Parenthesis/brackets
        (:LPAREN (values 0 0))
        (:RPAREN (values 0 0))
        (:LBRACKET (values 0 0))
        (:RBRACKET (values 0 0))
        ;; Semicolon
        (:SEMICOLON (values 2 1))
        (t (values nil nil))))


;; Binding power for prefix operations
;; returns (values nil right-bp) for each prefix operator.
;; Note: prefix operators only have a right binding power, since
;; Nothing to the left
(defun prefix-binding-power (token)
    (let
        ((prefix-power 65))
        (alexandria:switch ((first token) :test 'eq)
            (:INCREMENT (values nil prefix-power))
            (:DECREMENT (values nil prefix-power))
            (:BANG (values nil prefix-power))
            (:BITNOT (values nil prefix-power))
            (:PLUS (values nil prefix-power))
            (:MINUS (values nil prefix-power))
            (t (values nil nil)))))

;; Binding power for postfix operations
(defun postfix-binding-power (token)
    (let 
        ((postfix-power 70))
        (alexandria:switch ((first token) :test 'eq)
            (:INCREMENT (values nil postfix-power))
            (:DECREMENT (values nil postfix-power))
            (t (values nil nil)))))