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
        ;; Assignment
        (:ASSIGN (values 15 16))
        (:ASPLUS (values 15 16))
        (:ASMINUS (values 15 16))
        (:ASEXPONENT (values 15 16))
        (:ASTIMES (values 15 16))
        (:ASDIV (values 15 16))
        (:ASMOD (values 15 16))
        (:ASLSHIFT (values 15 16))
        (:ASRSHIFT (values 15 16))
        (:ASURSHIFT (values 15 16))
        (:ASBITAND (values 15 16))
        (:ASXOR (values 15 16))
        (:ASBITOR (values 15 16))
        (:ASLOGAND (values 15 16))
        (:ASLOGOR (values 15 16))
        (:TERNARY (values 15 16))
        (:ARROW (values 15 16))
        ;; Parenthesis
        (:LPAREN (values 0 0))
        (:RPAREN (values 0 0))
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