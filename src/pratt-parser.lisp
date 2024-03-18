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
                            (infix-binding-power (first op))
                            (progn 
                                ;; If left binding power of op is less than minimum binding power,
                                ;; break loop
                                (if (< l-bp min-bp) (loop-finish) nil)
                                ;; Advance the token stream to "bump past" the operator
                                (setq token-stream (cdr token-stream))
                                ;; Parse the right-hand side of the current operator
                                ;; this will also change the token stream, so need to capture
                                ;; the new token stream as well as the parsed right side
                                (multiple-value-bind
                                    (right new-token-stream)
                                    (expr-bp token-stream r-bp)
                                    (progn 
                                        ;; Update the token stream
                                        (setq token-stream new-token-stream)
                                        ;; Update our left side to include parsed right side
                                        (setq left `(:BopExpr ,left ,(first op) ,right)))))))))
            ;; Finally, return left and updated token stream
            (values left token-stream))))

;; Binding power for infix operations
;; returns (values left-bp right-bp) for each infix operator
(defun infix-binding-power (operator)
    (alexandria:switch (operator :test 'eq)
        (:PLUS (values 1 2))
        (:MINUS (values 1 2))
        (:TIMES (values 3 4))
        (:DIV (values 3 4))
        (:POWER (values 8 7))
        (t (values nil nil))))


;; Binding power for prefix operations
;; returns (values nil right-bp) for each prefix operator.
;; Note: prefix operators only have a right binding power, since
;; Nothing to the left
(defun prefix-binding-power (token)
    (alexandria:switch ((first token) :test 'eq)
        (:MINUS (values nil 6))
        (t (values nil nil))))