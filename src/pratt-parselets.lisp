;;;; pratt-parselets.lisp

(in-package #:lisp-js)

;; Parselet for number tokens
(defun parse-number (token-stream)
    (destructuring-bind 
        (_ token-value)
        (first token-stream)
        ;; Return new NumVal and token stream
        ;; to the right of this operation
        (values 
            `(:NumVal ,token-value)
            (cdr token-stream))))

;; Parselet for boolean tokens
(defun parse-bool (token-stream)
    (destructuring-bind 
        (_ token-value)
        (first token-stream)
        ;; Return new BoolVal and token stream
        ;; to the right of this operation
        (values 
            `(:BoolVal ,token-value)
            (cdr token-stream))))

;; Parselet for string tokens
(defun parse-str (token-stream)
    (destructuring-bind 
        (_ token-value)
        (first token-stream)
        ;; Return new StrVal and token stream
        ;; to the right of this operation
        (values 
            `(:StrVal ,token-value)
            (cdr token-stream))))

;; Parselet for infix negative
(defun parse-prefix-negative (token-stream)
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
                `(:NegUop ,right)
                new-token-stream))))

;; Maps token type to its null denotation parselet
(defun null-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:NUMBER 'parse-number)
        (:BOOLEAN 'parse-bool)
        (:STRING 'parse-str)
        (:MINUS 'parse-prefix-negative)))

(defun parse-infix-plus (token-stream left)
    )

(defun left-denotations (token)
    (alexandria:switch ((first token) :test 'eq)
        (:PLUS 'parse-infix-plus)))