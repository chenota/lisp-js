;;;; tokenizer.lisp

(in-package #:lisp-js)

;; (ql:quickload "cl-ppcre")

;; Define scanners for tokens
(defparameter *js-token-scanners*
    ;; List of patterns and what the resultant token should be.
    ;; Precedence is given to tokens higher up on the list.\
    ;; Format: (TOKEN-PATTERN TOKEN-NAME)
    (let 
        ((tokens 
            '(("\\(" :LPAREN-KW)
              ("\\)" :RPAREN-KW))))
        ;; Loop through list and compile a regular expression for each pattern
        ;; Result is a list of (TOKEN-REGEX TOKEN-NAME)
        (mapcar 
            (lambda 
                (x) 
                (destructuring-bind 
                    (pattern result) 
                    x
                    `(,(cl-ppcre:create-scanner (concatenate 'string "^" pattern))
                      ,result)))
            tokens)))

;; Turn string into token list
(defun tokenize-string (target-string)
    (let 
        ;; Variables to hold important info
        ((start-idx 0)
         (end-idx (length target-string))
         (tokens nil))
        ;; Loop then return values, need progn to do this
        (progn
        ;; While loop through whole target string
            (loop while (< start-idx end-idx) do 
                ;; Go through all token scanners, grab first match
                (destructuring-bind
                    (token matched-string)
                    (reduce 
                        ;; Fold-left function
                        (lambda 
                            (acc new)
                            (if 
                                ;; If haven't found match...
                                (equal acc '(nil nil))
                                ;; Check next token scanner
                                (destructuring-bind
                                    (token-scanner result-token)
                                    new
                                    ;; Scan-to-strings returns multiple values,
                                    ;; capture both even though only care about first. 
                                    (multiple-value-bind 
                                        (matched-string _)
                                        (cl-ppcre:scan-to-strings 
                                            token-scanner
                                            target-string
                                            :start start-idx)
                                        (declare (ignore _))
                                        (if
                                            ;; If found a match...
                                            matched-string
                                            ;; Return token and string that matched it
                                            `(,result-token ,matched-string)
                                            ;; Otherwise, return nil nil
                                            '(nil nil))))
                                ;; Otherwise, return what already have
                                acc))
                        *js-token-scanners*
                        :initial-value '(nil nil))
                    (if
                        ;; If didn't find anything...
                        (equal `(,token ,matched-string) '(nil nil))
                        ;; Set tokens to nil, break the loop by making end-idx equal
                        ;; to start-idx (breaks loop while preserving value of start-idx
                        ;; to return later)
                        (setq tokens nil end-idx start-idx)
                        ;; Otherwise, append new token to tokens list and 
                        ;; increment start index to ignore string that just matched
                        (setq 
                            tokens (cons `(,token ,matched-string) tokens)
                            start-idx (+ start-idx (length matched-string))))))
                ;; Return reversed token list, since consing results reverses list
                ;; also return start-idx for potential error messages later
                (values (reverse tokens) start-idx))))