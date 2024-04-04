;;;; tokenizer.lisp

(in-package #:lisp-js)

;; Define scanners for tokens
(defparameter *js-token-scanners*
    ;; List of patterns and what the resultant token should be.
    ;; Precedence is given to tokens higher up on the list.
    ;; Format: (TOKEN-PATTERN TOKEN-NAME)
    (let 
        ((tokens 
              ;; Comments
            '(("//.*\\n" nil)
              ;; Printing
              ("console\\.log" :PRINT)
              ;; Strings
              ("(\\x22).*(\\x22)" :STRING)
              ("(\\x27).*(\\x27)" :STRING)
              ;; Semicolon
              (";" :SEMICOLON)
              ;; Grouping operators
              ("\\(" :LPAREN)
              ("\\)" :RPAREN)
              ("{" :LBRACKET)
              ("}" :RBRACKET)
              ("\\[" :LSQBRACKET)
              ("\\]" :RSQBRACKET)
              ;; Delimiters
              ("," :COMMA)
              (":" :COLON)
              ;; Assignment
              ("\\+=" :ASPLUS)
              ("-=" :ASMINUS)
              ("\\*\\*=" :ASEXPONENT)
              ("\\*=" :ASTIMES)
              ("/=" :ASDIV)
              ("%=" :ASMOD)
              ("<<=" :ASLSHIFT)
              (">>=" :ASRSHIFT)
              (">>>=" :ASURSHIFT)
              ("&=" :ASBITAND)
              ("\\^=" :ASXOR)
              ("\\|=" :ASBITOR)
              ("&&=" :ASLOGAND)
              ("\\|\\|=" :ASLOGOR)
              ;; Ternary operator
              ("\\?" :TERNARY)
              ;; Arithmetic operators
              ("%" :MOD)
              ("\\*\\*" :POWER)
              ("\\+\\+" :INCREMENT)
              ("--" :DECREMENT)
              ("\\+" :PLUS)
              ("-" :MINUS)
              ("\\*" :TIMES)
              ("/" :DIV)
              ;; Logical operators
              ("&&" :LOGAND)
              ("\\|\\|" :LOGOR)
              ;; Bitwise operators
              ("&" :BITAND)
              ("\\|" :BITOR)
              ("\\^" :XOR)
              ("~" :BITNOT)
              (">>>" :URSHIFT)
              ("<<" :LSHIFT)
              (">>" :RSHIFT)
              ;; Comparison operators
              (">=" :GTE)
              (">" :GT)
              ("<=" :LTE)
              ("<" :LT)
              ("===" :STREQ)
              ("==" :EQ)
              ("!==" :STRINEQ)
              ("!=" :INEQ)
              ;; Boolean
              ("false" :BOOLEAN)
              ("true" :BOOLEAN)
              ("!" :BANG)
              ;; Functions
              ("function" :FUNCTION)
              ("=>" :ARROW)
              ;; Assignment
              ("=" :ASSIGN)
              ;; Keywords
              ("return" :RETURN)
              ("const" :CONST)
              ("let" :LET)
              ("undefined" :UNDEFINED)
              ("for" :FOR)
              ("while" :WHILE)
              ("if" :IF)
              ("else" :ELSE)
              ("in" :IN)
              ;; Variables
              ("[a-zA-Z_$][a-zA-Z0-9_$]*" :IDENTIFIER)
              ;; Numbers
              ("((([0-9]+)(\\.?)([0-9]*))|(\\.[0-9]+))(e(\\+?|-)[0-9]+)?" :NUMBER)
              ;; Period
              ("\\." :DOT)
              ;; Whitespace
              ("\\s+" nil))))
        ;; Loop through list and compile a regular expression for each pattern
        ;; Result is a list of (TOKEN-REGEX TOKEN-NAME)
        (mapcar 
            (lambda 
                (x) 
                (destructuring-bind 
                    (pattern result) 
                    x
                    ;; Force the pattern to match at beginning of string
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
                ;; Clean token list then return with start idx
                (values 
                    ;; Reduce through entire token stream
                    (reduce 
                        (lambda 
                            (acc new)
                            ;; Destructure token into token type and value
                            (destructuring-bind
                                (token string-match)
                                new 
                                ;; Check for special case
                                (case token 
                                    ;; If NIL token, don't keep
                                    ('nil acc)
                                    ;; If NUMBER token, convert value to number
                                    (:NUMBER (cons `(,token ,(token-str-to-num string-match)) acc))
                                    ;; If BOOLEAN token, convert value to boolean
                                    (:BOOLEAN (cons `(,token ,(token-str-to-bool string-match)) acc))
                                    ;; If STRING token, get rid of quotes at first and last character
                                    ;; Basically reverses the string a couple of times and takes off first character
                                    (:STRING (cons `(,token ,(coerce (cdr (reverse (cdr (reverse (coerce string-match 'list))))) 'string)) acc))
                                    ;; Otherwise, keep original value
                                    (t (cons new acc)))))
                        ;; Add EOF token to end of token stream
                        (cons '(:EOF nil) tokens)
                        :initial-value nil)
                    start-idx))))