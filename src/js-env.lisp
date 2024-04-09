;; js-env.lisp

(in-package :lisp-js)

(defparameter *stack* '(nil))

;; Push a new empty frame onto stack
(defun push-frame nil 
    (setq *stack* (cons nil *stack*)))

;; Pop frame off of stack
(defun pop-frame nil 
    (if (<= (length *stack*) 1)
        (error "Error: Attempted to pop last stack frame!")
        (setq *stack* (cdr *stack*))))

;; Push variable onto current stack frame
(defun push-current-frame (name value)
    (setf (first *stack*)
        (cons 
            ;; New conscell associating name to value
            (cons name value) 
            ;; Cons new key-value onto first stack frame
            (first *stack*))))

;; Search a single stack frame
(defun search-frame (frame name)
    (reduce 
        (lambda (acc new-var)
            (if 
                (and 
                    (not acc) 
                    (equal (car new-var) name))
                (cdr new-var)
                acc))
        frame 
        :initial-value nil))

(defun search-current-frame (name)
    (search-frame (first *stack*) name))

;; Search whole stack for variable
(defun search-stack (name)
    ;; Loop through all stack frames
    (reduce
        (lambda (acc new-frame)
            (if  
                (not acc)
                (search-frame new-frame name)
                acc))
        *stack*
        :initial-value nil))

(defparameter *heap* nil)

(defun push-heap (value)
    (progn 
        (setf *heap* (cons value *heap*))
        `(:RefVal ,(- (length *heap*) 1))))

(defun get-heap (ref)
    (if 
        (and 
            (eq (first ref) :RefVal)
            (>= (second ref) 0)
            (< (second ref) (length *heap*)))
        (nth (second ref) *heap*)
        (error (format nil "Error: Invalid heap access with ~A~%" ref))))

(defun set-heap (ref value)
    (if 
        (and 
            (eq (first ref) :RefVal)
            (>= (second ref) 0)
            (< (second ref) (length *heap*)))
        (setf (nth (second ref) *heap*) value)
        (error (format nil "Error: Invalid heap access with ~A~%" ref))))