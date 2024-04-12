;; js-env.lisp

(in-package :lisp-js)

;; Push a new empty frame onto stack
(defparameter *stack* '(nil))

;; Pop frame off of stack
(defun pop-frame nil 
    (if (<= (length *stack*) 1)
        (error "Error: Attempted to pop last stack frame!")
        (setq *stack* (cdr *stack*))))

;; Push variable onto current stack frame
(defun push-to-current-frame (name value)
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

;; Compress whole stack into one frame, remove duplicate variables
(defun compress-stack (&key frame-num) 
    ;; Variable list
    (first (reduce 
        (lambda (compressed-stack new-frame)
            (let 
                ((frame-vars 
                    (reduce
                        (lambda (compressed-frame new-var)
                            (if  
                                ;; Check if variable already in stack
                                (member (car new-var) (cdr compressed-stack) :test 'string=)
                                ;; If so don't do anything
                                compressed-frame 
                                ;; If not, add to compressed frame
                                (cons new-var compressed-frame)))
                        new-frame
                        :initial-value nil)))
                ;; Update compressed stack w/ new variables
                (cons 
                    ;; New variables on stack
                    (append frame-vars (car compressed-stack)) 
                    ;; Store new variable names so don't pull again
                    (reduce (lambda (acc new) (cons (first new) acc)) frame-vars :initial-value (cdr compressed-stack)))))
        *stack*
        :initial-value (cons nil nil))))

(defun push-frame (frame)
    (setq *stack* (cons frame *stack*)))

(defun push-empty-frame nil
    (setq *stack* (cons nil *stack*)))

(defparameter *heap* nil)

(defun push-heap (value)
    (progn 
        (setf *heap* (nconc *heap* (list value)))
        `(:RefVal ,(- (length *heap*) 1))))

(defun get-heap (ref)
    (if 
        (and 
            (or 
                (eq (first ref) :RefVal)
                (eq (first ref) :ObjRef))
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