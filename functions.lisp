

;;; Simple function that takes an arguent.
(defun hello-world (name)
  (format t "Hello, ~a" name))


;;; Simple function with 2 arguments
(defun verbose-sum (x y)
  (format t "The sum of ~d and ~d is ~d. ~%" x y (+ x y)))


;;; function with optional arguments
(defun optional-args (a b &optional c d)
  (list a b c d))

;;; (optional-args 1 )      --->   gives error
;;; ; Evaluation aborted on #<CCL::TOO-FEW-ARGUMENTS #xC83094E>.
;;; (optional-args 1 2)      --->  (1 2 NIL NIL)
;;; (optional-args 1 2 3)    --->  (1 2 3 NIL)
;;; (optional-args 1 2 3 4)  --->  (1 2 3 4)



;;; function with optional arguments with default values.
(defun optional-args2 (a b &optional (c 0) (d 0))
  (list a b c d))

;;; CL-USER> (optional-args2  1 2 )
;;; (1 2 0 0)
;;; CL-USER> (optional-args2  1 2 3 )
;;; (1 2 3 0)



;;; functions with optional arguments and "-supplied-p"
;;; THe "-supplied-p" variable let you know if the value is using the default
;;; value or not. This is unique in lisp.

(defun optional-args3 (a b &optional (c 0 c-supplied-p) (d 0 d-supplied-p))
  (list a b c c-supplied-p d  d-supplied-p))

;;; CL-USER> (optional-args3  1 2 3 )
;;; (1 2 3 T 0 NIL)



;;; function with the "&rest" args

(defun rest-args (a &rest rest)
  (list a rest))

;;; CL-USER> (rest-args 1 2 3 4 5)
;;; (1 (2 3 4 5))



;;; function with keyword args
(defun keyword-args1 (&key a b c)
  (list a b c))

;;; CL-USER> (keyword-args 1 2 3)
    ; Evaluation aborted on #<CCL::SIMPLE-PROGRAM-ERROR #xC7EF4D6>.
;;; CL-USER> (keyword-args :a 1 :b 2 :c 3)
;;; (1 2 3)
;;; CL-USER> (keyword-args :a 1 :b 2 )
;;; (1 2 NIL)
;;; CL-USER> (keyword-args :a 1 )
;;; (1 NIL NIL)



;;; function with keyword args, with default values and "-supplied-p"
(defun keyword-args2 (&key a (b 7) (c 8  c-supplied-p))
  (list a b c c-supplied-p))

;;; CL-USER> (keyword-args2 :a 1)
;;; (1 7 8 NIL)
;;; CL-USER> (keyword-args2 :c 5  :a 1)
;;; (1 7 5 T)
