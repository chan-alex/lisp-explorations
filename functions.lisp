

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


;;; Note:  mixing &optional, &rest, &optional or &rest together can lead to surprising
;;; behavior. Take care when doing so.



;;; functions returns the last evaluated value.
(defun return1 (x)
  (if (< x 10)
      0
      1))

;;; It is possible to use RETURN-FROM to explicit return.
;;; Actually RETURN-FROM returns from BLOCKS and since DEFUN
;;; wraps functions body in a BLOCK, this works too.
(defun return2 (x)
  (if (< x 10)
      (return-from return2 0)
      (return-from return2 1)))


;;; functions are 1st class objects.
(defun times2 (x)
  (* x 2))

#'times2
;;; returns #<Compiled-function TIMES2 #xC835296>

;;; (times2 20) is same as
(funcall #'times2 20)

;;; FUNCALL is used when you know the exact number of args to call the function with.
;;; Use APPLY when the number of arguments is only know at run time.
;;; APPLY only expect the args to be in a list.
(apply #'times2 '(20))


;;; Anonymous functions can be created with lambda.
(lambda (x) (* 2 x))

;;; CL-USER> (lambda (x) (* 2 x))
;;; #<Anonymous Function #xC8B3B76>

;;; They can be called with FUNCALL or APPLY
;;; CL-USER> (funcall #'(lambda (x) (* 2 x)) 20)
;;; 40

;;; CL-USER> (apply #'(lambda (x) (* 2 x)) '(20))
;;; 40

;;; or called directly.
;;; CL-USER> ((lambda (x) (* 2 x)) 20)
;;; 40


