


;;; Variable shadowing
;;; LET variables shadow variables binded outside of it.

(defun variable-shadow (x)
  (format t "THe value of x is ~a~%" x)
  (let ((x 1))
    (format t "THe value of x is ~a~%" x)
    (let ((x 2))
      (format t "THe value of x is ~a~%" x)
      (let ((x 3))
	(format t "THe value of x is ~a~%" x)))))


;;; Variables in the LET form can only be used in the body of the LET
;;; This won't compile. The y variable can't access the initial value of X
(let ((x 0) (y (+ x 1)))
  (format t "THe value of x and y is ~a and ~a~%" x y))

;;; The LET* form allows the initial values to be used other initial values.
(let* ((x 0) (y (+ x 1)))
  (format t "THe value of x and y is ~a and ~a~%" x y))



;;; Lexical scope and closures

;;; This demonstrates a closure.
;;; The anonymous function captures the binding of variable 'x'
(defparameter *fn*
  (let ((x 10))
    #'(lambda () (setf x (+ x 1)))))


;;; Repeated calls of the anonymous function increments the variable 'x'.
;;; CL-USER> (funcall *fn*)
;;; 11

;;; CL-USER> (funcall *fn*)
;;; 12

;;; CL-USER> (funcall *fn*)
;;; 13


;;; Dynamic variabiess

(defvar *count* 0
  "this is a count.")


(defparameter *adjustable-value*  100
  "A value that can be adjusted")


;;; DEFVAR and DEFPARAMETER are like global varibles.
;;; Can be use anywhere after assignment.

;;; Difference between defvar and defparameter:

;;; DEFPARAMETER will always assign a value to the named variable.
;;; DEFVAR will only assign a value if the variable is not defined.

;;; DEFVAR best used for value that should stay the saem after source code
;;; is changed.

;;; DEFPARAMETER are for values that you want to change after source code change.


(defvar *count* 0)

(defun print-count ()
  (format t "Before incf, *count* is ~d ~%" *count*)
  (incf *count*)
  (format t "After incf,  *count* is ~d ~%" *count*))


;;; this function demomstrates how LET handles dynamic variables like DEFVAR nad DERPARAMETER
;;; it knows to change the binding the special variable *count*  in toe body and
;;; and rebind it after the body.
(defun defvar-test()
  (print-count)
  (let ((*count* 100))
    (print-count))
  (print-count))



;;; In lisp, constants are declare with DEFCONSTANT
;;; Note the "+" below is just for naming convention
(defconstant +constant-1+   12)

;;; One thing about DEFCONSTANT is that it reserved the name of the constant.
;;; It can't even be used in function arguments.
;;; The below won't even compile.
;;; (defun test-constant-arg (+constant-1+)
;;;   (format t "The value is ~a ~%" +constant-1+))
 

;;; Although common lisp allows  DEFCONSTANT to be redefined, what happenes after
;;; is not well specpifed. Implementation speciific
;;; The below will cannot be compiled in Clozure Lisp
;;; (defconstant +constant-1+   14)


