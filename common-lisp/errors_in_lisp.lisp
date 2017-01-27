

;; By default, signalling an error invokes the debugger.

;;    (/3 0)  ; this will cause an error to be signalling.


;; Errors can also be signalled using ERROR
;;    (error "Got problem!")


;; Other ways of signalling errors include ECASE . This is like CASE except it signals
;; error if no clause match

;; (ecase 1
;;  (2 3)
;;  (4 5))
;;  Will signal "ovalue 1 is not of the expected type (MEMBER 2 4)."

;; This is ok
(ecase 2
  (2 3)
  (4 5))


;; CHECK-TYPE macro takes a place, a type and an optional string.
;; It signals a correctable error if the value of the place is not of the expected type.
;; The handler will give an option of giving a new value.

(let ((some-value "wrong"))
  (check-type some-value integer "an integer")   ; this will drop into the  debugger 
  (format t "The value of some-value is: ~a ~%" some-value))


;; CHECK-TYPE is defined in terms of ASSERT which takes a test expression and a list of
;; one of more places, follow by argument to give to ERROR.

(let ((curry 'mutton))
  (assert (eql curry 'chicken) ; this is the test
	  (curry)   ; this what gets set in the debugger if the user choose to do so.
	  "I want ~A curry!" 'chicken)
  (format t "I got ~a curry!  ~%" curry))



;; Handling erroers.

;; The easiest way to handle errors is with IGNORE-ERRORS.
;; This macro behaves like PROGN if none of its argument causes an error.
;; If an error is signalled, it will immediately return two values: NIL and the
;; condition that was signalled.
(ignore-errors (/ 1 0))


;;  CL-USER> (ignore-errors (/ 1 0))
;;  ;Compiler warnings :
;;  ;   In an anonymous lambda form: Error: "DIVISION-BY-ZERO detected
;;  ;   performing / on (1 0)" 
;;  ;   signalled during compile-time evaluation of (/ 1 0) .
;;  NIL
;;  #<DIVISION-BY-ZERO #xCD68AC6>



;; Another way of handling error is with HANDLER-CASE
;; Like case, its first argument is evaluated and used to determine what happens next.
;; If no error is signalled, the value of the expression is returned.
;; If an error occurs, the follow clauses are searched for one that matches the type
;; of the error.
(defun div(x y)
  (handler-case (/ x y)
    (division-by-zero  () (format t "divide-by-zero error caught"))
    (arithmetic-error  () (format t "arithmetic-error caught"))
    (type-error  () (format t "type-error caught"))))



