

;; Lisp has a condition system which is similar to exception in other languages.
;; The condition system is more flexible because in addtion to signaling errors
;; and providing a way to handle errors, it also provides a way to restart from errors.


;; A condiion is an object whose class indicates the nature of the condition and
;; whose instance variable carry information about the details of the circumstances
;; that triggered the condition.


;; Conditions are defined by the DEFINE-CONDITION macro which is essentially the
;; same as DEFCLASS except the superclass of objects defined by it is CONDITION
;; instead of STANDARD-OBJECT. Also a condition's slots cannot be access by SLOT-VALUE.
;; You have to specify a :reader or :accessor option for any slot that you want to use.
;; MAKE-CONDITION initializes the slots of new conditions based on the :initargs it's passed.
;; THere is no further way to customize a condition.

;; :report is for the report function. Must have argments for the condition and stream.

(define-condition too-hot (error)
  ((temperature :initarg :temp
		:reader temp))
  (:report (lambda (condition stream)
	     (format stream  "Temperature now is ~A celsius. Too hot! " (temp condition)))))


;; There's 2 way to signal a condition

;; SIGNAL will not interrupt the flow of execution unless there's a HANDLE-CASE
(signal (make-condition 'too-hot :temp 40))

;; This will throw us into the debugger.
;;   (error (make-condition 'too-hot :temp 40))



  
