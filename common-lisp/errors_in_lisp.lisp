

;; Lisp has a condition system which is similar to exception in other languages.
;; The condition system is more flexible because in addtion to signaling errors
;; and providing a way to handle errors, it also provides a way to restart from errors.



;; A condiion is an object whose class indicates the nature of the condition and
;; whose instance variable carry information about the details of the circumstances
;; that triggered the condition.


;; Conditions are defined by the DEFINE-CONDITION macro which is essentially the
;; same as DEFCLASS except the superclass of objects defined by it is CONDITION
;; instead of STANDARD-OBJECT.


