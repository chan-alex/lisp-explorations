
;; CONS takes 2 arguments and returns a con cell contain the 2 values

(print
 (cons "a" "b"))    ;; ("a" . "b")

;; The 2 values are called the CAR and CDR.

(print
 (car (cons "a" "b")))  ;; "a"


(print
 (cdr (cons "a" "b")))  ;; "b"


;; Both CAR and CDR are SETF-able places.

(defparameter *a_cons* (cons "a" "b"))

(print *a_cons*)
(setf (car *a_cons*) 1)
(setf (cdr *a_cons*) 2)
(print *a_cons*)
