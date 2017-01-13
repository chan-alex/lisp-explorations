


;; Association lists or alists are lists of cons cells
;; Can do lookup with them similar to hash tables. For small number of items, can be
;; faster than hash tables.

(defparameter *alist1* '((1 . "one") (2 . "two") (3 . "three") (4 . "four")))

(print
 (assoc 3 *alist1*))   ; prints (3 . "three")


(defparameter *alist2* '(( "one" . 1) ("two" . 2) ("three" . 3) ("four" . 4)))

;; ASSOC uses EQL for testing. Which is why the below prints NIL
(print
 (assoc "three" *alist2*))  ; prints NIL


;; the solution is to pass in a different comparing function via :test keyword.
(print
 (assoc "three" *alist2* :test #'string=))  ; prints ("three" . 3)


;; ACONS is used to add a key/value pair to a list. It is non-modifying so you have to use
;; SETF or PUSH

(setf *alist2* (acons "five" 5 *alist2*))
(push (acons "six" 6 *alist2*) *alist2*)


