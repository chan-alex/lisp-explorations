
;; CON Cells
;; CONS takes 2 arguments and returns a con cell contain the 2 values

(print
 (cons "a" "b"))    ;; ("a" . "b")

;; The 2 values are called the CAR and CDR.

(print
 (car (cons "a" "b")))  ;; "a"


(print
 (cdr (cons "a" "b")))  ;; "b"

;; CON cell can hold element of any type.

;; Both CAR and CDR are SETF-able places.

(defparameter *a_cons* (cons "a" "b"))

(print *a_cons*)
(setf (car *a_cons*) 1)
(setf (cdr *a_cons*) 2)
(print *a_cons*)


;; A list has a NIL at the end.

(cons 1 (cons 2 (cons 3 NIL)))
(cons 1 (cons 2 (cons 3 '())))


;; Because CON cells can hold any types so can list
(cons "a" (cons 2.0 (cons 'x NIL)))

;;; You can use FIRST and REST in place of CAR and CRD
(defparameter *list1* '(1 2 3 4 5))
(print (car *list1*))
(print (cdr *list1*))

(print (first *list1*))
(print (rest  *list1*))


;; functional style as applied to lists in lisp
;; Most of lisp's list manuplating functions are in functional style.
;; They return new values instead of modifying arguments in place.
;; THis is done so that the result can SHARE cons cell.


(print
 (append (list "a" "b") (list "c" "d")))

;; the above APPEND will return ("a" "b" "c" "d").
;; THe thing to note is that APPEND does not really create all 5 cons cell.
;; it just create the 1st two cells ("a" . "b") and linked it to the
;; ("c" "d") list.
;; This way of sharing structure saves works and memory while still giving the
;; same desired result.

;; This next section of code demonstrates this:

(defparameter *list1* '( "a"  "b"))
(defparameter *list2* '( "c"  "d"))
(defparameter *list3* (append *list1* *list2*))


(defun print_lists ()
  (print *list1*)
  (print *list2*)
  (print *list3*)
  NIL)

(print_lists)
(setf (car *list2*) 0)
(print_lists)   ; this will show not only *list2* has changed, so has *list3*.


;; One could say that APPEND is "destructively" or has side-effects.
;; And there can be described as "recycling" type of effects. APPEND reuses existing
;; cons cells tp do it's thing.


(defparameter *list4* '(1 2 3 4 5))


;; REVERSE is a functional. Returns a new list via CONS.
(print
 (reverse *list4*))

(print *list4*)    ; *list4* stays the same.

;; NREVERSE is a destructive version of REVERSE. It will modify *list4*
;; In general, function with recycling type of side effects starts with N.
(print
 (nreverse *list4*))

(print *list4*)  ;; *list4* is now (1).


;; N* functions generally are safe to use only if you are sure the arguments are not going
;; to be used after the functions returns. The side effects are not specified tighty enough
;; to be replied on.

;; There are a few N* functions who behvaior can be replied on.



;; Although destructive functions can be unsafe to use, there is an safe and idiomtic way to
;; to use them: building up a list via CONS-ing and then call the destructive function on this
;; newly created list.


(defun nreverse-random(n range)
  (let ((result NIL))
    (dotimes (i n)
      (push (random range) result))
    (print result)
    (nreverse result)))


(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))
