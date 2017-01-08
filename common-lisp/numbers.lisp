
;;; Numbers can be different textual form but will be translated to the same
;;; object resprestations

;;; both 10 and 20/2 are the same.
(print 10)
(print 20/2)

;;; As is #xa
(print #xa)

;;; 2/3 and 4/6 are the same.
(print 2/3)
(print 4/6)


;;; Number is different bases.

;;; #B or #b are for binary

(print #B10111)
(print #b10111)

;;; (print #B10112) this throws an error.


;;; #O or #o are for octal numbers
(print #O717)
(print #o717)

;;; #X or #x are for hexadecimal numbers
(print #XABC)
(print #xabc)


;;; floating numbers
;;; Common Lisp defines four subtypes of floating-point number: short, single, double, and long.
(print 1.0)
(print 1e0)
(print 0.345)
(print .345)
(print 345e-3)
(print 345E-3)

;;; COmplex numbers
(print #c(1 2))    ; 1 + 2j
(print #c(2/3   3/4))
(print #c(1 0))


