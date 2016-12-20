


;;; COND

;;; Multi branch conditional can look ugly.

(defvar x 5)

(if (= x 1)
    (format t "x is equal to 1 ~%")
    (if (= x 2)
	(format t "x is equal to 2 ~%")
	(if (= x 3)
	    (format t "x is equal to 3 ~%")
	    (if (= x 4)
		(format t "x is equal to 4 ~%")
		(if (= x 5)
		    (format t "x is equal to 5 ~%"))))))


;;; COND is a better choice.
(COND
  ((= x 1) (format t "x is equal to 1 ~%"))
  ((= x 2) (format t "x is equal to 2 ~%"))
  ((= x 3) (format t "x is equal to 3 ~%"))
  ((= x 4) (format t "x is equal to 4 ~%"))
  ((= x 5) (format t "x is equal to 5 ~%")))


;;; COND is also a macro that expands to:
(IF (= X 1)
    (PROGN (FORMAT T "x is equal to 1 ~%"))
    (IF (= X 2)
	(PROGN (FORMAT T "x is equal to 2 ~%"))
	(IF (= X 3)
	    (PROGN (FORMAT T "x is equal to 3 ~%"))
	    (IF (= X 4)
		(PROGN(FORMAT T "x is equal to 4 ~%"))
		(IF (= X 5)
		    (PROGN(FORMAT T "x is equal to 5 ~%"))
		    NIL)))))


;;; Loop: DOLIST

;;; DOLIST loops through a list
(dolist (y `(1 2 3 4 5)) (print y))

;; 1 
;; 2 
;; 3 
;; 4 
;; 5 


;;; You can use RETURN to exit DOLIST early
(dolist (y `(1 2 3 4 5))
  (print y)
  (if (oddp y)
      (return)))


;;; DOMTIMES is counting loops

(dotimes (i 10)
  (format t "~d bottle on the wall ~%" i))

;;; You can also use RETURN to break out of the loop.

;;; DOMTIMES and DOLUST can be nested.

(dotimes (i 5)
  (dotimes (h 5)
    (print i)
    (print h)))


(dotimes (i 5)
  (dolist (y `(1 2 3 4 5))
    (print y)))
