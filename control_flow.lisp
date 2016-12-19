



(defparameter x 0)



;;; This is the standard IF usage.
(if (< x  10)
    (format t "X is less then 10 ~%")
    (format t "X is more then 10 ~%"))
    

;;; Use PROGN if you want to do a series of actions in the body of the IF.
;;; This pattern applies to other control flow structures too.
(if (< x  10)
    (progn
      (format t "X is less then 10 ~%")
      (format t "Do something here. ~%"))
    (progn
      (format t "X is more then 10 ~%")
      (format t "Do something here. ~%")))

