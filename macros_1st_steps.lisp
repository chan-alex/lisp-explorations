



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
      (format t "Do something here. ~%")))



;;; Having to use PROGN is bit verbose.
;;; Another way to do the same is to use WHEN
(when (< x 10)
      (format t "X is less then 10 ~%")
      (format t "Do something here. ~%"))

;;; WHEN is really a macro that translates to the previous IF

;;; CL-USER> (macroexpand-1 `(when (< x 10)
;;;      (format t "X is less then 10 ~%")
;;;      (format t "Do something here. ~%")))
;;; (IF (< X 10) (PROGN (FORMAT T "X is less then 10 ~%") (FORMAT T "Do something here. ~%")))
;;; T
;;; CL-USER>


;;; One way to write WHEN is this way:
(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))


(my-when (< x 10)
      (format t "X is less then 10 ~%")
      (format t "Do something here. ~%"))
