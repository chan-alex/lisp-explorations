


(defclass counter()
  ((count :accessor counter-count
	  :initform 0)))

(defmethod increment ((c counter))
  (incf (counter-count c)))

(defmethod decrement ((c counter))
  (when (> (counter-count c) 0)
    (decf (counter-count c))))


(defparameter c1 (make-instance 'counter)) 

(increment c1)
(increment c1)
(decrement c1)
(decrement c1)
(decrement c1)
(print (counter-count c1))
