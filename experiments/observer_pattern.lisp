

;; A simple example of the observer pattern in common lisp


(defclass eventnotifier()
  ((observers :accessor event-observers
	      :initform '())))


;;(defgeneric observer-subscribe (eventnotifier observer)
;;  (:documentation "add observer to eventnotifier"))

(defmethod observer-subscribe  ((en eventnotifier) observer)
    (push observer (event-observers en)))



;;(defgeneric notify-observers (eventnotifier)
;;  (:documentation "Notify subscribed observers."))

(defmethod notify-observers((en eventnotifier))
  (dolist (observer (event-observers en))
    (funcall observer)))







;; Testing

(defparameter event_notifier (make-instance 'eventnotifier))
  

(defun observer1()
  (format t "This is observer1 ~%"))



(observer-subscribe event_notifier  #'observer1)
(observer-subscribe event_notifier  #'(lambda() (format t "This is observer2 ~%")))
