


;; Generic methods can augmented by auxilary methods.
;; These are methods that can be called before, after and around the primary method.

(defclass standard_english()
  ())

(defmethod speak ((se standard_english) sentence)
  (format t "~A " sentence))


(speak (make-instance 'standard_english) "I am hungry")




(defclass singlish_accent (standard_english)
  ())


;; This is an example of an "before" auxilary method
(defmethod speak :before ((sa singlish_accent) sentence)
  (format t "~A, " "Eh"))

;; THis is an example of an "after" auxiliary method
(defmethod speak :after ((sa singlish_accent) sentence)
  (format t "~A " "ah"))


(speak (make-instance 'singlish_accent) "I am hungry")



;; TODO to add around axx methods.
;; If there is an "around" method, it gets called 1st and the rest of the method will
;; only run if the around-method decides to let them.

;; An around primary method can use CALL-NEXT-METHOD to invoke the next method.
;; NEXT-METHOD-P can be invoke to test whether there is a next method to call.


(defclass appropriate_accent (standard_english)
  ((singlish_ok  :accessor singlish_ok
		 :initarg  :singlish_ok
		 :initform NIL)))
		 

(defmethod speak :around ((aa appropriate_accent) sentence)
  (if (singlish_ok aa)
      (speak (make-instance 'singlish_accent) sentence)
      (call-next-method)))
