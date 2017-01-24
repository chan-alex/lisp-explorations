


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
