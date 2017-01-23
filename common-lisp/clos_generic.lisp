

(defclass contact()
  ((name :accessor contact-name
	 :initarg :name)
   (email :accessor contact-email
	  :initarg :email)))

(defclass supplier(contact)
  ())



;; A generic function is made up of one or more methods.
;; Methods are defined using DEFMETHOD.
;; Methods are specialized for classes.
;; The unique about generic function is that you can keep adding methods to it.

(defmethod send_email((c contact))
  (format t "Sending email to a contact ~a at ~a ~%"
	  (contact-name c) (contact-email c)))

(defmethod send_email((c supplier))
  (format t "Sending email to a supplier ~a at ~a ~%"
	  (contact-name c) (contact-email c)))



(defmethod send_order((s supplier))
  (format t "Sending order to ~a at ~a ~%"
	  (contact-name s) (contact-email s)))



(let ((c1 (make-instance 'contact :name "johh" :email "john@example.com"))
      (s1 (make-instance 'supplier :name "xdel" :email "order@xdel.com")))

  ;; the generic function send_email works for both contact and supplier objects
  (send_email c1)
  (send_email s1)

  ;; send_order works only for supplier class
  (send_order s1)

  ;; send_order will not work for contact class because there are no applicable method
  ;; for this class.
  ;;(send_order c1)   <--- this will throw error.

  )



;; generic functions can also apply to other objects beside those defined by DEFCLASS.
(defmethod add ((x number) (y number))
  (+ x y))

(defmethod add ((x string) (y string))
  (list x y))

(defmethod add ((x list) (y list))
  (append x y))

;; methods can be specialized on obejct as determined by EQL.
(defmethod add ((x (eql 'fire)) (y (eql 'wood)))
  'smoke)

;; CL-USER>  (add 'fire 'wood)
;; SMOKE

;; CL-USER>  (add 'fire 'water)
;;  throws error


;; generic methods can have as complex a parameter list as ordinary lis functions but
;; they must be congruent e.g. they must have the same number of arguments, the same number
;; of optional arguments etc etc
;; The generic meothod defined below will throw an error here:

;; (defmethod add ((x number) (y number) (z number))
;;   (+ x y z))
