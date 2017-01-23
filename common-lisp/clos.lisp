

;; DEFCLASS creates a class
(defclass Point()
  (x y))  ; x and y are object properties.

;; MAKE-INSTANCE creates objects from classes
(let ((p1 (make-instance 'Point)) (p2 '()))

  (setf p2 (make-instance 'Point))

  ;;SLOT-VALUE can be used to access the properties in the objects.
  (setf (slot-value p1 'x) 1)
  (setf (slot-value p1 'y) 1)

  (format t "Using SLOT-VALUE: p1.x is ~A, p1.y is ~A ~%"
   	  (slot-value p1 'x)
	  (slot-value p1 'y)))


;; SLOT-VALUE can be replaced with :accessor if defined
(defclass Point()
  ((x :accessor Point-x)
   (y :accessor Point-y)))


(let ((p1 (make-instance 'Point)))

  (setf (Point-x p1) 1)
  (setf (Point-y p1) 1)  

  (format t "Using :accessor : p1.x is ~A, p1.y is ~A ~%"
	  (Point-x p1)
	  (Point-y p1)))


;; To initialize slot values, use :initiform argument.
;; To enable the initializing of a slot value during the call to MAKE-INSTANCE
;; use :initarg. The input to :initarg is usuall a symbol but doesn't have to be.

(defclass Point()
  ((x :accessor Point-x
      :initarg :x
      :initform 0)
   (y :accessor Point-y
      :initarg :y
      :initform 0)))

(let ((p1 (make-instance 'Point))
      (p2 (make-instance 'Point :x 1 :y 2)))

  (format t "Testing :initform and :initarg: ~%")
  (format t " p1.x is ~A, p1.y is ~A ~%"
	  (Point-x p1)
	  (Point-y p1))

  (format t " p2.x is ~A, p2.y is ~A ~%"
	  (Point-x p2)
	  (Point-y p2)))


;; It is possible to define a slot to be shared by all instanes of the same class
;; using ":allocation :class". btw ":allocation :instance" is default so need to explicitly
;; define this.

(defclass testclass1()
  ((message :accessor test1class1-msg
	    :allocation :class
	    :initform "")))

(let ((tc1 (make-instance 'testclass1))
      (tc2 (make-instance 'testclass1)))
      
  (print tc1))
