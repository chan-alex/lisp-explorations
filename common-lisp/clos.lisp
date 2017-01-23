

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
  ((message :accessor testclass1-msg
	    :allocation :class
	    :initform "")))

(let ((tc1 (make-instance 'testclass1))
      (tc2 (make-instance 'testclass1)))

  (setf (testclass1-msg tc1) "This is a message from tc1")
  (print (testclass1-msg tc2))
  
  (setf (testclass1-msg tc2) "This is a message from tc2")
  (print (testclass1-msg tc1)))  


;; multiple Inheritance
;; The second argument of DEFCLASS is a list of superclasses.
;; A class inherits the union of the slots of its superclasses.

(defclass Point()
  ((x :accessor Point-x
      :initarg :x
      :initform 0)
   (y :accessor Point-y
      :initarg :y
      :initform 0)))

(defclass RGB()
  ((red :accessor RGB-r
	:initarg :r
	:initform 0)
   (green :accessor RGB-g
	:initarg :g
	:initform 0)
   (blue :accessor RGB-b
	:initarg :b
	:initform 0)))

(defclass Circle()
  ((radius :accessor Circle-radius
	   :initarg :radius )))
	   :initform 0)))


(defclass screen-Circle(Point RGB circle)
  ((radius :initform 10))) ;; Here it is possible to specify different default valuees for slots.


(let ((cc (make-instance 'screen-Circle
			 :x 10 :y 10
			 :r 255 :g 0 :b 0
			 )))
  (format t "Co-cordinates: x=~a   y=~a  ~%" (Point-x cc) (Point-y cc))
  (format t "RGB value: red=~a   green=~a   blue=~a  ~%" (RGB-r cc) (RGB-g cc) (RGB-b cc))
  (format t "Radius: radius=~a    ~%" (Circle-radius cc)))


;; As with other object oriented languages that support multiple inheritance,
;; the diamond problem allows existing in CLOS. There are 2 rules to resolve this.
;; In CLOS, this is resolved with preceence list 
