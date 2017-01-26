

(defclass Point()
  ((x :accessor Point-x
      :initarg :x
      :initform 0)
   (y :accessor Point-y
      :initarg :y
      :initform 0)))


(defclass Shape()
  ())

(defclass Square(Shape)
  ())

(defclass rectangle(Square)
  ())

(defclass Circle(Shape)
  ())


(defgeneric draw(Shape)
  (:documentation "Draws the shape"))


(defmethod draw((s Shape))
  (format t "Drawing shape: ~a ~%" s))

(defmethod draw((sq Square))
  (format t "Drawing square: ~a ~%" sq))



(let ((s  (make-instance 'Shape))
      (sq (make-instance 'Square))
      (rt (make-instance 'Rectangle))
      (c  (make-instance 'Circle)))

  ;; A demo of CLOS precedence
  (draw s)   ;; calls draw((s Shape))
  (draw sq)  ;; calls draw((sq Squre))
  (draw rt)  ;; calls draws((sq Square)) 
  (draw c))  ;; calls draw((s Shape))
