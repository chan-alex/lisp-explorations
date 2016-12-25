


;;; Vectors
;;; comes in 2 types: fixed sized and resizable.
;;; all members in a vector need to be the same type.

;;; fixed size vectors can be push this way 
(vector )
(vector 1 2 3 4 5)
;;; (vector 1 'a')   <--- tis won't compile.


;;; The literal notation for vectors is #(...)
#( )
#(1 2 4 5 5)
;;; #(1 2 3 4 'a')  <---- this won't copmile.

;;; CL-USER> (setf a (vector 1 2 3 4 5))
;;; #(1 2 3 4 5)


;;; note: Using #(...) as literal are fine but the effects of modifying a literal vector
;;; are not well defined so using VECTOR is the better choice.


;;; MAKE-ARRAY is the more general way of making vectors. Can create both fixed and resizable
;;; vectors. Can also create multidimensional arrays.
;;; Definition: vectors here refer to one dimensional arrays.

;;; creatng a fixed vector of 10 elements
(make-array 10 :initial-element 0)
;;; CL-USER> (make-array 10 :initial-element 0) 
;;; #(0 0 0 0 0 0 0 0 0 0)


;;; To create a resizeable vectors. Use the :fill-pointer 0 argument.

;;; Creating a resizable vector with room for 5 elements.
(defparameter *resizable-vector1*  (make-array 5 :fill-pointer 0))

;;; Use VECTOR-PUSH to add elements


