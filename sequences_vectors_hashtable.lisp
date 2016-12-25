


;;; Vectors
;;; comes in 2 types: fixed sized and resizable.


;;; fixed size vectors can be push this way 
(vector )
(vector 1 2 3 4 5)
(vector 1 2 3 4 5 'a #\b)   ; don't have to be same type.


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

;;; Use VECTOR-PUSH to add elements. Will increment the fill-pointer and returns the value
;;; of the fill-pointer where the new element was added.
(vector-push 1 *resizable-vector1*)   ; this returns 0
(vector-push 2 *resizable-vector1*)   ; this returns 1


;;; Use VECTOR-POP to remove the most recently added memeber
(vector-pop  *resizable-vector1*)   ; this returns 2
(vector-pop  *resizable-vector1*)   ; this returns 1

;;; *resizable-vector1*  maybe resizable but can only hold up to 5 elements.
(vector-push 1 *resizable-vector1*)   ; this returns 0
(vector-push 2 *resizable-vector1*)   ; this returns 1
(vector-push 3 *resizable-vector1*)   ; this returns 0
(vector-push 4 *resizable-vector1*)   ; this returns 1
(vector-push 5 *resizable-vector1*)   ; this returns 0
(vector-push 6 *resizable-vector1*)   ; this returns NIL and vector remains at 5. limit hit.
