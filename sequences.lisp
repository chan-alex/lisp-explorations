


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
(vector-push 1 *resizable-vector1*)   
(vector-push 2 *resizable-vector1*)   
(vector-push 3 *resizable-vector1*)   
(vector-push 4 *resizable-vector1*)   
(vector-push 5 *resizable-vector1*)   
(vector-push 6 *resizable-vector1*)   ; this returns NIL and vector remains at 5. limit hit.


;;; To make an adjustable vector, need to pass an ":adjustable" argument to MAKE-ARRAY.
;;; Then use VECTOR-PUSH-EXTEND, which is like VECTOR-PUSH will expand the array as needed.
(defparameter *resizable-vector2*  (make-array 5 :fill-pointer 0 :adjustable t))
(vector-push-extend 1 *resizable-vector2*)   
(vector-push-extend 2 *resizable-vector2*)   
(vector-push-extend 3 *resizable-vector2*)   
(vector-push-extend 4 *resizable-vector2*)   
(vector-push-extend 5 *resizable-vector2*)   
(vector-push-extend 6 *resizable-vector2*)   ; This will work.

;;; It is possible to make specialized arrays. This type of array can only hold
;;; element of a certain type. Provides faster access.
;;; Lisp strings are an exmaple.
;;; For this pass :element-type argument to MAKE-ARRAY.
(defparameter *specialized-vector1*
  (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character))

;;; (vector-push 1 *SPECIALIZED-VECTOR1*)   this will throw error.



;;; Sequence functions.
;;; applies to all sequences. includes string

(defparameter *vec1* (vector 1 2 3 #\a #\b #\c))


;;; returns the length of the sequence
(print (length *vec1*))

;;; ELT returns the nth element of sequence.
;;; Start from 0.
(print (elt *vec1* 2))

;;; ELT is a SETF-able place.
(setf (elt *vec1* 2) 100)

;;; count
(print
 (count 1 #(2 3 1 3 45 1 1 1)))

;;; remove
(print
 (remove 1 #(2 3 1 3 45 1 1 1)))

;;; substitute
(print
 (substitute 1000 1 #(2 3 1 3 45 1 1 1))) 

(print
 (substitute #\x #\b "aaaaabbbcccc")) 


;;; The above sequence functions can accept additional keywords that change how they work.

;;; :test keyword can be use to specify a dififerent comparator insread of the default one
;;; EQL. Pass in a function that takes 2 argument and return a boolean.
;;; :key keyword can be used to pass in a function to extract key value from the sequence.
;;; :start     - starting index
;;; :from-end  - reverse direction. Start from end.
;;; :count     - indicate number to subsititue or remove.

;;; sequence function variants.
;;; The above functions have higher-order variants that take a function that can be called
;;; on each element of the sequence.

(print
 (count-if #'oddp '(3 4 5 3 3 4 4 4)))

(print
 (remove-if #'(lambda (x) (char= x #\x)) "banaxxxxna"))

;;; these variants also thake the same keywords as the standard versison.


;;; Other sequence functions

;;; CONCATENATE. Does what the name say. But need to specify kind of sequence to possible
;;; in case the types of arguments are different.
(print
 (concatenate 'list '(1 2 3) '(4 5 6)))

(print
 (concatenate 'list '(1 2 3) "abcde"))

(print
 (concatenate 'vector '(1 2 3) "abcde"))


;;; REVERSE
;;; non-destructive. returns a new sequence.
(print
 (reverse '(1 2 3 4 5 6)))


;;; COPY-SEQ
;;; returns a new copy of the sequence.
(copy-seq '(1 2 3 4 5 6))


;;; SORT - takes 2 arguments. first the sequence to sort and a 2-argument predicate
(sort #( "ghi" "def" "abc") #'string<)

;;; STABLE-SORT is the same. The difference is that it gurantees not to re-order any
;;; element considered equal by the sort predicate. SORT only gurantess the sequence
;;; will be sorted. It will reorder the elements in any way.
(stable-sort #( "ghi" "def" "abc") #'string<)

;;; TAKE NOTE: Both are DESTRUCTIVE functions. If you don't this, pass in a copy.
(defparameter *unsorted1* #( "ghi" "def" "abc"))
(sort *unsorted1* #'string<)
(print *unsorted1*) ; will be changed.

(defparameter *unsorted2* #( "ghi" "def" "abc"))
(sort (copy-seq *unsorted1*) #'string<)
(print *unsorted2*)

;;; both SORT and STABLE-SORT take in a :keyword argument to pass in a function
;;; to extrace the values to be passed into the sorting predicate for sorting.
(defparameter *unsorted3* #( "ghi" "def" "abc"))
(sort (copy-seq *unsorted3*) #'string<
       :key #'(lambda(x) "xxx"))
(print *unsorted3*)



;;; MERGE - merges destructively 2 sequences according to an order determined by
;;; a predicate.
;;; If both sequences are already sorted to the predicate, the result sequence will also be sorted.
;;; If not, the resulting sequence will not be sorted but there will be interweaving.
(print 
 (merge 'vector '(10 15 20) '(12 17 22) #'>))

(print 
 (merge 'vector '(10 15 20) '(12 17 22) #'<))



;;; Sequence manipulation.

;;; SUBSEQ extracts a part of a sequence.
(defparameter *seq1* '(1 2 3 4 5 6 7 8 9 10))

(print
 (subseq *seq1* 4))

(print
 (subseq *seq1* 4 8))


;;; SUBSEQ returns SETF-able value. You can modify the sequence
(print *seq1*)
(setf (subseq *seq1* 4 8) '("A" "B" "C" "D"))
(print *seq1*)

;;; Note: when SETF-ing returned values from SUBSEQ, the extra parts will be ignored.
;;; If shorter, the only starting part of the subsequence will be modified.

;;; Use FILL to set the element of a sequence to a certain value.
;;; Use :start and :end keywords to specifiy start and end
(fill *seq1* 0)
(print *seq1*)

(fill *seq1* 1 :start 4 :end 8)
(print *seq1*)

;;; Searching a sequence.

;;; POSTITION returns the position of a

(print
 (position 1 *seq1* ))


;;; to find a subseqnece with a sequence. Use SEARCH.
(print
 (search '(1 1 1) *seq1*))


;;; To find where 2 sequences diverge, use MISMATCH
(print
 (mismatch "abcdefg"  "abcd1234"))

;; MISMATCH also take a number of keyword arguments.




;;; Sequence predicate

;;; EVERY - return T if predicate is true for every element of a sequence.
(print
 (every #'oddp  '(1 2 3 4 5 6)))  ; returns NIL

(print
 (every #'oddp  '(3 3 3 3 3 3)))  ; returns T


;;; SOME - return T if predicate is true for some of the elements of a sequence.
(print
 (some #'oddp  '(1 2 3 4 5 6)))  ; return T

(print
 (every #'oddp  '(4 4 4 4 4 4)))  ; returns NIL



;;; NOTANY - return T only if all elements fails the predicate.
(print
 (notany #'oddp  '(1 2 3 4 5 6)))  ; return NUL

(print
 (notany  #'oddp  '(4 4 4 4 4 4)))  ; return T



;;; NOTEVERY - return NUL if all elemetn satisfy th predicate.
(print
 (notevery #'oddp  '(1 2 3 4 5 6)))  ; return NUL

(print
 (notevery  #'oddp  '(4 4 4 4 4 4)))  ; return s

