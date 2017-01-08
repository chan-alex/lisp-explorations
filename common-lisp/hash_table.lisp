

;;; Hash tables are created with MAKE-HASH-TABLE
(defparameter *hash_table* (make-hash-table))


;;; Use GETHASH to get stored values from hash tables.
;;; GETHASH returns SETF-able location, it can be used to put items in too.

(gethash 'k1  *hash_table*)    ; returns NIL

(setf (gethash 'k1  *hash_table*)  100)

(print
 (gethash 'k1  *hash_table*) )   ; returns something now.



;;; GETHASH has a problem. Since it retuns NIL when no key is present, how do you tell
;;; if the value associated with a key is actually NIL?
;;; The solution is know that GETHASH actually returns a multiple value result.
;;; Use MULTIPLE-VALUE-BIND
(defun gethash2 (key hash_table)
  (multiple-value-bind  (value present) (gethash key hash_table)
     (if present
         (format nil "Key found in hash table. Value is:  ~a " value)
         (format nil "No key found."))))


(setf (gethash 'k2  *hash_table*)  NIL)
(print (gethash2 'k2 *hash_table*))    


;;; Another thing about GETHASH - be default it returns NIL if a key is not found.
;;; It is possible to get it to return something instead:
(print (gethash "nosuchkey" *hash_table* 0))  ;;; this return 0 instead of NIL.

                  
;;; Removing keys from hastables - use REMHASH
(remhash 'k2 *hash_table*)
(print (gethash2 'k2 *hash_table*)) ; this now says 'k2 is not found.


;;; Iterating over a hash table.
;;; Use MAPHASH 
(defparameter *hash_table* (make-hash-table))

(setf (gethash 'k1 *hash_table*) 1)
(setf (gethash 'k2 *hash_table*) 2)
(setf (gethash 'k3 *hash_table*) 3)
(setf (gethash 'k4 *hash_table*) 4)
(setf (gethash 'k5 *hash_table*) 5)

(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *hash_table*)



;;; Non-obvious thing about hast-table - use strings as key

(defparameter *hash_table* (make-hash-table))

(setf (gethash "k1" *hash_table*) 1)
(setf (gethash "k2" *hash_table*) 2)
(setf (gethash "k3" *hash_table*) 3)
(setf (gethash "k4" *hash_table*) 4)
(setf (gethash "k5" *hash_table*) 5)

(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *hash_table*)


;;; ^^^^ The above code will compile but will not work correctly.
(gethash "k2" *hash_table*)   ;;; will return NIL even tho the maphash shows that it is there.


;;; HASH-TABLE by default use EQL for key equality comparsion. EQL does not work for string.
;;; Have to create the hash-table that uses EQUAL

(defparameter *hash_table* (make-hash-table :test #'EQUAL))
