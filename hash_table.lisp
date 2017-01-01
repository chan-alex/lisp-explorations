

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
(print (gethash2 'k2 *hash_table*))   ; 

                  
