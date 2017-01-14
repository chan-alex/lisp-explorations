


;; Association lists or alists are lists of cons cells
;; Can do lookup with them similar to hash tables. For small number of items, can be
;; faster than hash tables.

(defparameter *alist1* '((1 . "one") (2 . "two") (3 . "three") (4 . "four")))

(print
 (assoc 3 *alist1*))   ; prints (3 . "three")


(defparameter *alist2* '(( "one" . 1) ("two" . 2) ("three" . 3) ("four" . 4)))

;; ASSOC uses EQL for testing. Which is why the below prints NIL
(print
 (assoc "three" *alist2*))  ; prints NIL


;; the solution is to pass in a different comparing function via :test keyword.
(print
 (assoc "three" *alist2* :test #'string=))  ; prints ("three" . 3)


;; ACONS is used to add a key/value pair to a list. It is non-modifying so you have to use
;; SETF or PUSH

(setf *alist2* (acons "five" 5 *alist2*))
(push (acons "six" 6 *alist2*) *alist2*)


;; another way to create an alist is with PAIRLIS
(setf *alist2* (pairlis '(1 2 3) '("one" "two" "three")))




;; Property lists (plists) are just lists with alternate keys and values
;; plist only support GETF which uses only EQ to test for keys.
;; This makes number and character unsuitable as keys in plists as EQ is undefined 
;; for these types. plists usually uses symbols as keys:

(defparameter *plist1* '(:1 "one" :2 "two" :3 "three"))

(print
 (getf *plist1* :1))

;; Can use SETF on plists too
(setf (getf *plist1* :1) "ONE")


;; Use REMF to remove keys. REMF also always uses EQ for finding keys.
(remf *plist1* :1)
(print *plist1*)

(setf (getf *plist1* :1) "one")


;; GET-PROPERTIES can be used to extract multiple values from plistsa
;; It takes a plist and a list of keys to search for and returns, as multiple values,
;; the first key found, the corresponding value, and the head of the list starting
;; with the found key..
(multiple-value-bind (key value tail) (get-properties *plist1* '(:a :2 :3))
  (format t "Key = ~a ~%" key)
  (format t "value = ~a ~%" value)
  (format t "tail = ~a ~%" tail))
  
