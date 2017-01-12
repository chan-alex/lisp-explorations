
;; List can also be treated as sets.

;; One thing to note is that working on sets gets slower, the bigger they are.
;; so if a set get too big, can consider switch to a different data structure.

;; To build up a set, use ADJOIN

(defparameter set1 ())

(adjoin #\a set1)

;; ADJOIN is functional so need to use SETF
(setf set1 (adjoin #\a set1))

(print set1)   ; This returns  (#\a)


;; To directly add a member to a set use PUSHNEW
(pushnew #\b set1)
(pushnew #\c set1)
(pushnew #\d set1)

(print set1)   ; This returns  (#\d #\c #\b #\a)

; MEMEBER can be used for testing if a value is a member of a list.
(print (member #\b set1))   ; prints (#\b #\a) 
(print (member #\z set1))   ; prints NIL


;; Other related function are MEMBER-IF,  MEMBER-IF-NOT
(print 
  (member-if #'(lambda(x) (char< x #\d)) set1))  ; prints (#\c #\b #\a)


;; FIND is used to find a member within a set.
(print (find #\b set1))   ; prints #\b 


; Also related at FIND-IF and FIND-IF-NOT
(print 
  (find-if #'(lambda(x) (char< x #\d)) set1))  ; prints #\c


;; The set theoritc functions are  INTERSECTION, UNION, SET-DIFFERENCE, and SET-EXCLUSIVE-OR.
;; Each of these function take in 2 lists as argument and returns a new set.
