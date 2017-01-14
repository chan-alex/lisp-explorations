

;; DESTRUCTURING-BIND is used to destructure lists.

(destructuring-bind (a b c) (list 1 2 3)
   (format t "a = ~a  b = ~a  c = ~a ~%" a b c))


;; it works for trees
(destructuring-bind (a (b c) d e) (list 1 (list 2 3) 4 5)
   (format t "a = ~a  b = ~a  c = ~a d = ~a e = ~a ~%" a b c d e))


;; DESTRUCTURING-BIND support &key keywords
(destructuring-bind (&key a b c) (list  :a 1  :b 2 :c 3 )
  (format t "a = ~a  b = ~a  c = ~a ~%" a b c))    

;; this don't work tho
;; (destructuring-bind (&key a b c) (list  :x 1  :y 2 :z 3 ))



;; DESTRUCTURING-BIND also supports &optional and &whole keywords.


(destructuring-bind (&whole w  &key a b c) (list  :a 1  :b 2 :c 3 )
  (print w))    
