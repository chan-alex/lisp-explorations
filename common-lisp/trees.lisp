


;; Trees

;; lists can be used to from tree structures

;; tree1 here is really a tree underneath.
(defparameter *tree1*
      '((1 2 ("a""b" )) (3 4 ("bc" "d")) (5 6 ("e" "f"))))


;; You can make copies of a tree with either COPY-TREE or COPY-LIST.
;; Both do very different things.

(let ((t1 (copy-list *tree1*))  (t2 (copy-tree *tree1*)))
  (print (tree-equal *tree1* t1))
  (print (tree-equal *tree1* t2)))


;; COPY-LIST create a list that shares cons cell with the original tree.
;; COPY-TREE creates a complete cloning of the original tree.

(let ((t1 (copy-list *tree1*))  (t2 (copy-tree *tree1*)))
  (setf (car (car (cdr (cdr (car t1))))) "X")   ; change something on t1
  (print *tree1*)   ; now *tree1* has changed too.
  (print t1)
  (print t2))       ; t2 is unaffected because it is a complete copy.


  
;; SUBST - takes new and old item and a tree and replace the old item with the new item.
;; The tree retains the structure. NSUBST is the same but is destructive.
(print (subst "Y" 3 *tree1*))
(print *tree1*)
