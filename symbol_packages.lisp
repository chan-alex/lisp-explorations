

;;; Symbol are words.
;;; They are variable names, existing as objects in their own right.
;;; Symbol have to be quoted, otherwise they will be treated a variables.
'fried_rice
(quote fried_rice)

;;; They are always converted to uppercase.
(print
 (symbol-name 'fried_rice))

;;; Which means by default lisp is actually not case-sensitive.
;;; the below will compile.
(QuOte fried_riCe)

(print
 (eql 'fried_rICE 'FRIED_RICE))


;;; Symbols cannot contain space or anything that might be significant to the reader
;;; but there is a special syntax to include such characters.
(quote '|this is nice fried rice|)
(print
 (symbol-name '|this is nice fried rice|))


;;; Every symbol has a property list or plist.
;;; GET takes a symbol and key and return values associated with key
(print
 (get 'fried_rice 'chili))


;;; It uses EQL to compare keys. if key not found, GET returns NIL.
(setf (get 'fried_rice 'chili) "yes")

(print
 (get 'fried_rice 'chili))


;;; SYMBOL-PLIST returns property of a symbol
(setf (get 'fried_rice 'onion) "yes")

(print
 (symbol-plist 'fried_rice))


;;; Note: symbols are substantial objects. They have a name, package, value, function
;;; plist. They don't take a lot of space but they are real objects, not just names.
