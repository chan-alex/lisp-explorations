

;; Symbol are words.
;; They are variable names, existing as objects in their own right.
;; Symbol have to be quoted, otherwise they will be treated a variables.
'fried_rice
(quote fried_rice)

;; They are always converted to uppercase.
(print
 (symbol-name 'fried_rice))

;; Which means by default lisp is actually not case-sensitive.
;; the below will compile.
(QuOte fried_riCe)

(print
 (eql 'fried_rICE 'FRIED_RICE))


;; Symbols cannot contain space or anything that might be significant to the reader
;; but there is a special syntax to include such characters.
(quote '|this is nice fried rice|)
(print
 (symbol-name '|this is nice fried rice|))


;; Every symbol has a property list or plist.
;; GET takes a symbol and key and return values associated with key
(print
 (get 'fried_rice 'chili))


;; It uses EQL to compare keys. if key not found, GET returns NIL.
(setf (get 'fried_rice 'chili) "yes")

(print
 (get 'fried_rice 'chili))


;; SYMBOL-PLIST returns property of a symbol
(setf (get 'fried_rice 'onion) "yes")

(print
 (symbol-plist 'fried_rice))


;;; Note: symbols are substantial objects. They have a name, package, value, function
;;; plist. They don't take a lot of space but they are real objects, not just names.


;; Creating symbol

;; Every ordinary symbol belog to a package. Packages are conceptually symbol tables.
;; A symbol that belongs to a package is said to be interned in that package.
;; Functions and variables have symbols as their names.
;; Packages enfore modularity by restricting which symbols are accessible.

;; Most symbols are created when they are read.
;; The 1st time a symbol is typed. Lisp will create a new symbol object and intern it
;; in the current packeage (which by default is "common-lisp-user").

;; A symbol can be also be manually created with INTERN.
;; INTERN takes the name of the new symbol and optionally name of the package to intern the
;; symbol in. The package argument defaults to the default package.
(intern "chicken_rice")

;; INTERN will return 2 values, 1st is the name of the symbol. The second will show whether
;; the symbol was already created.

;; Note: Not all symbol are interned. Uninterned symbols are called gensyms. useful for macros.
(gensym)


;;; Packages

;; Large programs are often divided into packages. If you define your functions in a
;; seperate package, you can choose which symbol are visibl by explictly exporting them.

;; Example: Suppose a program is divided into 2 packages:  math and disp.
;; If the symbol fft is exported by the math package, then code in the disp package will be
;; able to refer to it as math:fft. Within the math package, it will be possible to refer to it
;; as simply fft .

;; Ah package is defined with DEFPACKAGE

(defpackage "RICE-DISHES"
  (:use "COMMON-LISP" "RICE-UTILITIES")
  (:nicknames "RICE")
  (:export "fried_rice" "chicken_rice" "paella"))

(in-package rice-dishes)

;; the above defines a package called "rice-dishes". It uses 2 other packages.
;; COMMON-LISP and "RICE-UTILITIES". This means it can use symbols from them without
;; package qualifiers (otherwise have to quatify CL:DOTIMES etc)
;; The RICE-DISHES exports just 3 symbols: "fried_rice" "chicken_rice" "paella".

;; The IN-PACKAGE line makes rice-dishes the current package.
;; All unqualified symbols in the rest of the file will belong to this package - unless
;; there is another package later.


;;; The standard packages
;; When lisp is first started, the value of *PACKAGE* is typically COMMON-LISP-USER.
;; Also known as CL-USER. 
(print *PACKAGE*)  ; #<Package "COMMON-LISP-USER">

;; CL-USER uses the COMMON-LISP package, which exports all the names defined by the
;; language standard. Which mean you can do this:
(print COMMON-LISP:*PACKAGE*)

;; or
(print CL:*PACKAGE*)  ; CL is the nickname for the COMMON-LISP package.



;; KEYWORD symbols
;; There is a package named KEYWORD. The symbols in this package have 2 unique properties
;; They always evaluate to themselves and you can referring to simply as ":<name of symbol"
;; without have to quality them. eg. :z instead of KEYWORD:z
;; Keywords are used because they are accessible anywhere.
