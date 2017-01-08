

;;; The reader syntax for character object starts with #\
;;; #\A  is 'A'
;;; Any characters, including '(' can be used after #\.

;;; For space and newlinue, it is better tor use "Space" and "Newlinue" as it
;;; much easier to read. Note: the captialization matters.

;;; Other semistandard names include: Tab, Page, Rubout, Linefeed, Return, and Backspace.
;;; Depends on implementation.




;;; CHAR= and CHAR-EQUAL is for comparing character equality
(print (char= #\A  #\A))
(print (char-equal #\A  #\A))

;;; CHAR/= for character inequality
(print (char/= #\A  #\A))
(print (char-not-equal  #\A  #\A))

;;; CHAR< for character "less then"
(print (char< #\A  #\B))
(print (char-lessp  #\A  #\B))

;;; CHAR> for character "greater than"
(print (char>  #\A  #\B))
(print (char-greaterp  #\A  #\B))


;;; CHAR<= for character not "greater then"
(print (char<= #\A  #\B))
(print (char-not-greaterp  #\A  #\B))

;;; CHAR> for character not "lesser than"
(print (char>  #\A  #\B))
(print (char-not-lessp  #\A  #\B))


;;; String are enclosed by double quotes.
;;; Use backslashes to escape quotes
(format t "foobar")
(format t "foo\"bar")


;;; string= for string equality.
(print (string= "hotbar" "hotdog"))
(print (string= "hotdog" "hotdog"))



;;; Note you can specify start, end for comparisions.
(print
 (string= "I is at the shopping mall"
	  "He is at the shoppling mall"
	  :start1 2 :end1 5
	  :start2 3 :end2 5 ))

(print (string= "foobarbaz" "quuxbarfoo" :start1 3 :end1 6 :start2 4 :end2 7))

;;; The string comparators that check for string differences don't return T or NIL
;;; They return the index in the first strings.

;;; string/= for string inequality.
;;; Also equivalent is STRING-NOT-EQUAL
(print (string/= "hotbar" "hotdog"))   ; returns 3
(print (string/= "hotdog" "hotdog"))   ; NIL.

;;; string< for string "lesser than".
;;; Also equivalent is STRING-LESSP
(print (string< "hotbar" "hotdog"))   ; returns 3
(print (string< "hotdog" "hotdog"))   ; NIL.

;;; string> for string "greater than"
;;; Also equivalent is STRING-CREATERP
(print (string> "hotbar" "hotdog"))   ; NIL
(print (string> "hotdog" "hotdog"))   ; NIL.

;;; string<= for string not  "greater than"
;;; Also equivalent is STRING-NOT-GREATERP
(print (string<= "hotbar" "hotdog"))   ; returns 3
(print (string<= "hotdog" "hotdog"))   ; returns 6

;;; string>= for string not  "lesser than"
;;; Also equivalent is STRING-NOT-LESSP
(print (string>= "hotbar" "hotdog"))   ; NIL
(print (string>= "hotdog" "hotdog"))   ; returns 6




