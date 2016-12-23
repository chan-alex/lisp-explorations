

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
