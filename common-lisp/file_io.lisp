

;; OPEN opens a file. It returns a character based input stream that can be read by
;; functions such as READ-LINE (reads a line) and READ-CHAR (reads a character)
;; and READ (reads a S-expression.
;; CLOSE closes the steam.


;; the code below reads a line.
(defun read_file1 (path)
  (let ((in (open path)))
    (format t "~a~%" (read-line in))
    (close in)))




;; the code below will throws an error is the file does not exist.
;;  (read_file "/etc/XXXXX") gives
;;
;;   No such file or directory : "/etc/XXXXX"
;;     [Condition of type CCL::SIMPLE-FILE-ERROR]


;; One way to handle the file open errors is to use the IF-DOES-NOT-EXIST keyword
(defun read_file2 (path)
  (let ((in (open path :if-does-not-exist nil)))
    (if in
	(progn
	  (format t "~a~%" (read-line in))
	  (close in))
	(format t "File does not exist ~%"))))



;; READ, READ-CHAR and READ-LINE all take in an optional argument (defaults to true)
;; to specify if they should signal an error when the end of file is reached.
;; if this argument is NIL, it will return the value of the 3rd argument instead.
;; This value is NIL by default.
;; The code below uses this to read the contents of a text file with READ-LINE.

(defun read_file3 (path)
  (let ((in (open path :if-does-not-exist nil)))
    (when in
	(loop 
	   (let ((line (read-line in nil)))  ; optional argument set to NIL
	     (if line
		 (format t "~a~%" line)
		 (return))))
	(close in))))


;; Same as read_file3, uses LOOP instead.
(defun read_file4 (path)
  (let ((in (open path :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	 while line do (format t "~a~%" line))
      (close in))))




;; To read binary data, use OPEN but pass in :element-type with argument of '(unsigned-byte 8)
;; to create a binary stream. THen use READ-BYTE to read. it returns 0 to 255 each time it is called.
; Modifying read_file3 to read in in binary mode.
(defun read_bfile1 (path)
  (let ((in (open path :element-type '(unsigned-byte 8) :if-does-not-exist nil)))
    (when in
	(loop 
	   (let ((byte (read-byte in nil)))  ; optional argument set to NIL
	     (if byte
		 (format t "~a => ~a~%" byte (code-char byte))  ; pretty binary and ascii
		 (return))))
	(close in))))
  



;; READ-SEQUENCE reads file in chunks. It is passed a sequence, usually a vector.
;; Then it will try to fill the vector with data. it returns the length of the sequence
;; or the index, if it was not able to fill it.
