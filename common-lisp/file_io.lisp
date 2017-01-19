

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

;; recursive style experiment
(defun read_func (line stream)
  (when line
	 (format t "~a~%" line)
	 (read_func (read-line stream nil) stream)))

(defun read_file4-r (path)
  (let ((in (open path :if-does-not-exist nil)))
    (when in
      (read_func (read-line in nil) in))))





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
;; Then it will try to fill (destructively) the vector with data. it returns the length of the seqence
;; or the index, if it was not able to fill it.

(defun read_file5 (path)
  (let ((in (open path)) (buf (make-array 20)))
    (when in
      (read-sequence buf in)   ; this first READ-SEQUENCE will advance the file position.
      (read-sequence buf in)
      (print buf))
    (close in)))


;; this function reads a file in chunks.
;; the last chunk will probably contain parts of previous chunk
;; Not adding additional code to account for that to keep it simple.
(defun read_file_in_chunks (path chunksize)
  (let ((in (open path)) (buf (make-array chunksize)))
    (when in
      (loop 
	 (let ((pos (read-sequence buf in)))
	   (format t "~a~%" buf)   ; the last chunk will probably contain parts of previous chunk
	   (if (< pos chunksize)
	       (return)))))
    (close in)))




;; Writing to files

;; To open file for writing, use OPEN with any specify :direction with keyword :output.
;; OPEN will signal an error the file  exists. To override, use :if-exists.
;; Set it to :superede to replace the file,
;; Set to :apppend to append to the file.

;; Once open, you can write the stream with WRITE_CHAR, WRITE-LINE, WRITE-STRING.
;; TERPRI (short for terminiate print) - unconditionally prints a newline character.
;; FRESH-LINE prints a newline character unless stream is at the beginning of a new line.

;; Note:  use one of the read_file* function above to test.
(defun write_file1 (filename)
  (let ((out (open filename :direction :output :if-exists :supersede)))
    (write-char #\X out)
    (fresh-line out)
    (write-line "this is a line" out)
    (close out)))


;; PRINT prints lisp data as S-expression follow by an end-of-line
;; PRIN1 just prints the S-expression
;; PPRINT is like PRINT and PRIN1 except it prints in a new looking way.
;; These functions are controlled by the *PRINT-READABLY* variable.

;; PRINC prints S-experession but in a way suitable for human consumption.
(defun write_file2 (filename)
  (let ((out (open filename :direction :output :if-exists :supersede))
	(sexpr1 '(list 'a' 'b' '(1 2 3) :keyword1)))

    (print sexpr1 out)
    (fresh-line out)	
    (prin1 sexpr1 out)
    (fresh-line out)	
    (pprint sexpr1 out)
    (fresh-line out)	
    (princ sexpr1 out)        
    (close out)))


;; Testing write_file2
(write_file2 "/tmp/test1")

(with-open-file (in "/tmp/test1")
  (let ((sexpr1 (read in)))   ; READ read and evaluate the S-expressions.
    (print
     (append sexpr1 '(4)))))) ; this shows sexpr1 is a valid S-expression.
  

