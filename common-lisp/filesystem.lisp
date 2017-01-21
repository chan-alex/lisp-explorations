

;; PROBE-FILE is used to test if a certain file exists
;; return NIL if not exist
;; returns the pathname is exist.
(defvar *pathname* (make-pathname :directory '(:absolute "var" "log")))

(print
 (probe-file  (make-pathname :name "message" :defaults *pathname*))) 

(print
 (probe-file  (make-pathname :name "system" :type "log" :defaults *pathname*))) 



;; DIRECTORY can be used to get a listing of contents of a directory.
;; Note: need to use the :wild to get the expect result

(print
 (directory *pathname*))  ; this just give a NIL


; The below return the pathname of /var/log/*.log files.
(print
 (directory (merge-pathnames *pathname* (make-pathname :name :wild :type "log"))))


;; CREATE-FILE - creates file
(defvar *tmpfile* (make-pathname :directory '(:absolute "tmp") :name "testfile" :type "txt"))

(create-file *tmpfile*) 
; (create-file "/tmp/testfile.txt")  works too but it's easier to manipulate pathnames later.

;; RENAME-FILE - renames file
(defvar *tmpfile1* (make-pathname  :name "testfile1" :type "txt" :defaults *tmpfile*))
(rename-file *tmpfile* *tmpfile1*)

;; DELETE-FILE - deletes file
(delete-file *tmpfile1*)


;; ENSURE-DIRECTORIES-EXIST is used to create directory
(ensure-directories-exist "/tmp/testdir1" :verbose t )


;; There's actually no ANSI standard function for deleting directories.
;; Each lisp implementation are likely to implement their own fucntion to do this.
;; Usually named DELETE-DIRECTORY
(delete-directory "/tmp/testdir1")



;; FILE-WRITE-DATE returns returns the time in number of seconds since midnight January 1,1900,
;; Greenwich mean time (GMT), that the file was last written, 
(print
 (file-write-date "/etc/hosts"))


;; FILE-AUTHOR returns the owner of the file
(print 
 (file-author "/etc/hosts"))

;; To find out the size of a file, use FILE-LENGTH
;; For historical reasons, it operates on streams instead of PATHNAMES
;; For best results, use a binary stream 
(with-open-file (in "/etc/hosts" :element-type '(unsigned-byte 8))
  (print (file-length in)))
