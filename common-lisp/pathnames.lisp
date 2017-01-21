
;; A pathname is a structured object that represents a filename
;; using six components: host, device, directory, name, type, and version. 
;; This is a OS system independent way of handling pathanmes.

;; To translate a string to pathname object use PATHNAME
(pathname "/var/log/system.log")

;; PATHNAME-DIRECTORY give the directory componemts
(print
 (pathname-directory (pathname "/var/log/system.log")))

;; PATHNAME-NAME  - give name
(print
 (pathname-name (pathname "/var/log/system.log")))

;; PATHNAME-TYPE - file extension
(print
 (pathname-type (pathname "/var/log/system.log")))

;; There's also PATHNAME-DEVICE
(print
 (pathname-device (pathname "C:/var/log/system.log")))

;; There's also PATHNAME-HOST
(print
 (pathname-host (pathname "C:/var/log/system.log")))


;; PATHNAME has its own READ syntax. Starts with #p
;; CL-USER> (pathname "/var/log/system.log") 
;; #P"/var/log/system.log"

;; The following translate pathname back to namestrings
(namestring #P"/var/log/system.log")  ; "/var/log/system.log"
(directory-namestring #P"/var/log/system.log")  ; "/var/log/"
(file-namestring #P"/var/log/system.log")  ; "system.log"



(defun read_file (path)
  (let ((in (open path :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	 while line do (format t "~a~%" line))
      (close in))))


;; PATHNAME will work with OPEN
(read_file4 (pathname "/etc/hosts"))



;; Forming pathnames

;; MAKE-PATHNAME can be used to create pathnames
(print
 (make-pathname
  :directory '(:absolute "var" "log")
  :name "system"
  :type "log"))



;; It may be better in many case not to create pathnames from scratch.
;; Can use MAKE-PATHNAME :defaults argument to supply values for any components not
;; not specified by other arguments

(print
 (make-pathname
  :name "system"
  :type "log"
  :defaults "/var/log/messages"))




;; MERGE-PATHNAMES can be uses to merge two PATHNAME

(print
 (merge-pathnames
  #p"/var/log/"
  #p"message.log"))


;; One thing to note is the Windowns and linux treat directories different.
;; In linux, directories are just another file. In windows, directories and files are
;; two different things. So when using MAKE-PATHNAME need to be aware

(make-pathname :directory '(:absolute "abc" "def") :name "ghi")  ; for linux
(make-pathname :directory '(:absolute "abc" "def" "ghi"))        ; for windows
