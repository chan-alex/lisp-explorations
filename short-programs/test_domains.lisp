

;; This program uses drakma http client to test a number of sites.  

(ql:quickload "drakma")

(defparameter *test-domains*
  '("http://www.goggle.sg"
    "http://www.yahoo.com.sg"
    "http://reddit.com"
    "https://news.ycombinator.com" ))
 
  
(defun get-return-code (url)
  (format t "Testing ~s ~%" url)
  (let ((return-code (nth-value 1 (drakma:http-request url))))
    (format t ">> Return code is ~d ~%" return-code)))


(defun test-urls (urls)
  (dolist (d urls)
    (get-return-code d)))


(test-urls *test-domains*)
