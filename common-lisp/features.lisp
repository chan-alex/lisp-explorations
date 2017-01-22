

;; There is a special variable *features* is a list of symbols where each symbol
;; represents a feature that is present in the specific lisp implementations.

(dolist (i *features*) (print i ))

;; This can be used write lisp code specific to certain lisp implementation.

;; The lisp reader understands a special syntax for *features*
;; #+ has this feature
;; #- don't have this feature.
;; can be conbimed with NOT, AND, OR

(defun lisp_specific_code()
  #+ sbcl  (do-sbcl-specific-things)
  #+ ccl   (do-ccl-specific-things)
  #+ clisp (do-clisp-specific-things)
  #- (or allergo cmu) (error "this is not implemented"))


;; If the above code is executed on sbcl, the above code will become:
(defun lisp_specific_code()
  (do-sbcl-specific-things))
 
;; if executed on Clozue Lisp, it becomes
(defun lisp_specific_code()
  (do-ccl-specific-things))


;; the key thing to note is that the skipped code is never seen by the compiler
;; so there is zero cost for having different versions of code for different
;; lisp implementations.
