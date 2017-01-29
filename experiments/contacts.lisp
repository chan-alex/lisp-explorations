

(defclass contactlist()
  ((contactlist :accessor contact-list
                :initform '())))

(defclass contact()
  ((name :accessor contact-name
	 :initarg :name
	 :initform "")
   (info :accessor contact-info
	 :initform '())))



(defgeneric add-contact(contactlist contact)
  (:documentation "Adds a contact to the list."))

(defmethod add-contact((cl contactlist) contact)
  (push contact (contact-list cl)))


(defgeneric search-contact(contactlist contact)
  (:documentation "Search a contact in a contact list."))

(defmethod search-contact((cl contactlist) search-fn )
  (remove-if-not search-fn (contact-list cl)))




(defparameter *contactlist* (make-instance 'contactlist))

(add-contact *contactlist* (make-instance 'contact :name "jim"))
(add-contact *contactlist* (make-instance 'contact :name "john"))

(print
 (contact-list *contactlist*))

(print
 (contact-name (car
  (search-contact *contactlist* #'(lambda(c) (string= "jim" (contact-name c)))))))
