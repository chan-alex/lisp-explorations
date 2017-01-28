

(defclass contactlist()
  ((contactlist :accessor contact-list
                :initform '())))

(defclass contact()
  ((name :accessor contact-name
	 :initarg :name
	 :initform "")
   (info :accessor contact-info
	 :initform '())))



(defgeneric add_contact(contactlist contact)
  (:documentation "Adds a contact to the list."))

(defmethod add_contact((cl contactlist) contact)
  (push contact (contact-list cl)))



(defparameter *contactlist* (make-instance 'contactlist))

(add_contact *contactlist* (make-instance 'contact :name "jim"))
(add_contact *contactlist* (make-instance 'contact :name "john"))

(print
 (contact-list *contactlist*))
