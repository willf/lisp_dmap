(in-package "CASPER")
;;------------------------------------------------------------------------------
;; 
;; File:    MICRO-CASPER.LISP 
;; Created: 11/10/94
;; Author:  Will Fitzgerald
;; 
;; Description: A 'micro' version of the Casper Customer Service Representative
;;              Tutor
;; 
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------

(eval-when (load eval compile)
  (unless (find-package :casper)
    (make-package :casper)))

(in-package :casper)
(use-package :tables)
(use-package :icp)

(export '(clear-statements def-csr def-cust def-response *casper-output* 
          *casper-output* casper))


;;------------------------------------------------------------------------------
;;  Statements and responses
;;------------------------------------------------------------------------------
;; A statement has a symbolic form and an English form. We store these in tables
;; keyed on the symbolic form. The response of a customer is stored in a table
;; keyed on the symbolic form of the CSR.

(defclass statement ()
  ((symbolic-form :initarg :symbolic-form :initform nil :accessor symbolic-form)
   (english-form :initarg :english-form :initform nil :accessor english-form)))

(defmethod print-object ((self statement) stream)
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~s" (symbolic-form self))))

(defclass csr-statement (statement) ())
(defclass customer-statement (statement) ())

(deftable cust-form-of)
(deftable csr-form-of)
(deftable response-of)

(defun clear-statements ()
  (clear-table (cust-form-of))
  (clear-table (csr-form-of))
  (clear-table (response-of)))

(defun csr->cust (csr-statement)
  "from a CSR statement (symbolic form) to a Customer's response"
  (cust-form-of (response-of csr-statement)))


;;------------------------------------------------------------------------------
;; CSR, Customer and Response definition macros.
;;------------------------------------------------------------------------------

(defmacro def-csr (symbolic-form english-form)
  `(progn
     (setf (csr-form-of ',symbolic-form)
           (make-instance 'csr-statement
          :symbolic-form ',symbolic-form
          :english-form ,english-form)) ',symbolic-form))

(defmacro def-cust (symbolic-form english-form)
  `(progn
     (setf (cust-form-of ',symbolic-form)
           (make-instance 'customer-statement
             :symbolic-form ',symbolic-form
             :english-form ,english-form)) ',symbolic-form))

(defmacro def-response (csr-statement cust-statement)
  `(setf (response-of ',csr-statement) ',cust-statement))

;;------------------------------------------------------------------------------
;; Interface functions. These are very primative.
;;------------------------------------------------------------------------------

(defvar *casper-output* *standard-output*)
(defvar *casper-input* *standard-input*)
(defvar *csr-prompt* "CSR? ")

(defun display-csr-statement (csr-statement)
  (format *casper-output* "~%CSR: ~A" (english-form csr-statement)))

(defun display-cust-statement (cust-statement)
  (format *casper-output* "~%Customer: ~A~%" (english-form cust-statement)))

(defun get-csr-statement ()
  (terpri *casper-output*)
  (princ *csr-prompt* *casper-output*)
  (let ((words (cl-user::->symbols (read-line  *casper-input* "" nil))))
    (icp words)
    (target-concept (choose-best (best-results 7) :key 'what-to-display))))

(defun what-to-display (result)
  (format nil "~4,2F ~S ~A" 
          (score result)
          (target-concept result)
          (let ((csr-statement (csr-form-of (target-concept result))))
            (if csr-statement (english-form csr-statement) ""))))
          
(defun choose-best (list &key (key 'identity))
  (format *casper-output* "~%Choose the best choice (0 for none):")
  (loop for item in list
        for i from 1 doing
        (format t "~%~2,D. ~A" i (funcall key item)))
  (format *casper-output* "~%~A" *csr-prompt*)
  (let ((result (read *casper-input* nil 0)))
    (if (or (not (integerp result)) (= result 0)) nil
        (nth (1- result) list))))

;;------------------------------------------------------------------------------
;; The Casper main loop
;;------------------------------------------------------------------------------
(defun simple-casper-loop (csr-statement-name)
  (let ((csr-statement (csr-form-of csr-statement-name)))
    (when csr-statement
      (display-csr-statement csr-statement)
      (let ((cust-statement (csr->cust csr-statement-name)))
        (when cust-statement
          (display-cust-statement cust-statement)
          (when (eq (symbolic-form cust-statement) 'cl-user::ring-off)
            (return-from simple-casper-loop t))))))
  (simple-casper-loop (get-csr-statement)))

(defun casper ()
  (simple-casper-loop 'cl-user::greet-customer))
