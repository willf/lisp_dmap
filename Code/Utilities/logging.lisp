(in-package "LOG")
;;------------------------------------------------------------------------------
;; 
;; File:    LOGGING.LISP 
;; Created: 10/19/94
;; Author:  Will Fitzgerald
;; 
;; Description: A simple logging facility
;; 
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------
(eval-when (load eval compile)
  (unless (find-package :log)
    (make-package :log)))

(in-package :log)
(use-package :tables)

(export '(reset-log set-logging record-log print-log with-logging))


;;------------------------------------------------------------------------------
;; A log is a list of statements keyed off a symbolic form.
;;------------------------------------------------------------------------------
(deftable log-of)

(defvar *logging* nil)
(defvar *log-keys* nil)

(defun reset-log ()
  (clear-table (log-of))
  (setf *log-keys* nil)
  *logging*)

;;------------------------------------------------------------------------------
;; Turning logging off and on.
;;------------------------------------------------------------------------------
(defun set-logging (&optional (value t))
  (setf *logging* value))

(defmacro with-logging (&rest body)
  `(let ((*logging* t))
    (reset-log)
    ,@body))

;;------------------------------------------------------------------------------
;; Making records in the log
;;------------------------------------------------------------------------------
(defun make-statement (string args)
  (format nil "~?" string args))

(defun record-log (logname string &rest args)
  (when *logging* 
    (push (make-statement string args) (log-of logname))
    (pushnew logname *log-keys* )
    *logging*))

;;------------------------------------------------------------------------------
;; Printing the log
;;------------------------------------------------------------------------------

(defun print-log (&optional logname (stream *standard-output*))
  (if logname
    (loop for log-entry in (reverse (log-of logname))
          doing
          (format stream "~A~%" log-entry))
    (loop for log-key in (reverse *log-keys*) doing
          (print-log log-key stream)))
  (values))




#|

(defun fact (n)
  (record-log 'fact "entering FACT with ~S" n)
  (cond
   ((= n 1) 1)
   (t (* (fact (1- n)) n))))

(set-logging)
(reset-log)
(fact 20)
(print-log)

(set-logging nil)
(with-logging (fact 4))
(print-log)
|#
