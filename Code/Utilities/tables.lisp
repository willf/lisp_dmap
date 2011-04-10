;;; A simple table utility
;;; ----------------------------------------------------------------------
;;; - File: tables.lisp
;;; - Author: Chris Riesbeck
;;; - Most recent update: 7/27/94

;;; ----------------------------------------------------------------------
;;; Defining a table function
;;; ----------------------------------------------------------------------

;;; (DEFTABLE name) => name
;;;
;;; DEFTABLE defines name to be a table function such that
;;; 
;;;   -  (name key) retrieves a value for key, if any
;;;   -  (SETF (name key) value) stores a value for key
;;;   -  (name) returns the internal table associated with name;
;;;      this is useful when manipulating tables (see below).
;;;
;;; The table is empty when name is defined (or redefined).
;;;
;;; Examples:
;;;
;;;   > (deftable AGE-of)
;;;   AGE-OF
;;;   > (age-of 'john)
;;;   NIL
;;;   > (setf (age-of 'john) 22)
;;;   22
;;;   > (age-of 'john)
;;;   22
;;;
;;; Note: DEFTABLE is a top-level form, like DEFUN. It is not for
;;; creating local table functions.  The following is wrong:
;;;
;;;   (defun foo (...)
;;;     (deftable baz)
;;;     ...)
;;;
;;; If you want a local table, use MAKE-HASH-TABLE and GETHASH.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load eval compile)
  (unless (find-package :tables)
    (make-package :tables)))

(in-package :tables)

(export '(deftable in-table-p remove-key clear-table map-table))


;;; ----------------------------------------------------------------------
;;; Implementation notes:
;;; 
;;;   - I avoided (DEFUN (SETF fn) ...) so as not to require CL 2
;;;   - I used PROGN to make the DEFSETF top-level for MacIntosh
;;;     Common Lisp.

(defmacro deftable (fn &optional test)
  (let ((set-fn (gensym)))
    `(progn
      (let* ((fn ',fn)
             (table (get-table fn ,test)))
        (defun ,fn (&optional (key nil key-given-p))
          (if key-given-p
            (gethash key table)
            table))
        (defun ,set-fn (arg1 &optional (arg2 nil arg2-p))
          (cond (arg2-p
                 (setf (gethash arg1 table) arg2))
                (t (set-table fn arg1)))))
      (defsetf ,fn ,set-fn)
      ',fn)))

(defvar *tables* (make-hash-table)
  "Table of DEFTABLE functions.")

(defun get-table (name test)
  (set-table name (make-hash-table :test (or test #'eql))))

(defun set-table (name table)
  (if (hash-table-p table)
      (setf (gethash name *tables*) table)
      (error "~S not a table" table)))

;;; ----------------------------------------------------------------------
;;; Manipulating tables
;;; ----------------------------------------------------------------------

;;; Certain functions need explicit access to the internal table. To
;;; get this table, call the table function with no arguments, e.g.,
;;; (AGE-OF).  This returns the internal table for AGE-OF, which
;;; can then be passed to a table manipulation function.
;;;
;;; Example: The following clears the AGE-OF table.
;;;
;;;   > (clear-table (age-of))
;;;
;;; The nature of the internal table is implementation-dependent.

;;; (IN-TABLE-P key table) => T or NIL
;;;   Returns true if key has a value in the table.
;;; (REMOVE-KEY key table) => T or NIL
;;;   Removes any entry for key in the table, and returns true
;;;   if there was one.
;;; (CLEAR-TABLE table) => table
;;;   Removes all entries from the table.
;;; (MAP-TABLE function table) => NIL
;;;   Calls (function key value) for every key and value in the table.
;;;   The order in which keys are found is implementation-dependent.

;;; ----------------------------------------------------------------------
;;; Implementation notes:
;;; 
;;;   - I avoided MULTIPLE-VALUE-BIND for Xlisp compatibility.

(let ((flag (list nil)))
  (defun in-table-p (key table)
    (not (eq flag (gethash key table flag)))))

(defun remove-key (key table) (remhash key table))

(defun clear-table (table) (clrhash table))

(defun map-table (fn table) (maphash fn table))

(provide "tables")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
7/27/94 [CKR]
Problem: If name is a function, (DEFTABLE name) would cause an error.
Cause:   Calling (name) doesn't do the right thing.
Change:  Store name->table associations in the table *TABLES*.

12/1/93 [CKR] 
Problem: If several packages used TABLES, they each loaded separate
         copies of TABLES.
Cause:   No TABLES package (because all functions were exported) that
         they could use.
Change:  Set up TABLES package.

11/4/92 [CKR] 
Problem: In some Lisps, e.g., MCL, the DEFSETF in DEFTABLE wasn't
         happening at the right time in compiled code.
Cause:   DEFSETF, a top-level form, was inside the LET.
Change:  Put DEFSETF outside the LET, in a PROGN.

9/30/92 [CKR] 
Problem: IN-TABLE-P returned multiple values instead of just T or NIL
Cause:   IN-TABLE-P defined as a simple call to GETHASH
Change:  Use (NOT (EQ flag (GETHASH ... flag)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#
