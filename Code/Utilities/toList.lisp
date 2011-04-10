(in-package "COMMON-LISP-USER")
;;------------------------------------------------------------------------------
;; 
;; File:    ->LIST.LISP 
;; Created: 2/25/93
;; Author:  Will Fitzgerald
;; 
;; Description: Simple conversion utilities for strings to lists
;; 
;;------------------------------------------------------------------------------


(defmethod ->list ((self string) &key 
                   (start 0) 
                   (char-bag '(#\Space))
                   (test #'(lambda (ch) (not (member ch char-bag :test 'char=))))
                   (post-process 'identity))
  "Converts SELF into a list,
     starting at START;
     dividing words at boundaries defined by characters in CHAR-BAG,
                 or at boundaries defined by TEST;
     each item is run through POST-PROCESS as it is created. POST-PROCESS can
     be destructive (eg, NSTRING-DOWNCASE)."
  (labels ((->list* (position)
             (let* ((pos (position-if-not test self :start position))
                    (new-pos (if pos (position-if test self :start pos) nil)))
               (cond
                ((and pos new-pos)
                 (cons (funcall post-process (subseq self position pos))
                       (->list* new-pos)))
                (pos (list (funcall post-process (subseq self position pos))))     
                (t (list (funcall post-process (subseq self position))))))))
    
    (let ((pos (position-if test self :start start)))     
      (if pos (->list*  pos) nil))))

(defmethod ->symbols ((self string) &optional (package *package*))
  "Converts a string into a list of symbols interned into PACKAGE, ignoring
   everything but alphanumerics and dashes."
  (->list self 
          :post-process #'(lambda (str) 
                            (intern (nstring-upcase str) package))
          :test #'(lambda (ch) (or (alphanumericp ch)
                                   (char= ch #\-)))))

(defmethod ->symbols ((self null) &optional (package *package*))
  (declare (ignore package)) nil)

                   
  
