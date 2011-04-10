(in-package "DMAP")
;;------------------------------------------------------------------------------
;; 
;; File:    MICRO-DMAP.LISP 
;; Created: 10/19/94
;; Author:  Will Fitzgerald
;; 
;; Description: Direct Memory Access Parsing.
;; based on various versions of DMAP by Chris Riesbeck.
;; 
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------

(eval-when (load eval compile)
  (unless (find-package :dmap)
    (make-package :dmap)))

(in-package :dmap)

(use-package :tables)
(use-package :frames)

(export '(add-phrasal-pattern def-phrase def-phrases
          parse reset-parser
          clear-predictions 
          call-backs))

;;------------------------------------------------------------------------------
;; Data structure for predictions. These are stored in tables keyed on the
;; "target" of their first phrasal pattern element
;;------------------------------------------------------------------------------

(defclass prediction ()
  ((base :initarg :base :initform nil :accessor base) 
   (phrasal-pattern :initarg :phrasal-pattern :initform nil :accessor phrasal-pattern) 
   (start :initarg :start :initform nil :accessor start)
   (next :initarg :next :initform nil :accessor next) 
   (slots :initarg :slots :initform nil :accessor slots)))

(defun make-prediction (&key base phrasal-pattern start next slots) 
  (make-instance 'prediction 
    :base base :phrasal-pattern phrasal-pattern :start start :next next :slots slots))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (tables:deftable anytime-predictions-on)
  (tables:deftable dynamic-predictions-on))

(defun add-phrasal-pattern (base phrasal-pattern)
  "Adds the phrasal pattern of base to the table of static predictions."
  (if (and (eql base (first phrasal-pattern)) (null (rest phrasal-pattern)))
    nil
    (progn (index-anytime-prediction
            (make-prediction :base base :phrasal-pattern phrasal-pattern)) 
           phrasal-pattern)))

(defmacro def-phrase (base &rest phrasal-pattern)
  (if (and (eql base (car phrasal-pattern)) (null (cdr phrasal-pattern)))
      (error "~S can't reference itself" base)
      `(progn (add-phrasal-pattern ',base ',phrasal-pattern)
              ',phrasal-pattern)))

(defmacro def-phrases (base &rest phrasal-patterns)
  `(loop for phrasal-pattern in ',phrasal-patterns doing
         (add-phrasal-pattern ',base phrasal-pattern)))

(defun index-anytime-prediction (prediction)
  "Put the phrasal pattern/prediction in the table for its target."
  (push prediction (anytime-predictions-on (prediction-target prediction))))

(defun index-dynamic-prediction (prediction)
  "Put the phrasal pattern/prediction in the table for its target."
  (push prediction (dynamic-predictions-on (prediction-target prediction))))

(defun predictions-on (index)
  (append (anytime-predictions-on index)
          (dynamic-predictions-on index)))

(defun clear-predictions (&optional (which :dynamic))
  (ecase which
    (:dynamic (clear-table (dynamic-predictions-on)))
    (:anytime (clear-table (anytime-predictions-on)))
    (:all (clear-table (dynamic-predictions-on))
          (clear-table (anytime-predictions-on)))))

;;------------------------------------------------------------------------------
;; Misc. data structures.
;;------------------------------------------------------------------------------

(defvar *dmap-pos* 0)           ;;global text position

;; Call backs are ad-hoc functions run when a concept (or one of its
;; specializations) is referenced. Function should take three
;; parameters: the item referenced, the start position in the text, and
;; the end position in the text.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (tables:deftable call-backs))

;;------------------------------------------------------------------------------
;; To parse is to reference every word in the text, looking for predictions
;; on the words.
;;------------------------------------------------------------------------------

(defun parse (sent)
  (dolist (w sent)
    (setq *dmap-pos* (1+ *dmap-pos*))
    (reference w *dmap-pos* *dmap-pos*)))

(defun reference (item start end)
  (dolist (abst (all-abstractions item))
    (dolist (prediction (predictions-on abst)) 
      (advance-prediction prediction item start end))
    (dolist (fn (call-backs abst)) 
      (funcall fn item start end))))

(defun advance-prediction (prediction item start end)
  "Advancing a phrasal pattern/prediction means:
   if the predicted phrasal pattern has been completely seen, to reference 
   the base of the prediction with the slots that have been collected;
   otherwise, to create a new prediction for the next item in the
   prediction phrasal pattern."
  (when (or (null (next prediction))
            (= (next prediction) start))
    (let ((base (base prediction))
          (phrasal-pattern (cdr (phrasal-pattern prediction)))
          (start (or (start prediction) start))
          (slots (extend-slots prediction item)))      
      (if (null phrasal-pattern)
        (reference (find-frame base slots) start end)
        (index-dynamic-prediction  
         (make-prediction :base base :phrasal-pattern phrasal-pattern :start start :next (1+ *dmap-pos*) 
                          :slots slots))))))

(defun extend-slots (prediction item)
  (let ((spec (first (phrasal-pattern prediction)))
        (slots (slots prediction)))
    (if (role-specifier-p spec)
        (if (abstp item (prediction-target prediction))
            slots
            (cons (list (role-specifier spec) (->name item)) slots))
        slots)))

(defun prediction-target (prediction)
  "The target of a phrasal pattern is based on the first item in the
   phrasal pattern yet to be seen. 
   If that item is a role-specifier, then the target is the 
   inherited filler of its role;
   Otherwise, it is just the item itself."
  (let ((spec (first (phrasal-pattern prediction))))
    (if (role-specifier-p spec)
        (let ((base (base prediction)))
          (or (inherited-attribute-value (frame-of base) (role-specifier spec))
              (error "~S not a role in ~S" (first spec) base)))
        spec)))

(defun role-specifier-p (item) (keywordp item))
(defun role-specifier (item) item)

;;------------------------------------------------------------------------------
;; Resetting the parser.
;;------------------------------------------------------------------------------

(defun reset-parser ()
  (setf *dmap-pos* 0)
  (clear-predictions :dynamic)
  t)
