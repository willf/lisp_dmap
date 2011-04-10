(in-package "FRAMES")
;;------------------------------------------------------------------------------
;; 
;; File:    MICRO-FRAME.LISP 
;; Created: 10/17/94
;; Author:  Will Fitzgerald
;; 
;; Description: based on various versions of frame code developed by Chris
;;              Riesbeck
;; 
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------------

(eval-when (load eval compile)
  (unless (find-package :frames)
    (make-package :frames)))

(in-package :frames)

(use-package :tables)

(export '(define-frame def-frame 
           frame-of ->frame ->name frame-p
           abstractions specializations all-abstractions features
           attribute-value inherited-attribute-value
           abstp specp part-of whole-of 
           abst-or-whole-of spec-or-part-of
           find-frame
           clear-frame-memory print-frames))

;;------------------------------------------------------------------------------
;; data structures for frames: Frames form a class, whose instances are stored
;; in a table keyed by their symbolic names.
;;------------------------------------------------------------------------------

(deftable frame-of)

(defclass frame ()
  ((name :initarg :name :accessor name)
   (abstractions :initarg :abstractions :initform nil :accessor abstractions)
   (specializations :initarg :specializations :initform nil :accessor specializations)
   (all-abstractions :initarg :all-abstractions :accessor all-abstractions)
   (features :initarg :features :initform nil :accessor features)))

(defmethod print-object ((frame frame) stream)
  (format stream "[~S]" (name frame)))

(defmethod name ((frame t)) frame)

(defun frame-p (object)
  (typep object (find-class 'frame)))

(defun ->frame (object)
  (if  (frame-p object) object
       (frame-of object)))

(defun ->name (object)
  (if (frame-p object)
    (name object)
    object))

(defun force-frame (name)
  (or (frame-of name)
      (setf (frame-of name) 
            (make-instance 'frame 
              :name name
              :all-abstractions (list name)))))

;;------------------------------------------------------------------------------
;; Data structure for features (slots). A attribute/value pair.
;;------------------------------------------------------------------------------

(defclass feature ()
  ((attribute :initarg :attribute :accessor attribute)
   (value :initarg :value :initform nil :accessor value)))

(defun feature-p (object)
  (typep object (find-class 'feature)))

(defmethod make-feature (attribute value)
  (make-instance 'feature :attribute attribute :value value))

(defun make-features (attribute-value-list)
  (loop for (attribute value) in attribute-value-list
        collect (make-feature attribute value)))

(defmethod print-object ((feature feature) stream)
  (with-slots (attribute value) feature
    (print-unreadable-object (feature stream :type t :identity t)
      (format stream "~S ~S" attribute value))))


(defmethod feature-named ((frame frame) attribute)
  (loop for feature in (features frame)
        when (eq (attribute feature) attribute)
        return feature))
  
(defmethod attribute-value ((frame frame) attribute)
  (let ((feature (feature-named frame attribute)))
    (if feature (value feature) nil)))

(defmethod (setf attribute-value) (value (frame frame) attribute)
  (let ((feature (feature-named frame attribute)))
    (if feature 
      (setf (value feature) value)
      (let ((new-feature (make-feature :attribute attribute :value value)))
        (push new-feature (features frame))
        value))))

;; inherited attribute values

(defun inherited-attribute-value (frame attribute)
  (or (attribute-value (->frame frame) attribute)
      (loop for abstraction in (abstractions frame)
            thereis (inherited-attribute-value (->frame abstraction) attribute))))
 
(defmethod part-of ((part frame) (whole frame))
  (member (name part) (all-features whole)
          :key 'value))

(defmethod part-of ((part t) (whole t))
  (let ((whole (frame-of whole))
        (part (frame-of part)))
    (if (and whole part)
      (part-of part whole)
      nil)))

(defmethod whole-of ((whole t) (part t))
  (part-of whole part))

;;------------------------------------------------------------------------------
;; Abstractions and specializations
;;------------------------------------------------------------------------------

(defmethod all-abstractions ((frame t))
  (let ((frame-maybe (frame-of frame)))
    (if frame-maybe
      (all-abstractions frame-maybe)
      (list frame))))

(defmethod update-specializations ((frame frame))
  (loop for abstraction in (abstractions frame) doing
        (setf (specializations (force-frame abstraction))
              (pushnew (name frame) (specializations (frame-of abstraction))))))

(defmethod update-abstractions ((frame frame)) 
  (setf (all-abstractions frame)
        (calculate-all-abstractions frame))
  (loop for specialization in (specializations frame) doing
        (update-abstractions (frame-of specialization))))
                    
(defmethod calculate-all-abstractions* ((frame frame))
  (cond 
   ((null (abstractions frame)) nil)
   (t (append (abstractions frame)
              (loop for abstraction in (abstractions frame)
                    appending
                    (calculate-all-abstractions* (force-frame abstraction)))))))

(defmethod calculate-all-abstractions ((frame frame))
  (cons (name frame) (remove-duplicates (calculate-all-abstractions* frame))))


(defmethod abstp ((abst frame) (spec frame))
  (member (name abst) (all-abstractions spec) :test 'eq))

(defmethod abstp ((abst t) (spec t))
  (let ((af (frame-of abst))
        (sf (frame-of spec)))
    (if (and af sf)
      (abstp af sf)
      (eql abst spec))))

(defmethod specp ((spec t) (abst t))
  (abstp abst spec))


(defun abst-or-whole-of (big small)
  (or (abstp big small)
      (whole-of big small)))

(defun spec-or-part-of (small big)
  (or (specp small big)
      (part-of small big)))

;;------------------------------------------------------------------------------
;; Interface to clear memory and define frames
;;------------------------------------------------------------------------------

(defun clear-frame-memory ()
  (clear-table (frame-of)))

(set-macro-character
 #\[
 #'(lambda(stream char)
     (declare (ignore char))
     `(frames:frame-of ',@(read-delimited-list #\] stream t)))
 nil  ; not non-terminating.  Cannot be embedded w/in symbols
 )

;;; causes a right-bracket w/o a left to signal an error
(set-macro-character #\] (get-macro-character #\) ) nil)

(defun define-frame (name abstractions attribute-value-list)
  (let ((frame (force-frame name)))
    (setf (abstractions frame) (mapcar 'name abstractions))
    (setf (features frame) (make-features attribute-value-list))
    (update-specializations frame)
    (update-abstractions frame)
    frame))

(defmacro def-frame (name &optional abstractions &rest attribute-value-list)
  `(define-frame ',name ',abstractions ',attribute-value-list))

;;------------------------------------------------------------------------------
;; Frame finding
;;------------------------------------------------------------------------------

(defmethod all-features ((frame frame))
  (remove-duplicates
   (append 
    (loop for abstraction in (abstractions frame)
          appending (all-features (frame-of abstraction)))
    (features frame))
   :key 'attribute))
           
   
(defun find-frame (abst features)
  "Find a frame starting at abst, with the features listed."
  (if (null features) (->frame abst)
      (let ((specs (find-specs abst features)))
        (if (and (null (rest specs))
                 (features-subsetp features (first specs)))
          (->frame (first specs))
          (define-frame 
                  (gen-frame-name (first specs))
                  specs features)))))

(defun find-specs (abst features)
  "Find the most specific specialization of abst."
  (or (remove-duplicates
       (loop for spec in (specializations (->frame abst))
             when (features-abstp spec features)
             nconc (find-specs spec features)))
      (list abst)))

(defun features-abstp (abst features)
  (loop for (attribute value) in features
        always
        (abstp (inherited-attribute-value (->frame abst) attribute) value)))

(defun features-subsetp (features abst)
  (subsetp features (all-features (->frame abst))
           :test
           #'(lambda (feature-list feature)
               (and (eql (first feature-list) (attribute feature))
                    (eql (second feature-list)  (value feature))))))


(defun gen-frame-name (name)
  (gentemp (format nil "~A-" (symbol-name name))))

(defun features->feature-specs (features)
  (loop for feature in features 
        collecting (list (attribute feature) (value feature))))


;;; Printing utilities
;;; ----------------------------------------------------------------------

;;; (DISPLAY-FRAME frame [stream]) => no values

;;; DISPLAY-FRAME prints the frame in a readable fashion on the stream
;;; (which defaults to the standard output). The frame argument
;;; can be either the name of a frame or an internal frame structure.
;;; Nested frames are printed in full form the first time they are
;;; seen.

(defun display-frame (frame &optional (stream *standard-output*))
  (cond ((null frame) nil)
	((not (frame-p frame))
	 (display-frame (frame-of frame)))
	(t
	 (let ((*frames-shown* '()))
	   (declare (special *frames-shown*))
	   (format stream "~%~S~%" (name frame))
	   (pprint-frame-info frame stream 4)
	   (format stream "~%")
	   (values)))))

;;; (PPRINT-frame-INFO frame stream left-margin) => undefined

;;; PPRINT-frame-INFO prints internal frame structures in a readable
;;; fashion on stream, indented left-margin number of spaces.

(defun pprint-frame-info (frame stream left-margin)
  (declare (special *frames-shown*))
  (unless (or (null frame) (member frame *frames-shown*))
    (push frame *frames-shown*)
    (loop for abst in (abstractions frame)
	  do (format stream "~VT:ISA ~S~%" left-margin abst))
    (loop for feature in (features frame)
	  do (format stream "~VT~S ~S~%" 
		     left-margin 
		     (attribute feature)
		     (value feature))
	     (pprint-frame-info (frame-of (value feature))
			      stream
			      (+ left-margin 4)))))

(defun display-frames-with-roles (name roles
		                       &optional (stream *standard-output*) 
		                       &aux shown)
  (labels ((show (name prefix)
             (let* ((frame (->frame name))
                    (specs (specializations frame))
                   (features (and roles (features frame))))
               (cond ((member name shown)
                      (format stream
			      (if (or specs features) "~S...~%" "~S~%") name))
                     (t
                      (format stream "~S~%" name)
                      (push name shown)
                      (when features
                        (let ((bar (if specs "|" " ")))
                          (dolist (feature features)
                            (when (and
                                   (feature-p feature)
                                   (member (attribute feature) roles))
                              (format stream "~A ~A ~S ~S~%" prefix bar
                                      (attribute feature)
                                      (value feature))))))
                      (when specs
                        (do ((next-prefix (format nil "~A |   " prefix))
                             (last-prefix (format nil "~A     " prefix))
                             (l specs (cdr l)))
                            ((null (cdr l))
                             (format stream "~A +-- " prefix)
                             (show (car l) last-prefix))
                          (format stream "~A |-- " prefix)
                          (show (car l) next-prefix))))))))
    (show name "")
    name))
