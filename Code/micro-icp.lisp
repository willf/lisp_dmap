(in-package "ICP")
;;------------------------------------------------------------------------------
;; 
;; File:    MICRO-ICP.LISP 
;; Created: 10/20/94
;; Author:  Will Fitzgerald
;; 
;; Description: Micro version of indexed concept parsing.
;; 
;;------------------------------------------------------------------------------

(eval-when (load eval compile)
  (unless (find-package :icp)
    (make-package :icp)))

(in-package :icp)

(use-package :frames)
(use-package :dmap)
(use-package :tables)
(use-package :log)

(export '(def-assoc clear-icp-memory icp information-value
           best-results *icp-results* print-icp-log m-reference-concept m-root
           score target-concept index-concepts ticp))

;;------------------------------------------------------------------------------
;; Data structures for index concepts, target concepts, and their relationships
;;------------------------------------------------------------------------------

;; an index set is a target concept and its associated index concepts

(defclass index-set ()
  ((target-concept :initarg :target-concept :accessor target-concept)
   (indices :initarg :indices :accessor indices)))

(defmethod print-object ((self index-set) stream)
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~s "  (target-concept self))
      (format stream "~s"(indices self))))

;; Data tables:
;; whether an object is a target concept;
;; from an index to all of the index sets it participates in;
;; from an index to all of the target concepts for which it is associated;
;; from an index to the number of target concepts it's associated with (for
;; calculating information value)

(deftable target-concept-p)
(deftable index->index-sets)
(deftable index->target-concepts)
(deftable index->target-concepts-cardinality)

(defun target-concept-cardinality ()
  "How many target concepts -- for calculating information value"
  (hash-table-count (target-concept-p)))

;;------------------------------------------------------------------------------
;; Installation of index sets.
;;------------------------------------------------------------------------------

(defmethod equal-instance-p ((index-set1 index-set) (index-set2 index-set))
  (and (eql (target-concept index-set1)
            (target-concept index-set2))
       (equal (indices index-set1)
              (indices index-set2))))

(defun set-index->target-concepts-cardinality (index)
  (if (index->target-concepts-cardinality index)
    (incf (index->target-concepts-cardinality index))
    (setf (index->target-concepts-cardinality index) 1)))

(defmethod install ((index-set index-set))
  (with-slots (target-concept indices) index-set
  (dolist (index indices)
    (unless (frame-of index)
      (warn "~S does not name a frame." index))
    (unless (member index (index->target-concepts index))      
      (push target-concept (index->target-concepts index))
      (set-index->target-concepts-cardinality index))
    (pushnew index-set (index->index-sets index) :test 'equal-instance-p))))

(defun add-index-set (target-concept indices)
  (setf (target-concept-p target-concept) t)
  (install (make-instance 'index-set 
             :target-concept target-concept
             :indices indices))
  target-concept)

(defmacro def-assoc (name &rest indices)
  `(progn
     (define-frame ',name '(m-reference-concept) nil)
     (add-index-set ',name ',indices)))


;;------------------------------------------------------------------------------
;; Class for result from the parser
;;------------------------------------------------------------------------------

(defclass icp-result ()
    ((score :initarg :score :initform 0 :accessor score)
     (target-concept :initarg :target-concept :initform nil :accessor target-concept)
     (index-concepts :initarg :index-concepts :initform nil :accessor index-concepts)))

(defmethod print-object ((self icp-result) stream)
  (with-slots (score target-concept index-concepts) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~4,2F ~S ~S" score target-concept index-concepts))))

(defun make-icp-result (score target-concept index-concepts)
  (make-instance 'icp-result
    :score score
    :target-concept target-concept
    :index-concepts index-concepts))

(defmethod score ((result null)) 0)

(defmethod target-concept ((result null)) nil)

(defmethod index-concepts ((result null)) nil)
  
;;------------------------------------------------------------------------------
;; ICP proper
;;------------------------------------------------------------------------------

(defvar *icp-results* () "A place to store the results of the parser")

(defun icp (words &optional (match-fn 'words->indices))
  (setf *icp-results*
        (remove-duplicates 
         (sort (score-index-sets (find-indices words match-fn))
               #'> :key #'score)
         :key 'target-concept :from-end t))
  (first *icp-results*))

(defun best-results (&optional n)
  (if n (first-n *icp-results* n) *icp-results*))

(defun find-indices (words match-fn)
  (funcall match-fn words))

(defun score-index-sets (found-indices)
  (loop for index-set in (candidate-index-sets found-indices)
        collect (make-icp-result
                 (index-set-score index-set found-indices)
                 (target-concept index-set)
                 (indices index-set))))

;;------------------------------------------------------------------------------
;; Find all candidate index sets from the index concepts seen
;;------------------------------------------------------------------------------

(defun candidate-index-sets (found-indices)
  (remove-duplicates
   (loop for index in (all-absts-in found-indices)
         append (index->index-sets index))))

(defun all-absts-in (concepts)
  (remove-duplicates 
   (loop for concept in concepts
         append (all-abstractions (frame-of concept)))))


;;------------------------------------------------------------------------------
;; Calculate the scores for each candidate index set
;; The real work is done by the appraiser functions. INDEX-SET-SCORE
;; just adds them up.  Appraisers with no votes are not called.
;;------------------------------------------------------------------------------

        
(defun index-set-score (index-set found-indices)
  (let ((score 0))
    (map-table #'(lambda (appraiser votes)
                   (unless (zerop votes)
                     (incf score
                           (call-appraiser appraiser 
                                           index-set found-indices))))
               (appraiser-votes))
    (log:record-log (target-concept index-set)
                    "Total score for target ~S  = ~5,3F"
                    (target-concept index-set)  score)
    (log:record-log (target-concept index-set)
                    "Associated index concepts: ~S~%~&~75,,,'-<~>~%" 
                    (indices index-set))
    score))

(defun call-appraiser (appraiser index-set found-indices)
  (let ((score 
         (* (funcall appraiser index-set found-indices)
            (appraiser-weight appraiser))))
    (log:record-log (target-concept index-set)
                    "~A score = ~5,3F (Raw score * ~5,3F weighting)~%"
                    appraiser score (appraiser-weight appraiser))
    score))
  
;;------------------------------------------------------------------------------
;; Information value functions
;;------------------------------------------------------------------------------

(defun probability-of-index (index)
  (let ((cardinality (index->target-concepts-cardinality index)))
    (if (null cardinality) least-positive-short-float 
        (/ cardinality 
           (target-concept-cardinality)))))

(defun information-value (index)
  (- (log (probability-of-index index) 2)))

;;------------------------------------------------------------------------------
;; Appraisers
;;------------------------------------------------------------------------------

;;; An appraiser is a function assigned a non-zero number of votes.
;;; The function should take an index-set and an found-indices and return
;;; a score between 0 and 1 inclusive. The score is then multiplied by
;;; the appraiser's weight, which is the number of votes associated
;;; with the appraiser divided by the total number of votes for all
;;; appraisers. 

;;; (ASSIGN-VOTES name [votes]) => name
;;;   Assigns the given number of votes to an appraiser. If no votes
;;;   are specified, 1 is assumed.

;;; Bookkeeping:
;;;
;;; As votes are assigned, we keep track of the total votes, to speed
;;; up calculating relative weights at parse time.

(deftable appraiser-votes)

(defvar *total-votes* 0
  "Total number of votes for appraisers.")

(defun clear-appraisers ()
  (clear-table (appraiser-votes))
  (setf *total-votes* 0))

(defun assign-votes (name &optional (votes 1))
  (setf (appraiser-votes name) votes)
  (tally-votes)
  name)

(defun tally-votes ()
  (setq *total-votes* 0)
  (map-table #'(lambda (name votes)
                 (declare (ignore name))
                 (incf *total-votes* votes))
             (appraiser-votes)))

(defun appraiser-weight (appraiser)
  (/ (appraiser-votes appraiser) *total-votes*))

;;------------------------------------------------------------------------------
;; Default appraiser functions
;;------------------------------------------------------------------------------

;;; Each of these appraisers compares a given index set against the
;;; pool of indices actually seen in the input:
;;; 
;;;   PREDICTED-SCORE -- how many predicted items were seen?
;;;   UNPREDICTED-SCORE -- how many items were not predicted?
;;;   UNSEEN-SCORE -- how many predicted items were not seen?


(defun predicted-score (index-set found-indices)
  (let* ((predicted (indices index-set))
         (predicted-items (predicted-items found-indices predicted))
         (score
          (/ (summed-value (target-concept index-set) predicted-items 'identity)
             (summed-value (target-concept index-set) predicted 'identity))))
    (log:record-log  (target-concept index-set)
                     "Predicted raw score = ~5,3F (successfully predicted / predicted)"
                     score)
    score))

(defun unpredicted-score (index-set found-indices)
  (let* ((predicted (indices index-set))
         (unpredicted-items (unpredicted-items found-indices predicted))
         (score
          (- 1 (/ (summed-value (target-concept index-set) unpredicted-items 'identity)
                  (summed-value (target-concept index-set) found-indices 'identity)))))
    (log:record-log  (target-concept index-set)
                     "Unpredicted raw score = ~5,3F (1 - unpredicted / seen)"
                     score)
    score))

(defun unseen-score (index-set found-indices)
  (let* ((predicted (indices index-set))
         (unseen-items (unseen-items found-indices predicted))
         (score
          (- 1 (/ (summed-value (target-concept index-set) unseen-items 'identity)
                  (summed-value (target-concept index-set) predicted 'identity)))))
    (log:record-log  (target-concept index-set)
                     "Unseen raw score = ~5,3F (1 - unseen / predicted)"
                     score)
    score))
 
(defun remove-parts (l)
  "remove index concepts that form part of another index concept"
  (remove-if #'(lambda (item) (member item l :test 'part-of))
             l))

(defun predicted-items (seen-set predicted-set)
  (intersection predicted-set seen-set :test 'abst-or-whole-of))

(defun unpredicted-items (seen-set predicted-set)
  (set-difference seen-set predicted-set :test 'spec-or-part-of))

(defun unpredicted-items (seen-set predicted-set)
  (set-difference (remove-parts seen-set) predicted-set :test 'specp))

(defun unseen-items (seen-set predicted-set)
  (set-difference predicted-set seen-set :test 'abst-or-whole-of))


(defun summed-value (base predicted-set fn)
  (let ((val
         (loop for item in predicted-set
               sum (funcall fn (information-value item)))))
    (log:record-log base "Summed value of ~S~:[ using ~A~;~*~] => ~5,3F"
        predicted-set (eql fn 'identity) fn val)
    val))


;;------------------------------------------------------------------------------
;; This is an example of how to write an expection appraiser, although this 
;; isn't used by default.
;;------------------------------------------------------------------------------ 

(defvar *expectations* nil 
  "A list of target concepts predicted")

(defun add-expectation (target-concept)
  (pushnew target-concept *expectations*))

(defun clear-expectations ()
  (setf *expectations* nil))

(defun set-expectations (target-concepts)
  (setf *expectations* target-concepts))

(defun expected-p (target-concept)
  (and (member target-concept *expectations* ) t))

(defun expected-score (index-set found-indices)
  (declare (ignore found-indices))
  (let* ((target-concept (target-concept index-set))
         (found (expected-p target-concept))
         (score (if found 1 0)))
    (log:record-log  target-concept
                     "Expected raw score = ~5,3F (1 if expected, 0 otherwise)"
                     score)
    score))

;;------------------------------------------------------------------------------
;; Weight the appraisers
;;------------------------------------------------------------------------------

(clear-appraisers)

(assign-votes 'predicted-score 2)
(assign-votes 'unpredicted-score)
(assign-votes 'unseen-score)

;;------------------------------------------------------------------------------
;; Clearing memory
;;------------------------------------------------------------------------------

(defun clear-icp-memory ()
  (clear-frame-memory)
  (clear-predictions :all)
  (clear-table (target-concept-p))
  (clear-table (index->target-concepts))
  (clear-table (index->index-sets))
  (clear-table (index->target-concepts-cardinality))
  t)

;;------------------------------------------------------------------------------
;; A DMAP-based index concept recognizer
;;------------------------------------------------------------------------------

(defun words->indices (sent)
  (reset-parser)
  (let (concepts)
    (setf (call-backs 'm-root)
          (list #'(lambda (item start end) 
                    (record-log 'DMAP "DMAP referenced ~S from ~S" 
                                item (subseq sent (1- start) end))
                    (push item concepts))))
    (parse sent)
    (setf concepts (mapcar #'frames::name concepts))
    (record-log 'WORDS->INDICES 
                "~S ~%produced the index pool ~S~%~75,,,'=<~>~%" 
                sent concepts)
    concepts))

;;------------------------------------------------------------------------------
;; Logging  & testing functions
;;------------------------------------------------------------------------------

(defun first-n (sequence n)
  (loop for i from 1 to n
        for el in sequence 
        collect el))

(defun print-icp-log (&optional (n 7) (stream *standard-output*))
  (print-log 'WORDs->INDICES stream)
  (loop for result in (first-n *icp-results* n) 
        doing
        (print-log (target-concept result) stream)))

(defmacro ticp (&rest words)
  `(with-logging
     (icp ',words)
     (print-icp-log)))
     

