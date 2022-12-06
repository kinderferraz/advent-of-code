(defpackage day4
  (:use :cl :arrow-macros))

(in-package :day4)

(defclass range ()
  ((start :accessor start :initarg :start)
   (end :accessor end :initarg :end)))

;;"returns true if range1 fully contains range2"
(defgeneric contains (range1 range2))

(defmethod contains ((range1 range) (range2 range))
  (with-accessors ((start1 start) (end1 end)) range1
    (with-accessors ((start2 start) (end2 end)) range2
      (and (>= start2 start1)
           (<= end2 end1)))))

(defmethod print-object ((range range) out)
  (with-slots (start end) range
    (print-unreadable-object (range out)
      (format out "(~A|~A)" start end))))

;; returns true if there's any overlap
(defmethod overlaps ((range1 range) (range2 range))
  (with-accessors ((s1 start) (e1 end)) range1
    (with-accessors ((s2 start) (e2 end)) range2
      (and (<= s1 e2)
           (>= e1 s2)))))

(defun str->range (str)
  (let ((range (uiop:split-string str :separator '(#\-))))
    (make-instance 'range :start (parse-integer (car range))
                          :end (parse-integer (cadr range)))))

(defun flat-map (function list)
  (mapcar #'(lambda (l)
              (mapcar function))
            list))

(defun run (file)
  (->> (uiop:read-file-lines file)
    (mapcar #'(lambda (str) (uiop:split-string str :separator '(#\,))))
    (flat-map #'str->range)
    (mapcar #'(lambda (ranges)
                (or (contains (car ranges) (cadr ranges))
                    (contains (cadr ranges) (car ranges)))))
    (remove-if #'null)
    (length)))

(defun run-overlaps (file)
  (->> (uiop:read-file-lines file)
    (mapcar #'(lambda (str) (uiop:split-string str :separator '(#\,))))
    (flat-map #'str->range)
    (mapcar #'(lambda (ranges)
                (overlaps (car ranges) (cadr ranges))))
    (remove-if #'null)
    (length)))
