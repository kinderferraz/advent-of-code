(defpackage challenge1
  (:use :cl :arrow-macros))

(in-package :challenge1)

(defun parse-group (group)
  (mapcar #'(lambda (sublist)
              (mapcar #'parse-integer sublist))
            group))

(defun get-max (grouped-list)
  ;;; '((1000 2000 3000) (4000) (5000 6000) (7000 8000 9000) (10000))
  (->> grouped-list
    (mapcar #'(lambda (elf-list)
                (reduce #'+ elf-list)))
    (reduce #'max)))

(defun read-and-group (filename)
  (let* ((acc (rec-read filename nil nil)))
    (close filename)
    acc))

(defun rec-read (file acc aux-acc)
  (let ((line (read-line file nil "EOF")))
    (cond ((string= line "EOF")
           (reverse (cons aux-acc acc)))
          ((string= line "")
           (progn (rec-read file (cons (reverse aux-acc) acc) nil)))
          (t (rec-read file acc (cons line aux-acc))))))

(defun sum-list (list)
  (reduce #'+ list))

(defun top3 (grouped-list)
  (-> grouped-list
    (->> (mapcar #'sum-list))
    (sort '>)
    (subseq 0 3)))

;;;;;; run

(let* ((stream (open #p"/home/kinder/Documents/lisps/advent-of-code/src/day1/input/day1.in"))
       (group (parse-group (read-and-group stream))))
  (get-max group)
  (sum-list (top3 group)))
