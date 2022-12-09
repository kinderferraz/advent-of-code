(defpackage day3
  (:use :cl :arrow-macros))

(in-package :day3)

(defun run (file)
  (->> (uiop:read-file-lines file)
    (split-sacks)
    (find-repeated)
    (flat-map #'char->point)
    (reduce #'+)))

(defun run-badges (file)
  (->> (uiop:read-file-lines file)
    (group-by-3 nil)
    (mapcar #'(lambda (group) (mapcar #'str->set group)))
    (find-repeated)
    (flat-map #'char->point)
    (reduce #'+)))

(defun split-sacks (sacks)
  (mapcar #'(lambda (sack)
              (let* ((half-point (/ (length sack) 2))
                     (first-comp (subseq sack 0 half-point))
                     (second-comp (subseq sack half-point)))
                (list (str->set first-comp) (str->set second-comp))))
          sacks))

(defun find-repeated (sacks)
  (mapcar #'(lambda (sack)
              (-> sack
                (->> (reduce #'intersection))
                (remove-duplicates :test #'char-equal)))
            sacks))

(defun group-by-3 (acc lists)
  (cond ((null lists) acc)
        (t (let ((first (car lists))
                 (second (cadr lists))
                 (third (caddr lists))
                 (rest (cdddr lists)))
             (group-by-3 (cons (list first second third) acc) rest)))))

(defun majuscule (int)
  (and (>= int 65)
       (<= int 90)))

(defun miniscule (int)
  (and (>= int 97)
       (<= int 122)))

(defun str->set (str)
  (loop for char across str
        collect char))

(defun char->point (char)
  (let ((int (char-int char)))
    (cond ((majuscule int) (+ 26 (- int 64)))
          ((miniscule int) (- int 96)))))

(defun flat-map (function list)
  (mapcar #'(lambda (char-list)
              (reduce #'append (mapcar function char-list)))
            list))

;;(run #p"/home/kinder/Documents/lisps/advent-of-code/src/day3/input/teste.in")
;;(run #p"/home/kinder/Documents/lisps/advent-of-code/src/day3/input/day3.in")

