(defpackage day6
  (:use :cl :arrow-macros))

(in-package :day6)

(defvar packet-marker 4)
(defvar message-marker 14)

(defun test ()
  (assert (= 7 (run "mjqjpqmgbljsphdztnvjfqwrcgsmlb" packet-marker)))
  (assert (= 5 (run "bvwbjplbgvbhsrlpgdmjqwftvncz"   packet-marker)))
  (assert (= 6 (run "nppdvjthqldpwncqszvftbrmjlhg"   packet-marker))))

(defun run (signal marker)
  (loop for char across signal
        for i = 1 then (incf i)
        with temp = nil
        do (progn
             (setf temp (append temp (list char)))
             (when  (= (length temp) marker)
               (when (= (length (remove-duplicates temp :test #'eq)) marker)
                 (return i))
               (setf temp (cdr temp))))))

(unless (test)
  (let ((string (uiop:read-file-line #p"day6/input/day6.in")))
    (values (run string packet-marker)
            (run string message-marker))))
