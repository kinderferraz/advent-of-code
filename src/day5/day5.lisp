(defpackage day5
  (:use :cl :arrow-macros))

(in-package :day5)


;;; input parse
(defvar testfile  #p"day5/input/teste.in")
(defvar inputfile #p"day5/input/day5.in")


;;;;;; fix: funciona apenas se tiver espaços ate o final
;;;;;; da linha, formando um quadrado
(defun tokenize-stacks (row)
  (->> row
    (str:replace-all "["  "")
    (str:replace-all "]" "")
    (str:replace-all "    " " ")))

(defun split-input (rows)
  (list (get-stacks rows)
        (get-moves  rows)))

(defun get-stacks (rows)
  (reverse (butlast (loop for row in rows
                  while (not (string= row ""))
                  collect (str:split #\  (tokenize-stacks row))))))

(defun get-moves (rows)
  (cdr (member "" rows :test #'string=)))


;; (let* (
;;        (input (mapcar #'fill-stacks ))
;;        (crates (normalize (get-stacks input)))
;;        (moves (get-moves input)))
;;   (loop for move in moves
;;         do (operate move crates))
;;   input
;;   ;;(reduce #'str:concat (mapcar #'first crates))
;;   )

(defun run (file function)
  (destructuring-bind (stacks moves) (split-input (uiop:read-file-lines file))
    (->> stacks
      (apply #'mapcar #'list)
      (mapcar #'reverse)
      (mapcar #'(lambda (stack)
                  (remove "" stack :test #'string=)))
      (operate function moves)
      (mapcar #'first)
      (reduce #'str:concat))))

;;;;;;

(defun get-instructions (move)
  (mapcar #'parse-integer (remove-if-not #'str:numeric? (str:words move))))

(defun crane-master-9000 (move crates)
  (destructuring-bind (amount from to) (get-instructions move)
    (dotimes (i amount)
      (push (pop (nth (1- from) crates)) (nth (1- to) crates)))))

(defun crane-master-9001 (move crates)
  (let ((crane nil))
    (destructuring-bind (amount from to) (get-instructions move)
      (dotimes (i amount)
        (push (pop (nth (1- from) crates)) crane))
      (dotimes (i amount)
        (push (pop crane) (nth (1- to) crates))))))

(defun operate (function moves stacks)
  (loop for move in moves
        do (funcall function move stacks))
  stacks)
