(defpackage day2
  (:use :cl :alexandria :arrow-macros))

(in-package :day2)

;;;;;; input
(defun read-game (file &optional decode-f)
  (let ((in (open file)))
    (progn
      (let ((game (read-all-matches in nil decode-f)))
          (close in)
        game))))

(defun read-all-matches (file acc &optional (decode-f #'moves))
  (let ((match (read-line file nil "EOF")))
    (cond ((string= match "EOF") (reverse acc))
          (t (read-all-matches file
                               (cons (mapcar decode-f (asdf::split-string match)) acc)
                               decode-f)))))

(defun moves (move)
  (cond ((or (string= move "A")
             (string= move "X"))
         :rock)
        ((or (string= move "B")
             (string= move "Y"))
         :paper)
        ((or (string= move "C")
             (string= move "Z"))
         :scissors)))

(defun moves-and-outcome (move)
  (cond ((string= move "A") :rock)
        ((string= move "B") :paper)
        ((string= move "C") :scissors)
        ((string= move "X") :lose)
        ((string= move "Y") :draw)
        ((string= move "Z") :win)))

;;;;;; moves

(defvar moves-win '((:scissors :rock)
                    (:rock     :paper)
                    (:paper    :scissors)))

(defvar moves-lose '((:scissors :paper)
                     (:paper    :rock)
                     (:rock     :scissors)))

;;;;;; points
(defun match-points (match)
  (let ((my-move (second match))
        (opponent-move (first match)))
    (+ (chosen-points my-move) (won my-move opponent-move))))

(defun match-points-2 (match)
  (let* ((opponent-move (first match))
         (outcome (second match))
         (my-move (figure-move opponent-move outcome)))
    (+ (chosen-points my-move) (won my-move opponent-move))))

(defun chosen-points (move)
  (cond ((eq move :rock)     1)
        ((eq move :paper)    2)
        ((eq move :scissors) 3)))

(defun won (my-move opponent-move)
  (cond ((eq    my-move opponent-move) 3)
        ((lose? my-move opponent-move) 0)
        ((won?  my-move opponent-move) 6)))

(defun lose? (my-move opponent-move)
  (cond ((and (eq my-move :rock)
              (eq opponent-move :paper))
         t)
        ((and (eq my-move :paper)
              (eq opponent-move :scissors))
         t)
        ((and (eq my-move :scissors)
              (eq opponent-move :rock))
         t)))

(defun won? (my-move opponent-move)
  (cond ((and (eq my-move :rock)
              (eq opponent-move :scissors)) t)
        ((and (eq my-move :paper)
              (eq opponent-move :rock)) t)
        ((and (eq my-move :scissors)
              (eq opponent-move :paper)) t)))

;;;;;; other strategy

(defun figure-move (my-move outcome)
  (cond ((eq outcome :draw) my-move)
        ((eq outcome :lose) (lose-move my-move))
        ((eq outcome :win)  (win-move my-move))))

(defun win-move (move)
  (cadr (assoc move moves-win)))

(defun lose-move (move)
  (cadr (assoc move moves-lose)))

;;;;;; run-first-part
(defun run-first-part (file)
  (->> (read-game file)
    (mapcar #'match-points)
    (reduce #'+)))

(run-first-part #p"day2/input/teste.in")
(run-first-part #p"day2/input/day2.in")

(defun run-second-part (file)
  (->> (read-game file #'moves-and-outcome)
    (mapcar #'match-points-2)
    (reduce #'+)))

(run-second-part #p"day2/input/teste.in")
(run-second-part #p"day2/input/day2.in")
