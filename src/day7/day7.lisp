(defpackage day7
  (:use :cl :arrow-macros))

(in-package :day7)

;;;;;;file tree
(defclass dir-tree ()
  ((dirs   :accessor dirs   :initform '())
   (files  :accessor files  :initform '())
   (name   :accessor name   :initarg  :name)
   (size   :accessor size   :initform 0 :type integer)
   (parent :accessor parent :initarg  :parent :initform nil)))

(defclass file ()
  ((name :accessor name :initarg :name)
   (size :accessor size :initarg :size :type integer)))

(defgeneric add-to-tree (tree new &optional chain))

(defmethod add-to-tree ((tree dir-tree) (dir dir-tree) &optional (chain t))
  (setf (dirs tree) (append (list dir) (dirs tree)))
  (if chain tree dir))

(defmethod add-to-tree ((dir dir-tree) (file file) &optional (chain t))
  (update-size dir (size file))
  (setf (files dir) (append (list file) (files dir)))
  (if chain dir file))

(defmethod update-size ((dir dir-tree) amount)
  ;; (format t "tree ~s" dir)
  (incf (size dir) amount)
  (when (parent dir) (update-size (parent dir) amount)))

(defmethod find-file ((dir dir-tree) name)
  (car (remove-if-not #'(lambda (file)
                      (string= (name file) name))
                    (files dir))))

(defmethod find-dir ((dir dir-tree) name)
  (car (remove-if-not #'(lambda (dir)
                         (string= (name dir) name))
                       (dirs dir))))

(defmethod go-to-root ((dir dir-tree))
  (if (null (parent dir))
      dir
      (go-to-root (parent dir))))

(defmethod to-list ((tree dir-tree))
  (alexandria:flatten (append (list tree) (mapcar #'to-list (dirs tree)))))

(defun print-tree (tree &optional (level 0))
  (format t "~&~A- ~A" (make-string level :initial-element #\ ) (name tree))
  (mapcar #'(lambda (tree)
              (print-tree tree (+ 3 level)))
          (dirs tree))
  (mapcar #'(lambda (file)
              (format t "~&~A- ~A (~A)" (make-string (+ 3 level) :initial-element #\ )
                      (name file) (size file)))
            (files tree))
  tree)

(defmethod print-object ((tree dir-tree) out)
  (with-slots (name size) tree
    (print-unreadable-object (tree out :type nil)
      (format out "~A : ~A" name size))))

;;;;;; parse input
(defun read-tree (file)
  (go-to-root (reduce #'parse (mapcar #'str:words (uiop:read-file-lines file))
                        :initial-value (make-instance 'dir-tree :name "/"))))

(defun parse (acc line)
  (labels ((add-dir (tree name)
             (add-to-tree tree (make-instance 'dir-tree :name name :parent tree)))
           (add-file (tree name size)
             (add-to-tree tree (make-instance 'file :name name :size (parse-integer size))))
           (navigate (tree where)
             (cond ((string= where "..") (parent tree))
                   ((string= where "/")  (go-to-root tree))
                   (t (find-dir tree where)))))
    (cond ((string= (second line) "cd") (navigate acc (third line)))  ;; navidate down to new dir or up to parent
          ((string= (second line) "ls") acc)                          ;; discard this line
          ((string= (first line)  "dir") (add-dir acc (second line)))
          ((str:numeric? (first line)) (add-file acc (second line) (first line))))))

;;;;;; test tree
(defvar total-size 70000000)
(defvar needed 30000000)

(defvar input (mapcar #'str:words (uiop:read-file-lines #p"day7/input/teste.in")))

(defvar test-tree (read-tree #p"day7/input/teste.in"))
(defvar day7-tree (read-tree #p"day7/input/day7.in"))

;;;;;; run part 1
;;;;;; pelo amor de deus quando metodo desnecessario

(defmethod smaller-p ((dirA dir-tree) (dirB dir-tree))
  (> (size dirA) (size dirB)))

(defmethod bigger-p ((dirA dir-tree) (dirB dir-tree))
  (< (size dirA) (size dirB)))

(defmethod smaller-p ((dir dir-tree) (size integer))
  (> (size dir) size))

(defmethod bigger-p ((dirA dir-tree) (size integer))
  (< (size dirA) size))

(defmethod get-smaller-dirs ((root dir-tree) size)
  (remove-if #'(lambda (dir) (smaller-p dir size))
               (sort (to-list root) #'smaller-p)))

(defmethod get-bigger-dirs ((root dir-tree) size)
  (remove-if #'(lambda (dir) (bigger-p dir size))
               (sort (to-list root) #'bigger-p)))

(defun run-part-1 (tree)
  (reduce #'+ (mapcar #'size (get-smaller-dirs tree 100000))))

(run-part-1 test-tree) ;;;=> (#<e : 584> #<a : 94853>) => 95437
(run-part-1 day7-tree) ;;;=> 1141028

;;;;;; run part 2


(defun find-minimum-dir (root)
  (let* ((free (- total-size (size root)))
         (to-clear (- needed free)))
    (car (get-bigger-dirs root to-clear))))

(find-minimum-dir test-tree) ;;; => #<d : 24933642>
(find-minimum-dir day7-tree) ;;; => #<nzmddp : 8278005>
