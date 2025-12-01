(defpackage :aoc2025-01
  (:use :cl :aoc2025-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2025-01)

(defvar *lines* (input-file-lines 1))

(defmacro do-rotations ((direction n lines &optional result) &body body)
  (let ((line (gensym "LINE-")))
    `(dolist (,line ,lines ,result)
       (let ((,direction (char ,line 0))
             (,n (parse-integer (subseq ,line 1))))
         ,@body))))

(defun solve-1 ()
  (let ((dial 50)
        (count 0))
    (do-rotations (direction n *lines* count)
      (setf dial (mod (ecase direction
                        (#\L (- dial n))
                        (#\R (+ dial n)))
                      100))
      (when (zerop dial)
        (incf count)))))

(defun solve-2 ()
  (let ((dial 50)
        (count 0))
    (do-rotations (direction n *lines* count)
      (ecase direction
        (#\L
         (dotimes (i n)
           (when (zerop (setf dial (mod (1+ dial) 100)))
             (incf count))))
        (#\R
         (dotimes (i n)
           (when (zerop (setf dial (mod (1- dial) 100)))
             (incf count))))))))
