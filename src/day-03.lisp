(defpackage :aoc2025-03
  (:use :cl :aoc2025-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2025-03)

(defvar *lines* (input-file-lines 3))

(defun parse-bank (string)
  (declare (type string string))
  (let* ((nb-batteries (length string))
         (batteries (make-array (list nb-batteries) :element-type 'integer)))
    (dotimes (i nb-batteries batteries)
      (setf (aref batteries i) (parse-integer string :start i :end (1+ i))))))

(defmacro dovector ((i value vector &key start end) &body body)
  (let ((vector-var (gensym "VECTOR-VAR-"))
        (end-var (gensym "END-VAR-")))
    `(do* ((,vector-var ,vector)
           (,end-var ,end)
           (,i ,start (1+ ,i)))
          ((>= ,i ,end-var))
       (let ((,value (aref ,vector-var ,i)))
         ,@body))))

(defun highest-battery-joltage (bank &key (start 0) end)
  (declare (type (vector integer) bank)
           (type (integer 0) start))
  (let (max-joltage)
    (dovector (i joltage bank :start start :end end)
      (setf max-joltage (max (or max-joltage 0) joltage)))
    max-joltage))

(defun highest-battery-group-joltage (bank group-size)
  (declare (type (vector integer) bank)
           (type (integer 1) group-size))
  (let ((max-joltage 0))
    (labels ((aux (total-joltage n i)
               (declare (type (integer 0) total-joltage n i))
               (cond
                 ((zerop n)
                  (setf max-joltage (max max-joltage total-joltage)))
                 (t
                  (let* ((end (1+ (- (length bank) n)))
                         (next-max-joltage
                           (highest-battery-joltage bank :start i :end end)))
                    (when next-max-joltage
                      (dovector (j joltage bank :start i :end end)
                        (when (= joltage next-max-joltage)
                          (aux (+ (* total-joltage 10) joltage)
                               (1- n) (1+ j))))))))))
      (aux 0 group-size 0))
    max-joltage))

(defun solve-1 ()
  (let ((banks (mapcar #'parse-bank *lines*))
        (sum 0))
    (dolist (bank banks sum)
      (incf sum (highest-battery-group-joltage bank 2)))))

(defun solve-2 ()
  (let ((banks (mapcar #'parse-bank *lines*))
        (sum 0))
    (dolist (bank banks sum)
      (incf sum (highest-battery-group-joltage bank 12)))))
