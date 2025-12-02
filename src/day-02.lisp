(defpackage :aoc2025-02
  (:use :cl :aoc2025-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2025-02)

(defvar *string* (input-file-data 2))

(defun parse-range (string)
  (let ((hyphen (position #\- string)))
    (cons (parse-integer string :end hyphen)
          (parse-integer string :start (1+ hyphen)))))

(defun parse-ranges (string)
  (declare (type string string))
  (mapcar #'parse-range (core:split-string string #\,)))

(defmacro dorange ((n range &optional result) &body body)
  (let ((range-var (gensym "RANGE-VAR-")))
    `(let ((,range-var ,range))
       (do ((,n (car ,range-var) (1+ ,n)))
           ((> ,n (cdr ,range-var))
            ,result)
         ,@body))))

(defun simple-invalid-id-p (id)
  (declare (type (integer 0) id))
  (let* ((string (write-to-string id))
         (string-length (length string)))
    (and (zerop (rem string-length 2))
         (let ((half (floor string-length 2)))
           (string= string string :end1 half :start2 half)))))

(defun invalid-id-p (id)
  (declare (type (integer 0) id))
  (let* ((string (write-to-string id))
         (string-length (length string)))
    (flet ((contains-repeated-group (group-length)
             (declare (type (integer 1) group-length))
             (and (zerop (rem string-length group-length))
                  (do ((i 1 (1+ i)))
                      ((>= i (floor string-length group-length))
                       t)
                    (let ((idx (* i group-length)))
                      (unless (string= string string
                                       :start1 0 :end1 group-length
                                       :start2 idx :end2 (+ idx group-length))
                        (return nil)))))))
      (do ((group-length 1 (1+ group-length)))
          ((> group-length (floor string-length 2)))
        (let ((found (contains-repeated-group group-length)))
          (when found
            (return-from invalid-id-p t)))))))

(defun solve-1 ()
  (let ((ranges (parse-ranges *string*))
        (sum 0))
    (dolist (range ranges sum)
      (dorange (id range)
        (when (simple-invalid-id-p id)
          (incf sum id))))))

(defun solve-2 ()
  (let ((ranges (parse-ranges *string*))
        (sum 0))
    (dolist (range ranges sum)
      (dorange (id range)
        (when (invalid-id-p id)
          (incf sum id))))))
