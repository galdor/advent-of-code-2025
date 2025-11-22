(defpackage :aoc2025-utils
  (:use :cl)
  (:export
   #:*year*
   #:*firefox-profile-directory*
   #:*http-user-agent*
   #:firefox-cookie
   #:default-firefox-profile
   #:download-input-file
   #:input-file-path
   #:input-file-data
   #:input-file-lines
   #:parse-lines
   #:format-solution-code
   #:solution-file-path
   #:generate-solution-file
   #:open-problem-page))

(in-package :aoc2025-utils)

(defparameter *year* 2025
  "The year of the Advent of Code event.")

(defparameter *firefox-profile-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative ".mozilla" "firefox"))
   (user-homedir-pathname))
  "The path of the directory containing Firefox profiles.")

(defparameter *http-user-agent*
  (format nil "https://github.com/galdor/advent-of-code-~D" *year*)
  "The user agent used for requests sent to the Advent of Code website.")

(defmacro with-temporary-file-copy ((copy-path path) &body body)
  "Evaluate BODY with COPY-PATH bound to the path of a temporary file created as
a copy of the file at PATH. The temporary file is always deleted after BODY
has been evaluated."
  (let ((input (gensym "INPUT-"))
        (output (gensym "OUTPUT-"))
        (data (gensym "DATA-")))
    `(let ((,copy-path (make-pathname :name (pathname-name ,path) :type "tmp"
                                      :defaults ,path)))
       (unwind-protect
            (progn
              (with-open-file (,input ,path :element-type '(unsigned-byte 8))
                (with-open-file
                    (,output ,copy-path :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create
                                        :element-type '(unsigned-byte 8))
                  (let ((,data (make-array
                                (file-length ,input)
                                :element-type (stream-element-type ,input))))
                    (read-sequence ,data ,input)
                    (write-sequence ,data ,output))))
              ,@body)
         (delete-file ,copy-path)))))

(defun firefox-cookie ()
  "Return the value of the cookie for the Advent of Code website stored in the
default Firefox profile.

Because Firefox stupidly locks its SQLite databases, we have to copy the
entire database file before reading it. We cannot even use the sqlite .dump
command, because the file is locked. Fingers crossed."
  (let* ((profile (default-firefox-profile))
         (cookie-db-path
           (merge-pathnames (make-pathname :directory (list :relative profile)
                                           :name "cookies" :type "sqlite")
                            *firefox-profile-directory*)))
    (with-temporary-file-copy (tmp-path cookie-db-path)
      (let* ((domain ".adventofcode.com")
             (query (format nil "SELECT value FROM moz_cookies WHERE host='~A'"
                            domain))
             (arguments (list "-readonly" "-batch" "-list"
                              (namestring tmp-path) query)))
        (multiple-value-bind (result code output error-output)
            (programs:run-program "sqlite3" arguments
                                  :standard-output t
                                  :error-output *error-output*
                                  :error t)
          (declare (ignore result code error-output))
          ;; The first line is the header line
          (let ((second-line-start (position #\Newline output)))
            (unless (and second-line-start
                         (< second-line-start (1- (length output))))
              (error "cannot parse sqlite output ~S" output))
            (let ((end (position #\Newline output
                                 :start (1+ second-line-start))))
              (subseq output (1+ second-line-start) end))))))))

(defun default-firefox-profile ()
  "Return the name of the default Firefox profile."
  (let ((profiles-ini-path
          (merge-pathnames (make-pathname :name "profiles" :type "ini")
                           *firefox-profile-directory*))
        (prefix "Default="))
    (handler-case
        (with-open-file (file profiles-ini-path :external-format :utf-8)
          (loop
            (let ((line (read-line file)))
              (when (and (> (length line) (length prefix))
                         (string= line prefix :end1 (length prefix)))
                (return-from default-firefox-profile
                  (subseq line (length prefix)))))))
      (end-of-file (c)
        (declare (ignore c))
        (error "default profile name not found in ~A" profiles-ini-path)))))

(defun download-input-file (day)
  "Download an input file and store it in the input directory. Return the
absolute path of the file."
  (let* ((uri (format nil "https://adventofcode.com/~D/day/~D/input"
                      *year* day))
         (output-path (input-file-path day))
         (cookie (firefox-cookie))
         (session (concatenate 'string "Cookie: session=" cookie))
         (user-agent (concatenate 'string "User-Agent: " *http-user-agent*))
         (arguments `("--silent"
                      "--show-error"
                      "--fail"
                      "--location"
                      "--header" ,session
                      "--header" ,user-agent
                      "--output" ,(namestring output-path)
                      ,uri)))
    (programs:run-program "curl" arguments
                          :error-output *error-output*
                          :error t)
    output-path))

(defun input-file-path (day)
  "Return the absolute path of an input file in the repository."
  (let ((system (format nil "aoc~D" *year*))
        (subpath (make-pathname :directory '(:relative "data")
                                :name (format nil "day-~2,'0D" day)
                                :type "txt")))
    (sysdef:system-file-path system subpath)))

(defun input-file-data (day)
  "Return the content of an input file as a string, downloading the file if it
does not exist locally."
  (let ((path (input-file-path day)))
    (unless (probe-file path)
      (download-input-file day))
    (system:read-file path :external-format text:*default-encoding*)))

(defun input-file-lines (day)
  "Return the content of an input file as a list of lines, downloading the file
if it does not exist locally."
  (parse-lines (input-file-data day)))

(defun parse-lines (string)
  "Parse a list of lines."
  (declare (type string string))
  (do* ((start 0)
        (end (length string))
        (lines nil))
       ((>= start end)
        (nreverse lines))
    (let ((eol (or (position #\Newline string :start start :end end) end)))
      (push (subseq string start eol) lines)
      (setf start (1+ eol)))))

(defun format-solution-code (day &key stream)
  "Generate the initial code for a solution to a daily problem and write it to a
stream."
  (flet ((form (control &rest args)
           (apply #'format stream control args)
           (terpri stream)))
    (form "~
(defpackage :aoc~D-~2,'0D
  (:use :cl :aoc~D-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))"
          *year* day *year*)
    (form "~%(in-package :aoc~D-~2,'0D)" *year* day)
    (form "~%(defvar *lines* (input-file-lines ~D))" day)
    (form "~%~
(defun solve-1 ()
  ;; TODO
  nil)")
    (form "~%~
(defun solve-2 ()
  ;; TODO
  nil)")))

(defun solution-file-path (day)
  "Return the absolute path of a solution file in the repository."
  (let ((system (format nil "aoc~D" *year*))
        (subpath (make-pathname :directory '(:relative "src")
                                :name (format nil "day-~2,'0D" day)
                                :type "lisp")))
    (sysdef:system-file-path system subpath)))

(defun generate-solution-file (day)
  "Generate the code for the solution file and write it. Return the path of the
file."
  (let ((path (solution-file-path day)))
    (with-open-file (file path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format-solution-code day :stream file))
    path))

(defun open-problem-page (day)
  "Open the web page for a specific daily problem in Firefox."
  (let* ((uri (format nil "https://adventofcode.com/~D/day/~D" *year* day)))
    (programs:run-program "firefox" (list uri)
                          :error-output *error-output*
                          :error t)
    nil))
