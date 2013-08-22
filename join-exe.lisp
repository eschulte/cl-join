;;; join-exe.lisp --- replacement for the `join' Unix core utility

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :join-exe)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (string= ,arg ,short) (string= ,arg ,long)) ,@body))
                    forms)))))

(defun quit (&optional (errno 0))
  #+sbcl (sb-ext:exit :code errno)
  #+ccl  (ccl:quit errno))

(defun parse-number (string)
  (when string (read-from-string string)))

(defun unsafe-open (native)
  "An unsafe open which doesn't check existence for /proc/*/fd/* files."
  #+sbcl
  (sb-impl::make-fd-stream (sb-unix:unix-open native sb-unix:o_rdonly #o666))
  #+ccl
  (ccl::make-fd-stream (ccl::fd-open native #$O_RDONLY)))

(defun file-to-lists (file regex)
  (mapcar {split regex}
          (let ((in (unsafe-open file)))
            (prog1 (loop :for line = (read-line in nil nil) :while line
                      :collect line)
              (close in)))))

(defun lists-to-stream (lines stream separator)
  (format stream (format nil "~~{~~{~~a~~^~a~~}~~^~~%~~}~~%" separator) lines))

(defun main (args)
  (let* ((help "Usage: ~a FILE1 FILE2 [OPTIONS...]
For each pair of input lines with identical join fields, write a line
to standard output.  The default join field is the first, delimited by
whitespace.  When FILE1 or FILE2 (not both) is -, read standard input.

Options:
 -n,--numbers ------- keys sorted according to numerical values
 -e,--empty EMPTY --- replace missing input fields with EMPTY
                      this fills unpaired lines as well like `join -a'
 -i,--ignore-case --- ignore case when comparing fields
 -j,--join FIELD ---- equivalent to -1 FIELD -2 FIELD
 -t,--sep REGEX ----- use REGEX as input field separator
 -o,--output CHAR --- use CHAR as output field separator
 -1,--field1 FIELD -- join on this FIELD of file 1
 -2,--field2 FIELD -- join on this FIELD of file 2
 --header ----------- treat the first line in each file as field head-
                      ers, print them without trying to pair them
 -r,--raw ----------- print as raw lisp~%")
         (self (pop args)))
    (when (or (not args) (< (length args) 2)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t help self) (quit))

    (let ((file1 (pop args))
          (file2 (pop args))
          (sep "[\t \r\n]")
          (o-sep #\Tab)
          (key-1 0)
          (key-2 0)
          num empty ignore-case headers raw save)
      (getopts
       ("-s" "--save"        (setf save t)) ; for debugging
       ("-n" "--numbers"     (setf num t))
       ("-e" "--empty"       (setf empty (pop args)))
       ("-i" "--ignore-case" (setf ignore-case t))
       ("-j" "--join"        (let ((keys (parse-number (pop args))))
                               (setf key-1 keys key-2 keys)))
       ("-t" "--sep"         (setf sep (pop args)))
       ("-o" "--output"      (setf o-sep (pop args)))
       ("-1" "--field1"      (setf key-1 (parse-number (pop args))))
       ("-2" "--field2"      (setf key-2 (parse-number (pop args))))
       (nil "--header"       (setf headers t))
       ("-r" "--raw"         (setf raw t)))

      (let ((list1 (file-to-lists file1 sep))
            (list2 (file-to-lists file2 sep)))
        (flet ((keys-func (id)
                 (if num
                     (lambda (list) (parse-number (nth id list)))
                     (lambda (list) (nth id list))))
               (vals-func (id)
                 (lambda (list) (loop :for el :in list :as index :from 0
                              :unless (= index id) :collect el))))

          (let ((joined
                 (join
                  list1 list2 (if num #'< #'string<)
                  :empty empty
                  :test (if num #'= (if ignore-case #'string-equal #'string=))
                  :start (if headers 1 0)
                  :key-1 (keys-func key-1)
                  :key-2 (keys-func key-2)
                  :val-1 (vals-func key-1)
                  :val-2 (vals-func key-2))))
            (if save
                (format t "~&~S~%"
                        (list list1 list2))
                (if raw
                    (format t "~&~S~%" joined)
                    (lists-to-stream joined t o-sep)))))))))
