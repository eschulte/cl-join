;;; join.lisp --- replacement for the `join' Unix core utility

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :join)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(block getopts
       (loop :for ,arg = (pop args) :while ,arg :do
          (cond
            ,@(mapcar (lambda (args)
                        (cond
                          ((and (first args) (second args))
                           `((or (scan ,(first args)  ,arg)
                                 (scan ,(second args) ,arg))
                             ,@(cddr args)))
                          ((first args)
                           `((scan ,(first args)  ,arg) ,@(cddr args)))
                          ((second args)
                           `((scan ,(second args)  ,arg) ,@(cddr args)))))
                      forms)
            (t (push ,arg args) (return-from getopts)))))))

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

(defun join (lists predicate &key (key #'car) (val #'cdr) (test #'eql) empty)
  "Combine elements of LISTS with matching KEYs.  Lists must be sorted.

Optional arguments:
KEY -------- returns the key by which lists should be joined
VAL -------- returns the value for each list (must return a list)
TEST ------- is used to determine equality.
EMPTY ------ may be set to a value used to replaces missing input fields."
  (declare (optimize (speed 3) (safety 0))
           (type function key val test)
           (type list lists))
  (let ((empties (mapcar (constantly empty) (car lists)))
        (indices (let (all)
                   (dotimes (n (length lists) (nreverse all)) (push n all))))
        results)
    (loop :while (some [#'not #'null] lists) :do
       (let* ((base (extremum (remove nil (mapcar [key #'car] lists))
                              predicate))
              (matching (mapcar (lambda (it)
                                  (and it (funcall test base it)))
                                (mapcar [key #'car] lists))))
         (flet ((accept (list)
                  (push (cons base (mapcan val (copy-tree list))) results)))
           (cond
             ((every #'identity matching)
              (accept (loop :for i :in indices :collect (pop (nth i lists)))))
             (empty
              (accept (loop :for i :in indices :as matched :in matching
                         :collect (if matched (pop (nth i lists)) empties))))
             (t (loop :for i :in indices :as matched :in matching
                   :collect (when matched (pop (nth i lists)))))))))
    (nreverse results)))

(defun main (args)
  (let* ((help "Usage: ~a [OPTIONS...] FILES...

For each pair of input lines with identical join fields, write a
line to standard output.  The default join field is the first,
delimited by whitespace.  (TODO: When a single FILE is -, read
standard input.)

Options:
 -n,--numbers ------- keys sorted according to numerical values
 -e,--empty EMPTY --- replace missing input fields with EMPTY
                      includes unpaired lines
 -i,--ignore-case --- ignore case when comparing fields
 -j,--join FIELD ---- join on this FIELD of each file
 -v,--value FIELD --- only include FIELD of each file in output
 -t,--sep REGEX ----- use REGEX as input field separator
 -o,--output CHAR --- use CHAR as output field separator
 TODO: -N,--fieldN FIELD -- join on this FIELD of file N
 --header ----------- treat the first line in each file as field
                      headers, print them without pairing them~%")
         (self (pop args)))
    (when (or (not args) (< (length args) 2)
              (and (>= (length (car args)) 2)
                   (string= (subseq (car args) 0 2) "-h"))
              (and (>= (length (car args)) 3)
                   (string= (subseq (car args) 0 3) "--h")))
      (format t help self) (quit))

    (let ((sep "[\t \r\n]")
          (o-sep #\Tab)
          (key 0)
          num empty ignore-case val headers raw)
      (getopts
       ("-n" "--numbers"     (setf num t))
       ("-e" "--empty"       (setf empty (pop args)))
       ("-i" "--ignore-case" (setf ignore-case t))
       ("-v" "--value"       (setf val (parse-number (pop args))))
       ("-j" "--join"        (setf key (parse-number (pop args))))
       ("-t" "--sep"         (setf sep (pop args)))
       ("-o" "--output"      (setf o-sep (pop args)))
       (nil "--header"       (setf headers t)))

      (lists-to-stream
       (join (mapcar {file-to-lists _ sep} args)
             (if num #'< #'string<)
             :empty empty
             :test
             (if num #'= (if ignore-case #'string-equal #'string=))
             :key (if num
                      (lambda (list) (parse-number (nth key list)))
                      (lambda (list) (nth key list)))
             :val (if val
                      (lambda (list) (list (nth val list)))
                      (lambda (list)
                        (loop :for el :in list :as index :from 0
                           :unless (= index key) :collect el))))
       t o-sep))))
