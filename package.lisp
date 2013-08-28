(defpackage #:join
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :cl-ppcre)
  (:export :join :main))
