(defpackage #:join-exe
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :join
        :trivial-shell
        :cl-ppcre)
  (:export :main))
