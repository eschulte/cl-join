(defpackage #:join-exe
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :join
        :cl-launch
        :trivial-shell
        :cl-ppcre)
  (:export :main))
