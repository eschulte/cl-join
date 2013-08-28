(defsystem :join
  :description "An improved version of the join Unix coreutil."
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               cl-ppcre)
  :serial t
  :components
  ((:file "package")
   (:file "join")))
