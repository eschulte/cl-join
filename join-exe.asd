(defsystem :join-exe
  :description "An improved version of the join Unix coreutil."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               join
               cl-launch
               trivial-shell
               split-sequence)
  :components
  ((:file "package-exe")
   (:file "join-exe" :depends-on ("package-exe"))))