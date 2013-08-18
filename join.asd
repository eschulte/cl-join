(defsystem :join
  :description "join sequences on similar elements"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria curry-compose-reader-macros)
  :components
  ((:file "package")
   (:file "join" :depends-on ("package"))))
