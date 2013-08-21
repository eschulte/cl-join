(defsystem :join
  :description "join sequences on similar elements"
  :version "0.0.1"
  :licence "GPL V3"
  :components
  ((:file "package")
   (:file "join" :depends-on ("package"))))
