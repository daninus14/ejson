(defsystem #:ejson-tests
  :version "0.0.0"
  :description "Tests for the ejson library"
  :author "Daniel Nussenbaum"
  :license "MIT"
  :components
  ((:file "ejson-tests"))
  :perform
  (test-op (o c) (symbol-call '#:ejson-tests '#:run))
  :depends-on
  (#:alexandria
   #:fiveam
   #:flexi-streams
   (:feature (:not :ecl) #:float-features)
   #:ejson
   #:uiop))
