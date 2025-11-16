(defsystem #:ejson-tests
  :version "1.0.0"
  :description "Tests for the ejson library"
  :author "Daniel Nussenbaum"
  :license "MIT"
  :components ((:module "test" :components ((:file "jzon-tests"))))
  :perform
  (test-op (o c) (symbol-call '#:com.inuoe.jzon-tests '#:run))
  :depends-on
  (#:alexandria
   #:fiveam
   #:flexi-streams
   (:feature (:not :ecl) #:float-features)
   #:com.inuoe.jzon
   #:uiop))
