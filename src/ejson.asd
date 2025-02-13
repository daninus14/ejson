(defsystem #:ejson
  :version "0.9.0"
  :description "Extensible,correct and safe JSON RFC 8259 reader/writer"
  :author "Daniel Nussenbaum github.com/daninus14"
  :license "MIT"
  :depends-on (#:closer-mop
               #:flexi-streams
               (:feature (:not :ecl) #:float-features)
               #:trivial-gray-streams
               #:uiop)
  ;; :in-order-to ((test-op (test-op "ejson-tests")))
  :components ((:file "eisel-lemire")
               (:file "ratio-to-double")
               (:file "schubfach")
               (:file "ejson" :depends-on ("eisel-lemire" "ratio-to-double" "schubfach"))))
