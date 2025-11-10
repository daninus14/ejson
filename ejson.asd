(defsystem #:ejson
  :version "0.9.0"
  :description "Extensible,correct and safe JSON RFC 8259 reader/writer"
  :author "Daniel Nussenbaum github.com/daninus14"
  :license "MIT"
  :depends-on (#:closer-mop
               #:flexi-streams
               #:cl-change-case
               (:feature (:not :ecl) #:float-features)
               #:trivial-gray-streams
               #:uiop)
  ;; :in-order-to ((test-op (test-op "ejson-tests")))
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "eisel-lemire")
                             (:file "ratio-to-double")
                             (:file "schubfach")
                             ;; (:file "camel-case" :depends-on ("packages"))
                             ;; (:file "case-from-cljson" :depends-on ("packages"))
                             (:file "ejson" :depends-on ("packages" "eisel-lemire" "ratio-to-double" "schubfach"))))))
