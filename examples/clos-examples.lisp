(ql:quickload :ejson)

(defclass c1 ()
  (a b))

(defclass c2 (c1)
  (c))
