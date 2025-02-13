;;; The comments are usually output 
(ql:quickload :ejson)

(defclass c1 ()
  ((a :initarg :a :accessor a)
   (b :initarg :b :accessor b)))

(defclass c2 (c1)
  ((c :initarg :c :accessor c)))

(let ((o1 (make-instance 'c1 :a 1 :b 1))
      (o2 (make-instance 'c2 :a 2 :b 2 :c 2)))
  (describe o1)
  (describe o2)
  (format t "~%~A~%" (ejson:stringify o1))
  (format t "~%~A~%" (ejson:stringify o2)))

;; #<C1 {10094A7533}>
;; [standard-object]

;; Slots with :INSTANCE allocation:
;; A                              = 1
;; B                              = 1
;; #<C2 {1009E2C4C3}>
;; [standard-object]

;; Slots with :INSTANCE allocation:
;; A                              = 2
;; B                              = 2
;; C                              = 2

;; {"a":1,"b":1}

;; {"a":2,"b":2,"c":2}
;; NIL
