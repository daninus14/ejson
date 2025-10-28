(defpackage ejson
  (:use #:cl)
  (:export
   ;;; Settings
   #:*serialize-lisp-case-to-camel-case*
   #:*deserialize-camel-case-to-lisp-case*
   
   ;;; Read
   #:parse   

   ;;;; Streaming reader
   #:parser
   #:make-parser
   #:parse-next
   #:parse-next-element
   #:close-parser
   #:with-parser

   ;; Reading utils
   #:span

   ;;; Write
   #:stringify

   ;;; Types
   #:json-atom
   #:json-element

   ;;; Conditions
   #:json-error
   #:json-limit-error
   #:json-parse-error
   #:json-parse-limit-error
   #:json-eof-error
   #:json-write-error
   #:json-write-limit-error
   #:json-recursive-write-error

   ;;; Simple extensible writing
   #:coerced-fields
   #:coerce-key

   ;;; Streaming Writer
   #:writer
   #:make-writer
   #:close-writer
   #:with-writer

   ;; Extensible serialization
   #:write-value

   #:begin-array
   #:write-values
   #:end-array
   #:with-array
   #:write-array

   ;; Writer operations
   #:begin-object
   #:write-key
   #:write-properties
   #:end-object
   #:with-object
   #:write-object

   ;; Dynavar interface to the writer
   #:*writer*
   #:with-writer*
   #:write-value*
   #:begin-array*
   #:write-values*
   #:end-array*
   #:with-array*
   #:write-array*
   #:begin-object*
   #:write-key*
   #:write-property*
   #:write-properties*
   #:end-object*
   #:with-object*
   #:write-object*)
  (:import-from #:closer-mop)
  (:import-from #:flexi-streams)
  (:import-from #:float-features)
  (:import-from #:uiop))
