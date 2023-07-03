(in-package #:cl)
(defpackage #:plump-bundle
  (:nicknames #:org.shirakumo.plump.bundle)
  (:use #:cl #:plump-dom)
  ;; definitions.lisp
  (:export
   #:*format-version*)
  ;; frontend.lisp
  (:export
   #:input
   #:output
   #:output-to-file)
  ;; machinery.lisp
  (:shadow
   #:write-string))

(defpackage #:plump-bundle-writers
  (:use))

(defpackage #:plump-bundle-readers
  (:use))
