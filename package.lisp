#|
 This file is a part of Plump-Bundle
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
