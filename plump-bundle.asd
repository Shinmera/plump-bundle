#|
 This file is a part of Plump-Bundle
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem plump-bundle
  :name "Plump-Bundle"
  :version "0.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A binary storage format for Plump documents."
  :homepage "https://github.com/Shinmera/plump-bundle"
  :serial T
  :components ((:file "package")
               (:file "machinery")
               (:file "definitions")
               (:file "frontend"))
  :depends-on (:plump-dom
               :babel
               :fast-io
               :closer-mop))
