#|
 This file is a part of Plump-Bundle
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.bundle)

(defgeneric input (input &optional root)
  (:method ((input flexi-streams:flexi-input-stream) &optional (root (make-root)))
    (let ((*stream* input)
          (*root* root))
      (read! file)
      *root*))
  (:method ((input stream) &optional (root (make-root)))
    (assert (equal (stream-element-type input) '(unsigned-byte 8))
            () "Stream element-type must be (UNSIGNED-BYTE 8)")
    (input (flexi-streams:make-flexi-stream input :external-format :utf-8) root))
  (:method ((input pathname) &optional (root (make-root)))
    (with-open-file (stream input :direction :input :element-type '(unsigned-byte 8))
      (input stream root)))
  (:method ((input string) &optional (root (make-root)))
    (input (parse-namestring input) root))
  (:method ((input vector) &optional (root (make-root)))
    (check-type input (vector (unsigned-byte 8)))
    (input (flexi-streams:make-in-memory-input-stream input) root)))

(defgeneric output (target root)
  (:method ((output flexi-streams:flexi-output-stream) root)
    (let ((*stream* output)
          (*root* root)
          (*parent* root))
      (write! file)
      output))
  (:method ((output stream) root)
    (assert (equal (stream-element-type output) '(unsigned-byte 8))
            () "Stream element-type must be (UNSIGNED-BYTE 8)")
    (output (flexi-streams:make-flexi-stream output :external-format :utf-8) root))
  (:method ((output pathname) root)
    (with-open-file (stream output :direction :output :element-type '(unsigned-byte 8))
      (output output root)
      output))
  (:method ((output string) root)
    (output (parse-namestring output) root))
  (:method ((output (eql :vector)) root)
    (let ((stream (flexi-streams:make-in-memory-output-stream)))
      (output stream root)
      (flexi-streams:get-output-stream-sequence stream))))

(defun output-to-file (pathname root &key (if-exists :error) (if-does-not-exist :create))
  (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (output stream root)))