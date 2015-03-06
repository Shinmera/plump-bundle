#|
 This file is a part of Plump-Bundle
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.bundle)

(defgeneric input (input)
  (:method ((input fast-io::input-buffer))
    (let ((*buffer* input)
          (*parent*))
      (read! file)
      *parent*))
  (:method ((input stream))
    (assert (subtypep (stream-element-type input) '(unsigned-byte 8))
            () "Stream element-type must be (UNSIGNED-BYTE 8)")
    (fast-io:with-fast-input (buffer NIL input)
      (input buffer)))
  (:method ((input pathname))
    (with-open-file (stream input :direction :input :element-type '(unsigned-byte 8))
      (input stream)))
  (:method ((input string))
    (input (parse-namestring input)))
  (:method ((input vector))
    (check-type input (vector (unsigned-byte 8)))
    (fast-io:with-fast-input (buffer input)
      (input buffer)))
  (:documentation "Parse INPUT to a Plump document.
Methods are defined for 
  FAST-IO::INPUT-BUFFER
  STREAM (unsigned-byte 8) element-type
  PATHNAME
  STRING treated as namestring
  VECTOR (unsigned-byte 8) element-type"))

(defgeneric output (target root)
  (:method ((output fast-io::output-buffer) root)
    (let ((*buffer* output)
          (*parent* root))
      (write! file)
      output))
  (:method ((output stream) root)
    (assert (subtypep (stream-element-type output) '(unsigned-byte 8))
            () "Stream element-type must be (UNSIGNED-BYTE 8)")
    (fast-io:with-fast-output (buffer output)
      (output buffer root)))
  (:method ((output pathname) root)
    (with-open-file (stream output :direction :output :element-type '(unsigned-byte 8))
      (output output root)
      output))
  (:method ((output string) root)
    (output (parse-namestring output) root))
  (:method ((output (eql :vector)) root)
    (fast-io:with-fast-output (buffer :vector)
      (output buffer root)))
  (:documentation "Turn ROOT into a bundle and store it in TARGET.
Methods are definted for
  FAST-IO::OUTPUT-BUFFER
  STREAM (unsigned-byte 8) element-type
  PATHNAME
  STRING treated as namestring
  :VECTOR"))

(defun output-to-file (pathname root &key (if-exists :error) (if-does-not-exist :create))
  "A wrapper around OUTPUT to allow specifying of IF-EXISTS and IF-DOES-NOT-EXIST."
  (with-open-file (stream pathname :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (output stream root)))
