#|
 This file is a part of Plump-Bundle
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.bundle)
(defvar *format-version* 0)

(define-writer byte (byte)
  (check-type byte (unsigned-byte 8))
  (write-byte byte *stream*))

(define-reader byte ()
  (read-byte *stream*))

(define-writer integer (int)
  (check-type int (integer 0 4294967296))
  (loop for i downfrom (* 8 4) by 8 above 0
        do (write-byte (ldb (byte 8 i) int) *stream*)))

(define-reader integer ()
  (loop repeat 4
        for int = (read-byte *stream*)
        then (+ (* int #x100) (read-byte *stream*))
        finally (return int)))

(define-writer string (string)
  (write! integer (length string))
  (write-string string *stream*))

(define-reader string ()
  (let ((array (make-array (read! integer) :element-type 'character)))
    (read-sequence array *stream*)))

(define-writer version ()
  (write! byte *format-version*))

(define-reader version ()
  (read! byte))

(define-writer timestamp ()
  (write! integer (get-universal-time)))

(define-reader timestamp ()
  (read! integer))

(define-writer type (type)
  (write-sequence type *stream*))

(define-reader type ()
  (let ((type (make-array 4 :element-type 'character)))
    (read-sequence type *stream*)
    type))

(define-writer md5 ()
  ;(write-sequence (md5 *data*) *stream*)
  )

(define-reader md5 ()
  (let ((octets (make-array 16 :element-type '(unsigned-byte 8))))
    (read-sequence octets *stream*)
    (when *check-consistency*
      )))

(define-writer end-file ()
  (close *stream*))

(define-reader end-file ()
  (let ((byte (read-byte *stream* NIL NIL)))
    (when byte
      (error "File should end, but additional byte ~s read!" byte))))

(define-writer parent (arg)
  (declare (ignore arg)))

(define-reader parent ()
  *root*)

(define-writer children (children)
  (write! integer (length children))
  (loop for chunk across children
        do (write! chunk chunk)))

(define-reader children ()
  (let* ((num (read! integer))
         (array (make-child-array num)))
    (loop for i from 0 below num
          do (setf (aref array i)
                   (read! chunk)))
    array))

(define-writer map (map)
  (write! integer (hash-table-count map))
  (maphash (lambda (k v)
             (write! string k)
             (write! string v)) map))

(define-reader map ()
  (let* ((num (read! integer))
         (map (make-map num)))
    (loop repeat num
          for k = (read! string)
          for v = (read! string)
          do (setf (gethash k map) v))
    map))

(define-section file
  (header)
  (body)
  (footer))

(define-section header
  #x89 ; 
  #x50 ; ASCII P
  #x4c ; ASCII L
  #x55 ; ASCII U
  #x4d ; ASCII M
  #x50 ; ASCII P
  #x0d ; ASCII CR
  #x0a ; ASCII LF
  #x1a ; DOS EOF
  #x0a ; ASCII LF
  #x01 ; ASCII SOH
  (version)
  (timestamp))

(define-section body
  #x02 ; ASCII STX
  (root *root*)
  #x04 ; ASCII ETX
  )

(define-section footer
  (md5)
  (end-file))

(define-chunk-translator "NULL" null)

(define-reader null ()
  ())

(define-writer null (instance)
  (declare (ignore instance)))

(define-chunk ("NODE" node))

(define-chunk ("NEST" nesting-node)
  :children children)

(define-chunk ("CHLD" child-node)
  :parent parent)

(define-chunk ("TXND" textual-node)
  :text string)

(define-chunk ("ROOT" root)
  :children children)

(define-chunk ("TEXT" text-node)
  :parent parent
  :text string)

(define-chunk ("COMM" comment)
  :parent parent
  :text string)

(define-chunk ("DOCT" doctype)
  :parent parent
  :doctype string)

(define-chunk ("ELEM" element)
  :parent parent
  :tag-name string
  :attributes map
  :children children)

(define-chunk ("FTXT" fulltext-element)
  :parent parent
  :tag-name string
  :attributes map
  :children children)

(define-chunk ("XMLH" xml-header)
  :parent parent
  :attributes map)

(define-chunk ("CDAT" cdata)
  :parent parent
  :text string)

(define-chunk ("PROC" processing-instruction)
  :parent parent
  :tag-name string
  :text string)
