#|
 This file is a part of Plump-Bundle
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.bundle)
(defvar *format-version* 0)
(declaim ((unsigned-byte 8) *format-version*))

(define-writer version ()
  (write! byte *format-version*))

(define-reader version ()
  (let ((version (read! byte)))
    (when (/= version *format-version*)
      (warn "Version mismatch! Document is v~d, library implements v~d. Continuing with fingers crossed."
            version *format-version*))))

(define-writer timestamp ()
  (write! integer (get-universal-time)))

(define-reader timestamp ()
  (read! integer))

(define-writer md5 ()
  ;(write-sequence (md5 *data*) *stream*)
  )

(define-reader md5 ()
  (let ((octets (make-array 16 :element-type '(unsigned-byte 8))))
    (fast-io:fast-read-sequence octets *buffer*)
    (when *check-consistency*
      )))

(define-writer end-file ()
  )

(define-reader end-file ()
  (let ((byte (fast-io:fast-read-byte *buffer* NIL NIL)))
    (when byte
      (error "File should end, but additional byte ~s read!" byte))))

(define-writer parent (arg)
  (declare (ignore arg)))

(define-reader parent ()
  *parent*)

(define-writer children (&optional (node *parent*))
  (let ((children (the (vector node) (children node))))
    (write! integer (length children))
    (loop for chunk across children
          do (write! chunk chunk))))

(define-reader children ()
  (let* ((num (read! integer))
         (array (make-child-array num)))
    (loop for i from 0 below num
          do (setf (aref array i)
                   (read! chunk)))
    (setf (fill-pointer array) num)
    (setf (children *parent*) array)))

(define-writer map (map)
  (write! integer (hash-table-count map))
  (maphash (lambda (k v)
             (write! string k)
             (write! string v)) map))

(define-reader map ()
  (let* ((num (read! integer))
         (map (make-attribute-map num)))
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
  (timestamp)
  #x02 ; ASCII STX)

(define-section footer
  #x04 ; ASCII ETX
  (end-file))

(define-reader body ()
  (setf *parent* (read! chunk)))

(define-writer body ()
  (write! chunk *parent*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (chunk-translation "NULL") 'null))

(define-reader null ()
  ())

(define-writer null (instance)
  (declare (ignore instance)))

(define-chunk ("NODE" node)
              ())

(define-chunk ("NEST" nesting-node)
              (:children children))

(define-chunk ("CHLD" child-node)
              (:parent parent))

(define-chunk ("TXND" textual-node)
              (:text string))

(define-chunk ("ROOT" root)
              ()
  (children))

(define-chunk ("TEXT" text-node)
              (:parent parent
               :text string))

(define-chunk ("COMM" comment)
              (:parent parent
               :text string))

(define-chunk ("DOCT" doctype)
              (:parent parent
               :doctype string))

(define-chunk ("ELEM" element)
              (:parent parent
               :tag-name string
               :attributes map)
  (children))

(define-chunk ("FTXT" fulltext-element)
              (:parent parent
               :tag-name string
               :attributes map)
  (children))

(define-chunk ("XMLH" xml-header)
              (:parent parent
               :attributes map))

(define-chunk ("CDAT" cdata)
              (:parent parent
               :text string))

(define-chunk ("PROC" processing-instruction)
              (:parent parent
               :tag-name string
               :text string))

(define-chunk-translator)
