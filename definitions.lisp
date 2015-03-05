(defvar *check-consistency* NIL)
(defvar *version* 0)

(define-writer integer (stream int)
  (check-type int (integer 0 4294967296))
  (loop for i from (* 8 4) by (- 8) above 0
        do (write-byte (ldb (byte 8 i) int) stream)))

(define-reader integer (stream)
  (loop repeat 4
        for int = (read-byte stream)
        then (+ (* int #x100) (read-byte stream))
        finally (return int)))

(define-writer version (stream)
  (write-byte *version* stream))

(define-reader version (stream)
  (read-byte stream))

(define-writer timestamp (stream)
  (write-type '4byte-uint stream (get-universal-time)))

(define-reader timestamp (stream)
  (read-type '4byte-uint stream))

(define-writer type (stream type)
  (write-sequence type stream))

(define-reader type (stream)
  (let ((type (make-array 4 :element-type 'character)))
    (read-sequence type stream)
    type))

(define-writer count (stream vector)
  (write-type '4byte-uint stream (length vector)))

(define-reader count (stream)
  (read-type '4byte-uint stream))

(define-writer md5 (stream)
  (write-sequence (md5 *data*) stream))

(define-reader md5 (stream)
  (let ((octets (make-array 16 :element-type '(unsigned-byte 8))))
    (read-sequence octets stream)
    (when *check-consistency*
      )))

(define-writer end-file (stream)
  (close stream))

(define-reader end-file (stream)
  (let ((byte (read-byte stream NIL NIL)))
    (when byte
      (error "File should end, but additional byte ~s read!" ))))

(define-writer children (stream element)
  (write-type 'count stream (children element))
  (loop for chunk across (children element)
        do (write-type 'chunk stream chunk)))

(define-reader children (stream)
  (let* ((num (read-type 'count stream))
         (array (make-child-array num)))
    (loop for i from 0 below num
          do (setf (aref array i)
                   (read-type 'chunk stream)))))

(define-writer string (stream string)
  (write-string string stream))

(define-reader string (stream)
  (let* ((num (read-type 'count stream))
         (array (make-array num :element-type 'character)))
    (read-sequence array stream)))

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

(define-chunk ("ROOT" root)
  (children root))

(define-chunk ("TEXT" text-node)
  (string (text text-node)))

(define-chunk ("COMM" comment)
  (string (text text-node)))

(define-chunk ("DOCT" doctype)
  (string (doctype text-node)))

(define-chunk ("ELEM" element)
  (tag-name (tag-name element))
  (attribute-map (attributes element))
  (children element))

(define-chunk ("TXT" fulltext-element)
  (tag-name (tag-name element))
  (attribute-map (attributes element))
  (children element))
