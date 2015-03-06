#|
 This file is a part of Plump-Bundle
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.bundle)

(defvar *check-consistency* NIL)
(defvar *buffer*)
(defvar *parent*)
(defvar *root*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reader-name (name)
    (intern (string name) "PLUMP-BUNDLE-READERS"))

  (defun writer-name (name)
    (intern (string name) "PLUMP-BUNDLE-WRITERS")))

(defmacro define-reader (name args &body body)
  `(defun ,(reader-name name) ,args
     (declare (optimize speed))
     ,@body))

(defmacro define-writer (name args &body body)
  `(defun ,(writer-name name) ,args
     (declare (optimize speed))
     ,@body))

(defmacro read! (reader &rest args)
  `(,(reader-name reader) ,@args))

(defmacro write! (writer &rest args)
  `(,(writer-name writer) ,@args))

(define-writer byte (byte)
  (check-type byte (unsigned-byte 8))
  (fast-io:fast-write-byte byte *buffer*))

(define-reader byte ()
  (fast-io:fast-read-byte *buffer*))

(define-writer integer (int)
  (check-type int (unsigned-byte 32))
  (fast-io:writeu32-be int *buffer*))

(define-reader integer ()
  (the (unsigned-byte 32) (fast-io:readu32-be *buffer*)))

(define-writer char (char)
  (fast-io:fast-write-byte (char-code char) *buffer*))

(define-reader char ()
  (code-char (fast-io:fast-read-byte *buffer*)))

(define-writer string (string)
  (let ((octets (the vector (babel:string-to-octets string :encoding :utf-8))))
    (write! integer (length octets))
    (fast-io:fast-write-sequence octets *buffer*)))

(define-reader string (&optional (length (read! integer)))
  (let ((seq (make-array length :element-type '(unsigned-byte 8))))
    (fast-io:fast-read-sequence seq *buffer*)
    (babel:octets-to-string seq :encoding :utf-8)))

(defmacro check-equal (part reader type-format)
  (let ((read (gensym "READ")))
    `(let ((,read ,reader))
       (assert (equal ,part ,read) ()
               ,(format NIL "Expected ~a but got ~a"
                        type-format type-format)
               ,part ,read))))

(defun transform-writer-part (part)
  (etypecase part
    ((unsigned-byte 8) `(write! byte ,part))
    (character `(write! char ,part))
    (string `(write! string ,part))
    ((and list (not null)) `(write! ,@part))))

(defun transform-reader-part (part)
  (etypecase part
    ((unsigned-byte 8) `(check-equal ,part (read! byte) "#x~2,'0x"))
    (character `(check-equal ,part (read! char) "#\~c"))
    (string `(check-equal ,part (read! string ,(length part)) "~s"))
    ((and list (not null)) `(read! ,@part))))

(defmacro define-section (name &body parts)
  `(progn
     (define-reader ,name ()
       ,@(loop for part in parts
               collect (transform-reader-part part)))
     (define-writer ,name ()
       ,@(loop for part in parts
               collect (transform-writer-part part)))))

(defun find-accessor-for-initarg (class initarg)
  (labels ((scan (class)
             (dolist (slotdef (c2mop:class-direct-slots class))
               (dolist (slotinit (c2mop:slot-definition-initargs slotdef))
                 (when (eql slotinit initarg)
                   (return-from find-accessor-for-initarg
                     (first (c2mop:slot-definition-readers slotdef))))))
             (dolist (superclass (c2mop:class-direct-superclasses class))
               (unless (eql superclass (find-class 'standard-object))
                 (scan superclass)))))
    (scan class)))

(defvar *chunk-translations* ())

(defun chunk-translation (type)
  (second (find type *chunk-translations* :key #'first :test #'equalp)))

(defun (setf chunk-translation) (class-name type)
  (etypecase type
    (string (setf type (babel:string-to-octets type :encoding :utf-8)))
    ((vector (unsigned-byte 8))))
  (pushnew (list type class-name) *chunk-translations* :key #'first :test #'equalp))


(declaim (inline type=))
(defun type= (a b)
  (declare ((simple-array (unsigned-byte 8) (4)) a b))
  (and (= (aref a 0) (aref b 0))
       (= (aref a 1) (aref b 1))
       (= (aref a 2) (aref b 2))
       (= (aref a 3) (aref b 3))))

(define-writer type (type)
  (loop for byte across (the (simple-array (unsigned-byte 8)) type)
        do (write! byte byte)))

(define-reader type ()
  (let ((type (make-array 4 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 4
          do (setf (aref type i) (read! byte)))
    type))

(defmacro define-chunk-translator ()
  (let ((read-type (gensym "TYPE"))
        (chunk (gensym "CHUNK")))
    `(progn
       (define-reader chunk ()
         (let ((,read-type (read! type)))
           (cond ,@(loop for (type class-name) in *chunk-translations*
                         collect `((type= ,type ,read-type)
                                   (read! ,class-name)))
                 (T (error "Unrecognised block type ~s." ,read-type)))))
       
       (define-writer chunk (,chunk)
         (cond ,@(loop for (type class-name) in *chunk-translations*
                       collect `((eql (class-of ,chunk) (load-time-value (find-class ',class-name)))
                                 (write! type ,type)
                                 (write! ,class-name ,chunk)))
           (T (error "Don't know how to translate ~a to chunk." ,chunk)))))))

(defmacro define-chunk ((type class-name) initargs &body body)
  (let ((class (find-class class-name))
        (instance (gensym "INSTANCE")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (chunk-translation ,type) ',class-name))
       ;; ??? PARENT ???
       (define-reader ,class-name ()
         (let ((*parent*
                 (make-instance ',class-name
                                ,@(loop for (initarg part) on initargs by #'cddr
                                        append `(,initarg (read! ,part))))))
           ,@(mapcar #'transform-reader-part body)
           *parent*))
       
       (define-writer ,class-name (&optional (,instance *parent*))
         (declare (ignorable ,instance))
         (let ((*parent* ,instance))
           ,@(loop for (initarg part) on initargs by #'cddr
                   for accessor = (or (find-accessor-for-initarg class initarg)
                                      (error "No accessor for initarg ~a found on class ~a" initarg class-name))
                   collect `(write! ,part (,accessor ,instance)))
           
           ,@(mapcar #'transform-writer-part body)
           *parent*)))))

