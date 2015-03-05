#|
 This file is a part of Plump-Bundle
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.bundle)

(defvar *check-consistency* NIL)
(defvar *stream*)
(defvar *parent*)
(defvar *root*)

(defun reader-name (name)
  (intern (string name) "PLUMP-BUNDLE-READERS"))

(defun writer-name (name)
  (intern (string name) "PLUMP-BUNDLE-WRITERS"))

(defmacro define-reader (name args &body body)
  `(defun ,(reader-name name) ,args
     #+NIL (format T "~&~2,'0x ~a" (file-position *stream*) ',(reader-name name))
     ,@body))

(defmacro define-writer (name args &body body)
  `(defun ,(writer-name name) ,args
     ,@body))

(defmacro read! (reader &rest args)
  `(,(reader-name reader) ,@args))

(defmacro write! (writer &rest args)
  `(,(writer-name writer) ,@args))

(defun read-string (length &optional (stream *standard-input*))
  (let ((seq (make-array length :element-type 'character)))
    (read-sequence seq stream)
    seq))

(defmacro check-equal (part reader type-format)
  (let ((read (gensym "READ")))
    `(let ((,read ,reader))
       (assert (equal ,part ,read) ()
               ,(format NIL "Expected ~a but got ~a at file-position ~~d."
                        type-format type-format)
               ,part ,read (file-position *stream*)))))

(defun transform-writer-part (part)
  (etypecase part
    ((unsigned-byte 8) `(write-byte ,part *stream*))
    (character `(write-char ,part *stream*))
    (string `(write-string ,part *stream*))
    ((and list (not null)) `(write! ,@part))))

(defun transform-reader-part (part)
  (etypecase part
    ((unsigned-byte 8) `(check-equal ,part (read-byte *stream*) "#x~2,'0x"))
    (character `(check-equal ,part (read-char *stream*) "#\~c"))
    (string `(check-equal ,part (read-string ,(length part) *stream*) "~s"))
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
  (second (find type *chunk-translations* :key #'first :test #'string=)))

(defun (setf chunk-translation) (class-name type)
  (pushnew (list type class-name) *chunk-translations* :key #'first :test #'string=))

(defmacro define-chunk-translator ()
  (let ((read-type (gensym "TYPE"))
        (chunk (gensym "CHUNK")))
    `(progn
       (define-reader chunk ()
         (let ((,read-type (read! type)))
           (cond ,@(loop for (type class-name) in *chunk-translations*
                         collect `((string= ,type ,read-type)
                                   (read! ,class-name)))
                 (T (error "Unrecognised block type ~s." ,read-type)))))
       
       (define-writer chunk (,chunk)
         (etypecase ,chunk
           ,@(loop for (type class-name) in *chunk-translations*
                   collect `(,class-name
                             (write! type ,type)
                             (write! ,class-name ,chunk))))))))

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

