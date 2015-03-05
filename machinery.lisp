#|
 This file is a part of Plump-Bundle
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.plump.bundle)

(defvar *check-consistency* NIL)
(defvar *stream*)
(defvar *root*)

(defun reader-name (name)
  (intern (string name) "PLUMP-BUNDLE-READERS"))

(defun writer-name (name)
  (intern (string name) "PLUMP-BUNDLE-WRITERS"))

(defmacro define-reader (name args &body body)
  `(defun ,(reader-name name) ,args
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

(defun transform-reader-part (part)
  (etypecase part
    ((unsigned-byte 8) `(write-byte ,part *stream*))
    (character `(write-char ,part *stream*))
    (string `(write-string ,part *stream*))
    ((and list (not null)) `(write! ,@part))))

(defun transform-writer-part (part)
  (etypecase part
    ((unsigned-byte 8) `(assert (= ,part (read-byte *stream*))))
    (character `(assert (char= ,part (read-char *stream*))))
    (string `(assert (string= ,part (read-string ,(length part) *stream*))))
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

(defmacro define-chunk-translator (type class-name)
  (pushnew (list type class-name) *chunk-translations* :key #'first :test #'string=)
  (let ((read-type (gensym "TYPE"))
        (chunk (gensym "CHUNK")))
    `(progn
       (define-reader chunk ()
         (let ((,read-type (read! type)))
           (cond ,@(loop for (type class-name) in *chunk-translations*
                         collect `((string= ,type ,read-type)
                                   (read! ,class-name))))))
       (define-writer chunk (,chunk)
         (etypecase ,chunk
           ,@(loop for (type class-name) in *chunk-translations*
                   collect `(,class-name
                             (write! type ,type)
                             (write! ,class-name ,chunk))))))))

(defmacro define-chunk ((type class-name) &body initargs)
  (let ((class (find-class class-name))
        (instance (gensym "INSTANCE")))
    `(progn
       (define-chunk-translator ,type ,class-name)
       ;; ??? PARENT ???
       (define-reader ,class-name ()
         (make-instance ',class-name
                        ,@(loop for (initarg part) on initargs by #'cddr
                                append `(,initarg (read! ,part)))))
       (define-writer ,class-name (,instance)
         (declare (ignorable ,instance))
         ,@(loop for (initarg part) on initargs by #'cddr
                 for accessor = (or (find-accessor-for-initarg class initarg)
                                    (error "No accessor for initarg ~a found on class ~a" initarg class-name))
                 collect `(write! ,part (,accessor ,instance)))))))

