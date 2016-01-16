(in-package :cl-user)
(defpackage :inmemory-cache.util
  (:use :cl)
  (:export :ftype*
           :octets))
(in-package :inmemory-cache.util)

(deftype octets (&optional (size '*))
  `(array (unsigned-byte 8) (,size)))

(annot:defannotation ftype* (typespec name)
    (:alias ftype :inline t :arity 2)
  "Shorthand for (DECLARE (INLINE ...))."
  (let ((symbol (annot.util:definition-form-symbol name))
        (type (annot.util:definition-form-type name)))
    (if (and symbol
             (member type
                     '(defun defmethod)))
        `(progn
           (declaim (ftype ,typespec ,symbol))
           ,name)
        `(declare (ftype ,typespec ,name)))))
