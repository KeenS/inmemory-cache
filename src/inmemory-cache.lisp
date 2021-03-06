(in-package :cl-user)
(defpackage :inmemory-cache
  (:use :cl :inmemory-cache.util)
  (:import-from :inmemory-cache.messagepack
                :encode-to-buffer
                :encoding-size
                :decode-from-buffer)
  (:export
   :+kilo+
   :+mega+
   :+giga+
   :+peta+
   :cache
   :make-cache
   :put-cache
   :get-cache))
(in-package :inmemory-cache)
(annot:enable-annot-syntax)

(defconstant +entry-size+ 1024)
(defconstant +kilo+ 1024)
(defconstant +mega+ (* 1024 +kilo+))
(defconstant +giga+ (* 1024 +mega+))
(defconstant +peta+ (* 1024 +giga+))

@ftype (function (integer) (octets))
(defun make-octets (size)
  (make-array size :element-type '(unsigned-byte 8)))

@ftype (function (integer) bit-vector)
(defun make-bit-vector (size)
  (make-array size :element-type 'bit))

;;; FIXME: use hash functions
@ftype (function (octets) fixnum)
(defun cache-hash (octets)
  (loop :for octet :across octets
     :summing octet))

(defstruct (cache
             (:constructor %make-cache
                           (bucket open-table)))
  (bucket (make-octets 0) :type octets)
  (hash-function #'cache-hash :type (function (octets) fixnum))
  (open-table (make-bit-vector 0) :type bit-vector))

@ftype (function (integer) integer)
(defun normalize-to-entry-size (int)
  (* +entry-size+ (truncate (+ int +entry-size+ -1) +entry-size+)))

@ftype (function (integer) cache)
(defun make-cache (size)
  (let* ((length (normalize-to-entry-size size))
         (size   (/ length +entry-size+)))
    (%make-cache (make-octets length) (make-bit-vector size))))

@ftype (function (cache) integer)
(defun cache-bucket-size (cache)
  (length (cache-open-table cache)))

@ftype (function (octets octets t integer integer) integer)
(defun write-entry-unsafe (bucket key value expire start)
  (setf start (encode-to-buffer (+ (encoding-size expire) (encoding-size key) (encoding-size value)) bucket start))
  (setf start (encode-to-buffer expire bucket start))
  (setf start (encode-to-buffer key bucket start))
  (encode-to-buffer value bucket start))

@ftype (function (cache octets t integer) integer)
(defun put-cache (cache key value expire)
  (with-slots (bucket hash-function open-table) cache
    (let* ((bucket-size (length open-table))
           (hash (funcall hash-function key))
           (pos  (rem hash bucket-size)))
      (setf (aref open-table pos) 1)
      (write-entry-unsafe bucket key value expire (* pos +entry-size+)))))

@ftype (function (octets octets integer) (values (or octets null) (or integer null)))
(defun read-entry-unsafe (buffer search-key start)
  (let (expire key value)
    ;; length is unused
    (setf (values length start) (decode-from-buffer buffer start))

    (setf (values expire start) (decode-from-buffer buffer start))
    (when (< expire (get-universal-time))
      ;; expired
      (return-from read-entry-unsafe (values nil expire)))

    (setf (values key start) (decode-from-buffer buffer start))
    (when (mismatch key search-key)
      ;; hash was equal but key isn't equal
      (return-from read-entry-unsafe (values nil nil)))

    (setf value (decode-from-buffer buffer start))
    (values value expire)))

@ftype (function (cache octets) (values (or octets null) (or integer null)))
(defun get-cache (cache key)
  (with-slots (bucket hash-function open-table) cache
    (let* ((bucket-size (length open-table))
           (hash (funcall hash-function key))
           (pos  (rem hash bucket-size)))
      (if (zerop (aref open-table pos))
          (values nil nil)
          (multiple-value-bind (value expire) (read-entry-unsafe bucket key (* pos +entry-size+))
            (when (and expire (null value))
              (setf (aref open-table pos) 0)
              (setf expire nil))
            (values value expire))))))
