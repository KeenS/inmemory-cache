(in-package :cl-user)
(defpackage :inmemory-cache
  (:use :cl)
  (:import-from :inmemory-cache.messagepack
                :encode-to-buffer
                :decode-from-buffer)
  (:export
   :+kilo+
   :+mega+
   :+giga+
   :+peta+
   :cache
   :make-cache
   :cache-entry
   :cache-entry-key
   :cache-entry-value
   :cache-entry-expire
   :put-cache
   :get-cache))
(in-package :inmemory-cache)

(defconstant +kilo+ 1024)
(defconstant +mega+ (* 1024 +kilo+))
(defconstant +giga+ (* 1024 +mega+))
(defconstant +peta+ (* 1024 +giga+))


(deftype octets (&optional (size '*))
  `(array (unsigned-byte 8) (,size)))

(declaim (ftype (function (integer) (octets)) make-octets))
(defun make-octets (size)
  (make-array size :element-type '(unsigned-byte 8)))

(declaim (ftype (function (octets) fixnum) cache-hash))
(defun cache-hash (octets)
  (loop :for octet :across octets
     :summing octet))

(defstruct (cache
             (:constructor %make-cache
                           (bucket)))
  (bucket (make-octets 0) :type octets)
  (hash-function #'cache-hash :type (function (octets) fixnum)))

(defun normalize-to-entry-size (int)
  (* +entry-size+ (truncate (+ int +entry-size+ -1) +entry-size+)))

(declaim (ftype (function (integer) cache) make-cache))
(defun make-cache (size)
  (%make-cache (make-octets (normalize-to-entry-size size))))

(defstruct cache-entry
  (key (make-octets 0) :type (octets))
  (value (make-octets 0) :type (octets))
  (expire 0 :type (unsigned-byte)))

(defun write-entry-unsafe (bucket key value expire start)
  (let ((keylen (length key))
        (valuelen (length value)))
    (setf start (encode-to-buffer (+ keylen valuelen +expirelen+) bucket start))
    (setf start (encode-to-buffer expire bucket start))
    (setf start (encode-to-buffer key bucket start))
    (encode-to-buffer value bucket start)))

(defun put-cache (cache key value expire)
  (with-slots (bucket hash-function) cache
    (let* ((bucketlen (length bucket))
           (hash (funcall hash-function key)))
      (write-entry-unsafe bucket key value expire (rem (normalize-to-entry-size hash) bucketlen)))))

(defun read-entry-unsafe (buffer search-key start)
  (let (len expire key value)
    (setf (values len start) (decode-from-buffer buffer start))
    (setf (values expire start) (decode-from-buffer buffer start))
    (when (< expire (get-universal-time))
      (return-from read-entry (values nil len)))
    (setf (values key start) (decode-from-buffer buffer start))
    (when (mismatch key search-key)
      (return-from read-entry (values nil len)))
    (setf value (decode-from-buffer buffer start)) 
    (values (make-cache-entry :key key :value value :expire expire)
            len)))

(defun get-cache (cache key)
  (with-slots (bucket hash-function) cache
    (let* ((bucketlen (length bucket))
           (hash (funcall hash-function key)))
      (read-entry-unsafe bucket key (rem (normalize-to-entry-size hash) bucketlen)))))

#+ (or)
(let ((cache (make-cache 1024)) 
      (a (coerce #1A(97) '(octets)))
      (b (coerce #1A(98) '(octets))))
  (put-cache cache a b (+ (get-universal-time) (* 60 60)))
  (get-cache cache a)
  cache)

#+ (or)
(let ((cache (make-cache 1024)) 
      (a (coerce #1A(97) '(octets)))
      (b (coerce #1A(98) '(octets))))
  (put-cache cache a b (- (get-universal-time) (* 60 60)))
  (get-cache cache a)
  cache)
