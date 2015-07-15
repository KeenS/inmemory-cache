(in-package :cl-user)
(defpackage :inmemory-cache
  (:use :cl)
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

(defconstant +expirelen+ 4
  "The number of bytes to stand for expire")
(defconstant +lengthlen+ 4
  "The number of bytes to stand for length")
(defconstant +entry-size+ 128
  "The size of one entry")
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

#+(or) (make-cache 20)

(defstruct cache-entry
  (key (make-octets 0) :type (octets))
  (value (make-octets 0) :type (octets))
  (expire 0 :type (unsigned-byte)))

(defun write-expire (bucket expire start)
  "write expire to bucket. big endian."
  (let (div rem)
    (incf start +expirelen+)
    (loop :repeat +expirelen+ :do
       (progn
         (decf start)
         (multiple-value-setq (div rem) (truncate expire 256))
         (setf (aref bucket start) rem)
         (setf expire div)))))

(defun write-length (bucket length start)
  "write expire to bucket. little endian."
  (let (div rem)
    (incf  start +lengthlen+)
    (loop :repeat +lengthlen+ :do
       (progn
         (decf start)
         (multiple-value-setq (div rem) (truncate length 256))
         (setf (aref bucket start) rem)
         (setf length div)))))

(defun write-entry-unsafe (bucket key value expire start)
  (let ((keylen (length key))
        (valuelen (length value)))
    
    (write-length bucket (+ keylen valuelen +expirelen+) start)
    (incf start +lengthlen+)
    (write-expire bucket expire start)
    (incf start +expirelen+)

    (write-length bucket keylen start)
    (incf start +lengthlen+)
    (replace bucket key   :start1 start)
    (incf start (length key))
    
    (write-length bucket valuelen start)
    (incf start +lengthlen+)
    (replace bucket value :start1 start)))

(defun put-cache (cache key value expire)
  (with-slots (bucket hash-function) cache
    (let* ((bucketlen (length bucket))
           (hash (funcall hash-function key)))
      
      (write-entry-unsafe bucket key value expire (rem (normalize-to-entry-size hash) bucketlen)))))

(defun read-expire (buffer start)
  (loop :with result := 0
     :repeat +expirelen+
     :do
     (progn
       (setf result (ash result 8))
       (incf result (aref buffer start))
       (incf start))
     :finally (return result)))

(defun read-length (buffer start)
  (loop :with result := 0
     :repeat +lengthlen+
     :do
     (progn
       (setf result (ash result 8))
       (incf result (aref buffer start))
       (incf start))
     :finally (return result)))


(defun read-entry (buffer search-key start)
  (let (len expire keylen key valuelen value)
    (setf len (read-length buffer start))
    (incf start +lengthlen+)
    (setf expire (read-expire buffer start))
    (when (< expire (get-universal-time))
        (return-from read-entry (values nil len)))
    (incf start +expirelen+)
    (setf keylen (read-length buffer start))
    (when (/= keylen (length search-key))
        (return-from read-entry (values nil len)))
    (incf start +lengthlen+)
    (when (mismatch buffer search-key :start1 start :end1 (+ start (length search-key)))
        (return-from read-entry (values nil len)))
    (setf key (subseq buffer start (+ start keylen)))
    (incf start keylen)
    (setf valuelen (read-length buffer start))
    (incf start +lengthlen+)
    (setf value (subseq buffer start (+ start valuelen)))
    (values (make-cache-entry :key key :value value :expire expire)
            len)))

(defun get-cache (cache key)
  (with-slots (bucket hash-function) cache
    (let* ((bucketlen (length bucket))
           (hash (funcall hash-function key)))
      (read-entry bucket key (rem (normalize-to-entry-size hash) bucketlen)))))

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
