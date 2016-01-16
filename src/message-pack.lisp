(in-package :cl-user)
(defpackage :inmemory-cache.messagepack
  (:use :cl :inmemory-cache.util)
  (:export
   :encode
   :encoding-size
   :encode-to-buffer
   :decode-from-buffer))
(in-package :inmemory-cache.messagepack)
(annot:enable-annot-syntax)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (format nil "~{~a~}" args))
  (defun mksymb (&rest args)
    (intern (apply #'mkstr args)))
  (defun speed-for-size (size)
    (if (< size (integer-length most-positive-fixnum))
        3
        0)))
(defmacro signed-unsigned-convertors (size)
  `(progn
     (defun ,(mksymb 'sb size '-> 'ub size) (sb)
       (declare (optimize (debug 0) (safety 0) (speed ,(speed-for-size size)))
                (type (integer ,(- (expt 2 (1- size))) ,(1- (expt 2 (1- size)))) sb))
       (if (< sb 0)
           (ldb (byte ,size 0) sb)
           sb))
     (defun ,(mksymb 'ub size '-> 'sb size) (sb)
       (declare (optimize (debug 0) (safety 0) (speed ,(speed-for-size size)))
                (type (mod ,(expt 2 size)) sb))
       (if (logbitp (1- ,size) sb)
           (- (1+ (logxor (1- (expt 2 ,size)) sb)))
           sb))))

(signed-unsigned-convertors 8)
(signed-unsigned-convertors 16)
(signed-unsigned-convertors 32)
(signed-unsigned-convertors 64)

(defmacro store-big-endian (number buffer byte-count start)
  (let ((g-number     (gensym "number"))
        (g-buffer     (gensym "buffer"))
        (g-start      (gensym "start"))
        (g-byte-count (gensym "byte-count")))
    `(let* ((,g-number      ,number)
            (,g-buffer      ,buffer)
            (,g-byte-count  ,byte-count)
            (,g-start       ,start))
       ,@(loop
            :for i :from 0 :below byte-count
            :collect `(setf (aref ,g-buffer (- (+ ,g-byte-count ,g-start -1) ,i)) (ldb (byte 8 ,(* 8 i)) ,g-number))))))

(defun encode (data)
  (let* ((size (encoding-size data))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (encode-to-buffer data buffer 0)
    buffer))


(defun encoding-size (data)
  (etypecase data
    (integer    (encoding-size-integer     data))
    (float      (encoding-size-float       data))
    (null       (encoding-size-null        data))
    (boolean    (encoding-size-boolean     data))
    (string     (encoding-size-string      data))
    ((array (unsigned-byte 8) (*)) (encoding-size-octets data))
    (vector     (encoding-size-vector      data))
    (hash-table (encoding-size-hash-table  data))))

@ftype (function (t octets integer) integer)
(defun encode-to-buffer (data buffer start)
  (etypecase data
    (integer    (encode-integer-to-buffer     data buffer start))
    (float      (encode-float-to-buffer       data buffer start))
    (null       (encode-null-to-buffer        data buffer start))
    (boolean    (encode-boolean-to-buffer     data buffer start))
    (string     (encode-string-to-buffer      data buffer start))
    ((array (unsigned-byte 8) (*)) (encode-octets-to-buffer data buffer start))
    (vector     (encode-vector-to-buffer      data buffer start))
    (hash-table (encode-hash-table-to-buffer  data buffer start))))


(defmacro define-encoder (type name lambda-list &body body)
  `(progn
     (declaim (ftype (function (,type (array (unsigned-byte 8) (*)) t) integer) ,name))
     (defun ,name ,lambda-list
       ,@body)))


(defun encoding-size-integer (data)
  ;; (declare (type fixnum data))
  (cond ((<= 0 data 127)
         1)
        ((<= -32 data -1)
         1)
        ((<= 0 data 255)
         2)
        ((<= 0 data 65535)
         3)
        ((<= 0 data (1- (expt 2 32)))
         5)
        ((<= 0 data (1- (expt 2 64)))
         9)
        ((<= -128 data 127)
         2)
        ((<= -32768 data 32767)
         3)
        ((<= (- (expt 2 31)) data (1- (expt 2 31)))
         5)
        ((<= (- (expt 2 63)) data (1- (expt 2 63)))
         9)
        (t (error "Integer too large or too small."))))

(define-encoder integer encode-integer-to-buffer (data buffer start)
  (cond ((<= 0 data 127)
         (setf (aref buffer start) data)
         (+ 1 start))
        ((<= -32 data -1)
         (setf (aref buffer start) (sb8->ub8 data))
         (+ 1 start))
        ((<= 0 data 255)
         (setf (aref buffer start) #xcc)
         (incf start)
         (setf (aref buffer start) data)
         (+ 1 start))
        ((<= 0 data 65535)
         (setf (aref buffer start) #xcd)
         (incf start)
         (store-big-endian data buffer 2 start)
         (+ 2 start))
        ((<= 0 data (1- (expt 2 32)))
         (setf (aref buffer start) #xce)
         (incf start)
         (store-big-endian data buffer 4 start)
         (+ 4 start))
        ((<= 0 data (1- (expt 2 64)))
         (setf (aref buffer start) #xcf)
         (incf start)
         (store-big-endian data buffer 8 start)
         (+ 8 start))
        ((<= -128 data 127)
         (setf (aref buffer start) #xd0)
         (incf start)
         (setf (aref buffer start) (ldb (byte 8 0) data))
         (+ 1 start))
        ((<= -32768 data 32767)
         (setf (aref buffer start) #xd1)
         (incf start)
         (store-big-endian (ldb (byte 16 0) data) buffer 2 start)
         (+ 2 start))
        ((<= (- (expt 2 31)) data (1- (expt 2 31)))
         (setf (aref buffer start) #xd2)
         (incf start)
         (store-big-endian (ldb (byte 32 0) data) buffer 4 start)
         (+ 4 start))
        ((<= (- (expt 2 63)) data (1- (expt 2 63)))
         (setf (aref buffer start) #xd3)
         (incf start)
         (store-big-endian (ldb (byte 64 0) data) buffer 8 start)
         (+ 8 start))
        (t (error "Integer too large or too small."))))


(defun encoding-size-float (data)
  (etypecase data
    (single-float 5)
    (double-float 9)))


#+sbcl (define-encoder float encode-float-to-buffer (data buffer start)
         (etypecase data
           (single-float
            (setf (aref buffer start) #xca)
            (incf start)
            (store-big-endian (sb-kernel:single-float-bits data) buffer 4 start)
            (+ 4 start))
           (double-float
            (setf (aref buffer start) #xcb)
            (incf start)
            (store-big-endian (sb-kernel:double-float-high-bits data) buffer 4 start)
            (incf start 4)
            (store-big-endian (sb-kernel:double-float-low-bits data) buffer 4 start)
            (+ 4 start))))

#+ccl (define-encoder float encode-float-to-buffer (data buffer start)
        (etypecase
            (single-float
             (error "No cl-messagepack support for single precision floats in CCL."))
          (double-float
           (setf (aref buffer start )#xcb)
           (incf start)
           (multiple-value-bind (hi lo)
               (ccl::double-float-bits data)
             (store-big-endian hi buffer 4 start)
             (incf start 4)
             (store-big-endian lo buffer 4 start)
             (+ 4 start)))))

#-(or sbcl ccl)
(define-encoder float encode-float-to-buffer (data buffer start)
  (declare (ignore data buffer start))
  (error (format nil "No floating point support yet on ~a" (lisp-implementation-type))))



(defun encoding-size-null (data)
  (declare (ignore data))
  1)

(define-encoder null encode-null-to-buffer (data buffer start)
  (declare (ignore data))
  (setf (aref buffer start) #xc0)
  (+ 1 start))

(defun encoding-size-boolean (data)
  (declare (ignore data))
  1)

(define-encoder boolean encode-boolean-to-buffer (data buffer start)
  (declare (ignore data))
  (setf (aref buffer start) #xc3)
  (+ 1 start))

(defun encoding-size-sequence-length (len short-length typecode-8-p)
  (cond ((and (<= 0 len short-length)
              (plusp short-length))
         1)
        ((and (<= 0 len (1- (expt 2 8)))
              typecode-8-p)
         2)
        ((<= 0 len (1- (expt 2 16)))
         3)
        ((<= 0 len (1- (expt 2 32)))
         5)))

(defun encode-sequence-length-to-buffer (len buffer start 
                               short-prefix short-length
                               typecode-8 typecode-16 typecode-32)
  (cond ((and (<= 0 len short-length)
              (plusp short-length)
              (plusp short-prefix))
         (setf (aref buffer start) (+ short-prefix len))
         (+ 1 start))
        ((and (<= 0 len (1- (expt 2 8)))
              (plusp typecode-8))
         (setf (aref buffer start) typecode-8)
         (incf start)
         (store-big-endian len buffer 1 start)
         (+ 1 start))
        ((<= 0 len (1- (expt 2 16)))
         (setf (aref buffer start) typecode-16)
         (incf start)
         (store-big-endian len buffer 2 start)
         (+ 2 start))
        ((<= 0 len (1- (expt 2 32)))
         (setf (aref buffer start) typecode-32)
         (incf start)
         (store-big-endian len buffer 4 start)
         (+ 4 start))))


(defun encoding-size-string (data)
  (+ (encoding-size-sequence-length (babel:string-size-in-octets data) 31 t) (length data)))

(define-encoder string encode-string-to-buffer (data buffer start)
  (setf start (encode-sequence-length-to-buffer (babel:string-size-in-octets data) buffer start #xa0 31 #xd9 #xda #xdb))
  (replace buffer (babel:string-to-octets data) :start1 start)
  (+ (length data) start))


(defun encoding-size-hash-table (data)
  (+ (encoding-size-sequence-length (hash-table-count data) 15 nil)
     (loop :for key :being :the :hash-keys :of data :using (hash-value value)
        :summing (+ (encoding-size key) (encoding-size value)))))

(define-encoder hash-table encode-hash-table-to-buffer  (data buffer start)
  (setf start (encode-sequence-length-to-buffer (hash-table-count data) buffer start #x80 15 0 #xde #xdf))
  (loop :for key :being :the :hash-keys :of data :using (hash-value value)
     do (progn
          (setf start (encode-to-buffer key   buffer start))
          (setf start (encode-to-buffer value buffer start))))
  start)


(defun encoding-size-vector (data)
  (+ (encoding-size-sequence-length (length data) 15 nil)
     (loop :for elm :across data
        :summing (encoding-size elm))))

(define-encoder vector encode-vector-to-buffer (data buffer start)
  (setf start (encode-sequence-length-to-buffer (length data) buffer start #x90 15 0 #xdc #xdd))
  (loop :for elm :across data
     :do (setf start (encode-to-buffer elm buffer start)))
  start)


(defun encoding-size-octets (data)
  (+ (encoding-size-sequence-length (length data) 0 t)
     (length data)))

(define-encoder (array (unsigned-byte 8) (*)) encode-octets-to-buffer (data buffer start)
  (setf start (encode-sequence-length-to-buffer (length data) buffer start 0 0 #xc4 #xc5 #xc6))
  (replace buffer data :start1 start)
  (+ (length data) start))




(defun load-big-endian (buffer start size)
  (loop
     :with result := 0
     :repeat size :do
     (progn
       (setf result (+ (ash result 8) (aref buffer start)))
       (incf start))
     :finally (return result)))


(defun decode-from-buffer (buffer &optional (start 0))
  (let ((byte (aref buffer start)))
    (incf start)
    (cond 
      ((= 0 (ldb (byte 1 7) byte)) (values byte start))
      ((= 8 (ldb (byte 4 4) byte)) (decode-map-from-buffer buffer start (ldb (byte 4 0) byte)))
      ((= 9 (ldb (byte 4 4) byte)) (decode-array-from-buffer buffer start (ldb (byte 4 0) byte)))
      ((= 5 (ldb (byte 3 5) byte)) (decode-string-from-buffer buffer start (ldb (byte 5 0) byte)))
      ((= 7 (ldb (byte 3 5) byte)) (values (ub8->sb8 byte) start))
      (t (case byte
           ((#xc0) (values nil start))
           ; #xc1: never used
           ((#xc2) (values nil start))
           ((#xc3) (values t   start))
           ((#xc4) (decode-octets-from-buffer buffer (+ 1 start) (load-big-endian buffer start 1)))
           ((#xc5) (decode-octets-from-buffer buffer (+ 2 start) (load-big-endian buffer start 2)))
           ((#xc6) (decode-octets-from-buffer buffer (+ 4 start) (load-big-endian buffer start 4)))
           ; #xc7 ~ #xc9 ext
           ((#xca) (decode-float-from-buffer buffer start))
           ((#xcb) (decode-double-from-buffer buffer start))
           ((#xcc) (values (load-big-endian buffer start 1) (+ 1 start)))
           ((#xcd) (values (load-big-endian buffer start 2) (+ 2 start)))
           ((#xce) (values (load-big-endian buffer start 4) (+ 4 start)))
           ((#xcf) (values (load-big-endian buffer start 8) (+ 8 start)))
           ((#xd0) (values (ub8->sb8   (load-big-endian buffer start 1)) (+ 1 start)))
           ((#xd1) (values (ub16->sb16 (load-big-endian buffer start 2)) (+ 2 start)))
           ((#xd2) (values (ub32->sb32 (load-big-endian buffer start 4)) (+ 4 start)))
           ((#xd3) (values (ub64->sb64 (load-big-endian buffer start 8)) (+ 8 start)))
           ; #xd4 ~ #xd8: fixext
           ((#xd9) (decode-string-from-buffer buffer (+ 1 start) (load-big-endian buffer start 1)))
           ((#xda) (decode-string-from-buffer buffer (+ 2 start) (load-big-endian buffer start 2)))
           ((#xdb) (decode-string-from-buffer buffer (+ 4 start) (load-big-endian buffer start 4)))
           ((#xdc) (decode-array-from-buffer buffer (+ 2 start) (load-big-endian buffer start 2)))
           ((#xdd) (decode-array-from-buffer buffer (+ 4 start) (load-big-endian buffer start 4)))
           ((#xde) (decode-map-from-buffer buffer (+ 2 start) (load-big-endian buffer start 2)))
           ((#xdf) (decode-map-from-buffer buffer (+ 4 start) (load-big-endian buffer start 4))))))))

#+sbcl
(defun decode-float-from-buffer (buffer start)
  (values (sb-kernel:make-single-float (load-big-endian buffer start 4)) (+ 4 start)))
#-sbcl
(defun decode-float-from-buffer (buffer start)
  (declare (ignore buffer start))
  (error "No floating support yen on ~a." (lisp-implementation-type)))

#+sbcl
(defun decode-double-from-buffer (buffer start)
  (values (sb-kernel:make-double-float (ub32->sb32 (load-big-endian buffer start 4))
                                       (ub32->sb32 (load-big-endian buffer (+ 4 start) 4)))
          (+ 8 start)))

#+ccl
(defun decode-double-from-buffer (buffer start)
  (values (ccl::double-float-from-bits (load-big-endian buffer start 4)
                                       (load-big-endian buffer (+ 4 start) 4))
          (+ 8 start)))
#-(or ccl sbcl)
(defun decode-double-from-buffer (buffer start)
  (declare (ignore buffer start))
  (error "No floating support yen on ~a." (lisp-implementation-type)))

(defun decode-string-from-buffer (buffer start size)
  (values
   (babel:octets-to-string buffer :start start :end (+ start size))
   (+ size start)))

(defun decode-array-from-buffer (buffer start size)
  (let ((array (make-array size)))
    (loop
       :for i :from 0 :below size
       :repeat size :do
       (setf (values (aref array i) start) (decode-from-buffer buffer start)))
    (values array start)))

(defun decode-map-from-buffer (buffer start size)
  (let ((hash (make-hash-table :size size)))
    (loop
       :with key
       :repeat size :do
       (progn
         (setf (values key start) (decode-from-buffer buffer start))
         (setf (values (gethash key hash) start) (decode-from-buffer buffer start))))
    (values hash start)))

(defun decode-octets-from-buffer (buffer start size)
  (let ((octets (make-array size :element-type '(unsigned-byte 8))))
    (replace octets buffer :start2 start :end2 (+ start size))
    (values octets (+ start size))))
