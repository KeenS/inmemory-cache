(in-package :cl-user)
(defpackage inmemory-cache-test
  (:use :cl
        :prove
        :inmemory-cache.util
        :inmemory-cache))
(in-package :inmemory-cache-test)


(plan nil)
(let ((cache (make-cache (* 1 +kilo+)))
      (key (map '(array (unsigned-byte 8) (*)) #'char-code "key"))
      (value (map '(array (unsigned-byte 8) (*)) #'char-code "value"))
      (expire (+ (get-universal-time) 60))
      entry)
  (put-cache cache key value expire)
  (setf entry (get-cache cache key))
  (is (cache-entry-key entry) key :test #'equalp)
  (is (cache-entry-value entry) value :test #'equalp)
  (is (cache-entry-expire entry) expire :test #'equalp))

(let ((cache (make-cache (* 1 +kilo+)))
      (key (map '(array (unsigned-byte 8) (*)) #'char-code "key"))
      (value (map '(array (unsigned-byte 8) (*)) #'char-code "value"))
      (value2 (map '(array (unsigned-byte 8) (*)) #'char-code "value2"))
      (expire (+ (get-universal-time) 60))
      entry)
  (put-cache cache key value expire)
  (put-cache cache key value2 expire)
  (setf entry (get-cache cache key))
  (is (cache-entry-key entry) key :test #'equalp)
  (is (cache-entry-value entry) value2 :test #'equalp)
  (is (cache-entry-expire entry) expire :test #'equalp))

(let ((cache (make-cache (* 1 +kilo+)))
      (key (map '(array (unsigned-byte 8) (*)) #'char-code "key"))
      (value (map '(array (unsigned-byte 8) (*)) #'char-code "value"))
      (expire (- (get-universal-time) 60))
      entry)
  (put-cache cache key value expire)
  (setf entry (get-cache cache key))
  (is entry nil))

(finalize)
