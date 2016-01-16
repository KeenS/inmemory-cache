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
      (expire (+ (get-universal-time) 60)))
  (put-cache cache key value expire)
  (multiple-value-bind (res-value res-expire) (get-cache cache key)
   (is res-value value :test #'equalp)
   (is res-expire expire :test #'equalp)))

(let ((cache (make-cache (* 1 +kilo+)))
      (key (map '(array (unsigned-byte 8) (*)) #'char-code "key"))
      (value (map '(array (unsigned-byte 8) (*)) #'char-code "value"))
      (value2 (map '(array (unsigned-byte 8) (*)) #'char-code "value2"))
      (expire (+ (get-universal-time) 60)))
  (put-cache cache key value expire)
  (put-cache cache key value2 expire)
  (multiple-value-bind (res-value res-expire) (get-cache cache key)
   (is res-value value2 :test #'equalp)
   (is res-expire expire :test #'equalp)))

(let ((cache (make-cache (* 1 +kilo+)))
      (key (map '(array (unsigned-byte 8) (*)) #'char-code "key"))
      (value (map '(array (unsigned-byte 8) (*)) #'char-code "value"))
      (expire (- (get-universal-time) 60)))
  (put-cache cache key value expire)
  (multiple-value-bind (res-value res-expire) (get-cache cache key)
      (is res-value nil)
      (is res-expire nil)))

(finalize)
