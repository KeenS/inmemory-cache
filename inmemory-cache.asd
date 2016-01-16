#|
  This file is a part of inmemory-cache project.
  Copyright (c) 2015 κeen
|#

#|
  Author: κeen
|#

(in-package :cl-user)
(defpackage inmemory-cache-asd
  (:use :cl :asdf))
(in-package :inmemory-cache-asd)

(defsystem inmemory-cache
  :version "0.1"
  :author "κeen"
  :license "BSD"
  :depends-on (:babel
               :cl-annot)
  :components ((:module "src"
                :components
                ((:file "inmemory-cache" :depends-on ("util" "message-pack"))
                 (:file "message-pack" :depends-on ("util"))
                 (:file "util"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op inmemory-cache-test))))
