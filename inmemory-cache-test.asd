#|
  This file is a part of inmemory-cache project.
  Copyright (c) 2015 κeen
|#

(in-package :cl-user)
(defpackage inmemory-cache-test-asd
  (:use :cl :asdf))
(in-package :inmemory-cache-test-asd)

(defsystem inmemory-cache-test
  :author "κeen"
  :license "BSD"
  :depends-on (:inmemory-cache
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "inmemory-cache")
                 (:test-file "message-pack"))))
  :description "Test system for inmemory-cache"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
