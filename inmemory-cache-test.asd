#|
  This file is a part of inmemory-cache project.
  Copyright (c) 2015 ?een
|#

(in-package :cl-user)
(defpackage inmemory-cache-test-asd
  (:use :cl :asdf))
(in-package :inmemory-cache-test-asd)

(defsystem inmemory-cache-test
  :author "?een"
  :license "BSD"
  :depends-on (:inmemory-cache
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "inmemory-cache"))))
  :description "Test system for inmemory-cache"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
