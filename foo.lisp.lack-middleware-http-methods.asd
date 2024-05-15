;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.lack-middleware-http-methods"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/lack-middleware-http-methods"
  :bug-tracker "https://github.com/lisplizards/lack-middleware-http-methods/issues"
  :source-control (:git "https://github.com/lisplizards/lack-middleware-http-methods.git")
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "middleware" :depends-on ("package"))
                 (:file "package"))))
  :description "Lack middleware to respond 501 when an HTTP request method is unsupported by the server"
  :in-order-to ((test-op (test-op "foo.lisp.lack-middleware-http-methods/tests"))))

(defsystem "foo.lisp.lack-middleware-http-methods/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.lack-middleware-http-methods"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "middleware" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.lack-middleware-http-methods"
  :perform (test-op (op c) (symbol-call :rove :run c)))
