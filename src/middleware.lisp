;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/http-methods)

(deftype keyword-list ()
  `(and list
        (satisfies keyword-list-p)))

(defparameter *lack-middleware-http-methods*
  #'(lambda (app &key methods on-unknown)
      (declare (optimize (speed 0) (safety 3) (debug 3))
               (type function app)
               (type keyword-list methods)
               (type (or list function) on-unknown))
      (check-type methods list)
      (check-type on-unknown (or list function))
      (dolist (method methods)
        (check-type method keyword))
      (dolist (method '(:GET :HEAD))
        (unless (member method methods)
          (setq methods `(,method ,@methods))))
      (assert (= (length methods)
                 (length (remove-duplicates methods :test #'eq)))
              nil
              "HTTP method list contains duplicate items")
      #'(lambda (env)
          (declare (optimize (speed 3) (safety 0) (debug 0))
                   (type list env))
          (if (member (getf env :request-method) methods :test #'eq)
              (funcall app env)
              (etypecase on-unknown
                (null `(501 (:content-type "text/plain"
                             :content-length 15)
                            ("Not Implemented")))
                (list on-unknown)
                (function (funcall on-unknown env)))))))

(defun keyword-list-p (lst)
  (declare (type list lst))
  (every #'keywordp lst))
