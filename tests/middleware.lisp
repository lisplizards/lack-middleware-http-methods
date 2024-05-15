;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/http-methods/tests)

(define-condition not-implemented-error (simple-error)
  ())

(defun foo (env)
  (ecase (getf env :request-method)
    (:GET `(200
            (:content-type "text/plain"
             :content-length 13)
            ("Hello, World.")))
    (:HEAD `(200
             (:content-type "text/plain"
              :content-length 13)
             ("")))))

(defun bar (env)
  (ecase (getf env :request-method)
    (:GET `(200
            (:content-type "text/plain"
             :content-length 13)
            ("Hello, World.")))
    (:HEAD `(200
             (:content-type "text/plain"
              :content-length 13)
             ("")))
    (:DELETE `(200
               (:content-type "application/octet-stream"
                :content-length 0)
               (#())))))

(deftest default-known-http-methods
    (testing
     "when no methods are configured, responds to GET and HEAD and responds 501 otherwise"
     (let ((app (funcall lack/middleware/http-methods:*lack-middleware-http-methods*
                         #'foo)))
       (let ((response (funcall app '(:request-method :GET))))
         (ok (equal response
                    `(200
                      (:content-type "text/plain"
                       :content-length 13)
                      ("Hello, World.")))))
       (let ((response (funcall app '(:request-method :HEAD))))
         (ok (equal response
                    `(200
                      (:content-type "text/plain"
                       :content-length 13)
                      ("")))))
       (let ((response (funcall app '(:request-method :POST))))
         (ok (equal response
                    `(501
                      (:content-type "text/plain"
                       :content-length 15)
                      ("Not Implemented")))))
       (let ((response (funcall app '(:request-method :PUT))))
         (ok (equal response
                    `(501
                      (:content-type "text/plain"
                       :content-length 15)
                      ("Not Implemented")))))
       (let ((response (funcall app '(:request-method :PATCH))))
         (ok (equal response
                    `(501
                      (:content-type "text/plain"
                       :content-length 15)
                      ("Not Implemented")))))
       (let ((response (funcall app '(:request-method :DELETE))))
         (ok (equal response
                    `(501
                      (:content-type "text/plain"
                       :content-length 15)
                      ("Not Implemented")))))
       (let ((response (funcall app '(:request-method :OPTIONS))))
         (ok (equal response
                    `(501
                      (:content-type "text/plain"
                       :content-length 15)
                      ("Not Implemented")))))
       (let ((response (funcall app '(:request-method :TRACE))))
         (ok (equal response
                    `(501
                      (:content-type "text/plain"
                       :content-length 15)
                      ("Not Implemented")))))
       (let ((response (funcall app '(:request-method :CONNECT))))
         (ok (equal response
                    `(501
                      (:content-type "text/plain"
                       :content-length 15)
                      ("Not Implemented"))))))))

(deftest methods-option
    (testing
     "registers GET and HEAD by default and allows registering methods with the 'methods' keyword argument"
     (let ((app (funcall lack/middleware/http-methods:*lack-middleware-http-methods*
                         #'bar
                         :methods '(:DELETE))))
       (let ((response (funcall app '(:request-method :GET))))
         (ok (equal response
                    `(200
                      (:content-type "text/plain"
                       :content-length 13)
                      ("Hello, World.")))))
       (let ((response (funcall app '(:request-method :HEAD))))
         (ok (equal response
                    `(200
                      (:content-type "text/plain"
                       :content-length 13)
                      ("")))))
       (let ((response (funcall app '(:request-method :DELETE))))
         (ok (equalp response
                     `(200
                       (:content-type "application/octet-stream"
                        :content-length 0)
                       (#())))))
       (let ((response (funcall app '(:request-method :POST))))
         (ok (equal response
                    `(501
                      (:content-type "text/plain"
                       :content-length 15)
                      ("Not Implemented"))))))))

(deftest on-unknown-option
    (testing
     "allows overriding the default 501 response with a user-provided Clack response list"
     (let ((app (funcall lack/middleware/http-methods:*lack-middleware-http-methods*
                         #'foo
                         :on-unknown `(501
                                       (:content-type "application/json"
                                        :content-length 32)
                                       ("{\"http_error\":\"not_implemented\"}")))))
       (let ((response (funcall app '(:request-method :PATCH))))
         (ok (equal response
                    `(501
                      (:content-type "application/json"
                       :content-length 32)
                      ("{\"http_error\":\"not_implemented\"}")))))))

  (testing
   "allows overriding the default 501 response with the result of a user-provided function"
   (let ((app (funcall lack/middleware/http-methods:*lack-middleware-http-methods*
                       #'foo
                       :methods '(:POST)
                       :on-unknown #'(lambda (env)
                                       (declare (ignore env))
                                       `(501
                                         (:content-type "text/plain"
                                          :content-length 20)
                                         ("501: Not Implemented"))))))
     (let ((response (funcall app '(:request-method :PATCH))))
       (ok (equal response
                  `(501
                    (:content-type "text/plain"
                     :content-length 20)
                    ("501: Not Implemented"))))))

   (let ((app (funcall lack/middleware/http-methods:*lack-middleware-http-methods*
                       #'foo
                       :on-unknown #'(lambda (env)
                                       (declare (ignore env))
                                       (signal 'not-implemented-error)))))
     (ok (signals (funcall app '(:request-method :PATCH))
                  'not-implemented-error)))))
