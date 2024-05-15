# lack-middleware-http-methods

> Responds with 501: Not Implemented when the request method is unsupported by the server

## Usage

Wrap app:

```lisp
(funcall lack/middleware/http-methods:*lack-middleware-http-methods*
         *app*
         :methods '(:GET :HEAD :POST :PUT :PATCH :DELETE :OPTIONS))
```

Lack Builder:

```lisp
(lack:builder
 (:http-methods :methods '(:GET :HEAD :POST :PUT :PATCH :DELETE :OPTIONS))
 *app*)
```

Custom handler as list:

```lisp
(lack:builder
 (:http-methods :methods '(:GET :HEAD :POST :PUT :PATCH :DELETE :OPTIONS)
                :on-unknown `(501
                              (:content-type "application/json"
                               :content-length 32)
                              ("{\"http_error\":\"not_implemented\"}")))
 *app*)
```

Custom handler as function:

```lisp
(lack:builder
 (:http-methods :methods '(:GET :HEAD :POST :PUT :PATCH :DELETE :OPTIONS)
                :on-unknown #'(lambda (env)
                                (declare (ignore env))
                                `(501
                                  (:content-type "application/json"
                                   :content-length 32)
                                  ("{\"http_error\":\"not_implemented\"}"))))
 *app*)
```

Methods `HEAD` and `GET` are configured by default as these are required to
be supported by HTTP servers, but can be provided in the `methods` parameter.

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.lack-middleware-http-methods)
```

## Installation

Not in Quicklisp, so clone to "local-projects/".

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
