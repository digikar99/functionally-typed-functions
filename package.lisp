(defpackage :dependently-typed-functions
  (:use :cl :alexandria)
  (:import-from :extensible-compound-types
                #:typexpand
                #:specializing)
  (:import-from :extensible-compound-types.impl
                #:simplify-and-type)
  (:export #:def-dept-fun
           #:dependently-typed-function
           #:infer-return-type
           #:infer-return-type*))
