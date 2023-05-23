(defpackage :dependently-typed-functions
  (:use :extensible-compound-types-cl)
  (:import-from #:alexandria #:parse-body #:with-gensyms)
  (:import-from :extensible-compound-types
                #:typexpand
                #:specializing
                #:extype
                #:upgraded-cl-type)
  (:import-from :extensible-compound-types.impl
                #:simplify-and-type
                #:simplify-or-type)
  (:export #:def-dept-fun
           #:dependently-typed-function
           #:infer-return-type
           #:infer-return-type*))
