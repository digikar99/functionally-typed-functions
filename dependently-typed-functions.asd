(defsystem "dependently-typed-functions"
  :depends-on ("alexandria"
               "closer-mop"
               "optima"
               "cl-form-types"
               "extensible-compound-types")
  :components ((:file "package")
               (:file "utils")
               (:file "compiler-macro")
               (:file "dependently-typed-functions")))
