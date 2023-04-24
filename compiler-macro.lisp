(in-package :dependently-typed-functions)

(defun enhance-lambda-declarations (lambda-list arg-types)
  (loop :for arg  :in lambda-list
        :for type :in arg-types
        :collect `(type ,type ,arg)))

(defun dept-fun-compiler-macro-function (form &optional env)

  (multiple-value-bind (name args)
      (if (eq 'funcall (car form))
          (values (optima:match (second form)
                    ((list 'cl:function name)
                     name)
                    (variable
                     variable))
                  (rest (rest form)))
          (values (first form)
                  (rest form)))

    (let* ((arg-types   (loop :for arg :in args
                              :collect (cl-form-types:nth-form-type arg env 0 t t)))
           (dept-fun    (fdefinition name))
           (return-type (apply (dept-fun-type-lambda dept-fun) arg-types))
           (lambda-expr (optima:ematch (dept-fun-lambda-expr dept-fun)
                          ((list* 'cl:lambda lambda-list body)
                           `(cl:lambda ,lambda-list
                              (declare ,@(enhance-lambda-declarations
                                          lambda-list arg-types))
                              ,@body)))))

      (if return-type
          `(the ,return-type
                (,lambda-expr ,@args))
          form))))

(defun dept-fun-compiler-macro (form &optional env)
  (dept-fun-compiler-macro-function form env))