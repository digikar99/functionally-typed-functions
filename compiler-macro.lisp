(in-package :functionally-typed-functions)

(defun enhance-lambda-declarations (lambda-list arg-types)
  (let* ((rest-position (position '&rest lambda-list))
         (non-rest-parameters (remove-if (lambda (p)
                                           (member p lambda-list-keywords))
                                         (if rest-position
                                             (subseq lambda-list 0 rest-position)
                                             lambda-list))))
    (loop :for arg  :in non-rest-parameters
          :for type :in arg-types
          :nconc `((extype ,type ,arg)
                   (cl:type ,(upgraded-cl-type type) ,arg)))))

(defun typed-fun-compiler-macro-function (form &optional env)

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
           (typed-fun   (fdefinition name))
           (return-type (apply (typed-fun-type-lambda typed-fun) arg-types))
           (lambda-expr (optima:ematch (typed-fun-lambda-expr typed-fun)
                          ((list* 'cl:lambda lambda-list body)
                           `(cl:lambda ,lambda-list
                              (declare ,@(enhance-lambda-declarations
                                          lambda-list arg-types))
                              ,@body)))))

      (if return-type
          `(the ,return-type
                (,lambda-expr ,@args))
          form))))

(defun typed-fun-compiler-macro (form &optional env)
  (typed-fun-compiler-macro-function form env))
