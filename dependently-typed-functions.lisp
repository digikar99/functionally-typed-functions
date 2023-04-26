(in-package :dependently-typed-functions)

(defclass dependently-typed-function ()
  ((name :initarg :name
         :reader dept-fun-name)
   (documentation :initarg :documentation
                  :type (or string null)
                  :accessor dept-fun-documentation)
   (source :initarg :source :reader dept-fun-source)
   (lambda-list :initarg :lambda-list :type list
                :initform (error "LAMBDA-LIST must be supplied.")
                :reader dept-fun-lambda-list)
   (type-lambda :initarg :type-lambda
                :reader dept-fun-type-lambda)
   (lambda-expr :initarg :lambda-expr :reader dept-fun-lambda-expr)
   #+sbcl (%lock
           :initform (sb-thread:make-mutex :name "GF lock")
           :reader sb-pcl::gf-lock))
  (:metaclass closer-mop:funcallable-standard-class))

(defmacro def-dept-fun (name lambda-list type-computing-form &body body)
  "Define a DEPENDENTLY-TYPED-FUNCTION with NAME

TYPE-COMPUTING-FORM should take the same arguments as specified by the
LAMBDA-LIST however, the TYPE-COMPUTING-FORM will be called with the types
of the arguments at compile time or run time.

BODY will be called with the arguments themselves."
  (assert (null (intersection lambda-list lambda-list-keywords))
          ()
          "Currently DEPENDENTLY-TYPED-FUNCTION supports only required arguments")
  (multiple-value-bind (rem-forms decl doc-string)
      (parse-body body :documentation t)
    (with-gensyms (return-type return-value)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (fdefinition ',name)
               (make-instance 'dependently-typed-function
                              :name ',name
                              :documentation ,doc-string
                              :source #+sbcl (sb-c:source-location) #-sbcl nil
                              :lambda-list ',lambda-list
                              :lambda-expr '(cl:lambda ,lambda-list ,@decl ,@rem-forms)
                              :type-lambda (cl:lambda ,lambda-list
                                             ,type-computing-form)))
         (closer-mop:set-funcallable-instance-function
          (fdefinition ',name)
          (cl:lambda ,lambda-list
            ,@decl
            (let ((,return-type
                    ((cl:lambda ,lambda-list
                       ,type-computing-form)
                     ,@(loop :for arg :in lambda-list
                             :collect `(list 'eql ,arg)))))
              (assert ,return-type
                      ()
                      'simple-type-error
                      :format-control "Arguments~%  ~S~%do not satisfy type signature~%  ~S"
                      :format-arguments (list (list ,@lambda-list)
                                              ',type-computing-form))
              (let ((,return-value (locally ,@rem-forms)))
                (assert (typep ,return-value ,return-type) ())
                ,return-value))))
         (setf (compiler-macro-function ',name) #'dept-fun-compiler-macro)
         ',name))))

(defun infer-return-type (dept-fun-designator &rest argument-types)
  (apply (dept-fun-type-lambda (etypecase dept-fun-designator
                                 ((or list symbol) (fdefinition dept-fun-designator))
                                 (dependently-typed-function dept-fun-designator)))
         argument-types))
