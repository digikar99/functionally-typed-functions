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
  (assert (subsetp (intersection lambda-list lambda-list-keywords) '(&optional))
          ()
          "Currently DEPENDENTLY-TYPED-FUNCTION supports only required and optional arguments")
  (multiple-value-bind (rem-forms decl doc-string)
      (parse-body body :documentation t)
    (let ((type-lambda `(cl:lambda ,lambda-list
                          ,(ignorable-form-from-lambda-list lambda-list)
                          ,type-computing-form)))
      (with-gensyms (return-type i rv return-values type-lambda-sym)
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (fdefinition ',name)
                 (make-instance 'dependently-typed-function
                                :name ',name
                                :documentation ,doc-string
                                :source #+sbcl (sb-c:source-location) #-sbcl nil
                                :lambda-list ',lambda-list
                                :lambda-expr '(cl:lambda ,lambda-list ,@decl ,@rem-forms)
                                :type-lambda ,type-lambda))
           (closer-mop:set-funcallable-instance-function
            (fdefinition ',name)
            (cl:lambda ,lambda-list
              ,@decl
              (let* ((,type-lambda-sym ,type-lambda)
                     (,return-type
                       ,(type-lambda-call-form type-lambda-sym lambda-list)))
                (assert ,return-type
                        ()
                        'simple-type-error
                        :format-control
                        "Arguments~%  ~S~%do not satisfy type signature~%  ~S"
                        :format-arguments
                        (list (list ,@(user-parameters-from-lambda-list lambda-list))
                              ',type-computing-form))
                (let ((,return-values (multiple-value-list (locally ,@rem-forms))))
                  (loop :for ,rv :in ,return-values
                        :for ,i :from 0
                        :do (assert (typep ,rv (cl-form-types:nth-value-type ,return-type ,i))
                                    ()))
                  (values-list ,return-values)))))
           (setf (compiler-macro-function ',name) #'dept-fun-compiler-macro)
           ',name)))))

(defun infer-return-type (dept-fun-designator &rest argument-types)
  (apply (dept-fun-type-lambda (etypecase dept-fun-designator
                                 ((or list symbol) (fdefinition dept-fun-designator))
                                 (dependently-typed-function dept-fun-designator)))
         argument-types))
