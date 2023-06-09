(in-package :functionally-typed-functions)

(defclass functionally-typed-function ()
  ((name :initarg :name
         :reader typed-fun-name)
   (documentation :initarg :documentation
                  :type (or string null)
                  :accessor typed-fun-documentation)
   (source :initarg :source :reader typed-fun-source)
   (lambda-list :initarg :lambda-list :type list
                :initform (error "LAMBDA-LIST must be supplied.")
                :reader typed-fun-lambda-list)
   (type-lambda :initarg :type-lambda
                :reader typed-fun-type-lambda)
   (lambda-expr :initarg :lambda-expr :reader typed-fun-lambda-expr)
   #+sbcl (%lock
           :initform (sb-thread:make-mutex :name "GF lock")
           :reader sb-pcl::gf-lock))
  (:metaclass closer-mop:funcallable-standard-class))

(defmacro def-typed-fun (name lambda-list type-computing-form &body body)
  "Define a FUNCTIONALLY-TYPED-FUNCTION with NAME

TYPE-COMPUTING-FORM should take the same arguments as specified by the
LAMBDA-LIST however, the TYPE-COMPUTING-FORM will be called with the types
of the arguments at compile time or run time.

BODY will be called with the arguments themselves."
  (assert (subsetp (intersection lambda-list lambda-list-keywords)
                   '(&optional &key &rest))
          ()
          "Currently FUNCTIONALLY-TYPED-FUNCTION supports only required, optional,
rest, and keyword arguments")
  (when (and (member '&key lambda-list)
             (not (member '&rest lambda-list)))
    (let ((key-position (position '&key lambda-list)))
      (setq lambda-list (append (subseq lambda-list 0 key-position)
                                (list '&rest (gensym "ARGS"))
                                (subseq lambda-list key-position)))))
  (multiple-value-bind (rem-forms decl doc-string)
      (parse-body body :documentation t)
    (let ((type-lambda `(cl:lambda ,lambda-list
                          ,(ignorable-form-from-lambda-list lambda-list)
                          ,type-computing-form)))
      (with-gensyms (return-type i rv return-values type-lambda-sym)
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (fdefinition ',name)
                 (make-instance 'functionally-typed-function
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
           (setf (compiler-macro-function ',name) #'typed-fun-compiler-macro)
           ',name)))))

(defun infer-return-type (typed-fun-designator &rest argument-types)
  (apply (typed-fun-type-lambda (etypecase typed-fun-designator
                                  ((or list symbol) (fdefinition typed-fun-designator))
                                  (functionally-typed-function typed-fun-designator)))
         argument-types))

(defun infer-return-type* (typed-fun-designator
                           positional-arguments
                           &optional keyword-arguments)
  (apply (typed-fun-type-lambda (etypecase typed-fun-designator
                                  ((or list symbol) (fdefinition typed-fun-designator))
                                  (functionally-typed-function typed-fun-designator)))
         (nconc (loop :for arg :in positional-arguments
                      :collect `(eql ,arg))
                (loop :for arg :in keyword-arguments
                      :for i :from 0
                      :collect (if (evenp i)
                                   arg
                                   `(eql ,arg))))))
