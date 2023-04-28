(in-package #:dependently-typed-functions)

(defun ignorable-form-from-lambda-list (lambda-list)
  (let ((vars (loop :for state := 'required
                    :for form :in lambda-list
                    :if (member form (cons 'required lambda-list-keywords))
                      :do (setq state form)
                    :else
                      :appending (etypecase form
                                   (symbol (list form))
                                   (list (ecase (length form)
                                           (1 (list (first form)))
                                           (2 (list (first form)))
                                           (3 (list (first form)
                                                    (third form)))))))))
    `(declare (ignorable ,@vars))))

(defun parameters-from-lambda-list (lambda-list &optional collect-keywords)
  (loop :for state := 'required
        :for form :in lambda-list
        :nconcing (if (member form lambda-list-keywords)
                      (if collect-keywords
                          (list form)
                          nil)
                      (etypecase form
                        (symbol (list form))
                        (list (ecase (length form)
                                (1 (list (first form)))
                                (2 (list (first form)))
                                (3 (list (first form)
                                         (third form)))))))))

(defun user-parameters-from-lambda-list (lambda-list)
  (loop :for state := 'required
        :for form :in lambda-list
        :if (member form (cons 'required lambda-list-keywords))
          :do (setq state form)
        :else
          :appending (etypecase form
                       (symbol (list form))
                       (list (ecase (length form)
                               (1 (list (first form)))
                               (2 (list (first form)))
                               (3 (list (first form))))))))

(declaim (inline wrap-in-eql/compile-time
                 wrap-in-eql/run-time/rest
                 wrap-in-eql/run-time/key))
(defun wrap-in-eql/compile-time (parameter) `(list 'eql ,parameter))
(defun wrap-in-eql/run-time/rest (parameters)
  (loop :for parameter :in parameters :collect `(eql ,parameter)))
(defun wrap-in-eql/run-time/key (parameters)
  (print
   (loop :for parameter :in parameters
         :for i :from 0
         :collect (if (evenp i)
                      parameter
                      `(eql ,parameter)))))

(defun type-lambda-call-form (fun-sym lambda-list)
  (let* ((rest-position  (position '&rest lambda-list))
         (rest-parameter (when rest-position
                           (nth (1+ rest-position) lambda-list)))
         (key-position   (position '&key lambda-list)))
    (if (member '&optional lambda-list)
        (let* ((optional-position   (position '&optional lambda-list))
               (required-parameters (subseq lambda-list 0 optional-position))
               (optional-parameters (subseq lambda-list (1+ optional-position)
                                            (or rest-position (length lambda-list)))))
          `(cond ,@(loop :for (name default supplied-p) :in (reverse optional-parameters)
                         :for optional-idx :downfrom (length optional-parameters) :above 0
                         :for parameters := (append required-parameters
                                                    (mapcar #'first
                                                            (subseq optional-parameters
                                                                    0 optional-idx)))
                         :collect `(,supplied-p
                                    (apply ,fun-sym
                                           ,@(mapcar #'wrap-in-eql/compile-time
                                                     parameters)
                                           ,rest-parameter)))
                 (t
                  (apply ,fun-sym ,@(mapcar #'wrap-in-eql/compile-time
                                            required-parameters)
                         ,rest-parameter))))
        `(apply ,fun-sym ,@(mapcar #'wrap-in-eql/compile-time
                                   (subseq lambda-list 0
                                           (or rest-position (length lambda-list))))
                ,(if key-position
                     `(wrap-in-eql/run-time/key  ,rest-parameter)
                     `(wrap-in-eql/run-time/rest ,rest-parameter))))))
