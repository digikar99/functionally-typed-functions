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
        ;; FIXME: This only handles required and optional arguments
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

(defun type-lambda-call-form (fun-sym lambda-list)
  (flet ((wrap-in-eql (parameter)
           `(list 'eql ,parameter)))
    (if (member '&optional lambda-list)
        (let* ((optional-position (position '&optional lambda-list))
               (required-parameters (subseq lambda-list 0 optional-position))
               (optional-parameters (subseq lambda-list (1+ optional-position))))
          `(cond ,@(loop :for (name default supplied-p) :in (reverse optional-parameters)
                         :for optional-idx :downfrom (length optional-parameters) :above 0
                         :for parameters := (append required-parameters
                                                    (mapcar #'first
                                                            (subseq optional-parameters
                                                                    0 optional-idx)))
                         :collect `(,supplied-p
                                    (funcall ,fun-sym
                                             ,@(mapcar #'wrap-in-eql parameters))))
                 (t
                  (funcall ,fun-sym ,@(mapcar #'wrap-in-eql required-parameters)))))
        `(funcall ,fun-sym ,@(mapcar #'wrap-in-eql lambda-list)))))
