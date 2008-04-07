;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defclass application-form (form)
  ((operator :accessor operator-of :initarg :operator)
   (arguments :accessor arguments-of :initarg :arguments)))

(defunwalker-handler application-form (operator arguments)
  (cons operator (unwalk-forms arguments)))

(defclass lexical-application-form (application-form)
  ((code :accessor code-of :initarg :code)))

(defclass walked-lexical-application-form (lexical-application-form)
  ())

(defclass unwalked-lexical-application-form (lexical-application-form)
  ())

(defclass free-application-form (application-form)
  ())

(defclass lambda-application-form (application-form)
  ())

(defunwalker-handler lambda-application-form (operator arguments)
  ;; The cadr is for getting rid of (function ...) which we can't have
  ;; at the beginning of a form.
  (cons (cadr (unwalk-form operator)) (unwalk-forms arguments)))

(defwalker-handler application (form parent env)
  (block nil
    (destructuring-bind (op &rest args)
        form
      (when (and (consp op)
                 (eq 'cl:lambda (car op)))
        (return
          (with-form-object (application lambda-application-form :parent parent :source form)
            (setf (operator-of application) (walk-form op application env)
                  (arguments-of application) (mapcar (lambda (form)
                                                       (walk-form form application env))
                                                     args)))))
      (awhen (lookup-in-walkenv :macro op env)
        (return (walk-form (funcall it form (cdr env)) parent env)))
      (when (and (symbolp op) (macro-function op))
        (multiple-value-bind (expansion expanded)
            (macroexpand-1 form (cdr env))
          (when expanded
            (return (walk-form expansion parent env)))))
      (let ((app (aif (lookup-in-walkenv :function op env)
                      (make-instance 'walked-lexical-application-form :code it)
                      (if (lookup-in-walkenv :unwalked-function op env)
                          (make-instance 'unwalked-lexical-application-form)
                          (progn
                            (when (and *warn-undefined*
                                       (symbolp op)
                                       (not (fboundp op)))
                              (warn 'undefined-function-reference :name op))
                            (make-instance 'free-application-form))))))
        (setf (operator-of app) op
              (parent-of app) parent
              (source-of app) form
              (arguments-of app) (mapcar (lambda (form)
                                           (walk-form form app env))
                                         args))
        app))))

;;;; Functions

(defclass function-form (form)
  ())

(defclass lambda-function-form (function-form implicit-progn-with-declare-mixin)
  ((arguments :accessor arguments-of :initarg :arguments)))

(defunwalker-handler lambda-function-form (arguments body declares)
  `(function
    (lambda ,(unwalk-lambda-list arguments)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body))))

(defclass function-object-form (form)
  ((name :accessor name-of :initarg :name)))

(defunwalker-handler function-object-form (name)
  `(function ,name))

(defclass lexical-function-object-form (function-object-form)
  ())

(defclass walked-lexical-function-object-form (lexical-function-object-form)
  ())

(defclass unwalked-lexical-function-object-form (lexical-function-object-form)
  ())

(defclass free-function-object-form (function-object-form)
  ())

(defwalker-handler function (form parent env)
  (if (and (listp (second form))
           (eql 'cl:lambda (first (second form))))
      ;; (function (lambda ...))
      (walk-lambda (second form) parent env)
      ;; (function foo)
      (make-instance (if (lookup-in-walkenv :function (second form) env)
                         'walked-lexical-function-object-form
                         (if (lookup-in-walkenv :unwalked-function (second form) env)
                             'unwalked-lexical-function-object-form
                             'free-function-object-form))
                     :name (second form)
                     :parent parent :source form)))

(defun walk-lambda (form parent env)
  (with-form-object (func lambda-function-form
                          :parent parent
                          :source form)
    ;; 1) parse the argument list creating a list of FUNCTION-ARGUMENT-FORM objects
    (multiple-value-setf ((arguments-of func) env)
      (walk-lambda-list (second form) func env))
    ;; 2) parse the body
    (multiple-value-setf ((body-of func) _ (declares func))
      (walk-implict-progn func (cddr form) env :declare t))
    ;; all done
    func))

(defun walk-lambda-list (lambda-list parent env &key allow-specializers macro-p)
  (declare (ignore macro-p))
  (let ((result (list)))
    (flet ((extend-env (argument)
             (unless (typep argument 'allow-other-keys-function-argument-form)
               (augment-walkenv! env :variable (name-of argument) argument))))
      (parse-lambda-list lambda-list
                         (lambda (kind name argument)
                           (declare (ignore name))
                           (let ((parsed
                                  (case kind
                                    ((nil)
                                     (if allow-specializers
                                         (walk-specialized-argument-form argument parent env)
                                         (make-instance 'required-function-argument-form
                                                        :name argument :parent parent :source argument)))
                                    (&optional
                                     (walk-optional-argument argument parent env))
                                    (&allow-other-keys
                                     (make-instance 'allow-other-keys-function-argument-form
                                                    :parent parent :source nil))
                                    (&rest (make-instance 'rest-function-argument-form :name argument
                                                          :parent parent :source argument))
                                    (&key
                                     (walk-keyword-argument argument parent env)))))
                             (when parsed
                               (push parsed result)
                               (extend-env parsed)))))
      (values (nreverse result) env))))

(defclass function-argument-form (form)
  ((name :accessor name-of :initarg :name)))

(defmethod print-object ((argument function-argument-form) stream)
  (print-unreadable-object (argument stream :type t :identity t)
    (if (slot-boundp argument 'name)
        (format stream "~S" (name-of argument))
        (write-string "#<unbound name>" stream))))

(defclass required-function-argument-form (function-argument-form)
  ())

(defunwalker-handler required-function-argument-form (name)
  name)

(defclass specialized-function-argument-form (required-function-argument-form)
  ((specializer :accessor specializer-of :initarg :specializer)))

(defun walk-specialized-argument-form (form parent env)
  (declare (ignore env))
  (make-instance 'specialized-function-argument-form
                 :name (if (listp form)
                           (first form)
                           form)
                 :specializer (if (listp form)
                                  (second form)
                                  t)
                 :parent parent
                 :source form))

(defunwalker-handler specialized-function-argument-form (name specializer)
  (if (eq specializer t)
      name
      `(,name ,specializer)))

(defclass optional-function-argument-form (function-argument-form)
  ((default-value :accessor default-value-of :initarg :default-value)
   (supplied-p-parameter :accessor supplied-p-parameter :initarg :supplied-p-parameter)))

(defun walk-optional-argument (form parent env)
  (destructuring-bind (name &optional default-value supplied-p-parameter)
      (ensure-list form)
    (with-form-object (arg optional-function-argument-form
                           :parent parent
                           :source form
                           :name name
                           :supplied-p-parameter supplied-p-parameter)
      (setf (default-value-of arg) (walk-form default-value arg env)))))

(defunwalker-handler optional-function-argument-form (name default-value supplied-p-parameter)
  (let ((default-value (unwalk-form default-value)))
    (cond ((and name default-value supplied-p-parameter)
           `(,name ,default-value ,supplied-p-parameter))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid optional argument")))))

(defclass keyword-function-argument-form (function-argument-form)
  ((keyword-name :accessor keyword-name-of :initarg :keyword-name)
   (default-value :accessor default-value-of :initarg :default-value)
   (supplied-p-parameter :accessor supplied-p-parameter :initarg :supplied-p-parameter)))

(defun effective-keyword-name-of (k)
  (or (keyword-name-of k)
      (intern (symbol-name (name-of k)) :keyword)))

(defun walk-keyword-argument (form parent env)
  (destructuring-bind (name &optional default-value supplied-p-parameter)
      (ensure-list form)
    (let ((name (if (consp name)
                    (second name)
                    name))
          (keyword (if (consp name)
                       (first name)
                       nil)))
      (with-form-object (arg keyword-function-argument-form
                             :parent parent
                             :source form
                             :name name
                             :keyword-name keyword
                             :supplied-p-parameter supplied-p-parameter)
        (setf (default-value-of arg) (walk-form default-value arg env))))))

(defunwalker-handler keyword-function-argument-form (keyword-name name default-value supplied-p-parameter)
  (let ((default-value (unwalk-form default-value)))
    (cond ((and keyword-name name default-value supplied-p-parameter)
           `((,keyword-name ,name) ,default-value ,supplied-p-parameter))
          ((and name default-value supplied-p-parameter)
           `(,name ,default-value ,supplied-p-parameter))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid keyword argument")))))

(defclass allow-other-keys-function-argument-form (function-argument-form)
  ())

(defunwalker-handler allow-other-keys-function-argument-form ()
  '&allow-other-keys)

(defclass rest-function-argument-form (function-argument-form)
  ())

(defunwalker-handler rest-function-argument-form (name)
  name)

(defun unwalk-lambda-list (arguments)
  (let (optional-p rest-p keyword-p)
    (mapcan #'(lambda (form)
                (append
                 (typecase form
                   (optional-function-argument-form
                    (unless optional-p
                      (assert (not keyword-p))
                      (assert (not rest-p))
                      (setq optional-p t)
                      '(&optional)))
                   (rest-function-argument-form
                    (unless rest-p
                      (assert (not keyword-p))
                      (setq rest-p t)
                      '(&rest)))
                   (keyword-function-argument-form
                    (unless keyword-p
                      (setq keyword-p t)
                      '(&key))))
                 (list (unwalk-form form))))
            arguments)))

;;;; FLET/LABELS

(defclass function-binding-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass flet-form (function-binding-form)
  ())

(defwalker-handler flet (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (flet flet-form :parent parent :source form)
      ;; build up the objects for the bindings in the original env
      (loop
         :for (name args . body) :in binds
         :collect (cons name (walk-form `(lambda ,args ,@body) flet env)) :into bindings
         :finally (setf (bindings-of flet) bindings))
      ;; walk the body in the new env
      (multiple-value-setf ((body-of flet) _ (declares flet))
        (walk-implict-progn flet
                            body
                            (loop
                               :with env = env
                               :for (name . lambda) :in (bindings-of flet)
                               :do (augment-walkenv! env :function name lambda)
                               :finally (return env))
                            :declare t)))))

;; TODO factor out stuff in flet-form and labels-form
(defunwalker-handler flet-form (bindings body declares)
  `(flet ,(mapcar (lambda (bind)
                    (cons (car bind)
                          ;; remove (function (lambda ...)) of the function bindings
                          (cdadr (unwalk-form (cdr bind)))))
                  bindings)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

(defclass labels-form (function-binding-form)
  ())

(defwalker-handler labels (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (labels labels-form :parent parent :source form :bindings '())
      ;; we need to walk over the bindings twice. the first pass
      ;; creates some 'empty' lambda objects in the environment so
      ;; that walked-lexical-application-form and walked-lexical-function-object-form
      ;; have something to point to. the second pass then walks the
      ;; actual bodies of the form filling in the previously created
      ;; objects.
      (loop
         :for (name arguments . body) :in binds
         :for lambda = (make-instance 'lambda-function-form
                                      :parent labels
                                      :source (list* name arguments body))
         :do (progn
               (push (cons name lambda) (bindings-of labels))
               (augment-walkenv! env :function name lambda)))
      (setf (bindings-of labels) (nreverse (bindings-of labels)))
      (loop
         :for form :in binds
         :for (arguments . body) = (cdr form)
         :for binding :in (bindings-of labels)
         :for lambda = (cdr binding)
         :for tmp-lambda = (walk-lambda `(lambda ,arguments ,@body) labels env)
         :do (setf (body-of lambda) (body-of tmp-lambda)
                   (arguments-of lambda) (arguments-of tmp-lambda)
                   (declares lambda) (declares tmp-lambda)))
      (multiple-value-setf ((body-of labels) _ (declares labels))
        (walk-implict-progn labels body env :declare t)))))

(defunwalker-handler labels-form (bindings body declares)
  `(labels ,(mapcar (lambda (bind)
                      (cons (car bind)
                            ;; remove (function (lambda ...)) of the function bindings
                            (cdadr (unwalk-form (cdr bind)))))
                    bindings)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

