;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

;;;; Atoms

(defclass constant-form (form)
  ((value :accessor value-of :initarg :value)))

(defunwalker-handler constant-form (value)
  (typecase value
    (symbol `(quote ,value))
    (cons   `(quote ,value))
    (t value)))

(defclass variable-reference (form)
  ((name :accessor name-of :initarg :name)))

(defunwalker-handler variable-reference (name)
  name)

(defmethod print-object ((v variable-reference) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~S" (name-of v))))

(defclass local-variable-reference (variable-reference)
  ())

(defclass local-lexical-variable-reference (local-variable-reference)
  ()
  (:documentation "A reference to a local variable defined in the
  lexical environment outside of the form passed to walk-form."))

(defclass free-variable-reference (variable-reference)
  ())

(defwalker-handler +atom-marker+ (form parent env)
  (cond
    ((not (or (symbolp form) (consp form)))
     (make-instance 'constant-form :value form
                    :parent parent :source form))
    ((lookup-walk-env env :let form)
     (make-instance 'local-variable-reference :name form
                    :parent parent :source form))
    ((lookup-walk-env env :lexical-let form)
     (make-instance 'local-lexical-variable-reference :name form
                    :parent parent :source form))
    ((lookup-walk-env env :symbol-macrolet form)
     (walk-form (lookup-walk-env env :symbol-macrolet form) parent env))
    ((nth-value 1 (macroexpand-1 form))
     ;; a globaly defined symbol-macro
     (walk-form (macroexpand-1 form) parent env))
    (t
     (when (and *warn-undefined*
                (not (boundp form)))
       (warn 'undefined-variable-reference :name form))
     (make-instance 'free-variable-reference :name form
                    :parent parent :source form))))

;;;; BLOCK/RETURN-FROM

(defclass block-form (form implicit-progn-mixin)
  ((name :accessor name-of :initarg :name)))

(defwalker-handler block (form parent env)
  (destructuring-bind (block-name &rest body)
      (cdr form)
    (with-form-object (block block-form
                       :parent parent :source form
                       :name block-name)
      (setf (body block) (walk-implict-progn block
                                             body
                                             (register-walk-env env :block block-name block))))))

(defunwalker-handler block-form (name body)
  `(block ,name ,@(unwalk-forms body)))

(defclass return-from-form (form)
  ((target-block :accessor target-block :initarg :target-block)
   (result :accessor result :initarg :result)))


(define-condition return-from-unknown-block (error)
  ((block-name :accessor block-name :initarg :block-name))
  (:report (lambda (condition stream)
             (format stream "Unable to return from block named ~S." (block-name condition)))))

(defwalker-handler return-from (form parent env)
  (destructuring-bind (block-name &optional (value '(values)))
      (cdr form)
    (if (lookup-walk-env env :block block-name)
        (with-form-object (return-from return-from-form :parent parent :source form
                           :target-block (lookup-walk-env env :block block-name))
          (setf (result return-from) (walk-form value return-from env)))
        (restart-case
            (error 'return-from-unknown-block :block-name block-name)
          (add-block ()
            :report "Add this block and continue."
            (walk-form form parent (register-walk-env env :block block-name :unknown-block)))))))

(defunwalker-handler return-from-form (target-block result)
  `(return-from ,(name-of target-block) ,(unwalk-form result)))

;;;; CATCH/THROW

(defclass catch-form (form implicit-progn-mixin)
  ((tag :accessor tag :initarg :tag)))

(defwalker-handler catch (form parent env)
  (destructuring-bind (tag &body body)
      (cdr form)
    (with-form-object (catch catch-form :parent parent :source form)
      (setf (tag catch) (walk-form tag catch env)
            (body catch) (walk-implict-progn catch body env)))))

(defunwalker-handler catch-form (tag body)
  `(catch ,(unwalk-form tag) ,@(unwalk-forms body)))

(defclass throw-form (form)
  ((tag :accessor tag :initarg :tag)
   (value :accessor value-of :initarg :value)))

(defwalker-handler throw (form parent env)
  (destructuring-bind (tag &optional (result '(values)))
      (cdr form)
    (with-form-object (throw throw-form :parent parent :source form)
      (setf (tag throw) (walk-form tag throw env)
            (value-of throw) (walk-form result throw env)))))

(defunwalker-handler throw-form (tag value)
  `(throw ,(unwalk-form tag) ,(unwalk-form value)))

;;;; EVAL-WHEN

(defclass eval-when-form (form implicit-progn-mixin)
  ((eval-when-times :accessor eval-when-times :initarg :eval-when-times)))

(defwalker-handler eval-when (form parent env)
  (destructuring-bind (times &body body)
      (cdr form)
    (with-form-object (eval-when eval-when-form :parent parent :source form)
      (setf (eval-when-times eval-when) times
            (body eval-when) (walk-implict-progn eval-when body env)))))

(defunwalker-handler eval-when-form (body eval-when-times)
  `(eval-when ,eval-when-times
     ,@(unwalk-forms body)))

;;;; IF

(defclass if-form (form)
  ((consequent :accessor consequent :initarg :consequent)
   (then :accessor then :initarg :then)
   (else :accessor else :initarg :else)))

(defwalker-handler if (form parent env)
  (with-form-object (if if-form :parent parent :source form)
    (setf (consequent if) (walk-form (second form) if env)
          (then if) (walk-form (third form) if env)
          (else if) (walk-form (fourth form) if env))))

(defunwalker-handler if-form (consequent then else)
  `(if ,(unwalk-form consequent) ,(unwalk-form then) ,(unwalk-form else)))

;;;; FLET/LABELS

(defclass function-binding-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass flet-form (function-binding-form)
  ())

(defclass labels-form (function-binding-form)
  ())

(defwalker-handler flet (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (flet flet-form :parent parent :source form)
      ;;;; build up the objects for the bindings in the original env
      (loop
         for (name args . body) in binds
         collect (cons name (walk-form `(lambda ,args ,@body) flet env)) into bindings
         finally (setf (binds flet) bindings))
      ;;;; walk the body in the new env
      (multiple-value-setf ((body flet) nil (declares flet))
                           (walk-implict-progn flet
                                               body
                                               (loop
                                                  with env = env
                                                  for (name . lambda) in (binds flet)
                                                  do (extend-walk-env env :flet name lambda)
                                                  finally (return env))
                                               :declare t)))))

(defwalker-handler labels (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (labels labels-form :parent parent :source form :binds '())
      ;; we need to walk over the bindings twice. the first pass
      ;; creates some 'empty' lambda objects in the environment so
      ;; that local-application-form and local-function-object-form
      ;; have something to point to. the second pass then walks the
      ;; actual bodies of the form filling in the previously created
      ;; objects.
      (loop
         for (name arguments . body) in binds
         for lambda = (make-instance 'lambda-function-form
                                     :parent labels
                                     :source (list* name arguments body))
         do (push (cons name lambda) (binds labels))
         do (extend-walk-env env :flet name lambda))
      (setf (binds labels) (nreverse (binds labels)))
      (loop
         for form in binds
         for (arguments . body) = (cdr form)
         for binding in (binds labels)
         for lambda = (cdr binding)
         for tmp-lambda = (walk-lambda `(lambda ,arguments ,@body) labels env)
         do (setf (body lambda) (body tmp-lambda)
                  (arguments-of lambda) (arguments-of tmp-lambda)
                  (declares lambda) (declares tmp-lambda)))
      (multiple-value-setf ((body labels) nil (declares labels)) (walk-implict-progn labels body env :declare t)))))

;; TODO factor out stuff
(defunwalker-handler flet-form (binds body declares)
  (flet ((unwalk-flet (binds)
           (mapcar #'(lambda (bind)
                       (cons (car bind)
                             ;; remove (function (lambda ...)) of the function bindings
                             (cdadr (unwalk-form (cdr bind)))))
                   binds)))
    `(flet ,(unwalk-flet binds)
       ,@(unwalk-declarations declares)
       ,@(unwalk-forms body))))

(defunwalker-handler labels-form (binds body declares)
  (flet ((unwalk-labels (binds)
           (mapcar #'(lambda (bind)
                       (cons (car bind)
                             ;; remove (function (lambda ...)) of the function bindings
                             (cdadr (unwalk-form (cdr bind)))))
                   binds)))
    `(labels ,(unwalk-labels binds)
       ,@(unwalk-declarations declares)
       ,@(unwalk-forms body))))

;;;; LET/LET*

(defclass variable-binding-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass let-form (variable-binding-form)
  ())

(defwalker-handler let (form parent env)
  (with-form-object (let let-form :parent parent :source form)
    (setf (binds let) (mapcar (lambda (binding)
                                   (destructuring-bind (var &optional initial-value)
                                       (ensure-list binding)
                                     (cons var (walk-form initial-value let env))))
                                 (second form)))
    (multiple-value-bind (b e d declarations)
        (split-body (cddr form) env :parent let :declare t)
      (declare (ignore b e d))
      (loop for (var . value) :in (binds let) do
            (unless (find-if (lambda (declaration)
                               (and (typep declaration 'special-declaration-form)
                                    (eq var (name-of declaration)))) declarations)
              (extend-walk-env env :let var :dummy)))
      (multiple-value-setf ((body let) nil (declares let))
                           (walk-implict-progn let (cddr form) env :declare t)))))

(defunwalker-handler let-form (binds body declares)
  (flet ((unwalk-let (binds)
           (mapcar #'(lambda (bind)
                       (list (car bind) (unwalk-form (cdr bind))))
                   binds)))
    `(let ,(unwalk-let binds)
       ,@(unwalk-declarations declares)
       ,@(unwalk-forms body))))

(defclass let*-form (variable-binding-form)
  ())

(defwalker-handler let* (form parent env)
  (with-form-object (let* let*-form :parent parent :source form :binds '())
    (dolist* ((var &optional initial-value) (mapcar #'ensure-list (second form)))
      (push (cons var (walk-form initial-value let* env)) (binds let*))
      (extend-walk-env env :let var :dummy))
    (setf (binds let*) (nreverse (binds let*)))
    (multiple-value-setf ((body let*) nil (declares let*)) (walk-implict-progn let* (cddr form) env :declare t))))

(defunwalker-handler let*-form (binds body declares)
  (flet ((unwalk-let* (binds)
           (mapcar #'(lambda (bind)
                       (list (car bind) (unwalk-form (cdr bind))))
                   binds)))
    `(let* ,(unwalk-let* binds)
       ,@(unwalk-declarations declares)
       ,@(unwalk-forms body))))

;;;; LOAD-TIME-VALUE

(defclass load-time-value-form (form)
  ((value :accessor value-of)
   (read-only-p :accessor read-only-p)))

(defwalker-handler load-time-value (form parent env)
  (with-form-object (load-time-value load-time-value-form
                                     :parent parent :source form)
    (setf (value-of load-time-value) (walk-form (second form) load-time-value env)
          (read-only-p load-time-value) (third form))))

(defunwalker-handler load-time-value-form (body read-only)
  `(load-time-value ,(unwalk-form body) ,read-only))

;;;; LOCALLY

(defclass locally-form (form implicit-progn-with-declare-mixin)
  ())

(defwalker-handler locally (form parent env)
  (with-form-object (locally locally-form :parent parent :source form)
    (multiple-value-setf ((body locally) nil (declares locally)) (walk-implict-progn locally (cdr form) env :declare t))))

(defunwalker-handler locally-form (body declares)
  `(locally ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

;;;; MACROLET

(defclass macrolet-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler macrolet (form parent env)
  (with-form-object (macrolet macrolet-form :parent parent :source form
                              :binds '())
    (dolist* ((name args &body body) (second form))
      (let ((handler (parse-macro-definition name args body (cdr env))))
        (extend-walk-env env :macrolet name handler)
        (push (cons name handler) (binds macrolet))))
    (setf (binds macrolet) (nreverse (binds macrolet)))
    (multiple-value-setf ((body macrolet) nil (declares macrolet))
      (walk-implict-progn macrolet (cddr form) env :declare t))))

(defunwalker-handler macrolet-form (body binds declares)
  ;; We ignore the binds, because the expansion has already taken
  ;; place at walk-time.
  (declare (ignore binds))
  `(locally ,@(unwalk-declarations declares) ,@(unwalk-forms body)))

;;;; MULTIPLE-VALUE-CALL

(defclass multiple-value-call-form (form)
  ((func :accessor func :initarg :func)
   (arguments :accessor arguments-of :initarg :arguments)))

(defwalker-handler multiple-value-call (form parent env)
  (with-form-object (m-v-c multiple-value-call-form :parent parent :source form)
    (setf (func m-v-c) (walk-form (second form) m-v-c env)
          (arguments-of m-v-c) (mapcar (lambda (f) (walk-form f m-v-c env))
                                       (cddr form)))))

(defunwalker-handler multiple-value-call-form (func arguments)
  `(multiple-value-call ,(unwalk-form func) ,@(unwalk-forms arguments)))

;;;; MULTIPLE-VALUE-PROG1

(defclass multiple-value-prog1-form (form)
  ((first-form :accessor first-form :initarg :first-form)
   (other-forms :accessor other-forms :initarg :other-forms)))

(defwalker-handler multiple-value-prog1 (form parent env)
  (with-form-object (m-v-p1 multiple-value-prog1-form :parent parent :source form)
    (setf (first-form m-v-p1) (walk-form (second form) m-v-p1 env)
          (other-forms m-v-p1) (mapcar (lambda (f) (walk-form f m-v-p1 env))
                                       (cddr form)))))

(defunwalker-handler multiple-value-prog1-form (first-form other-forms)
  `(multiple-value-prog1 ,(unwalk-form first-form) ,@(unwalk-forms other-forms)))

;;;; PROGN

(defclass progn-form (form implicit-progn-mixin)
  ())

(defwalker-handler progn (form parent env)
  (with-form-object (progn progn-form :parent parent :source form)
    (setf (body progn) (walk-implict-progn progn (cdr form) env))))

(defunwalker-handler progn-form (body)
  `(progn ,@(unwalk-forms body)))

;;;; PROGV

(defclass progv-form (form implicit-progn-mixin)
  ((vars-form :accessor vars-form :initarg :vars-form)
   (values-form :accessor values-form :initarg :values-form)))

(defwalker-handler progv (form parent env)
  (with-form-object (progv progv-form :parent parent :source form)
    (setf (vars-form progv) (walk-form (cadr form) progv env))
    (setf (values-form progv) (walk-form (caddr form) progv env))
    (setf (body progv) (walk-implict-progn progv (cdddr form) env))
    progv))

(defunwalker-handler progv-form (body vars-form values-form)
  `(progv ,(unwalk-form vars-form) ,(unwalk-form values-form) ,@(unwalk-forms body)))

;;;; QUOTE

(defwalker-handler quote (form parent env)
  (make-instance 'constant-form :parent parent :source form :value (second form)))

;;;; SETQ

(defclass setq-form (form)
  ((variable-name
    :accessor variable-name-of
    :initarg :variable-name)
   (value
    :accessor value-of
    :initarg :value)))

(defwalker-handler setq (form parent env)
  ;; the SETQ handler needs to be able to deal with symbol-macrolets
  ;; which haven't yet been expanded and may expand into something
  ;; requiring setf and not setq.
  (let ((effective-code '()))
    (loop
       for (name value) on (cdr form) by #'cddr
       if (lookup-walk-env env :symbol-macrolet name)
         do (push `(setf ,(lookup-walk-env env :symbol-macrolet name) ,value) effective-code)
       else
         do (push `(setq ,name ,value) effective-code))
    (if (= 1 (length effective-code))
        ;; only one form, the "simple case"
        (destructuring-bind (type var value)
            (first effective-code)
          (ecase type
            (setq (with-form-object (setq setq-form :parent parent :source form
                                          :variable-name var)
                    (setf (value-of setq) (walk-form value setq env))))
            (setf (walk-form (first effective-code) parent env))))
        ;; multiple forms
        (with-form-object (progn progn-form :parent parent :source form)
          (setf (body progn) (walk-implict-progn progn effective-code env))))))

(defunwalker-handler setq-form (variable-name value)
  `(setq ,variable-name ,(unwalk-form value)))

;;;; SYMBOL-MACROLET

(defclass symbol-macrolet-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler symbol-macrolet (form parent env)
  (with-form-object (symbol-macrolet symbol-macrolet-form :parent parent :source form
                                     :binds '())
    (dolist* ((symbol expansion) (second form))
      (extend-walk-env env :symbol-macrolet symbol expansion)
      (push (cons symbol expansion) (binds symbol-macrolet)))
    (setf (binds symbol-macrolet) (nreverse (binds symbol-macrolet)))
    (multiple-value-setf ((body symbol-macrolet) nil (declares symbol-macrolet))
      (walk-implict-progn symbol-macrolet (cddr form) env :declare t))))

(defunwalker-handler symbol-macrolet-form (body binds declares)
  ;; We ignore the binds, because the expansion has already taken
  ;; place at walk-time.
  (declare (ignore binds))
  `(locally ,@(unwalk-declarations declares) ,@(unwalk-forms body)))

;;;; TAGBODY/GO

(defclass tagbody-form (form implicit-progn-mixin)
  ())

(defwalker-handler tagbody (form parent env)
  (with-form-object (tagbody tagbody-form :parent parent :source form :body (cdr form))
    (extend-walk-env env :tagbody 'enclosing-tagbody tagbody)
    (flet ((go-tag-p (form)
             (or (symbolp form) (integerp form))))
      ;; the loop below destructuivly modifies the body of tagbody,
      ;; since it's the same object as the source we need to copy it.
      (setf (body tagbody) (copy-list (body tagbody)))
      (loop
         for part on (body tagbody)
         if (go-tag-p (car part))
           do (extend-walk-env env :tag (car part) (cdr part)))
      (loop
         for part on (body tagbody)
         if (go-tag-p (car part))
           do (setf (car part) (make-instance 'go-tag-form :parent tagbody
                                              :source (car part)
                                              :name (car part)))
         else
           do (setf (car part) (walk-form (car part) tagbody env))))))

(defunwalker-handler tagbody-form (body)
  `(tagbody ,@(unwalk-forms body)))

(defclass go-tag-form (form)
  ((name :accessor name-of :initarg :name)))

(defunwalker-handler go-tag-form (name)
  name)

(defclass go-form (form)
  ((jump-target :accessor jump-target-of :initarg :jump-target)
   (name :accessor name-of :initarg :name)
   (enclosing-tagbody :accessor enclosing-tagbody-of :initarg :enclosing-tagbody)))

(defwalker-handler go (form parent env)
  (make-instance 'go-form
                 :parent parent
                 :source form
                 :name (second form)
                 :jump-target (lookup-walk-env env :tag (second form))
                 :enclosing-tagbody (lookup-walk-env env :tagbody 'enclosing-tagbody)))

(defunwalker-handler go-form (name)
  `(go ,name))

;;;; THE

(defclass the-form (form)
  ((type :accessor type-of :initarg :type)
   (value :accessor value-of :initarg :value)))

(defwalker-handler the (form parent env)
  (with-form-object (the the-form :parent parent :source form
                                  :type (second form))
    (setf (value-of the) (walk-form (third form) the env))))

(defunwalker-handler the-form (type value)
  `(the ,type ,(unwalk-form value)))

;;;; UNWIND-PROTECT

(defclass unwind-protect-form (form)
  ((protected-form :accessor protected-form :initarg :protected-form)
   (cleanup-form :accessor cleanup-form :initarg :cleanup-form)))

(defwalker-handler unwind-protect (form parent env)
  (with-form-object (unwind-protect unwind-protect-form :parent parent
                                    :source form)
    (setf (protected-form unwind-protect) (walk-form (second form) unwind-protect env)
          (cleanup-form unwind-protect) (walk-implict-progn unwind-protect (cddr form) env))))

(defunwalker-handler unwind-protect-form (protected-form cleanup-form)
  `(unwind-protect ,(unwalk-form protected-form) ,@(unwalk-forms cleanup-form)))

;;;; LOAD-TIME-VALUE

(defclass load-time-value-form (form)
  ((body :accessor body :initarg :body)
   (read-only :initform nil :accessor read-only-p :initarg :read-only)
   (value :accessor value-of)))

(defmethod initialize-instance :after ((self load-time-value-form) &key)
  (setf (value-of self) (eval (body self))))

(defwalker-handler load-time-value (form parent env)
  (assert (<= (length form) 3))
  (with-form-object (load-time-value load-time-value-form :parent parent
                                     :body form
                                     :read-only (third form))
    (setf (body load-time-value) (walk-form (second form)))))
