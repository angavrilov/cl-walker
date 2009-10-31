;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker-test)

(defsuite* (test/utils :in test))

(deftest test-collect-variable-references (form expected-count)
  (with-walker-configuration (:undefined-reference-handler nil)
    (bind ((ast (walk-form form)))
      (is (= expected-count
             (length (collect-variable-references ast)))))))

(deftest test/utils/collect-variable-references/1 ()
  (loop
     :for (form expected-count) :in
     '((var 1)
       ((fn var1 var2) 2)
       ((progn (+ var1 var2)) 2)
       ((flet ((fn (x)
                 (+ x a)))
          (+ var1 (fn var2)))
        4)
       ((macrolet ((mac (x)
                     `(+ ,x a)))
          (mac (+ var1 var2)))
        3)
       ((let ((a (+ b c)))
          a)
        3))
     :do (test-collect-variable-references form expected-count)))

(deftest test/utils/var-bindings ()
  (let* ((ast (walk-form '(let ((a 1) (b 2))
                           (declare (special b))
                           a b)))
         (binds (bindings-of ast))
         (ops (body-of ast)))
    (is (eql (first binds)
             (binding-of (first ops))))
    (is (special-binding? (second binds)))
    (is (typep (second ops)
               'special-variable-reference-form))))

(deftest test/utils/func-bindings ()
  (let* ((ast (walk-form '(labels ((foo () (foo)))
                           (flet ((bar () #'foo (bar)))
                             (foo)
                             (bar)))))
         (foo-code (first (bindings-of ast)))
         (lab-body (first (body-of ast)))
         (bar-code (first (bindings-of lab-body)))
         (flt-body (body-of lab-body)))
    (is (eql (code-of (first (body-of foo-code)))
             foo-code))
    (is (eql (binding-of (first (body-of bar-code)))
             foo-code))
    (is (typep (second (body-of bar-code))
               'free-application-form))
    (is (eql (code-of (first flt-body))
             foo-code))
    (is (eql (code-of (second flt-body))
             bar-code))))

(deftest test/utils/func-args ()
  (let* ((ast (walk-form '(lambda (a &optional (b a) &key (c b))
                           a b c)))
         (args (arguments-of ast)))
    (is (every (lambda (x) (typep x 'binding-entry-mixin))
               args))
    (is (every #'eql args
               (mapcar #'binding-of (body-of ast))))
    (is (eql (first args)
             (binding-of (default-value-of (second args)))))
    (is (eql (second args)
             (binding-of (default-value-of (third args)))))))
