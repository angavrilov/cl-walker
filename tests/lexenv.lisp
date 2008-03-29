;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker-test)

(defsuite* (test/lexenv :in test))

;; TODO would be better to use macroexpand-all for this, because stefil's errors get to cl:compile
;; which aborts at the first failure and skips the other assertions
(defun compile* (form)
  (handler-bind ((warning #'muffle-warning)
                 #+sbcl (sb-ext:compiler-note #'muffle-warning))
    (compile nil `(lambda ()
                    ,form)))
  (values))

(defsuite* (test/lexenv/query :in test/lexenv))

(deftest test/lexenv/query/variables ()
  (compile*
   `(symbol-macrolet ((a 42)
                      (b 43))
      (flet ((f1 () 1)
             (f2 () 2))
        (macrolet ((m1 () 1)
                   (m2 () 2))
          (let ((x 1)
                (y 2)
                (z 3))
            (declare (ignore z))
            (macrolet ((dummy (&environment env)
                         (is (equal '(y x)
                                    (collect-variables-in-lexenv env)))
                         (bind ((ignored 0)
                                (non-ignored 0))
                           (do-variables-in-lexenv (env name ignored?)
                             (is (and (symbolp name)
                                      (eq (symbol-package name) ,*package*)))
                             (if ignored?
                                 (incf ignored)
                                 (incf non-ignored)))
                           (is (= ignored 1))
                           (is (= non-ignored 2)))
                         (is (equal '(z y x)
                                    (collect-variables-in-lexenv env :include-ignored t)))
                         (is (find-variable-in-lexenv 'x env))
                         (is (not (find-variable-in-lexenv 'z env)))
                         (is (not (find-variable-in-lexenv 'a env)))
                         (is (not (find-variable-in-lexenv 'f1 env)))
                         (is (not (find-variable-in-lexenv 'm1 env)))
                         (is (find-variable-in-lexenv 'z env :include-ignored t))
                         nil))
              (dummy))))))))

(deftest test/lexenv/query/functions ()
  (compile*
   `(symbol-macrolet ((a 42)
                      (b 43))
      (flet ((f1 () 1)
             (f2 () 2))
        (macrolet ((m1 () 1)
                   (m2 () 2))
          (let ((x 1)
                (y 2)
                (z 3))
            (declare (ignore z))
            (macrolet ((dummy (&environment env)
                         (is (equal '(f2 f1)
                                    (collect-functions-in-lexenv env)))
                         (bind ((functions 0))
                           (do-functions-in-lexenv (env name)
                             (is (and (symbolp name)
                                      (eq (symbol-package name) ,*package*)))
                             (incf functions))
                           (is (= functions 2)))
                         (is (find-function-in-lexenv 'f1 env))
                         (is (not (find-function-in-lexenv 'foo env)))
                         (is (not (find-function-in-lexenv 'a env)))
                         (is (not (find-function-in-lexenv 'm1 env)))
                         (is (not (find-function-in-lexenv 'dummy env)))
                         nil))
              (dummy))))))))

(deftest test/lexenv/query/macros ()
  (compile*
   `(symbol-macrolet ((a 42)
                      (b 43))
      (flet ((f1 () 1)
             (f2 () 2))
        (macrolet ((m1 () 1)
                   (m2 () 2))
          (let ((x 1)
                (y 2)
                (z 3))
            (declare (ignore z))
            (macrolet ((dummy (&environment env)
                         (is (equal '(dummy m1 m2)
                                    (collect-macros-in-lexenv env)))
                         (bind ((macros 0))
                           (do-macros-in-lexenv (env name fn)
                             (is (and (symbolp name)
                                      (eq (symbol-package name) ,*package*)))
                             (is (functionp fn))
                             (incf macros))
                           (is (= macros 3)))
                         (is (find-macro-in-lexenv 'm1 env))
                         (is (not (find-macro-in-lexenv 'f1 env)))
                         (is (not (find-macro-in-lexenv 'a env)))
                         (is (not (find-macro-in-lexenv 'x env)))
                         nil))
              (dummy))))))))

(deftest test/lexenv/query/symbol-macros ()
  (compile*
   `(symbol-macrolet ((a 42)
                      (b 43))
      (flet ((f1 () 1)
             (f2 () 2))
        (macrolet ((m1 () 1)
                   (m2 () 2))
          (let ((x 1)
                (y 2)
                (z 3))
            (declare (ignore z))
            (macrolet ((dummy (&environment env)
                         (is (equal '(a b)
                                    (collect-symbol-macros-in-lexenv env)))
                         (bind ((symbol-macros 0))
                           (do-symbol-macros-in-lexenv (env name definition)
                             (is (and (symbolp name)
                                      (eq (symbol-package name) ,*package*)))
                             (is (not (functionp definition)))
                             (incf symbol-macros))
                           (is (= symbol-macros 2)))
                         (is (find-symbol-macro-in-lexenv 'a env))
                         (is (not (find-symbol-macro-in-lexenv 'm1 env)))
                         (is (not (find-symbol-macro-in-lexenv 'f1 env)))
                         (is (not (find-symbol-macro-in-lexenv 'x env)))
                         nil))
              (dummy))))))))

(deftest test/lexenv/query/blocks ()
  (compile*
   `(block b1
      (flet ((f1 () 1)
             (f2 () 2))
        (block b2
          (let ((x 1)
                (y 2)
                (z 3))
            (declare (ignore z))
            (macrolet ((dummy (&environment env)
                         (is (equal '(b2 b1)
                                    (collect-blocks-in-lexenv env)))
                         (bind ((blocks 0))
                           (do-blocks-in-lexenv (env name)
                             (is (and (symbolp name)
                                      (eq (symbol-package name) ,*package*)))
                             (incf blocks))
                           (is (= blocks 2)))
                         (is (find-block-in-lexenv 'b1 env))
                         (is (not (find-block-in-lexenv 'dummy env)))
                         (is (not (find-block-in-lexenv 'f1 env)))
                         (is (not (find-block-in-lexenv 'x env)))
                         nil))
              (dummy))))))))

